// include first to avoid ambiguity for comparison operator from
// util/spaceship.h
#include "llvm/MC/MCAsmInfo.h"

#include "backend_tv/lifter.h"

#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/AsmParser/Parser.h"
#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/InitializePasses.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInstBuilder.h"
#include "llvm/MC/MCInstPrinter.h"
#include "llvm/MC/MCInstrAnalysis.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCParser/MCAsmParser.h"
#include "llvm/MC/MCParser/MCTargetAsmParser.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/MC/MCTargetOptions.h"
#include "llvm/MC/MCTargetOptionsCommandFlags.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Pass.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Triple.h"
#include "llvm/Transforms/Utils/Cloning.h"

#define GET_INSTRINFO_ENUM
#include "Target/AArch64/AArch64GenInstrInfo.inc"

#define GET_REGINFO_ENUM
#include "Target/AArch64/AArch64GenRegisterInfo.inc"

#include <algorithm>
#include <fstream>
#include <iostream>
#include <ranges>
#include <regex>
#include <sstream>
#include <unordered_map>
#include <utility>
#include <vector>

using namespace std;
using namespace llvm;
using namespace lifter;

// avoid collisions with the upstream AArch64 namespace
namespace llvm::AArch64 {
const unsigned N = 100000000;
const unsigned Z = 100000001;
const unsigned C = 100000002;
const unsigned V = 100000003;
} // namespace llvm::AArch64

// do not delete this line
mc::RegisterMCTargetOptionsFlags MOF;

namespace {

// Represents a basic block of machine instructions
class MCBasicBlock {
private:
  string name;
  vector<MCInst> Instrs;
  SetVector<MCBasicBlock *> Succs;

public:
  MCBasicBlock(const string &name) : name(name) {}

  const string &getName() const {
    return name;
  }

  auto &getInstrs() {
    return Instrs;
  }

  bool empty() const {
    return Instrs.size() == 0;
  }

  auto &getSuccs() {
    return Succs;
  }

  void addInst(MCInst &inst) {
    Instrs.push_back(inst);
  }

  void addInstBegin(MCInst &&inst) {
    Instrs.insert(Instrs.begin(), std::move(inst));
  }

  void addSucc(MCBasicBlock *succ_block) {
    Succs.insert(succ_block);
  }
};

struct OffsetSym {
  string sym;
  long offset;
};

typedef variant<char, OffsetSym> RODataItem;

struct MCGlobal {
  string name;
  Align align;
  string section;
  vector<RODataItem> data;
};

class MCFunction {
  string name;
  unsigned label_cnt{0};

public:
  MCInstrAnalysis *IA;
  MCInstPrinter *IP;
  MCRegisterInfo *MRI;
  vector<MCBasicBlock> BBs;
  vector<MCGlobal> MCglobals;

  MCFunction() {
    MCGlobal g{
        .name = "__stack_chk_guard",
        .align = Align(8),
        .section = ".rodata",
        // FIXME -- use symbolic data here? does this matter?
        .data = {'7', '7', '7', '7', '7', '7', '7', '7'},
    };
    MCglobals.push_back(g);
  }

  void setName(const string &_name) {
    name = _name;
  }

  MCBasicBlock *addBlock(const string &b_name) {
    return &BBs.emplace_back(b_name);
  }

  string getName() {
    return name;
  }

  string getLabel() {
    return name + to_string(++label_cnt);
  }

  MCBasicBlock *findBlockByName(const string &b_name) {
    for (auto &bb : BBs)
      if (bb.getName() == b_name)
        return &bb;
    return nullptr;
  }

  void checkEntryBlock() {
    // LLVM doesn't let the entry block be a jump target, but assembly
    // does; we can fix that up by adding an extra block at the start
    // of the function. simplifyCFG will clean this up when it's not
    // needed.
    BBs.emplace(BBs.begin(), "arm_tv_entry");
    MCInst jmp_instr;
    jmp_instr.setOpcode(AArch64::B);
    jmp_instr.addOperand(MCOperand::createImm(1));
    BBs[0].addInstBegin(std::move(jmp_instr));
  }
};

class arm2llvm {
  Module *LiftedModule{nullptr};
  LLVMContext &Ctx = LiftedModule->getContext();
  MCFunction &MF;
  Function &srcFn;
  Function *liftedFn{nullptr};
  MCBasicBlock *MCBB{nullptr};
  BasicBlock *LLVMBB{nullptr};
  MCInstPrinter *instrPrinter{nullptr};
  MCInst *CurInst{nullptr}, *PrevInst{nullptr};
  unsigned armInstNum{0}, llvmInstNum{0};
  map<unsigned, Value *> RegFile;
  Value *stackMem{nullptr};
  unordered_map<string, Constant *> LLVMglobals;
  Value *initialSP, *initialReg[32];
  Function *assertDecl;

  // Map of ADRP MCInsts to the string representations of the operand variable
  // names
  unordered_map<MCInst *, string> instExprVarMap;
  const DataLayout &DL;

  struct deferredGlobal {
    string name;
    GlobalVariable *val;
  };
  vector<deferredGlobal> deferredGlobs;

  Constant *lazyAddGlobal(string newGlobal) {
    *out << "  lazyAddGlobal '" << newGlobal << "'\n";

    // unmangle these
    if (newGlobal == "memcpy")
      newGlobal = "llvm.memcpy.p0.p0.i64";
    if (newGlobal == "memset")
      newGlobal = "llvm.memset.p0.i64";
    if (newGlobal == "memmove")
      newGlobal = "llvm.memmove.p0.p0.i64";

    if (newGlobal == "__stack_chk_fail") {
      return new GlobalVariable(*LiftedModule, getIntTy(8), false,
                                GlobalValue::LinkageTypes::ExternalLinkage,
                                nullptr, "__stack_chk_fail");
    }

    // is the global the address of a function?
    if (newGlobal == liftedFn->getName()) {
      // pointer to the function we're lifting into is a special case
      *out << "  yay it's me!\n";
      return liftedFn;
    }
    for (auto &f : *srcFn.getParent()) {
      auto name = f.getName();
      if (name != newGlobal)
        continue;
      *out << "  creating function '" << newGlobal << "'\n";
      auto newF = Function::Create(f.getFunctionType(),
                                   GlobalValue::LinkageTypes::ExternalLinkage,
                                   name, LiftedModule);
      if (f.hasFnAttribute(Attribute::NoReturn))
        newF->addFnAttr(Attribute::NoReturn);
      if (f.hasRetAttribute(Attribute::NoAlias))
        newF->addRetAttr(Attribute::NoAlias);
      if (f.hasRetAttribute(Attribute::SExt))
        newF->addRetAttr(Attribute::SExt);
      if (f.hasRetAttribute(Attribute::ZExt))
        newF->addRetAttr(Attribute::ZExt);
      return newF;
    }

    // globals that are definitions in the assembly can be lifted
    // directly. here we have to deal with a mix of literal and
    // symbolic data, and also deal with globals whose initializers
    // reference each other. we support the latter by creating an
    // initializer-free dummy variable right now, and then later
    // creating the actual thing we need
    for (const auto &g : MF.MCglobals) {
      string name{demangle(g.name)};
      if (name != newGlobal)
        continue;
      *out << "creating lifted global " << name << " from the assembly\n";
      auto size = g.data.size();
      *out << "  section = " << g.section << "\n";

      vector<Type *> tys;
      vector<Constant *> vals;
      for (unsigned i = 0; i < size; ++i) {
        auto &data = g.data[i];
        if (holds_alternative<char>(data)) {
          tys.push_back(getIntTy(8));
          vals.push_back(ConstantInt::get(getIntTy(8), get<char>(data)));
        } else if (holds_alternative<OffsetSym>(data)) {
          auto s = get<OffsetSym>(data);
          *out << "  it's a symbol named " << s.sym << "\n";
          auto res = LLVMglobals.find(s.sym);
          Constant *con;
          if (res == LLVMglobals.end()) {
            // ok, this global hasn't been lifted yet. but it might
            // already be in the deferred queue, in which case we can
            // use its dummy version.
            bool found = false;
            for (auto [name, _con] : deferredGlobs) {
              if (name == s.sym) {
                found = true;
                con = _con;
                break;
              }
            }
            if (!found) {
              *out << "  breaking recursive loop by deferring " << s.sym
                   << "\n";
              auto *dummy =
                  new GlobalVariable(*LiftedModule, getIntTy(8), false,
                                     GlobalValue::LinkageTypes::ExternalLinkage,
                                     nullptr, s.sym + "_tmp");
              deferredGlobs.push_back({.name = demangle(s.sym), .val = dummy});
              con = dummy;
            }
          } else {
            *out << "  found it using regular lookup\n";
            con = res->second;
          }
          Constant *offset = getIntConst(s.offset, 64);
          Constant *ptr =
              ConstantExpr::getGetElementPtr(getIntTy(8), con, offset);
          tys.push_back(PointerType::get(Ctx, 0));
          vals.push_back(ptr);
        } else {
          assert(false);
        }
      }
      auto *ty = StructType::create(tys);
      auto initializer = ConstantStruct::get(ty, vals);
      bool isConstant =
          g.section.starts_with(".rodata") || g.section == ".text";
      auto *glob = new GlobalVariable(
          *LiftedModule, ty, isConstant,
          GlobalValue::LinkageTypes::ExternalLinkage, initializer, name);
      glob->setAlignment(g.align);
      return glob;
    }

    // globals that are only declarations have to be registered in
    // LLVM IR, but they don't show up anywhere in the assembly -- the
    // declaration is implicit. so here we find those and create them
    // in the lifted module
    for (auto &srcFnGlobal : srcFn.getParent()->globals()) {
      if (!srcFnGlobal.isDeclaration())
        continue;
      string name{srcFnGlobal.getName()};
      if (name != newGlobal)
        continue;
      *out << "creating declaration for global variable " << name << "\n";
      *out << "  linkage = " << srcFnGlobal.getLinkage() << "\n";
      auto *glob = new GlobalVariable(
          *LiftedModule, srcFnGlobal.getValueType(), srcFnGlobal.isConstant(),
          GlobalValue::LinkageTypes::ExternalLinkage,
          /*initializer=*/nullptr, name);
      glob->setAlignment(srcFnGlobal.getAlign());
      return glob;
    }

    *out << "ERROR: symbol '" << newGlobal << "' not found\n";
    assert(false);
  }

  // create lifted globals only on demand -- saves time and clutter for
  // large modules
  Constant *lookupGlobal(const string &nm) {
    auto name = demangle(nm);
    *out << "lookupGlobal '" << name << "'\n";

    auto glob = LLVMglobals.find(name);
    if (glob != LLVMglobals.end())
      return glob->second;

    auto *g = lazyAddGlobal(name);
    assert(g);
    LLVMglobals[name] = g;

    // lifting this one may have necessitated lifting other variables,
    // which we deferred, and created placeholders instead. fill those
    // in now. this may keep cascading for a while, no problem! our
    // overall goal here is to end up lifting the transitive closure
    // of stuff reachable from the function we're lifting, and nothing
    // else.
    while (!deferredGlobs.empty()) {
      *out << "  processing a deferred global\n";
      auto def = deferredGlobs.at(0);
      deferredGlobs.erase(deferredGlobs.begin());
      auto g2 = lazyAddGlobal(def.name);
      def.val->replaceAllUsesWith(g2);
      def.val->eraseFromParent();
      LLVMglobals[def.name] = g2;
    }

    return g;
  }

  BasicBlock *getBB(MCOperand &jmp_tgt) {
    assert(jmp_tgt.isExpr() && "[getBB] expected expression operand");
    assert((jmp_tgt.getExpr()->getKind() == MCExpr::ExprKind::SymbolRef) &&
           "[getBB] expected symbol ref as jump operand");
    const MCSymbolRefExpr &SRE = cast<MCSymbolRefExpr>(*jmp_tgt.getExpr());
    const MCSymbol &Sym = SRE.getSymbol();
    StringRef name = Sym.getName();
    for (auto &bb : *liftedFn) {
      if (bb.getName() == name)
        return &bb;
    }
    return nullptr;
  }

  // FIXME -- do this without the strings, just keep a map or something
  BasicBlock *getBBByName(StringRef name) {
    for (auto &bb : *liftedFn) {
      if (bb.getName() == name)
        return &bb;
    }
    assert(false && "basic block not found in getBBByName()");
  }

  static bool isSIMDandFPRegOperand(MCOperand &op) {
    assert(op.isReg() && "[isSIMDandFPRegOperand] expected register operand");
    unsigned reg = op.getReg();
    return (reg >= AArch64::Q0 && reg <= AArch64::Q31) ||
           (reg >= AArch64::D0 && reg <= AArch64::D31) ||
           (reg >= AArch64::S0 && reg <= AArch64::S31);
  }

  Constant *getIntConst(uint64_t val, u_int64_t bits) {
    return ConstantInt::get(Ctx, llvm::APInt(bits, val));
  }

  VectorType *getVecTy(unsigned eltSize, unsigned numElts, bool isFP = false) {
    Type *eTy;
    if (isFP) {
      eTy = getFPType(eltSize);
    } else {
      eTy = getIntTy(eltSize);
    }
    auto ec = ElementCount::getFixed(numElts);
    return VectorType::get(eTy, ec);
  }

  Constant *getUndefVec(unsigned numElts, unsigned eltSize) {
    auto eTy = getIntTy(eltSize);
    auto ec = ElementCount::getFixed(numElts);
    return ConstantVector::getSplat(ec, UndefValue::get(eTy));
  }

  Constant *getZeroVec(unsigned numElts, unsigned eltSize) {
    auto ec = ElementCount::getFixed(numElts);
    return ConstantVector::getSplat(ec, getIntConst(0, eltSize));
  }

  Type *getIntTy(unsigned bits) {
    return Type::getIntNTy(Ctx, bits);
  }

  Type *getFPType(unsigned bits) {
    if (bits == 16)
      return Type::getHalfTy(Ctx);
    else if (bits == 32)
      return Type::getFloatTy(Ctx);
    else if (bits == 64)
      return Type::getDoubleTy(Ctx);
    else
      assert(false && "unsupported floating point type");
  }

  // Create and return a ConstantVector out of the vector of Constant vals
  Value *getVectorConst(const std::vector<Constant *> &vals) {
    return ConstantVector::get(vals);
  }

  // Takes an LLVM Type*, constructs a mask value of this type with
  // mask value = W - 1 where W is the bitwidth of the element type if a vector
  // type or the bitwidth of the type if an integer
  Value *getMaskByType(Type *llvm_ty) {
    assert((llvm_ty->isIntegerTy() || llvm_ty->isVectorTy()) &&
           "getMaskByType only handles integer or vector type right now\n");
    Value *mask_value;

    if (llvm_ty->isIntegerTy()) {
      auto W = llvm_ty->getIntegerBitWidth();
      mask_value = getIntConst(W - 1, W);
    } else if (llvm_ty->isVectorTy()) {
      VectorType *shift_value_type = ((VectorType *)llvm_ty);
      auto W_element = shift_value_type->getScalarSizeInBits();
      auto numElements = shift_value_type->getElementCount().getFixedValue();
      vector<Constant *> widths;

      // Push numElements x (W_element-1)'s to the vector widths
      for (unsigned int i = 0; i < numElements; i++) {
        widths.push_back(
            ConstantInt::get(Ctx, llvm::APInt(W_element, W_element - 1)));
      }

      // Get a ConstantVector of the widths
      mask_value = getVectorConst(widths);
    } else {
      *out << "ERROR: getMaskByType encountered unhandled/unknown type\n";
      exit(-1);
    }

    return mask_value;
  }

  [[noreturn]] void visitError() {
    out->flush();
    string str(instrPrinter->getOpcodeName(CurInst->getOpcode()));
    *out << "\nERROR: Unsupported AArch64 instruction: " << str << "\n";
    out->flush();
    exit(-1);
  }

  const set<int> s_flag = {
      AArch64::ADDSWri, AArch64::ADDSWrs, AArch64::ADDSWrx, AArch64::ADDSXri,
      AArch64::ADDSXrs, AArch64::ADDSXrx, AArch64::SUBSWri, AArch64::SUBSWrs,
      AArch64::SUBSWrx, AArch64::SUBSXri, AArch64::SUBSXrs, AArch64::SUBSXrx,
      AArch64::ANDSWri, AArch64::ANDSWrr, AArch64::ANDSWrs, AArch64::ANDSXri,
      AArch64::ANDSXrr, AArch64::ANDSXrs, AArch64::BICSWrs, AArch64::BICSXrs,
      AArch64::ADCSXr,  AArch64::ADCSWr,
  };

  const set<int> instrs_32 = {
      AArch64::TBNZW,      AArch64::TBZW,       AArch64::CBZW,
      AArch64::CBNZW,      AArch64::ADDWrx,     AArch64::ADDSWrs,
      AArch64::ADDSWri,    AArch64::ADDWrs,     AArch64::ADDWri,
      AArch64::ADDSWrx,    AArch64::ADCWr,      AArch64::ADCSWr,
      AArch64::ASRVWr,     AArch64::SUBWri,     AArch64::SUBWrs,
      AArch64::SUBWrx,     AArch64::SUBSWrs,    AArch64::SUBSWri,
      AArch64::SUBSWrx,    AArch64::SBFMWri,    AArch64::CSELWr,
      AArch64::ANDWri,     AArch64::ANDWrr,     AArch64::ANDWrs,
      AArch64::ANDSWri,    AArch64::ANDSWrr,    AArch64::ANDSWrs,
      AArch64::MADDWrrr,   AArch64::MSUBWrrr,   AArch64::EORWri,
      AArch64::CSINVWr,    AArch64::CSINCWr,    AArch64::MOVZWi,
      AArch64::MOVNWi,     AArch64::MOVKWi,     AArch64::LSLVWr,
      AArch64::LSRVWr,     AArch64::ORNWrs,     AArch64::UBFMWri,
      AArch64::BFMWri,     AArch64::ORRWrs,     AArch64::ORRWri,
      AArch64::SDIVWr,     AArch64::UDIVWr,     AArch64::EXTRWrri,
      AArch64::EORWrs,     AArch64::RORVWr,     AArch64::RBITWr,
      AArch64::CLZWr,      AArch64::REVWr,      AArch64::CSNEGWr,
      AArch64::BICWrs,     AArch64::BICSWrs,    AArch64::EONWrs,
      AArch64::REV16Wr,    AArch64::Bcc,        AArch64::CCMPWr,
      AArch64::CCMPWi,     AArch64::LDRWui,     AArch64::LDRBBroW,
      AArch64::LDRBBroX,   AArch64::LDRBroW,    AArch64::LDRBroX,
      AArch64::LDRHHroW,   AArch64::LDRHHroX,   AArch64::LDRHroW,
      AArch64::LDRHroX,    AArch64::LDRWroW,    AArch64::LDRWroX,
      AArch64::LDRSBWroW,  AArch64::LDRSBWroX,  AArch64::LDRSBXroW,
      AArch64::LDRSBXroX,  AArch64::LDRSHWroW,  AArch64::LDRSHWroX,
      AArch64::LDRSHXroW,  AArch64::LDRSHXroX,  AArch64::LDRSWroW,
      AArch64::LDRSWroX,   AArch64::LDRSui,     AArch64::LDRBBui,
      AArch64::LDRBui,     AArch64::LDRSBWui,   AArch64::LDRSHWui,
      AArch64::LDRSBWpre,  AArch64::LDRSHWpre,  AArch64::LDRSBWpost,
      AArch64::LDRSHWpost, AArch64::LDRHHui,    AArch64::LDRHui,
      AArch64::LDURBi,     AArch64::LDURBBi,    AArch64::LDURHi,
      AArch64::LDURHHi,    AArch64::LDURSi,     AArch64::LDURWi,
      AArch64::LDURSBWi,   AArch64::LDURSHWi,   AArch64::LDRBBpre,
      AArch64::LDRBpre,    AArch64::LDRHHpre,   AArch64::LDRHpre,
      AArch64::LDRWpre,    AArch64::LDRSpre,    AArch64::LDRBBpost,
      AArch64::LDRBpost,   AArch64::LDRHHpost,  AArch64::LDRHpost,
      AArch64::LDRWpost,   AArch64::LDRSpost,   AArch64::STRBBpost,
      AArch64::STRBpost,   AArch64::STRHHpost,  AArch64::STRHpost,
      AArch64::STRWpost,   AArch64::STRSpost,   AArch64::STRWui,
      AArch64::STRBBroW,   AArch64::STRBBroX,   AArch64::STRBroW,
      AArch64::STRBroX,    AArch64::STRHHroW,   AArch64::STRHHroX,
      AArch64::STRHroW,    AArch64::STRHroX,    AArch64::STRWroW,
      AArch64::STRWroX,    AArch64::CCMNWi,     AArch64::CCMNWr,
      AArch64::STRBBui,    AArch64::STRBui,     AArch64::STPWi,
      AArch64::STPSi,      AArch64::STPWpre,    AArch64::STPSpre,
      AArch64::STPWpost,   AArch64::STPSpost,   AArch64::STRHHui,
      AArch64::STRHui,     AArch64::STURBBi,    AArch64::STURBi,
      AArch64::STURHHi,    AArch64::STURHi,     AArch64::STURWi,
      AArch64::STURSi,     AArch64::STRSui,     AArch64::LDPWi,
      AArch64::LDPSi,      AArch64::LDPWpre,    AArch64::LDPSpre,
      AArch64::LDPWpost,   AArch64::LDPSpost,   AArch64::STRBBpre,
      AArch64::STRBpre,    AArch64::STRHHpre,   AArch64::STRHpre,
      AArch64::STRWpre,    AArch64::STRSpre,    AArch64::FADDSrr,
      AArch64::FSUBSrr,    AArch64::FCMPSrr,    AArch64::FCMPESrr,
      AArch64::FCMPSri,    AArch64::FCMPESri,   AArch64::FMOVSWr,
      AArch64::INSvi32gpr, AArch64::INSvi16gpr, AArch64::INSvi8gpr,
      AArch64::FCVTZUUWHr, AArch64::FCVTZUUWSr, AArch64::FCVTZUUXHr,
      AArch64::FCVTZUUXSr, AArch64::FCVTZSUWHr, AArch64::FCVTZSUWSr,
      AArch64::FCVTZSUXHr, AArch64::FCVTZSUXSr, AArch64::FCVTZSv1i32,
      AArch64::FCVTSHr,    AArch64::FCVTDHr,    AArch64::FCVTHSr,
      AArch64::FCVTDSr,    AArch64::FCVTZSUWSr, AArch64::FCSELSrrr,
      AArch64::FMULSrr,    AArch64::FDIVSrr,    AArch64::FMADDSrrr,
      AArch64::FMSUBSrrr,  AArch64::FNMADDSrrr, AArch64::FNMSUBSrrr,
      AArch64::FNMULSrr,   AArch64::FSQRTSr,    AArch64::FMULv1i32_indexed,
      AArch64::FABSSr,     AArch64::UQADDv1i32, AArch64::SQSUBv1i32,
      AArch64::SQADDv1i32, AArch64::FMOVSr,     AArch64::FNEGSr,
      AArch64::BRK,        AArch64::UCVTFUWSri, AArch64::UCVTFUWDri,
      AArch64::SCVTFUWSri, AArch64::SCVTFUWDri, AArch64::SCVTFv1i32,
  };

  const set<int> instrs_64 = {
      AArch64::BL,
      AArch64::BR,
      AArch64::BLR,
      AArch64::ADDXrx,
      AArch64::ADDSXrs,
      AArch64::ADDSXri,
      AArch64::ADDXrs,
      AArch64::ADDXri,
      AArch64::ADDSXrx,
      AArch64::ADDv2i32,
      AArch64::ADDv4i16,
      AArch64::ADDv8i8,
      AArch64::USHLv8i8,
      AArch64::USHLv1i64,
      AArch64::USHLv4i16,
      AArch64::USHLv2i32,
      AArch64::SSHLv8i8,
      AArch64::SSHLv1i64,
      AArch64::SSHLv4i16,
      AArch64::SSHLv2i32,
      AArch64::SUBv2i32,
      AArch64::SUBv4i16,
      AArch64::SUBv8i8,
      AArch64::UABDv8i8,
      AArch64::UABDv4i16,
      AArch64::UABDv2i32,
      AArch64::SABDv8i8,
      AArch64::SABDv4i16,
      AArch64::SABDv2i32,
      AArch64::ADCXr,
      AArch64::ADCSXr,
      AArch64::ASRVXr,
      AArch64::SUBXri,
      AArch64::SUBXrs,
      AArch64::SUBXrx,
      AArch64::SUBSXrs,
      AArch64::SUBSXri,
      AArch64::SUBSXrx,
      AArch64::SBFMXri,
      AArch64::CSELXr,
      AArch64::ANDXri,
      AArch64::ANDXrr,
      AArch64::ANDXrs,
      AArch64::ANDSXri,
      AArch64::ANDSXrr,
      AArch64::ANDSXrs,
      AArch64::MADDXrrr,
      AArch64::MSUBXrrr,
      AArch64::EORXri,
      AArch64::CSINVXr,
      AArch64::CSINCXr,
      AArch64::MOVZXi,
      AArch64::MOVNXi,
      AArch64::MOVKXi,
      AArch64::LSLVXr,
      AArch64::LSRVXr,
      AArch64::ORNXrs,
      AArch64::UBFMXri,
      AArch64::BFMXri,
      AArch64::ORRXrs,
      AArch64::ORRXri,
      AArch64::SDIVXr,
      AArch64::UDIVXr,
      AArch64::EXTRXrri,
      AArch64::EORXrs,
      AArch64::SMADDLrrr,
      AArch64::UMADDLrrr,
      AArch64::RORVXr,
      AArch64::RBITXr,
      AArch64::CLZXr,
      AArch64::REVXr,
      AArch64::CSNEGXr,
      AArch64::BICXrs,
      AArch64::BICSXrs,
      AArch64::EONXrs,
      AArch64::SMULHrr,
      AArch64::UMULHrr,
      AArch64::REV32Xr,
      AArch64::REV16Xr,
      AArch64::SMSUBLrrr,
      AArch64::UMSUBLrrr,
      AArch64::PHI,
      AArch64::TBZX,
      AArch64::TBNZX,
      AArch64::B,
      AArch64::CBZX,
      AArch64::CBNZX,
      AArch64::CCMPXr,
      AArch64::CCMPXi,
      AArch64::LDRXui,
      AArch64::LDRXpre,
      AArch64::LDRDpre,
      AArch64::LDRXpost,
      AArch64::LDRDpost,
      AArch64::LDPXpre,
      AArch64::LDPDpre,
      AArch64::LDPXpost,
      AArch64::LDPDpost,
      AArch64::LDPSWi,
      AArch64::LDPXi,
      AArch64::LDPDi,
      AArch64::LDRDui,
      AArch64::LDRXroW,
      AArch64::LDRXroX,
      AArch64::LDURDi,
      AArch64::LDURXi,
      AArch64::LDURSBXi,
      AArch64::LDURSHXi,
      AArch64::LDURSWi,
      AArch64::STRDui,
      AArch64::MSR,
      AArch64::MRS,
      AArch64::LDRSBXui,
      AArch64::LDRSHXui,
      AArch64::LDRSWui,
      AArch64::LDRSBXpre,
      AArch64::LDRSHXpre,
      AArch64::LDRSWpre,
      AArch64::LDRSBXpost,
      AArch64::LDRSHXpost,
      AArch64::LDRSWpost,
      AArch64::STRXui,
      AArch64::STRXpost,
      AArch64::STRDpost,
      AArch64::STRXroW,
      AArch64::STRXroX,
      AArch64::STPXi,
      AArch64::STPDi,
      AArch64::STPXpre,
      AArch64::STPDpre,
      AArch64::STPXpost,
      AArch64::STPDpost,
      AArch64::CCMNXi,
      AArch64::CCMNXr,
      AArch64::STURXi,
      AArch64::STURDi,
      AArch64::ADRP,
      AArch64::STRXpre,
      AArch64::STRDpre,
      AArch64::FADDDrr,
      AArch64::FMULDrr,
      AArch64::FDIVDrr,
      AArch64::FSQRTDr,
      AArch64::FNMULDrr,
      AArch64::FMADDDrrr,
      AArch64::FMSUBDrrr,
      AArch64::FNMADDDrrr,
      AArch64::FNMSUBDrrr,
      AArch64::FMULv1i64_indexed,
      AArch64::FABSDr,
      AArch64::FSUBDrr,
      AArch64::FCMPDrr,
      AArch64::FCMPEDrr,
      AArch64::FCMPDri,
      AArch64::FCMPEDri,
      AArch64::NOTv8i8,
      AArch64::CNTv8i8,
      AArch64::ANDv8i8,
      AArch64::EORv8i8,
      AArch64::FMOVDXr,
      AArch64::INSvi64gpr,
      AArch64::MOVID,
      AArch64::FCVTZUUWDr,
      AArch64::FCVTZUUXDr,
      AArch64::FCVTZSUWDr,
      AArch64::FCVTZSUXDr,
      AArch64::FCVTZSv1i64,
      AArch64::FCVTHDr,
      AArch64::FCVTSDr,
      AArch64::FMOVDr,
      AArch64::FMOVv2f32_ns,
      AArch64::DUPv2i32gpr,
      AArch64::UMULLv2i32_v2i64,
      AArch64::USHLLv8i8_shift,
      AArch64::USHLLv2i32_shift,
      AArch64::USHLLv4i16_shift,
      AArch64::XTNv8i8,
      AArch64::XTNv4i16,
      AArch64::XTNv2i32,
      AArch64::MLSv2i32,
      AArch64::NEGv1i64,
      AArch64::NEGv4i16,
      AArch64::NEGv8i8,
      AArch64::NEGv2i32,
      AArch64::DUPv8i8gpr,
      AArch64::DUPv4i16gpr,
      AArch64::FCSELDrrr,
      AArch64::UADDLVv8i8v,
      AArch64::UADDLVv4i16v,
      AArch64::SADDLVv8i8v,
      AArch64::SADDLVv4i16v,
      AArch64::UADDLPv8i8_v4i16,
      AArch64::UADDLPv4i16_v2i32,
      AArch64::UADDLPv2i32_v1i64,
      AArch64::SADDLPv8i8_v4i16,
      AArch64::SADDLPv4i16_v2i32,
      AArch64::SADDLPv2i32_v1i64,
      AArch64::UABDLv8i8_v8i16,
      AArch64::UABDLv4i16_v4i32,
      AArch64::UABDLv2i32_v2i64,
      AArch64::SABDLv8i8_v8i16,
      AArch64::SABDLv4i16_v4i32,
      AArch64::SABDLv2i32_v2i64,
      AArch64::UADALPv8i8_v4i16,
      AArch64::UADALPv4i16_v2i32,
      AArch64::UADALPv2i32_v1i64,
      AArch64::SADALPv8i8_v4i16,
      AArch64::SADALPv4i16_v2i32,
      AArch64::SADALPv2i32_v1i64,
      AArch64::BIFv8i8,
      AArch64::BSLv8i8,
      AArch64::BICv4i16,
      AArch64::BICv8i8,
      AArch64::BICv2i32,
      AArch64::ADDVv8i8v,
      AArch64::ADDVv4i16v,
      AArch64::SHLv8i8_shift,
      AArch64::SHLv4i16_shift,
      AArch64::SHLv2i32_shift,
      AArch64::SSHRv4i16_shift,
      AArch64::SSHRv8i8_shift,
      AArch64::SSHRv2i32_shift,
      AArch64::ADDPv8i8,
      AArch64::ADDPv4i16,
      AArch64::ADDPv2i32,
      AArch64::CMEQv2i32,
      AArch64::CMHIv8i8,
      AArch64::CMHIv4i16,
      AArch64::CMHIv2i32,
      AArch64::CMHIv1i64,
      AArch64::CMGTv1i64,
      AArch64::CMEQv2i32rz,
      AArch64::CMEQv4i16,
      AArch64::CMEQv4i16rz,
      AArch64::CMEQv8i8,
      AArch64::CMEQv8i8rz,
      AArch64::CMGEv2i32,
      AArch64::CMGEv4i16,
      AArch64::CMGEv8i8,
      AArch64::CMGTv2i32,
      AArch64::CMGTv4i16,
      AArch64::CMGTv8i8,
      AArch64::CMHSv2i32,
      AArch64::CMHSv4i16,
      AArch64::CMHSv8i8,
      AArch64::CMLTv2i32rz,
      AArch64::CMLTv4i16rz,
      AArch64::CMLTv8i8rz,
      AArch64::CMTSTv8i8,
      AArch64::SSHLLv8i8_shift,
      AArch64::SSHLLv4i16_shift,
      AArch64::SSHLLv2i32_shift,
      AArch64::ZIP2v4i16,
      AArch64::ZIP2v2i32,
      AArch64::ZIP2v8i8,
      AArch64::ZIP1v4i16,
      AArch64::ZIP1v2i32,
      AArch64::ZIP1v8i8,
      AArch64::RBITv8i8,
      AArch64::BITv8i8,
      AArch64::MULv2i32_indexed,
      AArch64::MULv4i16_indexed,
      AArch64::MULv2i32,
      AArch64::MULv8i8,
      AArch64::MULv4i16,
      AArch64::UZP2v8i8,
      AArch64::UZP2v4i16,
      AArch64::UZP1v8i8,
      AArch64::UZP1v4i16,
      AArch64::USHRv8i8_shift,
      AArch64::USHRv4i16_shift,
      AArch64::USHRv2i32_shift,
      AArch64::SMULLv8i8_v8i16,
      AArch64::SMULLv2i32_v2i64,
      AArch64::SMULLv4i16_v4i32,
      AArch64::SMULLv4i16_indexed,
      AArch64::SMULLv2i32_indexed,
      AArch64::USRAv8i8_shift,
      AArch64::USRAv4i16_shift,
      AArch64::USRAv2i32_shift,
      AArch64::SMINv8i8,
      AArch64::SMINv4i16,
      AArch64::SMINv2i32,
      AArch64::SMAXv8i8,
      AArch64::SMAXv4i16,
      AArch64::SMAXv2i32,
      AArch64::UMINv8i8,
      AArch64::UMINv4i16,
      AArch64::UMINv2i32,
      AArch64::UMAXv8i8,
      AArch64::UMAXv4i16,
      AArch64::UMAXv2i32,
      AArch64::UMULLv4i16_indexed,
      AArch64::UMULLv2i32_indexed,
      AArch64::UMULLv8i8_v8i16,
      AArch64::UMULLv4i16_v4i32,
      AArch64::CLZv2i32,
      AArch64::CLZv4i16,
      AArch64::CLZv8i8,
      AArch64::ABSv1i64,
      AArch64::ABSv8i8,
      AArch64::ABSv4i16,
      AArch64::ABSv2i32,
      AArch64::MVNIv2s_msl,
      AArch64::MVNIv2i32,
      AArch64::MVNIv4i16,
      AArch64::SMINVv8i8v,
      AArch64::UMINVv8i8v,
      AArch64::SMAXVv8i8v,
      AArch64::UMAXVv8i8v,
      AArch64::SMINVv4i16v,
      AArch64::UMINVv4i16v,
      AArch64::SMAXVv4i16v,
      AArch64::UMAXVv4i16v,
      AArch64::ORRv8i8,
      AArch64::ORRv2i32,
      AArch64::ORRv4i16,
      AArch64::REV64v2i32,
      AArch64::REV64v4i16,
      AArch64::REV64v8i8,
      AArch64::TRN1v4i16,
      AArch64::TRN1v8i8,
      AArch64::TRN2v8i8,
      AArch64::TRN2v4i16,
      AArch64::UQSUBv8i8,
      AArch64::UQSUBv4i16,
      AArch64::UQSUBv2i32,
      AArch64::TBLv8i8One,
      AArch64::TBLv8i8Two,
      AArch64::TBLv8i8Three,
      AArch64::TBLv8i8Four,
      AArch64::MOVIv2s_msl,
      AArch64::MOVIv8b_ns,
      AArch64::MOVIv4i16,
      AArch64::EXTv8i8,
      AArch64::MLAv2i32_indexed,
      AArch64::MLAv4i16_indexed,
      AArch64::MLAv8i8,
      AArch64::MLAv2i32,
      AArch64::MLAv4i16,
      AArch64::ORNv8i8,
      AArch64::CMTSTv4i16,
      AArch64::CMTSTv2i32,
      AArch64::CMEQv1i64,
      AArch64::CMGTv4i16rz,
      AArch64::CMEQv1i64rz,
      AArch64::CMGTv8i8rz,
      AArch64::CMLEv4i16rz,
      AArch64::CMGEv4i16rz,
      AArch64::CMLEv8i8rz,
      AArch64::CMLTv1i64rz,
      AArch64::CMGTv2i32rz,
      AArch64::CMGEv8i8rz,
      AArch64::CMLEv2i32rz,
      AArch64::CMGEv2i32rz,
      AArch64::UQADDv1i64,
      AArch64::UQADDv8i8,
      AArch64::UQADDv4i16,
      AArch64::UQADDv2i32,
      AArch64::REV16v8i8,
      AArch64::REV32v4i16,
      AArch64::REV32v8i8,
      AArch64::MLSv2i32_indexed,
      AArch64::MLSv4i16_indexed,
      AArch64::MLSv8i8,
      AArch64::MLSv4i16,
      AArch64::SQADDv1i64,
      AArch64::SQADDv8i8,
      AArch64::SQADDv4i16,
      AArch64::SQADDv2i32,
      AArch64::SQSUBv1i64,
      AArch64::SQSUBv8i8,
      AArch64::SQSUBv4i16,
      AArch64::SQSUBv2i32,
      AArch64::ADDv1i64,
      AArch64::SHADDv8i8,
      AArch64::SHADDv4i16,
      AArch64::SHADDv2i32,
      AArch64::SHSUBv8i8,
      AArch64::SHSUBv4i16,
      AArch64::SHSUBv2i32,
      AArch64::UADDLv8i8_v8i16,
      AArch64::UADDLv4i16_v4i32,
      AArch64::UADDLv2i32_v2i64,
      AArch64::UADDWv8i8_v8i16,
      AArch64::UADDWv4i16_v4i32,
      AArch64::UADDWv2i32_v2i64,
      AArch64::SADDLv8i8_v8i16,
      AArch64::SADDLv4i16_v4i32,
      AArch64::SADDLv2i32_v2i64,
      AArch64::SADDWv8i8_v8i16,
      AArch64::SADDWv4i16_v4i32,
      AArch64::SADDWv2i32_v2i64,
      AArch64::USUBLv8i8_v8i16,
      AArch64::USUBLv4i16_v4i32,
      AArch64::USUBLv2i32_v2i64,
      AArch64::USUBWv8i8_v8i16,
      AArch64::USUBWv4i16_v4i32,
      AArch64::USUBWv2i32_v2i64,
      AArch64::SSUBLv8i8_v8i16,
      AArch64::SSUBLv4i16_v4i32,
      AArch64::SSUBLv2i32_v2i64,
      AArch64::SSUBWv8i8_v8i16,
      AArch64::SSUBWv4i16_v4i32,
      AArch64::SSUBWv2i32_v2i64,
      AArch64::FNEGv2f32,
      AArch64::FNEGDr,
      AArch64::UCVTFUXSri,
      AArch64::UCVTFUXDri,
      AArch64::SCVTFUXSri,
      AArch64::SCVTFUXDri,
      AArch64::SCVTFv1i64,
  };

  const set<int> instrs_128 = {
      AArch64::DUPv8i8lane,
      AArch64::DUPv4i16lane,
      AArch64::DUPv2i32lane,
      AArch64::UADDLv16i8_v8i16,
      AArch64::UADDLv8i16_v4i32,
      AArch64::UADDLv4i32_v2i64,
      AArch64::UADDWv16i8_v8i16,
      AArch64::UADDWv8i16_v4i32,
      AArch64::UADDWv4i32_v2i64,
      AArch64::SADDLv16i8_v8i16,
      AArch64::SADDLv8i16_v4i32,
      AArch64::SADDLv4i32_v2i64,
      AArch64::SADDWv16i8_v8i16,
      AArch64::SADDWv8i16_v4i32,
      AArch64::SADDWv4i32_v2i64,
      AArch64::USUBLv16i8_v8i16,
      AArch64::USUBLv8i16_v4i32,
      AArch64::USUBLv4i32_v2i64,
      AArch64::USUBWv16i8_v8i16,
      AArch64::USUBWv8i16_v4i32,
      AArch64::USUBWv4i32_v2i64,
      AArch64::SSUBLv16i8_v8i16,
      AArch64::SSUBLv8i16_v4i32,
      AArch64::SSUBLv4i32_v2i64,
      AArch64::SSUBWv16i8_v8i16,
      AArch64::SSUBWv8i16_v4i32,
      AArch64::SSUBWv4i32_v2i64,
      AArch64::UMLALv4i16_indexed,
      AArch64::UMLALv8i16_indexed,
      AArch64::UMLALv2i32_indexed,
      AArch64::UMLALv4i32_indexed,
      AArch64::UMLALv8i8_v8i16,
      AArch64::UMLALv4i16_v4i32,
      AArch64::UMLALv2i32_v2i64,
      AArch64::UMLALv16i8_v8i16,
      AArch64::UMLALv8i16_v4i32,
      AArch64::UMLALv4i32_v2i64,
      AArch64::UMLSLv4i16_indexed,
      AArch64::UMLSLv8i16_indexed,
      AArch64::UMLSLv2i32_indexed,
      AArch64::UMLSLv4i32_indexed,
      AArch64::UMLSLv8i8_v8i16,
      AArch64::UMLSLv4i16_v4i32,
      AArch64::UMLSLv2i32_v2i64,
      AArch64::UMLSLv16i8_v8i16,
      AArch64::UMLSLv8i16_v4i32,
      AArch64::UMLSLv4i32_v2i64,
      AArch64::SMLALv4i16_indexed,
      AArch64::SMLALv8i16_indexed,
      AArch64::SMLALv2i32_indexed,
      AArch64::SMLALv4i32_indexed,
      AArch64::SMLALv8i8_v8i16,
      AArch64::SMLALv4i16_v4i32,
      AArch64::SMLALv2i32_v2i64,
      AArch64::SMLALv16i8_v8i16,
      AArch64::SMLALv8i16_v4i32,
      AArch64::SMLALv4i32_v2i64,
      AArch64::SMLSLv4i16_indexed,
      AArch64::SMLSLv8i16_indexed,
      AArch64::SMLSLv2i32_indexed,
      AArch64::SMLSLv4i32_indexed,
      AArch64::SMLSLv8i8_v8i16,
      AArch64::SMLSLv4i16_v4i32,
      AArch64::SMLSLv2i32_v2i64,
      AArch64::SMLSLv16i8_v8i16,
      AArch64::SMLSLv8i16_v4i32,
      AArch64::SMLSLv4i32_v2i64,
      AArch64::SQADDv2i64,
      AArch64::SQADDv4i32,
      AArch64::SQADDv16i8,
      AArch64::SQADDv8i16,
      AArch64::SQSUBv2i64,
      AArch64::SQSUBv4i32,
      AArch64::SQSUBv16i8,
      AArch64::SQSUBv8i16,
      AArch64::MLSv16i8,
      AArch64::MLSv8i16,
      AArch64::MLSv4i32,
      AArch64::MLSv4i32_indexed,
      AArch64::MLSv8i16_indexed,
      AArch64::REV16v16i8,
      AArch64::REV32v8i16,
      AArch64::REV32v16i8,
      AArch64::UQADDv2i64,
      AArch64::UQADDv4i32,
      AArch64::UQADDv16i8,
      AArch64::UQADDv8i16,
      AArch64::CMLEv8i16rz,
      AArch64::CMLEv16i8rz,
      AArch64::CMGTv16i8rz,
      AArch64::CMGTv8i16rz,
      AArch64::CMGTv2i64rz,
      AArch64::CMGEv8i16rz,
      AArch64::CMLEv4i32rz,
      AArch64::CMGEv2i64rz,
      AArch64::CMLEv2i64rz,
      AArch64::CMGEv16i8rz,
      AArch64::CMGEv4i32rz,
      AArch64::ORNv16i8,
      AArch64::MLAv8i16_indexed,
      AArch64::MLAv4i32_indexed,
      AArch64::MLAv16i8,
      AArch64::MLAv8i16,
      AArch64::MLAv4i32,
      AArch64::SHRNv2i32_shift,
      AArch64::SHRNv4i16_shift,
      AArch64::SHRNv8i8_shift,
      AArch64::SHRNv4i32_shift,
      AArch64::SHRNv8i16_shift,
      AArch64::SHRNv16i8_shift,
      AArch64::MOVIv4s_msl,
      AArch64::TBLv16i8One,
      AArch64::TBLv16i8Two,
      AArch64::TBLv16i8Three,
      AArch64::TBLv16i8Four,
      AArch64::UQSUBv2i64,
      AArch64::UQSUBv4i32,
      AArch64::UQSUBv16i8,
      AArch64::UQSUBv8i16,
      AArch64::TRN1v16i8,
      AArch64::TRN1v8i16,
      AArch64::TRN1v4i32,
      AArch64::TRN2v16i8,
      AArch64::TRN2v8i16,
      AArch64::TRN2v4i32,
      AArch64::REV64v8i16,
      AArch64::REV64v16i8,
      AArch64::ORRv16i8,
      AArch64::ORRv8i16,
      AArch64::ORRv4i32,
      AArch64::SMAXVv4i32v,
      AArch64::UMAXVv4i32v,
      AArch64::SMINVv4i32v,
      AArch64::UMINVv4i32v,
      AArch64::UMINVv8i16v,
      AArch64::SMINVv8i16v,
      AArch64::UMAXVv8i16v,
      AArch64::SMAXVv8i16v,
      AArch64::SMINVv16i8v,
      AArch64::UMINVv16i8v,
      AArch64::SMAXVv16i8v,
      AArch64::UMAXVv16i8v,
      AArch64::MVNIv4s_msl,
      AArch64::MVNIv8i16,
      AArch64::MVNIv4i32,
      AArch64::ABSv2i64,
      AArch64::ABSv16i8,
      AArch64::ABSv8i16,
      AArch64::ABSv4i32,
      AArch64::CLZv16i8,
      AArch64::CLZv8i16,
      AArch64::CLZv4i32,
      AArch64::UMULLv16i8_v8i16,
      AArch64::UMULLv8i16_v4i32,
      AArch64::UMULLv4i32_v2i64,
      AArch64::UMULLv8i16_indexed,
      AArch64::UMULLv4i32_indexed,
      AArch64::SMINv16i8,
      AArch64::SMINv8i16,
      AArch64::SMINv4i32,
      AArch64::SMAXv16i8,
      AArch64::SMAXv8i16,
      AArch64::SMAXv4i32,
      AArch64::UMINv16i8,
      AArch64::UMINv8i16,
      AArch64::UMINv4i32,
      AArch64::UMAXv16i8,
      AArch64::UMAXv8i16,
      AArch64::UMAXv4i32,
      AArch64::USRAv16i8_shift,
      AArch64::USRAv8i16_shift,
      AArch64::USRAv2i64_shift,
      AArch64::USRAv4i32_shift,
      AArch64::SMULLv8i16_v4i32,
      AArch64::SMULLv16i8_v8i16,
      AArch64::SMULLv4i32_v2i64,
      AArch64::SMULLv4i32_indexed,
      AArch64::SMULLv8i16_indexed,
      AArch64::USHRv16i8_shift,
      AArch64::USHRv8i16_shift,
      AArch64::USHRv4i32_shift,
      AArch64::UZP2v8i16,
      AArch64::UZP2v16i8,
      AArch64::UZP1v16i8,
      AArch64::UZP2v4i32,
      AArch64::UZP1v8i16,
      AArch64::UZP1v4i32,
      AArch64::MULv8i16_indexed,
      AArch64::MULv4i32_indexed,
      AArch64::MULv16i8,
      AArch64::MULv8i16,
      AArch64::MULv4i32,
      AArch64::RBITv16i8,
      AArch64::BITv16i8,
      AArch64::ZIP2v8i16,
      AArch64::ZIP2v2i64,
      AArch64::ZIP2v16i8,
      AArch64::ZIP2v4i32,
      AArch64::ZIP1v8i16,
      AArch64::ZIP1v16i8,
      AArch64::ZIP1v2i64,
      AArch64::ZIP1v4i32,
      AArch64::SSHLLv4i32_shift,
      AArch64::SSHLLv8i16_shift,
      AArch64::SSHLLv16i8_shift,
      AArch64::ADDPv16i8,
      AArch64::ADDPv4i32,
      AArch64::ADDPv8i16,
      AArch64::ADDPv2i64,
      AArch64::ADDPv2i64p,
      AArch64::SSHRv16i8_shift,
      AArch64::SSHRv2i64_shift,
      AArch64::SSHRv8i16_shift,
      AArch64::SSHRv4i32_shift,
      AArch64::SHLv16i8_shift,
      AArch64::SHLv8i16_shift,
      AArch64::SHLv4i32_shift,
      AArch64::SHLv2i64_shift,
      AArch64::USHLLv8i16_shift,
      AArch64::USHLLv16i8_shift,
      AArch64::USHLLv4i32_shift,
      AArch64::DUPi16,
      AArch64::DUPi64,
      AArch64::DUPi32,
      AArch64::FMOVXDr,
      AArch64::LDPQi,
      AArch64::LDPQpre,
      AArch64::LDPQpost,
      AArch64::LDRQroX,
      AArch64::LDURQi,
      AArch64::LD1i8,
      AArch64::LD1i16,
      AArch64::LD1i32,
      AArch64::LD1i64,
      AArch64::LD1i8_POST,
      AArch64::LD1i16_POST,
      AArch64::LD1i32_POST,
      AArch64::LD1i64_POST,
      AArch64::LD1Rv8b,
      AArch64::LD1Rv16b,
      AArch64::LD1Rv4h,
      AArch64::LD1Rv8h,
      AArch64::LD1Rv2s,
      AArch64::LD1Rv4s,
      AArch64::LD1Rv1d,
      AArch64::LD1Rv2d,
      AArch64::LD1Onev8b,
      AArch64::LD1Onev16b,
      AArch64::LD1Onev4h,
      AArch64::LD1Onev8h,
      AArch64::LD1Onev2s,
      AArch64::LD1Onev4s,
      AArch64::LD1Onev1d,
      AArch64::LD1Onev2d,
      AArch64::LD1Onev8b_POST,
      AArch64::LD1Onev16b_POST,
      AArch64::LD1Onev4h_POST,
      AArch64::LD1Onev8h_POST,
      AArch64::LD1Onev2s_POST,
      AArch64::LD1Onev4s_POST,
      AArch64::LD1Onev1d_POST,
      AArch64::LD1Onev2d_POST,
      AArch64::LD1Twov8b,
      AArch64::LD1Twov16b,
      AArch64::LD1Twov4h,
      AArch64::LD1Twov8h,
      AArch64::LD1Twov2s,
      AArch64::LD1Twov4s,
      AArch64::LD1Twov1d,
      AArch64::LD1Twov2d,
      AArch64::LD1Twov8b_POST,
      AArch64::LD1Twov16b_POST,
      AArch64::LD1Twov4h_POST,
      AArch64::LD1Twov8h_POST,
      AArch64::LD1Twov2s_POST,
      AArch64::LD1Twov4s_POST,
      AArch64::LD1Twov1d_POST,
      AArch64::LD1Twov2d_POST,
      AArch64::LD1Threev8b,
      AArch64::LD1Threev16b,
      AArch64::LD1Threev4h,
      AArch64::LD1Threev8h,
      AArch64::LD1Threev2s,
      AArch64::LD1Threev4s,
      AArch64::LD1Threev1d,
      AArch64::LD1Threev2d,
      AArch64::LD1Threev8b_POST,
      AArch64::LD1Threev16b_POST,
      AArch64::LD1Threev4h_POST,
      AArch64::LD1Threev8h_POST,
      AArch64::LD1Threev2s_POST,
      AArch64::LD1Threev4s_POST,
      AArch64::LD1Threev1d_POST,
      AArch64::LD1Threev2d_POST,
      AArch64::LD1Fourv8b,
      AArch64::LD1Fourv16b,
      AArch64::LD1Fourv4h,
      AArch64::LD1Fourv8h,
      AArch64::LD1Fourv2s,
      AArch64::LD1Fourv4s,
      AArch64::LD1Fourv1d,
      AArch64::LD1Fourv2d,
      AArch64::LD1Fourv8b_POST,
      AArch64::LD1Fourv16b_POST,
      AArch64::LD1Fourv4h_POST,
      AArch64::LD1Fourv8h_POST,
      AArch64::LD1Fourv2s_POST,
      AArch64::LD1Fourv4s_POST,
      AArch64::LD1Fourv1d_POST,
      AArch64::LD1Fourv2d_POST,
      AArch64::LD2Twov8b,
      AArch64::LD2Twov16b,
      AArch64::LD2Twov4h,
      AArch64::LD2Twov8h,
      AArch64::LD2Twov2s,
      AArch64::LD2Twov4s,
      AArch64::LD2Twov2d,
      AArch64::LD2Twov8b_POST,
      AArch64::LD2Twov16b_POST,
      AArch64::LD2Twov4h_POST,
      AArch64::LD2Twov8h_POST,
      AArch64::LD2Twov2s_POST,
      AArch64::LD2Twov4s_POST,
      AArch64::LD2Twov2d_POST,
      AArch64::LD3Threev8b,
      AArch64::LD3Threev16b,
      AArch64::LD3Threev4h,
      AArch64::LD3Threev8h,
      AArch64::LD3Threev2s,
      AArch64::LD3Threev4s,
      AArch64::LD3Threev2d,
      AArch64::LD3Threev8b_POST,
      AArch64::LD3Threev16b_POST,
      AArch64::LD3Threev4h_POST,
      AArch64::LD3Threev8h_POST,
      AArch64::LD3Threev2s_POST,
      AArch64::LD3Threev4s_POST,
      AArch64::LD3Threev2d_POST,
      AArch64::LD4Fourv8b,
      AArch64::LD4Fourv16b,
      AArch64::LD4Fourv4h,
      AArch64::LD4Fourv8h,
      AArch64::LD4Fourv2s,
      AArch64::LD4Fourv4s,
      AArch64::LD4Fourv2d,
      AArch64::LD4Fourv8b_POST,
      AArch64::LD4Fourv16b_POST,
      AArch64::LD4Fourv4h_POST,
      AArch64::LD4Fourv8h_POST,
      AArch64::LD4Fourv2s_POST,
      AArch64::LD4Fourv4s_POST,
      AArch64::LD4Fourv2d_POST,
      AArch64::STPQi,
      AArch64::STPQpre,
      AArch64::STPQpost,
      AArch64::STRQroX,
      AArch64::ST1i8,
      AArch64::ST1i16,
      AArch64::ST1i32,
      AArch64::ST1i64,
      AArch64::ST1i8_POST,
      AArch64::ST1i16_POST,
      AArch64::ST1i32_POST,
      AArch64::ST1i64_POST,
      AArch64::ST1Onev8b,
      AArch64::ST1Onev16b,
      AArch64::ST1Onev4h,
      AArch64::ST1Onev8h,
      AArch64::ST1Onev2s,
      AArch64::ST1Onev4s,
      AArch64::ST1Onev1d,
      AArch64::ST1Onev2d,
      AArch64::ST1Onev8b_POST,
      AArch64::ST1Onev16b_POST,
      AArch64::ST1Onev4h_POST,
      AArch64::ST1Onev8h_POST,
      AArch64::ST1Onev2s_POST,
      AArch64::ST1Onev4s_POST,
      AArch64::ST1Onev1d_POST,
      AArch64::ST1Onev2d_POST,
      AArch64::ST1Twov8b,
      AArch64::ST1Twov16b,
      AArch64::ST1Twov4h,
      AArch64::ST1Twov8h,
      AArch64::ST1Twov2s,
      AArch64::ST1Twov4s,
      AArch64::ST1Twov1d,
      AArch64::ST1Twov2d,
      AArch64::ST1Twov8b_POST,
      AArch64::ST1Twov16b_POST,
      AArch64::ST1Twov4h_POST,
      AArch64::ST1Twov8h_POST,
      AArch64::ST1Twov2s_POST,
      AArch64::ST1Twov4s_POST,
      AArch64::ST1Twov1d_POST,
      AArch64::ST1Twov2d_POST,
      AArch64::ST1Threev8b,
      AArch64::ST1Threev16b,
      AArch64::ST1Threev4h,
      AArch64::ST1Threev8h,
      AArch64::ST1Threev2s,
      AArch64::ST1Threev4s,
      AArch64::ST1Threev1d,
      AArch64::ST1Threev2d,
      AArch64::ST1Threev8b_POST,
      AArch64::ST1Threev16b_POST,
      AArch64::ST1Threev4h_POST,
      AArch64::ST1Threev8h_POST,
      AArch64::ST1Threev2s_POST,
      AArch64::ST1Threev4s_POST,
      AArch64::ST1Threev1d_POST,
      AArch64::ST1Threev2d_POST,
      AArch64::ST1Fourv8b,
      AArch64::ST1Fourv16b,
      AArch64::ST1Fourv4h,
      AArch64::ST1Fourv8h,
      AArch64::ST1Fourv2s,
      AArch64::ST1Fourv4s,
      AArch64::ST1Fourv1d,
      AArch64::ST1Fourv2d,
      AArch64::ST1Fourv8b_POST,
      AArch64::ST1Fourv16b_POST,
      AArch64::ST1Fourv4h_POST,
      AArch64::ST1Fourv8h_POST,
      AArch64::ST1Fourv2s_POST,
      AArch64::ST1Fourv4s_POST,
      AArch64::ST1Fourv1d_POST,
      AArch64::ST1Fourv2d_POST,
      AArch64::ST2Twov8b,
      AArch64::ST2Twov16b,
      AArch64::ST2Twov4h,
      AArch64::ST2Twov8h,
      AArch64::ST2Twov2s,
      AArch64::ST2Twov4s,
      AArch64::ST2Twov2d,
      AArch64::ST2Twov8b_POST,
      AArch64::ST2Twov16b_POST,
      AArch64::ST2Twov4h_POST,
      AArch64::ST2Twov8h_POST,
      AArch64::ST2Twov2s_POST,
      AArch64::ST2Twov4s_POST,
      AArch64::ST2Twov2d_POST,
      AArch64::ST3Threev8b,
      AArch64::ST3Threev16b,
      AArch64::ST3Threev4h,
      AArch64::ST3Threev8h,
      AArch64::ST3Threev2s,
      AArch64::ST3Threev4s,
      AArch64::ST3Threev2d,
      AArch64::ST3Threev8b_POST,
      AArch64::ST3Threev16b_POST,
      AArch64::ST3Threev4h_POST,
      AArch64::ST3Threev8h_POST,
      AArch64::ST3Threev2s_POST,
      AArch64::ST3Threev4s_POST,
      AArch64::ST3Threev2d_POST,
      AArch64::ST4Fourv8b,
      AArch64::ST4Fourv16b,
      AArch64::ST4Fourv4h,
      AArch64::ST4Fourv8h,
      AArch64::ST4Fourv2s,
      AArch64::ST4Fourv4s,
      AArch64::ST4Fourv2d,
      AArch64::ST4Fourv8b_POST,
      AArch64::ST4Fourv16b_POST,
      AArch64::ST4Fourv4h_POST,
      AArch64::ST4Fourv8h_POST,
      AArch64::ST4Fourv2s_POST,
      AArch64::ST4Fourv4s_POST,
      AArch64::ST4Fourv2d_POST,
      AArch64::ADDv8i16,
      AArch64::ADDv2i64,
      AArch64::ADDv4i32,
      AArch64::ADDv16i8,
      AArch64::SUBv8i16,
      AArch64::SUBv2i64,
      AArch64::SUBv4i32,
      AArch64::SUBv16i8,
      AArch64::UABDv16i8,
      AArch64::UABDv8i16,
      AArch64::UABDv4i32,
      AArch64::SABDv16i8,
      AArch64::SABDv8i16,
      AArch64::SABDv4i32,
      AArch64::LDRQui,
      AArch64::LDRQpre,
      AArch64::LDRQpost,
      AArch64::STURQi,
      AArch64::STRQui,
      AArch64::STRQpre,
      AArch64::STRQpost,
      AArch64::FMOVDi,
      AArch64::FMOVSi,
      AArch64::FMOVWSr,
      AArch64::FMOVv4f32_ns,
      AArch64::FMOVv2f64_ns,
      AArch64::CNTv16i8,
      AArch64::MOVIv2d_ns,
      AArch64::MOVIv4i32,
      AArch64::EXTv16i8,
      AArch64::MOVIv2i32,
      AArch64::ANDv16i8,
      AArch64::EORv16i8,
      AArch64::UMOVvi32,
      AArch64::UMOVvi8,
      AArch64::UMOVvi8_idx0,
      AArch64::MOVIv16b_ns,
      AArch64::UMOVvi64,
      AArch64::UMOVvi16,
      AArch64::UMOVvi16_idx0,
      AArch64::SMOVvi8to32_idx0,
      AArch64::SMOVvi8to32,
      AArch64::SMOVvi16to32_idx0,
      AArch64::SMOVvi16to32,
      AArch64::SMOVvi8to64_idx0,
      AArch64::SMOVvi8to64,
      AArch64::SMOVvi16to64_idx0,
      AArch64::SMOVvi16to64,
      AArch64::SMOVvi32to64_idx0,
      AArch64::SMOVvi32to64,
      AArch64::INSvi64lane,
      AArch64::INSvi8lane,
      AArch64::INSvi16lane,
      AArch64::INSvi32lane,
      AArch64::DUPv16i8gpr,
      AArch64::DUPv8i16gpr,
      AArch64::DUPv4i32gpr,
      AArch64::DUPv2i64gpr,
      AArch64::DUPv16i8lane,
      AArch64::DUPv8i16lane,
      AArch64::DUPv4i32lane,
      AArch64::DUPv2i64lane,
      AArch64::MOVIv8i16,
      AArch64::REV64v4i32,
      AArch64::USHRv2i64_shift,
      AArch64::NOTv16i8,
      AArch64::NEGv16i8,
      AArch64::NEGv8i16,
      AArch64::NEGv2i64,
      AArch64::NEGv4i32,
      AArch64::USHLv16i8,
      AArch64::USHLv8i16,
      AArch64::USHLv4i32,
      AArch64::USHLv2i64,
      AArch64::SSHLv16i8,
      AArch64::SSHLv8i16,
      AArch64::SSHLv4i32,
      AArch64::SSHLv2i64,
      AArch64::SHADDv16i8,
      AArch64::SHADDv8i16,
      AArch64::SHADDv4i32,
      AArch64::SHSUBv16i8,
      AArch64::SHSUBv8i16,
      AArch64::SHSUBv4i32,
      AArch64::UADDLVv8i16v,
      AArch64::UADDLVv4i32v,
      AArch64::UADDLVv16i8v,
      AArch64::SADDLVv8i16v,
      AArch64::SADDLVv4i32v,
      AArch64::SADDLVv16i8v,
      AArch64::UADDLPv8i16_v4i32,
      AArch64::UADDLPv4i32_v2i64,
      AArch64::UADDLPv16i8_v8i16,
      AArch64::SADDLPv8i16_v4i32,
      AArch64::SADDLPv4i32_v2i64,
      AArch64::SADDLPv16i8_v8i16,
      AArch64::UABDLv16i8_v8i16,
      AArch64::UABDLv8i16_v4i32,
      AArch64::UABDLv4i32_v2i64,
      AArch64::SABDLv16i8_v8i16,
      AArch64::SABDLv8i16_v4i32,
      AArch64::SABDLv4i32_v2i64,
      AArch64::UADALPv4i32_v2i64,
      AArch64::UADALPv16i8_v8i16,
      AArch64::UADALPv8i16_v4i32,
      AArch64::SADALPv4i32_v2i64,
      AArch64::SADALPv16i8_v8i16,
      AArch64::SADALPv8i16_v4i32,
      AArch64::BIFv16i8,
      AArch64::BSLv16i8,
      AArch64::BICv8i16,
      AArch64::BICv4i32,
      AArch64::BICv16i8,
      AArch64::ADDVv16i8v,
      AArch64::ADDVv8i16v,
      AArch64::ADDVv4i32v,
      AArch64::CMHIv16i8,
      AArch64::CMHIv8i16,
      AArch64::CMHIv4i32,
      AArch64::CMHIv2i64,
      AArch64::CMEQv16i8,
      AArch64::CMEQv16i8rz,
      AArch64::CMEQv2i64,
      AArch64::CMEQv2i64rz,
      AArch64::CMEQv4i32,
      AArch64::CMEQv4i32rz,
      AArch64::CMEQv8i16,
      AArch64::CMEQv8i16rz,
      AArch64::CMGEv16i8,
      AArch64::CMGEv2i64,
      AArch64::CMGEv4i32,
      AArch64::CMGEv8i16,
      AArch64::CMGTv16i8,
      AArch64::CMGTv2i64,
      AArch64::CMGTv4i32,
      AArch64::CMGTv4i32rz,
      AArch64::CMGTv8i16,
      AArch64::CMHSv16i8,
      AArch64::CMHSv2i64,
      AArch64::CMHSv4i32,
      AArch64::CMHSv8i16,
      AArch64::CMLTv16i8rz,
      AArch64::CMLTv2i64rz,
      AArch64::CMLTv4i32rz,
      AArch64::CMLTv8i16rz,
      AArch64::CMTSTv16i8,
      AArch64::CMTSTv2i64,
      AArch64::CMTSTv4i32,
      AArch64::CMTSTv8i16,
      AArch64::CMHIv16i8,
      AArch64::CMHIv8i16,
      AArch64::CMHIv4i32,
      AArch64::CMHIv2i64,
      AArch64::XTNv16i8,
      AArch64::XTNv8i16,
      AArch64::XTNv4i32,
      AArch64::FNEGv4f32,
      AArch64::FNEGv2f64,
  };

  bool has_s(int instr) {
    return s_flag.contains(instr);
  }

  // decodeBitMasks - Decode a logical immediate value in the form
  // "N:immr:imms" (where the immr and imms fields are each 6 bits) into the
  // integer value it represents with regSize bits. Implementation of the
  // DecodeBitMasks function from the ARMv8 manual.
  //
  // WARNING: tmask is untested
  pair<uint64_t, uint64_t> decodeBitMasks(uint64_t val, unsigned regSize) {
    // Extract the N, imms, and immr fields.
    unsigned N = (val >> 12) & 1;
    unsigned immr = (val >> 6) & 0x3f;
    unsigned imms = val & 0x3f;

    assert((regSize == 64 || N == 0) && "undefined logical immediate encoding");
    int len = 31 - llvm::countl_zero((N << 6) | (~imms & 0x3f));
    assert(len >= 0 && len <= 6 && "undefined logical immediate encoding");
    unsigned size = (1 << len);
    unsigned R = immr & (size - 1);
    unsigned S = imms & (size - 1);
    unsigned d = ((S - R) & (size - 1));
    assert(S != size - 1 && d <= size - 1 &&
           "undefined logical immediate encoding");
    uint64_t wmask = (1ULL << (S + 1)) - 1;
    uint64_t tmask = (1ULL << min(d + 1, (unsigned)63)) - 1;
    // Rotate wmask right R times to get wmask
    for (unsigned i = 0; i < R; ++i)
      wmask = ((wmask & 1) << (size - 1)) | (wmask >> 1);

    // Replicate the wmask to fill the regSize.
    while (size != regSize) {
      wmask |= (wmask << size);
      tmask |= (tmask << size);
      size *= 2;
    }

    return make_pair(wmask, tmask);
  }

  unsigned getInstSize(int instr) {
    if (instrs_32.contains(instr))
      return 32;
    if (instrs_64.contains(instr))
      return 64;
    if (instrs_128.contains(instr))
      return 128;
    *out << "getInstSize encountered unknown instruction\n";
    visitError();
  }

  // from getShiftType/getShiftValue:
  // https://github.com/llvm/llvm-project/blob/93d1a623cecb6f732db7900baf230a13e6ac6c6a/llvm/lib/Target/AArch64/MCTargetDesc/AArch64AddressingModes.h#L74
  Value *regShift(Value *value, int encodedShift) {
    if (encodedShift == 0)
      return value;

    int shift_type = (encodedShift >> 6) & 0x7;
    auto W = getBitWidth(value);
    auto exp = getIntConst(encodedShift & 0x3f, W);

    switch (shift_type) {
    case 0:
      return createMaskedShl(value, exp);
    case 1:
      return createMaskedLShr(value, exp);
    case 2:
      return createMaskedAShr(value, exp);
    case 3:
      // ROR shift
      return createFShr(value, value, exp);
    default:
      // FIXME: handle other case (msl)
      *out << "\nERROR: shift type not supported\n\n";
      exit(-1);
    }
  }

  // lifted instructions are named using the number of the ARM
  // instruction they come from
  string nextName() {
    stringstream ss;
    ss << "a" << armInstNum << "_" << llvmInstNum++;
    return ss.str();
  }

  AllocaInst *createAlloca(Type *ty, Value *sz, const string &NameStr) {
    return new AllocaInst(ty, 0, sz, NameStr, LLVMBB);
  }

  GetElementPtrInst *createGEP(Type *ty, Value *v, ArrayRef<Value *> idxlist,
                               const string &NameStr) {
    return GetElementPtrInst::Create(ty, v, idxlist, NameStr, LLVMBB);
  }

  void createBranch(Value *c, BasicBlock *t, BasicBlock *f) {
    BranchInst::Create(t, f, c, LLVMBB);
  }

  void createBranch(BasicBlock *dst) {
    BranchInst::Create(dst, LLVMBB);
  }

  LoadInst *createLoad(Type *ty, Value *ptr) {
    return new LoadInst(ty, ptr, nextName(), false, Align(1), LLVMBB);
  }

  void createStore(Value *v, Value *ptr) {
    new StoreInst(v, ptr, false, Align(1), LLVMBB);
  }

  Value *createTrap() {
    auto decl = Intrinsic::getDeclaration(LiftedModule, Intrinsic::trap);
    return CallInst::Create(decl, "", LLVMBB);
  }

  Value *createSMin(Value *a, Value *b) {
    auto decl =
        Intrinsic::getDeclaration(LiftedModule, Intrinsic::smin, a->getType());
    return CallInst::Create(decl, {a, b}, nextName(), LLVMBB);
  }

  Value *createSMax(Value *a, Value *b) {
    auto decl =
        Intrinsic::getDeclaration(LiftedModule, Intrinsic::smax, a->getType());
    return CallInst::Create(decl, {a, b}, nextName(), LLVMBB);
  }

  Value *createUMin(Value *a, Value *b) {
    auto decl =
        Intrinsic::getDeclaration(LiftedModule, Intrinsic::umin, a->getType());
    return CallInst::Create(decl, {a, b}, nextName(), LLVMBB);
  }

  Value *createUMax(Value *a, Value *b) {
    auto decl =
        Intrinsic::getDeclaration(LiftedModule, Intrinsic::umax, a->getType());
    return CallInst::Create(decl, {a, b}, nextName(), LLVMBB);
  }

  Value *createFNeg(Value *v) {
    return UnaryOperator::CreateFNeg(v, nextName(), LLVMBB);
  }

  Value *createFAbs(Value *v) {
    auto fabs_decl =
        Intrinsic::getDeclaration(LiftedModule, Intrinsic::fabs, v->getType());
    return CallInst::Create(fabs_decl, {v}, nextName(), LLVMBB);
  }

  CallInst *createSSubOverflow(Value *a, Value *b) {
    auto ssub_decl = Intrinsic::getDeclaration(
        LiftedModule, Intrinsic::ssub_with_overflow, a->getType());
    return CallInst::Create(ssub_decl, {a, b}, nextName(), LLVMBB);
  }

  CallInst *createSAddOverflow(Value *a, Value *b) {
    auto sadd_decl = Intrinsic::getDeclaration(
        LiftedModule, Intrinsic::sadd_with_overflow, a->getType());
    return CallInst::Create(sadd_decl, {a, b}, nextName(), LLVMBB);
  }

  CallInst *createUSubOverflow(Value *a, Value *b) {
    auto usub_decl = Intrinsic::getDeclaration(
        LiftedModule, Intrinsic::usub_with_overflow, a->getType());
    return CallInst::Create(usub_decl, {a, b}, nextName(), LLVMBB);
  }

  CallInst *createUAddOverflow(Value *a, Value *b) {
    auto uadd_decl = Intrinsic::getDeclaration(
        LiftedModule, Intrinsic::uadd_with_overflow, a->getType());
    return CallInst::Create(uadd_decl, {a, b}, nextName(), LLVMBB);
  }

  CallInst *createUAddSat(Value *a, Value *b) {
    auto uadd_decl = Intrinsic::getDeclaration(
        LiftedModule, Intrinsic::uadd_sat, a->getType());
    return CallInst::Create(uadd_decl, {a, b}, nextName(), LLVMBB);
  }

  CallInst *createUSubSat(Value *a, Value *b) {
    auto usub_decl = Intrinsic::getDeclaration(
        LiftedModule, Intrinsic::usub_sat, a->getType());
    return CallInst::Create(usub_decl, {a, b}, nextName(), LLVMBB);
  }

  CallInst *createSAddSat(Value *a, Value *b) {
    auto sadd_decl = Intrinsic::getDeclaration(
        LiftedModule, Intrinsic::sadd_sat, a->getType());
    return CallInst::Create(sadd_decl, {a, b}, nextName(), LLVMBB);
  }

  CallInst *createSSubSat(Value *a, Value *b) {
    auto ssub_decl = Intrinsic::getDeclaration(
        LiftedModule, Intrinsic::ssub_sat, a->getType());
    return CallInst::Create(ssub_decl, {a, b}, nextName(), LLVMBB);
  }

  CallInst *createCtPop(Value *v) {
    auto decl =
        Intrinsic::getDeclaration(LiftedModule, Intrinsic::ctpop, v->getType());
    return CallInst::Create(decl, {v}, nextName(), LLVMBB);
  }

  // first argument is an i16
  CallInst *createConvertFromFP16(Value *v, Type *ty) {
    auto cvt_decl = Intrinsic::getDeclaration(LiftedModule,
                                              Intrinsic::convert_from_fp16, ty);
    return CallInst::Create(cvt_decl, {v}, nextName(), LLVMBB);
  }

  CastInst *createPtrToInt(Value *v, Type *ty) {
    return new PtrToIntInst(v, ty, nextName(), LLVMBB);
  }

  InsertElementInst *createInsertElement(Value *vec, Value *val, int idx) {
    auto idxv = getIntConst(idx, 32);
    return InsertElementInst::Create(vec, val, idxv, nextName(), LLVMBB);
  }

  ExtractElementInst *createExtractElement(Value *v, Value *idx) {
    return ExtractElementInst::Create(v, idx, nextName(), LLVMBB);
  }

  ExtractElementInst *createExtractElement(Value *v, int idx) {
    auto idxv = getIntConst(idx, 32);
    return ExtractElementInst::Create(v, idxv, nextName(), LLVMBB);
  }

  ShuffleVectorInst *createShuffleVector(Value *v, ArrayRef<int> mask) {
    return new ShuffleVectorInst(v, mask, nextName(), LLVMBB);
  }

  Value *getIndexedElement(unsigned idx, unsigned eltSize, unsigned reg) {
    assert(getRegSize(reg) == 128 && "Expected 128-bit register");
    auto *ty = getVecTy(eltSize, 128 / eltSize);
    auto *r = createBitCast(readFromReg(reg), ty);
    return createExtractElement(r, idx);
  }

  Value *getIndexedFPElement(unsigned idx, unsigned eltSize, unsigned reg) {
    assert(getRegSize(reg) == 128 && "Expected 128-bit register");
    auto *element = getIndexedElement(idx, eltSize, reg);
    return createBitCast(element, getFPType(eltSize));
  }

  ExtractValueInst *createExtractValue(Value *v, ArrayRef<unsigned> idxs) {
    return ExtractValueInst::Create(v, idxs, nextName(), LLVMBB);
  }

  ReturnInst *createReturn(Value *v) {
    return ReturnInst::Create(Ctx, v, LLVMBB);
  }

  CallInst *createFShr(Value *a, Value *b, Value *c) {
    auto *decl =
        Intrinsic::getDeclaration(LiftedModule, Intrinsic::fshr, a->getType());
    return CallInst::Create(decl, {a, b, c}, nextName(), LLVMBB);
  }

  CallInst *createFShl(Value *a, Value *b, Value *c) {
    auto *decl =
        Intrinsic::getDeclaration(LiftedModule, Intrinsic::fshl, a->getType());
    return CallInst::Create(decl, {a, b, c}, nextName(), LLVMBB);
  }

  CallInst *createBitReverse(Value *v) {
    auto *decl = Intrinsic::getDeclaration(LiftedModule, Intrinsic::bitreverse,
                                           v->getType());
    return CallInst::Create(decl, {v}, nextName(), LLVMBB);
  }

  CallInst *createAbs(Value *v) {
    auto *decl =
        Intrinsic::getDeclaration(LiftedModule, Intrinsic::abs, v->getType());
    return CallInst::Create(decl, {v, getIntConst(0, 1)}, nextName(), LLVMBB);
  }

  CallInst *createCtlz(Value *v) {
    auto *decl =
        Intrinsic::getDeclaration(LiftedModule, Intrinsic::ctlz, v->getType());
    return CallInst::Create(decl, {v, getIntConst(0, 1)}, nextName(), LLVMBB);
  }

  CallInst *createBSwap(Value *v) {
    auto *decl =
        Intrinsic::getDeclaration(LiftedModule, Intrinsic::bswap, v->getType());
    return CallInst::Create(decl, {v}, nextName(), LLVMBB);
  }

  CallInst *createVectorReduceAdd(Value *v) {
    auto *decl = Intrinsic::getDeclaration(
        LiftedModule, Intrinsic::vector_reduce_add, v->getType());
    return CallInst::Create(decl, {v}, nextName(), LLVMBB);
  }

  CallInst *createFusedMultiplyAdd(Value *a, Value *b, Value *c) {
    auto *decl =
        Intrinsic::getDeclaration(LiftedModule, Intrinsic::fma, a->getType());
    return CallInst::Create(decl, {a, b, c}, nextName(), LLVMBB);
  }

  CallInst *createSQRT(Value *v) {
    auto *decl =
        Intrinsic::getDeclaration(LiftedModule, Intrinsic::sqrt, v->getType());
    return CallInst::Create(decl, {v}, nextName(), LLVMBB);
  }

  SelectInst *createSelect(Value *cond, Value *a, Value *b) {
    return SelectInst::Create(cond, a, b, nextName(), LLVMBB);
  }

  ICmpInst *createICmp(ICmpInst::Predicate p, Value *a, Value *b) {
    return new ICmpInst(*LLVMBB, p, a, b, nextName());
  }

  FCmpInst *createFCmp(FCmpInst::Predicate p, Value *a, Value *b) {
    return new FCmpInst(*LLVMBB, p, a, b, nextName());
  }

  BinaryOperator *createBinop(Value *a, Value *b, Instruction::BinaryOps op) {
    return BinaryOperator::Create(op, a, b, nextName(), LLVMBB);
  }

  BinaryOperator *createUDiv(Value *a, Value *b) {
    return BinaryOperator::Create(Instruction::UDiv, a, b, nextName(), LLVMBB);
  }

  BinaryOperator *createSDiv(Value *a, Value *b) {
    return BinaryOperator::Create(Instruction::SDiv, a, b, nextName(), LLVMBB);
  }

  BinaryOperator *createMul(Value *a, Value *b) {
    return BinaryOperator::Create(Instruction::Mul, a, b, nextName(), LLVMBB);
  }

  BinaryOperator *createAdd(Value *a, Value *b) {
    return BinaryOperator::Create(Instruction::Add, a, b, nextName(), LLVMBB);
  }

  BinaryOperator *createSub(Value *a, Value *b) {
    return BinaryOperator::Create(Instruction::Sub, a, b, nextName(), LLVMBB);
  }

  Value *createRawLShr(Value *a, Value *b) {
    return BinaryOperator::Create(Instruction::LShr, a, b, nextName(), LLVMBB);
  }

  Value *createMaskedLShr(Value *a, Value *b) {
    assert(a->getType() == b->getType() && "Expected values of same type");

    // Get an LLVM mask for b to get shift value less than bit width of a
    // In LLVM shift >= bitwidth -> poison
    auto mask = getMaskByType(a->getType());
    assert(a->getType() == mask->getType() && "Expected values of same type");

    auto masked =
        BinaryOperator::Create(Instruction::And, mask, b, nextName(), LLVMBB);
    return BinaryOperator::Create(Instruction::LShr, a, masked, nextName(),
                                  LLVMBB);
  }

  Value *createRawAShr(Value *a, Value *b) {
    return BinaryOperator::Create(Instruction::AShr, a, b, nextName(), LLVMBB);
  }

  Value *createMaskedAShr(Value *a, Value *b) {
    assert(a->getType() == b->getType() && "Expected values of same type");

    // Get an LLVM mask for b to get shift value less than bit width of a
    // In LLVM shift >= bitwidth -> poison
    auto mask = getMaskByType(a->getType());
    assert(a->getType() == mask->getType() && "Expected values of same type");

    auto masked =
        BinaryOperator::Create(Instruction::And, mask, b, nextName(), LLVMBB);
    return BinaryOperator::Create(Instruction::AShr, a, masked, nextName(),
                                  LLVMBB);
  }

  Value *createRawShl(Value *a, Value *b) {
    return BinaryOperator::Create(Instruction::Shl, a, b, nextName(), LLVMBB);
  }

  Value *createMaskedShl(Value *a, Value *b) {
    assert(a->getType() == b->getType() && "Expected values of same type");

    // Get an LLVM mask for b to get shift value less than bit width of a
    // In LLVM shift >= bitwidth -> poison
    auto mask = getMaskByType(a->getType());
    assert(a->getType() == mask->getType() && "Expected values of same type");

    auto masked =
        BinaryOperator::Create(Instruction::And, mask, b, nextName(), LLVMBB);
    return BinaryOperator::Create(Instruction::Shl, a, masked, nextName(),
                                  LLVMBB);
  }

  Value *getLowOnes(int ones, int w) {
    auto zero = getIntConst(0, ones);
    auto one = getIntConst(1, ones);
    auto minusOne = createSub(zero, one);
    return createZExt(minusOne, getIntTy(w));
  }

  Value *createMSL(Value *a, int b) {
    auto v = BinaryOperator::Create(Instruction::Shl, a,
                                    getIntConst(b, getBitWidth(a)), nextName(),
                                    LLVMBB);
    auto ones = getLowOnes(b, getBitWidth(a));
    return createOr(v, ones);
  }

  BinaryOperator *createAnd(Value *a, Value *b) {
    return BinaryOperator::Create(Instruction::And, a, b, nextName(), LLVMBB);
  }

  BinaryOperator *createOr(Value *a, Value *b) {
    return BinaryOperator::Create(Instruction::Or, a, b, nextName(), LLVMBB);
  }

  BinaryOperator *createXor(Value *a, Value *b) {
    return BinaryOperator::Create(Instruction::Xor, a, b, nextName(), LLVMBB);
  }

  BinaryOperator *createNot(Value *a) {
    auto *ty = a->getType();
    auto Zero = ConstantInt::get(ty, 0);
    auto One = ConstantInt::get(ty, 1);
    auto NegOne =
        BinaryOperator::Create(Instruction::Sub, Zero, One, nextName(), LLVMBB);
    return BinaryOperator::Create(Instruction::Xor, a, NegOne, nextName(),
                                  LLVMBB);
  }

  FreezeInst *createFreeze(Value *v) {
    return new FreezeInst(v, nextName(), LLVMBB);
  }

  CastInst *createTrunc(Value *v, Type *t) {
    return CastInst::Create(Instruction::Trunc, v, t, nextName(), LLVMBB);
  }

  CastInst *createZExt(Value *v, Type *t) {
    return CastInst::Create(Instruction::ZExt, v, t, nextName(), LLVMBB);
  }

  CastInst *createSExt(Value *v, Type *t) {
    return CastInst::Create(Instruction::SExt, v, t, nextName(), LLVMBB);
  }

  CastInst *createFPToUI(Value *v, Type *t) {
    return CastInst::Create(Instruction::FPToUI, v, t, nextName(), LLVMBB);
  }

  CastInst *createFPToSI(Value *v, Type *t) {
    return CastInst::Create(Instruction::FPToSI, v, t, nextName(), LLVMBB);
  }

  CastInst *createUIToFP(Value *v, Type *t) {
    return CastInst::Create(Instruction::UIToFP, v, t, nextName(), LLVMBB);
  }

  CastInst *createSIToFP(Value *v, Type *t) {
    return CastInst::Create(Instruction::SIToFP, v, t, nextName(), LLVMBB);
  }

  CastInst *createFPTrunc(Value *v, Type *t) {
    return CastInst::Create(Instruction::FPTrunc, v, t, nextName(), LLVMBB);
  }

  CastInst *createFPExt(Value *v, Type *t) {
    return CastInst::Create(Instruction::FPExt, v, t, nextName(), LLVMBB);
  }

  CastInst *createBitCast(Value *v, Type *t) {
    return CastInst::Create(Instruction::BitCast, v, t, nextName(), LLVMBB);
  }

  CastInst *createCast(Value *v, Type *t, Instruction::CastOps op) {
    return CastInst::Create(op, v, t, nextName(), LLVMBB);
  }

  Value *splatImm(Value *v, unsigned numElts, unsigned eltSize, bool shift) {
    if (shift) {
      assert(CurInst->getOperand(3).isImm());
      v = regShift(v, getImm(3));
    }
    if (getBitWidth(v) > eltSize)
      v = createTrunc(v, getIntTy(eltSize));
    Value *res = getUndefVec(numElts, eltSize);
    for (unsigned i = 0; i < numElts; ++i)
      res = createInsertElement(res, v, i);
    return res;
  }

  Value *splat(Value *v, unsigned numElts, unsigned eltSize) {
    assert(getBitWidth(v) == eltSize);
    Value *res = getUndefVec(numElts, eltSize);
    for (unsigned i = 0; i < numElts; ++i)
      res = createInsertElement(res, v, i);
    return res;
  }

  enum class extKind { SExt, ZExt, None };

  Value *addPairs(Value *src, unsigned eltSize, unsigned numElts, extKind ext) {
    auto bigEltTy = getIntTy(2 * eltSize);
    Value *res = getUndefVec(numElts / 2, 2 * eltSize);
    for (unsigned i = 0; i < numElts; i += 2) {
      auto elt1 = createExtractElement(src, i);
      auto elt2 = createExtractElement(src, i + 1);
      auto ext1 = ext == extKind::SExt ? createSExt(elt1, bigEltTy)
                                       : createZExt(elt1, bigEltTy);
      auto ext2 = ext == extKind::SExt ? createSExt(elt2, bigEltTy)
                                       : createZExt(elt2, bigEltTy);
      auto sum = createAdd(ext1, ext2);
      res = createInsertElement(res, sum, i / 2);
    }
    return res;
  }

  // Creates LLVM IR instructions which take two values with the same
  // number of bits, bit casting them to vectors of numElts elements
  // of size eltSize and doing an operation on them. In cases where
  // LLVM does not have an appropriate vector instruction, we perform
  // the operation element-wise.
  Value *createVectorOp(function<Value *(Value *, Value *)> op, Value *a,
                        Value *b, unsigned eltSize, unsigned numElts,
                        bool elementWise, extKind ext, bool splatImm2,
                        bool immShift, bool isUpper, bool operandTypesDiffer) {
    if (splatImm2)
      b = splatImm(b, numElts, eltSize, immShift);

    auto vTy = getVecTy(eltSize, numElts);

    if (operandTypesDiffer) {
      if (isUpper) {
        a = createBitCast(a, getVecTy(2 * eltSize, numElts / 2));
      } else {
        a = createBitCast(a, getVecTy(2 * eltSize, numElts));
      }
    } else {
      a = createBitCast(a, vTy);
    }
    b = createBitCast(b, vTy);

    // some instructions double element widths
    if (ext == extKind::ZExt) {
      if (!operandTypesDiffer) {
        a = createZExt(a, getVecTy(2 * eltSize, numElts));
      }
      b = createZExt(b, getVecTy(2 * eltSize, numElts));
    }
    if (ext == extKind::SExt) {
      if (!operandTypesDiffer) {
        a = createSExt(a, getVecTy(2 * eltSize, numElts));
      }
      b = createSExt(b, getVecTy(2 * eltSize, numElts));
    }

    Value *res = nullptr;
    if (elementWise) {
      res = getUndefVec(numElts, eltSize);
      for (unsigned i = 0; i < numElts; ++i) {
        auto aa = createExtractElement(a, i);
        auto bb = createExtractElement(b, i);
        auto cc = op(aa, bb);
        res = createInsertElement(res, cc, i);
      }
    } else {
      // Some instructions use the upper half of the operands.
      if (isUpper) {
        assert(eltSize * numElts == 128);
        if (ext == extKind::ZExt || ext == extKind::SExt) {
          if (!operandTypesDiffer) {
            a = createBitCast(a, getVecTy(128, 2));
          }
          b = createBitCast(b, getVecTy(128, 2));
        } else {
          if (!operandTypesDiffer) {
            a = createBitCast(a, getVecTy(64, 2));
          }
          b = createBitCast(b, getVecTy(64, 2));
        }

        if (!operandTypesDiffer) {
          a = createExtractElement(a, 1);
          a = createBitCast(a, getVecTy(eltSize * 2, numElts / 2));
        }

        b = createExtractElement(b, 1);
        b = createBitCast(b, getVecTy(eltSize * 2, numElts / 2));
      }
      assert(getBitWidth(a) == getBitWidth(b) &&
             "Expected values of same bit width");
      res = op(a, b);
    }
    return res;
  }

  static unsigned int getBitWidth(Type *ty) {
    if (auto vTy = dyn_cast<VectorType>(ty)) {
      return vTy->getScalarSizeInBits() *
             vTy->getElementCount().getFixedValue();
    } else if (ty->isIntegerTy()) {
      return ty->getIntegerBitWidth();
    } else if (ty->isHalfTy()) {
      return 16;
    } else if (ty->isFloatTy()) {
      return 32;
    } else if (ty->isDoubleTy()) {
      return 64;
    } else if (ty->isPointerTy()) {
      return 64;
    } else {
      ty->dump();
      assert(false && "Unhandled type");
    }
  }

  static unsigned int getBitWidth(Value *V) {
    return getBitWidth(V->getType());
  }

  // Returns bitWidth corresponding the registers
  unsigned getRegSize(unsigned Reg) {
    if (Reg == AArch64::N || Reg == AArch64::Z || Reg == AArch64::C ||
        Reg == AArch64::V)
      return 1;
    if (Reg >= AArch64::B0 && Reg <= AArch64::B31)
      return 8;
    if (Reg >= AArch64::H0 && Reg <= AArch64::H31)
      return 16;
    if ((Reg >= AArch64::W0 && Reg <= AArch64::W30) ||
        (Reg >= AArch64::S0 && Reg <= AArch64::S31) || Reg == AArch64::WZR ||
        Reg == AArch64::WSP)
      return 32;
    if ((Reg >= AArch64::X0 && Reg <= AArch64::X28) ||
        (Reg >= AArch64::D0 && Reg <= AArch64::D31) || Reg == AArch64::XZR ||
        Reg == AArch64::SP || Reg == AArch64::FP || Reg == AArch64::LR)
      return 64;
    if (Reg >= AArch64::Q0 && Reg <= AArch64::Q31)
      return 128;
    assert(false && "unhandled register");
  }

  // Maps ARM registers to backing registers
  unsigned mapRegToBackingReg(unsigned Reg) {
    if (Reg == AArch64::WZR)
      return AArch64::XZR;
    else if (Reg == AArch64::W29)
      return AArch64::FP;
    else if (Reg == AArch64::W30)
      return AArch64::LR;
    else if (Reg == AArch64::WSP)
      return AArch64::SP;
    else if (Reg >= AArch64::W0 && Reg <= AArch64::W28)
      return Reg - AArch64::W0 + AArch64::X0;
    else if (Reg >= AArch64::X0 && Reg <= AArch64::X28)
      return Reg - AArch64::X0 + AArch64::X0;
    // Dealias rules for NEON SIMD/floating-point registers
    // https://developer.arm.com/documentation/den0024/a/AArch64-Floating-point-and-NEON/NEON-and-Floating-Point-architecture/Floating-point
    else if (Reg >= AArch64::B0 && Reg <= AArch64::B31)
      return Reg - AArch64::B0 + AArch64::Q0;
    else if (Reg >= AArch64::H0 && Reg <= AArch64::H31)
      return Reg - AArch64::H0 + AArch64::Q0;
    else if (Reg >= AArch64::S0 && Reg <= AArch64::S31)
      return Reg - AArch64::S0 + AArch64::Q0;
    else if (Reg >= AArch64::D0 && Reg <= AArch64::D31)
      return Reg - AArch64::D0 + AArch64::Q0;
    assert(RegFile[Reg] &&
           "ERROR: Cannot have a register without a backing store"
           " register corresponding it.");
    return Reg;
  }

  // return pointer to the backing store for a register, doing the
  // necessary de-aliasing
  Value *dealiasReg(unsigned Reg) {
    auto RegAddr = RegFile[mapRegToBackingReg(Reg)];
    assert(RegAddr);
    return RegAddr;
  }

  // always does a full-width read
  Value *readFromReg(unsigned Reg) {
    auto RegAddr = dealiasReg(Reg);
    return createLoad(getIntTy(getRegSize(mapRegToBackingReg(Reg))), RegAddr);
  }

  Value *readPtrFromReg(unsigned Reg) {
    auto RegAddr = dealiasReg(Reg);
    return createLoad(PointerType::get(Ctx, 0), RegAddr);
  }

  void updateReg(Value *V, u_int64_t reg, bool SExt = false) {
    // important -- squash updates to the zero register
    if (reg == AArch64::WZR || reg == AArch64::XZR)
      return;

    if (V->getType()->isVectorTy())
      V = createBitCast(V, getIntTy(getBitWidth(V)));
    else if (V->getType()->isPointerTy())
      V = createPtrToInt(V, getIntTy(getBitWidth(V)));
    else if (V->getType()->isFloatingPointTy())
      V = createBitCast(V, getIntTy(getBitWidth(V)));

    auto destRegSize = getRegSize(reg);
    auto realRegSize = getRegSize(mapRegToBackingReg(reg));

    // important to chop the value down to the destination register
    // size before extending it again
    if (destRegSize < getBitWidth(V))
      V = createTrunc(V, getIntTy(destRegSize));

    // now sign extend if asked and appropriate
    if (SExt && getBitWidth(V) < 32 && destRegSize == 32)
      V = createSExt(V, getIntTy(32));
    if (SExt && getBitWidth(V) < 64 && destRegSize == 64)
      V = createSExt(V, getIntTy(64));

    // annnnd zero out the rest of the destination reg!
    if (getBitWidth(V) < realRegSize)
      V = createZExt(V, getIntTy(realRegSize));

    assert(getBitWidth(V) == realRegSize &&
           "ERROR: register update should be full width");

    createStore(V, dealiasReg(reg));
  }

  Value *readInputReg(int idx) {
    auto op = CurInst->getOperand(idx);
    assert(op.isReg());
    auto reg = op.getReg();
    auto regSize = getRegSize(reg);
    auto val = readFromReg(reg);
    if (regSize < getBitWidth(val))
      val = createTrunc(val, getIntTy(regSize));
    return val;
  }

  // FIXME: stop using this -- instructions should know what they're loading
  // FIXME: then remove getInstSize!
  // TODO: make it so that lshr generates code on register lookups
  // some instructions make use of this, and the semantics need to be
  // worked out
  Value *readFromOperand(int idx, unsigned size = 0) {
    auto op = CurInst->getOperand(idx);
    if (!size)
      size = getInstSize(CurInst->getOpcode());
    // Expr operand is required for a combination of ADRP and ADDXri address
    // calculation
    assert(op.isImm() || op.isReg() || op.isExpr());

    if (!(size == 8 || size == 16 || size == 32 || size == 64 || size == 128)) {
      *out << "\nERROR: Registers can be 8, 16, 32, 64 or 128 bits\n\n";
      exit(-1);
    }

    Value *V = nullptr;
    if (op.isImm()) {
      V = getIntConst(op.getImm(), size);
    } else if (op.isReg()) {
      V = readFromReg(op.getReg());
      if (size == 32) {
        // Always truncate since backing registers are either 64 or 128 bits
        V = createTrunc(V, getIntTy(32));
      } else if (size == 64 && getBitWidth(V) == 128) {
        // Only truncate if the backing register read from is 128 bits
        // i.e. If value V is 128 bits
        V = createTrunc(V, getIntTy(64));
      }
    } else {
      auto [val, _] = getExprVar(op.getExpr());
      V = val;
    }
    return V;
  }

  Value *readFromFPOperand(int idx, unsigned size) {
    assert((size == 16 || size == 32 || size == 64) &&
           "Only 16, 32 and 64 bit FP regs");
    auto val = readFromOperand(idx, size);
    return createBitCast(val, getFPType(size));
  }

  Value *readFromVecOperand(int idx, unsigned int eltSize, unsigned int numElts,
                            bool isUpperHalf = false, bool isFP = false) {
    VectorType *ty;
    auto regVal = readFromOperand(idx);
    if (eltSize * numElts < getBitWidth(regVal)) {
      regVal = createTrunc(regVal, getIntTy(eltSize * numElts));
    }

    assert(eltSize * numElts == getBitWidth(regVal));
    if (isUpperHalf) {
      assert(eltSize * numElts == 128);
      auto casted = createBitCast(regVal, getVecTy(64, 2));

      regVal = createExtractElement(casted, 1);
      ty = getVecTy(eltSize, numElts / 2);
    } else {
      ty = getVecTy(eltSize, numElts, isFP);
    }
    return createBitCast(regVal, ty);
  }

  void updateOutputReg(Value *V, bool SExt = false) {
    auto destReg = CurInst->getOperand(0).getReg();
    updateReg(V, destReg, SExt);
  }

  std::tuple<string, long> getOffset(const string &var) {
    std::smatch m;
    std::regex offset("^(.*)\\+([0-9]+)$");
    if (std::regex_search(var, m, offset)) {
      auto root = m[1];
      auto offset = m[2];
      *out << "  root = " << root << "\n";
      *out << "  offset = " << offset << "\n";
      return make_tuple(root, stol(offset));
    } else {
      return make_tuple(var, 0);
    }
  }

  // Reads an Expr and maps containing string variable to a global variable
  void mapExprVar(const MCExpr *expr) {
    std::string name;
    llvm::raw_string_ostream ss(name);
    expr->print(ss, nullptr);

    // If the expression starts with a relocation specifier, strip it and map
    // the rest to a string name of the global variable. Assuming there is only
    // one relocation specifier, and it is at the beginning
    // (std::regex_constants::match_continuous).
    // eg: ":lo12:a" becomes  "a"
    std::smatch sm1;
    std::regex reloc("^:[a-z0-9_]+:");
    if (std::regex_search(name, sm1, reloc)) {
      name = sm1.suffix();
    }

    name = demangle(name);
    auto [root, offset] = getOffset(name);
    name = root;

    if (!lookupGlobal(name)) {
      *out << "\ncan't find global '" << name << "'\n";
      *out << "ERROR: Unknown global in ADRP\n\n";
      exit(-1);
    }

    instExprVarMap[CurInst] = name;
  }

  string demangle(const string &name) {
    if (name.rfind(".L.", 0) == 0) {
      // the assembler has mangled local symbols, which start with a
      // dot, by prefixing them with ".L"; here we demangle
      return name.substr(2);
    } else {
      return name;
    }
  }

  // Reads an Expr and gets the global variable corresponding the containing
  // string variable. Assuming the Expr consists of a single global variable.
  pair<Value *, bool> getExprVar(const MCExpr *expr) {
    Value *globalVar;
    // Default to true meaning store the ptr global value rather than loading
    // the value from the global
    bool storePtr = true;
    std::string sss;

    // Matched strings
    std::smatch sm;

    // Regex to match relocation specifiers
    std::regex re(":[a-z0-9_]+:");

    llvm::raw_string_ostream ss(sss);
    expr->print(ss, nullptr);

    auto [root, offset] = getOffset(sss);
    sss = root;

    // If the expression starts with a relocation specifier, strip it and look
    // for the rest (variable in the Expr) in the instExprVarMap and globals.
    // Assuming there is only one relocation specifier, and it is at the
    // beginning (std::regex_constants::match_continuous).
    if (std::regex_search(sss, sm, re,
                          std::regex_constants::match_continuous)) {
      string stringVar = sm.suffix();
      // Check the relocation specifiers to determine whether to store ptr
      // global value in the register or load the value from the global
      if (!sm.empty() && (sm[0] == ":lo12:")) {
        storePtr = false;
      }

      stringVar = demangle(stringVar);

      if (!lookupGlobal(stringVar)) {
        *out << "\nERROR: instruction mentions '" << stringVar << "'\n";
        *out << "which is not a global variable we know about\n\n";
        exit(-1);
      }

      // Look through all visited ADRP instructions to find one in which
      // stringVar was the operand used.
      bool foundStringVar = false;
      for (const auto &exprVar : instExprVarMap) {
        if (exprVar.second == stringVar) {
          foundStringVar = true;
          break;
        }
      }

      if (!foundStringVar) {
        *out << "\nERROR: Did not use \"" << stringVar
             << "\" in an ADRP "
                "instruction\n\n";
        exit(-1);
      }

      globalVar = lookupGlobal(stringVar);
      if (!globalVar) {
        *out << "\nERROR: global not found\n\n";
        exit(-1);
      }
    } else {
      globalVar = lookupGlobal(demangle(sss));
      if (!globalVar) {
        *out << "\nERROR: global not found\n\n";
        exit(-1);
      }
    }

    assert(globalVar);
    if (offset != 0) {
      // FIXME -- would be better to return the root symbol and the
      // offset separately, and let the caller do the pointer
      // arithmetic
      globalVar =
          createGEP(getIntTy(8), globalVar, {getIntConst(offset, 64)}, "");
    }
    return make_pair(globalVar, storePtr);
  }

  Value *getV() {
    return createLoad(getIntTy(1), dealiasReg(AArch64::V));
  }

  Value *getZ() {
    return createLoad(getIntTy(1), dealiasReg(AArch64::Z));
  }

  Value *getN() {
    return createLoad(getIntTy(1), dealiasReg(AArch64::N));
  }

  Value *getC() {
    return createLoad(getIntTy(1), dealiasReg(AArch64::C));
  }

  uint64_t replicate8to64(uint64_t v) {
    uint64_t ret = 0;
    for (int i = 0; i < 8; ++i) {
      bool b = (v & 128) != 0;
      ret <<= 8;
      if (b)
        ret |= 0xff;
      v <<= 1;
    }
    return ret;
  }

  // negative shift exponents go the other direction
  Value *createUSHL(Value *a, Value *b) {
    auto zero = getIntConst(0, getBitWidth(b));
    auto c = createICmp(ICmpInst::Predicate::ICMP_SGT, b, zero);
    auto neg = createSub(zero, b);
    auto posRes = createMaskedShl(a, b);
    auto negRes = createMaskedLShr(a, neg);
    return createSelect(c, posRes, negRes);
  }

  // negative shift exponents go the other direction
  Value *createSSHL(Value *a, Value *b) {
    auto zero = getIntConst(0, getBitWidth(b));
    auto c = createICmp(ICmpInst::Predicate::ICMP_SGT, b, zero);
    auto neg = createSub(zero, b);
    auto posRes = createMaskedShl(a, b);
    auto negRes = createMaskedAShr(a, neg);
    return createSelect(c, posRes, negRes);
  }

  Value *rev(Value *in, unsigned eltSize, unsigned amt) {
    assert(eltSize == 8 || eltSize == 16 || eltSize == 32);
    assert(getBitWidth(in) == 64 || getBitWidth(in) == 128);
    if (getBitWidth(in) == 64)
      in = createZExt(in, getIntTy(128));
    Value *rev = getUndefVec(128 / eltSize, eltSize);
    in = createBitCast(in, getVecTy(eltSize, 128 / eltSize));
    for (unsigned i = 0; i < (128 / amt); ++i) {
      auto innerCount = amt / eltSize;
      for (unsigned j = 0; j < innerCount; j++) {
        auto elt = createExtractElement(in, (i * innerCount) + j);
        rev = createInsertElement(rev, elt,
                                  (i * innerCount) + innerCount - j - 1);
      }
    }
    return rev;
  }

  Value *dupElts(Value *v, unsigned numElts, unsigned eltSize) {
    unsigned w = numElts * eltSize;
    assert(w == 64 || w == 128);
    assert(getBitWidth(v) == eltSize);
    Value *res = getUndefVec(numElts, eltSize);
    for (unsigned i = 0; i < numElts; i++)
      res = createInsertElement(res, v, i);
    return res;
  }

  Value *concat(Value *a, Value *b) {
    int wa = getBitWidth(a);
    int wb = getBitWidth(b);
    auto wide_a = createZExt(a, getIntTy(wa + wb));
    auto wide_b = createZExt(b, getIntTy(wa + wb));
    auto shifted_a = createRawShl(wide_a, getIntConst(wb, wa + wb));
    return createOr(shifted_a, wide_b);
  }

  Value *conditionHolds(uint64_t cond) {
    assert(cond < 16);

    // cond<0> == '1' && cond != '1111'
    auto invert_bit = (cond & 1) && (cond != 15);

    cond >>= 1;

    auto cur_v = getV();
    auto cur_z = getZ();
    auto cur_n = getN();
    auto cur_c = getC();

    auto falseVal = getIntConst(0, 1);
    auto trueVal = getIntConst(1, 1);

    Value *res = nullptr;
    switch (cond) {
    case 0:
      res = cur_z;
      break; // EQ/NE
    case 1:
      res = cur_c;
      break; // CS/CC
    case 2:
      res = cur_n;
      break; // MI/PL
    case 3:
      res = cur_v;
      break; // VS/VC
    case 4: {
      // HI/LS: PSTATE.C == '1' && PSTATE.Z == '0'
      // C == 1
      auto c_cond = createICmp(ICmpInst::Predicate::ICMP_EQ, cur_c, trueVal);
      // Z == 0
      auto z_cond = createICmp(ICmpInst::Predicate::ICMP_EQ, cur_z, falseVal);
      // C == 1 && Z == 0
      res = createAnd(c_cond, z_cond);
      break;
    }
    case 5:
      // GE/LT PSTATE.N == PSTATE.V
      res = createICmp(ICmpInst::Predicate::ICMP_EQ, cur_n, cur_v);
      break;
    case 6: {
      // GT/LE PSTATE.N == PSTATE.V && PSTATE.Z == 0
      auto n_eq_v = createICmp(ICmpInst::Predicate::ICMP_EQ, cur_n, cur_v);
      auto z_cond = createICmp(ICmpInst::Predicate::ICMP_EQ, cur_z, falseVal);
      res = createAnd(n_eq_v, z_cond);
      break;
    }
    case 7:
      res = trueVal;
      break;
    default:
      assert(false && "invalid input to conditionHolds()");
      break;
    }

    assert(res != nullptr && "condition code was not generated");

    if (invert_bit)
      res = createXor(res, trueVal);

    return res;
  }

  tuple<Value *, tuple<Value *, Value *, Value *, Value *>>
  addWithCarry(Value *l, Value *r, Value *carryIn) {
    assert(getBitWidth(l) == getBitWidth(r));
    assert(getBitWidth(carryIn) == 1);

    auto size = getBitWidth(l);

    // Deal with size+1 bit integers so that we can easily calculate the c/v
    // PSTATE bits.
    auto ty = l->getType();
    auto tyPlusOne = getIntTy(size + 1);
    auto carry = createZExt(carryIn, tyPlusOne);

    auto uAdd = createAdd(createZExt(l, tyPlusOne), createZExt(r, tyPlusOne));
    auto unsignedSum = createAdd(uAdd, carry);

    auto sAdd = createAdd(createSExt(l, tyPlusOne), createSExt(r, tyPlusOne));
    auto signedSum = createAdd(sAdd, carry);

    auto zero = getIntConst(0, size);
    auto res = createTrunc(unsignedSum, ty);

    auto newN = createICmp(ICmpInst::Predicate::ICMP_SLT, res, zero);
    auto newZ = createICmp(ICmpInst::Predicate::ICMP_EQ, res, zero);
    auto newC = createICmp(ICmpInst::Predicate::ICMP_NE, unsignedSum,
                           createZExt(res, tyPlusOne));
    auto newV = createICmp(ICmpInst::Predicate::ICMP_NE, signedSum,
                           createSExt(res, tyPlusOne));

    return {res, {newN, newZ, newC, newV}};
  };

  void setV(Value *V) {
    assert(getBitWidth(V) == 1);
    updateReg(V, AArch64::V);
  }

  void setZ(Value *V) {
    assert(getBitWidth(V) == 1);
    updateReg(V, AArch64::Z);
  }

  void setN(Value *V) {
    assert(getBitWidth(V) == 1);
    updateReg(V, AArch64::N);
  }

  void setC(Value *V) {
    assert(getBitWidth(V) == 1);
    updateReg(V, AArch64::C);
  }

  void setZUsingResult(Value *V) {
    auto W = getBitWidth(V);
    auto zero = getIntConst(0, W);
    auto z = createICmp(ICmpInst::Predicate::ICMP_EQ, V, zero);
    setZ(z);
  }

  void setNUsingResult(Value *V) {
    auto W = getBitWidth(V);
    auto zero = getIntConst(0, W);
    auto n = createICmp(ICmpInst::Predicate::ICMP_SLT, V, zero);
    setN(n);
  }

  void assertSame(Value *a, Value *b) {
    auto *c = createICmp(ICmpInst::Predicate::ICMP_EQ, a, b);
    CallInst::Create(assertDecl, {c}, "", LLVMBB);
  }

  // From https://github.com/agustingianni/retools
  inline uint64_t Replicate(uint64_t bit, int N) {
    if (!bit)
      return 0;
    if (N == 64)
      return 0xffffffffffffffffLL;
    return (1ULL << N) - 1;
  }

  // From https://github.com/agustingianni/retools
  inline uint64_t Replicate32x2(uint64_t bits32) {
    return (bits32 << 32) | bits32;
  }

  // From https://github.com/agustingianni/retools
  inline uint64_t Replicate16x4(uint64_t bits16) {
    return Replicate32x2((bits16 << 16) | bits16);
  }

  // From https://github.com/agustingianni/retools
  inline uint64_t Replicate8x8(uint64_t bits8) {
    return Replicate16x4((bits8 << 8) | bits8);
  }

  // From https://github.com/agustingianni/retools
  inline uint64_t VFPExpandImm(uint64_t imm8, unsigned N) {
    unsigned E = ((N == 32) ? 8 : 11) - 2; // E in {6, 9}
    unsigned F = N - E - 1;                // F in {25, 54}
    uint64_t imm8_6 = (imm8 >> 6) & 1;     // imm8<6>
    uint64_t sign = (imm8 >> 7) & 1;       // imm8<7>
    // NOT(imm8<6>):Replicate(imm8<6>,{5, 8})
    uint64_t exp = ((imm8_6 ^ 1) << (E - 1)) | Replicate(imm8_6, E - 1);
    // imm8<5:0> : Zeros({19, 48})
    uint64_t frac = ((imm8 & 0x3f) << (F - 6)) | Replicate(0, F - 6);
    uint64_t res = (sign << (E + F)) | (exp << F) | frac;
    return res;
  }

  // From https://github.com/agustingianni/retools
  // Implementation of: bits(64) AdvSIMDExpandImm(bit op, bits(4) cmode, bits(8)
  // imm8)
  inline uint64_t AdvSIMDExpandImm(unsigned op, unsigned cmode, unsigned imm8) {
    uint64_t imm64 = 0;

    switch (cmode >> 1) {
    case 0:
      imm64 = Replicate32x2(imm8);
      break;
    case 1:
      imm64 = Replicate32x2(imm8 << 8);
      break;
    case 2:
      imm64 = Replicate32x2(imm8 << 16);
      break;
    case 3:
      imm64 = Replicate32x2(imm8 << 24);
      break;
    case 4:
      imm64 = Replicate16x4(imm8);
      break;
    case 5:
      imm64 = Replicate16x4(imm8 << 8);
      break;
    case 6:
      if ((cmode & 1) == 0)
        imm64 = Replicate32x2((imm8 << 8) | 0xFF);
      else
        imm64 = Replicate32x2((imm8 << 16) | 0xFFFF);
      break;
    case 7:
      if ((cmode & 1) == 0 && op == 0)
        imm64 = Replicate8x8(imm8);

      if ((cmode & 1) == 0 && op == 1) {
        imm64 = 0;
        imm64 |= (imm8 & 0x80) ? 0xFF : 0x00;
        imm64 <<= 8;
        imm64 |= (imm8 & 0x40) ? 0xFF : 0x00;
        imm64 <<= 8;
        imm64 |= (imm8 & 0x20) ? 0xFF : 0x00;
        imm64 <<= 8;
        imm64 |= (imm8 & 0x10) ? 0xFF : 0x00;
        imm64 <<= 8;
        imm64 |= (imm8 & 0x08) ? 0xFF : 0x00;
        imm64 <<= 8;
        imm64 |= (imm8 & 0x04) ? 0xFF : 0x00;
        imm64 <<= 8;
        imm64 |= (imm8 & 0x02) ? 0xFF : 0x00;
        imm64 <<= 8;
        imm64 |= (imm8 & 0x01) ? 0xFF : 0x00;
      }

      if ((cmode & 1) == 1 && op == 0) {
        uint64_t imm8_7 = (imm8 >> 7) & 1;
        uint64_t imm8_6 = (imm8 >> 6) & 1;
        uint64_t imm8_50 = imm8 & 63;
        uint64_t imm32 = (imm8_7 << (1 + 5 + 6 + 19)) |
                         ((imm8_6 ^ 1) << (5 + 6 + 19)) |
                         (Replicate(imm8_6, 5) << (6 + 19)) | (imm8_50 << 19);
        imm64 = Replicate32x2(imm32);
      }

      if ((cmode & 1) == 1 && op == 1) {
        // imm64 =
        // imm8<7>:NOT(imm8<6>):Replicate(imm8<6>,8):imm8<5:0>:Zeros(48);
        uint64_t imm8_7 = (imm8 >> 7) & 1;
        uint64_t imm8_6 = (imm8 >> 6) & 1;
        uint64_t imm8_50 = imm8 & 63;
        imm64 = (imm8_7 << 63) | ((imm8_6 ^ 1) << 62) |
                (Replicate(imm8_6, 8) << 54) | (imm8_50 << 48);
      }
      break;
    default:
      abort();
    }

    return imm64;
  }

  vector<Value *> marshallArgs(Function *fn) {
    if (fn->getReturnType()->isStructTy()) {
      *out << "\nERROR: we don't support structures in return values yet\n\n";
      exit(-1);
    }
    if (fn->getReturnType()->isArrayTy()) {
      *out << "\nERROR: we don't support arrays in return values yet\n\n";
      exit(-1);
    }
    unsigned vecArgNum = 0;
    unsigned scalarArgNum = 0;
    // unsigned stackSlot = 0;
    vector<Value *> args;
    for (auto arg = fn->arg_begin(); arg != fn->arg_end(); ++arg) {
      auto *argTy = arg->getType();
      if (argTy->isStructTy()) {
        *out << "\nERROR: we don't support structures in arguments yet\n\n";
        exit(-1);
      }
      if (argTy->isArrayTy()) {
        *out << "\nERROR: we don't support arrays in arguments yet\n\n";
        exit(-1);
      }
      Value *param{nullptr};
      if (argTy->isFloatingPointTy() || argTy->isVectorTy()) {
        if (scalarArgNum < 8) {
          param = readFromReg(AArch64::Q0 + vecArgNum);
          if (getBitWidth(argTy) < 128)
            param = createTrunc(param, getIntTy(getBitWidth(argTy)));
          param = createBitCast(param, argTy);
          ++vecArgNum;
        } else {
          // FIXME load from stack
          assert(false);
        }
      } else if (argTy->isIntegerTy() || argTy->isPointerTy()) {
        assert(getBitWidth(arg) <= 64);
        // FIXME check signext and zeroext
        if (scalarArgNum < 8) {
          param = readFromReg(AArch64::X0 + scalarArgNum);
          ++scalarArgNum;
        } else {
          // FIXME load from stack
          assert(false);
        }
        if (argTy->isPointerTy()) {
          param = new IntToPtrInst(param, PointerType::get(Ctx, 0), "", LLVMBB);
        } else {
          if (getBitWidth(arg) < 64)
            param = createTrunc(param, getIntTy(getBitWidth(arg)));
        }
      }
      args.push_back(param);
    }
    *out << "marshalled up " << args.size() << " arguments\n";
    return args;
  }

  void doCall() {
    auto &op0 = CurInst->getOperand(0);
    assert(op0.isExpr());
    auto [expr, _] = getExprVar(op0.getExpr());
    assert(expr);
    string calleeName = (string)expr->getName();

    // yikes
    if (calleeName == "__stack_chk_fail") {
      createTrap();
      return;
    }

    *out << "lifting a call, callee is: '" << calleeName << "'\n";
    auto *callee = dyn_cast<Function>(expr);
    assert(callee);
    auto FC = FunctionCallee(callee);
    auto args = marshallArgs(callee);

    // yikes -- these functions have an LLVM "immediate" as their last
    // argument; this is not present in the assembly at all, we have
    // to provide it by hand
    if (calleeName == "llvm.memset.p0.i64")
      args[3] = getIntConst(0, 1);
    if (calleeName == "llvm.memcpy.p0.p0.i64")
      args[3] = getIntConst(0, 1);
    if (calleeName == "llvm.memmove.p0.p0.i64")
      args[3] = getIntConst(0, 1);

    auto CI = CallInst::Create(FC, args, "", LLVMBB);
    auto RV = enforceABIRules(CI, callee->hasRetAttribute(Attribute::SExt),
                              callee->hasRetAttribute(Attribute::ZExt));
    // FIXME invalidate machine state that needs invalidating
    auto retTy = callee->getReturnType();
    if (retTy->isIntegerTy() || retTy->isPointerTy()) {
      updateReg(RV, AArch64::X0);
    } else if (retTy->isFloatingPointTy() || retTy->isVectorTy()) {
      updateReg(RV, AArch64::Q0);
    }
  }

  void doReturn() {
    auto i32 = getIntTy(32);
    auto i64 = getIntTy(64);

    if (true) {
      /*
       * ABI stuff: on all return paths, check that callee-saved +
       * other registers have been reset to their previous
       * values. these values were saved at the top of the function so
       * the trivially dominate all returns
       */
      // FIXME: check callee-saved vector registers
      // FIXME: make sure code doesn't touch 16, 17?
      // FIXME: check FP and LR?
      assertSame(initialSP, readFromReg(AArch64::SP));
      for (unsigned r = 19; r <= 28; ++r)
        assertSame(initialReg[r], readFromReg(AArch64::X0 + r));
    }

    auto *retTyp = srcFn.getReturnType();
    if (retTyp->isVoidTy()) {
      createReturn(nullptr);
    } else {
      Value *retVal = nullptr;
      if (retTyp->isVectorTy() || retTyp->isFloatingPointTy()) {
        retVal = readFromReg(AArch64::Q0);
      } else {
        retVal = readFromReg(AArch64::X0);
      }
      if (retTyp->isPointerTy()) {
        retVal = new IntToPtrInst(retVal, PointerType::get(Ctx, 0), "", LLVMBB);
      } else {
        auto retWidth = DL.getTypeSizeInBits(retTyp);
        auto retValWidth = DL.getTypeSizeInBits(retVal->getType());

        if (retWidth < retValWidth)
          retVal = createTrunc(retVal, getIntTy(retWidth));

        // mask off any don't-care bits
        if (has_ret_attr && (origRetWidth < 32)) {
          assert(retWidth >= origRetWidth);
          assert(retWidth == 64);
          auto trunc = createTrunc(retVal, i32);
          retVal = createZExt(trunc, i64);
        }

        if ((retTyp->isVectorTy() || retTyp->isFloatingPointTy()) &&
            !has_ret_attr)
          retVal = createBitCast(retVal, retTyp);
      }
      createReturn(retVal);
    }
  }

  bool disjoint(const set<int> &a, const set<int> &b) {
    set<int> i;
    std::set_intersection(a.begin(), a.end(), b.begin(), b.end(),
                          std::inserter(i, i.begin()));
    return i.empty();
  }

  int64_t getImm(int idx) {
    return CurInst->getOperand(idx).getImm();
  }

  enum ExtendType { SXTB, SXTH, SXTW, SXTX, UXTB, UXTH, UXTW, UXTX };

  // Follows the "Library pseudocode for aarch64/instrs/extendreg/ExtendReg"
  // from ARM manual
  // val is always 64 bits and shiftAmt is always 0-4
  Value *extendAndShiftValue(Value *val, enum ExtendType extType,
                             int shiftAmt) {
    assert(val->getType()->getIntegerBitWidth() == 64);
    assert(shiftAmt >= 0 && shiftAmt <= 4);

    // size is always 64 for offset shifting instructions
    //    auto size = getInstSize(opcode);
    auto size = 64;
    auto ty = getIntTy(size);
    auto isSigned = (extType & 0x4) != 0x4;

    // extendSize is necessary so that we can start with the word size
    // ARM wants us to (byte, half, full) and then sign extend to a new
    // size. Without extendSize being used for a trunc, a lot of masking
    // and more manual work to sign extend would be necessary
    unsigned extendSize = 8 << (extType & 0x3);

    // Make sure to not trunc to the same size as the parameter.
    if (extendSize != (unsigned)size) {
      auto truncType = getIntTy(extendSize);
      val = createTrunc(val, truncType);
      val =
          createCast(val, ty, isSigned ? Instruction::SExt : Instruction::ZExt);
    }

    // shift may not be there, it may just be the extend
    if (shiftAmt != 0)
      val = createMaskedShl(val, getIntConst(shiftAmt, size));

    return val;
  }

  tuple<Value *, Value *> getParamsLoadReg() {
    auto &op0 = CurInst->getOperand(0);
    auto &op1 = CurInst->getOperand(1);
    auto &op2 = CurInst->getOperand(2);
    auto &op3 = CurInst->getOperand(3);
    auto &op4 = CurInst->getOperand(4);
    assert(op0.isReg() && op1.isReg() && op2.isReg());
    assert(op3.isImm() && op4.isImm());

    auto baseReg = op1.getReg();
    auto offsetReg = op2.getReg();
    auto extendTypeVal = op3.getImm();
    auto shiftAmtVal = op4.getImm();

    assert((baseReg >= AArch64::X0 && baseReg <= AArch64::X28) ||
           (baseReg == AArch64::SP) || (baseReg == AArch64::LR) ||
           (baseReg == AArch64::FP) || (baseReg == AArch64::XZR));
    assert((offsetReg >= AArch64::X0 && offsetReg <= AArch64::X28) ||
           (offsetReg == AArch64::FP) || (offsetReg == AArch64::SP) ||
           (offsetReg == AArch64::XZR) ||
           (offsetReg >= AArch64::W0 && offsetReg <= AArch64::W29) ||
           (offsetReg == AArch64::WZR));

    int extTyp = -1, shiftAmt;
    if ((offsetReg >= AArch64::W0 && offsetReg <= AArch64::W29) ||
        offsetReg == AArch64::WZR) {
      extTyp = extendTypeVal ? SXTW : UXTW;
    } else if ((offsetReg >= AArch64::X0 && offsetReg <= AArch64::X28) ||
               offsetReg == AArch64::FP || offsetReg == AArch64::XZR) {
      // The manual assigns a value LSL to extTyp if extendTypeVal is 1
      // which for a value of 64 bits, is the same as UXTX
      extTyp = extendTypeVal ? SXTX : UXTX;
    }

    switch (CurInst->getOpcode()) {
    case AArch64::LDRBBroW:
    case AArch64::LDRBBroX:
    case AArch64::LDRBroW:
    case AArch64::LDRBroX:
    case AArch64::LDRSBWroW:
    case AArch64::LDRSBWroX:
    case AArch64::LDRSBXroW:
    case AArch64::LDRSBXroX:
      shiftAmt = 0;
      break;
    case AArch64::LDRHHroW:
    case AArch64::LDRHHroX:
    case AArch64::LDRHroW:
    case AArch64::LDRHroX:
    case AArch64::LDRSHWroW:
    case AArch64::LDRSHWroX:
    case AArch64::LDRSHXroW:
    case AArch64::LDRSHXroX:
      shiftAmt = shiftAmtVal ? 1 : 0;
      break;
    case AArch64::LDRWroW:
    case AArch64::LDRWroX:
    case AArch64::LDRSroW:
    case AArch64::LDRSroX:
    case AArch64::LDRSWroW:
    case AArch64::LDRSWroX:
      shiftAmt = shiftAmtVal ? 2 : 0;
      break;
    case AArch64::LDRXroW:
    case AArch64::LDRXroX:
    case AArch64::LDRDroW:
    case AArch64::LDRDroX:
      shiftAmt = shiftAmtVal ? 3 : 0;
      break;
    case AArch64::LDRQroW:
    case AArch64::LDRQroX:
      shiftAmt = shiftAmtVal ? 4 : 0;
      break;
    default:
      *out << "\nError Unknown opcode\n";
      visitError();
    }

    auto baseAddr = readPtrFromReg(baseReg);
    auto offset = extendAndShiftValue(readFromReg(offsetReg),
                                      (ExtendType)extTyp, shiftAmt);

    return make_pair(baseAddr, offset);
  }

  Value *makeLoadWithOffset(Value *base, Value *offset, int size) {
    // Create a GEP instruction based on a byte addressing basis (8 bits)
    // returning pointer to base + offset
    assert(base);
    auto ptr = createGEP(getIntTy(8), base, {offset}, "");

    // Load Value val in the pointer returned by the GEP instruction
    return createLoad(getIntTy(8 * size), ptr);
  }

  unsigned decodeRegSet(unsigned r) {
    switch (r) {
    case AArch64::D0:
    case AArch64::D0_D1:
    case AArch64::D0_D1_D2:
    case AArch64::D0_D1_D2_D3:
      return AArch64::D0;
    case AArch64::D1:
    case AArch64::D1_D2:
    case AArch64::D1_D2_D3:
    case AArch64::D1_D2_D3_D4:
      return AArch64::D1;
    case AArch64::D2:
    case AArch64::D2_D3:
    case AArch64::D2_D3_D4:
    case AArch64::D2_D3_D4_D5:
      return AArch64::D2;
    case AArch64::D3:
    case AArch64::D3_D4:
    case AArch64::D3_D4_D5:
    case AArch64::D3_D4_D5_D6:
      return AArch64::D3;
    case AArch64::D4:
    case AArch64::D4_D5:
    case AArch64::D4_D5_D6:
    case AArch64::D4_D5_D6_D7:
      return AArch64::D4;
    case AArch64::D5:
    case AArch64::D5_D6:
    case AArch64::D5_D6_D7:
    case AArch64::D5_D6_D7_D8:
      return AArch64::D5;
    case AArch64::D6:
    case AArch64::D6_D7:
    case AArch64::D6_D7_D8:
    case AArch64::D6_D7_D8_D9:
      return AArch64::D6;
    case AArch64::D7:
    case AArch64::D7_D8:
    case AArch64::D7_D8_D9:
    case AArch64::D7_D8_D9_D10:
      return AArch64::D7;
    case AArch64::D8:
    case AArch64::D8_D9:
    case AArch64::D8_D9_D10:
    case AArch64::D8_D9_D10_D11:
      return AArch64::D8;
    case AArch64::D9:
    case AArch64::D9_D10:
    case AArch64::D9_D10_D11:
    case AArch64::D9_D10_D11_D12:
      return AArch64::D9;
    case AArch64::D10:
    case AArch64::D10_D11:
    case AArch64::D10_D11_D12:
    case AArch64::D10_D11_D12_D13:
      return AArch64::D10;
    case AArch64::D11:
    case AArch64::D11_D12:
    case AArch64::D11_D12_D13:
    case AArch64::D11_D12_D13_D14:
      return AArch64::D11;
    case AArch64::D12:
    case AArch64::D12_D13:
    case AArch64::D12_D13_D14:
    case AArch64::D12_D13_D14_D15:
      return AArch64::D12;
    case AArch64::D13:
    case AArch64::D13_D14:
    case AArch64::D13_D14_D15:
    case AArch64::D13_D14_D15_D16:
      return AArch64::D13;
    case AArch64::D14:
    case AArch64::D14_D15:
    case AArch64::D14_D15_D16:
    case AArch64::D14_D15_D16_D17:
      return AArch64::D14;
    case AArch64::D15:
    case AArch64::D15_D16:
    case AArch64::D15_D16_D17:
    case AArch64::D15_D16_D17_D18:
      return AArch64::D15;
    case AArch64::D16:
    case AArch64::D16_D17:
    case AArch64::D16_D17_D18:
    case AArch64::D16_D17_D18_D19:
      return AArch64::D16;
    case AArch64::D17:
    case AArch64::D17_D18:
    case AArch64::D17_D18_D19:
    case AArch64::D17_D18_D19_D20:
      return AArch64::D17;
    case AArch64::D18:
    case AArch64::D18_D19:
    case AArch64::D18_D19_D20:
    case AArch64::D18_D19_D20_D21:
      return AArch64::D18;
    case AArch64::D19:
    case AArch64::D19_D20:
    case AArch64::D19_D20_D21:
    case AArch64::D19_D20_D21_D22:
      return AArch64::D19;
    case AArch64::D20:
    case AArch64::D20_D21:
    case AArch64::D20_D21_D22:
    case AArch64::D20_D21_D22_D23:
      return AArch64::D20;
    case AArch64::D21:
    case AArch64::D21_D22:
    case AArch64::D21_D22_D23:
    case AArch64::D21_D22_D23_D24:
      return AArch64::D21;
    case AArch64::D22:
    case AArch64::D22_D23:
    case AArch64::D22_D23_D24:
    case AArch64::D22_D23_D24_D25:
      return AArch64::D22;
    case AArch64::D23:
    case AArch64::D23_D24:
    case AArch64::D23_D24_D25:
    case AArch64::D23_D24_D25_D26:
      return AArch64::D23;
    case AArch64::D24:
    case AArch64::D24_D25:
    case AArch64::D24_D25_D26:
    case AArch64::D24_D25_D26_D27:
      return AArch64::D24;
    case AArch64::D25:
    case AArch64::D25_D26:
    case AArch64::D25_D26_D27:
    case AArch64::D25_D26_D27_D28:
      return AArch64::D25;
    case AArch64::D26:
    case AArch64::D26_D27:
    case AArch64::D26_D27_D28:
    case AArch64::D26_D27_D28_D29:
      return AArch64::D26;
    case AArch64::D27:
    case AArch64::D27_D28:
    case AArch64::D27_D28_D29:
    case AArch64::D27_D28_D29_D30:
      return AArch64::D27;
    case AArch64::D28:
    case AArch64::D28_D29:
    case AArch64::D28_D29_D30:
    case AArch64::D28_D29_D30_D31:
      return AArch64::D28;
    case AArch64::D29:
    case AArch64::D29_D30:
    case AArch64::D29_D30_D31:
    case AArch64::D29_D30_D31_D0:
      return AArch64::D29;
    case AArch64::D30:
    case AArch64::D30_D31:
    case AArch64::D30_D31_D0:
    case AArch64::D30_D31_D0_D1:
      return AArch64::D30;
    case AArch64::D31:
    case AArch64::D31_D0:
    case AArch64::D31_D0_D1:
    case AArch64::D31_D0_D1_D2:
      return AArch64::D31;
    case AArch64::Q0:
    case AArch64::Q0_Q1:
    case AArch64::Q0_Q1_Q2:
    case AArch64::Q0_Q1_Q2_Q3:
      return AArch64::Q0;
    case AArch64::Q1:
    case AArch64::Q1_Q2:
    case AArch64::Q1_Q2_Q3:
    case AArch64::Q1_Q2_Q3_Q4:
      return AArch64::Q1;
    case AArch64::Q2:
    case AArch64::Q2_Q3:
    case AArch64::Q2_Q3_Q4:
    case AArch64::Q2_Q3_Q4_Q5:
      return AArch64::Q2;
    case AArch64::Q3:
    case AArch64::Q3_Q4:
    case AArch64::Q3_Q4_Q5:
    case AArch64::Q3_Q4_Q5_Q6:
      return AArch64::Q3;
    case AArch64::Q4:
    case AArch64::Q4_Q5:
    case AArch64::Q4_Q5_Q6:
    case AArch64::Q4_Q5_Q6_Q7:
      return AArch64::Q4;
    case AArch64::Q5:
    case AArch64::Q5_Q6:
    case AArch64::Q5_Q6_Q7:
    case AArch64::Q5_Q6_Q7_Q8:
      return AArch64::Q5;
    case AArch64::Q6:
    case AArch64::Q6_Q7:
    case AArch64::Q6_Q7_Q8:
    case AArch64::Q6_Q7_Q8_Q9:
      return AArch64::Q6;
    case AArch64::Q7:
    case AArch64::Q7_Q8:
    case AArch64::Q7_Q8_Q9:
    case AArch64::Q7_Q8_Q9_Q10:
      return AArch64::Q7;
    case AArch64::Q8:
    case AArch64::Q8_Q9:
    case AArch64::Q8_Q9_Q10:
    case AArch64::Q8_Q9_Q10_Q11:
      return AArch64::Q8;
    case AArch64::Q9:
    case AArch64::Q9_Q10:
    case AArch64::Q9_Q10_Q11:
    case AArch64::Q9_Q10_Q11_Q12:
      return AArch64::Q9;
    case AArch64::Q10:
    case AArch64::Q10_Q11:
    case AArch64::Q10_Q11_Q12:
    case AArch64::Q10_Q11_Q12_Q13:
      return AArch64::Q10;
    case AArch64::Q11:
    case AArch64::Q11_Q12:
    case AArch64::Q11_Q12_Q13:
    case AArch64::Q11_Q12_Q13_Q14:
      return AArch64::Q11;
    case AArch64::Q12:
    case AArch64::Q12_Q13:
    case AArch64::Q12_Q13_Q14:
    case AArch64::Q12_Q13_Q14_Q15:
      return AArch64::Q12;
    case AArch64::Q13:
    case AArch64::Q13_Q14:
    case AArch64::Q13_Q14_Q15:
    case AArch64::Q13_Q14_Q15_Q16:
      return AArch64::Q13;
    case AArch64::Q14:
    case AArch64::Q14_Q15:
    case AArch64::Q14_Q15_Q16:
    case AArch64::Q14_Q15_Q16_Q17:
      return AArch64::Q14;
    case AArch64::Q15:
    case AArch64::Q15_Q16:
    case AArch64::Q15_Q16_Q17:
    case AArch64::Q15_Q16_Q17_Q18:
      return AArch64::Q15;
    case AArch64::Q16:
    case AArch64::Q16_Q17:
    case AArch64::Q16_Q17_Q18:
    case AArch64::Q16_Q17_Q18_Q19:
      return AArch64::Q16;
    case AArch64::Q17:
    case AArch64::Q17_Q18:
    case AArch64::Q17_Q18_Q19:
    case AArch64::Q17_Q18_Q19_Q20:
      return AArch64::Q17;
    case AArch64::Q18:
    case AArch64::Q18_Q19:
    case AArch64::Q18_Q19_Q20:
    case AArch64::Q18_Q19_Q20_Q21:
      return AArch64::Q18;
    case AArch64::Q19:
    case AArch64::Q19_Q20:
    case AArch64::Q19_Q20_Q21:
    case AArch64::Q19_Q20_Q21_Q22:
      return AArch64::Q19;
    case AArch64::Q20:
    case AArch64::Q20_Q21:
    case AArch64::Q20_Q21_Q22:
    case AArch64::Q20_Q21_Q22_Q23:
      return AArch64::Q20;
    case AArch64::Q21:
    case AArch64::Q21_Q22:
    case AArch64::Q21_Q22_Q23:
    case AArch64::Q21_Q22_Q23_Q24:
      return AArch64::Q21;
    case AArch64::Q22:
    case AArch64::Q22_Q23:
    case AArch64::Q22_Q23_Q24:
    case AArch64::Q22_Q23_Q24_Q25:
      return AArch64::Q22;
    case AArch64::Q23:
    case AArch64::Q23_Q24:
    case AArch64::Q23_Q24_Q25:
    case AArch64::Q23_Q24_Q25_Q26:
      return AArch64::Q23;
    case AArch64::Q24:
    case AArch64::Q24_Q25:
    case AArch64::Q24_Q25_Q26:
    case AArch64::Q24_Q25_Q26_Q27:
      return AArch64::Q24;
    case AArch64::Q25:
    case AArch64::Q25_Q26:
    case AArch64::Q25_Q26_Q27:
    case AArch64::Q25_Q26_Q27_Q28:
      return AArch64::Q25;
    case AArch64::Q26:
    case AArch64::Q26_Q27:
    case AArch64::Q26_Q27_Q28:
    case AArch64::Q26_Q27_Q28_Q29:
      return AArch64::Q26;
    case AArch64::Q27:
    case AArch64::Q27_Q28:
    case AArch64::Q27_Q28_Q29:
    case AArch64::Q27_Q28_Q29_Q30:
      return AArch64::Q27;
    case AArch64::Q28:
    case AArch64::Q28_Q29:
    case AArch64::Q28_Q29_Q30:
    case AArch64::Q28_Q29_Q30_Q31:
      return AArch64::Q28;
    case AArch64::Q29:
    case AArch64::Q29_Q30:
    case AArch64::Q29_Q30_Q31:
    case AArch64::Q29_Q30_Q31_Q0:
      return AArch64::Q29;
    case AArch64::Q30:
    case AArch64::Q30_Q31:
    case AArch64::Q30_Q31_Q0:
    case AArch64::Q30_Q31_Q0_Q1:
      return AArch64::Q30;
    case AArch64::Q31:
    case AArch64::Q31_Q0:
    case AArch64::Q31_Q0_Q1:
    case AArch64::Q31_Q0_Q1_Q2:
      return AArch64::Q31;
    default:
      assert(false && "missing case in decodeRegSet");
      break;
    }
  }

  Value *tblHelper2(vector<Value *> &tbl, Value *idx, unsigned i) {
    if (i == tbl.size())
      return getIntConst(0, 8);
    auto cond = createICmp(ICmpInst::Predicate::ICMP_ULT, idx,
                           getIntConst((i + 1) * 16, 8));
    auto adjIdx = createSub(idx, getIntConst(i * 16, 8));
    auto t = createExtractElement(tbl.at(i), adjIdx);
    auto f = tblHelper2(tbl, idx, i + 1);
    return createSelect(cond, t, f);
  }

  Value *tblHelper(vector<Value *> &tbl, Value *idx) {
    return tblHelper2(tbl, idx, 0);
  }

  tuple<Value *, int> getParamsLoadImmed() {
    auto &op0 = CurInst->getOperand(0);
    auto &op1 = CurInst->getOperand(1);
    auto &op2 = CurInst->getOperand(2);
    assert(op0.isReg() && op1.isReg());
    assert(op2.isImm());
    auto baseReg = op1.getReg();
    assert((baseReg >= AArch64::X0 && baseReg <= AArch64::X28) ||
           (baseReg == AArch64::SP) || (baseReg == AArch64::LR) ||
           (baseReg == AArch64::FP) || (baseReg == AArch64::XZR));
    auto baseAddr = readPtrFromReg(baseReg);
    return make_pair(baseAddr, op2.getImm());
  }

  // Creates instructions to store val in memory pointed by base + offset
  // offset and size are in bytes
  Value *makeLoadWithOffset(Value *base, int offset, unsigned size) {
    // Get offset as a 64-bit LLVM constant
    auto offsetVal = getIntConst(offset, 64);

    // Create a GEP instruction based on a byte addressing basis (8 bits)
    // returning pointer to base + offset
    assert(base);
    auto ptr = createGEP(getIntTy(8), base, {offsetVal}, "");

    // Load Value val in the pointer returned by the GEP instruction
    return createLoad(getIntTy(8 * size), ptr);
  }

  tuple<Value *, Value *, Value *> getParamsStoreReg() {
    auto &op0 = CurInst->getOperand(0);
    auto &op1 = CurInst->getOperand(1);
    auto &op2 = CurInst->getOperand(2);
    auto &op3 = CurInst->getOperand(3);
    auto &op4 = CurInst->getOperand(4);
    assert(op0.isReg() && op1.isReg() && op2.isReg());
    assert(op3.isImm() && op4.isImm());

    auto baseReg = op1.getReg();
    auto offsetReg = op2.getReg();
    auto extendTypeVal = op3.getImm();
    auto shiftAmtVal = op4.getImm();

    assert((baseReg >= AArch64::X0 && baseReg <= AArch64::X28) ||
           (baseReg == AArch64::SP) || (baseReg == AArch64::LR) ||
           (baseReg == AArch64::FP) || (baseReg == AArch64::XZR));
    assert((offsetReg >= AArch64::X0 && offsetReg <= AArch64::X28) ||
           (offsetReg == AArch64::FP) || (offsetReg == AArch64::XZR) ||
           (offsetReg >= AArch64::W0 && offsetReg <= AArch64::W28) ||
           (offsetReg == AArch64::WZR));

    int extTyp, shiftAmt;
    if ((offsetReg >= AArch64::W0 && offsetReg <= AArch64::W28) ||
        offsetReg == AArch64::WZR) {
      extTyp = extendTypeVal ? SXTW : UXTW;
    } else if ((offsetReg >= AArch64::X0 && offsetReg <= AArch64::X28) ||
               offsetReg == AArch64::FP || offsetReg == AArch64::XZR) {
      // The manual assigns a value LSL to extTyp if extendTypeVal is 1
      // which for a value of 64 bits, is the same as UXTX
      extTyp = extendTypeVal ? SXTX : UXTX;
    }

    switch (CurInst->getOpcode()) {
    case AArch64::STRBBroW:
    case AArch64::STRBBroX:
    case AArch64::STRBroW:
    case AArch64::STRBroX:
      shiftAmt = 0;
      break;
    case AArch64::STRHHroW:
    case AArch64::STRHHroX:
    case AArch64::STRHroW:
    case AArch64::STRHroX:
      shiftAmt = shiftAmtVal ? 1 : 0;
      break;
    case AArch64::STRWroW:
    case AArch64::STRWroX:
    case AArch64::STRSroW:
    case AArch64::STRSroX:
      shiftAmt = shiftAmtVal ? 2 : 0;
      break;
    case AArch64::STRXroW:
    case AArch64::STRXroX:
    case AArch64::STRDroW:
    case AArch64::STRDroX:
      shiftAmt = shiftAmtVal ? 3 : 0;
      break;
    case AArch64::STRQroW:
    case AArch64::STRQroX:
      shiftAmt = shiftAmtVal ? 4 : 0;
      break;
    default:
      *out << "\nError Unknown opcode\n";
      visitError();
    }

    auto baseAddr = readPtrFromReg(baseReg);
    auto offset = extendAndShiftValue(readFromReg(offsetReg),
                                      (ExtendType)extTyp, shiftAmt);

    return make_tuple(baseAddr, offset, readFromReg(op0.getReg()));
  }

  void storeToMemoryValOffset(Value *base, Value *offset, u_int64_t size,
                              Value *val) {
    // Create a GEP instruction based on a byte addressing basis (8 bits)
    // returning pointer to base + offset
    assert(base);
    auto ptr = createGEP(getIntTy(8), base, {offset}, "");

    // Store Value val in the pointer returned by the GEP instruction
    createStore(val, ptr);
  }

  tuple<Value *, int, Value *> getStoreParams() {
    auto &op0 = CurInst->getOperand(0);
    auto &op1 = CurInst->getOperand(1);
    auto &op2 = CurInst->getOperand(2);
    assert(op0.isReg() && op1.isReg());

    if (op2.isImm()) {
      auto baseReg = op1.getReg();
      assert((baseReg >= AArch64::X0 && baseReg <= AArch64::X28) ||
             (baseReg == AArch64::SP) || (baseReg == AArch64::LR) ||
             (baseReg == AArch64::FP) || (baseReg == AArch64::XZR));
      auto baseAddr = readPtrFromReg(baseReg);
      return make_tuple(baseAddr, op2.getImm(), readFromReg(op0.getReg()));
    } else {
      assert(op2.isExpr());
      auto [globalVar, _] = getExprVar(op2.getExpr());
      return make_tuple(globalVar, 0, readFromReg(op0.getReg()));
    }
  }

  // Creates instructions to store val in memory pointed by base + offset
  // offset and size are in bytes
  void storeToMemoryImmOffset(Value *base, u_int64_t offset, u_int64_t size,
                              Value *val) {
    // Get offset as a 64-bit LLVM constant
    auto offsetVal = getIntConst(offset, 64);

    // Create a GEP instruction based on a byte addressing basis (8 bits)
    // returning pointer to base + offset
    assert(base);
    auto ptr = createGEP(getIntTy(8), base, {offsetVal}, "");

    // Store Value val in the pointer returned by the GEP instruction
    createStore(val, ptr);
  }

public:
  arm2llvm(Module *LiftedModule, MCFunction &MF, Function &srcFn,
           MCInstPrinter *instrPrinter)
      : LiftedModule(LiftedModule), MF(MF), srcFn(srcFn),
        instrPrinter(instrPrinter), DL(srcFn.getParent()->getDataLayout()) {

    // sanity checking
    assert(disjoint(instrs_32, instrs_64));
    assert(disjoint(instrs_32, instrs_128));
    assert(disjoint(instrs_64, instrs_128));
    *out << (instrs_32.size() + instrs_64.size() + instrs_128.size())
         << " instructions supported\n";

    // we'll want this later
    vector<Type *> args{getIntTy(1)};
    FunctionType *assertTy =
        FunctionType::get(Type::getVoidTy(Ctx), args, false);
    assertDecl = Function::Create(assertTy, Function::ExternalLinkage,
                                  "llvm.assert", LiftedModule);
  }

  // Visit an MCInst and convert it to LLVM IR
  // See: https://documentation-service.arm.com/static/6245e8f0f7d10f7540e0c054
  void liftInst(MCInst &I) {
    auto opcode = I.getOpcode();
    PrevInst = CurInst;
    CurInst = &I;

    auto i1 = getIntTy(1);
    auto i8 = getIntTy(8);
    auto i16 = getIntTy(16);
    auto i32 = getIntTy(32);
    auto i64 = getIntTy(64);
    auto i128 = getIntTy(128);

    switch (opcode) {

    // we're abusing this opcode -- better hope we don't run across these for
    // real
    case AArch64::SEH_Nop:
      break;

    case AArch64::BRK: {
      createTrap();
      break;
    }

    case AArch64::BL: {
      doCall();
      break;
    }

    case AArch64::B: {
      BasicBlock *dst{nullptr};
      // JDR: I don't understand this
      if (CurInst->getOperand(0).isImm()) {
        // handles the case when we add an entry block with no predecessors
        auto &dst_name = MF.BBs[getImm(0)].getName();
        dst = getBBByName(dst_name);
      } else {
        dst = getBB(CurInst->getOperand(0));
      }
      if (dst) {
        createBranch(dst);
      } else {
        // ok, if we don't have a destination block then we left this
        // dangling on purpose, with the assumption that it's a tail
        // call
        doCall();
        doReturn();
      }
      break;
    }

    case AArch64::BR: {
      *out << "\nERROR: BR not supported\n\n";
      exit(-1);
    }

    case AArch64::BLR: {
      *out << "\nERROR: BLR not supported\n\n";
      exit(-1);
    }

    case AArch64::RET:
      doReturn();
      break;

    case AArch64::Bcc: {
      auto cond_val_imm = getImm(0);
      auto cond_val = conditionHolds(cond_val_imm);

      auto &jmp_tgt_op = CurInst->getOperand(1);
      assert(jmp_tgt_op.isExpr() && "expected expression");
      assert((jmp_tgt_op.getExpr()->getKind() == MCExpr::ExprKind::SymbolRef) &&
             "expected symbol ref as bcc operand");
      const MCSymbolRefExpr &SRE = cast<MCSymbolRefExpr>(*jmp_tgt_op.getExpr());
      const MCSymbol &Sym = SRE.getSymbol();

      auto *dst_true = getBBByName(Sym.getName());

      assert(MCBB->getSuccs().size() == 1 || MCBB->getSuccs().size() == 2);
      const string *dst_false_name = nullptr;
      for (auto &succ : MCBB->getSuccs()) {
        if (succ->getName() != Sym.getName()) {
          dst_false_name = &succ->getName();
          break;
        }
      }
      auto *dst_false =
          getBBByName(dst_false_name ? *dst_false_name : Sym.getName());

      createBranch(cond_val, dst_true, dst_false);
      break;
    }

    case AArch64::MRS: {
      // https://developer.arm.com/documentation/ddi0595/2021-06/AArch64-Registers/NZCV--Condition-Flags
      auto imm = getImm(1);
      if (imm != 55824) {
        *out << "\nERROR: NZCV is the only supported case for MRS\n\n";
        exit(-1);
      }

      auto N = createZExt(getN(), i64);
      auto Z = createZExt(getZ(), i64);
      auto C = createZExt(getC(), i64);
      auto V = createZExt(getV(), i64);

      auto NS = createMaskedShl(N, getIntConst(31, 64));
      auto NZ = createMaskedShl(Z, getIntConst(30, 64));
      auto NC = createMaskedShl(C, getIntConst(29, 64));
      auto NV = createMaskedShl(V, getIntConst(28, 64));

      Value *res = getIntConst(0, 64);
      res = createOr(res, NS);
      res = createOr(res, NZ);
      res = createOr(res, NC);
      res = createOr(res, NV);
      updateOutputReg(res);
      break;
    }

    case AArch64::MSR: {
      // https://developer.arm.com/documentation/ddi0595/2021-06/AArch64-Registers/NZCV--Condition-Flags
      auto imm = getImm(0);
      if (imm != 55824) {
        *out << "\nERROR: NZCV is the only supported case for MSR\n\n";
        exit(-1);
      }

      auto i64_0 = getIntConst(0, 64);
      auto i64_1 = getIntConst(1, 64);

      auto Nmask = createMaskedShl(i64_1, getIntConst(31, 64));
      auto Zmask = createMaskedShl(i64_1, getIntConst(30, 64));
      auto Cmask = createMaskedShl(i64_1, getIntConst(29, 64));
      auto Vmask = createMaskedShl(i64_1, getIntConst(28, 64));

      auto reg = readFromOperand(1);
      auto Nval = createAnd(Nmask, reg);
      auto Zval = createAnd(Zmask, reg);
      auto Cval = createAnd(Cmask, reg);
      auto Vval = createAnd(Vmask, reg);

      setN(createICmp(ICmpInst::Predicate::ICMP_NE, Nval, i64_0));
      setZ(createICmp(ICmpInst::Predicate::ICMP_NE, Zval, i64_0));
      setC(createICmp(ICmpInst::Predicate::ICMP_NE, Cval, i64_0));
      setV(createICmp(ICmpInst::Predicate::ICMP_NE, Vval, i64_0));
      break;
    }

    case AArch64::ADDWrs:
    case AArch64::ADDWri:
    case AArch64::ADDWrx:
    case AArch64::ADDSWrs:
    case AArch64::ADDSWri:
    case AArch64::ADDSWrx:
    case AArch64::ADDXrs:
    case AArch64::ADDXri:
    case AArch64::ADDXrx:
    case AArch64::ADDSXrs:
    case AArch64::ADDSXri:
    case AArch64::ADDSXrx: {
      Value *a = nullptr;
      Value *b = nullptr;
      bool break_outer_switch = false;

      switch (opcode) {
      case AArch64::ADDWrx:
      case AArch64::ADDSWrx:
      case AArch64::ADDXrx:
      case AArch64::ADDSXrx: {
        auto size = getInstSize(opcode);
        auto ty = getIntTy(size);
        auto extendImm = getImm(3);
        auto extendType = ((extendImm >> 3) & 0x7);
        auto isSigned = (extendType & 0x4) != 0;

        // extendSize is necessary so that we can start with the word size
        // ARM wants us to (byte, half, full) and then sign extend to a new
        // size. Without extendSize being used for a trunc, a lot of masking
        // and more manual work to sign extend would be necessary
        unsigned extendSize = 8 << (extendType & 0x3);
        auto shift = extendImm & 0x7;

        b = readFromOperand(2);

        // Make sure to not to trunc to the same size as the parameter.
        // Sometimes ADDrx is generated using 32 bit registers and "extends" to
        // a 32 bit value. This is seen as a type error by LLVM, but is valid
        // ARM
        if (extendSize != (unsigned)size) {
          auto truncType = getIntTy(extendSize);
          b = createTrunc(b, truncType);
          b = createCast(b, ty,
                         isSigned ? Instruction::SExt : Instruction::ZExt);
        }

        // shift may not be there, it may just be the extend
        if (shift != 0)
          b = createMaskedShl(b, getIntConst(shift, size));
        break;
      }
      default:
        b = readFromOperand(2);
        b = regShift(b, getImm(3));
        if (b->getType()->isPointerTy()) {
          // This control path is for PC-Relative addressing.
          auto reg = CurInst->getOperand(0).getReg();
          updateReg(b, reg);
          break_outer_switch = true;
          break;
        }
        break;
      }
      // Let the PC-Relative addressing control path break here instead of the
      // end of the case as we do not want any more instructions created.
      if (break_outer_switch)
        break;

      a = readFromOperand(1);

      if (has_s(opcode)) {
        auto sadd = createSAddOverflow(a, b);
        auto result = createExtractValue(sadd, {0});
        auto new_v = createExtractValue(sadd, {1});

        auto uadd = createUAddOverflow(a, b);
        auto new_c = createExtractValue(uadd, {1});

        setV(new_v);
        setC(new_c);
        setNUsingResult(result);
        setZUsingResult(result);
        updateOutputReg(result);
      }

      updateOutputReg(createAdd(a, b));
      break;
    }

    case AArch64::ADCXr:
    case AArch64::ADCWr:
    case AArch64::ADCSXr:
    case AArch64::ADCSWr: {
      auto a = readFromOperand(1);
      auto b = readFromOperand(2);

      auto [res, flags] = addWithCarry(a, b, getC());
      updateOutputReg(res);

      if (has_s(opcode)) {
        auto [n, z, c, v] = flags;
        setN(n);
        setZ(z);
        setC(c);
        setV(v);
      }

      break;
    }

    case AArch64::ASRVWr:
    case AArch64::ASRVXr: {
      auto size = getInstSize(opcode);
      auto a = readFromOperand(1);
      auto b = readFromOperand(2);

      auto shift_amt =
          createBinop(b, getIntConst(size, size), Instruction::URem);
      auto res = createMaskedAShr(a, shift_amt);
      updateOutputReg(res);
      break;
    }

      // SUBrx is a subtract instruction with an extended register.
      // ARM has 8 types of extensions:
      // 000 -> uxtb
      // 001 -> uxth
      // 010 -> uxtw
      // 011 -> uxtx
      // 100 -> sxtb
      // 110 -> sxth
      // 101 -> sxtw
      // 111 -> sxtx
      // To figure out if the extension is signed, we can use (extendType / 4)
      // Since the types repeat byte, half word, word, etc. for signed and
      // unsigned extensions, we can use 8 << (extendType & 0x3) to calculate
      // the extension's byte size
    case AArch64::SUBWri:
    case AArch64::SUBWrs:
    case AArch64::SUBWrx:
    case AArch64::SUBSWrs:
    case AArch64::SUBSWri:
    case AArch64::SUBSWrx:
    case AArch64::SUBXri:
    case AArch64::SUBXrs:
    case AArch64::SUBXrx:
    case AArch64::SUBSXrs:
    case AArch64::SUBSXri:
    case AArch64::SUBSXrx: {
      auto size = getInstSize(opcode);
      auto ty = getIntTy(size);
      assert(CurInst->getNumOperands() == 4); // dst, lhs, rhs, shift amt
      assert(CurInst->getOperand(3).isImm());

      // convert lhs, rhs operands to IR::Values
      auto a = readFromOperand(1);
      Value *b = nullptr;
      switch (opcode) {
      case AArch64::SUBWrx:
      case AArch64::SUBSWrx:
      case AArch64::SUBXrx:
      case AArch64::SUBSXrx: {
        auto extendImm = getImm(3);
        auto extendType = (extendImm >> 3) & 0x7;
        auto isSigned = extendType / 4;
        // extendSize is necessary so that we can start with the word size
        // ARM wants us to (byte, half, full) and then sign extend to a new
        // size. Without extendSize being used for a trunc, a lot of masking
        // and more manual work to sign extend would be necessary
        unsigned extendSize = 8 << (extendType & 0x3);
        auto shift = extendImm & 0x7;
        b = readFromOperand(2);

        // Make sure to not to trunc to the same size as the parameter.
        // Sometimes SUBrx is generated using 32 bit registers and "extends" to
        // a 32 bit value. This is seen as a type error by LLVM, but is valid
        // ARM
        if (extendSize != ty->getIntegerBitWidth()) {
          auto truncType = getIntTy(extendSize);
          b = createTrunc(b, truncType);
          b = createCast(b, ty,
                         isSigned ? Instruction::SExt : Instruction::ZExt);
        }

        // shift may not be there, it may just be the extend
        if (shift != 0)
          b = createMaskedShl(b, getIntConst(shift, size));
        break;
      }
      default:
        b = readFromOperand(2);
        b = regShift(b, getImm(3));
      }

      // make sure that lhs and rhs conversion succeeded, type lookup succeeded
      if (!ty || !a || !b)
        visitError();

      if (has_s(opcode)) {
        auto ssub = createSSubOverflow(a, b);
        auto result = createExtractValue(ssub, {0});
        auto new_v = createExtractValue(ssub, {1});
        setC(createICmp(ICmpInst::Predicate::ICMP_UGE, a, b));
        setZ(createICmp(ICmpInst::Predicate::ICMP_EQ, a, b));
        setV(new_v);
        setNUsingResult(result);
        updateOutputReg(result);
      } else {
        auto sub = createSub(a, b);
        updateOutputReg(sub);
      }
      break;
    }

    case AArch64::FCSELDrrr:
    case AArch64::FCSELSrrr:
    case AArch64::CSELWr:
    case AArch64::CSELXr: {
      assert(CurInst->getNumOperands() == 4); // dst, lhs, rhs, cond
      // TODO decode condition and find the approprate cond val
      assert(CurInst->getOperand(1).isReg() && CurInst->getOperand(2).isReg());
      assert(CurInst->getOperand(3).isImm());

      auto a = readFromOperand(1);
      auto b = readFromOperand(2);
      if (!a || !b)
        visitError();

      auto cond_val_imm = getImm(3);
      auto cond_val = conditionHolds(cond_val_imm);

      auto result = createSelect(cond_val, a, b);
      updateOutputReg(result);
      break;
    }

    case AArch64::ANDWri:
    case AArch64::ANDWrr:
    case AArch64::ANDWrs:
    case AArch64::ANDSWri:
    case AArch64::ANDSWrr:
    case AArch64::ANDSWrs:
    case AArch64::ANDXri:
    case AArch64::ANDXrr:
    case AArch64::ANDXrs:
    case AArch64::ANDSXri:
    case AArch64::ANDSXrr:
    case AArch64::ANDSXrs: {
      auto size = getInstSize(opcode);
      Value *rhs = nullptr;
      if (CurInst->getOperand(2).isImm()) {
        auto [wmask, _] = decodeBitMasks(getImm(2), size);
        rhs = getIntConst(wmask, size);
      } else {
        rhs = readFromOperand(2);
      }

      // We are in a ANDrs case. We need to handle a shift
      if (CurInst->getNumOperands() == 4) {
        // the 4th operand (if it exists) must be an immediate
        assert(CurInst->getOperand(3).isImm());
        rhs = regShift(rhs, getImm(3));
      }

      auto and_op = createAnd(readFromOperand(1), rhs);

      if (has_s(opcode)) {
        setNUsingResult(and_op);
        setZUsingResult(and_op);
        setC(getIntConst(0, 1));
        setV(getIntConst(0, 1));
      }

      updateOutputReg(and_op);
      break;
    }

    case AArch64::MADDWrrr:
    case AArch64::MADDXrrr: {
      auto mul_lhs = readFromOperand(1);
      auto mul_rhs = readFromOperand(2);
      auto addend = readFromOperand(3);

      auto mul = createMul(mul_lhs, mul_rhs);
      auto add = createAdd(mul, addend);
      updateOutputReg(add);
      break;
    }

    case AArch64::UMADDLrrr: {
      auto size = getInstSize(opcode);
      auto mul_lhs = readFromOperand(1);
      auto mul_rhs = readFromOperand(2);
      auto addend = readFromOperand(3);

      auto lhs_masked = createAnd(mul_lhs, getIntConst(0xffffffffUL, size));
      auto rhs_masked = createAnd(mul_rhs, getIntConst(0xffffffffUL, size));
      auto mul = createMul(lhs_masked, rhs_masked);
      auto add = createAdd(mul, addend);
      updateOutputReg(add);
      break;
    }

    case AArch64::SMADDLrrr: {
      // Signed Multiply-Add Long multiplies two 32-bit register values,
      // adds a 64-bit register value, and writes the result to the 64-bit
      // destination register.
      auto mul_lhs = readFromOperand(1);
      auto mul_rhs = readFromOperand(2);
      auto addend = readFromOperand(3);

      // The inputs are automatically zero extended, but we want sign extension,
      // so we need to truncate them back to i32s
      auto lhs_trunc = createTrunc(mul_lhs, i32);
      auto rhs_trunc = createTrunc(mul_rhs, i32);

      // For signed multiplication, must sign extend the lhs and rhs to not
      // overflow
      auto lhs_ext = createSExt(lhs_trunc, i64);
      auto rhs_ext = createSExt(rhs_trunc, i64);

      auto mul = createMul(lhs_ext, rhs_ext);
      auto add = createAdd(mul, addend);
      updateOutputReg(add);
      break;
    }

    case AArch64::SMSUBLrrr:
    case AArch64::UMSUBLrrr: {
      // SMSUBL: Signed Multiply-Subtract Long.
      // UMSUBL: Unsigned Multiply-Subtract Long.
      auto *mul_lhs = readFromOperand(1);
      auto *mul_rhs = readFromOperand(2);
      auto *minuend = readFromOperand(3);

      // The inputs are automatically zero extended, but we want sign
      // extension for signed, so we need to truncate them back to i32s
      auto lhs_trunc = createTrunc(mul_lhs, i32);
      auto rhs_trunc = createTrunc(mul_rhs, i32);

      Value *lhs_extended = nullptr;
      Value *rhs_extended = nullptr;
      if (opcode == AArch64::SMSUBLrrr) {
        // For signed multiplication, must sign extend the lhs and rhs to not
        // overflow
        lhs_extended = createSExt(lhs_trunc, i64);
        rhs_extended = createSExt(rhs_trunc, i64);
      } else {
        lhs_extended = createZExt(lhs_trunc, i64);
        rhs_extended = createZExt(rhs_trunc, i64);
      }

      auto mul = createMul(lhs_extended, rhs_extended);
      auto subtract = createSub(minuend, mul);
      updateOutputReg(subtract);
      break;
    }

    case AArch64::SMULHrr:
    case AArch64::UMULHrr: {
      // SMULH: Signed Multiply High
      // UMULH: Unsigned Multiply High
      auto mul_lhs = readFromOperand(1);
      auto mul_rhs = readFromOperand(2);

      // For unsigned multiplication, must zero extend the lhs and rhs to not
      // overflow For signed multiplication, must sign extend the lhs and rhs to
      // not overflow
      Value *lhs_extended = nullptr, *rhs_extended = nullptr;
      if (opcode == AArch64::UMULHrr) {
        lhs_extended = createZExt(mul_lhs, i128);
        rhs_extended = createZExt(mul_rhs, i128);
      } else {
        lhs_extended = createSExt(mul_lhs, i128);
        rhs_extended = createSExt(mul_rhs, i128);
      }

      auto mul = createMul(lhs_extended, rhs_extended);
      // After multiplying, shift down 64 bits to get the top half of the i128
      // into the bottom half
      auto shift = createMaskedLShr(mul, getIntConst(64, 128));

      // Truncate to the proper size:
      auto trunc = createTrunc(shift, i64);
      updateOutputReg(trunc);
      break;
    }

    case AArch64::MSUBWrrr:
    case AArch64::MSUBXrrr: {
      auto mul_lhs = readFromOperand(1);
      auto mul_rhs = readFromOperand(2);
      auto minuend = readFromOperand(3);
      auto mul = createMul(mul_lhs, mul_rhs);
      auto sub = createSub(minuend, mul);
      updateOutputReg(sub);
      break;
    }

    case AArch64::SBFMWri:
    case AArch64::SBFMXri: {
      auto size = getInstSize(opcode);
      auto ty = getIntTy(size);
      auto src = readFromOperand(1);
      auto immr = getImm(2);
      auto imms = getImm(3);

      auto r = getIntConst(immr, size);

      // arithmetic shift right (ASR) alias is perferred when:
      // imms == 011111 and size == 32 or when imms == 111111 and size = 64
      if ((size == 32 && imms == 31) || (size == 64 && imms == 63)) {
        auto dst = createMaskedAShr(src, r);
        updateOutputReg(dst);
        break;
      }

      // SXTB
      if (immr == 0 && imms == 7) {
        auto trunc = createTrunc(src, i8);
        auto dst = createSExt(trunc, ty);
        updateOutputReg(dst);
        break;
      }

      // SXTH
      if (immr == 0 && imms == 15) {
        auto trunc = createTrunc(src, i16);
        auto dst = createSExt(trunc, ty);
        updateOutputReg(dst);
        break;
      }

      // SXTW
      if (immr == 0 && imms == 31) {
        auto trunc = createTrunc(src, i32);
        auto dst = createSExt(trunc, ty);
        updateOutputReg(dst);
        break;
      }

      // SBFIZ
      if (imms < immr) {
        auto pos = size - immr;
        auto width = imms + 1;
        auto mask = ((uint64_t)1 << (width)) - 1;
        auto bitfield_mask = (uint64_t)1 << (width - 1);

        auto masked = createAnd(src, getIntConst(mask, size));
        auto bitfield_lsb = createAnd(src, getIntConst(bitfield_mask, size));
        auto insert_ones = createOr(masked, getIntConst(~mask, size));
        auto bitfield_lsb_set = createICmp(ICmpInst::Predicate::ICMP_NE,
                                           bitfield_lsb, getIntConst(0, size));
        auto res = createSelect(bitfield_lsb_set, insert_ones, masked);
        auto shifted_res = createMaskedShl(res, getIntConst(pos, size));
        updateOutputReg(shifted_res);
        break;
      }
      // FIXME: this requires checking if SBFX is preferred.
      // For now, assume this is always SBFX
      auto width = imms + 1;
      auto mask = ((uint64_t)1 << (width)) - 1;
      auto pos = immr;

      //*out << "SBFX:\n";
      //*out << "size = " << size << "\n";
      //*out << "width = " << width << "\n";
      //*out << "pos = " << pos << "\n";

      auto masked = createAnd(src, getIntConst(mask, size));
      auto l_shifted = createRawShl(masked, getIntConst(size - width, size));
      auto shifted_res =
          createRawAShr(l_shifted, getIntConst(size - width + pos, size));
      updateOutputReg(shifted_res);
      break;
    }

    case AArch64::CCMPWi:
    case AArch64::CCMPWr:
    case AArch64::CCMPXi:
    case AArch64::CCMPXr: {
      assert(CurInst->getNumOperands() == 4);

      auto lhs = readFromOperand(0);
      auto imm_rhs = readFromOperand(1);

      if (!lhs || !imm_rhs)
        visitError();

      auto imm_flags = getImm(2);
      auto imm_v_val = getIntConst((imm_flags & 1) ? 1 : 0, 1);
      auto imm_c_val = getIntConst((imm_flags & 2) ? 1 : 0, 1);
      auto imm_z_val = getIntConst((imm_flags & 4) ? 1 : 0, 1);
      auto imm_n_val = getIntConst((imm_flags & 8) ? 1 : 0, 1);

      auto cond_val_imm = getImm(3);
      auto cond_val = conditionHolds(cond_val_imm);

      auto ssub = createSSubOverflow(lhs, imm_rhs);
      auto result = createExtractValue(ssub, {0});
      auto zero_val = getIntConst(0, getBitWidth(result));

      auto new_n = createICmp(ICmpInst::Predicate::ICMP_SLT, result, zero_val);
      auto new_z = createICmp(ICmpInst::Predicate::ICMP_EQ, lhs, imm_rhs);
      auto new_c = createICmp(ICmpInst::Predicate::ICMP_UGE, lhs, imm_rhs);
      auto new_v = createExtractValue(ssub, {1});

      auto new_n_flag = createSelect(cond_val, new_n, imm_n_val);
      auto new_z_flag = createSelect(cond_val, new_z, imm_z_val);
      auto new_c_flag = createSelect(cond_val, new_c, imm_c_val);
      auto new_v_flag = createSelect(cond_val, new_v, imm_v_val);

      setN(new_n_flag);
      setZ(new_z_flag);
      setC(new_c_flag);
      setV(new_v_flag);
      break;
    }

    case AArch64::EORWri:
    case AArch64::EORXri: {
      auto size = getInstSize(opcode);
      assert(CurInst->getNumOperands() == 3); // dst, src, imm
      assert(CurInst->getOperand(1).isReg() && CurInst->getOperand(2).isImm());

      auto a = readFromOperand(1);
      auto [wmask, _] = decodeBitMasks(getImm(2), size);
      auto imm_val = getIntConst(wmask,
                                 size); // FIXME, need to decode immediate val
      if (!a || !imm_val)
        visitError();

      auto res = createXor(a, imm_val);
      updateOutputReg(res);
      break;
    }
    case AArch64::EORWrs:
    case AArch64::EORXrs: {
      auto lhs = readFromOperand(1);
      auto rhs = readFromOperand(2);
      rhs = regShift(rhs, getImm(3));
      auto result = createXor(lhs, rhs);
      updateOutputReg(result);
      break;
    }

    case AArch64::CCMNWi:
    case AArch64::CCMNWr:
    case AArch64::CCMNXi:
    case AArch64::CCMNXr: {
      auto a = readFromOperand(0);
      auto b = readFromOperand(1);
      auto nzcv = getImm(2);
      auto cond_val_imm = getImm(3);

      auto zero = getIntConst(0, 1);
      auto one = getIntConst(1, 1);

      auto [res, flags] = addWithCarry(a, b, zero);
      auto [n, z, c, v] = flags;

      auto cond = conditionHolds(cond_val_imm);
      setN(createSelect(cond, n, (nzcv & 8) ? one : zero));
      setZ(createSelect(cond, z, (nzcv & 4) ? one : zero));
      setC(createSelect(cond, c, (nzcv & 2) ? one : zero));
      setV(createSelect(cond, v, (nzcv & 1) ? one : zero));

      break;
    }

    case AArch64::CSINVWr:
    case AArch64::CSINVXr:
    case AArch64::CSNEGWr:
    case AArch64::CSNEGXr: {
      auto size = getInstSize(opcode);
      // csinv dst, a, b, cond
      // if (cond) a else ~b
      assert(CurInst->getNumOperands() == 4); // dst, lhs, rhs, cond
      // TODO decode condition and find the approprate cond val
      assert(CurInst->getOperand(1).isReg() && CurInst->getOperand(2).isReg());
      assert(CurInst->getOperand(3).isImm());

      auto a = readFromOperand(1);
      auto b = readFromOperand(2);
      if (!a || !b)
        visitError();

      auto cond_val_imm = getImm(3);
      auto cond_val = conditionHolds(cond_val_imm);

      auto neg_one = getIntConst(-1, size);
      auto inverted_b = createXor(b, neg_one);

      if (opcode == AArch64::CSNEGWr || opcode == AArch64::CSNEGXr) {
        auto negated_b = createAdd(inverted_b, getIntConst(1, size));
        auto ret = createSelect(cond_val, a, negated_b);
        updateOutputReg(ret);
      } else {
        auto ret = createSelect(cond_val, a, inverted_b);
        updateOutputReg(ret);
      }
      break;
    }

    case AArch64::CSINCWr:
    case AArch64::CSINCXr: {
      auto size = getInstSize(opcode);
      assert(CurInst->getOperand(1).isReg() && CurInst->getOperand(2).isReg());
      assert(CurInst->getOperand(3).isImm());

      auto a = readFromOperand(1);
      auto b = readFromOperand(2);

      auto cond_val_imm = getImm(3);
      auto cond_val = conditionHolds(cond_val_imm);

      auto inc = createAdd(b, getIntConst(1, size));
      auto sel = createSelect(cond_val, a, inc);

      updateOutputReg(sel);
      break;
    }

    case AArch64::MOVZWi:
    case AArch64::MOVZXi: {
      auto size = getInstSize(opcode);
      assert(CurInst->getOperand(0).isReg());
      assert(CurInst->getOperand(1).isImm());
      auto lhs = readFromOperand(1);
      lhs = regShift(lhs, getImm(2));
      auto rhs = getIntConst(0, size);
      auto ident = createAdd(lhs, rhs);
      updateOutputReg(ident);
      break;
    }

    case AArch64::MOVNWi:
    case AArch64::MOVNXi: {
      auto size = getInstSize(opcode);
      assert(CurInst->getOperand(0).isReg());
      assert(CurInst->getOperand(1).isImm());
      assert(CurInst->getOperand(2).isImm());

      auto lhs = readFromOperand(1);
      lhs = regShift(lhs, getImm(2));
      auto neg_one = getIntConst(-1, size);
      auto not_lhs = createXor(lhs, neg_one);

      updateOutputReg(not_lhs);
      break;
    }

    case AArch64::LSLVWr:
    case AArch64::LSLVXr: {
      auto lhs = readFromOperand(1);
      auto rhs = readFromOperand(2);
      auto exp = createMaskedShl(lhs, rhs);
      updateOutputReg(exp);
      break;
    }

    case AArch64::LSRVWr:
    case AArch64::LSRVXr: {
      auto lhs = readFromOperand(1);
      auto rhs = readFromOperand(2);
      auto exp = createMaskedLShr(lhs, rhs);
      updateOutputReg(exp);
      break;
    }

    case AArch64::ORNWrs:
    case AArch64::ORNXrs: {
      auto size = getInstSize(opcode);
      auto lhs = readFromOperand(1);
      auto rhs = readFromOperand(2);
      rhs = regShift(rhs, getImm(3));

      auto neg_one = getIntConst(-1, size);
      auto not_rhs = createXor(rhs, neg_one);
      auto ident = createOr(lhs, not_rhs);
      updateOutputReg(ident);
      break;
    }

    case AArch64::MOVKWi:
    case AArch64::MOVKXi: {
      auto size = getInstSize(opcode);
      auto dest = readFromOperand(1);
      auto lhs = readFromOperand(2);
      lhs = regShift(lhs, getImm(3));

      uint64_t bitmask;
      auto shift_amt = getImm(3);

      if (opcode == AArch64::MOVKWi) {
        assert(shift_amt == 0 || shift_amt == 16);
        bitmask = (shift_amt == 0) ? 0xffff0000 : 0x0000ffff;
      } else {
        assert(shift_amt == 0 || shift_amt == 16 || shift_amt == 32 ||
               shift_amt == 48);
        bitmask = ~(((uint64_t)0xffff) << shift_amt);
      }

      auto bottom_bits = getIntConst(bitmask, size);
      auto cleared = createAnd(dest, bottom_bits);
      auto ident = createOr(cleared, lhs);
      updateOutputReg(ident);
      break;
    }

    case AArch64::UBFMWri:
    case AArch64::UBFMXri: {
      auto size = getInstSize(opcode);
      auto src = readFromOperand(1);
      auto immr = getImm(2);
      auto imms = getImm(3);

      // LSL is preferred when imms != 31 and imms + 1 == immr
      if (size == 32 && imms != 31 && imms + 1 == immr) {
        auto dst = createMaskedShl(src, getIntConst(31 - imms, size));
        updateOutputReg(dst);
        break;
      }

      // LSL is preferred when imms != 63 and imms + 1 == immr
      if (size == 64 && imms != 63 && imms + 1 == immr) {
        auto dst = createMaskedShl(src, getIntConst(63 - imms, size));
        updateOutputReg(dst);
        break;
      }

      // LSR is preferred when imms == 31 or 63 (size - 1)
      if (imms == size - 1) {
        auto dst = createMaskedLShr(src, getIntConst(immr, size));
        updateOutputReg(dst);
        break;
      }

      // UBFIZ
      if (imms < immr) {
        auto pos = size - immr;
        auto width = imms + 1;
        auto mask = ((uint64_t)1 << (width)) - 1;
        auto masked = createAnd(src, getIntConst(mask, size));
        auto shifted = createMaskedShl(masked, getIntConst(pos, size));
        updateOutputReg(shifted);
        break;
      }

      // UXTB
      if (immr == 0 && imms == 7) {
        auto mask = ((uint64_t)1 << 8) - 1;
        auto masked = createAnd(src, getIntConst(mask, size));
        updateOutputReg(masked);
        break;
      }

      // UXTH
      if (immr == 0 && imms == 15) {
        auto mask = ((uint64_t)1 << 16) - 1;
        auto masked = createAnd(src, getIntConst(mask, size));
        updateOutputReg(masked);
        break;
      }

      // UBFX
      // FIXME: this requires checking if UBFX is preferred.
      // For now, assume this is always UBFX
      // we mask from lsb to lsb + width and then perform a logical shift right
      auto width = imms + 1;
      auto mask = ((uint64_t)1 << (width)) - 1;
      auto pos = immr;

      auto masked = createAnd(src, getIntConst(mask, size));
      auto shifted_res = createMaskedLShr(masked, getIntConst(pos, size));
      updateOutputReg(shifted_res);
      break;
    }

    case AArch64::BFMWri:
    case AArch64::BFMXri: {
      auto size = getInstSize(opcode);
      auto dst = readFromOperand(1);
      auto src = readFromOperand(2);

      auto immr = getImm(3);
      auto imms = getImm(4);

      // FIXME -- would be better to use decodeBitMasks() here, as
      // shown in the ARM docs

      if (imms >= immr) {
        // BFXIL

        auto bits = (imms - immr + 1);
        auto pos = immr;

        auto mask = (((uint64_t)1 << bits) - 1) << pos;

        auto masked = createAnd(src, getIntConst(mask, size));
        auto shifted = createMaskedLShr(masked, getIntConst(pos, size));
        auto cleared =
            createAnd(dst, getIntConst((uint64_t)(-1) << bits, size));
        auto res = createOr(cleared, shifted);
        updateOutputReg(res);
      } else {
        auto bits = imms + 1;
        auto pos = size - immr;

        // This mask deletes `bits` number of bits starting at `pos`.
        // If the mask is for a 32 bit value, it will chop off the top 32 bits
        // of the 64 bit mask to keep the mask to a size of 32 bits
        auto mask = ~((((uint64_t)1 << bits) - 1) << pos) &
                    ((uint64_t)-1 >> (64 - size));

        // get `bits` number of bits from the least significant bits
        auto bitfield =
            createAnd(src, getIntConst(~((uint64_t)-1 << bits), size));

        // move the bitfield into position
        auto moved = createMaskedShl(bitfield, getIntConst(pos, size));

        // carve out a place for the bitfield
        auto masked = createAnd(dst, getIntConst(mask, size));
        // place the bitfield
        auto res = createOr(masked, moved);
        updateOutputReg(res);
      }
      break;
    }

    case AArch64::ORRWri:
    case AArch64::ORRXri: {
      auto size = getInstSize(opcode);
      auto lhs = readFromOperand(1);
      auto imm = getImm(2);
      auto [wmask, _] = decodeBitMasks(imm, size);
      auto result = createOr(lhs, getIntConst(wmask, size));
      updateOutputReg(result);
      break;
    }

    case AArch64::ORRWrs:
    case AArch64::ORRXrs: {
      auto lhs = readFromOperand(1);
      auto rhs = readFromOperand(2);
      rhs = regShift(rhs, getImm(3));
      auto result = createOr(lhs, rhs);
      updateOutputReg(result);
      break;
    }

    // can't directly lift ARM sdiv to LLVM sdiv because the latter
    // is UB for divide by zero and INT_MIN / -1
    case AArch64::SDIVWr:
    case AArch64::SDIVXr: {
      auto Size = getInstSize(opcode);
      auto Zero = getIntConst(0, Size);
      auto AllOnes = createSub(Zero, getIntConst(1, Size));
      auto IntMin =
          createMaskedShl(getIntConst(1, Size), getIntConst(Size - 1, Size));
      auto LHS = readFromOperand(1);
      auto RHS = readFromOperand(2);
      auto ResMem = createAlloca(getIntTy(Size), getIntConst(1, 64), "");
      createStore(Zero, ResMem);
      auto RHSIsZero = createICmp(ICmpInst::Predicate::ICMP_EQ, RHS, Zero);
      auto LHSIsIntMin = createICmp(ICmpInst::Predicate::ICMP_EQ, LHS, IntMin);
      auto RHSIsAllOnes =
          createICmp(ICmpInst::Predicate::ICMP_EQ, RHS, AllOnes);
      auto IsOverflow = createAnd(LHSIsIntMin, RHSIsAllOnes);
      auto Cond = createOr(RHSIsZero, IsOverflow);
      auto DivBB = BasicBlock::Create(Ctx, "", liftedFn);
      auto ContBB = BasicBlock::Create(Ctx, "", liftedFn);
      createBranch(Cond, ContBB, DivBB);
      LLVMBB = DivBB;
      auto DivResult = createSDiv(LHS, RHS);
      createStore(DivResult, ResMem);
      createBranch(ContBB);
      LLVMBB = ContBB;
      auto result = createLoad(getIntTy(Size), ResMem);
      updateOutputReg(result);
      break;
    }

    // can't directly lift ARM udiv to LLVM udiv because the latter
    // is UB for divide by zero
    case AArch64::UDIVWr:
    case AArch64::UDIVXr: {
      auto size = getInstSize(opcode);
      auto zero = getIntConst(0, size);
      auto lhs = readFromOperand(1);
      auto rhs = readFromOperand(2);
      auto A = createAlloca(getIntTy(size), getIntConst(1, 64), "");
      createStore(zero, A);
      auto rhsIsZero = createICmp(ICmpInst::Predicate::ICMP_EQ, rhs, zero);
      auto DivBB = BasicBlock::Create(Ctx, "", liftedFn);
      auto ContBB = BasicBlock::Create(Ctx, "", liftedFn);
      createBranch(rhsIsZero, ContBB, DivBB);
      LLVMBB = DivBB;
      auto divResult = createUDiv(lhs, rhs);
      createStore(divResult, A);
      createBranch(ContBB);
      LLVMBB = ContBB;
      auto result = createLoad(getIntTy(size), A);
      updateOutputReg(result);
      break;
    }

    case AArch64::EXTRWrri:
    case AArch64::EXTRXrri: {
      auto op1 = readFromOperand(1);
      auto op2 = readFromOperand(2);
      auto shift = readFromOperand(3);
      auto result = createFShr(op1, op2, shift);
      updateOutputReg(result);
      break;
    }

    case AArch64::RORVWr:
    case AArch64::RORVXr: {
      auto op = readFromOperand(1);
      auto shift = readFromOperand(2);
      auto result = createFShr(op, op, shift);
      updateOutputReg(result);
      break;
    }

    case AArch64::RBITWr:
    case AArch64::RBITXr: {
      auto op = readFromOperand(1);
      auto result = createBitReverse(op);
      updateOutputReg(result);
      break;
    }

    case AArch64::REVWr:
    case AArch64::REVXr:
      updateOutputReg(createBSwap(readFromOperand(1)));
      break;

    case AArch64::CLZWr:
    case AArch64::CLZXr: {
      auto op = readFromOperand(1);
      auto result = createCtlz(op);
      updateOutputReg(result);
      break;
    }

    case AArch64::EONWrs:
    case AArch64::EONXrs:
    case AArch64::BICWrs:
    case AArch64::BICXrs:
    case AArch64::BICSWrs:
    case AArch64::BICSXrs: {
      auto size = getInstSize(opcode);
      // BIC:
      // return = op1 AND NOT (optional shift) op2
      // EON:
      // return = op1 XOR NOT (optional shift) op2

      auto op1 = readFromOperand(1);
      auto op2 = readFromOperand(2);

      // If there is a shift to be performed on the second operand
      if (CurInst->getNumOperands() == 4) {
        // the 4th operand (if it exists) must b an immediate
        assert(CurInst->getOperand(3).isImm());
        op2 = regShift(op2, getImm(3));
      }

      // Perform NOT
      auto neg_one = getIntConst(-1, size);
      auto inverted_op2 = createXor(op2, neg_one);

      // Perform final Op: AND for BIC, XOR for EON
      Value *ret = nullptr;
      switch (opcode) {
      case AArch64::BICWrs:
      case AArch64::BICXrs:
      case AArch64::BICSXrs:
      case AArch64::BICSWrs:
        ret = createAnd(op1, inverted_op2);
        break;
      case AArch64::EONWrs:
      case AArch64::EONXrs:
        ret = createXor(op1, inverted_op2);
        break;
      default:
        assert(false && "missed case in EON/BIC");
      }

      // FIXME: it might be better to have EON instruction separate since there
      //    no "S" instructions for EON
      if (has_s(opcode)) {
        setNUsingResult(ret);
        setZUsingResult(ret);
        setC(getIntConst(0, 1));
        setV(getIntConst(0, 1));
      }

      updateOutputReg(ret);
      break;
    }

    case AArch64::REV16Xr: {
      // REV16Xr: Reverse bytes of 64 bit value in 16-bit half-words.
      auto size = getInstSize(opcode);
      auto val = readFromOperand(1);
      auto first_part = createMaskedShl(val, getIntConst(8, size));
      auto first_part_and =
          createAnd(first_part, getIntConst(0xFF00FF00FF00FF00UL, size));
      auto second_part = createMaskedLShr(val, getIntConst(8, size));
      auto second_part_and =
          createAnd(second_part, getIntConst(0x00FF00FF00FF00FFUL, size));
      auto combined_val = createOr(first_part_and, second_part_and);
      updateOutputReg(combined_val);
      break;
    }

    case AArch64::REV16Wr:
    case AArch64::REV32Xr: {
      // REV16Wr: Reverse bytes of 32 bit value in 16-bit half-words.
      // REV32Xr: Reverse bytes of 64 bit value in 32-bit words.
      auto size = getInstSize(opcode);
      auto val = readFromOperand(1);

      // Reversing all of the bytes, then performing a rotation by half the
      // width reverses bytes in 16-bit halfwords for a 32 bit int and reverses
      // bytes in a 32-bit word for a 64 bit int
      auto reverse_val = createBSwap(val);
      auto ret =
          createFShr(reverse_val, reverse_val, getIntConst(size / 2, size));
      updateOutputReg(ret);
      break;
    }

    case AArch64::LDRSBXui:
    case AArch64::LDRSBWui:
    case AArch64::LDRSHXui:
    case AArch64::LDRSHWui:
    case AArch64::LDRSWui:
    case AArch64::LDRBBui:
    case AArch64::LDRBui:
    case AArch64::LDRHHui:
    case AArch64::LDRHui:
    case AArch64::LDRWui:
    case AArch64::LDRSui:
    case AArch64::LDRXui:
    case AArch64::LDRDui:
    case AArch64::LDRQui: {
      unsigned size;
      switch (opcode) {
      case AArch64::LDRBBui:
      case AArch64::LDRBui:
      case AArch64::LDRSBXui:
      case AArch64::LDRSBWui:
        size = 1;
        break;
      case AArch64::LDRHHui:
      case AArch64::LDRHui:
      case AArch64::LDRSHXui:
      case AArch64::LDRSHWui:
        size = 2;
        break;
      case AArch64::LDRWui:
      case AArch64::LDRSui:
      case AArch64::LDRSWui:
        size = 4;
        break;
      case AArch64::LDRXui:
      case AArch64::LDRDui:
        size = 8;
        break;
      case AArch64::LDRQui:
        size = 16;
        break;
      default:
        assert(false);
      }

      bool sExt = opcode == AArch64::LDRSBXui || opcode == AArch64::LDRSBWui ||
                  opcode == AArch64::LDRSHXui || opcode == AArch64::LDRSHWui ||
                  opcode == AArch64::LDRSWui;

      MCOperand &op2 = CurInst->getOperand(2);
      if (op2.isExpr()) {
        auto [globalVar, storePtr] = getExprVar(op2.getExpr());
        if (storePtr) {
          Value *ptrToInt = createPtrToInt(globalVar, getIntTy(size * 8));
          updateOutputReg(ptrToInt, sExt);
        } else {
          auto loaded = makeLoadWithOffset(globalVar, 0, size);
          updateOutputReg(loaded, sExt);
        }
      } else {
        auto [base, imm] = getParamsLoadImmed();
        auto loaded = makeLoadWithOffset(base, imm * size, size);
        updateOutputReg(loaded, sExt);
      }
      break;
    }

    case AArch64::LDURSBWi:
    case AArch64::LDURSBXi:
    case AArch64::LDURSHWi:
    case AArch64::LDURSHXi:
    case AArch64::LDURSWi:
    case AArch64::LDURBi:
    case AArch64::LDURBBi:
    case AArch64::LDURHi:
    case AArch64::LDURHHi:
    case AArch64::LDURSi:
    case AArch64::LDURWi:
    case AArch64::LDURDi:
    case AArch64::LDURXi:
    case AArch64::LDURQi: {
      unsigned size;
      if (opcode == AArch64::LDURBBi || opcode == AArch64::LDURBi ||
          opcode == AArch64::LDURSBWi || opcode == AArch64::LDURSBXi)
        size = 1;
      else if (opcode == AArch64::LDURHHi || opcode == AArch64::LDURHi ||
               opcode == AArch64::LDURSHWi || opcode == AArch64::LDURSHXi)
        size = 2;
      else if (opcode == AArch64::LDURWi || opcode == AArch64::LDURSi ||
               opcode == AArch64::LDURSWi)
        size = 4;
      else if (opcode == AArch64::LDURXi || opcode == AArch64::LDURDi)
        size = 8;
      else if (opcode == AArch64::LDURQi)
        size = 16;
      else
        assert(false);

      bool sExt = opcode == AArch64::LDURSBWi || opcode == AArch64::LDURSBXi ||
                  opcode == AArch64::LDURSHWi || opcode == AArch64::LDURSHXi ||
                  opcode == AArch64::LDURSWi;
      auto [base, imm] = getParamsLoadImmed();
      // Start offset as a 9-bit signed integer
      assert(imm <= 255 && imm >= -256);
      auto offset = getIntConst(imm, 9);
      Value *offsetVal = createSExt(offset, i64);
      auto loaded = makeLoadWithOffset(base, offsetVal, size);
      updateOutputReg(loaded, sExt);
      break;
    }
    case AArch64::LDRSBWpre:
    case AArch64::LDRSBXpre:
    case AArch64::LDRSHWpre:
    case AArch64::LDRSHXpre:
    case AArch64::LDRSWpre:
    case AArch64::LDRBBpre:
    case AArch64::LDRBpre:
    case AArch64::LDRHHpre:
    case AArch64::LDRHpre:
    case AArch64::LDRWpre:
    case AArch64::LDRSpre:
    case AArch64::LDRXpre:
    case AArch64::LDRDpre:
    case AArch64::LDRQpre:
    case AArch64::LDRSBWpost:
    case AArch64::LDRSBXpost:
    case AArch64::LDRSHWpost:
    case AArch64::LDRSHXpost:
    case AArch64::LDRSWpost:
    case AArch64::LDRBBpost:
    case AArch64::LDRBpost:
    case AArch64::LDRHHpost:
    case AArch64::LDRHpost:
    case AArch64::LDRWpost:
    case AArch64::LDRSpost:
    case AArch64::LDRXpost:
    case AArch64::LDRDpost:
    case AArch64::LDRQpost: {
      unsigned size;
      switch (opcode) {
      case AArch64::LDRSBWpre:
      case AArch64::LDRSBXpre:
      case AArch64::LDRBBpre:
      case AArch64::LDRBpre:
      case AArch64::LDRSBWpost:
      case AArch64::LDRSBXpost:
      case AArch64::LDRBBpost:
      case AArch64::LDRBpost:
        size = 1;
        break;
      case AArch64::LDRSHWpre:
      case AArch64::LDRSHXpre:
      case AArch64::LDRHHpre:
      case AArch64::LDRHpre:
      case AArch64::LDRSHWpost:
      case AArch64::LDRSHXpost:
      case AArch64::LDRHHpost:
      case AArch64::LDRHpost:
        size = 2;
        break;
      case AArch64::LDRSWpre:
      case AArch64::LDRWpre:
      case AArch64::LDRSpre:
      case AArch64::LDRSWpost:
      case AArch64::LDRWpost:
      case AArch64::LDRSpost:
        size = 4;
        break;
      case AArch64::LDRXpre:
      case AArch64::LDRDpre:
      case AArch64::LDRXpost:
      case AArch64::LDRDpost:
        size = 8;
        break;
      case AArch64::LDRQpre:
      case AArch64::LDRQpost:
        size = 16;
        break;
      default:
        assert(false);
      }
      bool sExt =
          opcode == AArch64::LDRSBWpre || opcode == AArch64::LDRSBXpre ||
          opcode == AArch64::LDRSHWpre || opcode == AArch64::LDRSHXpre ||
          opcode == AArch64::LDRSWpre || opcode == AArch64::LDRSBWpost ||
          opcode == AArch64::LDRSBXpost || opcode == AArch64::LDRSHWpost ||
          opcode == AArch64::LDRSHXpost || opcode == AArch64::LDRSWpost;
      auto &op0 = CurInst->getOperand(0);
      auto &op1 = CurInst->getOperand(1);
      auto &op2 = CurInst->getOperand(2);
      auto &op3 = CurInst->getOperand(3);
      assert(op0.isReg() && op1.isReg() && op2.isReg());
      assert(op0.getReg() == op2.getReg());
      assert(op3.isImm());

      // For pre and post index memory instructions, the destination register
      // is at position 1
      auto destReg = op1.getReg();
      auto baseReg = op2.getReg();
      auto imm = op3.getImm();
      assert((baseReg >= AArch64::X0 && baseReg <= AArch64::X28) ||
             (baseReg == AArch64::SP) || (baseReg == AArch64::LR) ||
             (baseReg == AArch64::FP) || (baseReg == AArch64::XZR));
      auto base = readPtrFromReg(baseReg);
      auto baseAddr = createPtrToInt(base, i64);

      // Start offset as a 9-bit signed integer
      assert(imm <= 255 && imm >= -256);
      auto offset = getIntConst(imm, 9);
      Value *offsetVal = createSExt(offset, i64);
      Value *zeroVal = getIntConst(0, 64);

      bool isPre = opcode == AArch64::LDRBBpre || opcode == AArch64::LDRBpre ||
                   opcode == AArch64::LDRHHpre || opcode == AArch64::LDRHpre ||
                   opcode == AArch64::LDRWpre || opcode == AArch64::LDRSpre ||
                   opcode == AArch64::LDRXpre || opcode == AArch64::LDRDpre ||
                   opcode == AArch64::LDRQpre || opcode == AArch64::LDRSBWpre ||
                   opcode == AArch64::LDRSBXpre ||
                   opcode == AArch64::LDRSHWpre ||
                   opcode == AArch64::LDRSHXpre || opcode == AArch64::LDRSWpre;

      auto loaded = makeLoadWithOffset(base, isPre ? offsetVal : zeroVal, size);
      updateReg(loaded, destReg, sExt);

      auto added = createAdd(baseAddr, offsetVal);
      updateOutputReg(added, sExt);
      break;
    }

    case AArch64::LDRBBroW:
    case AArch64::LDRBBroX:
    case AArch64::LDRBroW:
    case AArch64::LDRBroX:
    case AArch64::LDRHHroW:
    case AArch64::LDRHHroX:
    case AArch64::LDRHroW:
    case AArch64::LDRHroX:
    case AArch64::LDRWroW:
    case AArch64::LDRWroX:
    case AArch64::LDRSroW:
    case AArch64::LDRSroX:
    case AArch64::LDRXroW:
    case AArch64::LDRXroX:
    case AArch64::LDRDroW:
    case AArch64::LDRDroX:
    case AArch64::LDRQroW:
    case AArch64::LDRQroX:
    case AArch64::LDRSBWroW:
    case AArch64::LDRSBWroX:
    case AArch64::LDRSBXroW:
    case AArch64::LDRSBXroX:
    case AArch64::LDRSHWroW:
    case AArch64::LDRSHWroX:
    case AArch64::LDRSHXroW:
    case AArch64::LDRSHXroX:
    case AArch64::LDRSWroW:
    case AArch64::LDRSWroX: {
      unsigned size;

      switch (opcode) {
      case AArch64::LDRBBroW:
      case AArch64::LDRBBroX:
      case AArch64::LDRBroW:
      case AArch64::LDRBroX:
      case AArch64::LDRSBWroW:
      case AArch64::LDRSBWroX:
      case AArch64::LDRSBXroW:
      case AArch64::LDRSBXroX:
        size = 1;
        break;
      case AArch64::LDRHHroW:
      case AArch64::LDRHHroX:
      case AArch64::LDRHroW:
      case AArch64::LDRHroX:
      case AArch64::LDRSHWroW:
      case AArch64::LDRSHWroX:
      case AArch64::LDRSHXroW:
      case AArch64::LDRSHXroX:
        size = 2;
        break;
      case AArch64::LDRWroX:
      case AArch64::LDRWroW:
      case AArch64::LDRSroW:
      case AArch64::LDRSroX:
      case AArch64::LDRSWroW:
      case AArch64::LDRSWroX:
        size = 4;
        break;
      case AArch64::LDRXroW:
      case AArch64::LDRXroX:
      case AArch64::LDRDroW:
      case AArch64::LDRDroX:
        size = 8;
        break;
      case AArch64::LDRQroW:
      case AArch64::LDRQroX:
        size = 16;
        break;
      default:
        *out << "\nError Unknown opcode\n";
        visitError();
        break;
      }

      bool sExt =
          opcode == AArch64::LDRSBWroW || opcode == AArch64::LDRSBWroX ||
          opcode == AArch64::LDRSBXroW || opcode == AArch64::LDRSBXroX ||
          opcode == AArch64::LDRSHWroW || opcode == AArch64::LDRSHWroX ||
          opcode == AArch64::LDRSHXroW || opcode == AArch64::LDRSHXroX ||
          opcode == AArch64::LDRSWroW || opcode == AArch64::LDRSWroX;

      auto [base, offset] = getParamsLoadReg();
      auto loaded = makeLoadWithOffset(base, offset, size);
      updateOutputReg(loaded, sExt);
      break;
    }

    case AArch64::LD1i8:
    case AArch64::LD1i16:
    case AArch64::LD1i32:
    case AArch64::LD1i64:
    case AArch64::LD1i8_POST:
    case AArch64::LD1i16_POST:
    case AArch64::LD1i32_POST:
    case AArch64::LD1i64_POST: {
      unsigned numElts, eltSize;

      switch (opcode) {
      case AArch64::LD1i8:
      case AArch64::LD1i8_POST:
        numElts = 16;
        eltSize = 8;
        break;
      case AArch64::LD1i16:
      case AArch64::LD1i16_POST:
        numElts = 8;
        eltSize = 16;
        break;
      case AArch64::LD1i32:
      case AArch64::LD1i32_POST:
        numElts = 4;
        eltSize = 32;
        break;
      case AArch64::LD1i64:
      case AArch64::LD1i64_POST:
        numElts = 2;
        eltSize = 64;
        break;
      default:
        *out << "\nError Unknown opcode\n";
        visitError();
        break;
      }
      unsigned nregs;
      switch (opcode) {
      case AArch64::LD1i8:
      case AArch64::LD1i16:
      case AArch64::LD1i32:
      case AArch64::LD1i64:
      case AArch64::LD1i8_POST:
      case AArch64::LD1i16_POST:
      case AArch64::LD1i32_POST:
      case AArch64::LD1i64_POST:
        nregs = 1;
        break;
      default:
        assert(false);
        break;
      }
      bool isPost =
          opcode == AArch64::LD1i8_POST || opcode == AArch64::LD1i16_POST ||
          opcode == AArch64::LD1i32_POST || opcode == AArch64::LD1i64_POST;

      auto regCounter =
          decodeRegSet(CurInst->getOperand(isPost ? 2 : 1).getReg());
      auto index = getImm(isPost ? 3 : 2);
      auto baseReg = CurInst->getOperand(isPost ? 4 : 3).getReg();
      assert((baseReg >= AArch64::X0 && baseReg <= AArch64::X28) ||
             (baseReg == AArch64::SP) || (baseReg == AArch64::LR) ||
             (baseReg == AArch64::FP) || (baseReg == AArch64::XZR));
      assert((regCounter >= AArch64::Q0 && regCounter <= AArch64::Q31));
      Value *totalOffset{nullptr};
      if (isPost) {
        if (CurInst->getOperand(5).isReg() &&
            CurInst->getOperand(5).getReg() != AArch64::XZR) {
          totalOffset = readFromReg(CurInst->getOperand(3).getReg());
        } else {
          totalOffset = getIntConst(nregs * (eltSize / 8), 64);
        }
      }

      auto base = readPtrFromReg(baseReg);
      auto baseAddr = createPtrToInt(base, i64);

      auto offset = 0;
      for (unsigned i = 0; i < nregs; i++, offset += eltSize / 8) {
        auto loaded = makeLoadWithOffset(base, offset, eltSize / 8);
        auto dst = readFromReg(regCounter);
        auto dst_casted = createBitCast(dst, getVecTy(eltSize, numElts));
        auto updated = createInsertElement(dst_casted, loaded, index);
        updateReg(updated, regCounter);

        regCounter++;
        if (regCounter > AArch64::Q31)
          regCounter = AArch64::Q0;
      }

      if (isPost) {
        auto added = createAdd(baseAddr, totalOffset);
        updateOutputReg(added);
      }
      break;
    }

    case AArch64::LD1Rv8b:
    case AArch64::LD1Rv16b:
    case AArch64::LD1Rv4h:
    case AArch64::LD1Rv8h:
    case AArch64::LD1Rv2s:
    case AArch64::LD1Rv4s:
    case AArch64::LD1Rv2d: {
      auto &op0 = CurInst->getOperand(0);
      auto &op1 = CurInst->getOperand(1);

      assert(op0.isReg() && op1.isReg());

      // Read source
      auto dst = readFromReg(op0.getReg());
      if (getRegSize(op0.getReg()) == 64) {
        dst = createTrunc(dst, i64);
      }
      auto baseReg = op1.getReg();
      assert((baseReg >= AArch64::X0 && baseReg <= AArch64::X28) ||
             (baseReg == AArch64::SP) || (baseReg == AArch64::LR) ||
             (baseReg == AArch64::FP) || (baseReg == AArch64::XZR));

      auto base = readPtrFromReg(baseReg);

      unsigned numElts, eltSize;

      switch (opcode) {
      case AArch64::LD1Rv8b:
        numElts = 8;
        eltSize = 8;
        break;
      case AArch64::LD1Rv16b:
        numElts = 16;
        eltSize = 8;
        break;
      case AArch64::LD1Rv4h:
        numElts = 4;
        eltSize = 16;
        break;
      case AArch64::LD1Rv8h:
        numElts = 8;
        eltSize = 16;
        break;
      case AArch64::LD1Rv2s:
        numElts = 2;
        eltSize = 32;
        break;
      case AArch64::LD1Rv4s:
        numElts = 4;
        eltSize = 32;
        break;
      case AArch64::LD1Rv1d:
        numElts = 1;
        eltSize = 64;
        break;
      case AArch64::LD1Rv2d:
        numElts = 2;
        eltSize = 64;
        break;
      default:
        *out << "\nError Unknown opcode\n";
        visitError();
        break;
      }

      auto loaded = makeLoadWithOffset(base, 0, eltSize / 8);
      auto casted = createBitCast(dst, getVecTy(eltSize, numElts));

      // FIXME: Alternative to creating many insertelement instructions, insert
      //  the element in the first lane and create a shufflevector to copy it to
      //  the rest of the lanes doing the same task in 2 instructions. The
      //  shuffle mask will just be <numElts x i32> zeroinitializer
      Value *updated_dst = casted;
      for (unsigned i = 0; i < numElts; i++) {
        updated_dst = createInsertElement(updated_dst, loaded, i);
      }

      updateOutputReg(updated_dst);
      break;
    }

    case AArch64::LD1Onev8b:
    case AArch64::LD1Onev16b:
    case AArch64::LD1Onev4h:
    case AArch64::LD1Onev8h:
    case AArch64::LD1Onev2s:
    case AArch64::LD1Onev4s:
    case AArch64::LD1Onev1d:
    case AArch64::LD1Onev2d:
    case AArch64::LD1Onev8b_POST:
    case AArch64::LD1Onev16b_POST:
    case AArch64::LD1Onev4h_POST:
    case AArch64::LD1Onev8h_POST:
    case AArch64::LD1Onev2s_POST:
    case AArch64::LD1Onev4s_POST:
    case AArch64::LD1Onev1d_POST:
    case AArch64::LD1Onev2d_POST:
    case AArch64::LD2Twov8b:
    case AArch64::LD2Twov16b:
    case AArch64::LD2Twov4h:
    case AArch64::LD2Twov8h:
    case AArch64::LD2Twov2s:
    case AArch64::LD2Twov4s:
    case AArch64::LD2Twov2d:
    case AArch64::LD2Twov8b_POST:
    case AArch64::LD2Twov16b_POST:
    case AArch64::LD2Twov4h_POST:
    case AArch64::LD2Twov8h_POST:
    case AArch64::LD2Twov2s_POST:
    case AArch64::LD2Twov4s_POST:
    case AArch64::LD2Twov2d_POST:
    case AArch64::LD3Threev8b:
    case AArch64::LD3Threev16b:
    case AArch64::LD3Threev4h:
    case AArch64::LD3Threev8h:
    case AArch64::LD3Threev2s:
    case AArch64::LD3Threev4s:
    case AArch64::LD3Threev2d:
    case AArch64::LD3Threev8b_POST:
    case AArch64::LD3Threev16b_POST:
    case AArch64::LD3Threev4h_POST:
    case AArch64::LD3Threev8h_POST:
    case AArch64::LD3Threev2s_POST:
    case AArch64::LD3Threev4s_POST:
    case AArch64::LD3Threev2d_POST:
    case AArch64::LD4Fourv8b:
    case AArch64::LD4Fourv16b:
    case AArch64::LD4Fourv4h:
    case AArch64::LD4Fourv8h:
    case AArch64::LD4Fourv2s:
    case AArch64::LD4Fourv4s:
    case AArch64::LD4Fourv2d:
    case AArch64::LD4Fourv8b_POST:
    case AArch64::LD4Fourv16b_POST:
    case AArch64::LD4Fourv4h_POST:
    case AArch64::LD4Fourv8h_POST:
    case AArch64::LD4Fourv2s_POST:
    case AArch64::LD4Fourv4s_POST:
    case AArch64::LD4Fourv2d_POST: {
      unsigned numElts, eltSize;
      bool fullWidth;
      switch (opcode) {
      case AArch64::LD1Onev8b:
      case AArch64::LD1Onev8b_POST:
      case AArch64::LD1Twov8b:
      case AArch64::LD1Twov8b_POST:
      case AArch64::LD1Threev8b:
      case AArch64::LD1Threev8b_POST:
      case AArch64::LD1Fourv8b:
      case AArch64::LD1Fourv8b_POST:
      case AArch64::LD2Twov8b:
      case AArch64::LD2Twov8b_POST:
      case AArch64::LD3Threev8b:
      case AArch64::LD3Threev8b_POST:
      case AArch64::LD4Fourv8b:
      case AArch64::LD4Fourv8b_POST:
        numElts = 8;
        eltSize = 8;
        fullWidth = false;
        break;
      case AArch64::LD1Onev16b:
      case AArch64::LD1Onev16b_POST:
      case AArch64::LD1Twov16b:
      case AArch64::LD1Twov16b_POST:
      case AArch64::LD1Threev16b:
      case AArch64::LD1Threev16b_POST:
      case AArch64::LD1Fourv16b:
      case AArch64::LD1Fourv16b_POST:
      case AArch64::LD2Twov16b:
      case AArch64::LD2Twov16b_POST:
      case AArch64::LD3Threev16b:
      case AArch64::LD3Threev16b_POST:
      case AArch64::LD4Fourv16b:
      case AArch64::LD4Fourv16b_POST:
        numElts = 16;
        eltSize = 8;
        fullWidth = true;
        break;
      case AArch64::LD1Onev4h:
      case AArch64::LD1Onev4h_POST:
      case AArch64::LD1Twov4h:
      case AArch64::LD1Twov4h_POST:
      case AArch64::LD1Threev4h:
      case AArch64::LD1Threev4h_POST:
      case AArch64::LD1Fourv4h:
      case AArch64::LD1Fourv4h_POST:
      case AArch64::LD2Twov4h:
      case AArch64::LD2Twov4h_POST:
      case AArch64::LD3Threev4h:
      case AArch64::LD3Threev4h_POST:
      case AArch64::LD4Fourv4h:
      case AArch64::LD4Fourv4h_POST:
        numElts = 4;
        eltSize = 16;
        fullWidth = false;
        break;
      case AArch64::LD1Onev8h:
      case AArch64::LD1Onev8h_POST:
      case AArch64::LD1Twov8h:
      case AArch64::LD1Twov8h_POST:
      case AArch64::LD1Threev8h:
      case AArch64::LD1Threev8h_POST:
      case AArch64::LD1Fourv8h:
      case AArch64::LD1Fourv8h_POST:
      case AArch64::LD2Twov8h:
      case AArch64::LD2Twov8h_POST:
      case AArch64::LD3Threev8h:
      case AArch64::LD3Threev8h_POST:
      case AArch64::LD4Fourv8h:
      case AArch64::LD4Fourv8h_POST:
        numElts = 8;
        eltSize = 16;
        fullWidth = true;
        break;
      case AArch64::LD1Onev2s:
      case AArch64::LD1Onev2s_POST:
      case AArch64::LD1Twov2s:
      case AArch64::LD1Twov2s_POST:
      case AArch64::LD1Threev2s:
      case AArch64::LD1Threev2s_POST:
      case AArch64::LD1Fourv2s:
      case AArch64::LD1Fourv2s_POST:
      case AArch64::LD2Twov2s:
      case AArch64::LD2Twov2s_POST:
      case AArch64::LD3Threev2s:
      case AArch64::LD3Threev2s_POST:
      case AArch64::LD4Fourv2s:
      case AArch64::LD4Fourv2s_POST:
        numElts = 2;
        eltSize = 32;
        fullWidth = false;
        break;
      case AArch64::LD1Onev4s:
      case AArch64::LD1Onev4s_POST:
      case AArch64::LD1Twov4s:
      case AArch64::LD1Twov4s_POST:
      case AArch64::LD1Threev4s:
      case AArch64::LD1Threev4s_POST:
      case AArch64::LD1Fourv4s:
      case AArch64::LD1Fourv4s_POST:
      case AArch64::LD2Twov4s:
      case AArch64::LD2Twov4s_POST:
      case AArch64::LD3Threev4s:
      case AArch64::LD3Threev4s_POST:
      case AArch64::LD4Fourv4s:
      case AArch64::LD4Fourv4s_POST:
        numElts = 4;
        eltSize = 32;
        fullWidth = true;
        break;
      case AArch64::LD1Onev1d:
      case AArch64::LD1Onev1d_POST:
      case AArch64::LD1Twov1d:
      case AArch64::LD1Twov1d_POST:
      case AArch64::LD1Threev1d:
      case AArch64::LD1Threev1d_POST:
      case AArch64::LD1Fourv1d:
      case AArch64::LD1Fourv1d_POST:
        numElts = 1;
        eltSize = 64;
        fullWidth = false;
        break;
      case AArch64::LD1Onev2d:
      case AArch64::LD1Onev2d_POST:
      case AArch64::LD1Twov2d:
      case AArch64::LD1Twov2d_POST:
      case AArch64::LD1Threev2d:
      case AArch64::LD1Threev2d_POST:
      case AArch64::LD1Fourv2d:
      case AArch64::LD1Fourv2d_POST:
      case AArch64::LD2Twov2d:
      case AArch64::LD2Twov2d_POST:
      case AArch64::LD3Threev2d:
      case AArch64::LD3Threev2d_POST:
      case AArch64::LD4Fourv2d:
      case AArch64::LD4Fourv2d_POST:
        numElts = 2;
        eltSize = 64;
        fullWidth = true;
        break;
      default:
        assert(false);
        break;
      }

      unsigned nregs;
      switch (opcode) {
      case AArch64::LD1Onev8b:
      case AArch64::LD1Onev16b:
      case AArch64::LD1Onev4h:
      case AArch64::LD1Onev8h:
      case AArch64::LD1Onev2s:
      case AArch64::LD1Onev4s:
      case AArch64::LD1Onev1d:
      case AArch64::LD1Onev2d:
      case AArch64::LD1Onev8b_POST:
      case AArch64::LD1Onev16b_POST:
      case AArch64::LD1Onev4h_POST:
      case AArch64::LD1Onev8h_POST:
      case AArch64::LD1Onev2s_POST:
      case AArch64::LD1Onev4s_POST:
      case AArch64::LD1Onev1d_POST:
      case AArch64::LD1Onev2d_POST:
        nregs = 1;
        break;
      case AArch64::LD1Twov8b:
      case AArch64::LD1Twov16b:
      case AArch64::LD1Twov4h:
      case AArch64::LD1Twov8h:
      case AArch64::LD1Twov2s:
      case AArch64::LD1Twov4s:
      case AArch64::LD1Twov1d:
      case AArch64::LD1Twov2d:
      case AArch64::LD1Twov8b_POST:
      case AArch64::LD1Twov16b_POST:
      case AArch64::LD1Twov4h_POST:
      case AArch64::LD1Twov8h_POST:
      case AArch64::LD1Twov2s_POST:
      case AArch64::LD1Twov4s_POST:
      case AArch64::LD1Twov1d_POST:
      case AArch64::LD1Twov2d_POST:
      case AArch64::LD2Twov8b:
      case AArch64::LD2Twov16b:
      case AArch64::LD2Twov4h:
      case AArch64::LD2Twov8h:
      case AArch64::LD2Twov2s:
      case AArch64::LD2Twov4s:
      case AArch64::LD2Twov2d:
      case AArch64::LD2Twov8b_POST:
      case AArch64::LD2Twov16b_POST:
      case AArch64::LD2Twov4h_POST:
      case AArch64::LD2Twov8h_POST:
      case AArch64::LD2Twov2s_POST:
      case AArch64::LD2Twov4s_POST:
      case AArch64::LD2Twov2d_POST:
        nregs = 2;
        break;
      case AArch64::LD1Threev8b:
      case AArch64::LD1Threev16b:
      case AArch64::LD1Threev4h:
      case AArch64::LD1Threev8h:
      case AArch64::LD1Threev2s:
      case AArch64::LD1Threev4s:
      case AArch64::LD1Threev1d:
      case AArch64::LD1Threev2d:
      case AArch64::LD1Threev8b_POST:
      case AArch64::LD1Threev16b_POST:
      case AArch64::LD1Threev4h_POST:
      case AArch64::LD1Threev8h_POST:
      case AArch64::LD1Threev2s_POST:
      case AArch64::LD1Threev4s_POST:
      case AArch64::LD1Threev1d_POST:
      case AArch64::LD1Threev2d_POST:
      case AArch64::LD3Threev8b:
      case AArch64::LD3Threev16b:
      case AArch64::LD3Threev4h:
      case AArch64::LD3Threev8h:
      case AArch64::LD3Threev2s:
      case AArch64::LD3Threev4s:
      case AArch64::LD3Threev2d:
      case AArch64::LD3Threev8b_POST:
      case AArch64::LD3Threev16b_POST:
      case AArch64::LD3Threev4h_POST:
      case AArch64::LD3Threev8h_POST:
      case AArch64::LD3Threev2s_POST:
      case AArch64::LD3Threev4s_POST:
      case AArch64::LD3Threev2d_POST:
        nregs = 3;
        break;
      case AArch64::LD1Fourv8b:
      case AArch64::LD1Fourv16b:
      case AArch64::LD1Fourv4h:
      case AArch64::LD1Fourv8h:
      case AArch64::LD1Fourv2s:
      case AArch64::LD1Fourv4s:
      case AArch64::LD1Fourv1d:
      case AArch64::LD1Fourv2d:
      case AArch64::LD1Fourv8b_POST:
      case AArch64::LD1Fourv16b_POST:
      case AArch64::LD1Fourv4h_POST:
      case AArch64::LD1Fourv8h_POST:
      case AArch64::LD1Fourv2s_POST:
      case AArch64::LD1Fourv4s_POST:
      case AArch64::LD1Fourv1d_POST:
      case AArch64::LD1Fourv2d_POST:
      case AArch64::LD4Fourv8b:
      case AArch64::LD4Fourv16b:
      case AArch64::LD4Fourv4h:
      case AArch64::LD4Fourv8h:
      case AArch64::LD4Fourv2s:
      case AArch64::LD4Fourv4s:
      case AArch64::LD4Fourv2d:
      case AArch64::LD4Fourv8b_POST:
      case AArch64::LD4Fourv16b_POST:
      case AArch64::LD4Fourv4h_POST:
      case AArch64::LD4Fourv8h_POST:
      case AArch64::LD4Fourv2s_POST:
      case AArch64::LD4Fourv4s_POST:
      case AArch64::LD4Fourv2d_POST:
        nregs = 4;
        break;
      default:
        assert(false);
        break;
      }
      bool isPost = opcode == AArch64::LD1Onev8b_POST ||
                    opcode == AArch64::LD1Onev16b_POST ||
                    opcode == AArch64::LD1Onev4h_POST ||
                    opcode == AArch64::LD1Onev8h_POST ||
                    opcode == AArch64::LD1Onev2s_POST ||
                    opcode == AArch64::LD1Onev4s_POST ||
                    opcode == AArch64::LD1Onev1d_POST ||
                    opcode == AArch64::LD1Onev2d_POST ||
                    opcode == AArch64::LD1Twov8b_POST ||
                    opcode == AArch64::LD1Twov16b_POST ||
                    opcode == AArch64::LD1Twov4h_POST ||
                    opcode == AArch64::LD1Twov8h_POST ||
                    opcode == AArch64::LD1Twov2s_POST ||
                    opcode == AArch64::LD1Twov4s_POST ||
                    opcode == AArch64::LD1Twov1d_POST ||
                    opcode == AArch64::LD1Twov2d_POST ||
                    opcode == AArch64::LD1Threev8b_POST ||
                    opcode == AArch64::LD1Threev16b_POST ||
                    opcode == AArch64::LD1Threev4h_POST ||
                    opcode == AArch64::LD1Threev8h_POST ||
                    opcode == AArch64::LD1Threev2s_POST ||
                    opcode == AArch64::LD1Threev4s_POST ||
                    opcode == AArch64::LD1Threev1d_POST ||
                    opcode == AArch64::LD1Threev2d_POST ||
                    opcode == AArch64::LD1Fourv8b_POST ||
                    opcode == AArch64::LD1Fourv16b_POST ||
                    opcode == AArch64::LD1Fourv4h_POST ||
                    opcode == AArch64::LD1Fourv8h_POST ||
                    opcode == AArch64::LD1Fourv2s_POST ||
                    opcode == AArch64::LD1Fourv4s_POST ||
                    opcode == AArch64::LD1Fourv1d_POST ||
                    opcode == AArch64::LD1Fourv2d_POST ||
                    opcode == AArch64::LD2Twov8b_POST ||
                    opcode == AArch64::LD2Twov16b_POST ||
                    opcode == AArch64::LD2Twov4h_POST ||
                    opcode == AArch64::LD2Twov8h_POST ||
                    opcode == AArch64::LD2Twov2s_POST ||
                    opcode == AArch64::LD2Twov4s_POST ||
                    opcode == AArch64::LD2Twov2d_POST ||
                    opcode == AArch64::LD3Threev8b_POST ||
                    opcode == AArch64::LD3Threev16b_POST ||
                    opcode == AArch64::LD3Threev4h_POST ||
                    opcode == AArch64::LD3Threev8h_POST ||
                    opcode == AArch64::LD3Threev2s_POST ||
                    opcode == AArch64::LD3Threev4s_POST ||
                    opcode == AArch64::LD3Threev2d_POST ||
                    opcode == AArch64::LD4Fourv8b_POST ||
                    opcode == AArch64::LD4Fourv16b_POST ||
                    opcode == AArch64::LD4Fourv4h_POST ||
                    opcode == AArch64::LD4Fourv8h_POST ||
                    opcode == AArch64::LD4Fourv2s_POST ||
                    opcode == AArch64::LD4Fourv4s_POST ||
                    opcode == AArch64::LD4Fourv2d_POST;

      auto regCounter =
          decodeRegSet(CurInst->getOperand(isPost ? 1 : 0).getReg());
      auto baseReg = CurInst->getOperand(isPost ? 2 : 1).getReg();
      assert((baseReg >= AArch64::X0 && baseReg <= AArch64::X28) ||
             (baseReg == AArch64::SP) || (baseReg == AArch64::LR) ||
             (baseReg == AArch64::FP) || (baseReg == AArch64::XZR));
      assert((regCounter >= AArch64::Q0 && regCounter <= AArch64::Q31) ||
             (regCounter >= AArch64::D0 && regCounter <= AArch64::D31));
      Value *offset = nullptr;
      if (isPost) {
        if (CurInst->getOperand(3).isReg() &&
            CurInst->getOperand(3).getReg() != AArch64::XZR) {
          offset = readFromReg(CurInst->getOperand(3).getReg());
        } else {
          offset = getIntConst(nregs * numElts * (eltSize / 8), 64);
        }
      }

      // Load the entire memory block to be stored in all the registers at once
      // [nregs * numElts * (eltSize / 8)] bytes
      auto base = readPtrFromReg(baseReg);
      auto baseAddr = createPtrToInt(base, i64);
      auto loaded =
          makeLoadWithOffset(base, 0, nregs * numElts * (eltSize / 8));
      auto casted = createBitCast(loaded, getVecTy(eltSize, nregs * numElts));

      // Deinterleave the loaded vector into nregs vectors and store them in the
      // registers
      int mask[numElts];
      ArrayRef<int> maskRef(mask, numElts);
      Value *res;

      // Outer loop control for register to store into
      for (unsigned j = 0; j < nregs; j++) {
        for (unsigned i = 0, maskVal = j; i < numElts; i++, maskVal += nregs) {
          assert(maskVal < nregs * numElts);
          mask[i] = maskVal;
        }
        res = createShuffleVector(casted, maskRef);
        updateReg(res, regCounter);

        regCounter++;
        if (fullWidth && regCounter > AArch64::Q31)
          regCounter = AArch64::Q0;
        else if (!fullWidth && regCounter > AArch64::D31)
          regCounter = AArch64::D0;
      }

      if (isPost) {
        auto added = createAdd(baseAddr, offset);
        updateOutputReg(added);
      }

      break;
    }

    case AArch64::STURBBi:
    case AArch64::STURBi:
    case AArch64::STURHHi:
    case AArch64::STURHi:
    case AArch64::STURWi:
    case AArch64::STURSi:
    case AArch64::STURXi:
    case AArch64::STURDi:
    case AArch64::STURQi:
    case AArch64::STRBBui:
    case AArch64::STRBui:
    case AArch64::STRHHui:
    case AArch64::STRHui:
    case AArch64::STRWui:
    case AArch64::STRSui:
    case AArch64::STRXui:
    case AArch64::STRDui:
    case AArch64::STRQui: {
      auto [base, imm, val] = getStoreParams();

      bool isScaled = opcode == AArch64::STRBBui || opcode == AArch64::STRBui ||
                      opcode == AArch64::STRHHui || opcode == AArch64::STRHui ||
                      opcode == AArch64::STRWui || opcode == AArch64::STRSui ||
                      opcode == AArch64::STRXui || opcode == AArch64::STRDui ||
                      opcode == AArch64::STRQui;

      switch (opcode) {
      case AArch64::STURBBi:
      case AArch64::STURBi:
      case AArch64::STRBBui:
      case AArch64::STRBui: {
        storeToMemoryImmOffset(base, isScaled ? imm * 1 : imm, 1,
                               createTrunc(val, i8));
        break;
      }
      case AArch64::STURHHi:
      case AArch64::STURHi:
      case AArch64::STRHHui:
      case AArch64::STRHui: {
        storeToMemoryImmOffset(base, isScaled ? imm * 2 : imm, 2,
                               createTrunc(val, i16));
        break;
      }
      case AArch64::STURWi:
      case AArch64::STURSi:
      case AArch64::STRWui:
      case AArch64::STRSui: {
        storeToMemoryImmOffset(base, isScaled ? imm * 4 : imm, 4,
                               createTrunc(val, i32));
        break;
      }
      case AArch64::STURXi:
      case AArch64::STRXui: {
        storeToMemoryImmOffset(base, isScaled ? imm * 8 : imm, 8, val);
        break;
      }
      case AArch64::STURDi:
      case AArch64::STRDui: {
        storeToMemoryImmOffset(base, isScaled ? imm * 8 : imm, 8,
                               createTrunc(val, i64));
        break;
      }
      case AArch64::STURQi:
      case AArch64::STRQui: {
        storeToMemoryImmOffset(base, isScaled ? imm * 16 : imm, 16, val);
        break;
      }
      default: {
        assert(false);
        break;
      }
      }
      break;
    }

    case AArch64::STRBBpre:
    case AArch64::STRBpre:
    case AArch64::STRHHpre:
    case AArch64::STRHpre:
    case AArch64::STRWpre:
    case AArch64::STRSpre:
    case AArch64::STRXpre:
    case AArch64::STRDpre:
    case AArch64::STRQpre:
    case AArch64::STRBBpost:
    case AArch64::STRBpost:
    case AArch64::STRHHpost:
    case AArch64::STRHpost:
    case AArch64::STRWpost:
    case AArch64::STRSpost:
    case AArch64::STRXpost:
    case AArch64::STRDpost:
    case AArch64::STRQpost: {
      auto &op0 = CurInst->getOperand(0);
      auto &op1 = CurInst->getOperand(1);
      auto &op2 = CurInst->getOperand(2);
      auto &op3 = CurInst->getOperand(3);
      assert(op0.isReg() && op1.isReg() && op2.isReg());
      assert(op0.getReg() == op2.getReg());
      assert(op3.isImm());

      // For pre and post index memory instructions, the destination register
      // is at position 1
      auto srcReg = op1.getReg();
      auto baseReg = op2.getReg();
      auto imm = op3.getImm();
      assert((baseReg >= AArch64::X0 && baseReg <= AArch64::X28) ||
             (baseReg == AArch64::SP) || (baseReg == AArch64::LR) ||
             (baseReg == AArch64::FP) || (baseReg == AArch64::XZR));
      auto base = readPtrFromReg(baseReg);
      auto baseAddr = createPtrToInt(base, i64);

      unsigned size;
      Value *loaded = nullptr;
      switch (opcode) {
      case AArch64::STRBBpre:
      case AArch64::STRBpre:
      case AArch64::STRBBpost:
      case AArch64::STRBpost:
        size = 1;
        loaded = createTrunc(readFromReg(srcReg), i8);
        break;
      case AArch64::STRHHpre:
      case AArch64::STRHpre:
      case AArch64::STRHHpost:
      case AArch64::STRHpost:
        size = 2;
        loaded = createTrunc(readFromReg(srcReg), i16);
        break;
      case AArch64::STRWpre:
      case AArch64::STRSpre:
      case AArch64::STRWpost:
      case AArch64::STRSpost:
        size = 4;
        loaded = createTrunc(readFromReg(srcReg), i32);
        break;
      case AArch64::STRXpre:
      case AArch64::STRXpost:
        size = 8;
        loaded = readFromReg(srcReg);
        break;
      case AArch64::STRDpre:
      case AArch64::STRDpost:
        size = 8;
        loaded = createTrunc(readFromReg(srcReg), i64);
        break;
      case AArch64::STRQpre:
      case AArch64::STRQpost:
        size = 16;
        loaded = readFromReg(srcReg);
        break;
      default:
        assert(false);
      }

      // Start offset as a 9-bit signed integer
      assert(imm <= 255 && imm >= -256);
      auto offset = getIntConst(imm, 9);
      Value *offsetVal = createSExt(offset, i64);
      Value *zeroVal = getIntConst(0, 64);

      bool isPre = opcode == AArch64::STRBBpre || opcode == AArch64::STRBpre ||
                   opcode == AArch64::STRHHpre || opcode == AArch64::STRHpre ||
                   opcode == AArch64::STRWpre || opcode == AArch64::STRSpre ||
                   opcode == AArch64::STRXpre || opcode == AArch64::STRDpre ||
                   opcode == AArch64::STRQpre;

      storeToMemoryValOffset(base, isPre ? offsetVal : zeroVal, size, loaded);

      auto added = createAdd(baseAddr, offsetVal);
      updateOutputReg(added);
      break;
    }

    case AArch64::STRBBroW:
    case AArch64::STRBBroX:
    case AArch64::STRBroW:
    case AArch64::STRBroX:
    case AArch64::STRHHroW:
    case AArch64::STRHHroX:
    case AArch64::STRHroW:
    case AArch64::STRHroX:
    case AArch64::STRWroW:
    case AArch64::STRWroX:
    case AArch64::STRSroW:
    case AArch64::STRSroX:
    case AArch64::STRXroW:
    case AArch64::STRXroX:
    case AArch64::STRDroW:
    case AArch64::STRDroX:
    case AArch64::STRQroW:
    case AArch64::STRQroX: {
      auto [base, offset, val] = getParamsStoreReg();

      switch (opcode) {
      case AArch64::STRBBroW:
      case AArch64::STRBBroX:
      case AArch64::STRBroW:
      case AArch64::STRBroX:
        storeToMemoryValOffset(base, offset, 1, createTrunc(val, i8));
        break;
      case AArch64::STRHHroW:
      case AArch64::STRHHroX:
      case AArch64::STRHroW:
      case AArch64::STRHroX:
        storeToMemoryValOffset(base, offset, 2, createTrunc(val, i16));
        break;
      case AArch64::STRWroW:
      case AArch64::STRWroX:
      case AArch64::STRSroW:
      case AArch64::STRSroX:
        storeToMemoryValOffset(base, offset, 4, createTrunc(val, i32));
        break;
      case AArch64::STRXroW:
      case AArch64::STRXroX:
        storeToMemoryValOffset(base, offset, 8, val);
        break;
      case AArch64::STRDroW:
      case AArch64::STRDroX:
        storeToMemoryValOffset(base, offset, 8, createTrunc(val, i64));
        break;
      case AArch64::STRQroW:
      case AArch64::STRQroX:
        storeToMemoryValOffset(base, offset, 16, val);
        break;
      default:
        *out << "\nError Unknown opcode\n";
        visitError();
      }
      break;
    }

    case AArch64::ST1i8:
    case AArch64::ST1i16:
    case AArch64::ST1i32:
    case AArch64::ST1i64:
    case AArch64::ST1i8_POST:
    case AArch64::ST1i16_POST:
    case AArch64::ST1i32_POST:
    case AArch64::ST1i64_POST: {
      unsigned numElts, eltSize;

      switch (opcode) {
      case AArch64::ST1i8:
      case AArch64::ST1i8_POST:
        numElts = 16;
        eltSize = 8;
        break;
      case AArch64::ST1i16:
      case AArch64::ST1i16_POST:
        numElts = 8;
        eltSize = 16;
        break;
      case AArch64::ST1i32:
      case AArch64::ST1i32_POST:
        numElts = 4;
        eltSize = 32;
        break;
      case AArch64::ST1i64:
      case AArch64::ST1i64_POST:
        numElts = 2;
        eltSize = 64;
        break;
      default:
        *out << "\nError Unknown opcode\n";
        visitError();
        break;
      }

      unsigned nregs;
      switch (opcode) {
      case AArch64::ST1i8:
      case AArch64::ST1i16:
      case AArch64::ST1i32:
      case AArch64::ST1i64:
      case AArch64::ST1i8_POST:
      case AArch64::ST1i16_POST:
      case AArch64::ST1i32_POST:
      case AArch64::ST1i64_POST:
        nregs = 1;
        break;
      default:
        assert(false);
        break;
      }
      bool isPost =
          opcode == AArch64::ST1i8_POST || opcode == AArch64::ST1i16_POST ||
          opcode == AArch64::ST1i32_POST || opcode == AArch64::ST1i64_POST;

      auto regCounter =
          decodeRegSet(CurInst->getOperand(isPost ? 1 : 0).getReg());
      auto index = getImm(isPost ? 2 : 1);
      auto baseReg = CurInst->getOperand(isPost ? 3 : 2).getReg();
      assert((baseReg >= AArch64::X0 && baseReg <= AArch64::X28) ||
             (baseReg == AArch64::SP) || (baseReg == AArch64::LR) ||
             (baseReg == AArch64::FP) || (baseReg == AArch64::XZR));
      assert((regCounter >= AArch64::Q0 && regCounter <= AArch64::Q31));

      Value *offset = nullptr;
      if (isPost) {
        if (CurInst->getOperand(4).isReg() &&
            CurInst->getOperand(4).getReg() != AArch64::XZR) {
          offset = readFromReg(CurInst->getOperand(4).getReg());
        } else {
          offset = getIntConst(nregs * (eltSize / 8), 64);
        }
      }

      auto base = readPtrFromReg(baseReg);
      auto baseAddr = createPtrToInt(base, i64);

      Value *valueToStore = getUndefVec(nregs, eltSize);
      // Outer loop control for register to load from
      for (unsigned j = 0; j < nregs; j++) {
        Value *registerjValue = readFromReg(regCounter);

        auto casted = createBitCast(registerjValue, getVecTy(eltSize, numElts));
        auto loaded = createExtractElement(casted, index);

        valueToStore = createInsertElement(valueToStore, loaded, j);

        regCounter++;
        if (regCounter > AArch64::Q31)
          regCounter = AArch64::Q0;
      }

      storeToMemoryImmOffset(base, 0, nregs * eltSize / 8, valueToStore);

      if (isPost) {
        auto added = createAdd(baseAddr, offset);
        updateOutputReg(added);
      }
      break;
    }

    case AArch64::ST1Onev8b:
    case AArch64::ST1Onev16b:
    case AArch64::ST1Onev4h:
    case AArch64::ST1Onev8h:
    case AArch64::ST1Onev2s:
    case AArch64::ST1Onev4s:
    case AArch64::ST1Onev1d:
    case AArch64::ST1Onev2d:
    case AArch64::ST1Onev8b_POST:
    case AArch64::ST1Onev16b_POST:
    case AArch64::ST1Onev4h_POST:
    case AArch64::ST1Onev8h_POST:
    case AArch64::ST1Onev2s_POST:
    case AArch64::ST1Onev4s_POST:
    case AArch64::ST1Onev1d_POST:
    case AArch64::ST1Onev2d_POST:
    case AArch64::ST2Twov8b:
    case AArch64::ST2Twov16b:
    case AArch64::ST2Twov4h:
    case AArch64::ST2Twov8h:
    case AArch64::ST2Twov2s:
    case AArch64::ST2Twov4s:
    case AArch64::ST2Twov2d:
    case AArch64::ST2Twov8b_POST:
    case AArch64::ST2Twov16b_POST:
    case AArch64::ST2Twov4h_POST:
    case AArch64::ST2Twov8h_POST:
    case AArch64::ST2Twov2s_POST:
    case AArch64::ST2Twov4s_POST:
    case AArch64::ST2Twov2d_POST:
    case AArch64::ST3Threev8b:
    case AArch64::ST3Threev16b:
    case AArch64::ST3Threev4h:
    case AArch64::ST3Threev8h:
    case AArch64::ST3Threev2s:
    case AArch64::ST3Threev4s:
    case AArch64::ST3Threev2d:
    case AArch64::ST3Threev8b_POST:
    case AArch64::ST3Threev16b_POST:
    case AArch64::ST3Threev4h_POST:
    case AArch64::ST3Threev8h_POST:
    case AArch64::ST3Threev2s_POST:
    case AArch64::ST3Threev4s_POST:
    case AArch64::ST3Threev2d_POST:
    case AArch64::ST4Fourv8b:
    case AArch64::ST4Fourv16b:
    case AArch64::ST4Fourv4h:
    case AArch64::ST4Fourv8h:
    case AArch64::ST4Fourv2s:
    case AArch64::ST4Fourv4s:
    case AArch64::ST4Fourv2d:
    case AArch64::ST4Fourv8b_POST:
    case AArch64::ST4Fourv16b_POST:
    case AArch64::ST4Fourv4h_POST:
    case AArch64::ST4Fourv8h_POST:
    case AArch64::ST4Fourv2s_POST:
    case AArch64::ST4Fourv4s_POST:
    case AArch64::ST4Fourv2d_POST: {
      unsigned numElts, eltSize;
      bool fullWidth;
      switch (opcode) {
      case AArch64::ST1Onev8b:
      case AArch64::ST1Onev8b_POST:
      case AArch64::ST1Twov8b:
      case AArch64::ST1Twov8b_POST:
      case AArch64::ST1Threev8b:
      case AArch64::ST1Threev8b_POST:
      case AArch64::ST1Fourv8b:
      case AArch64::ST1Fourv8b_POST:
      case AArch64::ST2Twov8b:
      case AArch64::ST2Twov8b_POST:
      case AArch64::ST3Threev8b:
      case AArch64::ST3Threev8b_POST:
      case AArch64::ST4Fourv8b:
      case AArch64::ST4Fourv8b_POST:
        numElts = 8;
        eltSize = 8;
        fullWidth = false;
        break;
      case AArch64::ST1Onev16b:
      case AArch64::ST1Onev16b_POST:
      case AArch64::ST1Twov16b:
      case AArch64::ST1Twov16b_POST:
      case AArch64::ST1Threev16b:
      case AArch64::ST1Threev16b_POST:
      case AArch64::ST1Fourv16b:
      case AArch64::ST1Fourv16b_POST:
      case AArch64::ST2Twov16b:
      case AArch64::ST2Twov16b_POST:
      case AArch64::ST3Threev16b:
      case AArch64::ST3Threev16b_POST:
      case AArch64::ST4Fourv16b:
      case AArch64::ST4Fourv16b_POST:
        numElts = 16;
        eltSize = 8;
        fullWidth = true;
        break;
      case AArch64::ST1Onev4h:
      case AArch64::ST1Onev4h_POST:
      case AArch64::ST1Twov4h:
      case AArch64::ST1Twov4h_POST:
      case AArch64::ST1Threev4h:
      case AArch64::ST1Threev4h_POST:
      case AArch64::ST1Fourv4h:
      case AArch64::ST1Fourv4h_POST:
      case AArch64::ST2Twov4h:
      case AArch64::ST2Twov4h_POST:
      case AArch64::ST3Threev4h:
      case AArch64::ST3Threev4h_POST:
      case AArch64::ST4Fourv4h:
      case AArch64::ST4Fourv4h_POST:
        numElts = 4;
        eltSize = 16;
        fullWidth = false;
        break;
      case AArch64::ST1Onev8h:
      case AArch64::ST1Onev8h_POST:
      case AArch64::ST1Twov8h:
      case AArch64::ST1Twov8h_POST:
      case AArch64::ST1Threev8h:
      case AArch64::ST1Threev8h_POST:
      case AArch64::ST1Fourv8h:
      case AArch64::ST1Fourv8h_POST:
      case AArch64::ST2Twov8h:
      case AArch64::ST2Twov8h_POST:
      case AArch64::ST3Threev8h:
      case AArch64::ST3Threev8h_POST:
      case AArch64::ST4Fourv8h:
      case AArch64::ST4Fourv8h_POST:
        numElts = 8;
        eltSize = 16;
        fullWidth = true;
        break;
      case AArch64::ST1Onev2s:
      case AArch64::ST1Onev2s_POST:
      case AArch64::ST1Twov2s:
      case AArch64::ST1Twov2s_POST:
      case AArch64::ST1Threev2s:
      case AArch64::ST1Threev2s_POST:
      case AArch64::ST1Fourv2s:
      case AArch64::ST1Fourv2s_POST:
      case AArch64::ST2Twov2s:
      case AArch64::ST2Twov2s_POST:
      case AArch64::ST3Threev2s:
      case AArch64::ST3Threev2s_POST:
      case AArch64::ST4Fourv2s:
      case AArch64::ST4Fourv2s_POST:
        numElts = 2;
        eltSize = 32;
        fullWidth = false;
        break;
      case AArch64::ST1Onev4s:
      case AArch64::ST1Onev4s_POST:
      case AArch64::ST1Twov4s:
      case AArch64::ST1Twov4s_POST:
      case AArch64::ST1Threev4s:
      case AArch64::ST1Threev4s_POST:
      case AArch64::ST1Fourv4s:
      case AArch64::ST1Fourv4s_POST:
      case AArch64::ST2Twov4s:
      case AArch64::ST2Twov4s_POST:
      case AArch64::ST3Threev4s:
      case AArch64::ST3Threev4s_POST:
      case AArch64::ST4Fourv4s:
      case AArch64::ST4Fourv4s_POST:
        numElts = 4;
        eltSize = 32;
        fullWidth = true;
        break;
      case AArch64::ST1Onev1d:
      case AArch64::ST1Onev1d_POST:
      case AArch64::ST1Twov1d:
      case AArch64::ST1Twov1d_POST:
      case AArch64::ST1Threev1d:
      case AArch64::ST1Threev1d_POST:
      case AArch64::ST1Fourv1d:
      case AArch64::ST1Fourv1d_POST:
        numElts = 1;
        eltSize = 64;
        fullWidth = false;
        break;
      case AArch64::ST1Onev2d:
      case AArch64::ST1Onev2d_POST:
      case AArch64::ST1Twov2d:
      case AArch64::ST1Twov2d_POST:
      case AArch64::ST1Threev2d:
      case AArch64::ST1Threev2d_POST:
      case AArch64::ST1Fourv2d:
      case AArch64::ST1Fourv2d_POST:
      case AArch64::ST2Twov2d:
      case AArch64::ST2Twov2d_POST:
      case AArch64::ST3Threev2d:
      case AArch64::ST3Threev2d_POST:
      case AArch64::ST4Fourv2d:
      case AArch64::ST4Fourv2d_POST:
        numElts = 2;
        eltSize = 64;
        fullWidth = true;
        break;
      default:
        assert(false);
        break;
      }

      unsigned nregs;
      switch (opcode) {
      case AArch64::ST1Onev8b:
      case AArch64::ST1Onev16b:
      case AArch64::ST1Onev4h:
      case AArch64::ST1Onev8h:
      case AArch64::ST1Onev2s:
      case AArch64::ST1Onev4s:
      case AArch64::ST1Onev1d:
      case AArch64::ST1Onev2d:
      case AArch64::ST1Onev8b_POST:
      case AArch64::ST1Onev16b_POST:
      case AArch64::ST1Onev4h_POST:
      case AArch64::ST1Onev8h_POST:
      case AArch64::ST1Onev2s_POST:
      case AArch64::ST1Onev4s_POST:
      case AArch64::ST1Onev1d_POST:
      case AArch64::ST1Onev2d_POST:
        nregs = 1;
        break;
      case AArch64::ST1Twov8b:
      case AArch64::ST1Twov16b:
      case AArch64::ST1Twov4h:
      case AArch64::ST1Twov8h:
      case AArch64::ST1Twov2s:
      case AArch64::ST1Twov4s:
      case AArch64::ST1Twov1d:
      case AArch64::ST1Twov2d:
      case AArch64::ST1Twov8b_POST:
      case AArch64::ST1Twov16b_POST:
      case AArch64::ST1Twov4h_POST:
      case AArch64::ST1Twov8h_POST:
      case AArch64::ST1Twov2s_POST:
      case AArch64::ST1Twov4s_POST:
      case AArch64::ST1Twov1d_POST:
      case AArch64::ST1Twov2d_POST:
      case AArch64::ST2Twov8b:
      case AArch64::ST2Twov16b:
      case AArch64::ST2Twov4h:
      case AArch64::ST2Twov8h:
      case AArch64::ST2Twov2s:
      case AArch64::ST2Twov4s:
      case AArch64::ST2Twov2d:
      case AArch64::ST2Twov8b_POST:
      case AArch64::ST2Twov16b_POST:
      case AArch64::ST2Twov4h_POST:
      case AArch64::ST2Twov8h_POST:
      case AArch64::ST2Twov2s_POST:
      case AArch64::ST2Twov4s_POST:
      case AArch64::ST2Twov2d_POST:
        nregs = 2;
        break;
      case AArch64::ST1Threev8b:
      case AArch64::ST1Threev16b:
      case AArch64::ST1Threev4h:
      case AArch64::ST1Threev8h:
      case AArch64::ST1Threev2s:
      case AArch64::ST1Threev4s:
      case AArch64::ST1Threev1d:
      case AArch64::ST1Threev2d:
      case AArch64::ST1Threev8b_POST:
      case AArch64::ST1Threev16b_POST:
      case AArch64::ST1Threev4h_POST:
      case AArch64::ST1Threev8h_POST:
      case AArch64::ST1Threev2s_POST:
      case AArch64::ST1Threev4s_POST:
      case AArch64::ST1Threev1d_POST:
      case AArch64::ST1Threev2d_POST:
      case AArch64::ST3Threev8b:
      case AArch64::ST3Threev16b:
      case AArch64::ST3Threev4h:
      case AArch64::ST3Threev8h:
      case AArch64::ST3Threev2s:
      case AArch64::ST3Threev4s:
      case AArch64::ST3Threev2d:
      case AArch64::ST3Threev8b_POST:
      case AArch64::ST3Threev16b_POST:
      case AArch64::ST3Threev4h_POST:
      case AArch64::ST3Threev8h_POST:
      case AArch64::ST3Threev2s_POST:
      case AArch64::ST3Threev4s_POST:
      case AArch64::ST3Threev2d_POST:
        nregs = 3;
        break;
      case AArch64::ST1Fourv8b:
      case AArch64::ST1Fourv16b:
      case AArch64::ST1Fourv4h:
      case AArch64::ST1Fourv8h:
      case AArch64::ST1Fourv2s:
      case AArch64::ST1Fourv4s:
      case AArch64::ST1Fourv1d:
      case AArch64::ST1Fourv2d:
      case AArch64::ST1Fourv8b_POST:
      case AArch64::ST1Fourv16b_POST:
      case AArch64::ST1Fourv4h_POST:
      case AArch64::ST1Fourv8h_POST:
      case AArch64::ST1Fourv2s_POST:
      case AArch64::ST1Fourv4s_POST:
      case AArch64::ST1Fourv1d_POST:
      case AArch64::ST1Fourv2d_POST:
      case AArch64::ST4Fourv8b:
      case AArch64::ST4Fourv16b:
      case AArch64::ST4Fourv4h:
      case AArch64::ST4Fourv8h:
      case AArch64::ST4Fourv2s:
      case AArch64::ST4Fourv4s:
      case AArch64::ST4Fourv2d:
      case AArch64::ST4Fourv8b_POST:
      case AArch64::ST4Fourv16b_POST:
      case AArch64::ST4Fourv4h_POST:
      case AArch64::ST4Fourv8h_POST:
      case AArch64::ST4Fourv2s_POST:
      case AArch64::ST4Fourv4s_POST:
      case AArch64::ST4Fourv2d_POST:
        nregs = 4;
        break;
      default:
        assert(false);
        break;
      }
      bool isPost = opcode == AArch64::ST1Onev8b_POST ||
                    opcode == AArch64::ST1Onev16b_POST ||
                    opcode == AArch64::ST1Onev4h_POST ||
                    opcode == AArch64::ST1Onev8h_POST ||
                    opcode == AArch64::ST1Onev2s_POST ||
                    opcode == AArch64::ST1Onev4s_POST ||
                    opcode == AArch64::ST1Onev1d_POST ||
                    opcode == AArch64::ST1Onev2d_POST ||
                    opcode == AArch64::ST1Twov8b_POST ||
                    opcode == AArch64::ST1Twov16b_POST ||
                    opcode == AArch64::ST1Twov4h_POST ||
                    opcode == AArch64::ST1Twov8h_POST ||
                    opcode == AArch64::ST1Twov2s_POST ||
                    opcode == AArch64::ST1Twov4s_POST ||
                    opcode == AArch64::ST1Twov1d_POST ||
                    opcode == AArch64::ST1Twov2d_POST ||
                    opcode == AArch64::ST1Threev8b_POST ||
                    opcode == AArch64::ST1Threev16b_POST ||
                    opcode == AArch64::ST1Threev4h_POST ||
                    opcode == AArch64::ST1Threev8h_POST ||
                    opcode == AArch64::ST1Threev2s_POST ||
                    opcode == AArch64::ST1Threev4s_POST ||
                    opcode == AArch64::ST1Threev1d_POST ||
                    opcode == AArch64::ST1Threev2d_POST ||
                    opcode == AArch64::ST1Fourv8b_POST ||
                    opcode == AArch64::ST1Fourv16b_POST ||
                    opcode == AArch64::ST1Fourv4h_POST ||
                    opcode == AArch64::ST1Fourv8h_POST ||
                    opcode == AArch64::ST1Fourv2s_POST ||
                    opcode == AArch64::ST1Fourv4s_POST ||
                    opcode == AArch64::ST1Fourv1d_POST ||
                    opcode == AArch64::ST1Fourv2d_POST ||
                    opcode == AArch64::ST2Twov8b_POST ||
                    opcode == AArch64::ST2Twov16b_POST ||
                    opcode == AArch64::ST2Twov4h_POST ||
                    opcode == AArch64::ST2Twov8h_POST ||
                    opcode == AArch64::ST2Twov2s_POST ||
                    opcode == AArch64::ST2Twov4s_POST ||
                    opcode == AArch64::ST2Twov2d_POST ||
                    opcode == AArch64::ST3Threev8b_POST ||
                    opcode == AArch64::ST3Threev16b_POST ||
                    opcode == AArch64::ST3Threev4h_POST ||
                    opcode == AArch64::ST3Threev8h_POST ||
                    opcode == AArch64::ST3Threev2s_POST ||
                    opcode == AArch64::ST3Threev4s_POST ||
                    opcode == AArch64::ST3Threev2d_POST ||
                    opcode == AArch64::ST4Fourv8b_POST ||
                    opcode == AArch64::ST4Fourv16b_POST ||
                    opcode == AArch64::ST4Fourv4h_POST ||
                    opcode == AArch64::ST4Fourv8h_POST ||
                    opcode == AArch64::ST4Fourv2s_POST ||
                    opcode == AArch64::ST4Fourv4s_POST ||
                    opcode == AArch64::ST4Fourv2d_POST;

      auto regCounter =
          decodeRegSet(CurInst->getOperand(isPost ? 1 : 0).getReg());
      auto baseReg = CurInst->getOperand(isPost ? 2 : 1).getReg();
      assert((baseReg >= AArch64::X0 && baseReg <= AArch64::X28) ||
             (baseReg == AArch64::SP) || (baseReg == AArch64::LR) ||
             (baseReg == AArch64::FP) || (baseReg == AArch64::XZR));
      assert((regCounter >= AArch64::Q0 && regCounter <= AArch64::Q31) ||
             (regCounter >= AArch64::D0 && regCounter <= AArch64::D31));
      Value *offset = nullptr;
      if (isPost) {
        if (CurInst->getOperand(3).isReg() &&
            CurInst->getOperand(3).getReg() != AArch64::XZR) {
          offset = readFromReg(CurInst->getOperand(3).getReg());
        } else {
          offset = getIntConst(nregs * numElts * (eltSize / 8), 64);
        }
      }

      auto base = readPtrFromReg(baseReg);
      auto baseAddr = createPtrToInt(base, i64);

      int mask[nregs * numElts];
      ArrayRef<int> maskRef(mask, nregs * numElts);

      Value *valueToStore = getUndefVec(nregs, numElts * eltSize);
      // Outer loop control for register to load from
      for (unsigned j = 0; j < nregs; j++) {
        Value *registerjValue = readFromReg(regCounter);
        if (!fullWidth) {
          registerjValue = createTrunc(registerjValue, i64);
        }

        valueToStore = createInsertElement(valueToStore, registerjValue, j);

        // Creating mask for the shufflevector
        for (unsigned i = 0, index = j; i < numElts; i++, index += nregs) {
          assert(index < nregs * numElts);
          mask[index] = j * numElts + i;
        }

        regCounter++;
        if (fullWidth && regCounter > AArch64::Q31)
          regCounter = AArch64::Q0;
        else if (!fullWidth && regCounter > AArch64::D31)
          regCounter = AArch64::D0;
      }

      // Shuffle the vector to interleave the nregs vectors
      auto casted =
          createBitCast(valueToStore, getVecTy(eltSize, nregs * numElts));
      Value *res = createShuffleVector(casted, maskRef);

      storeToMemoryImmOffset(base, 0, nregs * numElts * (eltSize / 8), res);

      if (isPost) {
        auto added = createAdd(baseAddr, offset);
        updateOutputReg(added);
      }

      break;
    }

    case AArch64::LDPSWi:
    case AArch64::LDPWi:
    case AArch64::LDPSi:
    case AArch64::LDPXi:
    case AArch64::LDPDi:
    case AArch64::LDPQi: {
      auto &op0 = CurInst->getOperand(0);
      auto &op1 = CurInst->getOperand(1);
      auto &op2 = CurInst->getOperand(2);
      auto &op3 = CurInst->getOperand(3);
      assert(op0.isReg() && op1.isReg() && op2.isReg());
      assert(op3.isImm());

      auto baseReg = op2.getReg();
      assert((baseReg >= AArch64::X0 && baseReg <= AArch64::X28) ||
             (baseReg == AArch64::SP) || (baseReg == AArch64::LR) ||
             (baseReg == AArch64::XZR) || (baseReg == AArch64::FP));
      auto baseAddr = readPtrFromReg(baseReg);

      int size = 0;
      switch (opcode) {
      case AArch64::LDPSWi:
      case AArch64::LDPWi:
      case AArch64::LDPSi: {
        size = 4;
        break;
      }
      case AArch64::LDPXi:
      case AArch64::LDPDi: {
        size = 8;
        break;
      }
      case AArch64::LDPQi: {
        size = 16;
        break;
      }
      default: {
        *out << "\nError Unknown opcode\n";
        visitError();
      }
      }
      assert(size != 0);

      bool SExt = opcode == AArch64::LDPSWi;

      auto imm = op3.getImm();
      auto out1 = op0.getReg();
      auto out2 = op1.getReg();

      updateReg(makeLoadWithOffset(baseAddr, imm * size, size), out1, SExt);
      updateReg(makeLoadWithOffset(baseAddr, (imm + 1) * size, size), out2,
                SExt);
      break;
    }

    case AArch64::STPWi:
    case AArch64::STPSi:
    case AArch64::STPXi:
    case AArch64::STPDi:
    case AArch64::STPQi: {
      auto &op0 = CurInst->getOperand(0);
      auto &op1 = CurInst->getOperand(1);
      auto &op2 = CurInst->getOperand(2);
      auto &op3 = CurInst->getOperand(3);
      assert(op0.isReg() && op1.isReg() && op2.isReg());
      assert(op3.isImm());

      auto baseReg = op2.getReg();
      assert((baseReg >= AArch64::X0 && baseReg <= AArch64::X28) ||
             (baseReg == AArch64::SP) || (baseReg == AArch64::LR) ||
             (baseReg == AArch64::FP));
      auto baseAddr = readPtrFromReg(baseReg);
      auto val1 = readFromReg(op0.getReg());
      auto val2 = readFromReg(op1.getReg());

      auto imm = op3.getImm();

      u_int64_t size = 0;
      switch (opcode) {
      case AArch64::STPWi:
      case AArch64::STPSi: {
        size = 4;
        val1 = createTrunc(val1, i32);
        val2 = createTrunc(val2, i32);
        break;
      }
      case AArch64::STPXi: {
        size = 8;
        break;
      }
      case AArch64::STPDi: {
        size = 8;
        val1 = createTrunc(val1, i64);
        val2 = createTrunc(val2, i64);
        break;
      }
      case AArch64::STPQi: {
        size = 16;
        break;
      }
      default: {
        *out << "\nError Unknown opcode\n";
        visitError();
        break;
      }
      }
      assert(size != 0);

      storeToMemoryImmOffset(baseAddr, imm * size, size, val1);
      storeToMemoryImmOffset(baseAddr, (imm + 1) * size, size, val2);
      break;
    }

    case AArch64::LDPWpre:
    case AArch64::LDPSpre:
    case AArch64::LDPXpre:
    case AArch64::LDPDpre:
    case AArch64::LDPQpre:
    case AArch64::LDPWpost:
    case AArch64::LDPSpost:
    case AArch64::LDPXpost:
    case AArch64::LDPDpost:
    case AArch64::LDPQpost: {
      unsigned scale;
      switch (opcode) {
      case AArch64::LDPWpre:
      case AArch64::LDPSpre:
      case AArch64::LDPWpost:
      case AArch64::LDPSpost: {
        scale = 2;
        break;
      }
      case AArch64::LDPXpre:
      case AArch64::LDPDpre:
      case AArch64::LDPXpost:
      case AArch64::LDPDpost: {
        scale = 3;
        break;
      }
      case AArch64::LDPQpre:
      case AArch64::LDPQpost: {
        scale = 4;
        break;
      }
      default: {
        *out << "\nError Unknown opcode\n";
        visitError();
      }
      }
      unsigned size = pow(2, scale);
      auto &op0 = CurInst->getOperand(0);
      auto &op1 = CurInst->getOperand(1);
      auto &op2 = CurInst->getOperand(2);
      auto &op3 = CurInst->getOperand(3);
      auto &op4 = CurInst->getOperand(4);
      assert(op0.isReg() && op1.isReg() && op2.isReg() && op3.isReg());
      assert(op0.getReg() == op3.getReg());
      assert(op4.isImm());

      auto destReg1 = op1.getReg();
      auto destReg2 = op2.getReg();
      auto baseReg = op3.getReg();
      auto imm = op4.getImm();
      assert((baseReg >= AArch64::X0 && baseReg <= AArch64::X28) ||
             (baseReg == AArch64::SP) || (baseReg == AArch64::LR) ||
             (baseReg == AArch64::FP) || (baseReg == AArch64::XZR));
      auto base = readPtrFromReg(baseReg);
      auto baseAddr = createPtrToInt(base, i64);

      // Start offset as 7-bit signed integer
      assert(imm <= 63 && imm >= -64);
      auto offset = getIntConst(imm, 7);
      Value *offsetVal1 =
          createMaskedShl(createSExt(offset, i64), getIntConst(scale, 64));
      Value *offsetVal2 = createAdd(offsetVal1, getIntConst(size, 64));
      auto zeroVal = getIntConst(0, 64);

      bool isPre = opcode == AArch64::LDPWpre || opcode == AArch64::LDPSpre ||
                   opcode == AArch64::LDPXpre || opcode == AArch64::LDPDpre ||
                   opcode == AArch64::LDPQpre;

      Value *loaded1, *loaded2;
      if (isPre) {
        loaded1 = makeLoadWithOffset(base, offsetVal1, size);
        loaded2 = makeLoadWithOffset(base, offsetVal2, size);
      } else {
        loaded1 = makeLoadWithOffset(base, zeroVal, size);
        loaded2 = makeLoadWithOffset(base, getIntConst(size, 64), size);
      }
      updateReg(loaded1, destReg1);
      updateReg(loaded2, destReg2);

      auto added = createAdd(baseAddr, offsetVal1);
      updateOutputReg(added);
      break;
    }

    case AArch64::STPWpre:
    case AArch64::STPSpre:
    case AArch64::STPXpre:
    case AArch64::STPDpre:
    case AArch64::STPQpre:
    case AArch64::STPWpost:
    case AArch64::STPSpost:
    case AArch64::STPXpost:
    case AArch64::STPDpost:
    case AArch64::STPQpost: {
      auto &op0 = CurInst->getOperand(0);
      auto &op1 = CurInst->getOperand(1);
      auto &op2 = CurInst->getOperand(2);
      auto &op3 = CurInst->getOperand(3);
      auto &op4 = CurInst->getOperand(4);
      assert(op0.isReg() && op1.isReg() && op2.isReg() && op3.isReg());
      assert(op0.getReg() == op3.getReg());
      assert(op4.isImm());

      auto srcReg1 = op1.getReg();
      auto srcReg2 = op2.getReg();
      auto baseReg = op3.getReg();
      auto imm = op4.getImm();
      assert((baseReg >= AArch64::X0 && baseReg <= AArch64::X28) ||
             (baseReg == AArch64::SP) || (baseReg == AArch64::LR) ||
             (baseReg == AArch64::FP) || (baseReg == AArch64::XZR));
      auto base = readPtrFromReg(baseReg);
      auto baseAddr = createPtrToInt(base, i64);

      unsigned scale;
      Value *loaded1, *loaded2;

      switch (opcode) {
      case AArch64::STPWpre:
      case AArch64::STPSpre:
      case AArch64::STPWpost:
      case AArch64::STPSpost: {
        scale = 2;
        loaded1 = createTrunc(readFromReg(srcReg1), i32);
        loaded2 = createTrunc(readFromReg(srcReg2), i32);
        break;
      }
      case AArch64::STPXpre:
      case AArch64::STPXpost: {
        scale = 3;
        loaded1 = readFromReg(srcReg1);
        loaded2 = readFromReg(srcReg2);
        break;
      }
      case AArch64::STPDpre:
      case AArch64::STPDpost: {
        scale = 3;
        loaded1 = createTrunc(readFromReg(srcReg1), i64);
        loaded2 = createTrunc(readFromReg(srcReg2), i64);
        break;
      }
      case AArch64::STPQpre:
      case AArch64::STPQpost: {
        scale = 4;
        loaded1 = readFromReg(srcReg1);
        loaded2 = readFromReg(srcReg2);
        break;
      }
      default: {
        *out << "\nError Unknown opcode\n";
        visitError();
      }
      }
      unsigned size = pow(2, scale);

      // Start offset as 7-bit signed integer
      assert(imm <= 63 && imm >= -64);
      auto offset = getIntConst(imm, 7);
      Value *offsetVal1 =
          createMaskedShl(createSExt(offset, i64), getIntConst(scale, 64));
      Value *offsetVal2 = createAdd(offsetVal1, getIntConst(size, 64));
      auto zeroVal = getIntConst(0, 64);

      bool isPre = opcode == AArch64::STPWpre || opcode == AArch64::STPSpre ||
                   opcode == AArch64::STPXpre || opcode == AArch64::STPDpre ||
                   opcode == AArch64::STPQpre;

      if (isPre) {
        storeToMemoryValOffset(base, offsetVal1, size, loaded1);
        storeToMemoryValOffset(base, offsetVal2, size, loaded2);
      } else {
        storeToMemoryValOffset(base, zeroVal, size, loaded1);
        storeToMemoryValOffset(base, getIntConst(size, 64), size, loaded2);
      }

      auto added = createAdd(baseAddr, offsetVal1);
      updateOutputReg(added);
      break;
    }

    case AArch64::ADRP: {
      assert(CurInst->getOperand(0).isReg());
      mapExprVar(CurInst->getOperand(1).getExpr());
      break;
    }

    case AArch64::CBZW:
    case AArch64::CBZX: {
      auto operand = readFromOperand(0);
      assert(operand != nullptr && "operand is null");
      auto cond_val = createICmp(ICmpInst::Predicate::ICMP_EQ, operand,
                                 getIntConst(0, getBitWidth(operand)));
      auto dst_true = getBB(CurInst->getOperand(1));
      assert(dst_true);
      assert(MCBB->getSuccs().size() == 1 || MCBB->getSuccs().size() == 2);

      BasicBlock *dst_false{nullptr};
      if (MCBB->getSuccs().size() == 1) {
        dst_false = dst_true;
      } else {
        const string *dst_false_name = nullptr;
        for (auto &succ : MCBB->getSuccs()) {
          if (succ->getName() != dst_true->getName()) {
            dst_false_name = &succ->getName();
            break;
          }
        }
        assert(dst_false_name != nullptr);
        dst_false = getBBByName(*dst_false_name);
      }
      createBranch(cond_val, dst_true, dst_false);
      break;
    }

    case AArch64::CBNZW:
    case AArch64::CBNZX: {
      auto operand = readFromOperand(0);
      assert(operand != nullptr && "operand is null");
      auto cond_val = createICmp(ICmpInst::Predicate::ICMP_NE, operand,
                                 getIntConst(0, getBitWidth(operand)));

      auto dst_true = getBB(CurInst->getOperand(1));
      assert(dst_true);
      assert(MCBB->getSuccs().size() == 2 && "expected 2 successors");

      const string *dst_false_name = nullptr;
      for (auto &succ : MCBB->getSuccs()) {
        if (succ->getName() != dst_true->getName()) {
          dst_false_name = &succ->getName();
          break;
        }
      }
      assert(dst_false_name != nullptr);
      auto *dst_false = getBBByName(*dst_false_name);
      createBranch(cond_val, dst_true, dst_false);
      break;
    }

    case AArch64::TBZW:
    case AArch64::TBZX:
    case AArch64::TBNZW:
    case AArch64::TBNZX: {
      auto size = getInstSize(opcode);
      auto operand = readFromOperand(0);
      assert(operand != nullptr && "operand is null");
      auto bit_pos = getImm(1);
      auto shift = createMaskedLShr(operand, getIntConst(bit_pos, size));
      auto cond_val = createTrunc(shift, i1);

      auto &jmp_tgt_op = CurInst->getOperand(2);
      assert(jmp_tgt_op.isExpr() && "expected expression");
      assert((jmp_tgt_op.getExpr()->getKind() == MCExpr::ExprKind::SymbolRef) &&
             "expected symbol ref as bcc operand");
      const MCSymbolRefExpr &SRE = cast<MCSymbolRefExpr>(*jmp_tgt_op.getExpr());
      const MCSymbol &Sym =
          SRE.getSymbol(); // FIXME refactor this into a function
      auto *dst_false = getBBByName(Sym.getName());

      assert(MCBB->getSuccs().size() == 1 || MCBB->getSuccs().size() == 2);

      const string *dst_true_name = nullptr;
      for (auto &succ : MCBB->getSuccs()) {
        if (succ->getName() != Sym.getName()) {
          dst_true_name = &succ->getName();
          break;
        }
      }
      auto *dst_true =
          getBBByName(dst_true_name ? *dst_true_name : Sym.getName());

      if (opcode == AArch64::TBNZW || opcode == AArch64::TBNZX)
        createBranch(cond_val, dst_false, dst_true);
      else
        createBranch(cond_val, dst_true, dst_false);
      break;
    }

    case AArch64::INSvi8lane:
    case AArch64::INSvi16lane:
    case AArch64::INSvi32lane:
    case AArch64::INSvi64lane: {
      unsigned w;
      if (opcode == AArch64::INSvi8lane) {
        w = 8;
      } else if (opcode == AArch64::INSvi16lane) {
        w = 16;
      } else if (opcode == AArch64::INSvi32lane) {
        w = 32;
      } else if (opcode == AArch64::INSvi64lane) {
        w = 64;
      } else {
        assert(false);
      }
      auto in = readFromVecOperand(3, w, 128 / w);
      auto out = readFromVecOperand(1, w, 128 / w);
      auto ext = createExtractElement(in, getImm(4));
      auto ins = createInsertElement(out, ext, getImm(2));
      updateOutputReg(ins);
      break;
    }

    case AArch64::INSvi8gpr:
    case AArch64::INSvi16gpr:
    case AArch64::INSvi32gpr:
    case AArch64::INSvi64gpr: {
      unsigned w;
      if (opcode == AArch64::INSvi8gpr) {
        w = 8;
      } else if (opcode == AArch64::INSvi16gpr) {
        w = 16;
      } else if (opcode == AArch64::INSvi32gpr) {
        w = 32;
      } else if (opcode == AArch64::INSvi64gpr) {
        w = 64;
      } else {
        assert(false);
      }
      auto val = readFromOperand(3);
      // need to clear extraneous bits
      if (w < 32)
        val = createTrunc(val, getIntTy(w));
      auto lane = getImm(2);
      auto orig = readFromReg(CurInst->getOperand(1).getReg());
      auto vec = createBitCast(orig, getVecTy(w, 128 / w));
      auto inserted = createInsertElement(vec, val, lane);
      updateOutputReg(inserted);
      break;
    }

    case AArch64::FNEGSr:
    case AArch64::FNEGDr: {
      auto operandSize = getRegSize(CurInst->getOperand(1).getReg());
      auto fVal = readFromFPOperand(1, operandSize);
      auto res = createFNeg(fVal);
      updateOutputReg(res);
      break;
    }

    case AArch64::FNEGv2f32:
    case AArch64::FNEGv4f32:
    case AArch64::FNEGv2f64: {
      unsigned eltSize, numElts;
      switch (opcode) {
      case AArch64::FNEGv2f32: {
        eltSize = 32;
        numElts = 2;
        break;
      }
      case AArch64::FNEGv4f16: {
        eltSize = 16;
        numElts = 4;
        break;
      }
      case AArch64::FNEGv4f32: {
        eltSize = 32;
        numElts = 4;
        break;
      }
      case AArch64::FNEGv2f64: {
        eltSize = 64;
        numElts = 2;
        break;
      }
      case AArch64::FNEGv8f16: {
        eltSize = 16;
        numElts = 8;
        break;
      }
      default: {
        assert(false);
        break;
      }
      }
      auto v = readFromVecOperand(1, eltSize, numElts, false, true);
      auto res = createFNeg(v);
      updateOutputReg(res);
      break;
    }

    case AArch64::FCVTZUUWHr:
    case AArch64::FCVTZUUWSr:
    case AArch64::FCVTZUUWDr:
    case AArch64::FCVTZUUXHr:
    case AArch64::FCVTZUUXSr:
    case AArch64::FCVTZUUXDr:
    case AArch64::FCVTZSUWHr:
    case AArch64::FCVTZSUWSr:
    case AArch64::FCVTZSUWDr:
    case AArch64::FCVTZSUXHr:
    case AArch64::FCVTZSUXSr:
    case AArch64::FCVTZSUXDr: {
      auto &op0 = CurInst->getOperand(0);
      auto &op1 = CurInst->getOperand(1);
      assert(op0.isReg() && op1.isReg());

      auto isSigned =
          opcode == AArch64::FCVTZSUWHr || opcode == AArch64::FCVTZSUWSr ||
          opcode == AArch64::FCVTZSUWDr || opcode == AArch64::FCVTZSUXHr ||
          opcode == AArch64::FCVTZSUXSr || opcode == AArch64::FCVTZSUXDr;

      auto op0Size = getRegSize(op0.getReg());
      auto op1Size = getRegSize(op1.getReg());

      auto fp_val = readFromFPOperand(1, op1Size);
      auto converted = isSigned ? createFPToSI(fp_val, getIntTy(op0Size))
                                : createFPToUI(fp_val, getIntTy(op0Size));
      updateOutputReg(converted);
      break;
    }

    case AArch64::FCVTZSv1i32:
    case AArch64::FCVTZSv1i64: {
      auto &op0 = CurInst->getOperand(0);
      auto &op1 = CurInst->getOperand(1);
      assert(op0.isReg() && op1.isReg());

      auto isSigned =
          opcode == AArch64::FCVTZSv1i32 || opcode == AArch64::FCVTZSv1i64;

      auto op0Size = getRegSize(op0.getReg());
      auto op1Size = getRegSize(op1.getReg());

      auto fp_val = readFromFPOperand(1, op1Size);
      auto converted = isSigned ? createFPToSI(fp_val, getIntTy(op0Size))
                                : createFPToUI(fp_val, getIntTy(op0Size));
      updateOutputReg(converted);
      break;
    }

    case AArch64::FCVTSHr:
    case AArch64::FCVTDHr:
    case AArch64::FCVTHSr:
    case AArch64::FCVTDSr:
    case AArch64::FCVTHDr:
    case AArch64::FCVTSDr: {
      auto &op0 = CurInst->getOperand(0);
      auto &op1 = CurInst->getOperand(1);
      assert(op0.isReg() && op1.isReg());

      auto op0Size = getRegSize(op0.getReg());
      auto op1Size = getRegSize(op1.getReg());

      auto fTy = getFPType(op0Size);
      auto fp_val = readFromFPOperand(1, op1Size);
      auto converted = op0Size < op1Size ? createFPTrunc(fp_val, fTy)
                                         : createFPExt(fp_val, fTy);

      updateOutputReg(converted);
      break;
    }

    case AArch64::UCVTFUWSri:
    case AArch64::UCVTFUWDri:
    case AArch64::UCVTFUXSri:
    case AArch64::UCVTFUXDri:
    case AArch64::SCVTFUWSri:
    case AArch64::SCVTFUWDri:
    case AArch64::SCVTFUXSri:
    case AArch64::SCVTFUXDri: {
      auto &op0 = CurInst->getOperand(0);
      auto &op1 = CurInst->getOperand(1);
      assert(op0.isReg() && op1.isReg());

      auto isSigned =
          opcode == AArch64::SCVTFUWSri || opcode == AArch64::SCVTFUWDri ||
          opcode == AArch64::SCVTFUXSri || opcode == AArch64::SCVTFUXDri;

      auto fTy = getFPType(getRegSize(op0.getReg()));
      auto val = readFromOperand(1, getRegSize(op1.getReg()));
      auto converted =
          isSigned ? createSIToFP(val, fTy) : createUIToFP(val, fTy);

      updateOutputReg(converted);
      break;
    }

    case AArch64::SCVTFv1i32:
    case AArch64::SCVTFv1i64: {
      auto &op0 = CurInst->getOperand(0);
      auto &op1 = CurInst->getOperand(1);
      assert(op0.isReg() && op1.isReg());

      auto isSigned =
          opcode == AArch64::SCVTFv1i32 || opcode == AArch64::SCVTFv1i64;

      auto fTy = getFPType(getRegSize(op0.getReg()));
      auto val = readFromOperand(1, getRegSize(op1.getReg()));
      auto converted =
          isSigned ? createSIToFP(val, fTy) : createUIToFP(val, fTy);

      updateOutputReg(converted);
      break;
    }

    case AArch64::FMOVSWr:
    case AArch64::FMOVDXr:
    case AArch64::FMOVWSr:
    case AArch64::FMOVXDr:
    case AArch64::FMOVDr:
    case AArch64::FMOVSr: {
      auto v = readFromOperand(1);
      updateOutputReg(v);
      break;
    }

    case AArch64::FMOVSi:
    case AArch64::FMOVDi: {
      auto imm = getImm(1);
      assert(imm <= 256);
      int w = (opcode == AArch64::FMOVSi) ? 32 : 64;
      auto floatVal = getIntConst(VFPExpandImm(imm, w), 64);
      updateOutputReg(floatVal);
      break;
    }

    case AArch64::FMOVv2f32_ns:
    case AArch64::FMOVv4f32_ns:
    case AArch64::FMOVv2f64_ns: {
      bool bitWidth128 = false;
      unsigned numElts, eltSize, op;
      switch (opcode) {
      case AArch64::FMOVv2f32_ns: {
        numElts = 2;
        eltSize = 32;
        bitWidth128 = false;
        op = 0;
        break;
      }
      case AArch64::FMOVv4f32_ns: {
        numElts = 4;
        eltSize = 32;
        bitWidth128 = true;
        op = 0;
        break;
      }
      case AArch64::FMOVv2f64_ns: {
        numElts = 2;
        eltSize = 64;
        bitWidth128 = true;
        op = 1;
        break;
      }
      default:
        assert(false);
      }
      unsigned cmode = 15;
      auto imm = getImm(1);
      assert(imm <= 256);
      auto expandedImm = AdvSIMDExpandImm(op, cmode, imm);
      Constant *expandedImmVal = getIntConst(expandedImm, 64);
      if (bitWidth128) {
        // Create a 128-bit vector with the expanded immediate
        expandedImmVal =
            ConstantVector::getSplat(ElementCount::getFixed(2), expandedImmVal);
      }

      auto result =
          createBitCast(expandedImmVal, getVecTy(eltSize, numElts, true));
      updateOutputReg(result);

      break;
    }

    case AArch64::FABSSr:
    case AArch64::FABSDr: {
      auto a =
          readFromFPOperand(1, getRegSize(CurInst->getOperand(1).getReg()));
      auto res = createFAbs(a);
      updateOutputReg(res);
      break;
    }

    case AArch64::FMULSrr:
    case AArch64::FMULDrr:
    case AArch64::FNMULSrr:
    case AArch64::FNMULDrr:
    case AArch64::FDIVSrr:
    case AArch64::FDIVDrr:
    case AArch64::FADDSrr:
    case AArch64::FADDDrr:
    case AArch64::FSUBSrr:
    case AArch64::FSUBDrr: {
      Instruction::BinaryOps op;
      if (opcode == AArch64::FADDSrr || opcode == AArch64::FADDDrr) {
        op = Instruction::FAdd;
      } else if (opcode == AArch64::FMULSrr || opcode == AArch64::FMULDrr ||
                 opcode == AArch64::FNMULSrr || opcode == AArch64::FNMULDrr) {
        op = Instruction::FMul;
      } else if (opcode == AArch64::FDIVSrr || opcode == AArch64::FDIVDrr) {
        op = Instruction::FDiv;
      } else if (opcode == AArch64::FSUBSrr || opcode == AArch64::FSUBDrr) {
        op = Instruction::FSub;
      } else {
        assert(false && "missed a case");
      }

      auto a =
          readFromFPOperand(1, getRegSize(CurInst->getOperand(1).getReg()));
      auto b =
          readFromFPOperand(2, getRegSize(CurInst->getOperand(2).getReg()));

      Value *res = createBinop(a, b, op);
      if (opcode == AArch64::FNMULSrr || opcode == AArch64::FNMULDrr) {
        res = createFNeg(res);
      }
      updateOutputReg(res);
      break;
    }

    case AArch64::FMULv1i32_indexed:
    case AArch64::FMULv1i64_indexed: {
      auto &op0 = CurInst->getOperand(0);
      auto &op1 = CurInst->getOperand(1);
      auto &op2 = CurInst->getOperand(2);
      auto &op3 = CurInst->getOperand(3);
      assert(op0.isReg() && op1.isReg() && op2.isReg() && op3.isImm());
      Instruction::BinaryOps op;
      switch (opcode) {
      case AArch64::FMULv1i32_indexed:
      case AArch64::FMULv1i64_indexed: {
        op = Instruction::FMul;
        break;
      }
      default: {
        assert(false && "missed a case");
      }
      }

      auto eltSize = getRegSize(op1.getReg());
      auto a = readFromFPOperand(1, eltSize);
      auto b = getIndexedFPElement(op3.getImm(), eltSize, op2.getReg());
      auto res = createBinop(a, b, op);
      updateOutputReg(res);
      break;
    }

    case AArch64::FMADDSrrr:
    case AArch64::FMADDDrrr:
    case AArch64::FMSUBSrrr:
    case AArch64::FMSUBDrrr:
    case AArch64::FNMADDSrrr:
    case AArch64::FNMADDDrrr:
    case AArch64::FNMSUBSrrr:
    case AArch64::FNMSUBDrrr: {
      auto &op0 = CurInst->getOperand(0);
      auto &op1 = CurInst->getOperand(1);
      auto &op2 = CurInst->getOperand(2);
      auto &op3 = CurInst->getOperand(3);
      assert(op0.isReg() && op1.isReg() && op2.isReg() && op3.isReg());

      bool negateProduct = false, negateAddend = false;
      switch (opcode) {
      case AArch64::FMSUBSrrr:
      case AArch64::FMSUBDrrr: {
        negateProduct = true;
        break;
      }
      case AArch64::FNMADDSrrr:
      case AArch64::FNMADDDrrr: {
        negateAddend = true;
        negateProduct = true;
        break;
      }
      case AArch64::FNMSUBSrrr:
      case AArch64::FNMSUBDrrr: {
        negateAddend = true;
        break;
      }
      }

      auto a = readFromFPOperand(1, getRegSize(op1.getReg()));
      auto b = readFromFPOperand(2, getRegSize(op2.getReg()));
      auto c = readFromFPOperand(3, getRegSize(op3.getReg()));

      if (negateProduct) {
        a = createFNeg(a);
      }
      if (negateAddend) {
        c = createFNeg(c);
      }

      auto res = createFusedMultiplyAdd(a, b, c);

      updateOutputReg(res);
    }

    case AArch64::FSQRTSr:
    case AArch64::FSQRTDr: {
      auto operandSize = getRegSize(CurInst->getOperand(1).getReg());
      auto a = readFromFPOperand(1, operandSize);
      auto res = createSQRT(a);
      updateOutputReg(res);
      break;
    }

    case AArch64::FCMPSri:
    case AArch64::FCMPDri:
    case AArch64::FCMPESri:
    case AArch64::FCMPEDri:
    case AArch64::FCMPSrr:
    case AArch64::FCMPDrr:
    case AArch64::FCMPESrr:
    case AArch64::FCMPEDrr: {
      auto operandSize = getRegSize(CurInst->getOperand(0).getReg());
      auto a = readFromFPOperand(0, operandSize);
      Value *b;
      if (opcode == AArch64::FCMPSri || opcode == AArch64::FCMPDri ||
          opcode == AArch64::FCMPESri || opcode == AArch64::FCMPEDri) {
        b = ConstantFP::get(getFPType(operandSize), 0.0);
      } else {
        b = readFromFPOperand(1, getRegSize(CurInst->getOperand(1).getReg()));
      }
      setN(createFCmp(FCmpInst::Predicate::FCMP_OLT, a, b));
      setZ(createFCmp(FCmpInst::Predicate::FCMP_OEQ, a, b));
      setC(createFCmp(FCmpInst::Predicate::FCMP_UGT, a, b));
      setV(createFCmp(FCmpInst::Predicate::FCMP_UNO, a, b));
      break;
    }
    case AArch64::SMOVvi8to32_idx0:
    case AArch64::SMOVvi8to32:
    case AArch64::SMOVvi16to32_idx0:
    case AArch64::SMOVvi16to32:
    case AArch64::SMOVvi8to64_idx0:
    case AArch64::SMOVvi8to64:
    case AArch64::SMOVvi16to64_idx0:
    case AArch64::SMOVvi16to64:
    case AArch64::SMOVvi32to64_idx0:
    case AArch64::SMOVvi32to64: {
      auto val = readFromOperand(1);
      auto index = getImm(2);
      int64_t eltSizeLog2;
      Type *truncSize;

      switch (opcode) {
      case AArch64::SMOVvi8to32_idx0:
      case AArch64::SMOVvi8to32:
        eltSizeLog2 = 3;
        truncSize = i8;
        break;
      case AArch64::SMOVvi16to32_idx0:
      case AArch64::SMOVvi16to32:
        eltSizeLog2 = 4;
        truncSize = i16;
        break;
      case AArch64::SMOVvi8to64_idx0:
      case AArch64::SMOVvi8to64:
        eltSizeLog2 = 3;
        truncSize = i8;
        break;
      case AArch64::SMOVvi16to64_idx0:
      case AArch64::SMOVvi16to64:
        eltSizeLog2 = 4;
        truncSize = i16;
        break;
      case AArch64::SMOVvi32to64_idx0:
      case AArch64::SMOVvi32to64:
        eltSizeLog2 = 5;
        truncSize = i32;
        break;
      }
      auto shiftAmt = getIntConst(index << eltSizeLog2, 128);
      auto shifted = createRawLShr(val, shiftAmt);
      auto trunced = createTrunc(shifted, truncSize);
      updateOutputReg(trunced, true);
      break;
    }

    case AArch64::UMOVvi8:
    case AArch64::UMOVvi8_idx0:
    case AArch64::UMOVvi16:
    case AArch64::UMOVvi16_idx0:
    case AArch64::UMOVvi32:
    case AArch64::UMOVvi64: {
      unsigned sz;
      if (opcode == AArch64::UMOVvi8 || opcode == AArch64::UMOVvi8_idx0) {
        sz = 8;
      } else if (opcode == AArch64::UMOVvi16 ||
                 opcode == AArch64::UMOVvi16_idx0) {
        sz = 16;
      } else if (opcode == AArch64::UMOVvi32) {
        sz = 32;
      } else if (opcode == AArch64::UMOVvi64) {
        sz = 64;
      } else {
        assert(false);
      }
      unsigned idx;
      if (opcode == AArch64::UMOVvi8_idx0 || opcode == AArch64::UMOVvi16_idx0) {
        idx = 0;
      } else {
        idx = getImm(2);
      }
      auto vTy = getVecTy(sz, 128 / sz);
      auto reg = createBitCast(readFromOperand(1), vTy);
      auto val = createExtractElement(reg, idx);
      updateOutputReg(val);
      break;
    }

    case AArch64::MVNIv8i16:
    case AArch64::MVNIv4i32:
    case AArch64::MVNIv4i16:
    case AArch64::MVNIv2i32: {
      int numElts, eltSize;
      switch (opcode) {
      case AArch64::MVNIv8i16:
        numElts = 8;
        eltSize = 16;
        break;
      case AArch64::MVNIv4i32:
        numElts = 4;
        eltSize = 32;
        break;
      case AArch64::MVNIv4i16:
        numElts = 4;
        eltSize = 16;
        break;
      case AArch64::MVNIv2i32:
        numElts = 2;
        eltSize = 32;
        break;
      default:
        assert(false);
      }
      auto imm1 = getIntConst(getImm(1), eltSize);
      auto imm2 = getIntConst(getImm(2), eltSize);
      auto v = createNot(createRawShl(imm1, imm2));
      updateOutputReg(dupElts(v, numElts, eltSize));
      break;
    }

    case AArch64::MVNIv2s_msl:
    case AArch64::MVNIv4s_msl: {
      auto imm1 = getIntConst(getImm(1), 32);
      auto imm2 = getImm(2) & ~0x100;
      auto v = createNot(createMSL(imm1, imm2));
      int numElts = (opcode == AArch64::MVNIv2s_msl) ? 2 : 4;
      updateOutputReg(dupElts(v, numElts, 32));
      break;
    }

    case AArch64::MOVIv2s_msl:
    case AArch64::MOVIv4s_msl: {
      auto imm1 = getIntConst(getImm(1), 32);
      auto imm2 = getImm(2) & ~0x100;
      auto v = createMSL(imm1, imm2);
      int numElts = (opcode == AArch64::MOVIv2s_msl) ? 2 : 4;
      updateOutputReg(dupElts(v, numElts, 32));
      break;
    }

    case AArch64::MOVID:
    case AArch64::MOVIv2d_ns: {
      auto imm = getIntConst(replicate8to64(getImm(1)), 64);
      updateOutputReg(dupElts(imm, 2, 64));
      break;
    }

    case AArch64::MOVIv8b_ns: {
      auto v = getIntConst(getImm(1), 8);
      updateOutputReg(dupElts(v, 8, 8));
      break;
    }

    case AArch64::MOVIv16b_ns: {
      auto v = getIntConst(getImm(1), 8);
      updateOutputReg(dupElts(v, 16, 8));
      break;
    }

    case AArch64::MOVIv4i16: {
      auto imm1 = getImm(1);
      auto imm2 = getImm(2);
      auto val = getIntConst(imm1 << imm2, 16);
      updateOutputReg(dupElts(val, 4, 16));
      break;
    }

    case AArch64::MOVIv8i16: {
      auto imm1 = getImm(1);
      auto imm2 = getImm(2);
      auto val = getIntConst(imm1 << imm2, 16);
      updateOutputReg(dupElts(val, 8, 16));
      break;
    }

    case AArch64::MOVIv2i32: {
      auto imm1 = getImm(1);
      auto imm2 = getImm(2);
      auto val = getIntConst(imm1 << imm2, 32);
      updateOutputReg(dupElts(val, 2, 32));
      break;
    }

    case AArch64::MOVIv4i32: {
      auto imm1 = getImm(1);
      auto imm2 = getImm(2);
      auto val = getIntConst(imm1 << imm2, 32);
      updateOutputReg(dupElts(val, 4, 32));
      break;
    }

    case AArch64::EXTv8i8: {
      auto a = readFromOperand(1);
      auto b = readFromOperand(2);
      auto imm = getImm(3);
      auto both = concat(b, a);
      auto shifted = createRawLShr(both, getIntConst(8 * imm, 128));
      updateOutputReg(createTrunc(shifted, i64));
      break;
    }

    case AArch64::EXTv16i8: {
      auto a = readFromOperand(1);
      auto b = readFromOperand(2);
      auto imm = getImm(3);
      auto both = concat(b, a);
      auto shifted = createRawLShr(both, getIntConst(8 * imm, 256));
      updateOutputReg(createTrunc(shifted, i128));
      break;
    }

    case AArch64::REV64v4i32: {
      auto v = rev(readFromOperand(1), 32, 64);
      updateOutputReg(v);
      break;
    }

    case AArch64::REV64v2i32: {
      auto v = rev(readFromOperand(1), 32, 64);
      updateOutputReg(v);
      break;
    }

    case AArch64::REV64v4i16: {
      auto v = rev(readFromOperand(1), 16, 64);
      updateOutputReg(v);
      break;
    }

    case AArch64::REV64v8i8: {
      auto v = rev(readFromOperand(1), 8, 64);
      updateOutputReg(v);
      break;
    }

    case AArch64::REV64v8i16: {
      auto v = rev(readFromOperand(1), 16, 64);
      updateOutputReg(v);
      break;
    }

    case AArch64::REV64v16i8: {
      auto v = rev(readFromOperand(1), 8, 64);
      updateOutputReg(v);
      break;
    }

    case AArch64::REV16v8i8: {
      auto v = rev(readFromOperand(1), 8, 16);
      updateOutputReg(v);
      break;
    }

    case AArch64::REV16v16i8: {
      auto v = rev(readFromOperand(1), 8, 16);
      updateOutputReg(v);
      break;
    }

    case AArch64::REV32v4i16: {
      auto v = rev(readFromOperand(1), 16, 32);
      updateOutputReg(v);
      break;
    }

    case AArch64::REV32v8i8: {
      auto v = rev(readFromOperand(1), 8, 32);
      updateOutputReg(v);
      break;
    }

    case AArch64::REV32v8i16: {
      auto v = rev(readFromOperand(1), 16, 32);
      updateOutputReg(v);
      break;
    }

    case AArch64::REV32v16i8: {
      auto v = rev(readFromOperand(1), 8, 32);
      updateOutputReg(v);
      break;
    }

    case AArch64::DUPi16: {
      auto in = readFromVecOperand(1, 16, 8);
      auto ext = createExtractElement(in, getImm(2));
      updateOutputReg(ext);
      break;
    }

    case AArch64::DUPi32: {
      auto in = readFromVecOperand(1, 32, 4);
      auto ext = createExtractElement(in, getImm(2));
      updateOutputReg(ext);
      break;
    }

    case AArch64::DUPi64: {
      auto in = readFromVecOperand(1, 64, 2);
      auto ext = createExtractElement(in, getImm(2));
      updateOutputReg(ext);
      break;
    }

    case AArch64::DUPv8i8gpr: {
      auto t = createTrunc(readFromOperand(1), i8);
      updateOutputReg(dupElts(t, 8, 8));
      break;
    }

    case AArch64::DUPv16i8gpr: {
      auto t = createTrunc(readFromOperand(1), i8);
      updateOutputReg(dupElts(t, 16, 8));
      break;
    }

    case AArch64::DUPv8i16gpr: {
      auto t = createTrunc(readFromOperand(1), i16);
      updateOutputReg(dupElts(t, 8, 16));
      break;
    }

    case AArch64::DUPv4i16gpr: {
      auto t = createTrunc(readFromOperand(1), i16);
      updateOutputReg(dupElts(t, 4, 16));
      break;
    }

    case AArch64::DUPv4i32gpr: {
      auto t = createTrunc(readFromOperand(1), i32);
      updateOutputReg(dupElts(t, 4, 32));
      break;
    }

    case AArch64::DUPv2i32gpr: {
      auto t = createTrunc(readFromOperand(1), i32);
      updateOutputReg(dupElts(t, 2, 32));
      break;
    }

    case AArch64::DUPv2i64gpr: {
      updateOutputReg(dupElts(readFromOperand(1), 2, 64));
      break;
    }

    case AArch64::DUPv2i32lane: {
      auto in = readFromVecOperand(1, 32, 4);
      auto ext = createExtractElement(in, getImm(2));
      updateOutputReg(dupElts(ext, 2, 32));
      break;
    }

    case AArch64::DUPv2i64lane: {
      auto in = readFromVecOperand(1, 64, 2);
      auto ext = createExtractElement(in, getImm(2));
      updateOutputReg(dupElts(ext, 2, 64));
      break;
    }

    case AArch64::DUPv4i16lane: {
      auto in = readFromVecOperand(1, 16, 8);
      auto ext = createExtractElement(in, getImm(2));
      updateOutputReg(dupElts(ext, 4, 16));
      break;
    }

    case AArch64::DUPv4i32lane: {
      auto in = readFromVecOperand(1, 32, 4);
      auto ext = createExtractElement(in, getImm(2));
      updateOutputReg(dupElts(ext, 4, 32));
      break;
    }

    case AArch64::DUPv8i8lane: {
      auto in = readFromVecOperand(1, 8, 16);
      auto ext = createExtractElement(in, getImm(2));
      updateOutputReg(dupElts(ext, 8, 8));
      break;
    }

    case AArch64::DUPv8i16lane: {
      auto in = readFromVecOperand(1, 16, 8);
      auto ext = createExtractElement(in, getImm(2));
      updateOutputReg(dupElts(ext, 8, 16));
      break;
    }

    case AArch64::DUPv16i8lane: {
      auto in = readFromVecOperand(1, 8, 16);
      auto ext = createExtractElement(in, getImm(2));
      updateOutputReg(dupElts(ext, 16, 8));
      break;
    }

    case AArch64::BIFv8i8:
    case AArch64::BIFv16i8: {
      auto op1 = readFromOperand(1);
      auto op4 = readFromOperand(2);
      auto op3 = createNot(readFromOperand(3));
      auto res = createXor(op1, createAnd(createXor(op1, op4), op3));
      updateOutputReg(res);
      break;
    }

    case AArch64::BITv16i8:
    case AArch64::BITv8i8: {
      auto op4 = readFromOperand(2);
      auto op1 = readFromOperand(1);
      auto op3 = readFromOperand(3);
      auto res = createXor(op1, createAnd(createXor(op1, op4), op3));
      updateOutputReg(res);
      break;
    }

    case AArch64::BSLv8i8:
    case AArch64::BSLv16i8: {
      auto op1 = readFromOperand(3);
      auto op4 = readFromOperand(2);
      auto op3 = readFromOperand(1);
      auto res = createXor(op1, createAnd(createXor(op1, op4), op3));
      updateOutputReg(res);
      break;
    }

    case AArch64::CMGTv4i16rz:
    case AArch64::CMEQv1i64rz:
    case AArch64::CMGTv8i8rz:
    case AArch64::CMLEv4i16rz:
    case AArch64::CMGEv4i16rz:
    case AArch64::CMLEv8i8rz:
    case AArch64::CMLTv1i64rz:
    case AArch64::CMGTv2i32rz:
    case AArch64::CMGEv8i8rz:
    case AArch64::CMLEv2i32rz:
    case AArch64::CMGEv2i32rz:
    case AArch64::CMLEv8i16rz:
    case AArch64::CMLEv16i8rz:
    case AArch64::CMGTv16i8rz:
    case AArch64::CMGTv8i16rz:
    case AArch64::CMGTv2i64rz:
    case AArch64::CMGEv8i16rz:
    case AArch64::CMLEv4i32rz:
    case AArch64::CMGEv2i64rz:
    case AArch64::CMLEv2i64rz:
    case AArch64::CMGEv16i8rz:
    case AArch64::CMGEv4i32rz:
    case AArch64::CMEQv1i64:
    case AArch64::CMEQv16i8:
    case AArch64::CMEQv16i8rz:
    case AArch64::CMEQv2i32:
    case AArch64::CMEQv2i32rz:
    case AArch64::CMEQv2i64:
    case AArch64::CMEQv2i64rz:
    case AArch64::CMEQv4i16:
    case AArch64::CMEQv4i16rz:
    case AArch64::CMEQv4i32:
    case AArch64::CMEQv4i32rz:
    case AArch64::CMEQv8i16:
    case AArch64::CMEQv8i16rz:
    case AArch64::CMEQv8i8:
    case AArch64::CMEQv8i8rz:
    case AArch64::CMGEv16i8:
    case AArch64::CMGEv2i32:
    case AArch64::CMGEv2i64:
    case AArch64::CMGEv4i16:
    case AArch64::CMGEv4i32:
    case AArch64::CMGEv8i16:
    case AArch64::CMGEv8i8:
    case AArch64::CMGTv16i8:
    case AArch64::CMGTv2i32:
    case AArch64::CMGTv2i64:
    case AArch64::CMGTv4i16:
    case AArch64::CMGTv4i32:
    case AArch64::CMGTv4i32rz:
    case AArch64::CMGTv8i16:
    case AArch64::CMGTv8i8:
    case AArch64::CMHIv16i8:
    case AArch64::CMHIv1i64:
    case AArch64::CMGTv1i64:
    case AArch64::CMHIv2i32:
    case AArch64::CMHIv2i64:
    case AArch64::CMHIv4i16:
    case AArch64::CMHIv4i32:
    case AArch64::CMHIv8i16:
    case AArch64::CMHIv8i8:
    case AArch64::CMHSv16i8:
    case AArch64::CMHSv2i32:
    case AArch64::CMHSv2i64:
    case AArch64::CMHSv4i16:
    case AArch64::CMHSv4i32:
    case AArch64::CMHSv8i16:
    case AArch64::CMHSv8i8:
    case AArch64::CMLTv16i8rz:
    case AArch64::CMLTv2i32rz:
    case AArch64::CMLTv2i64rz:
    case AArch64::CMLTv4i16rz:
    case AArch64::CMLTv4i32rz:
    case AArch64::CMLTv8i16rz:
    case AArch64::CMLTv8i8rz:
    case AArch64::CMTSTv4i16:
    case AArch64::CMTSTv2i32:
    case AArch64::CMTSTv16i8:
    case AArch64::CMTSTv2i64:
    case AArch64::CMTSTv4i32:
    case AArch64::CMTSTv8i16:
    case AArch64::CMTSTv8i8: {
      auto a = readFromOperand(1);
      Value *b;
      switch (opcode) {
      case AArch64::CMGTv4i16rz:
      case AArch64::CMEQv1i64rz:
      case AArch64::CMGTv8i8rz:
      case AArch64::CMLEv4i16rz:
      case AArch64::CMGEv4i16rz:
      case AArch64::CMLEv8i8rz:
      case AArch64::CMLTv1i64rz:
      case AArch64::CMGTv2i32rz:
      case AArch64::CMGEv8i8rz:
      case AArch64::CMLEv2i32rz:
      case AArch64::CMGEv2i32rz:
      case AArch64::CMLEv8i16rz:
      case AArch64::CMLEv16i8rz:
      case AArch64::CMGTv16i8rz:
      case AArch64::CMGTv8i16rz:
      case AArch64::CMGTv2i64rz:
      case AArch64::CMGEv8i16rz:
      case AArch64::CMLEv4i32rz:
      case AArch64::CMGEv2i64rz:
      case AArch64::CMLEv2i64rz:
      case AArch64::CMGEv16i8rz:
      case AArch64::CMGEv4i32rz:
      case AArch64::CMEQv16i8rz:
      case AArch64::CMEQv2i32rz:
      case AArch64::CMEQv2i64rz:
      case AArch64::CMEQv4i16rz:
      case AArch64::CMEQv4i32rz:
      case AArch64::CMEQv8i16rz:
      case AArch64::CMEQv8i8rz:
      case AArch64::CMGTv4i32rz:
      case AArch64::CMLTv16i8rz:
      case AArch64::CMLTv2i32rz:
      case AArch64::CMLTv2i64rz:
      case AArch64::CMLTv4i16rz:
      case AArch64::CMLTv4i32rz:
      case AArch64::CMLTv8i16rz:
      case AArch64::CMLTv8i8rz:
        b = getIntConst(0, getInstSize(opcode));
        break;
      case AArch64::CMEQv1i64:
      case AArch64::CMEQv16i8:
      case AArch64::CMEQv2i32:
      case AArch64::CMEQv2i64:
      case AArch64::CMEQv4i16:
      case AArch64::CMEQv4i32:
      case AArch64::CMEQv8i16:
      case AArch64::CMEQv8i8:
      case AArch64::CMGEv16i8:
      case AArch64::CMGEv2i32:
      case AArch64::CMGEv2i64:
      case AArch64::CMGEv4i16:
      case AArch64::CMGEv4i32:
      case AArch64::CMGEv8i16:
      case AArch64::CMGEv8i8:
      case AArch64::CMGTv16i8:
      case AArch64::CMGTv2i32:
      case AArch64::CMGTv2i64:
      case AArch64::CMGTv4i16:
      case AArch64::CMGTv4i32:
      case AArch64::CMGTv8i16:
      case AArch64::CMGTv8i8:
      case AArch64::CMHIv16i8:
      case AArch64::CMGTv1i64:
      case AArch64::CMHIv1i64:
      case AArch64::CMHIv2i32:
      case AArch64::CMHIv2i64:
      case AArch64::CMHIv4i16:
      case AArch64::CMHIv4i32:
      case AArch64::CMHIv8i16:
      case AArch64::CMHIv8i8:
      case AArch64::CMHSv16i8:
      case AArch64::CMHSv2i32:
      case AArch64::CMHSv2i64:
      case AArch64::CMHSv4i16:
      case AArch64::CMHSv4i32:
      case AArch64::CMHSv8i16:
      case AArch64::CMHSv8i8:
      case AArch64::CMTSTv4i16:
      case AArch64::CMTSTv2i32:
      case AArch64::CMTSTv16i8:
      case AArch64::CMTSTv2i64:
      case AArch64::CMTSTv4i32:
      case AArch64::CMTSTv8i16:
      case AArch64::CMTSTv8i8:
        b = readFromOperand(2);
        break;
      default:
        assert(false);
      }

      int numElts, eltSize;
      switch (opcode) {
      case AArch64::CMEQv1i64rz:
      case AArch64::CMLTv1i64rz:
      case AArch64::CMHIv1i64:
      case AArch64::CMGTv1i64:
      case AArch64::CMEQv1i64:
        numElts = 1;
        eltSize = 64;
        break;
      case AArch64::CMLEv16i8rz:
      case AArch64::CMGTv16i8rz:
      case AArch64::CMGEv16i8rz:
      case AArch64::CMEQv16i8:
      case AArch64::CMEQv16i8rz:
      case AArch64::CMGEv16i8:
      case AArch64::CMGTv16i8:
      case AArch64::CMHIv16i8:
      case AArch64::CMHSv16i8:
      case AArch64::CMLTv16i8rz:
      case AArch64::CMTSTv16i8:
        numElts = 16;
        eltSize = 8;
        break;
      case AArch64::CMTSTv2i32:
      case AArch64::CMEQv2i32:
      case AArch64::CMGTv2i32rz:
      case AArch64::CMLEv2i32rz:
      case AArch64::CMGEv2i32rz:
      case AArch64::CMEQv2i32rz:
      case AArch64::CMGEv2i32:
      case AArch64::CMGTv2i32:
      case AArch64::CMHIv2i32:
      case AArch64::CMHSv2i32:
      case AArch64::CMLTv2i32rz:
        numElts = 2;
        eltSize = 32;
        break;
      case AArch64::CMEQv2i64:
      case AArch64::CMEQv2i64rz:
      case AArch64::CMGEv2i64:
      case AArch64::CMGTv2i64:
      case AArch64::CMHIv2i64:
      case AArch64::CMHSv2i64:
      case AArch64::CMGTv2i64rz:
      case AArch64::CMGEv2i64rz:
      case AArch64::CMLEv2i64rz:
      case AArch64::CMLTv2i64rz:
      case AArch64::CMTSTv2i64:
        numElts = 2;
        eltSize = 64;
        break;
      case AArch64::CMLEv4i16rz:
      case AArch64::CMGEv4i16rz:
      case AArch64::CMGTv4i16rz:
      case AArch64::CMTSTv4i16:
      case AArch64::CMEQv4i16:
      case AArch64::CMEQv4i16rz:
      case AArch64::CMGEv4i16:
      case AArch64::CMGTv4i16:
      case AArch64::CMHIv4i16:
      case AArch64::CMHSv4i16:
      case AArch64::CMLTv4i16rz:
        numElts = 4;
        eltSize = 16;
        break;
      case AArch64::CMLEv4i32rz:
      case AArch64::CMGEv4i32rz:
      case AArch64::CMEQv4i32:
      case AArch64::CMEQv4i32rz:
      case AArch64::CMGEv4i32:
      case AArch64::CMGTv4i32:
      case AArch64::CMGTv4i32rz:
      case AArch64::CMHIv4i32:
      case AArch64::CMHSv4i32:
      case AArch64::CMLTv4i32rz:
      case AArch64::CMTSTv4i32:
        numElts = 4;
        eltSize = 32;
        break;
      case AArch64::CMLEv8i16rz:
      case AArch64::CMGTv8i16rz:
      case AArch64::CMGEv8i16rz:
      case AArch64::CMEQv8i16:
      case AArch64::CMEQv8i16rz:
      case AArch64::CMGEv8i16:
      case AArch64::CMGTv8i16:
      case AArch64::CMHIv8i16:
      case AArch64::CMHSv8i16:
      case AArch64::CMLTv8i16rz:
      case AArch64::CMTSTv8i16:
        numElts = 8;
        eltSize = 16;
        break;
      case AArch64::CMGTv8i8rz:
      case AArch64::CMLEv8i8rz:
      case AArch64::CMGEv8i8rz:
      case AArch64::CMEQv8i8:
      case AArch64::CMEQv8i8rz:
      case AArch64::CMGEv8i8:
      case AArch64::CMGTv8i8:
      case AArch64::CMHIv8i8:
      case AArch64::CMHSv8i8:
      case AArch64::CMLTv8i8rz:
      case AArch64::CMTSTv8i8:
        numElts = 8;
        eltSize = 8;
        break;
      default:
        assert(false);
      }

      auto vTy = getVecTy(eltSize, numElts);
      a = createBitCast(a, vTy);
      b = createBitCast(b, vTy);
      Value *res;

      switch (opcode) {
      case AArch64::CMLEv16i8rz:
      case AArch64::CMLEv2i32rz:
      case AArch64::CMLEv2i64rz:
      case AArch64::CMLEv4i16rz:
      case AArch64::CMLEv4i32rz:
      case AArch64::CMLEv8i16rz:
      case AArch64::CMLEv8i8rz:
        res = createICmp(ICmpInst::Predicate::ICMP_SLE, a, b);
        break;
      case AArch64::CMEQv1i64rz:
      case AArch64::CMEQv1i64:
      case AArch64::CMEQv16i8:
      case AArch64::CMEQv16i8rz:
      case AArch64::CMEQv2i32:
      case AArch64::CMEQv2i32rz:
      case AArch64::CMEQv2i64:
      case AArch64::CMEQv2i64rz:
      case AArch64::CMEQv4i16:
      case AArch64::CMEQv4i16rz:
      case AArch64::CMEQv4i32:
      case AArch64::CMEQv4i32rz:
      case AArch64::CMEQv8i16:
      case AArch64::CMEQv8i16rz:
      case AArch64::CMEQv8i8:
      case AArch64::CMEQv8i8rz:
        res = createICmp(ICmpInst::Predicate::ICMP_EQ, a, b);
        break;
      case AArch64::CMGEv16i8rz:
      case AArch64::CMGEv2i32rz:
      case AArch64::CMGEv2i64rz:
      case AArch64::CMGEv4i16rz:
      case AArch64::CMGEv4i32rz:
      case AArch64::CMGEv8i16rz:
      case AArch64::CMGEv8i8rz:
      case AArch64::CMGEv16i8:
      case AArch64::CMGEv2i32:
      case AArch64::CMGEv2i64:
      case AArch64::CMGEv4i16:
      case AArch64::CMGEv4i32:
      case AArch64::CMGEv8i16:
      case AArch64::CMGEv8i8:
        res = createICmp(ICmpInst::Predicate::ICMP_SGE, a, b);
        break;
      case AArch64::CMGTv16i8rz:
      case AArch64::CMGTv2i32rz:
      case AArch64::CMGTv2i64rz:
      case AArch64::CMGTv4i16rz:
      case AArch64::CMGTv8i16rz:
      case AArch64::CMGTv8i8rz:
      case AArch64::CMGTv1i64:
      case AArch64::CMGTv16i8:
      case AArch64::CMGTv2i32:
      case AArch64::CMGTv2i64:
      case AArch64::CMGTv4i16:
      case AArch64::CMGTv4i32:
      case AArch64::CMGTv4i32rz:
      case AArch64::CMGTv8i16:
      case AArch64::CMGTv8i8:
        res = createICmp(ICmpInst::Predicate::ICMP_SGT, a, b);
        break;
      case AArch64::CMHIv16i8:
      case AArch64::CMHIv1i64:
      case AArch64::CMHIv2i32:
      case AArch64::CMHIv2i64:
      case AArch64::CMHIv4i16:
      case AArch64::CMHIv4i32:
      case AArch64::CMHIv8i16:
      case AArch64::CMHIv8i8:
        res = createICmp(ICmpInst::Predicate::ICMP_UGT, a, b);
        break;
      case AArch64::CMHSv16i8:
      case AArch64::CMHSv2i32:
      case AArch64::CMHSv2i64:
      case AArch64::CMHSv4i16:
      case AArch64::CMHSv4i32:
      case AArch64::CMHSv8i16:
      case AArch64::CMHSv8i8:
        res = createICmp(ICmpInst::Predicate::ICMP_UGE, a, b);
        break;
      case AArch64::CMLTv1i64rz:
      case AArch64::CMLTv16i8rz:
      case AArch64::CMLTv2i32rz:
      case AArch64::CMLTv2i64rz:
      case AArch64::CMLTv4i16rz:
      case AArch64::CMLTv4i32rz:
      case AArch64::CMLTv8i16rz:
      case AArch64::CMLTv8i8rz:
        res = createICmp(ICmpInst::Predicate::ICMP_SLT, a, b);
        break;
      case AArch64::CMTSTv2i32:
      case AArch64::CMTSTv4i16:
      case AArch64::CMTSTv16i8:
      case AArch64::CMTSTv2i64:
      case AArch64::CMTSTv4i32:
      case AArch64::CMTSTv8i16:
      case AArch64::CMTSTv8i8: {
        auto *tmp = createAnd(a, b);
        auto *zero = createBitCast(getIntConst(0, getInstSize(opcode)), vTy);
        res = createICmp(ICmpInst::Predicate::ICMP_NE, tmp, zero);
        break;
      }
      default:
        assert(false);
      }

      updateOutputReg(createSExt(res, vTy));
      break;
    }

    case AArch64::TBLv8i8One:
    case AArch64::TBLv8i8Two:
    case AArch64::TBLv8i8Three:
    case AArch64::TBLv8i8Four:
    case AArch64::TBLv16i8One:
    case AArch64::TBLv16i8Two:
    case AArch64::TBLv16i8Three:
    case AArch64::TBLv16i8Four: {
      int lanes;
      switch (opcode) {
      case AArch64::TBLv8i8One:
      case AArch64::TBLv8i8Two:
      case AArch64::TBLv8i8Three:
      case AArch64::TBLv8i8Four:
        lanes = 8;
        break;
      case AArch64::TBLv16i8One:
      case AArch64::TBLv16i8Two:
      case AArch64::TBLv16i8Three:
      case AArch64::TBLv16i8Four:
        lanes = 16;
        break;
      default:
        assert(false);
      }
      int nregs;
      switch (opcode) {
      case AArch64::TBLv8i8One:
      case AArch64::TBLv16i8One:
        nregs = 1;
        break;
      case AArch64::TBLv8i8Two:
      case AArch64::TBLv16i8Two:
        nregs = 2;
        break;
      case AArch64::TBLv8i8Three:
      case AArch64::TBLv16i8Three:
        nregs = 3;
        break;
      case AArch64::TBLv8i8Four:
      case AArch64::TBLv16i8Four:
        nregs = 4;
        break;
      default:
        assert(false);
      }
      auto vTy = getVecTy(8, lanes);
      auto fullTy = getVecTy(8, 16);
      auto baseReg = decodeRegSet(CurInst->getOperand(1).getReg());
      vector<Value *> regs;
      for (int i = 0; i < nregs; ++i) {
        regs.push_back(createBitCast(readFromReg(baseReg), fullTy));
        baseReg++;
        if (baseReg > AArch64::Q31)
          baseReg = AArch64::Q0;
      }
      auto src = createBitCast(readFromOperand(2), vTy);
      Value *res = getUndefVec(lanes, 8);
      for (int i = 0; i < lanes; ++i) {
        auto idx = createExtractElement(src, i);
        auto entry = tblHelper(regs, idx);
        res = createInsertElement(res, entry, i);
      }
      updateOutputReg(res);
      break;
    }

      // lane-wise binary vector instructions
    case AArch64::ORNv8i8:
    case AArch64::ORNv16i8:
    case AArch64::SMINv8i8:
    case AArch64::SMINv4i16:
    case AArch64::SMINv2i32:
    case AArch64::SMINv16i8:
    case AArch64::SMINv8i16:
    case AArch64::SMINv4i32:
    case AArch64::SMAXv8i8:
    case AArch64::SMAXv4i16:
    case AArch64::SMAXv2i32:
    case AArch64::SMAXv16i8:
    case AArch64::SMAXv8i16:
    case AArch64::SMAXv4i32:
    case AArch64::UMINv8i8:
    case AArch64::UMINv4i16:
    case AArch64::UMINv2i32:
    case AArch64::UMINv16i8:
    case AArch64::UMINv8i16:
    case AArch64::UMINv4i32:
    case AArch64::UMAXv8i8:
    case AArch64::UMAXv4i16:
    case AArch64::UMAXv2i32:
    case AArch64::UMAXv16i8:
    case AArch64::UMAXv8i16:
    case AArch64::UMAXv4i32:
    case AArch64::SMULLv8i8_v8i16:
    case AArch64::SMULLv2i32_v2i64:
    case AArch64::SMULLv4i16_v4i32:
    case AArch64::SMULLv8i16_v4i32:
    case AArch64::SMULLv16i8_v8i16:
    case AArch64::SMULLv4i32_v2i64:
    case AArch64::USHRv8i8_shift:
    case AArch64::USHRv4i16_shift:
    case AArch64::USHRv2i32_shift:
    case AArch64::USHRv16i8_shift:
    case AArch64::USHRv8i16_shift:
    case AArch64::USHRv4i32_shift:
    case AArch64::MULv2i32:
    case AArch64::MULv8i8:
    case AArch64::MULv4i16:
    case AArch64::MULv16i8:
    case AArch64::MULv8i16:
    case AArch64::MULv4i32:
    case AArch64::SSHRv4i16_shift:
    case AArch64::SSHRv8i8_shift:
    case AArch64::SSHRv2i32_shift:
    case AArch64::SSHRv16i8_shift:
    case AArch64::SSHRv2i64_shift:
    case AArch64::SSHRv8i16_shift:
    case AArch64::SSHRv4i32_shift:
    case AArch64::SHLv16i8_shift:
    case AArch64::SHLv8i16_shift:
    case AArch64::SHLv4i32_shift:
    case AArch64::SHLv2i64_shift:
    case AArch64::SHLv8i8_shift:
    case AArch64::SHLv4i16_shift:
    case AArch64::SHLv2i32_shift:
    case AArch64::BICv4i16:
    case AArch64::BICv8i8:
    case AArch64::BICv2i32:
    case AArch64::BICv8i16:
    case AArch64::BICv4i32:
    case AArch64::BICv16i8:
    case AArch64::ADDv2i32:
    case AArch64::ADDv1i64:
    case AArch64::ADDv2i64:
    case AArch64::ADDv4i16:
    case AArch64::ADDv4i32:
    case AArch64::ADDv8i8:
    case AArch64::ADDv8i16:
    case AArch64::ADDv16i8:
    case AArch64::UADDLv8i8_v8i16:
    case AArch64::UADDLv16i8_v8i16:
    case AArch64::UADDLv4i16_v4i32:
    case AArch64::UADDLv8i16_v4i32:
    case AArch64::UADDLv2i32_v2i64:
    case AArch64::UADDLv4i32_v2i64:
    case AArch64::UADDWv8i8_v8i16:
    case AArch64::UADDWv16i8_v8i16:
    case AArch64::UADDWv4i16_v4i32:
    case AArch64::UADDWv8i16_v4i32:
    case AArch64::UADDWv2i32_v2i64:
    case AArch64::UADDWv4i32_v2i64:
    case AArch64::SADDLv8i8_v8i16:
    case AArch64::SADDLv16i8_v8i16:
    case AArch64::SADDLv4i16_v4i32:
    case AArch64::SADDLv8i16_v4i32:
    case AArch64::SADDLv2i32_v2i64:
    case AArch64::SADDLv4i32_v2i64:
    case AArch64::SADDWv8i8_v8i16:
    case AArch64::SADDWv16i8_v8i16:
    case AArch64::SADDWv4i16_v4i32:
    case AArch64::SADDWv8i16_v4i32:
    case AArch64::SADDWv2i32_v2i64:
    case AArch64::SADDWv4i32_v2i64:
    case AArch64::USUBLv8i8_v8i16:
    case AArch64::USUBLv16i8_v8i16:
    case AArch64::USUBLv4i16_v4i32:
    case AArch64::USUBLv8i16_v4i32:
    case AArch64::USUBLv2i32_v2i64:
    case AArch64::USUBLv4i32_v2i64:
    case AArch64::USUBWv8i8_v8i16:
    case AArch64::USUBWv16i8_v8i16:
    case AArch64::USUBWv4i16_v4i32:
    case AArch64::USUBWv8i16_v4i32:
    case AArch64::USUBWv2i32_v2i64:
    case AArch64::USUBWv4i32_v2i64:
    case AArch64::SSUBLv8i8_v8i16:
    case AArch64::SSUBLv16i8_v8i16:
    case AArch64::SSUBLv4i16_v4i32:
    case AArch64::SSUBLv8i16_v4i32:
    case AArch64::SSUBLv2i32_v2i64:
    case AArch64::SSUBLv4i32_v2i64:
    case AArch64::SSUBWv8i8_v8i16:
    case AArch64::SSUBWv16i8_v8i16:
    case AArch64::SSUBWv4i16_v4i32:
    case AArch64::SSUBWv8i16_v4i32:
    case AArch64::SSUBWv2i32_v2i64:
    case AArch64::SSUBWv4i32_v2i64:
    case AArch64::SUBv2i32:
    case AArch64::SUBv2i64:
    case AArch64::SUBv4i16:
    case AArch64::SUBv4i32:
    case AArch64::SUBv8i8:
    case AArch64::SUBv8i16:
    case AArch64::SUBv16i8:
    case AArch64::EORv8i8:
    case AArch64::EORv16i8:
    case AArch64::ANDv8i8:
    case AArch64::ANDv16i8:
    case AArch64::ORRv8i8:
    case AArch64::ORRv16i8:
    case AArch64::ORRv2i32:
    case AArch64::ORRv4i16:
    case AArch64::ORRv8i16:
    case AArch64::ORRv4i32:
    case AArch64::UMULLv2i32_v2i64:
    case AArch64::UMULLv8i8_v8i16:
    case AArch64::UMULLv4i16_v4i32:
    case AArch64::UMULLv16i8_v8i16:
    case AArch64::UMULLv8i16_v4i32:
    case AArch64::UMULLv4i32_v2i64:
    case AArch64::USHLv1i64:
    case AArch64::USHLv4i16:
    case AArch64::USHLv16i8:
    case AArch64::USHLv8i16:
    case AArch64::USHLv2i32:
    case AArch64::USHLv4i32:
    case AArch64::USHLv8i8:
    case AArch64::USHLv2i64:
    case AArch64::SSHLv1i64:
    case AArch64::SSHLv4i16:
    case AArch64::SSHLv16i8:
    case AArch64::SSHLv8i16:
    case AArch64::SSHLv2i32:
    case AArch64::SSHLv4i32:
    case AArch64::SSHLv8i8:
    case AArch64::SSHLv2i64:
    case AArch64::SSHLLv8i8_shift:
    case AArch64::SSHLLv4i16_shift:
    case AArch64::SSHLLv2i32_shift:
    case AArch64::SSHLLv4i32_shift:
    case AArch64::SSHLLv8i16_shift:
    case AArch64::SSHLLv16i8_shift:
    case AArch64::USHLLv8i8_shift:
    case AArch64::USHLLv4i32_shift:
    case AArch64::USHLLv8i16_shift:
    case AArch64::USHLLv16i8_shift:
    case AArch64::USHLLv4i16_shift:
    case AArch64::USHLLv2i32_shift:
    case AArch64::USHRv2i64_shift: {
      unsigned op1Size = 0;
      switch (opcode) {
      case AArch64::UADDWv8i8_v8i16:
      case AArch64::UADDWv4i16_v4i32:
      case AArch64::UADDWv2i32_v2i64:
      case AArch64::USUBWv8i8_v8i16:
      case AArch64::USUBWv4i16_v4i32:
      case AArch64::USUBWv2i32_v2i64:
      case AArch64::SADDWv8i8_v8i16:
      case AArch64::SADDWv4i16_v4i32:
      case AArch64::SADDWv2i32_v2i64:
      case AArch64::SSUBWv8i8_v8i16:
      case AArch64::SSUBWv4i16_v4i32:
      case AArch64::SSUBWv2i32_v2i64: {
        op1Size = 128;
        break;
      }
      }
      auto a = readFromOperand(1, op1Size);
      auto b = readFromOperand(2);
      bool elementWise = false;
      bool splatImm2 = false;
      extKind ext = extKind::None;
      bool immShift = false;
      bool isUpper = false;
      function<Value *(Value *, Value *)> op;
      switch (opcode) {
      case AArch64::SMINv8i8:
      case AArch64::SMINv4i16:
      case AArch64::SMINv2i32:
      case AArch64::SMINv16i8:
      case AArch64::SMINv8i16:
      case AArch64::SMINv4i32:
        op = [&](Value *a, Value *b) { return createSMin(a, b); };
        break;
      case AArch64::SMAXv8i8:
      case AArch64::SMAXv4i16:
      case AArch64::SMAXv2i32:
      case AArch64::SMAXv16i8:
      case AArch64::SMAXv8i16:
      case AArch64::SMAXv4i32:
        op = [&](Value *a, Value *b) { return createSMax(a, b); };
        break;
      case AArch64::UMINv8i8:
      case AArch64::UMINv4i16:
      case AArch64::UMINv2i32:
      case AArch64::UMINv16i8:
      case AArch64::UMINv8i16:
      case AArch64::UMINv4i32:
        op = [&](Value *a, Value *b) { return createUMin(a, b); };
        break;
      case AArch64::UMAXv8i8:
      case AArch64::UMAXv4i16:
      case AArch64::UMAXv2i32:
      case AArch64::UMAXv16i8:
      case AArch64::UMAXv8i16:
      case AArch64::UMAXv4i32:
        op = [&](Value *a, Value *b) { return createUMax(a, b); };
        break;
      case AArch64::UMULLv16i8_v8i16:
      case AArch64::UMULLv8i16_v4i32:
      case AArch64::UMULLv4i32_v2i64:
        // these three cases are UMULL2
        a = createRawLShr(a, getIntConst(64, 128));
        b = createRawLShr(b, getIntConst(64, 128));
      case AArch64::UMULLv2i32_v2i64:
      case AArch64::UMULLv8i8_v8i16:
      case AArch64::UMULLv4i16_v4i32:
        ext = extKind::ZExt;
        op = [&](Value *a, Value *b) { return createMul(a, b); };
        break;
      case AArch64::SMULLv16i8_v8i16:
      case AArch64::SMULLv4i32_v2i64:
      case AArch64::SMULLv8i16_v4i32:
        // these three cases are SMULL2
        a = createRawLShr(a, getIntConst(64, 128));
        b = createRawLShr(b, getIntConst(64, 128));
      case AArch64::SMULLv8i8_v8i16:
      case AArch64::SMULLv2i32_v2i64:
      case AArch64::SMULLv4i16_v4i32:
        ext = extKind::SExt;
        op = [&](Value *a, Value *b) { return createMul(a, b); };
        break;
      case AArch64::USHRv8i8_shift:
      case AArch64::USHRv4i16_shift:
      case AArch64::USHRv2i32_shift:
      case AArch64::USHRv16i8_shift:
      case AArch64::USHRv8i16_shift:
      case AArch64::USHRv4i32_shift:
        splatImm2 = true;
        op = [&](Value *a, Value *b) { return createMaskedLShr(a, b); };
        break;
      case AArch64::MULv2i32:
      case AArch64::MULv8i8:
      case AArch64::MULv4i16:
      case AArch64::MULv16i8:
      case AArch64::MULv8i16:
      case AArch64::MULv4i32:
        op = [&](Value *a, Value *b) { return createMul(a, b); };
        break;
      case AArch64::SSHRv4i16_shift:
      case AArch64::SSHRv8i8_shift:
      case AArch64::SSHRv2i32_shift:
      case AArch64::SSHRv16i8_shift:
      case AArch64::SSHRv2i64_shift:
      case AArch64::SSHRv8i16_shift:
      case AArch64::SSHRv4i32_shift:
        splatImm2 = true;
        op = [&](Value *a, Value *b) { return createMaskedAShr(a, b); };
        break;
      case AArch64::SHLv16i8_shift:
      case AArch64::SHLv8i16_shift:
      case AArch64::SHLv4i32_shift:
      case AArch64::SHLv2i64_shift:
      case AArch64::SHLv8i8_shift:
      case AArch64::SHLv4i16_shift:
      case AArch64::SHLv2i32_shift:
        splatImm2 = true;
        op = [&](Value *a, Value *b) { return createMaskedShl(a, b); };
        break;
      case AArch64::BICv4i16:
      case AArch64::BICv8i8:
      case AArch64::BICv2i32:
      case AArch64::BICv8i16:
      case AArch64::BICv4i32:
      case AArch64::BICv16i8:
        if (CurInst->getOperand(2).isImm()) {
          splatImm2 = true;
          immShift = true;
        }
        op = [&](Value *a, Value *b) { return createAnd(a, createNot(b)); };
        break;
      case AArch64::USHLv1i64:
      case AArch64::USHLv4i16:
      case AArch64::USHLv16i8:
      case AArch64::USHLv8i16:
      case AArch64::USHLv2i32:
      case AArch64::USHLv4i32:
      case AArch64::USHLv8i8:
      case AArch64::USHLv2i64:
        op = [&](Value *a, Value *b) { return createUSHL(a, b); };
        elementWise = true;
        break;
      case AArch64::SSHLv1i64:
      case AArch64::SSHLv4i16:
      case AArch64::SSHLv16i8:
      case AArch64::SSHLv8i16:
      case AArch64::SSHLv2i32:
      case AArch64::SSHLv4i32:
      case AArch64::SSHLv8i8:
      case AArch64::SSHLv2i64:
        op = [&](Value *a, Value *b) { return createSSHL(a, b); };
        elementWise = true;
        break;
      case AArch64::ADDv2i32:
      case AArch64::ADDv1i64:
      case AArch64::ADDv2i64:
      case AArch64::ADDv4i16:
      case AArch64::ADDv4i32:
      case AArch64::ADDv8i8:
      case AArch64::ADDv8i16:
      case AArch64::ADDv16i8:
        op = [&](Value *a, Value *b) { return createAdd(a, b); };
        break;
      case AArch64::UADDLv16i8_v8i16:
      case AArch64::UADDLv8i16_v4i32:
      case AArch64::UADDLv4i32_v2i64:
      case AArch64::UADDWv16i8_v8i16:
      case AArch64::UADDWv8i16_v4i32:
      case AArch64::UADDWv4i32_v2i64:
      case AArch64::SADDLv16i8_v8i16:
      case AArch64::SADDLv8i16_v4i32:
      case AArch64::SADDLv4i32_v2i64:
      case AArch64::SADDWv16i8_v8i16:
      case AArch64::SADDWv8i16_v4i32:
      case AArch64::SADDWv4i32_v2i64:
        // These cases are UADDL2, UADDW2, SADDL2, SADDW2
        isUpper = true;
      case AArch64::UADDLv8i8_v8i16:
      case AArch64::UADDLv4i16_v4i32:
      case AArch64::UADDLv2i32_v2i64:
      case AArch64::UADDWv8i8_v8i16:
      case AArch64::UADDWv4i16_v4i32:
      case AArch64::UADDWv2i32_v2i64:
      case AArch64::SADDLv8i8_v8i16:
      case AArch64::SADDLv4i16_v4i32:
      case AArch64::SADDLv2i32_v2i64:
      case AArch64::SADDWv8i8_v8i16:
      case AArch64::SADDWv4i16_v4i32:
      case AArch64::SADDWv2i32_v2i64:
        if (opcode == AArch64::SADDLv8i8_v8i16 ||
            opcode == AArch64::SADDLv16i8_v8i16 ||
            opcode == AArch64::SADDLv4i16_v4i32 ||
            opcode == AArch64::SADDLv8i16_v4i32 ||
            opcode == AArch64::SADDLv2i32_v2i64 ||
            opcode == AArch64::SADDLv4i32_v2i64 ||
            opcode == AArch64::SADDWv8i8_v8i16 ||
            opcode == AArch64::SADDWv16i8_v8i16 ||
            opcode == AArch64::SADDWv4i16_v4i32 ||
            opcode == AArch64::SADDWv8i16_v4i32 ||
            opcode == AArch64::SADDWv2i32_v2i64 ||
            opcode == AArch64::SADDWv4i32_v2i64) {
          ext = extKind::SExt;
        } else {
          ext = extKind::ZExt;
        }
        op = [&](Value *a, Value *b) { return createAdd(a, b); };
        break;
      case AArch64::USUBLv16i8_v8i16:
      case AArch64::USUBLv8i16_v4i32:
      case AArch64::USUBLv4i32_v2i64:
      case AArch64::USUBWv16i8_v8i16:
      case AArch64::USUBWv8i16_v4i32:
      case AArch64::USUBWv4i32_v2i64:
      case AArch64::SSUBLv16i8_v8i16:
      case AArch64::SSUBLv8i16_v4i32:
      case AArch64::SSUBLv4i32_v2i64:
      case AArch64::SSUBWv16i8_v8i16:
      case AArch64::SSUBWv8i16_v4i32:
      case AArch64::SSUBWv4i32_v2i64:
        // These three cases are USUBL2, SSUBL2, USUBW2 and SSUBW2
        isUpper = true;
      case AArch64::USUBLv8i8_v8i16:
      case AArch64::USUBLv4i16_v4i32:
      case AArch64::USUBLv2i32_v2i64:
      case AArch64::USUBWv8i8_v8i16:
      case AArch64::USUBWv4i16_v4i32:
      case AArch64::USUBWv2i32_v2i64:
      case AArch64::SSUBLv8i8_v8i16:
      case AArch64::SSUBLv4i16_v4i32:
      case AArch64::SSUBLv2i32_v2i64:
      case AArch64::SSUBWv8i8_v8i16:
      case AArch64::SSUBWv4i16_v4i32:
      case AArch64::SSUBWv2i32_v2i64:
        if (opcode == AArch64::SSUBLv8i8_v8i16 ||
            opcode == AArch64::SSUBLv16i8_v8i16 ||
            opcode == AArch64::SSUBLv4i16_v4i32 ||
            opcode == AArch64::SSUBLv8i16_v4i32 ||
            opcode == AArch64::SSUBLv2i32_v2i64 ||
            opcode == AArch64::SSUBLv4i32_v2i64 ||
            opcode == AArch64::SSUBWv8i8_v8i16 ||
            opcode == AArch64::SSUBWv16i8_v8i16 ||
            opcode == AArch64::SSUBWv4i16_v4i32 ||
            opcode == AArch64::SSUBWv8i16_v4i32 ||
            opcode == AArch64::SSUBWv2i32_v2i64 ||
            opcode == AArch64::SSUBWv4i32_v2i64) {
          ext = extKind::SExt;
        } else {
          ext = extKind::ZExt;
        }
        op = [&](Value *a, Value *b) { return createSub(a, b); };
        break;
      case AArch64::SUBv2i32:
      case AArch64::SUBv2i64:
      case AArch64::SUBv4i16:
      case AArch64::SUBv4i32:
      case AArch64::SUBv8i8:
      case AArch64::SUBv8i16:
      case AArch64::SUBv16i8:
        op = [&](Value *a, Value *b) { return createSub(a, b); };
        break;
      case AArch64::EORv8i8:
      case AArch64::EORv16i8:
        op = [&](Value *a, Value *b) { return createXor(a, b); };
        break;
      case AArch64::ANDv8i8:
      case AArch64::ANDv16i8:
        op = [&](Value *a, Value *b) { return createAnd(a, b); };
        break;
      case AArch64::ORNv8i8:
      case AArch64::ORNv16i8:
        op = [&](Value *a, Value *b) { return createOr(a, createNot(b)); };
        break;
      case AArch64::ORRv8i8:
      case AArch64::ORRv16i8:
      case AArch64::ORRv2i32:
      case AArch64::ORRv4i16:
      case AArch64::ORRv8i16:
      case AArch64::ORRv4i32:
        if (CurInst->getOperand(2).isImm()) {
          splatImm2 = true;
          immShift = true;
        }
        op = [&](Value *a, Value *b) { return createOr(a, b); };
        break;
      case AArch64::SSHLLv4i32_shift:
      case AArch64::SSHLLv8i16_shift:
      case AArch64::SSHLLv16i8_shift:
        // these three cases are SSHLL2
        a = createRawLShr(a, getIntConst(64, 128));
      case AArch64::SSHLLv8i8_shift:
      case AArch64::SSHLLv4i16_shift:
      case AArch64::SSHLLv2i32_shift:
        ext = extKind::SExt;
        splatImm2 = true;
        op = [&](Value *a, Value *b) { return createRawShl(a, b); };
        break;
      case AArch64::USHLLv4i32_shift:
      case AArch64::USHLLv8i16_shift:
      case AArch64::USHLLv16i8_shift:
        // these three cases are USHLL2
        a = createRawLShr(a, getIntConst(64, 128));
      case AArch64::USHLLv4i16_shift:
      case AArch64::USHLLv2i32_shift:
      case AArch64::USHLLv8i8_shift:
        ext = extKind::ZExt;
        splatImm2 = true;
        op = [&](Value *a, Value *b) { return createRawShl(a, b); };
        break;
      case AArch64::USHRv2i64_shift:
        splatImm2 = true;
        op = [&](Value *a, Value *b) { return createRawLShr(a, b); };
        break;
      default:
        assert(false && "missed a case");
      }

      int eltSize;
      int numElts;
      switch (opcode) {
      case AArch64::USHLv1i64:
      case AArch64::SSHLv1i64:
      case AArch64::ADDv1i64:
        numElts = 1;
        eltSize = 64;
        break;
      case AArch64::ORRv2i32:
      case AArch64::UMULLv2i32_v2i64:
      case AArch64::SMINv2i32:
      case AArch64::SMAXv2i32:
      case AArch64::UMINv2i32:
      case AArch64::UMAXv2i32:
      case AArch64::SMULLv2i32_v2i64:
      case AArch64::USHRv2i32_shift:
      case AArch64::MULv2i32:
      case AArch64::SSHLLv2i32_shift:
      case AArch64::SSHRv2i32_shift:
      case AArch64::SHLv2i32_shift:
      case AArch64::SUBv2i32:
      case AArch64::ADDv2i32:
      case AArch64::USHLv2i32:
      case AArch64::SSHLv2i32:
      case AArch64::BICv2i32:
      case AArch64::USHLLv2i32_shift:
      case AArch64::UADDLv2i32_v2i64:
      case AArch64::UADDWv2i32_v2i64:
      case AArch64::SADDLv2i32_v2i64:
      case AArch64::SADDWv2i32_v2i64:
      case AArch64::USUBLv2i32_v2i64:
      case AArch64::USUBWv2i32_v2i64:
      case AArch64::SSUBLv2i32_v2i64:
      case AArch64::SSUBWv2i32_v2i64:
        numElts = 2;
        eltSize = 32;
        break;
      case AArch64::SSHRv2i64_shift:
      case AArch64::SHLv2i64_shift:
      case AArch64::ADDv2i64:
      case AArch64::SUBv2i64:
      case AArch64::USHLv2i64:
      case AArch64::SSHLv2i64:
        numElts = 2;
        eltSize = 64;
        break;
      case AArch64::USHRv2i64_shift:
        numElts = 2;
        eltSize = 64;
        break;
      case AArch64::ORRv4i16:
      case AArch64::UMULLv4i16_v4i32:
      case AArch64::SMINv4i16:
      case AArch64::SMAXv4i16:
      case AArch64::UMINv4i16:
      case AArch64::UMAXv4i16:
      case AArch64::SMULLv4i16_v4i32:
      case AArch64::USHRv4i16_shift:
      case AArch64::SSHLLv4i16_shift:
      case AArch64::SSHRv4i16_shift:
      case AArch64::ADDv4i16:
      case AArch64::SUBv4i16:
      case AArch64::USHLv4i16:
      case AArch64::SSHLv4i16:
      case AArch64::BICv4i16:
      case AArch64::USHLLv4i16_shift:
      case AArch64::SHLv4i16_shift:
      case AArch64::MULv4i16:
      case AArch64::UADDLv4i16_v4i32:
      case AArch64::UADDWv4i16_v4i32:
      case AArch64::SADDLv4i16_v4i32:
      case AArch64::SADDWv4i16_v4i32:
      case AArch64::USUBLv4i16_v4i32:
      case AArch64::USUBWv4i16_v4i32:
      case AArch64::SSUBLv4i16_v4i32:
      case AArch64::SSUBWv4i16_v4i32:
        numElts = 4;
        eltSize = 16;
        break;
      case AArch64::UMULLv4i32_v2i64:
      case AArch64::SMINv4i32:
      case AArch64::SMAXv4i32:
      case AArch64::UMINv4i32:
      case AArch64::UMAXv4i32:
      case AArch64::SMULLv4i32_v2i64:
      case AArch64::USHRv4i32_shift:
      case AArch64::MULv4i32:
      case AArch64::SSHLLv4i32_shift:
      case AArch64::SSHRv4i32_shift:
      case AArch64::SHLv4i32_shift:
      case AArch64::ADDv4i32:
      case AArch64::SUBv4i32:
      case AArch64::USHLv4i32:
      case AArch64::SSHLv4i32:
      case AArch64::BICv4i32:
      case AArch64::USHLLv4i32_shift:
      case AArch64::ORRv4i32:
      case AArch64::UADDLv4i32_v2i64:
      case AArch64::UADDWv4i32_v2i64:
      case AArch64::SADDLv4i32_v2i64:
      case AArch64::SADDWv4i32_v2i64:
      case AArch64::USUBLv4i32_v2i64:
      case AArch64::USUBWv4i32_v2i64:
      case AArch64::SSUBLv4i32_v2i64:
      case AArch64::SSUBWv4i32_v2i64:
        numElts = 4;
        eltSize = 32;
        break;
      case AArch64::ORNv8i8:
      case AArch64::UMULLv8i8_v8i16:
      case AArch64::SMINv8i8:
      case AArch64::SMAXv8i8:
      case AArch64::UMINv8i8:
      case AArch64::UMAXv8i8:
      case AArch64::SMULLv8i8_v8i16:
      case AArch64::MULv8i8:
      case AArch64::SSHLLv8i8_shift:
      case AArch64::SSHRv8i8_shift:
      case AArch64::SHLv8i8_shift:
      case AArch64::ADDv8i8:
      case AArch64::SUBv8i8:
      case AArch64::EORv8i8:
      case AArch64::ANDv8i8:
      case AArch64::ORRv8i8:
      case AArch64::USHLv8i8:
      case AArch64::SSHLv8i8:
      case AArch64::BICv8i8:
      case AArch64::USHRv8i8_shift:
      case AArch64::USHLLv8i8_shift:
      case AArch64::UADDLv8i8_v8i16:
      case AArch64::UADDWv8i8_v8i16:
      case AArch64::SADDLv8i8_v8i16:
      case AArch64::SADDWv8i8_v8i16:
      case AArch64::USUBLv8i8_v8i16:
      case AArch64::USUBWv8i8_v8i16:
      case AArch64::SSUBLv8i8_v8i16:
      case AArch64::SSUBWv8i8_v8i16:
        numElts = 8;
        eltSize = 8;
        break;
      case AArch64::ORRv8i16:
      case AArch64::UMULLv8i16_v4i32:
      case AArch64::SMULLv8i16_v4i32:
      case AArch64::USHRv8i16_shift:
      case AArch64::MULv8i16:
      case AArch64::SSHLLv8i16_shift:
      case AArch64::ADDv8i16:
      case AArch64::SUBv8i16:
      case AArch64::USHLv8i16:
      case AArch64::SSHLv8i16:
      case AArch64::BICv8i16:
      case AArch64::USHLLv8i16_shift:
      case AArch64::SHLv8i16_shift:
      case AArch64::SSHRv8i16_shift:
      case AArch64::SMINv8i16:
      case AArch64::SMAXv8i16:
      case AArch64::UMINv8i16:
      case AArch64::UMAXv8i16:
      case AArch64::UADDLv8i16_v4i32:
      case AArch64::UADDWv8i16_v4i32:
      case AArch64::SADDLv8i16_v4i32:
      case AArch64::SADDWv8i16_v4i32:
      case AArch64::USUBLv8i16_v4i32:
      case AArch64::USUBWv8i16_v4i32:
      case AArch64::SSUBLv8i16_v4i32:
      case AArch64::SSUBWv8i16_v4i32:
        numElts = 8;
        eltSize = 16;
        break;
      case AArch64::ORNv16i8:
      case AArch64::UMULLv16i8_v8i16:
      case AArch64::SMINv16i8:
      case AArch64::SMAXv16i8:
      case AArch64::UMINv16i8:
      case AArch64::UMAXv16i8:
      case AArch64::SMULLv16i8_v8i16:
      case AArch64::USHRv16i8_shift:
      case AArch64::MULv16i8:
      case AArch64::SSHLLv16i8_shift:
      case AArch64::ADDv16i8:
      case AArch64::SUBv16i8:
      case AArch64::EORv16i8:
      case AArch64::ANDv16i8:
      case AArch64::ORRv16i8:
      case AArch64::USHLv16i8:
      case AArch64::SSHLv16i8:
      case AArch64::BICv16i8:
      case AArch64::USHLLv16i8_shift:
      case AArch64::SHLv16i8_shift:
      case AArch64::SSHRv16i8_shift:
      case AArch64::UADDLv16i8_v8i16:
      case AArch64::UADDWv16i8_v8i16:
      case AArch64::SADDLv16i8_v8i16:
      case AArch64::SADDWv16i8_v8i16:
      case AArch64::USUBLv16i8_v8i16:
      case AArch64::USUBWv16i8_v8i16:
      case AArch64::SSUBLv16i8_v8i16:
      case AArch64::SSUBWv16i8_v8i16:
        numElts = 16;
        eltSize = 8;
        break;
      default:
        assert(false && "missed case");
        break;
      }

      // Some instructions have first operand of a different type than the
      // second operand (Twice the eltSize. Also half the number of elements if
      // isUpper)
      bool operandTypesDiffer = false;
      switch (opcode) {
      case AArch64::UADDWv8i8_v8i16:
      case AArch64::UADDWv16i8_v8i16:
      case AArch64::UADDWv4i16_v4i32:
      case AArch64::UADDWv8i16_v4i32:
      case AArch64::UADDWv2i32_v2i64:
      case AArch64::UADDWv4i32_v2i64:
      case AArch64::SADDWv8i8_v8i16:
      case AArch64::SADDWv16i8_v8i16:
      case AArch64::SADDWv4i16_v4i32:
      case AArch64::SADDWv8i16_v4i32:
      case AArch64::SADDWv2i32_v2i64:
      case AArch64::SADDWv4i32_v2i64:
      case AArch64::USUBWv8i8_v8i16:
      case AArch64::USUBWv16i8_v8i16:
      case AArch64::USUBWv4i16_v4i32:
      case AArch64::USUBWv8i16_v4i32:
      case AArch64::USUBWv2i32_v2i64:
      case AArch64::USUBWv4i32_v2i64:
      case AArch64::SSUBWv8i8_v8i16:
      case AArch64::SSUBWv16i8_v8i16:
      case AArch64::SSUBWv4i16_v4i32:
      case AArch64::SSUBWv8i16_v4i32:
      case AArch64::SSUBWv2i32_v2i64:
      case AArch64::SSUBWv4i32_v2i64:
        operandTypesDiffer = true;
        break;
      }

      auto res =
          createVectorOp(op, a, b, eltSize, numElts, elementWise, ext,
                         splatImm2, immShift, isUpper, operandTypesDiffer);
      updateOutputReg(res);
      break;
    }

    case AArch64::ADDPv2i64p: {
      auto vTy = getVecTy(64, 2);
      auto v = createBitCast(readFromOperand(1), vTy);
      auto res =
          createAdd(createExtractElement(v, 0), createExtractElement(v, 1));
      updateOutputReg(res);
      break;
    }

    case AArch64::UZP1v8i8:
    case AArch64::UZP1v4i16:
    case AArch64::UZP1v16i8:
    case AArch64::UZP1v8i16:
    case AArch64::UZP1v4i32:
    case AArch64::UZP2v8i8:
    case AArch64::UZP2v4i16:
    case AArch64::UZP2v8i16:
    case AArch64::UZP2v16i8:
    case AArch64::UZP2v4i32: {
      int which;
      switch (opcode) {
      case AArch64::UZP1v8i8:
      case AArch64::UZP1v4i16:
      case AArch64::UZP1v16i8:
      case AArch64::UZP1v8i16:
      case AArch64::UZP1v4i32:
        which = 0;
        break;
      case AArch64::UZP2v8i8:
      case AArch64::UZP2v4i16:
      case AArch64::UZP2v8i16:
      case AArch64::UZP2v16i8:
      case AArch64::UZP2v4i32:
        which = 1;
        break;
      default:
        assert(false);
      }
      int numElts, eltSize;
      if (opcode == AArch64::UZP1v8i8 || opcode == AArch64::UZP2v8i8) {
        numElts = 8;
        eltSize = 8;
      } else if (opcode == AArch64::UZP1v4i16 || opcode == AArch64::UZP2v4i16) {
        numElts = 4;
        eltSize = 16;
      } else if (opcode == AArch64::UZP1v16i8 || opcode == AArch64::UZP2v16i8) {
        numElts = 16;
        eltSize = 8;
      } else if (opcode == AArch64::UZP1v8i16 || opcode == AArch64::UZP2v8i16) {
        numElts = 8;
        eltSize = 16;
      } else if (opcode == AArch64::UZP1v4i32 || opcode == AArch64::UZP2v4i32) {
        numElts = 4;
        eltSize = 32;
      } else {
        assert(false);
      }
      auto vTy = getVecTy(eltSize, numElts);
      auto a = createBitCast(readFromOperand(1), vTy);
      auto b = createBitCast(readFromOperand(2), vTy);
      Value *res = getUndefVec(numElts, eltSize);
      for (int i = 0; i < numElts / 2; ++i) {
        auto e1 = createExtractElement(a, (i * 2) + which);
        auto e2 = createExtractElement(b, (i * 2) + which);
        res = createInsertElement(res, e1, i);
        res = createInsertElement(res, e2, i + (numElts / 2));
      }
      updateOutputReg(res);
      break;
    }

#define GET_SIZES4(INSN, SUFF)                                                 \
  if (opcode == AArch64::INSN##v2i32##SUFF) {                                  \
    numElts = 2;                                                               \
    eltSize = 32;                                                              \
  } else if (opcode == AArch64::INSN##v4i16##SUFF) {                           \
    numElts = 4;                                                               \
    eltSize = 16;                                                              \
  } else if (opcode == AArch64::INSN##v4i32##SUFF) {                           \
    numElts = 4;                                                               \
    eltSize = 32;                                                              \
  } else if (opcode == AArch64::INSN##v8i16##SUFF) {                           \
    numElts = 8;                                                               \
    eltSize = 16;                                                              \
  } else {                                                                     \
    assert(false);                                                             \
  }

    case AArch64::MULv8i16_indexed:
    case AArch64::MULv4i32_indexed:
    case AArch64::MULv2i32_indexed:
    case AArch64::MULv4i16_indexed: {
      unsigned eltSize, numElts;
      GET_SIZES4(MUL, _indexed);
      auto a = readFromVecOperand(1, eltSize, numElts);
      auto e2 = getIndexedElement(getImm(3), eltSize,
                                  CurInst->getOperand(2).getReg());
      Value *res = getUndefVec(numElts, eltSize);
      for (unsigned i = 0; i < numElts; ++i) {
        auto e1 = createExtractElement(a, i);
        res = createInsertElement(res, createMul(e1, e2), i);
      }
      updateOutputReg(res);
      break;
    }

    case AArch64::MLSv2i32_indexed:
    case AArch64::MLSv4i16_indexed:
    case AArch64::MLSv8i16_indexed:
    case AArch64::MLSv4i32_indexed: {
      int numElts, eltSize;
      GET_SIZES4(MLS, _indexed);
      auto vTy = getVecTy(eltSize, numElts);
      auto a = createBitCast(readFromOperand(1), vTy);
      auto b = createBitCast(readFromOperand(2), vTy);
      auto e = getIndexedElement(getImm(4), eltSize,
                                 CurInst->getOperand(3).getReg());
      auto spl = splat(e, numElts, eltSize);
      auto mul = createMul(b, spl);
      auto sum = createSub(a, mul);
      updateOutputReg(sum);
      break;
    }

    case AArch64::MLAv2i32_indexed:
    case AArch64::MLAv4i16_indexed:
    case AArch64::MLAv8i16_indexed:
    case AArch64::MLAv4i32_indexed: {
      int numElts, eltSize;
      GET_SIZES4(MLA, _indexed);
      auto vTy = getVecTy(eltSize, numElts);
      auto a = createBitCast(readFromOperand(1), vTy);
      auto b = createBitCast(readFromOperand(2), vTy);
      auto e = getIndexedElement(getImm(4), eltSize,
                                 CurInst->getOperand(3).getReg());
      auto spl = splat(e, numElts, eltSize);
      auto mul = createMul(b, spl);
      auto sum = createAdd(mul, a);
      updateOutputReg(sum);
      break;
    }

#define GET_SIZES5(INSN, SUFF)                                                 \
  if (opcode == AArch64::INSN##v8i8##SUFF) {                                   \
    numElts = 8;                                                               \
    eltSize = 8;                                                               \
  } else if (opcode == AArch64::INSN##v4i16##SUFF) {                           \
    numElts = 4;                                                               \
    eltSize = 16;                                                              \
  } else if (opcode == AArch64::INSN##v16i8##SUFF) {                           \
    numElts = 16;                                                              \
    eltSize = 8;                                                               \
  } else if (opcode == AArch64::INSN##v4i32##SUFF) {                           \
    numElts = 4;                                                               \
    eltSize = 32;                                                              \
  } else if (opcode == AArch64::INSN##v8i16##SUFF) {                           \
    numElts = 8;                                                               \
    eltSize = 16;                                                              \
  }

    case AArch64::TRN1v16i8:
    case AArch64::TRN1v8i16:
    case AArch64::TRN1v4i32:
    case AArch64::TRN1v4i16:
    case AArch64::TRN1v8i8:
    case AArch64::TRN2v8i8:
    case AArch64::TRN2v4i16:
    case AArch64::TRN2v16i8:
    case AArch64::TRN2v8i16:
    case AArch64::TRN2v4i32: {
      int numElts = -1, eltSize = -1;
      GET_SIZES5(TRN1, );
      GET_SIZES5(TRN2, );
      assert(numElts != -1 && eltSize != -1);
      int part;
      switch (opcode) {
      case AArch64::TRN1v16i8:
      case AArch64::TRN1v8i16:
      case AArch64::TRN1v4i32:
      case AArch64::TRN1v4i16:
      case AArch64::TRN1v8i8:
        part = 0;
        break;
      case AArch64::TRN2v8i8:
      case AArch64::TRN2v4i16:
      case AArch64::TRN2v16i8:
      case AArch64::TRN2v8i16:
      case AArch64::TRN2v4i32:
        part = 1;
        break;
      default:
        assert(false);
      }
      auto a = readFromVecOperand(1, eltSize, numElts);
      auto b = readFromVecOperand(2, eltSize, numElts);
      Value *res = getUndefVec(numElts, eltSize);
      for (int p = 0; p < numElts / 2; ++p) {
        auto *e1 = createExtractElement(a, (2 * p) + part);
        auto *e2 = createExtractElement(b, (2 * p) + part);
        res = createInsertElement(res, e1, 2 * p);
        res = createInsertElement(res, e2, (2 * p) + 1);
      }
      updateOutputReg(res);
      break;
    }

    case AArch64::SMINVv8i8v:
    case AArch64::UMINVv8i8v:
    case AArch64::SMAXVv8i8v:
    case AArch64::UMAXVv8i8v:
    case AArch64::SMINVv4i16v:
    case AArch64::UMINVv4i16v:
    case AArch64::SMAXVv4i16v:
    case AArch64::UMAXVv4i16v:
    case AArch64::SMAXVv4i32v:
    case AArch64::UMAXVv4i32v:
    case AArch64::SMINVv4i32v:
    case AArch64::UMINVv4i32v:
    case AArch64::UMINVv8i16v:
    case AArch64::SMINVv8i16v:
    case AArch64::UMAXVv8i16v:
    case AArch64::SMAXVv8i16v:
    case AArch64::SMINVv16i8v:
    case AArch64::UMINVv16i8v:
    case AArch64::SMAXVv16i8v:
    case AArch64::UMAXVv16i8v: {
      int numElts = -1, eltSize = -1;
      GET_SIZES5(SMINV, v);
      GET_SIZES5(SMAXV, v);
      GET_SIZES5(UMINV, v);
      GET_SIZES5(UMAXV, v);
      assert(numElts != -1 && eltSize != -1);
      auto v = readFromVecOperand(1, eltSize, numElts);
      Value *min = createExtractElement(v, 0);
      for (int i = 1; i < numElts; ++i) {
        auto e = createExtractElement(v, i);
        auto p{ICmpInst::Predicate::BAD_ICMP_PREDICATE};
        switch (opcode) {
        case AArch64::SMINVv8i8v:
        case AArch64::SMINVv4i16v:
        case AArch64::SMINVv8i16v:
        case AArch64::SMINVv16i8v:
        case AArch64::SMINVv4i32v:
          p = ICmpInst::Predicate::ICMP_SLT;
          break;
        case AArch64::UMINVv8i8v:
        case AArch64::UMINVv4i16v:
        case AArch64::UMINVv4i32v:
        case AArch64::UMINVv8i16v:
        case AArch64::UMINVv16i8v:
          p = ICmpInst::Predicate::ICMP_ULT;
          break;
        case AArch64::SMAXVv8i8v:
        case AArch64::SMAXVv4i16v:
        case AArch64::SMAXVv8i16v:
        case AArch64::SMAXVv16i8v:
        case AArch64::SMAXVv4i32v:
          p = ICmpInst::Predicate::ICMP_SGT;
          break;
        case AArch64::UMAXVv4i16v:
        case AArch64::UMAXVv4i32v:
        case AArch64::UMAXVv8i16v:
        case AArch64::UMAXVv8i8v:
        case AArch64::UMAXVv16i8v:
          p = ICmpInst::Predicate::ICMP_UGT;
          break;
        default:
          assert(false);
        }
        auto c = createICmp(p, min, e);
        min = createSelect(c, min, e);
      }
      updateOutputReg(min);
      break;
    }

#define GET_SIZES6(INSN, SUFF)                                                 \
  if (opcode == AArch64::INSN##v8i8##SUFF) {                                   \
    numElts = 8;                                                               \
    eltSize = 8;                                                               \
  } else if (opcode == AArch64::INSN##v4i16##SUFF) {                           \
    numElts = 4;                                                               \
    eltSize = 16;                                                              \
  } else if (opcode == AArch64::INSN##v2i32##SUFF) {                           \
    numElts = 2;                                                               \
    eltSize = 32;                                                              \
  } else if (opcode == AArch64::INSN##v16i8##SUFF) {                           \
    numElts = 16;                                                              \
    eltSize = 8;                                                               \
  } else if (opcode == AArch64::INSN##v4i32##SUFF) {                           \
    numElts = 4;                                                               \
    eltSize = 32;                                                              \
  } else if (opcode == AArch64::INSN##v8i16##SUFF) {                           \
    numElts = 8;                                                               \
    eltSize = 16;                                                              \
  }

    case AArch64::MLSv8i8:
    case AArch64::MLSv2i32:
    case AArch64::MLSv4i16:
    case AArch64::MLSv16i8:
    case AArch64::MLSv8i16:
    case AArch64::MLSv4i32: {
      unsigned numElts = -1, eltSize = -1;
      GET_SIZES6(MLS, );
      auto a = readFromVecOperand(1, eltSize, numElts);
      auto b = readFromVecOperand(2, eltSize, numElts);
      auto c = readFromVecOperand(3, eltSize, numElts);
      auto mul = createMul(b, c);
      auto sum = createSub(a, mul);
      updateOutputReg(sum);
      break;
    }

    case AArch64::MLAv8i8:
    case AArch64::MLAv2i32:
    case AArch64::MLAv4i16:
    case AArch64::MLAv16i8:
    case AArch64::MLAv8i16:
    case AArch64::MLAv4i32: {
      unsigned numElts = -1, eltSize = -1;
      GET_SIZES6(MLA, );
      auto a = readFromVecOperand(1, eltSize, numElts);
      auto b = readFromVecOperand(2, eltSize, numElts);
      auto c = readFromVecOperand(3, eltSize, numElts);
      auto mul = createMul(b, c);
      auto sum = createAdd(mul, a);
      updateOutputReg(sum);
      break;
    }

    case AArch64::SHRNv2i32_shift:
    case AArch64::SHRNv4i16_shift:
    case AArch64::SHRNv8i8_shift:
    case AArch64::SHRNv4i32_shift:
    case AArch64::SHRNv8i16_shift:
    case AArch64::SHRNv16i8_shift: {
      unsigned numElts, eltSize;
      GET_SIZES6(SHRN, _shift);
      bool topHalf;
      switch (opcode) {
      case AArch64::SHRNv2i32_shift:
      case AArch64::SHRNv4i16_shift:
      case AArch64::SHRNv8i8_shift:
        topHalf = false;
        break;
      case AArch64::SHRNv4i32_shift:
      case AArch64::SHRNv8i16_shift:
      case AArch64::SHRNv16i8_shift:
        topHalf = true;
        break;
      default:
        assert(false);
      }
      Value *op, *res;
      int exp;
      if (topHalf) {
        res = readFromVecOperand(1, eltSize, numElts);
        op = readFromOperand(2);
        exp = getImm(3);
        numElts /= 2;
      } else {
        op = readFromOperand(1);
        exp = getImm(2);
        res = getZeroVec(numElts, eltSize);
      }
      auto vTy = getVecTy(2 * eltSize, numElts);
      auto a = createBitCast(op, vTy);
      for (unsigned i = 0; i < numElts; ++i) {
        auto e = createExtractElement(a, i);
        auto shift = createMaskedLShr(e, getIntConst(exp, 2 * eltSize));
        auto trunc = createTrunc(shift, getIntTy(eltSize));
        int pos = topHalf ? (i + numElts) : i;
        res = createInsertElement(res, trunc, pos);
      }
      updateOutputReg(res);
      break;
    }

    case AArch64::UABDLv8i8_v8i16:
    case AArch64::UABDLv16i8_v8i16:
    case AArch64::UABDLv4i16_v4i32:
    case AArch64::UABDLv8i16_v4i32:
    case AArch64::UABDLv2i32_v2i64:
    case AArch64::UABDLv4i32_v2i64:
    case AArch64::SABDLv8i8_v8i16:
    case AArch64::SABDLv16i8_v8i16:
    case AArch64::SABDLv4i16_v4i32:
    case AArch64::SABDLv8i16_v4i32:
    case AArch64::SABDLv2i32_v2i64:
    case AArch64::SABDLv4i32_v2i64: {
      unsigned numElts, eltSize;
      bool isUpper = false;
      switch (opcode) {
      case AArch64::UABDLv8i8_v8i16:
      case AArch64::SABDLv8i8_v8i16:
        numElts = 8;
        eltSize = 8;
        break;
      case AArch64::UABDLv16i8_v8i16:
      case AArch64::SABDLv16i8_v8i16:
        numElts = 16;
        eltSize = 8;
        isUpper = true;
        break;
      case AArch64::UABDLv4i16_v4i32:
      case AArch64::SABDLv4i16_v4i32:
        numElts = 4;
        eltSize = 16;
        break;
      case AArch64::UABDLv8i16_v4i32:
      case AArch64::SABDLv8i16_v4i32:
        numElts = 8;
        eltSize = 16;
        isUpper = true;
        break;
      case AArch64::UABDLv2i32_v2i64:
      case AArch64::SABDLv2i32_v2i64:
        numElts = 2;
        eltSize = 32;
        break;
      case AArch64::UABDLv4i32_v2i64:
      case AArch64::SABDLv4i32_v2i64:
        numElts = 4;
        eltSize = 32;
        isUpper = true;
        break;
      default:
        assert(false);
      }

      bool isSigned = false;
      switch (opcode) {
      case AArch64::SABDLv8i8_v8i16:
      case AArch64::SABDLv16i8_v8i16:
      case AArch64::SABDLv4i16_v4i32:
      case AArch64::SABDLv8i16_v4i32:
      case AArch64::SABDLv2i32_v2i64:
      case AArch64::SABDLv4i32_v2i64:
        isSigned = true;
        break;
      }

      auto a = readFromVecOperand(1, eltSize, numElts, isUpper);
      auto b = readFromVecOperand(2, eltSize, numElts, isUpper);

      Value *extended_a, *extended_b;

      if (isSigned) {
        extended_a = createSExt(
            a, getVecTy(2 * eltSize, isUpper ? numElts / 2 : numElts));
        extended_b = createSExt(
            b, getVecTy(2 * eltSize, isUpper ? numElts / 2 : numElts));
      } else {
        extended_a = createZExt(
            a, getVecTy(2 * eltSize, isUpper ? numElts / 2 : numElts));
        extended_b = createZExt(
            b, getVecTy(2 * eltSize, isUpper ? numElts / 2 : numElts));
      }

      auto difference = createSub(extended_a, extended_b);
      auto abs = createAbs(difference);

      updateOutputReg(abs);
      break;
    }

      /*
      case AArch64::UMLALv4i16_indexed:
      case AArch64::UMLALv8i16_indexed:
      case AArch64::UMLALv2i32_indexed:
      case AArch64::UMLALv4i32_indexed:
      case AArch64::UMLSLv4i16_indexed:
      case AArch64::UMLSLv8i16_indexed:
      case AArch64::UMLSLv2i32_indexed:
      case AArch64::UMLSLv4i32_indexed:
      case AArch64::SMLALv4i16_indexed:
      case AArch64::SMLALv8i16_indexed:
      case AArch64::SMLALv2i32_indexed:
      case AArch64::SMLALv4i32_indexed:
      case AArch64::SMLSLv4i16_indexed:
      case AArch64::SMLSLv8i16_indexed:
      case AArch64::SMLSLv2i32_indexed:
      case AArch64::SMLSLv4i32_indexed: {
        unsigned numElts, eltSize;
        GET_SIZES4(SMLAL, _indexed);
        auto isUpper = opcode == AArch64::UMLALv8i16_indexed ||
                       opcode == AArch64::UMLSLv8i16_indexed ||
                       opcode == AArch64::SMLALv8i16_indexed ||
                       opcode == AArch64::SMLSLv8i16_indexed ||
                       opcode == AArch64::UMLALv4i32_indexed ||
                       opcode == AArch64::UMLSLv4i32_indexed ||
                       opcode == AArch64::SMLALv4i32_indexed ||
                       opcode == AArch64::SMLSLv4i32_indexed;
        auto isSigned = opcode == AArch64::SMLALv4i16_indexed ||
                        opcode == AArch64::SMLSLv4i16_indexed ||
                        opcode == AArch64::SMLALv8i16_indexed ||
                        opcode == AArch64::SMLSLv8i16_indexed ||
                        opcode == AArch64::SMLALv2i32_indexed ||
                        opcode == AArch64::SMLSLv2i32_indexed ||
                        opcode == AArch64::SMLALv4i32_indexed ||
                        opcode == AArch64::SMLSLv4i32_indexed;
        auto isSub = opcode == AArch64::UMLSLv4i16_indexed ||
                     opcode == AArch64::UMLSLv8i16_indexed ||
                     opcode == AArch64::UMLSLv2i32_indexed ||
                     opcode == AArch64::UMLSLv4i32_indexed ||
                     opcode == AArch64::SMLSLv4i16_indexed ||
                     opcode == AArch64::SMLSLv8i16_indexed ||
                     opcode == AArch64::SMLSLv2i32_indexed ||
                     opcode == AArch64::SMLSLv4i32_indexed;
        assert(isSIMDandFPRegOperand(CurInst->getOperand(0)) &&
               isSIMDandFPRegOperand(CurInst->getOperand(1)) &&
               CurInst->getOperand(0).getReg() ==
                   CurInst->getOperand(1).getReg());
        assert(CurInst->getOperand(4).isImm());

        auto destReg =
            readFromVecOperand(1, 2 * eltSize, isUpper ? numElts / 2 : numElts);
        auto a = readFromVecOperand(2, eltSize, numElts, isUpper);
        auto element = getIndexedElement(getImm(4), eltSize,
                                         CurInst->getOperand(3).getReg());
        auto splatElement = splat(element, numElts, eltSize);

        auto extended_a =
            isSigned ? createSExt(a, getVecTy(2 * eltSize,
                                              isUpper ? numElts / 2 : numElts))
                     : createZExt(a, getVecTy(2 * eltSize,
                                              isUpper ? numElts / 2 : numElts));
        auto extended_b =
            isSigned ? createSExt(
                           splatElement,
                           getVecTy(2 * eltSize, isUpper ? numElts / 2 :
      numElts)) : createZExt(splatElement, getVecTy(2 * eltSize, isUpper ?
      numElts / 2 : numElts));

        auto mul = createMul(extended_a, extended_b);
        auto sum = isSub ? createSub(destReg, mul) : createAdd(destReg, mul);

        updateOutputReg(sum);
        break;
      }
      */

    case AArch64::UMLALv8i8_v8i16:
    case AArch64::UMLALv16i8_v8i16:
    case AArch64::UMLALv4i16_v4i32:
    case AArch64::UMLALv8i16_v4i32:
    case AArch64::UMLALv2i32_v2i64:
    case AArch64::UMLALv4i32_v2i64:
    case AArch64::UMLSLv8i8_v8i16:
    case AArch64::UMLSLv16i8_v8i16:
    case AArch64::UMLSLv4i16_v4i32:
    case AArch64::UMLSLv8i16_v4i32:
    case AArch64::UMLSLv2i32_v2i64:
    case AArch64::UMLSLv4i32_v2i64:
    case AArch64::SMLALv8i8_v8i16:
    case AArch64::SMLALv16i8_v8i16:
    case AArch64::SMLALv4i16_v4i32:
    case AArch64::SMLALv8i16_v4i32:
    case AArch64::SMLALv2i32_v2i64:
    case AArch64::SMLALv4i32_v2i64:
    case AArch64::SMLSLv8i8_v8i16:
    case AArch64::SMLSLv16i8_v8i16:
    case AArch64::SMLSLv4i16_v4i32:
    case AArch64::SMLSLv8i16_v4i32:
    case AArch64::SMLSLv2i32_v2i64:
    case AArch64::SMLSLv4i32_v2i64: {
      unsigned numElts, eltSize;
      bool isUpper = false;
      switch (opcode) {
      case AArch64::UMLALv8i8_v8i16:
      case AArch64::UMLSLv8i8_v8i16:
      case AArch64::SMLALv8i8_v8i16:
      case AArch64::SMLSLv8i8_v8i16:
        numElts = 8;
        eltSize = 8;
        break;
      case AArch64::UMLALv16i8_v8i16:
      case AArch64::UMLSLv16i8_v8i16:
      case AArch64::SMLALv16i8_v8i16:
      case AArch64::SMLSLv16i8_v8i16:
        numElts = 16;
        eltSize = 8;
        isUpper = true;
        break;
      case AArch64::UMLALv4i16_v4i32:
      case AArch64::UMLSLv4i16_v4i32:
      case AArch64::SMLALv4i16_v4i32:
      case AArch64::SMLSLv4i16_v4i32:
        numElts = 4;
        eltSize = 16;
        break;
      case AArch64::UMLALv8i16_v4i32:
      case AArch64::UMLSLv8i16_v4i32:
      case AArch64::SMLALv8i16_v4i32:
      case AArch64::SMLSLv8i16_v4i32:
        numElts = 8;
        eltSize = 16;
        isUpper = true;
        break;
      case AArch64::UMLALv2i32_v2i64:
      case AArch64::UMLSLv2i32_v2i64:
      case AArch64::SMLALv2i32_v2i64:
      case AArch64::SMLSLv2i32_v2i64:
        numElts = 2;
        eltSize = 32;
        break;
      case AArch64::UMLALv4i32_v2i64:
      case AArch64::UMLSLv4i32_v2i64:
      case AArch64::SMLALv4i32_v2i64:
      case AArch64::SMLSLv4i32_v2i64:
        numElts = 4;
        eltSize = 32;
        isUpper = true;
        break;
      default:
        assert(false);
      }
      auto isSigned = opcode == AArch64::SMLALv8i8_v8i16 ||
                      opcode == AArch64::SMLSLv8i8_v8i16 ||
                      opcode == AArch64::SMLALv16i8_v8i16 ||
                      opcode == AArch64::SMLSLv16i8_v8i16 ||
                      opcode == AArch64::SMLALv4i16_v4i32 ||
                      opcode == AArch64::SMLSLv4i16_v4i32 ||
                      opcode == AArch64::SMLALv8i16_v4i32 ||
                      opcode == AArch64::SMLSLv8i16_v4i32 ||
                      opcode == AArch64::SMLALv2i32_v2i64 ||
                      opcode == AArch64::SMLSLv2i32_v2i64 ||
                      opcode == AArch64::SMLALv4i32_v2i64 ||
                      opcode == AArch64::SMLSLv4i32_v2i64;
      auto isSub = opcode == AArch64::UMLSLv8i8_v8i16 ||
                   opcode == AArch64::UMLSLv16i8_v8i16 ||
                   opcode == AArch64::UMLSLv4i16_v4i32 ||
                   opcode == AArch64::UMLSLv8i16_v4i32 ||
                   opcode == AArch64::UMLSLv2i32_v2i64 ||
                   opcode == AArch64::UMLSLv4i32_v2i64 ||
                   opcode == AArch64::SMLSLv8i8_v8i16 ||
                   opcode == AArch64::SMLSLv16i8_v8i16 ||
                   opcode == AArch64::SMLSLv4i16_v4i32 ||
                   opcode == AArch64::SMLSLv8i16_v4i32 ||
                   opcode == AArch64::SMLSLv2i32_v2i64 ||
                   opcode == AArch64::SMLSLv4i32_v2i64;
      assert(isSIMDandFPRegOperand(CurInst->getOperand(0)) &&
             isSIMDandFPRegOperand(CurInst->getOperand(1)) &&
             CurInst->getOperand(0).getReg() ==
                 CurInst->getOperand(1).getReg());
      auto destReg =
          readFromVecOperand(1, 2 * eltSize, isUpper ? numElts / 2 : numElts);
      auto a = readFromVecOperand(2, eltSize, numElts, isUpper);
      auto b = readFromVecOperand(3, eltSize, numElts, isUpper);

      auto extended_a =
          isSigned ? createSExt(a, getVecTy(2 * eltSize,
                                            isUpper ? numElts / 2 : numElts))
                   : createZExt(a, getVecTy(2 * eltSize,
                                            isUpper ? numElts / 2 : numElts));
      auto extended_b =
          isSigned ? createSExt(b, getVecTy(2 * eltSize,
                                            isUpper ? numElts / 2 : numElts))
                   : createZExt(b, getVecTy(2 * eltSize,
                                            isUpper ? numElts / 2 : numElts));

      auto mul = createMul(extended_a, extended_b);
      auto res = isSub ? createSub(destReg, mul) : createAdd(destReg, mul);

      updateOutputReg(res);
      break;
    }

    case AArch64::UABDv8i8:
    case AArch64::UABDv16i8:
    case AArch64::UABDv4i16:
    case AArch64::UABDv8i16:
    case AArch64::UABDv2i32:
    case AArch64::UABDv4i32:
    case AArch64::SABDv8i8:
    case AArch64::SABDv16i8:
    case AArch64::SABDv4i16:
    case AArch64::SABDv8i16:
    case AArch64::SABDv2i32:
    case AArch64::SABDv4i32: {
      unsigned numElts = -1, eltSize = -1;
      GET_SIZES6(SABD, );
      GET_SIZES6(UABD, );

      bool isSigned = false;
      switch (opcode) {
      case AArch64::SABDv8i8:
      case AArch64::SABDv16i8:
      case AArch64::SABDv4i16:
      case AArch64::SABDv8i16:
      case AArch64::SABDv2i32:
      case AArch64::SABDv4i32: {
        isSigned = true;
        break;
      }
      }

      auto a = readFromVecOperand(1, eltSize, numElts);
      auto b = readFromVecOperand(2, eltSize, numElts);

      Value *extended_a;
      Value *extended_b;

      if (isSigned) {
        extended_a = createSExt(a, getVecTy(2 * eltSize, numElts));
        extended_b = createSExt(b, getVecTy(2 * eltSize, numElts));
      } else {
        extended_a = createZExt(a, getVecTy(2 * eltSize, numElts));
        extended_b = createZExt(b, getVecTy(2 * eltSize, numElts));
      }

      auto difference = createSub(extended_a, extended_b);
      auto abs = createAbs(difference);
      auto truncated = createTrunc(abs, getVecTy(eltSize, numElts));

      updateOutputReg(truncated);
      break;
    }

#define GET_SIZES7(INSN, SUFF)                                                 \
  unsigned numElts, eltSize;                                                   \
  if (opcode == AArch64::INSN##v8i8##SUFF) {                                   \
    numElts = 8;                                                               \
    eltSize = 8;                                                               \
  } else if (opcode == AArch64::INSN##v4i16##SUFF) {                           \
    numElts = 4;                                                               \
    eltSize = 16;                                                              \
  } else if (opcode == AArch64::INSN##v2i32##SUFF) {                           \
    numElts = 2;                                                               \
    eltSize = 32;                                                              \
  } else if (opcode == AArch64::INSN##v16i8##SUFF) {                           \
    numElts = 16;                                                              \
    eltSize = 8;                                                               \
  } else if (opcode == AArch64::INSN##v4i32##SUFF) {                           \
    numElts = 4;                                                               \
    eltSize = 32;                                                              \
  } else if (opcode == AArch64::INSN##v8i16##SUFF) {                           \
    numElts = 8;                                                               \
    eltSize = 16;                                                              \
  } else if (opcode == AArch64::INSN##v2i64##SUFF) {                           \
    numElts = 2;                                                               \
    eltSize = 64;                                                              \
  } else {                                                                     \
    assert(false);                                                             \
  }

    case AArch64::USRAv8i8_shift:
    case AArch64::USRAv4i16_shift:
    case AArch64::USRAv2i32_shift:
    case AArch64::USRAv16i8_shift:
    case AArch64::USRAv8i16_shift:
    case AArch64::USRAv2i64_shift:
    case AArch64::USRAv4i32_shift: {
      GET_SIZES7(USRA, _shift);
      auto a = readFromVecOperand(1, eltSize, numElts);
      auto b = readFromVecOperand(2, eltSize, numElts);
      auto exp = getImm(3);
      Value *res = getUndefVec(numElts, eltSize);
      for (unsigned i = 0; i < numElts; ++i) {
        auto e1 = createExtractElement(a, i);
        auto e2 = createExtractElement(b, i);
        auto shift = createMaskedLShr(e2, getIntConst(exp, eltSize));
        auto sum = createAdd(e1, shift);
        res = createInsertElement(res, sum, i);
      }
      updateOutputReg(res);
      break;
    }

    case AArch64::ZIP1v4i16:
    case AArch64::ZIP1v2i32:
    case AArch64::ZIP1v8i8:
    case AArch64::ZIP1v8i16:
    case AArch64::ZIP1v16i8:
    case AArch64::ZIP1v2i64:
    case AArch64::ZIP1v4i32: {
      GET_SIZES7(ZIP1, );
      auto a = readFromVecOperand(1, eltSize, numElts);
      auto b = readFromVecOperand(2, eltSize, numElts);
      Value *res = getUndefVec(numElts, eltSize);
      for (unsigned i = 0; i < numElts / 2; ++i) {
        auto e1 = createExtractElement(a, i);
        auto e2 = createExtractElement(b, i);
        res = createInsertElement(res, e1, 2 * i);
        res = createInsertElement(res, e2, (2 * i) + 1);
      }
      updateOutputReg(res);
      break;
    }

    case AArch64::ZIP2v4i16:
    case AArch64::ZIP2v2i32:
    case AArch64::ZIP2v8i8:
    case AArch64::ZIP2v8i16:
    case AArch64::ZIP2v2i64:
    case AArch64::ZIP2v16i8:
    case AArch64::ZIP2v4i32: {
      GET_SIZES7(ZIP2, );
      auto a = readFromVecOperand(1, eltSize, numElts);
      auto b = readFromVecOperand(2, eltSize, numElts);
      Value *res = getUndefVec(numElts, eltSize);
      for (unsigned i = 0; i < numElts / 2; ++i) {
        auto e1 = createExtractElement(a, (numElts / 2) + i);
        auto e2 = createExtractElement(b, (numElts / 2) + i);
        res = createInsertElement(res, e1, 2 * i);
        res = createInsertElement(res, e2, (2 * i) + 1);
      }
      updateOutputReg(res);
      break;
    }

    case AArch64::ADDPv8i8:
    case AArch64::ADDPv4i16:
    case AArch64::ADDPv2i32:
    case AArch64::ADDPv16i8:
    case AArch64::ADDPv4i32:
    case AArch64::ADDPv8i16:
    case AArch64::ADDPv2i64: {
      GET_SIZES7(ADDP, );
      auto x = readFromOperand(1);
      auto y = readFromOperand(2);
      auto conc = concat(y, x);
      auto concTy = getVecTy(eltSize, numElts * 2);
      auto concV = createBitCast(conc, concTy);
      Value *res = getUndefVec(numElts, eltSize);
      for (unsigned e = 0; e < numElts; ++e) {
        *out << "e = " << e << "\n";
        auto elt1 = createExtractElement(concV, 2 * e);
        auto elt2 = createExtractElement(concV, (2 * e) + 1);
        auto sum = createAdd(elt1, elt2);
        res = createInsertElement(res, sum, e);
      }
      updateOutputReg(res);
      break;
    }

#define GET_SIZES9(INSN, SUFF)                                                 \
  unsigned numElts, eltSize;                                                   \
  if (opcode == AArch64::INSN##v8i8##SUFF) {                                   \
    numElts = 8;                                                               \
    eltSize = 8;                                                               \
  } else if (opcode == AArch64::INSN##v4i16##SUFF) {                           \
    numElts = 4;                                                               \
    eltSize = 16;                                                              \
  } else if (opcode == AArch64::INSN##v1i32##SUFF) {                           \
    numElts = 1;                                                               \
    eltSize = 32;                                                              \
  } else if (opcode == AArch64::INSN##v2i32##SUFF) {                           \
    numElts = 2;                                                               \
    eltSize = 32;                                                              \
  } else if (opcode == AArch64::INSN##v16i8##SUFF) {                           \
    numElts = 16;                                                              \
    eltSize = 8;                                                               \
  } else if (opcode == AArch64::INSN##v4i32##SUFF) {                           \
    numElts = 4;                                                               \
    eltSize = 32;                                                              \
  } else if (opcode == AArch64::INSN##v8i16##SUFF) {                           \
    numElts = 8;                                                               \
    eltSize = 16;                                                              \
  } else if (opcode == AArch64::INSN##v1i64##SUFF) {                           \
    numElts = 1;                                                               \
    eltSize = 64;                                                              \
  } else if (opcode == AArch64::INSN##v2i64##SUFF) {                           \
    numElts = 2;                                                               \
    eltSize = 64;                                                              \
  } else {                                                                     \
    assert(false);                                                             \
  }

      // FIXME: we're not doing this "If saturation occurs, the
      // cumulative saturation bit FPSR.QC is set." (same applies to
      // UQSUB)
    case AArch64::UQADDv1i32:
    case AArch64::UQADDv1i64:
    case AArch64::UQADDv8i8:
    case AArch64::UQADDv4i16:
    case AArch64::UQADDv2i32:
    case AArch64::UQADDv2i64:
    case AArch64::UQADDv4i32:
    case AArch64::UQADDv16i8:
    case AArch64::UQADDv8i16: {
      GET_SIZES9(UQADD, );
      auto x = readFromVecOperand(1, eltSize, numElts);
      auto y = readFromVecOperand(2, eltSize, numElts);
      auto res = createUAddSat(x, y);
      updateOutputReg(res);
      break;
    }

    case AArch64::UQSUBv1i32:
    case AArch64::UQSUBv1i64:
    case AArch64::UQSUBv8i8:
    case AArch64::UQSUBv4i16:
    case AArch64::UQSUBv2i32:
    case AArch64::UQSUBv2i64:
    case AArch64::UQSUBv4i32:
    case AArch64::UQSUBv16i8:
    case AArch64::UQSUBv8i16: {
      GET_SIZES9(UQSUB, );
      auto x = readFromVecOperand(1, eltSize, numElts);
      auto y = readFromVecOperand(2, eltSize, numElts);
      auto res = createUSubSat(x, y);
      updateOutputReg(res);
      break;
    }

    case AArch64::SQADDv1i32:
    case AArch64::SQADDv1i64:
    case AArch64::SQADDv8i8:
    case AArch64::SQADDv4i16:
    case AArch64::SQADDv2i32:
    case AArch64::SQADDv2i64:
    case AArch64::SQADDv4i32:
    case AArch64::SQADDv16i8:
    case AArch64::SQADDv8i16: {
      GET_SIZES9(SQADD, );
      auto x = readFromVecOperand(1, eltSize, numElts);
      auto y = readFromVecOperand(2, eltSize, numElts);
      auto res = createSAddSat(x, y);
      updateOutputReg(res);
      break;
    }

    case AArch64::SQSUBv1i32:
    case AArch64::SQSUBv1i64:
    case AArch64::SQSUBv8i8:
    case AArch64::SQSUBv4i16:
    case AArch64::SQSUBv2i32:
    case AArch64::SQSUBv2i64:
    case AArch64::SQSUBv4i32:
    case AArch64::SQSUBv16i8:
    case AArch64::SQSUBv8i16: {
      GET_SIZES9(SQSUB, );
      auto x = readFromVecOperand(1, eltSize, numElts);
      auto y = readFromVecOperand(2, eltSize, numElts);
      auto res = createSSubSat(x, y);
      updateOutputReg(res);
      break;
    }

    case AArch64::UMULLv4i16_indexed:
    case AArch64::UMULLv2i32_indexed:
    case AArch64::UMULLv8i16_indexed:
    case AArch64::UMULLv4i32_indexed:
    case AArch64::SMULLv4i32_indexed:
    case AArch64::SMULLv8i16_indexed:
    case AArch64::SMULLv4i16_indexed:
    case AArch64::SMULLv2i32_indexed: {
      int eltSize, numElts;
      if (opcode == AArch64::SMULLv8i16_indexed ||
          opcode == AArch64::UMULLv8i16_indexed) {
        numElts = 8;
        eltSize = 16;
      } else if (opcode == AArch64::SMULLv4i32_indexed ||
                 opcode == AArch64::UMULLv4i32_indexed) {
        numElts = 4;
        eltSize = 32;
      } else if (opcode == AArch64::SMULLv2i32_indexed ||
                 opcode == AArch64::UMULLv2i32_indexed) {
        numElts = 2;
        eltSize = 32;
      } else if (opcode == AArch64::SMULLv4i16_indexed ||
                 opcode == AArch64::UMULLv4i16_indexed) {
        numElts = 4;
        eltSize = 16;
      } else {
        assert(false);
      }
      auto vTy = getVecTy(eltSize, numElts);
      auto vBigTy = getVecTy(2 * eltSize, numElts);
      Value *a, *b;
      switch (opcode) {
      case AArch64::UMULLv4i16_indexed:
      case AArch64::UMULLv2i32_indexed:
      case AArch64::UMULLv8i16_indexed:
      case AArch64::UMULLv4i32_indexed:
        a = createZExt(createBitCast(readFromOperand(1), vTy), vBigTy);
        b = createZExt(createBitCast(readFromOperand(2), vTy), vBigTy);
        break;
      case AArch64::SMULLv4i32_indexed:
      case AArch64::SMULLv8i16_indexed:
      case AArch64::SMULLv4i16_indexed:
      case AArch64::SMULLv2i32_indexed:
        a = createSExt(createBitCast(readFromOperand(1), vTy), vBigTy);
        b = createSExt(createBitCast(readFromOperand(2), vTy), vBigTy);
        break;
      default:
        assert(false);
      }
      auto idx = getImm(3);
      Value *res = getUndefVec(numElts, 2 * eltSize);
      // offset is nonzero when we're dealing with SMULL2/UMULL2
      int offset = (opcode == AArch64::SMULLv4i32_indexed ||
                    opcode == AArch64::SMULLv8i16_indexed ||
                    opcode == AArch64::UMULLv4i32_indexed ||
                    opcode == AArch64::UMULLv8i16_indexed)
                       ? (numElts / 2)
                       : 0;
      for (int i = 0; i < numElts; ++i) {
        auto e1 = createExtractElement(a, i + offset);
        auto e2 = createExtractElement(b, idx);
        res = createInsertElement(res, createMul(e1, e2), i);
      }
      updateOutputReg(res);
      break;
    }
    case AArch64::SHADDv8i8:
    case AArch64::SHADDv16i8:
    case AArch64::SHADDv4i16:
    case AArch64::SHADDv8i16:
    case AArch64::SHADDv2i32:
    case AArch64::SHADDv4i32:
      //    case AArch64::SHSUBv8i8:
      //    case AArch64::SHSUBv16i8:
      //    case AArch64::SHSUBv4i16:
      //    case AArch64::SHSUBv8i16:
      //    case AArch64::SHSUBv2i32:
      //    case AArch64::SHSUBv4i32:
      {
        unsigned numElts, eltSize;
        switch (opcode) {
        case AArch64::SHADDv8i8:
        case AArch64::SHSUBv8i8:
          numElts = 8;
          eltSize = 8;
          break;
        case AArch64::SHADDv16i8:
        case AArch64::SHSUBv16i8:
          numElts = 16;
          eltSize = 8;
          break;
        case AArch64::SHADDv4i16:
        case AArch64::SHSUBv4i16:
          numElts = 4;
          eltSize = 16;
          break;
        case AArch64::SHADDv8i16:
        case AArch64::SHSUBv8i16:
          numElts = 8;
          eltSize = 16;
          break;
        case AArch64::SHADDv2i32:
        case AArch64::SHSUBv2i32:
          numElts = 2;
          eltSize = 32;
          break;
        case AArch64::SHADDv4i32:
        case AArch64::SHSUBv4i32:
          numElts = 4;
          eltSize = 32;
          break;
        default:
          assert(false);
          break;
        }

        auto a = createSExt(readFromVecOperand(1, eltSize, numElts),
                            getVecTy(2 * eltSize, numElts));
        auto b = createSExt(readFromVecOperand(2, eltSize, numElts),
                            getVecTy(2 * eltSize, numElts));

        auto res = opcode == AArch64::SHADDv8i8 ||
                           opcode == AArch64::SHADDv16i8 ||
                           opcode == AArch64::SHADDv4i16 ||
                           opcode == AArch64::SHADDv8i16 ||
                           opcode == AArch64::SHADDv2i32 ||
                           opcode == AArch64::SHADDv4i32
                       ? createAdd(a, b)
                       : createSub(a, b);

        std::vector<Constant *> vectorOfOnes(numElts,
                                             getIntConst(1, 2 * eltSize));
        auto shifted = createRawAShr(res, getVectorConst(vectorOfOnes));

        updateOutputReg(createTrunc(shifted, getVecTy(eltSize, numElts)));
        break;
      }

    case AArch64::XTNv2i32:
    case AArch64::XTNv4i32:
    case AArch64::XTNv4i16:
    case AArch64::XTNv8i16:
    case AArch64::XTNv8i8:
    case AArch64::XTNv16i8: {
      auto &op0 = CurInst->getOperand(0);
      u_int64_t srcReg = opcode == AArch64::XTNv2i32 ||
                                 opcode == AArch64::XTNv4i16 ||
                                 opcode == AArch64::XTNv8i8
                             ? 1
                             : 2;
      auto &op1 = CurInst->getOperand(srcReg);
      assert(isSIMDandFPRegOperand(op0) && isSIMDandFPRegOperand(op0));

      Value *src = readFromReg(op1.getReg());
      assert(getBitWidth(src) == 128 &&
             "Source value is not a vector with 128 bits");

      // eltSize is in bits
      u_int64_t eltSize, numElts, part;
      part = opcode == AArch64::XTNv2i32 || opcode == AArch64::XTNv4i16 ||
                     opcode == AArch64::XTNv8i8
                 ? 0
                 : 1;
      switch (opcode) {
      case AArch64::XTNv8i8:
      case AArch64::XTNv16i8:
        numElts = 8;
        eltSize = 8;
        break;
      case AArch64::XTNv4i16:
      case AArch64::XTNv8i16:
        numElts = 4;
        eltSize = 16;
        break;
      case AArch64::XTNv2i32:
      case AArch64::XTNv4i32:
        numElts = 2;
        eltSize = 32;
        break;
      default:
        *out << "\nError Unknown opcode\n";
        visitError();
        break;
      }

      // BitCast src to a vector of numElts x (2*eltSize) for narrowing
      assert(numElts * (2 * eltSize) == 128 && "BitCasting to wrong type");
      Value *src_vector = createBitCast(src, getVecTy(2 * eltSize, numElts));
      Value *narrowed_vector =
          createTrunc(src_vector, getVecTy(eltSize, numElts));

      Value *final_vector = narrowed_vector;
      // For XTN2 - insertion to upper half
      if (part == 1) {
        // Preserve the lower 64 bits so, read from destination register
        // and insert to the upper 64 bits
        Value *dest = readFromReg(op0.getReg());
        Value *original_dest_vector = createBitCast(dest, getVecTy(64, 2));

        Value *element = createBitCast(narrowed_vector, i64);

        final_vector = createInsertElement(original_dest_vector, element, 1);
      }

      // Write 64 bits for XTN or 128 bits XTN2 to output register
      updateOutputReg(final_vector);
      break;
    }

      // unary vector instructions
    case AArch64::ABSv1i64:
    case AArch64::ABSv8i8:
    case AArch64::ABSv4i16:
    case AArch64::ABSv2i32:
    case AArch64::ABSv2i64:
    case AArch64::ABSv16i8:
    case AArch64::ABSv8i16:
    case AArch64::ABSv4i32:
    case AArch64::CLZv2i32:
    case AArch64::CLZv4i16:
    case AArch64::CLZv8i8:
    case AArch64::CLZv16i8:
    case AArch64::CLZv8i16:
    case AArch64::CLZv4i32:
    case AArch64::UADALPv8i8_v4i16:
    case AArch64::UADALPv4i16_v2i32:
    case AArch64::UADALPv2i32_v1i64:
    case AArch64::UADALPv4i32_v2i64:
    case AArch64::UADALPv16i8_v8i16:
    case AArch64::UADALPv8i16_v4i32:
    case AArch64::SADALPv8i8_v4i16:
    case AArch64::SADALPv4i16_v2i32:
    case AArch64::SADALPv2i32_v1i64:
    case AArch64::SADALPv4i32_v2i64:
    case AArch64::SADALPv16i8_v8i16:
    case AArch64::SADALPv8i16_v4i32:
    case AArch64::UADDLPv4i16_v2i32:
    case AArch64::UADDLPv2i32_v1i64:
    case AArch64::UADDLPv8i8_v4i16:
    case AArch64::UADDLPv8i16_v4i32:
    case AArch64::UADDLPv4i32_v2i64:
    case AArch64::UADDLPv16i8_v8i16:
    case AArch64::SADDLPv8i8_v4i16:
    case AArch64::SADDLPv4i16_v2i32:
    case AArch64::SADDLPv2i32_v1i64:
    case AArch64::SADDLPv16i8_v8i16:
    case AArch64::SADDLPv8i16_v4i32:
    case AArch64::SADDLPv4i32_v2i64:
    case AArch64::UADDLVv8i16v:
    case AArch64::UADDLVv4i32v:
    case AArch64::UADDLVv8i8v:
    case AArch64::UADDLVv4i16v:
    case AArch64::UADDLVv16i8v:
    case AArch64::SADDLVv8i8v:
    case AArch64::SADDLVv16i8v:
    case AArch64::SADDLVv4i16v:
    case AArch64::SADDLVv8i16v:
    case AArch64::SADDLVv4i32v:
    case AArch64::NEGv1i64:
    case AArch64::NEGv4i16:
    case AArch64::NEGv8i8:
    case AArch64::NEGv16i8:
    case AArch64::NEGv2i32:
    case AArch64::NEGv8i16:
    case AArch64::NEGv2i64:
    case AArch64::NEGv4i32:
    case AArch64::NOTv8i8:
    case AArch64::NOTv16i8:
    case AArch64::CNTv8i8:
    case AArch64::CNTv16i8:
    case AArch64::ADDVv16i8v:
    case AArch64::ADDVv8i16v:
    case AArch64::ADDVv4i32v:
    case AArch64::ADDVv8i8v:
    case AArch64::ADDVv4i16v:
    case AArch64::RBITv8i8:
    case AArch64::RBITv16i8: {
      auto src = readFromOperand(1);
      u_int64_t eltSize, numElts;

      switch (opcode) {
      case AArch64::ABSv1i64:
      case AArch64::NEGv1i64:
        eltSize = 64;
        numElts = 1;
        break;
      case AArch64::ABSv4i16:
      case AArch64::CLZv4i16:
      case AArch64::NEGv4i16:
      case AArch64::UADDLVv4i16v:
      case AArch64::SADDLVv4i16v:
      case AArch64::UADDLPv4i16_v2i32:
      case AArch64::SADDLPv4i16_v2i32:
      case AArch64::UADALPv4i16_v2i32:
      case AArch64::SADALPv4i16_v2i32:
      case AArch64::ADDVv4i16v:
        eltSize = 16;
        numElts = 4;
        break;
      case AArch64::ABSv2i32:
      case AArch64::CLZv2i32:
      case AArch64::NEGv2i32:
      case AArch64::UADDLPv2i32_v1i64:
      case AArch64::SADDLPv2i32_v1i64:
      case AArch64::UADALPv2i32_v1i64:
      case AArch64::SADALPv2i32_v1i64:
        eltSize = 32;
        numElts = 2;
        break;
      case AArch64::ABSv8i16:
      case AArch64::CLZv8i16:
      case AArch64::NEGv8i16:
      case AArch64::UADDLVv8i16v:
      case AArch64::SADDLVv8i16v:
      case AArch64::UADDLPv8i16_v4i32:
      case AArch64::SADDLPv8i16_v4i32:
      case AArch64::UADALPv8i16_v4i32:
      case AArch64::SADALPv8i16_v4i32:
      case AArch64::ADDVv8i16v:
        eltSize = 16;
        numElts = 8;
        break;
      case AArch64::ABSv2i64:
      case AArch64::NEGv2i64:
        eltSize = 64;
        numElts = 2;
        break;
      case AArch64::ABSv4i32:
      case AArch64::CLZv4i32:
      case AArch64::NEGv4i32:
      case AArch64::UADDLVv4i32v:
      case AArch64::SADDLVv4i32v:
      case AArch64::UADDLPv4i32_v2i64:
      case AArch64::SADDLPv4i32_v2i64:
      case AArch64::UADALPv4i32_v2i64:
      case AArch64::SADALPv4i32_v2i64:
      case AArch64::ADDVv4i32v:
        eltSize = 32;
        numElts = 4;
        break;
      case AArch64::ABSv8i8:
      case AArch64::CLZv8i8:
      case AArch64::UADDLVv8i8v:
      case AArch64::SADDLVv8i8v:
      case AArch64::UADDLPv8i8_v4i16:
      case AArch64::SADDLPv8i8_v4i16:
      case AArch64::UADALPv8i8_v4i16:
      case AArch64::SADALPv8i8_v4i16:
      case AArch64::NEGv8i8:
      case AArch64::NOTv8i8:
      case AArch64::CNTv8i8:
      case AArch64::ADDVv8i8v:
      case AArch64::RBITv8i8:
        eltSize = 8;
        numElts = 8;
        break;
      case AArch64::ABSv16i8:
      case AArch64::CLZv16i8:
      case AArch64::RBITv16i8:
      case AArch64::ADDVv16i8v:
      case AArch64::NEGv16i8:
      case AArch64::UADDLVv16i8v:
      case AArch64::SADDLVv16i8v:
      case AArch64::UADDLPv16i8_v8i16:
      case AArch64::SADDLPv16i8_v8i16:
      case AArch64::UADALPv16i8_v8i16:
      case AArch64::SADALPv16i8_v8i16:
      case AArch64::NOTv16i8:
      case AArch64::CNTv16i8:
        eltSize = 8;
        numElts = 16;
        break;
      default:
        assert(false);
      }

      auto *vTy = getVecTy(eltSize, numElts);

      // Perform the operation
      switch (opcode) {
      case AArch64::ABSv1i64:
      case AArch64::ABSv8i8:
      case AArch64::ABSv4i16:
      case AArch64::ABSv2i32:
      case AArch64::ABSv2i64:
      case AArch64::ABSv16i8:
      case AArch64::ABSv8i16:
      case AArch64::ABSv4i32: {
        auto src_vector = createBitCast(src, vTy);
        auto res = createAbs(src_vector);
        updateOutputReg(res);
        break;
      }
      case AArch64::CLZv2i32:
      case AArch64::CLZv4i16:
      case AArch64::CLZv8i8:
      case AArch64::CLZv16i8:
      case AArch64::CLZv8i16:
      case AArch64::CLZv4i32: {
        auto src_vector = createBitCast(src, vTy);
        auto res = createCtlz(src_vector);
        updateOutputReg(res);
        break;
      }
      case AArch64::ADDVv16i8v:
      case AArch64::ADDVv8i16v:
      case AArch64::ADDVv4i32v:
      case AArch64::ADDVv8i8v:
      case AArch64::ADDVv4i16v: {
        auto src_vector = createBitCast(src, vTy);
        Value *sum = getIntConst(0, eltSize);
        for (unsigned i = 0; i < numElts; ++i) {
          auto elt = createExtractElement(src_vector, i);
          sum = createAdd(sum, elt);
        }
        // sum goes into the bottom lane, all others are zeroed out
        auto zero = getZeroVec(numElts, eltSize);
        auto res = createInsertElement(zero, sum, 0);
        updateOutputReg(res);
        break;
      }
      case AArch64::UADDLPv4i16_v2i32:
      case AArch64::UADDLPv2i32_v1i64:
      case AArch64::UADDLPv8i8_v4i16:
      case AArch64::UADDLPv8i16_v4i32:
      case AArch64::UADDLPv4i32_v2i64:
      case AArch64::UADDLPv16i8_v8i16: {
        auto src_vector = createBitCast(src, vTy);
        auto res = addPairs(src_vector, eltSize, numElts, extKind::ZExt);
        updateOutputReg(res);
        break;
      }
      case AArch64::SADDLPv4i16_v2i32:
      case AArch64::SADDLPv2i32_v1i64:
      case AArch64::SADDLPv8i8_v4i16:
      case AArch64::SADDLPv8i16_v4i32:
      case AArch64::SADDLPv4i32_v2i64:
      case AArch64::SADDLPv16i8_v8i16: {
        auto src_vector = createBitCast(src, vTy);
        auto res = addPairs(src_vector, eltSize, numElts, extKind::SExt);
        updateOutputReg(res);
        break;
      }
      case AArch64::UADALPv4i16_v2i32:
      case AArch64::UADALPv2i32_v1i64:
      case AArch64::UADALPv8i8_v4i16:
      case AArch64::UADALPv8i16_v4i32:
      case AArch64::UADALPv4i32_v2i64:
      case AArch64::UADALPv16i8_v8i16:
      case AArch64::SADALPv4i16_v2i32:
      case AArch64::SADALPv2i32_v1i64:
      case AArch64::SADALPv8i8_v4i16:
      case AArch64::SADALPv8i16_v4i32:
      case AArch64::SADALPv4i32_v2i64:
      case AArch64::SADALPv16i8_v8i16: {
        auto ext = opcode == AArch64::SADALPv4i16_v2i32 ||
                           opcode == AArch64::SADALPv2i32_v1i64 ||
                           opcode == AArch64::SADALPv8i8_v4i16 ||
                           opcode == AArch64::SADALPv8i16_v4i32 ||
                           opcode == AArch64::SADALPv4i32_v2i64 ||
                           opcode == AArch64::SADALPv16i8_v8i16
                       ? extKind::SExt
                       : extKind::ZExt;
        auto src2 = readFromOperand(2);
        auto src2_vector = createBitCast(src2, vTy);
        auto sum = addPairs(src2_vector, eltSize, numElts, ext);
        auto *bigTy = getVecTy(2 * eltSize, numElts / 2);
        Value *res = getUndefVec(numElts / 2, 2 * eltSize);
        auto src_vector = createBitCast(src, bigTy);
        for (unsigned i = 0; i < numElts / 2; ++i) {
          auto elt1 = createExtractElement(src_vector, i);
          auto elt2 = createExtractElement(sum, i);
          auto add = createAdd(elt1, elt2);
          res = createInsertElement(res, add, i);
        }
        updateOutputReg(res);
        break;
      }
      case AArch64::UADDLVv8i16v:
      case AArch64::UADDLVv4i32v:
      case AArch64::UADDLVv8i8v:
      case AArch64::UADDLVv4i16v:
      case AArch64::UADDLVv16i8v:
      case AArch64::SADDLVv8i8v:
      case AArch64::SADDLVv16i8v:
      case AArch64::SADDLVv4i16v:
      case AArch64::SADDLVv8i16v:
      case AArch64::SADDLVv4i32v: {
        bool isSigned =
            opcode == AArch64::SADDLVv8i8v || opcode == AArch64::SADDLVv16i8v ||
            opcode == AArch64::SADDLVv4i16v ||
            opcode == AArch64::SADDLVv8i16v || opcode == AArch64::SADDLVv4i32v;
        auto src_vector = createBitCast(src, vTy);
        auto bigTy = getIntTy(2 * eltSize);
        Value *sum = getIntConst(0, 2 * eltSize);
        for (unsigned i = 0; i < numElts; ++i) {
          auto elt = createExtractElement(src_vector, i);
          auto ext = isSigned ? createSExt(elt, bigTy) : createZExt(elt, bigTy);
          sum = createAdd(sum, ext);
        }
        updateOutputReg(sum);
        break;
      }
      case AArch64::NOTv8i8:
      case AArch64::NOTv16i8: {
        auto src_vector = createBitCast(src, vTy);
        auto neg_one = ConstantInt::get(vTy, APInt::getAllOnes(eltSize));
        updateOutputReg(createXor(src_vector, neg_one));
        break;
      }
      case AArch64::RBITv8i8:
      case AArch64::RBITv16i8: {
        auto src_vector = createBitCast(src, vTy);
        auto result = createBitReverse(src_vector);
        updateOutputReg(result);
        break;
      }
      case AArch64::CNTv8i8:
      case AArch64::CNTv16i8: {
        auto src_vector = createBitCast(src, vTy);
        updateOutputReg(createCtPop(src_vector));
        break;
      }
      case AArch64::NEGv1i64:
      case AArch64::NEGv4i16:
      case AArch64::NEGv8i8:
      case AArch64::NEGv16i8:
      case AArch64::NEGv2i32:
      case AArch64::NEGv8i16:
      case AArch64::NEGv2i64:
      case AArch64::NEGv4i32: {
        auto src_vector = createBitCast(src, vTy);
        auto zeroes = ConstantInt::get(vTy, APInt::getZero(eltSize));
        updateOutputReg(createSub(zeroes, src_vector));
        break;
      }
      default:
        assert(false);
      }
      break;
    }
    default:
      *out << funcToString(liftedFn);
      *out << "\nError "
              "detected----------partially-lifted-arm-target----------\n";
      visitError();
    }
  }

  // create the actual storage associated with a register -- all of its
  // asm-level aliases will get redirected here
  void createRegStorage(unsigned Reg, unsigned Width, const string &Name) {
    auto A = createAlloca(getIntTy(Width), getIntConst(1, 64), Name);
    auto F = createFreeze(PoisonValue::get(getIntTy(Width)));
    createStore(F, A);
    RegFile[Reg] = A;
  }

  /*
   * the idea here is that if a parameter to the lifted function, or
   * the return value from the lifted function is, for example, 8
   * bits, then we only want to initialize the lower 8 bits of the
   * register or stack slot, with the remaining bits containing junk,
   * in order to detect cases where the compiler incorrectly emits
   * code depending on that junk. on the other hand, if a parameter is
   * signext or zeroext then we have to actually initialize those
   * higher bits.
   *
   * FIXME -- this code was originally developed for scalar parameters
   * and we're mostly sort of hoping it also works for vectors. this
   * should work fine as long as the only vectors we accept are 64 and
   * 128 bits, which seemed (as of Nov 2023) to be the only ones with
   * a stable ABI
   */
  Value *enforceABIRules(Value *V, bool isSExt, bool isZExt) {
    auto i8 = getIntTy(8);
    auto i32 = getIntTy(32);
    auto argTy = V->getType();
    unsigned targetWidth;

    // no work needed
    if (argTy->isPointerTy() || argTy->isVoidTy())
      return V;

    if (argTy->isVectorTy() || argTy->isFloatingPointTy()) {
      auto W = getBitWidth(V);
      argTy = getIntTy(W);
      V = createBitCast(V, argTy);
      if (W <= 64)
        targetWidth = 64;
      else
        targetWidth = 128;
    } else {
      targetWidth = 64;
    }

    assert(argTy->isIntegerTy());

    /*
     * i1 has special two ABI rules. first, by default, an i1 is
     * implicitly zero-extended to i8. this is from AAPCS64. second,
     * if the i1 is a signext parameter, then this overrides the
     * zero-extension rule. this is from the LLVM folks.
     */
    if (getBitWidth(V) == 1) {
      if (isSExt)
        V = createSExt(V, i32);
      else
        V = createZExt(V, i8);
    }

    if (isSExt) {
      if (getBitWidth(V) < 32)
        V = createSExt(V, i32);
      else if (getBitWidth(V) > 32 && getBitWidth(V) < targetWidth)
        V = createSExt(V, getIntTy(targetWidth));
    }

    if (isZExt && getBitWidth(V) < targetWidth)
      V = createZExt(V, getIntTy(targetWidth));

    // finally, pad out any remaining bits with junk (frozen poisons)
    auto junkBits = targetWidth - getBitWidth(V);
    if (junkBits > 0) {
      auto junk = createFreeze(PoisonValue::get(getIntTy(junkBits)));
      auto ext1 = createZExt(junk, getIntTy(targetWidth));
      auto shifted =
          createRawShl(ext1, getIntConst(getBitWidth(V), targetWidth));
      auto ext2 = createZExt(V, getIntTy(targetWidth));
      V = createOr(shifted, ext2);
    }

    return V;
  }

  string linkageStr(GlobalValue::LinkageTypes linkage) {
    switch (linkage) {
    case GlobalValue::LinkageTypes::ExternalLinkage:
      return "External";
    case GlobalValue::LinkageTypes::AvailableExternallyLinkage:
      return "AvailableExternally";
    case GlobalValue::LinkageTypes::LinkOnceAnyLinkage:
      return "LinkOnceAny";
    case GlobalValue::LinkageTypes::LinkOnceODRLinkage:
      return "LinkOnceODR";
    case GlobalValue::LinkageTypes::WeakAnyLinkage:
      return "WeakAny";
    case GlobalValue::LinkageTypes::WeakODRLinkage:
      return "WeakODR";
    case GlobalValue::LinkageTypes::AppendingLinkage:
      return "Appending";
    case GlobalValue::LinkageTypes::InternalLinkage:
      return "Internal";
    case GlobalValue::LinkageTypes::PrivateLinkage:
      return "Private";
    case GlobalValue::LinkageTypes::ExternalWeakLinkage:
      return "ExternalWeak";
    case GlobalValue::LinkageTypes::CommonLinkage:
      return "Common";
    default:
      assert(false);
    }
  }

  Function *run() {
    auto i8 = getIntTy(8);
    auto i32 = getIntTy(32);
    auto i64 = getIntTy(64);

    // create a fresh function
    liftedFn =
        Function::Create(srcFn.getFunctionType(), GlobalValue::ExternalLinkage,
                         0, srcFn.getName(), LiftedModule);
    liftedFn->copyAttributesFrom(&srcFn);

    // create LLVM-side basic blocks
    vector<pair<BasicBlock *, MCBasicBlock *>> BBs;
    for (auto &mbb : MF.BBs) {
      auto bb = BasicBlock::Create(Ctx, mbb.getName(), liftedFn);
      BBs.push_back(make_pair(bb, &mbb));
    }

    // default to adding instructions to the entry block
    LLVMBB = BBs[0].first;

    // number of 8-byte stack slots for paramters
    const int numStackSlots = 32;
    // amount of stack available for use by the lifted function, in bytes
    const int localFrame = 1024;

    auto *allocTy =
        FunctionType::get(PointerType::get(Ctx, 0), {i32, i32}, false);
    auto *myAlloc = Function::Create(allocTy, GlobalValue::ExternalLinkage, 0,
                                     "myalloc", LiftedModule);
    myAlloc->addRetAttr(Attribute::NonNull);
    AttrBuilder B(Ctx);
    B.addAllocKindAttr(AllocFnKind::Alloc);
    B.addAllocSizeAttr(0, {});
    myAlloc->addFnAttrs(B);
    myAlloc->addParamAttr(1, Attribute::AllocAlign);
    stackMem =
        CallInst::Create(myAlloc,
                         {getIntConst(localFrame + (8 * numStackSlots), 32),
                          getIntConst(16, 32)},
                         "stack", LLVMBB);

    // allocate storage for the main register file
    for (unsigned Reg = AArch64::X0; Reg <= AArch64::X28; ++Reg) {
      stringstream Name;
      Name << "X" << Reg - AArch64::X0;
      createRegStorage(Reg, 64, Name.str());
      initialReg[Reg - AArch64::X0] = readFromReg(Reg);
    }

    // Allocating storage for thirty-two 128 bit NEON registers
    // https://developer.arm.com/documentation/den0024/a/AArch64-Floating-point-and-NEON?lang=en
    for (unsigned Reg = AArch64::Q0; Reg <= AArch64::Q31; ++Reg) {
      stringstream Name;
      Name << "Q" << Reg - AArch64::Q0;
      createRegStorage(Reg, 128, Name.str());
    }

    createRegStorage(AArch64::SP, 64, "SP");
    // load the base address for the stack memory; FIXME: this works
    // for accessing parameters but it doesn't support the general
    // case
    auto paramBase = createGEP(i8, stackMem, {getIntConst(localFrame, 64)}, "");
    createStore(paramBase, RegFile[AArch64::SP]);
    initialSP = readFromReg(AArch64::SP);

    // FP is X29; we'll initialize it later
    createRegStorage(AArch64::FP, 64, "FP");

    // LR is X30; FIXME initialize this
    createRegStorage(AArch64::LR, 64, "LR");

    // initializing to zero makes loads from XZR work; stores are
    // handled in updateReg()
    createRegStorage(AArch64::XZR, 64, "XZR");
    createStore(getIntConst(0, 64), RegFile[AArch64::XZR]);

    // allocate storage for PSTATE
    createRegStorage(AArch64::N, 1, "N");
    createRegStorage(AArch64::Z, 1, "Z");
    createRegStorage(AArch64::C, 1, "C");
    createRegStorage(AArch64::V, 1, "V");

    *out << "about to do callee-side ABI stuff\n";

    // implement the callee side of the ABI; FIXME -- this code only
    // supports integer parameters <= 64 bits and will require
    // significant generalization to handle large parameters
    unsigned vecArgNum = 0;
    unsigned scalarArgNum = 0;
    unsigned stackSlot = 0;

    for (Function::arg_iterator arg = liftedFn->arg_begin(),
                                E = liftedFn->arg_end(),
                                srcArg = srcFn.arg_begin();
         arg != E; ++arg, ++srcArg) {
      *out << "  processing " << getBitWidth(arg)
           << "-bit arg with vecArgNum = " << vecArgNum
           << ", scalarArgNum = " << scalarArgNum
           << ", stackSlot = " << stackSlot;
      auto *argTy = arg->getType();
      auto *val =
          enforceABIRules(arg, srcArg->hasSExtAttr(), srcArg->hasZExtAttr());

      // first 8 integer parameters go in the first 8 integer registers
      if ((argTy->isIntegerTy() || argTy->isPointerTy()) && scalarArgNum < 8) {
        auto Reg = AArch64::X0 + scalarArgNum;
        createStore(val, RegFile[Reg]);
        ++scalarArgNum;
        goto end;
      }

      // first 8 vector/FP parameters go in the first 8 vector registers
      if ((argTy->isVectorTy() || argTy->isFloatingPointTy()) &&
          vecArgNum < 8) {
        auto Reg = AArch64::Q0 + vecArgNum;
        createStore(val, RegFile[Reg]);
        ++vecArgNum;
        goto end;
      }

      // anything else goes onto the stack
      {
        // 128-bit alignment required for 128-bit arguments
        if ((getBitWidth(val) == 128) && ((stackSlot % 2) != 0)) {
          ++stackSlot;
          *out << " (actual stack slot = " << stackSlot << ")";
        }

        if (stackSlot >= numStackSlots) {
          *out << "\nERROR: maximum stack slots for parameter values "
                  "exceeded\n\n";
          exit(-1);
        }

        auto addr = createGEP(i64, paramBase, {getIntConst(stackSlot, 64)}, "");
        createStore(val, addr);

        if (getBitWidth(val) == 64) {
          stackSlot += 1;
        } else if (getBitWidth(val) == 128) {
          stackSlot += 2;
        } else {
          assert(false);
        }
      }

    end:
      *out << "\n";
    }

    *out << "done with callee-side ABI stuff\n";

    // initialize the frame pointer
    auto initFP = createGEP(i64, paramBase, {getIntConst(stackSlot, 64)}, "");
    createStore(initFP, RegFile[AArch64::FP]);

    *out << "about to lift the instructions\n";

    for (auto &[llvm_bb, mc_bb] : BBs) {
      *out << "visiting bb: " << mc_bb->getName() << "\n";
      LLVMBB = llvm_bb;
      MCBB = mc_bb;
      auto &mc_instrs = mc_bb->getInstrs();

      for (auto &inst : mc_instrs) {
        llvmInstNum = 0;
        liftInst(inst);
        ++armInstNum;
      }

      // machine code falls through but LLVM isn't allowed to
      if (!LLVMBB->getTerminator()) {
        auto succs = MCBB->getSuccs().size();
        if (succs == 0) {
          // this should only happen when we have a function with a
          // single, empty basic block, which should only when we
          // started with an LLVM function whose body is something
          // like UNREACHABLE
          doReturn();
        } else if (succs == 1) {
          auto *dst = getBBByName(MCBB->getSuccs()[0]->getName());
          createBranch(dst);
        }
      }
    }
    return liftedFn;
  }
};

// We're overriding MCStreamerWrapper to generate an MCFunction
// from the arm assembly. MCStreamerWrapper provides callbacks to handle
// different parts of the assembly file. The callbacks that we're
// using right now are all emit* functions.
class MCStreamerWrapper final : public MCStreamer {
  enum ASMLine { none = 0, label = 1, non_term_instr = 2, terminator = 3 };

private:
  MCInstrAnalysis *IA;
  MCInstPrinter *IP;
  MCRegisterInfo *MRI;

  MCBasicBlock *curBB{nullptr};
  unsigned prev_line{0};
  Align curAlign;
  string curSym;
  string curSec;
  bool FunctionEnded = false;

  vector<RODataItem> curROData;

public:
  MCFunction MF;
  unsigned cnt{0};

  MCStreamerWrapper(MCContext &Context, MCInstrAnalysis *IA, MCInstPrinter *IP,
                    MCRegisterInfo *MRI)
      : MCStreamer(Context), IA(IA), IP(IP), MRI(MRI) {
    MF.IA = IA;
    MF.IP = IP;
    MF.MRI = MRI;
  }

  void addConstant() {
    if (curROData.empty())
      return;

    MCGlobal g{
        .name = curSym,
        .align = curAlign,
        .section = curSec,
        .data = curROData,
    };
    MF.MCglobals.emplace_back(g);
    curROData.clear();
    *out << "created constant: " << curSym << "\n";
  }

  // We only want to intercept the emission of new instructions.
  virtual void emitInstruction(const MCInst &Inst,
                               const MCSubtargetInfo & /* unused */) override {

    assert(prev_line != ASMLine::none);

    if (prev_line == ASMLine::terminator)
      curBB = MF.addBlock(MF.getLabel());
    MCInst curInst(Inst);
    curBB->addInst(curInst);

    prev_line =
        IA->isTerminator(Inst) ? ASMLine::terminator : ASMLine::non_term_instr;
    auto num_operands = curInst.getNumOperands();
    for (unsigned i = 0; i < num_operands; ++i) {
      auto op = curInst.getOperand(i);
      if (op.isExpr()) {
        auto expr = op.getExpr();
        if (expr->getKind() == MCExpr::ExprKind::SymbolRef) {
          const MCSymbolRefExpr &SRE = cast<MCSymbolRefExpr>(*expr);
          const MCSymbol &Sym = SRE.getSymbol();
          *out << "target label : " << (string)Sym.getName()
               << ", offset=" << to_string(Sym.getOffset())
               << '\n'; // FIXME remove when done
        }
      }
    }

    *out << cnt++ << "  : ";
    std::string sss;
    llvm::raw_string_ostream ss(sss);
    Inst.dump_pretty(ss, IP, " ", MRI);
    *out << sss;
    if (IA->isBranch(Inst))
      *out << ": branch ";
    if (IA->isConditionalBranch(Inst))
      *out << ": conditional branch ";
    if (IA->isUnconditionalBranch(Inst))
      *out << ": unconditional branch ";
    if (IA->isTerminator(Inst))
      *out << ": terminator ";
    *out << "\n";
  }

  string attrName(MCSymbolAttr A) {
    switch (A) {
    case MCSA_ELF_TypeFunction:
      return "ELF function";
    case MCSA_ELF_TypeObject:
      return "ELF object";
    case MCSA_Global:
      return "global";
    default:
      assert(false && "unknown symbol attribute");
    }
  }

  virtual bool emitSymbolAttribute(MCSymbol *Symbol,
                                   MCSymbolAttr Attribute) override {
    *out << "[emitSymbolAttribute '" << Symbol->getName().str() << "']\n";
    if (false) {
      *out << "  Common? " << Symbol->isCommon() << "\n";
      *out << "  Variable? " << Symbol->isVariable() << "\n";
    }
    return true;
  }

  virtual void emitSymbolDesc(MCSymbol *Symbol, unsigned DescValue) override {
    *out << "[emitSymbolDesc '" << Symbol->getName().str() << "']\n";
  }

  virtual void emitCommonSymbol(MCSymbol *Symbol, uint64_t Size,
                                Align ByteAlignment) override {
    vector<RODataItem> data;
    string name{Symbol->getName()};
    for (uint64_t i = 0; i < Size; ++i)
      data.push_back(RODataItem{'0'});
    MCGlobal g{
        .name = name,
        .align = ByteAlignment,
        .section = "common",
        .data = data,
    };
    MF.MCglobals.emplace_back(g);
    *out << "[emitCommonSymbol = " << name << "]\n";
  }

  virtual void emitBytes(StringRef Data) override {
    auto len = Data.size();
    *out << "[emitBytes " << len << " bytes]\n";
    for (unsigned i = 0; i < len; ++i)
      curROData.push_back(RODataItem{Data[i]});
  }

  virtual void emitFill(const MCExpr &NumBytes, uint64_t FillValue,
                        SMLoc Loc) override {
    auto ce = dyn_cast<MCConstantExpr>(&NumBytes);
    if (ce) {
      assert(FillValue < 256);
      auto bytes = ce->getValue();
      *out << "[emitFill value = " << FillValue << ", size = " << bytes
           << "]\n";
      for (int i = 0; i < bytes; ++i)
        curROData.push_back(RODataItem{(char)FillValue});
    } else {
      *out << "[emitFill is unknown!]\n";
    }
  }

  virtual void emitZerofill(MCSection *Section, MCSymbol *Symbol = nullptr,
                            uint64_t Size = 0, Align ByteAlignment = Align(1),
                            SMLoc Loc = SMLoc()) override {
    *out << "[emitZerofill " << Size << " bytes]\n";
  }

  virtual void emitELFSize(MCSymbol *Symbol, const MCExpr *Value) override {
    *out << "[emitELFSize '" << Symbol->getName().str() << "']\n";
    addConstant();
  }

  virtual void emitDTPRel64Value(const MCExpr *Value) override {
    *out << "[emitDTPRel64Value]\n";
  }

  virtual void emitTPRel64Value(const MCExpr *Value) override {
    *out << "[emitTPRel64Value]\n";
  }

  virtual void emitGPRel64Value(const MCExpr *Value) override {
    *out << "[emitGPRel64Value]\n";
  }

  // FIXME -- we probably need a proper recursive descent parser for
  // MCExprs here
  virtual void emitValueImpl(const MCExpr *Value, unsigned Size,
                             SMLoc Loc = SMLoc()) override {
    if (auto SR = dyn_cast<MCSymbolRefExpr>(Value)) {
      const MCSymbol &Sym = SR->getSymbol();
      string name{Sym.getName()};
      *out << "[emitValue MCSymbolRefExpr= " << name << "]\n";
      OffsetSym s{name, 0};
      curROData.push_back(RODataItem{s});
    } else if (auto CE = dyn_cast<MCConstantExpr>(Value)) {
      *out << "[emitValue MCConstantExpr]\n";
      CE->dump();
      assert(false && "handle this");
    } else if (auto UE = dyn_cast<MCUnaryExpr>(Value)) {
      *out << "[emitValue MCUnaryExpr]\n";
      UE->dump();
      assert(false && "handle this");
    } else if (auto BE = dyn_cast<MCBinaryExpr>(Value)) {
      *out << "[emitValue MCBinaryExpr]\n";
      auto *LHS = dyn_cast<MCSymbolRefExpr>(BE->getLHS());
      assert(LHS);
      auto *RHS = dyn_cast<MCConstantExpr>(BE->getRHS());
      assert(RHS);
      OffsetSym s{(string)LHS->getSymbol().getName(), RHS->getValue()};
      curROData.push_back(RODataItem{s});
    } else if (auto TE = dyn_cast<MCTargetExpr>(Value)) {
      *out << "[emitValue MCTargetExpr]\n";
      TE->dump();
      assert(false && "handle this");
    } else {
      assert(false && "unexpected MCExpr type");
    }
  }

  virtual void emitValueToAlignment(Align Alignment, int64_t Value = 0,
                                    unsigned int ValueSize = 1,
                                    unsigned int MaxBytesToEmit = 0) override {
    *out << "[emitValueToAlignment= " << Alignment.value() << "]\n";
    curAlign = Alignment;
  }

  virtual void emitAssignment(MCSymbol *Symbol, const MCExpr *Value) override {
    *out << "[emitAssignment]\n";
  }

  virtual void emitLabel(MCSymbol *Symbol, SMLoc Loc) override {
    auto sp = getCurrentSection();
    string Lab = Symbol->getName().str();
    *out << "[emitLabel '" << Lab << "' in section '"
         << (string)(sp.first->getName()) << "']\n";

    addConstant();

    curSym = Symbol->getName().str();
    curSec = (string)sp.first->getName();

    if (Lab == ".Lfunc_end0")
      FunctionEnded = true;
    if (!FunctionEnded) {
      curBB = MF.addBlock(Lab);
      MCInst nop;
      // subsequent logic can be a bit simpler if we assume each BB
      // contains at least one instruction. might need to revisit this
      // later on.
      nop.setOpcode(AArch64::SEH_Nop);
      curBB->addInstBegin(std::move(nop));
      prev_line = ASMLine::label;
    }
  }

  string findTargetLabel(MCInst &Inst) {
    auto num_operands = Inst.getNumOperands();
    for (unsigned i = 0; i < num_operands; ++i) {
      auto op = Inst.getOperand(i);
      if (op.isExpr()) {
        auto expr = op.getExpr();
        if (expr->getKind() == MCExpr::ExprKind::SymbolRef) {
          const MCSymbolRefExpr &SRE = cast<MCSymbolRefExpr>(*expr);
          const MCSymbol &Sym = SRE.getSymbol();
          return Sym.getName().str();
        }
      }
    }
    assert(false && "could not find target label in arm branch instruction");
  }

  // Make sure that we have an entry label with no predecessors
  void checkEntryBlock() {
    MF.checkEntryBlock();
  }

  // Fill in the CFG
  void generateSuccessors() {
    *out << "generating basic block successors" << '\n';
    for (unsigned i = 0; i < MF.BBs.size(); ++i) {
      auto &cur_bb = MF.BBs[i];
      MCBasicBlock *next_bb_ptr = nullptr;
      if (i < MF.BBs.size() - 1)
        next_bb_ptr = &MF.BBs[i + 1];

      if (cur_bb.empty()) {
        *out
            << "generateSuccessors, encountered basic block with 0 instructions"
            << '\n';
        continue;
      }
      auto &last_mc_instr = cur_bb.getInstrs().back();
      // handle the special case of adding where we have added a new entry block
      // with no predecessors. This is hacky because I don't know the API to
      // create an MCExpr and have to create a branch with an immediate operand
      // instead
      if (i == 0 && (IA->isUnconditionalBranch(last_mc_instr)) &&
          last_mc_instr.getOperand(0).isImm()) {
        cur_bb.addSucc(next_bb_ptr);
        continue;
      }
      if (IA->isConditionalBranch(last_mc_instr)) {
        string target = findTargetLabel(last_mc_instr);
        auto target_bb = MF.findBlockByName(target);
        assert(target_bb);
        cur_bb.addSucc(target_bb);
        if (next_bb_ptr)
          cur_bb.addSucc(next_bb_ptr);
      } else if (IA->isUnconditionalBranch(last_mc_instr)) {
        string target = findTargetLabel(last_mc_instr);
        auto target_bb = MF.findBlockByName(target);
        if (target_bb) {
          cur_bb.addSucc(target_bb);
        } else {
          *out << "looks like a tail call to " << target << "\n";
        }
      } else if (IA->isReturn(last_mc_instr)) {
        continue;
      } else if (next_bb_ptr) {
        // add edge to next block
        cur_bb.addSucc(next_bb_ptr);
      }
    }
  }

  // Remove empty basic blocks, including .Lfunc_end
  void removeEmptyBlocks() {
    erase_if(MF.BBs, [](MCBasicBlock bb) { return bb.empty(); });
  }
};

} // namespace

namespace lifter {

std::ostream *out;
unsigned int origRetWidth;
bool has_ret_attr;
const Target *Targ;

void init() {
  static bool initialized = false;
  assert(!initialized);
  LLVMInitializeAArch64TargetInfo();
  LLVMInitializeAArch64Target();
  LLVMInitializeAArch64TargetMC();
  LLVMInitializeAArch64AsmParser();
  LLVMInitializeAArch64AsmPrinter();
  string Error;
  Targ = TargetRegistry::lookupTarget(TripleName, Error);
  if (!Targ) {
    *out << Error;
    exit(-1);
  }
  origRetWidth = 64;
  has_ret_attr = false;
  initialized = true;
}

pair<Function *, Function *> liftFunc(Module *OrigModule, Module *LiftedModule,
                                      Function *srcFn,
                                      unique_ptr<MemoryBuffer> MB) {
  llvm::SourceMgr SrcMgr;
  SrcMgr.AddNewSourceBuffer(std::move(MB), llvm::SMLoc());

  unique_ptr<MCInstrInfo> MCII(Targ->createMCInstrInfo());
  assert(MCII && "Unable to create instruction info!");

  Triple TheTriple(TripleName);

  auto MCOptions = mc::InitMCTargetOptionsFromFlags();
  unique_ptr<MCRegisterInfo> MRI(Targ->createMCRegInfo(TripleName));
  assert(MRI && "Unable to create target register info!");

  unique_ptr<MCSubtargetInfo> STI(
      Targ->createMCSubtargetInfo(TripleName, CPU, ""));
  assert(STI && "Unable to create subtarget info!");
  assert(STI->isCPUStringValid(CPU) && "Invalid CPU!");

  unique_ptr<MCAsmInfo> MAI(Targ->createMCAsmInfo(*MRI, TripleName, MCOptions));
  assert(MAI && "Unable to create MC asm info!");
  unique_ptr<MCInstPrinter> IP(
      Targ->createMCInstPrinter(TheTriple, 0, *MAI, *MCII, *MRI));
  IP->setPrintImmHex(true);

  auto Ana = make_unique<MCInstrAnalysis>(MCII.get());

  MCContext Ctx(TheTriple, MAI.get(), MRI.get(), STI.get(), &SrcMgr,
                &MCOptions);
  std::unique_ptr<MCObjectFileInfo> MCOFI(
      Targ->createMCObjectFileInfo(Ctx, false, false));
  Ctx.setObjectFileInfo(MCOFI.get());

  MCStreamerWrapper Str(Ctx, Ana.get(), IP.get(), MRI.get());
  Str.setUseAssemblerInfoForParsing(true);

  unique_ptr<MCAsmParser> Parser(createMCAsmParser(SrcMgr, Ctx, Str, *MAI));
  assert(Parser);

  unique_ptr<MCTargetAsmParser> TAP(
      Targ->createMCAsmParser(*STI, *Parser, *MCII, MCOptions));
  assert(TAP);
  Parser->setTargetParser(*TAP);

  if (Parser->Run(true)) {
    *out << "\nERROR: AsmParser failed\n";
    exit(-1);
  }

  Str.removeEmptyBlocks();
  Str.checkEntryBlock();
  Str.generateSuccessors();

  auto liftedFn = arm2llvm(LiftedModule, Str.MF, *srcFn, IP.get()).run();

  std::string sss;
  llvm::raw_string_ostream ss(sss);
  if (llvm::verifyModule(*LiftedModule, &ss)) {
    *out << sss << "\n\n";
    out->flush();
    *out << "\nERROR: Lifted module is broken, this should not happen\n";
    exit(-1);
  }

  return make_pair(srcFn, liftedFn);
}

} // namespace lifter
