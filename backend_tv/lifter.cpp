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
  MCBasicBlock(string name) : name(name) {}

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

class MCFunction {
  string name;
  unsigned label_cnt{0};

public:
  MCInstrAnalysis *IA;
  MCInstPrinter *IP;
  MCRegisterInfo *MRI;
  vector<MCBasicBlock> BBs;
  unordered_map<string, pair<uint64_t, uint64_t>> globals;

  MCFunction() {}

  void setName(string _name) {
    name = _name;
  }

  MCBasicBlock *addBlock(string b_name) {
    return &BBs.emplace_back(b_name);
  }

  string getName() {
    return name;
  }

  string getLabel() {
    return name + to_string(++label_cnt);
  }

  MCBasicBlock *findBlockByName(string b_name) {
    for (auto &bb : BBs)
      if (bb.getName() == b_name)
        return &bb;
    *out << "couldn't find block '" << b_name << "'\n";
    assert(false && "block not found");
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
  MCBasicBlock *MCBB{nullptr};
  BasicBlock *LLVMBB{nullptr};
  MCInstPrinter *instrPrinter{nullptr};
  MCInst *CurInst{nullptr}, *PrevInst{nullptr};
  unsigned armInstNum{0}, llvmInstNum{0};
  map<unsigned, Value *> RegFile;
  Value *stackMem{nullptr};
  unordered_map<string, GlobalVariable *> globals;
  Value *initialSP, *initialReg[32];

  // Map of ADRP MCInsts to the string representations of the operand variable
  // names
  unordered_map<MCInst *, string> instExprVarMap;
  bool DebugRegs;
  const DataLayout &DL;

  BasicBlock *getBB(Function &F, MCOperand &jmp_tgt) {
    assert(jmp_tgt.isExpr() && "[getBB] expected expression operand");
    assert((jmp_tgt.getExpr()->getKind() == MCExpr::ExprKind::SymbolRef) &&
           "[getBB] expected symbol ref as jump operand");
    const MCSymbolRefExpr &SRE = cast<MCSymbolRefExpr>(*jmp_tgt.getExpr());
    const MCSymbol &Sym = SRE.getSymbol();
    StringRef name = Sym.getName();
    for (auto &bb : F) {
      if (bb.getName() == name)
        return &bb;
    }
    assert(false && "basic block not found in getBB()");
  }

  // FIXME -- do this without the strings, just keep a map or something
  BasicBlock *getBBByName(Function &F, StringRef name) {
    for (auto &bb : F) {
      if (bb.getName() == name)
        return &bb;
    }
    assert(false && "basic block not found in getBBByName()");
  }

  static bool isSIMDandFPReg(MCOperand &op) {
    assert(op.isReg() && "[isSIMDandFPReg] expected register operand");
    unsigned reg = op.getReg();
    return (reg >= AArch64::Q0 && reg <= AArch64::Q31) ||
           (reg >= AArch64::D0 && reg <= AArch64::D31) ||
           (reg >= AArch64::S0 && reg <= AArch64::S31);
  }

  Type *getIntTy(unsigned int bits) {
    // just trying to catch silly errors, remove this sometime
    assert(bits > 0 && bits <= 256);
    return Type::getIntNTy(Ctx, bits);
  }

  Constant *getIntConst(uint64_t val, u_int64_t bits) {
    return ConstantInt::get(Ctx, llvm::APInt(bits, val));
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
      AArch64::ADDWrx,     AArch64::ADDSWrs,    AArch64::ADDSWri,
      AArch64::ADDWrs,     AArch64::ADDWri,     AArch64::ADDSWrx,
      AArch64::ADCWr,      AArch64::ADCSWr,     AArch64::ASRVWr,
      AArch64::SUBWri,     AArch64::SUBWrs,     AArch64::SUBWrx,
      AArch64::SUBSWrs,    AArch64::SUBSWri,    AArch64::SUBSWrx,
      AArch64::SBFMWri,    AArch64::CSELWr,     AArch64::ANDWri,
      AArch64::ANDWrr,     AArch64::ANDWrs,     AArch64::ANDSWri,
      AArch64::ANDSWrr,    AArch64::ANDSWrs,    AArch64::MADDWrrr,
      AArch64::MSUBWrrr,   AArch64::EORWri,     AArch64::CSINVWr,
      AArch64::CSINCWr,    AArch64::MOVZWi,     AArch64::MOVNWi,
      AArch64::MOVKWi,     AArch64::LSLVWr,     AArch64::LSRVWr,
      AArch64::ORNWrs,     AArch64::UBFMWri,    AArch64::BFMWri,
      AArch64::ORRWrs,     AArch64::ORRWri,     AArch64::SDIVWr,
      AArch64::UDIVWr,     AArch64::EXTRWrri,   AArch64::EORWrs,
      AArch64::RORVWr,     AArch64::RBITWr,     AArch64::CLZWr,
      AArch64::REVWr,      AArch64::CSNEGWr,    AArch64::BICWrs,
      AArch64::BICSWrs,    AArch64::EONWrs,     AArch64::REV16Wr,
      AArch64::Bcc,        AArch64::CCMPWr,     AArch64::CCMPWi,
      AArch64::LDRWui,     AArch64::LDRBBroW,   AArch64::LDRBBroX,
      AArch64::LDRHHroW,   AArch64::LDRHHroX,   AArch64::LDRWroW,
      AArch64::LDRWroX,    AArch64::LDRSui,     AArch64::LDRBBui,
      AArch64::LDRBui,     AArch64::LDRSBWui,   AArch64::LDRSWui,
      AArch64::LDRSHWui,   AArch64::LDRSBWui,   AArch64::LDRHHui,
      AArch64::LDRHui,     AArch64::LDURBi,     AArch64::LDURBBi,
      AArch64::LDURHi,     AArch64::LDURHHi,    AArch64::LDURSi,
      AArch64::LDURWi,     AArch64::STRWui,     AArch64::STRBBroW,
      AArch64::STRBBroX,   AArch64::STRHHroW,   AArch64::STRHHroX,
      AArch64::STRWroW,    AArch64::STRWroX,    AArch64::CCMNWi,
      AArch64::CCMNWr,     AArch64::STRBBui,    AArch64::STRBui,
      AArch64::STPWi,      AArch64::STRHHui,    AArch64::STRHui,
      AArch64::STURWi,     AArch64::STRSui,     AArch64::LDPWi,
      AArch64::STRWpre,    AArch64::FADDSrr,    AArch64::FSUBSrr,
      AArch64::FCMPSrr,    AArch64::FCMPSri,    AArch64::FMOVSWr,
      AArch64::INSvi32gpr, AArch64::INSvi16gpr, AArch64::INSvi8gpr,
      AArch64::FCVTSHr,    AArch64::FCVTZSUWSr, AArch64::FCSELSrrr,
      AArch64::FMULSrr,    AArch64::FABSSr,
  };

  const set<int> instrs_64 = {
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
      AArch64::TBZW,
      AArch64::TBZX,
      AArch64::TBNZW,
      AArch64::TBNZX,
      AArch64::B,
      AArch64::CBZW,
      AArch64::CBZX,
      AArch64::CBNZW,
      AArch64::CBNZX,
      AArch64::CCMPXr,
      AArch64::CCMPXi,
      AArch64::LDRXui,
      AArch64::LDRXpost,
      AArch64::LDPXpost,
      AArch64::LDPXi,
      AArch64::LDRDui,
      AArch64::LDRXroW,
      AArch64::LDRXroX,
      AArch64::LDURDi,
      AArch64::LDURXi,
      AArch64::STRDui,
      AArch64::MSR,
      AArch64::MRS,
      AArch64::LDRSBXui,
      AArch64::LDRSHXui,
      AArch64::STRXui,
      AArch64::STRXroW,
      AArch64::STRXroX,
      AArch64::STPXi,
      AArch64::CCMNXi,
      AArch64::CCMNXr,
      AArch64::STURXi,
      AArch64::ADRP,
      AArch64::STRXpre,
      AArch64::FADDDrr,
      AArch64::FMULDrr,
      AArch64::FABSDr,
      AArch64::FSUBDrr,
      AArch64::FCMPDrr,
      AArch64::FCMPDri,
      AArch64::NOTv8i8,
      AArch64::CNTv8i8,
      AArch64::ANDv8i8,
      AArch64::ORRv8i8,
      AArch64::EORv8i8,
      AArch64::FMOVDXr,
      AArch64::INSvi64gpr,
      AArch64::MOVID,
      AArch64::FCVTZSUWDr,
      AArch64::FMOVDr,
      AArch64::DUPv2i32gpr,
      AArch64::UMULLv2i32_v2i64,
      AArch64::USHLLv8i8_shift,
      AArch64::USHLLv2i32_shift,
      AArch64::USHLLv4i16_shift,
      AArch64::UADDLv8i8_v8i16,
      AArch64::USUBLv8i8_v8i16,
      AArch64::XTNv2i32,
      AArch64::XTNv4i16,
      AArch64::XTNv8i8,
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
      AArch64::UADDLPv4i16_v2i32,
      AArch64::UADDLPv2i32_v1i64,
      AArch64::UADDLPv8i8_v4i16,
      AArch64::UADALPv8i8_v4i16,
      AArch64::UADALPv4i16_v2i32,
      AArch64::CMHIv8i8,
      AArch64::CMHIv4i16,
      AArch64::CMHIv2i32,
      AArch64::CMHIv1i64,
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
  };

  const set<int> instrs_128 = {
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
      AArch64::DUPv8i8lane,
      AArch64::DUPv4i16lane,
      AArch64::DUPv2i32lane,
      AArch64::LDPQi,
      AArch64::LDRQroX,
      AArch64::LDURQi,
      AArch64::STPQi,
      AArch64::STRQroX,
      AArch64::ADDv8i16,
      AArch64::ADDv2i64,
      AArch64::ADDv4i32,
      AArch64::ADDv16i8,
      AArch64::SUBv8i16,
      AArch64::SUBv2i64,
      AArch64::SUBv4i32,
      AArch64::SUBv16i8,
      AArch64::LDRQui,
      AArch64::STRQui,
      AArch64::FMOVDi,
      AArch64::FMOVSi,
      AArch64::FMOVWSr,
      AArch64::CNTv16i8,
      AArch64::MOVIv2d_ns,
      AArch64::MOVIv4i32,
      AArch64::EXTv16i8,
      AArch64::MOVIv2i32,
      AArch64::ANDv16i8,
      AArch64::ORRv16i8,
      AArch64::EORv16i8,
      AArch64::UMOVvi32,
      AArch64::UMOVvi8,
      AArch64::UMOVvi8_idx0,
      AArch64::MOVIv16b_ns,
      AArch64::UMOVvi64,
      AArch64::UMOVvi16,
      AArch64::UMOVvi16_idx0,
      AArch64::SMOVvi16to32,
      AArch64::SMOVvi16to32_idx0,
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
      AArch64::UADDLVv8i16v,
      AArch64::UADDLVv4i32v,
      AArch64::UADDLVv16i8v,
      AArch64::UADDLPv8i16_v4i32,
      AArch64::UADDLPv4i32_v2i64,
      AArch64::UADDLPv16i8_v8i16,
      AArch64::UADALPv4i32_v2i64,
      AArch64::UADALPv16i8_v8i16,
      AArch64::UADALPv8i16_v4i32,
      AArch64::CMHIv16i8,
      AArch64::CMHIv8i16,
      AArch64::CMHIv4i32,
      AArch64::CMHIv2i64,
      AArch64::BIFv16i8,
      AArch64::BSLv16i8,
      AArch64::BICv8i16,
      AArch64::BICv4i32,
      AArch64::BICv16i8,
      AArch64::ADDVv16i8v,
      AArch64::ADDVv8i16v,
      AArch64::ADDVv4i32v,
  };

  bool has_s(int instr) {
    return s_flag.contains(instr);
  }

  /// decodeLogicalImmediate - Decode a logical immediate value in the form
  /// "N:immr:imms" (where the immr and imms fields are each 6 bits) into the
  /// integer value it represents with regSize bits.
  uint64_t decodeLogicalImmediate(uint64_t val, unsigned regSize) {
    // Extract the N, imms, and immr fields.
    unsigned N = (val >> 12) & 1;
    unsigned immr = (val >> 6) & 0x3f;
    unsigned imms = val & 0x3f;

    assert((regSize == 64 || N == 0) && "undefined logical immediate encoding");
    int len = 31 - llvm::countl_zero((N << 6) | (~imms & 0x3f));
    assert(len >= 0 && "undefined logical immediate encoding");
    unsigned size = (1 << len);
    unsigned R = immr & (size - 1);
    unsigned S = imms & (size - 1);
    assert(S != size - 1 && "undefined logical immediate encoding");
    uint64_t pattern = (1ULL << (S + 1)) - 1;
    for (unsigned i = 0; i < R; ++i)
      pattern = ((pattern & 1) << (size - 1)) | (pattern >> 1);

    // Replicate the pattern to fill the regSize.
    while (size != regSize) {
      pattern |= (pattern << size);
      size *= 2;
    }
    return pattern;
  }

  unsigned getInstSize(int instr) {
    if (instrs_32.contains(instr))
      return 32;
    if (instrs_64.contains(instr))
      return 64;
    if (instrs_128.contains(instr))
      return 128;
    *out << "getInstSize encountered unknown instruction"
         << "\n";
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

  CallInst *createCtPop(Value *v) {
    auto ctpop_decl =
        Intrinsic::getDeclaration(LiftedModule, Intrinsic::ctpop, v->getType());
    return CallInst::Create(ctpop_decl, {v}, nextName(), LLVMBB);
  }

  // first argument is an i16
  CallInst *createConvertFromFP16(Value *v, Type *ty) {
    auto cvt_decl = Intrinsic::getDeclaration(LiftedModule,
                                              Intrinsic::convert_from_fp16, ty);
    return CallInst::Create(cvt_decl, {v}, nextName(), LLVMBB);
  }

  CastInst *createConvertFPToSI(Value *v, Type *ty) {
    return new FPToSIInst(v, ty, nextName(), LLVMBB);
  }

  CastInst *createPtrToInt(Value *v, Type *ty) {
    return new PtrToIntInst(v, ty, nextName(), LLVMBB);
  }

  ExtractElementInst *createExtractElement(Value *v, Value *idx) {
    return ExtractElementInst::Create(v, idx, nextName(), LLVMBB);
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

  CastInst *createSExt(Value *v, Type *t) {
    return CastInst::Create(Instruction::SExt, v, t, nextName(), LLVMBB);
  }

  InsertElementInst *createInsertElement(Value *vec, Value *val, Value *index) {
    return InsertElementInst::Create(vec, val, index, nextName(), LLVMBB);
  }

  CastInst *createZExt(Value *v, Type *t) {
    return CastInst::Create(Instruction::ZExt, v, t, nextName(), LLVMBB);
  }

  CastInst *createBitCast(Value *v, Type *t) {
    return CastInst::Create(Instruction::BitCast, v, t, nextName(), LLVMBB);
  }

  CastInst *createCast(Value *v, Type *t, Instruction::CastOps op) {
    return CastInst::Create(op, v, t, nextName(), LLVMBB);
  }

  Value *splatImm(Value *v, unsigned eltCount, unsigned eltSize, bool shift) {
    assert(CurInst->getOperand(2).isImm());
    if (shift) {
      assert(CurInst->getOperand(3).isImm());
      v = regShift(v, getImm(3));
    }
    if (getBitWidth(v) > eltSize)
      v = createTrunc(v, getIntTy(eltSize));
    Value *res = ConstantVector::getSplat(ElementCount::getFixed(eltCount),
                                          UndefValue::get(getIntTy(eltSize)));
    for (unsigned i = 0; i < eltCount; ++i)
      res = createInsertElement(res, v, getIntConst(i, 32));
    return res;
  }

  Value *addPairs(Value *src, unsigned eltSize, unsigned numElts) {
    auto bigEltTy = getIntTy(2 * eltSize);
    Value *res = ConstantVector::getSplat(ElementCount::getFixed(numElts / 2),
                                          UndefValue::get(bigEltTy));
    for (unsigned i = 0; i < numElts; i += 2) {
      auto elt1 = createExtractElement(src, getIntConst(i, 32));
      auto elt2 = createExtractElement(src, getIntConst(i + 1, 32));
      auto ext1 = createZExt(elt1, bigEltTy);
      auto ext2 = createZExt(elt2, bigEltTy);
      auto sum = createAdd(ext1, ext2);
      res = createInsertElement(res, sum, getIntConst(i / 2, 32));
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
                        bool elementWise, bool zext, bool isICmp,
                        bool splatImm2, bool immShift) {
    assert(getBitWidth(a) == getBitWidth(b) &&
           "Expected values of same bit width");

    if (splatImm2)
      b = splatImm(b, numElts, eltSize, immShift);

    auto ec = ElementCount::getFixed(numElts);
    auto eTy = getIntTy(eltSize);
    auto vTy = VectorType::get(eTy, ec);

    a = createBitCast(a, vTy);
    b = createBitCast(b, vTy);

    // some instructions double element widths
    if (zext) {
      eTy = getIntTy(2 * eltSize);
      a = createZExt(a, VectorType::get(eTy, ec));
      b = createZExt(b, VectorType::get(eTy, ec));
    }

    Value *res = nullptr;
    if (elementWise) {
      res = ConstantVector::getSplat(ec, UndefValue::get(eTy));
      for (unsigned i = 0; i < numElts; ++i) {
        auto aa = createExtractElement(a, getIntConst(i, 32));
        auto bb = createExtractElement(b, getIntConst(i, 32));
        auto cc = op(aa, bb);
        res = createInsertElement(res, cc, getIntConst(i, 32));
      }
    } else {
      res = op(a, b);
    }
    if (isICmp)
      res = createSExt(res, vTy);
    return res;
  }

  static unsigned int getBitWidth(Value *V) {
    auto ty = V->getType();
    if (auto vTy = dyn_cast<VectorType>(ty)) {
      return vTy->getScalarSizeInBits() *
             vTy->getElementCount().getFixedValue();
    } else if (ty->isIntegerTy()) {
      return ty->getIntegerBitWidth();
    } else if (ty->isFloatTy()) {
      return 32;
    } else if (ty->isDoubleTy()) {
      return 64;
    } else {
      assert(false && "Unhandled type");
    }
  }

  // Returns bitWidth corresponding the registers.
  unsigned getRegSize(unsigned Reg) {
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

  void updateReg(Value *v, u_int64_t reg) {
    createStore(v, dealiasReg(reg));
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
  Value *readFromOperand(int idx) {
    auto op = CurInst->getOperand(idx);
    auto size = getInstSize(CurInst->getOpcode());
    // Expr operand is required for a combination of ADRP and ADDXri address
    // calculation
    assert(op.isImm() || op.isReg() || op.isExpr());

    if (!(size == 32 || size == 64 || size == 128)) {
      *out << "\nERROR: Only 32, 64 and 128 bit registers supported\n\n";
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
      auto [val, storePtr] = getExprVar(op.getExpr());
      V = val;
    }
    return V;
  }

  void updateOutputReg(Value *V, bool SExt = false) {
    auto destReg = CurInst->getOperand(0).getReg();

    // important -- squash updates to the zero register
    if (destReg == AArch64::WZR || destReg == AArch64::XZR)
      return;

    // FIXME do we really want to do this? and if so, do this for
    // floats too?
    if (V->getType()->isVectorTy())
      V = createBitCast(V, getIntTy(getBitWidth(V)));

    auto destRegSize = getRegSize(destReg);
    auto realRegSize = getRegSize(mapRegToBackingReg(destReg));

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

    updateReg(V, destReg);
  }

  // Reads an Expr and maps containing string variable to a global variable
  void mapExprVar(const MCExpr *expr) {
    std::string sss;

    // Matched strings
    std::smatch sm;

    // Regex to match relocation specifiers
    std::regex re(":[a-z0-9_]+:");

    llvm::raw_string_ostream ss(sss);
    expr->print(ss, nullptr);

    // If the expression starts with a relocation specifier, strip it and map
    // the rest to a string name of the global variable. Assuming there is only
    // one relocation specifier, and it is at the beginning
    // (std::regex_constants::match_continuous).
    // eg: ":lo12:a" becomes  "a"
    if (std::regex_search(sss, sm, re,
                          std::regex_constants::match_continuous)) {
      auto stringVar = sm.suffix();
      //      for (auto x:sm) { *out << x << " "; }
      //      *out << stringVar << "\n";
      if (!globals.contains(stringVar)) {
        *out << "\nERROR: ADRP mentions unknown global variable\n";
        *out << "'" << stringVar
             << "'  is not a global variable we know about\n";
        exit(-1);
      }
      instExprVarMap[CurInst] = stringVar;
    } else if (globals.contains(sss)) {
      instExprVarMap[CurInst] = sss;
    } else {
      *out << "\n";
      *out << "\nERROR: Unexpected MCExpr: '" << sss << "' \n";
      exit(-1);
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

    // If the expression starts with a relocation specifier, strip it and look
    // for the rest (variable in the Expr) in the instExprVarMap and globals.
    // Assuming there is only one relocation specifier, and it is at the
    // beginning (std::regex_constants::match_continuous).
    if (std::regex_search(sss, sm, re,
                          std::regex_constants::match_continuous)) {
      auto stringVar = sm.suffix();
      // Check the relocation specifiers to determine whether to store ptr
      // global value in the register or load the value from the global
      if (!sm.empty() && (sm[0] == ":lo12:")) {
        storePtr = false;
      }
      //  for (auto x:sm) { *out << x << " "; }
      //  *out << stringVar << "\n";
      if (!globals.contains(stringVar)) {
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

      auto glob = globals.find(stringVar);
      if (glob == globals.end()) {
        *out << "\nERROR: global not found\n\n";
        exit(-1);
      }
      globalVar = glob->second;
    } else {
      auto glob = globals.find(sss);
      if (glob == globals.end()) {
        *out << "\nERROR: global not found\n\n";
        exit(-1);
      }
      globalVar = glob->second;
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

  Value *getLowOnes(int ones, int w) {
    auto zero = getIntConst(0, ones);
    auto one = getIntConst(1, ones);
    auto minusOne = createSub(zero, one);
    return createZExt(minusOne, getIntTy(w));
  }

  Value *maskLower(Value *v, unsigned b) {
    auto w = getBitWidth(v);
    auto one = getIntConst(1, w);
    auto shifted = createRawShl(one, getIntConst(b, w));
    auto sub = createSub(shifted, one);
    return createAnd(v, sub);
  }

  // like LLVM's ExtractElement, but works on scalar values
  Value *extractFromVector(Value *val, unsigned eltWidth, unsigned lane) {
    unsigned w = getBitWidth(val);
    assert(w == 64 || w == 128);
    assert(lane >= 0);
    if (lane != 0) {
      auto shiftAmt = getIntConst(lane * eltWidth, w);
      val = createRawLShr(val, shiftAmt);
    }
    return createTrunc(val, getIntTy(eltWidth));
  }

  // like LLVM's InsertElement, but works on scalar values
  Value *insertIntoVector(Value *orig, Value *elt, unsigned eltWidth,
                          unsigned lane) {
    unsigned w = getBitWidth(orig);
    assert(w == 64 || w == 128);
    assert(eltWidth == 8 || eltWidth == 16 || eltWidth == 32 || eltWidth == 64);
    assert(getBitWidth(elt) == eltWidth);
    auto wTy = getIntTy(w);
    auto eltExt = createZExt(elt, wTy);
    auto shiftAmt = getIntConst(lane * eltWidth, w);
    auto shifted = createRawShl(eltExt, shiftAmt);
    auto mask = createRawShl(getLowOnes(eltWidth, w), shiftAmt);
    auto masked = createAnd(orig, createNot(mask));
    return createOr(masked, shifted);
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

  Value *rev64(Value *in, unsigned eltsize) {
    assert(eltsize == 8 || eltsize == 16 || eltsize == 32);
    assert(getBitWidth(in) == 128);
    Value *rev = getIntConst(0, 128);
    for (unsigned i = 0; i < 2; ++i) {
      auto innerCount = 64 / eltsize;
      for (unsigned j = 0; j < innerCount; j++) {
        auto elt = extractFromVector(in, eltsize, (i * innerCount) + j);
        rev = insertIntoVector(rev, elt, eltsize,
                               (i * innerCount) + innerCount - j - 1);
      }
    }
    return rev;
  }

  Value *dupElts(Value *v, unsigned elts, unsigned eltSize) {
    unsigned w = elts * eltSize;
    assert(w == 64 || w == 128);
    assert(getBitWidth(v) == eltSize);
    auto wTy = getIntTy(w);
    auto vext = createZExt(v, wTy);
    Value *ret = getIntConst(0, w);
    for (unsigned i = 0; i < elts; i++) {
      ret = createRawShl(ret, getIntConst(eltSize, w));
      ret = createOr(ret, vext);
    }
    return ret;
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
    // FIXME -- use our llvm.assert() idea, not llvm.assume() which is fragile
    auto *assert_decl =
        Intrinsic::getDeclaration(LiftedModule, Intrinsic::assume);
    CallInst::Create(assert_decl, {c}, "", LLVMBB);
  }

  Type *getFPOperandType(unsigned opcode) {
    auto size = getInstSize(opcode);
    Type *fTy;
    if (size == 32) {
      fTy = Type::getFloatTy(Ctx);
    } else if (size == 64) {
      fTy = Type::getDoubleTy(Ctx);
    } else {
      assert(false);
    }
    return fTy;
  }

  inline uint64_t Replicate(uint64_t bit, int N) {
    if (!bit)
      return 0;
    if (N == 64)
      return 0xffffffffffffffffLL;
    return (1ULL << N) - 1;
  }

  // this and the helper function above are from:
  // https://github.com/agustingianni/retools
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

  void doReturn() {
    auto i32 = getIntTy(32);
    auto i64 = getIntTy(64);

    if (false) {
      /*
       * ABI stuff: on all return paths, check that callee-saved +
       * other registers have been reset to their previous
       * values. these values were saved at the top of the function so
       * the trivially dominate all returns
       */
      // TODO: make sure code doesn't touch 16, 17?
      // check FP and LR?
      // z8-z23 are callee-saved
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

public:
  arm2llvm(Module *LiftedModule, MCFunction &MF, Function &srcFn,
           MCInstPrinter *instrPrinter, bool DebugRegs)
      : LiftedModule(LiftedModule), MF(MF), srcFn(srcFn),
        instrPrinter(instrPrinter), DebugRegs(DebugRegs),
        DL(srcFn.getParent()->getDataLayout()) {}

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

    // Make sure to not to trunc to the same size as the parameter.
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
    case AArch64::LDRBBroW:
    case AArch64::LDRBBroX:
      shiftAmt = 0;
      break;
    case AArch64::LDRHHroW:
    case AArch64::LDRHHroX:
      shiftAmt = shiftAmtVal ? 1 : 0;
      break;
    case AArch64::LDRWroW:
    case AArch64::LDRWroX:
      shiftAmt = shiftAmtVal ? 2 : 0;
      break;
    case AArch64::LDRXroW:
    case AArch64::LDRXroX:
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
    auto ptr = createGEP(getIntTy(8), base, {offset}, "");

    // Load Value val in the pointer returned by the GEP instruction
    return createLoad(getIntTy(8 * size), ptr);
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
      shiftAmt = 0;
      break;
    case AArch64::STRHHroW:
    case AArch64::STRHHroX:
      shiftAmt = shiftAmtVal ? 1 : 0;
      break;
    case AArch64::STRWroW:
    case AArch64::STRWroX:
      shiftAmt = shiftAmtVal ? 2 : 0;
      break;
    case AArch64::STRXroW:
    case AArch64::STRXroX:
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
    auto ptr = createGEP(getIntTy(8), base, {offset}, "");

    // Store Value val in the pointer returned by the GEP instruction
    createStore(val, ptr);
  }

  tuple<Value *, int, Value *> getParamsStoreImmed() {
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
    return make_tuple(baseAddr, op2.getImm(), readFromReg(op0.getReg()));
  }

  // Creates instructions to store val in memory pointed by base + offset
  // offset and size are in bytes
  void storeToMemoryImmOffset(Value *base, u_int64_t offset, u_int64_t size,
                              Value *val) {
    // Get offset as a 64-bit LLVM constant
    auto offsetVal = getIntConst(offset, 64);

    // Create a GEP instruction based on a byte addressing basis (8 bits)
    // returning pointer to base + offset
    auto ptr = createGEP(getIntTy(8), base, {offsetVal}, "");

    // Store Value val in the pointer returned by the GEP instruction
    createStore(val, ptr);
  }

  // Visit an MCInst and convert it to LLVM IR
  // See: https://documentation-service.arm.com/static/6245e8f0f7d10f7540e0c054
  void mc_visit(MCInst &I, Function &Fn) {
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
          if (reg != AArch64::WZR && reg != AArch64::XZR)
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
      auto ext = extractFromVector(readFromOperand(3), w, getImm(4));
      auto ins = insertIntoVector(readFromOperand(1), ext, w, getImm(2));
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
      auto inserted = insertIntoVector(orig, val, w, lane);
      updateOutputReg(inserted);
      break;
    }

    case AArch64::FMOVSWr:
    case AArch64::FMOVDXr:
    case AArch64::FMOVWSr:
    case AArch64::FMOVXDr:
    case AArch64::FMOVDr: {
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

    case AArch64::FABSSr:
    case AArch64::FABSDr: {
      auto fTy = getFPOperandType(opcode);
      auto a = createBitCast(readFromOperand(1), fTy);
      auto sizeTy = getIntTy(getInstSize(opcode));
      auto res = createBitCast(createFAbs(a), sizeTy);
      updateOutputReg(res);
      break;
    }

    case AArch64::FMULSrr:
    case AArch64::FMULDrr:
    case AArch64::FADDSrr:
    case AArch64::FADDDrr:
    case AArch64::FSUBSrr:
    case AArch64::FSUBDrr: {
      Instruction::BinaryOps op;
      if (opcode == AArch64::FADDSrr || opcode == AArch64::FADDDrr) {
        op = Instruction::FAdd;
      } else if (opcode == AArch64::FMULSrr || opcode == AArch64::FMULDrr) {
        op = Instruction::FMul;
      } else if (opcode == AArch64::FSUBSrr || opcode == AArch64::FSUBDrr) {
        op = Instruction::FSub;
      } else {
        assert(false && "missed a case");
      }

      auto fTy = getFPOperandType(opcode);
      auto a = createBitCast(readFromOperand(1), fTy);
      auto b = createBitCast(readFromOperand(2), fTy);

      auto sizeTy = getIntTy(getInstSize(opcode));
      auto res = createBitCast(createBinop(a, b, op), sizeTy);
      updateOutputReg(res);
      break;
    }

    case AArch64::FCVTZSUWDr: {
      auto val1 = createBitCast(readFromOperand(1), Type::getDoubleTy(Ctx));
      auto val2 = createConvertFPToSI(val1, getIntTy(32));
      updateOutputReg(val2);
      break;
    }

    case AArch64::FCVTZSUWSr: {
      auto val1 = createBitCast(readFromOperand(1), Type::getFloatTy(Ctx));
      auto val2 = createConvertFPToSI(val1, getIntTy(32));
      updateOutputReg(val2);
      break;
    }

    case AArch64::FCVTSHr: {
      auto val1 = createTrunc(readFromOperand(1), i16);
      auto val3 = createConvertFromFP16(val1, Type::getFloatTy(Ctx));
      auto val4 = createBitCast(val3, i32);
      updateOutputReg(val4);
      break;
    }

    case AArch64::FCMPSri:
    case AArch64::FCMPDri:
    case AArch64::FCMPSrr:
    case AArch64::FCMPDrr: {
      auto fTy = getFPOperandType(opcode);
      auto a = createBitCast(readFromOperand(0), fTy);
      Value *b;
      if (opcode == AArch64::FCMPSri || opcode == AArch64::FCMPDri) {
        b = ConstantFP::get(fTy, 0.0);
      } else {
        b = createBitCast(readFromOperand(1), fTy);
      }
      setN(createFCmp(FCmpInst::Predicate::FCMP_OLT, a, b));
      setZ(createFCmp(FCmpInst::Predicate::FCMP_OEQ, a, b));
      setC(createFCmp(FCmpInst::Predicate::FCMP_UGT, a, b));
      setV(createFCmp(FCmpInst::Predicate::FCMP_UNO, a, b));
      break;
    }

    case AArch64::SMOVvi16to32:
    case AArch64::SMOVvi16to32_idx0: {
      auto val = readFromOperand(1);
      auto imm = getImm(2);
      auto shiftAmt = getIntConst(imm * 16, 128);
      auto shifted = createRawLShr(val, shiftAmt);
      auto trunced = createTrunc(shifted, i16);
      auto sexted = createSExt(trunced, i32);
      updateOutputReg(sexted);
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
      auto val = readFromOperand(1);
      unsigned idx;
      if (opcode == AArch64::UMOVvi8_idx0 || opcode == AArch64::UMOVvi16_idx0) {
        idx = 0;
      } else {
        idx = getImm(2);
      }
      if (idx != 0) {
        auto shiftAmt = getIntConst(idx * sz, 128);
        val = createRawLShr(val, shiftAmt);
      }
      auto masked = maskLower(val, sz);
      updateOutputReg(createTrunc(masked, i64));
      break;
    }

    case AArch64::MOVIv16b_ns: {
      auto v = getIntConst(getImm(1), 8);
      updateOutputReg(dupElts(v, 16, 8));
      break;
    }

    case AArch64::MOVID:
    case AArch64::MOVIv2d_ns: {
      auto imm = getIntConst(replicate8to64(getImm(1)), 64);
      updateOutputReg(dupElts(imm, 2, 64));
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
      auto v = rev64(readFromOperand(1), 32);
      updateOutputReg(v);
      break;
    }

    case AArch64::DUPi16: {
      auto ext = extractFromVector(readFromOperand(1), 16, getImm(2));
      updateOutputReg(ext);
      break;
    }

    case AArch64::DUPi32: {
      auto ext = extractFromVector(readFromOperand(1), 32, getImm(2));
      updateOutputReg(ext);
      break;
    }

    case AArch64::DUPi64: {
      auto ext = extractFromVector(readFromOperand(1), 64, getImm(2));
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
      auto ext = extractFromVector(readFromOperand(1), 32, getImm(2));
      updateOutputReg(dupElts(ext, 2, 32));
      break;
    }

    case AArch64::DUPv2i64lane: {
      auto ext = extractFromVector(readFromOperand(1), 64, getImm(2));
      updateOutputReg(dupElts(ext, 2, 64));
      break;
    }

    case AArch64::DUPv4i16lane: {
      auto ext = extractFromVector(readFromOperand(1), 16, getImm(2));
      updateOutputReg(dupElts(ext, 4, 16));
      break;
    }

    case AArch64::DUPv4i32lane: {
      auto ext = extractFromVector(readFromOperand(1), 32, getImm(2));
      updateOutputReg(dupElts(ext, 4, 32));
      break;
    }

    case AArch64::DUPv8i8lane: {
      auto ext = extractFromVector(readFromOperand(1), 8, getImm(2));
      updateOutputReg(dupElts(ext, 8, 8));
      break;
    }

    case AArch64::DUPv8i16lane: {
      auto ext = extractFromVector(readFromOperand(1), 16, getImm(2));
      updateOutputReg(dupElts(ext, 8, 16));
      break;
    }

    case AArch64::DUPv16i8lane: {
      auto ext = extractFromVector(readFromOperand(1), 8, getImm(2));
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

    case AArch64::BSLv8i8:
    case AArch64::BSLv16i8: {
      auto op1 = readFromOperand(3);
      auto op4 = readFromOperand(2);
      auto op3 = readFromOperand(1);
      auto res = createXor(op1, createAnd(createXor(op1, op4), op3));
      updateOutputReg(res);
      break;
    }

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
    case AArch64::CMHIv8i8:
    case AArch64::CMHIv4i16:
    case AArch64::CMHIv2i32:
    case AArch64::CMHIv1i64:
    case AArch64::CMHIv16i8:
    case AArch64::CMHIv8i16:
    case AArch64::CMHIv4i32:
    case AArch64::CMHIv2i64:
    case AArch64::ADDv2i32:
    case AArch64::ADDv2i64:
    case AArch64::ADDv4i16:
    case AArch64::ADDv4i32:
    case AArch64::ADDv8i8:
    case AArch64::ADDv8i16:
    case AArch64::ADDv16i8:
    case AArch64::UADDLv8i8_v8i16:
    case AArch64::SUBv2i32:
    case AArch64::SUBv2i64:
    case AArch64::SUBv4i16:
    case AArch64::SUBv4i32:
    case AArch64::SUBv8i8:
    case AArch64::SUBv8i16:
    case AArch64::SUBv16i8:
    case AArch64::USUBLv8i8_v8i16:
    case AArch64::EORv8i8:
    case AArch64::EORv16i8:
    case AArch64::ANDv8i8:
    case AArch64::ANDv16i8:
    case AArch64::ORRv8i8:
    case AArch64::ORRv16i8:
    case AArch64::UMULLv2i32_v2i64:
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
    case AArch64::USHLLv8i8_shift:
    case AArch64::USHLLv4i32_shift:
    case AArch64::USHLLv8i16_shift:
    case AArch64::USHLLv16i8_shift:
    case AArch64::USHLLv4i16_shift:
    case AArch64::USHLLv2i32_shift:
    case AArch64::USHRv2i64_shift: {
      auto a = readFromOperand(1);
      auto b = readFromOperand(2);
      bool elementWise = false;
      bool isICmp = false;
      bool splatImm2 = false;
      bool zext = false;
      bool immShift = false;
      function<Value *(Value *, Value *)> op;
      switch (opcode) {
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
      case AArch64::CMHIv8i8:
      case AArch64::CMHIv4i16:
      case AArch64::CMHIv2i32:
      case AArch64::CMHIv1i64:
      case AArch64::CMHIv16i8:
      case AArch64::CMHIv8i16:
      case AArch64::CMHIv4i32:
      case AArch64::CMHIv2i64:
        op = [&](Value *a, Value *b) {
          return createICmp(ICmpInst::Predicate::ICMP_UGT, a, b);
        };
        isICmp = true;
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
      case AArch64::ADDv2i64:
      case AArch64::ADDv4i16:
      case AArch64::ADDv4i32:
      case AArch64::ADDv8i8:
      case AArch64::ADDv8i16:
      case AArch64::ADDv16i8:
        op = [&](Value *a, Value *b) { return createAdd(a, b); };
        break;
      case AArch64::UADDLv8i8_v8i16:
        zext = true;
        op = [&](Value *a, Value *b) { return createAdd(a, b); };
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
      case AArch64::USUBLv8i8_v8i16:
        zext = true;
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
      case AArch64::ORRv8i8:
      case AArch64::ORRv16i8:
        op = [&](Value *a, Value *b) { return createOr(a, b); };
        break;
      case AArch64::UMULLv2i32_v2i64:
        zext = true;
        op = [&](Value *a, Value *b) { return createMul(a, b); };
        break;
      case AArch64::USHLLv4i32_shift:
      case AArch64::USHLLv8i16_shift:
      case AArch64::USHLLv16i8_shift:
        // OK this is a little weird. at this level, the distinction
        // between ushll and ushll2 is that the former specifies a
        // 64-bit operand and the latter specifies a 128-bit operand
        // and then the shift is implied.
        a = createRawLShr(a, getIntConst(64, 128));
      case AArch64::USHLLv4i16_shift:
      case AArch64::USHLLv2i32_shift:
      case AArch64::USHLLv8i8_shift:
        zext = true;
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
      case AArch64::CMHIv1i64:
      case AArch64::USHLv1i64:
      case AArch64::SSHLv1i64:
        numElts = 1;
        eltSize = 64;
        break;
      case AArch64::SSHRv2i32_shift:
      case AArch64::SHLv2i32_shift:
      case AArch64::SUBv2i32:
      case AArch64::ADDv2i32:
      case AArch64::USHLv2i32:
      case AArch64::CMHIv2i32:
      case AArch64::SSHLv2i32:
      case AArch64::BICv2i32:
      case AArch64::USHLLv2i32_shift:
        numElts = 2;
        eltSize = 32;
        break;
      case AArch64::SSHRv2i64_shift:
      case AArch64::SHLv2i64_shift:
      case AArch64::ADDv2i64:
      case AArch64::SUBv2i64:
      case AArch64::USHLv2i64:
      case AArch64::CMHIv2i64:
      case AArch64::SSHLv2i64:
        numElts = 2;
        eltSize = 64;
        break;
      case AArch64::USHRv2i64_shift:
        numElts = 2;
        eltSize = 64;
        break;
      case AArch64::SSHRv4i16_shift:
      case AArch64::ADDv4i16:
      case AArch64::SUBv4i16:
      case AArch64::USHLv4i16:
      case AArch64::CMHIv4i16:
      case AArch64::SSHLv4i16:
      case AArch64::BICv4i16:
      case AArch64::USHLLv4i16_shift:
      case AArch64::SHLv4i16_shift:
        numElts = 4;
        eltSize = 16;
        break;
      case AArch64::SSHRv4i32_shift:
      case AArch64::SHLv4i32_shift:
      case AArch64::ADDv4i32:
      case AArch64::SUBv4i32:
      case AArch64::USHLv4i32:
      case AArch64::CMHIv4i32:
      case AArch64::SSHLv4i32:
      case AArch64::BICv4i32:
      case AArch64::USHLLv4i32_shift:
        numElts = 4;
        eltSize = 32;
        break;
      case AArch64::SSHRv8i8_shift:
      case AArch64::SHLv8i8_shift:
      case AArch64::ADDv8i8:
      case AArch64::SUBv8i8:
      case AArch64::EORv8i8:
      case AArch64::ANDv8i8:
      case AArch64::ORRv8i8:
      case AArch64::USHLv8i8:
      case AArch64::CMHIv8i8:
      case AArch64::SSHLv8i8:
      case AArch64::BICv8i8:
        numElts = 8;
        eltSize = 8;
        break;
      case AArch64::ADDv8i16:
      case AArch64::SUBv8i16:
      case AArch64::USHLv8i16:
      case AArch64::CMHIv8i16:
      case AArch64::SSHLv8i16:
      case AArch64::BICv8i16:
      case AArch64::USHLLv8i16_shift:
      case AArch64::SHLv8i16_shift:
      case AArch64::SSHRv8i16_shift:
        numElts = 8;
        eltSize = 16;
        break;
      case AArch64::ADDv16i8:
      case AArch64::SUBv16i8:
      case AArch64::EORv16i8:
      case AArch64::ANDv16i8:
      case AArch64::ORRv16i8:
      case AArch64::USHLv16i8:
      case AArch64::CMHIv16i8:
      case AArch64::SSHLv16i8:
      case AArch64::BICv16i8:
      case AArch64::USHLLv16i8_shift:
      case AArch64::SHLv16i8_shift:
      case AArch64::SSHRv16i8_shift:
        numElts = 16;
        eltSize = 8;
        break;
      case AArch64::UMULLv2i32_v2i64:
        numElts = 2;
        eltSize = 32;
        break;
      case AArch64::USHLLv8i8_shift:
        numElts = 8;
        eltSize = 8;
        break;
      case AArch64::UADDLv8i8_v8i16:
      case AArch64::USUBLv8i8_v8i16:
        numElts = 8;
        eltSize = 8;
        break;
      default:
        assert(false && "missed case");
        break;
      }

      auto res = createVectorOp(op, a, b, eltSize, numElts, elementWise, zext,
                                isICmp, splatImm2, immShift);
      updateOutputReg(res);
      break;
    }

    case AArch64::MLSv2i32: {
      auto accum = readFromOperand(1);
      auto a = readFromOperand(2);
      auto b = readFromOperand(3);
      auto v1 = createVectorOp(
          [&](Value *a, Value *b) { return createMul(a, b); }, a, b, 32, 2,
          /*cast=*/false, /*zext=*/false, /*isICmp=*/false,
          /*splatImm2=*/false, /*immShift=*/false);
      auto v2 = createVectorOp(
          [&](Value *a, Value *b) { return createSub(a, b); }, accum, v1, 32, 2,
          /*cast=*/false, /*zext=*/false, /*isICmp=*/false,
          /*splatImm2=*/false, /*immShift=*/false);
      updateOutputReg(v2);
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
        auto imm = decodeLogicalImmediate(getImm(2), size);
        rhs = getIntConst(imm, size);
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
        return;
      }

      // SXTB
      if (immr == 0 && imms == 7) {
        auto trunc = createTrunc(src, i8);
        auto dst = createSExt(trunc, ty);
        updateOutputReg(dst);
        return;
      }

      // SXTH
      if (immr == 0 && imms == 15) {
        auto trunc = createTrunc(src, i16);
        auto dst = createSExt(trunc, ty);
        updateOutputReg(dst);
        return;
      }

      // SXTW
      if (immr == 0 && imms == 31) {
        auto trunc = createTrunc(src, i32);
        auto dst = createSExt(trunc, ty);
        updateOutputReg(dst);
        return;
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
        return;
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
      return;
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
      auto decoded_immediate = decodeLogicalImmediate(getImm(2), size);
      auto imm_val = getIntConst(decoded_immediate,
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
        return;
      }

      // LSL is preferred when imms != 63 and imms + 1 == immr
      if (size == 64 && imms != 63 && imms + 1 == immr) {
        auto dst = createMaskedShl(src, getIntConst(63 - imms, size));
        updateOutputReg(dst);
        return;
      }

      // LSR is preferred when imms == 31 or 63 (size - 1)
      if (imms == size - 1) {
        auto dst = createMaskedLShr(src, getIntConst(immr, size));
        updateOutputReg(dst);
        return;
      }

      // UBFIZ
      if (imms < immr) {
        auto pos = size - immr;
        auto width = imms + 1;
        auto mask = ((uint64_t)1 << (width)) - 1;
        auto masked = createAnd(src, getIntConst(mask, size));
        auto shifted = createMaskedShl(masked, getIntConst(pos, size));
        updateOutputReg(shifted);
        return;
      }

      // UXTB
      if (immr == 0 && imms == 7) {
        auto mask = ((uint64_t)1 << 8) - 1;
        auto masked = createAnd(src, getIntConst(mask, size));
        updateOutputReg(masked);
        return;
      }

      // UXTH
      if (immr == 0 && imms == 15) {
        auto mask = ((uint64_t)1 << 16) - 1;
        auto masked = createAnd(src, getIntConst(mask, size));
        updateOutputReg(masked);
        return;
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
      return;
    }

    case AArch64::BFMWri:
    case AArch64::BFMXri: {
      auto size = getInstSize(opcode);
      auto dst = readFromOperand(1);
      auto src = readFromOperand(2);

      auto immr = getImm(3);
      auto imms = getImm(4);

      if (imms >= immr) {
        auto bits = (imms - immr + 1);
        auto pos = immr;

        auto mask = (((uint64_t)1 << bits) - 1) << pos;

        auto masked = createAnd(src, getIntConst(mask, size));
        auto shifted = createMaskedLShr(masked, getIntConst(pos, size));
        auto cleared =
            createAnd(dst, getIntConst((uint64_t)(-1) << bits, size));
        auto res = createOr(cleared, shifted);
        updateOutputReg(res);
        return;
      }

      auto bits = imms + 1;
      auto pos = size - immr;

      // This mask deletes `bits` number of bits starting at `pos`.
      // If the mask is for a 32 bit value, it will chop off the top 32 bits of
      // the 64 bit mask to keep the mask to a size of 32 bits
      auto mask =
          ~((((uint64_t)1 << bits) - 1) << pos) & ((uint64_t)-1 >> (64 - size));

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
      return;
    }

    case AArch64::ORRWri:
    case AArch64::ORRXri: {
      auto size = getInstSize(opcode);
      auto lhs = readFromOperand(1);
      auto imm = getImm(2);
      auto decoded = decodeLogicalImmediate(imm, size);
      auto result = createOr(lhs, getIntConst(decoded, size));
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
      auto DivBB = BasicBlock::Create(Ctx, "", &Fn);
      auto ContBB = BasicBlock::Create(Ctx, "", &Fn);
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
      auto DivBB = BasicBlock::Create(Ctx, "", &Fn);
      auto ContBB = BasicBlock::Create(Ctx, "", &Fn);
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

      /*
       * TODO: there's a lot of redundancy across memory operations
       * here, some serious refactoring is needed
       */

    case AArch64::LDRSBXui:
    case AArch64::LDRSBWui:
    case AArch64::LDRSHXui:
    case AArch64::LDRSHWui:
    case AArch64::LDRSWui: {
      auto [base, imm] = getParamsLoadImmed();

      unsigned size;
      if (opcode == AArch64::LDRSBXui || opcode == AArch64::LDRSBWui)
        size = 1;
      else if (opcode == AArch64::LDRSHXui || opcode == AArch64::LDRSHWui)
        size = 2;
      else if (opcode == AArch64::LDRSWui)
        size = 4;
      else
        assert(false);

      auto loaded = makeLoadWithOffset(base, imm * size, size);
      updateOutputReg(loaded, /*SExt=*/true);
      break;
    }

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
      if (opcode == AArch64::LDRBBui || opcode == AArch64::LDRBui)
        size = 1;
      else if (opcode == AArch64::LDRHHui || opcode == AArch64::LDRHui)
        size = 2;
      else if (opcode == AArch64::LDRWui || opcode == AArch64::LDRSui)
        size = 4;
      else if (opcode == AArch64::LDRXui || opcode == AArch64::LDRDui)
        size = 8;
      else if (opcode == AArch64::LDRQui)
        size = 16;
      else
        assert(false);

      MCOperand &op2 = CurInst->getOperand(2);
      if (op2.isExpr()) {
        auto [globalVar, storePtr] = getExprVar(op2.getExpr());
        if (!storePtr) {
          auto loaded = makeLoadWithOffset(globalVar, 0, size);
          updateOutputReg(loaded);
        } else {
          Value *ptrToInt = createPtrToInt(globalVar, getIntTy(size * 8));
          updateOutputReg(ptrToInt);
        }
      } else {
        auto [base, imm] = getParamsLoadImmed();
        auto loaded = makeLoadWithOffset(base, imm * size, size);
        updateOutputReg(loaded);
      }
      break;
    }
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
      if (opcode == AArch64::LDURBBi || opcode == AArch64::LDURBi)
        size = 1;
      else if (opcode == AArch64::LDURHHi || opcode == AArch64::LDURHi)
        size = 2;
      else if (opcode == AArch64::LDURWi || opcode == AArch64::LDURSi)
        size = 4;
      else if (opcode == AArch64::LDURXi || opcode == AArch64::LDURDi)
        size = 8;
      else if (opcode == AArch64::LDURQi)
        size = 16;
      else
        assert(false);

      auto [base, imm] = getParamsLoadImmed();
      auto loaded = makeLoadWithOffset(base, imm, size);
      updateOutputReg(loaded);
      break;
    }
    case AArch64::LDRXpost: {
      unsigned size = 8;
      auto &op0 = CurInst->getOperand(0);
      auto &op1 = CurInst->getOperand(1);
      auto &op2 = CurInst->getOperand(2);
      auto &op3 = CurInst->getOperand(3);
      assert(op0.isReg() && op1.isReg() && op2.isReg());
      assert(op0.getReg() == op2.getReg());
      assert(op3.isImm());

      // loaded value is going to end up in this register
      auto destReg = op1.getReg();

      // this register points to the value to be loaded, and it also
      // gets post-updated
      auto ptrReg = op0.getReg();
      auto addr = readPtrFromReg(ptrReg);
      auto imm = op3.getImm();
      auto loaded = makeLoadWithOffset(addr, imm * size, size);
      updateReg(loaded, destReg);
      auto offsetVal = getIntConst(imm, 64);
      auto toUpdate = readFromReg(ptrReg);
      auto added = createAdd(toUpdate, offsetVal);
      updateReg(added, ptrReg);
      break;
    }
    case AArch64::LDRBBroW:
    case AArch64::LDRBBroX:
    case AArch64::LDRHHroW:
    case AArch64::LDRHHroX:
    case AArch64::LDRWroW:
    case AArch64::LDRWroX:
    case AArch64::LDRXroW:
    case AArch64::LDRXroX:
    case AArch64::LDRQroX: {
      unsigned size;

      switch (opcode) {
      case AArch64::LDRBBroW:
      case AArch64::LDRBBroX:
        size = 1;
        break;
      case AArch64::LDRHHroW:
      case AArch64::LDRHHroX:
        size = 2;
        break;
      case AArch64::LDRWroX:
      case AArch64::LDRWroW:
        size = 4;
        break;
      case AArch64::LDRXroW:
      case AArch64::LDRXroX:
        size = 8;
        break;
      case AArch64::LDRQroW:
      case AArch64::LDRQroX:
        size = 16;
        break;
      default:
        *out << "\nError Unknown opcode\n";
        visitError();
      }

      auto [base, offset] = getParamsLoadReg();
      auto loaded = makeLoadWithOffset(base, offset, size);
      updateOutputReg(loaded);
      break;
    }
    case AArch64::STRBBui: {
      auto [base, imm, val] = getParamsStoreImmed();
      storeToMemoryImmOffset(base, imm * 1, 1, createTrunc(val, i8));
      break;
    }
    case AArch64::STRHHui:
    case AArch64::STRHui: {
      auto [base, imm, val] = getParamsStoreImmed();
      storeToMemoryImmOffset(base, imm * 2, 2, createTrunc(val, i16));
      break;
    }
    case AArch64::STURWi: {
      auto [base, imm, val] = getParamsStoreImmed();
      storeToMemoryImmOffset(base, imm * 1, 4, createTrunc(val, i32));
      break;
    }
    case AArch64::STURXi: {
      auto [base, imm, val] = getParamsStoreImmed();
      storeToMemoryImmOffset(base, imm * 1, 8, val);
      break;
    }
    case AArch64::STRWui:
    case AArch64::STRSui: {
      auto [base, imm, val] = getParamsStoreImmed();
      storeToMemoryImmOffset(base, imm * 4, 4, createTrunc(val, i32));
      break;
    }
    case AArch64::STRXui: {
      auto [base, imm, val] = getParamsStoreImmed();
      storeToMemoryImmOffset(base, imm * 8, 8, val);
      break;
    }
    case AArch64::STRDui: {
      auto [base, imm, val] = getParamsStoreImmed();
      storeToMemoryImmOffset(base, imm * 8, 8, createTrunc(val, i64));
      break;
    }
    case AArch64::STRQui: {
      auto [base, imm, val] = getParamsStoreImmed();
      storeToMemoryImmOffset(base, imm * 16, 16, val);
      break;
    }

    case AArch64::STRXpre:
    case AArch64::STRWpre: {
      auto &op0 = CurInst->getOperand(0);
      auto &op1 = CurInst->getOperand(1);
      auto &op2 = CurInst->getOperand(2);
      auto &op3 = CurInst->getOperand(3);
      assert(op0.isReg() && op1.isReg() && op2.isReg());
      assert(op0.getReg() == op2.getReg());
      assert(op3.isImm());

      auto baseReg = op2.getReg();
      assert((baseReg >= AArch64::X0 && baseReg <= AArch64::X28) ||
             (baseReg == AArch64::SP) || (baseReg == AArch64::LR) ||
             (baseReg == AArch64::FP) || (baseReg == AArch64::XZR));

      auto valToStore = readFromReg(op1.getReg());
      auto offsetVal = getIntConst(op3.getImm(), 64);
      auto basePtrInt = readFromReg(baseReg);
      auto newPtrAddr = createAdd(basePtrInt, offsetVal);
      updateReg(newPtrAddr, baseReg);
      auto shiftedPtr = readPtrFromReg(baseReg);

      if (getInstSize(opcode) == 32)
        valToStore = createTrunc(valToStore, i32);

      storeToMemoryImmOffset(shiftedPtr, 0, 8, valToStore);
      break;
    }
    case AArch64::STRBBroW:
    case AArch64::STRBBroX:
    case AArch64::STRHHroW:
    case AArch64::STRHHroX:
    case AArch64::STRWroW:
    case AArch64::STRWroX:
    case AArch64::STRXroW:
    case AArch64::STRXroX:
    case AArch64::STRQroX: {
      auto [base, offset, val] = getParamsStoreReg();

      switch (opcode) {
      case AArch64::STRBBroW:
      case AArch64::STRBBroX:
        storeToMemoryValOffset(base, offset, 1, createTrunc(val, i8));
        break;
      case AArch64::STRHHroW:
      case AArch64::STRHHroX:
        storeToMemoryValOffset(base, offset, 2, createTrunc(val, i16));
        break;
      case AArch64::STRWroW:
      case AArch64::STRWroX:
        storeToMemoryValOffset(base, offset, 4, createTrunc(val, i32));
        break;
      case AArch64::STRXroW:
      case AArch64::STRXroX:
        storeToMemoryValOffset(base, offset, 8, val);
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

    case AArch64::LDPWi:
    case AArch64::LDPXi:
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
             (baseReg == AArch64::XZR));
      auto baseAddr = readPtrFromReg(baseReg);

      int size = 0;
      switch (opcode) {
      case AArch64::LDPWi: {
        size = 4;
        break;
      }
      case AArch64::LDPXi: {
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

      auto imm = op3.getImm();
      auto out1 = op0.getReg();
      auto out2 = op1.getReg();
      if (out1 != AArch64::XZR && out1 != AArch64::WZR) {
        auto loaded = makeLoadWithOffset(baseAddr, imm * size, size);
        updateReg(loaded, out1);
      }
      if (out2 != AArch64::XZR && out2 != AArch64::WZR) {
        auto loaded = makeLoadWithOffset(baseAddr, (imm + 1) * size, size);
        updateReg(loaded, out2);
      }
      break;
    }

    case AArch64::STPWi:
    case AArch64::STPXi:
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
      case AArch64::STPWi: {
        size = 4;
        val1 = createTrunc(val1, i32);
        val2 = createTrunc(val2, i32);
        break;
      }
      case AArch64::STPXi: {
        size = 8;
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

    case AArch64::LDPXpost: {
      unsigned size = 8;
      auto &op0 = CurInst->getOperand(0);
      auto &op1 = CurInst->getOperand(1);
      auto &op2 = CurInst->getOperand(2);
      auto &op3 = CurInst->getOperand(3);
      auto &op4 = CurInst->getOperand(4);
      assert(op0.isReg() && op1.isReg() && op2.isReg() && op3.isReg());
      assert(op0.getReg() == op3.getReg());
      assert(op4.isImm());

      auto baseReg = op0.getReg();
      assert((baseReg >= AArch64::X0 && baseReg <= AArch64::X28) ||
             (baseReg == AArch64::SP) || (baseReg == AArch64::LR) ||
             (baseReg == AArch64::FP));
      auto addr = readPtrFromReg(baseReg);
      auto r1 = op1.getReg();
      auto r2 = op2.getReg();
      auto loaded1 = makeLoadWithOffset(addr, 0, size);
      auto loaded2 = makeLoadWithOffset(addr, size, size);
      updateReg(loaded1, r1);
      updateReg(loaded2, r2);

      auto offsetVal = getIntConst(op4.getImm(), 64);
      auto basePtrInt = readFromReg(baseReg);
      auto newPtrAddr = createAdd(basePtrInt, offsetVal);
      updateReg(newPtrAddr, baseReg);
      break;
    }

    case AArch64::STPXpre: {
      auto &op0 = CurInst->getOperand(0);
      auto &op1 = CurInst->getOperand(1);
      auto &op2 = CurInst->getOperand(2);
      auto &op3 = CurInst->getOperand(3);
      auto &op4 = CurInst->getOperand(4);
      assert(op0.isReg() && op1.isReg() && op2.isReg() && op3.isReg());
      assert(op0.getReg() == op3.getReg());
      assert(op4.isImm());

      auto baseReg = op0.getReg();
      assert((baseReg >= AArch64::X0 && baseReg <= AArch64::X28) ||
             (baseReg == AArch64::SP) || (baseReg == AArch64::LR) ||
             (baseReg == AArch64::FP));
      auto baseAddr = readPtrFromReg(baseReg);
      auto val1 = readFromReg(op1.getReg());
      auto val2 = readFromReg(op2.getReg());

      auto offsetVal = getIntConst(op4.getImm(), 64);
      auto basePtrInt = readFromReg(baseReg);
      auto newPtrAddr = createAdd(basePtrInt, offsetVal);
      updateReg(newPtrAddr, baseReg);

      auto imm = op4.getImm();
      unsigned size = 8;
      storeToMemoryImmOffset(baseAddr, imm * size, size, val1);
      storeToMemoryImmOffset(baseAddr, (imm + 1) * size, size, val2);
      break;
    }

    case AArch64::ADRP: {
      assert(CurInst->getOperand(0).isReg());
      mapExprVar(CurInst->getOperand(1).getExpr());
      break;
    }

    case AArch64::RET:
      doReturn();
      break;

    case AArch64::B: {
      const auto &op = CurInst->getOperand(0);
      if (op.isImm()) {
        // handles the case when we add an entry block with no predecessors
        auto &dst_name = MF.BBs[getImm(0)].getName();
        auto BB = getBBByName(Fn, dst_name);
        createBranch(BB);
        break;
      }

      auto dst_ptr = getBB(Fn, CurInst->getOperand(0));
      createBranch(dst_ptr);
      break;
    }

    case AArch64::Bcc: {
      auto cond_val_imm = getImm(0);
      auto cond_val = conditionHolds(cond_val_imm);

      auto &jmp_tgt_op = CurInst->getOperand(1);
      assert(jmp_tgt_op.isExpr() && "expected expression");
      assert((jmp_tgt_op.getExpr()->getKind() == MCExpr::ExprKind::SymbolRef) &&
             "expected symbol ref as bcc operand");
      const MCSymbolRefExpr &SRE = cast<MCSymbolRefExpr>(*jmp_tgt_op.getExpr());
      const MCSymbol &Sym = SRE.getSymbol();

      auto *dst_true = getBBByName(Fn, Sym.getName());

      assert(MCBB->getSuccs().size() == 1 || MCBB->getSuccs().size() == 2);
      const string *dst_false_name = nullptr;
      for (auto &succ : MCBB->getSuccs()) {
        if (succ->getName() != Sym.getName()) {
          dst_false_name = &succ->getName();
          break;
        }
      }
      auto *dst_false =
          getBBByName(Fn, dst_false_name ? *dst_false_name : Sym.getName());

      createBranch(cond_val, dst_true, dst_false);
      break;
    }

    case AArch64::CBZW:
    case AArch64::CBZX: {
      auto operand = readFromOperand(0);
      assert(operand != nullptr && "operand is null");
      auto cond_val = createICmp(ICmpInst::Predicate::ICMP_EQ, operand,
                                 getIntConst(0, getBitWidth(operand)));
      auto dst_true = getBB(Fn, CurInst->getOperand(1));
      assert(MCBB->getSuccs().size() == 2 && "expected 2 successors");

      const string *dst_false_name = nullptr;
      for (auto &succ : MCBB->getSuccs()) {
        if (succ->getName() != dst_true->getName()) {
          dst_false_name = &succ->getName();
          break;
        }
      }
      assert(dst_false_name != nullptr);
      auto *dst_false = getBBByName(Fn, *dst_false_name);
      createBranch(cond_val, dst_true, dst_false);
      break;
    }

    case AArch64::CBNZW:
    case AArch64::CBNZX: {
      auto operand = readFromOperand(0);
      assert(operand != nullptr && "operand is null");
      auto cond_val = createICmp(ICmpInst::Predicate::ICMP_NE, operand,
                                 getIntConst(0, getBitWidth(operand)));

      auto dst_true = getBB(Fn, CurInst->getOperand(1));
      assert(MCBB->getSuccs().size() == 2 && "expected 2 successors");

      const string *dst_false_name = nullptr;
      for (auto &succ : MCBB->getSuccs()) {
        if (succ->getName() != dst_true->getName()) {
          dst_false_name = &succ->getName();
          break;
        }
      }
      assert(dst_false_name != nullptr);
      auto *dst_false = getBBByName(Fn, *dst_false_name);
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
      auto *dst_false = getBBByName(Fn, Sym.getName());

      assert(MCBB->getSuccs().size() == 1 || MCBB->getSuccs().size() == 2);

      const string *dst_true_name = nullptr;
      for (auto &succ : MCBB->getSuccs()) {
        if (succ->getName() != Sym.getName()) {
          dst_true_name = &succ->getName();
          break;
        }
      }
      auto *dst_true =
          getBBByName(Fn, dst_true_name ? *dst_true_name : Sym.getName());

      if (opcode == AArch64::TBNZW || opcode == AArch64::TBNZX)
        createBranch(cond_val, dst_false, dst_true);
      else
        createBranch(cond_val, dst_true, dst_false);
      break;
    }

    case AArch64::XTNv2i32:
    case AArch64::XTNv4i16:
    case AArch64::XTNv8i8: {
      auto &op0 = CurInst->getOperand(0);
      auto &op1 = CurInst->getOperand(1);
      assert(isSIMDandFPReg(op0) && isSIMDandFPReg(op0));

      // The source value is always a vector with total width of 128 bits
      // The destination value is always a vector with total of 64 bits but we
      // need to read the full 128 bit SIMD&FP register and change the value
      // appropriately
      Value *src = readFromReg(op1.getReg());
      assert(getBitWidth(src) == 128 &&
             "Source value is not a vector with 128 bits");

      // eltSize is in bits
      u_int64_t eltSize, numElts, part;
      switch (opcode) {
      case AArch64::XTNv8i8:
        numElts = 8;
        eltSize = 8;
        part = 0;
        break;
      case AArch64::XTNv4i16:
        numElts = 4;
        eltSize = 16;
        part = 0;
        break;
      case AArch64::XTNv2i32:
        numElts = 2;
        eltSize = 32;
        part = 0;
        break;
      default:
        *out << "\nError Unknown opcode\n";
        visitError();
        break;
      }

      // BitCast src to a vector of numElts x (2*eltSize)
      assert(numElts * (2 * eltSize) == 128 && "BitCasting to wrong type");
      Value *src_vector =
          createBitCast(src, VectorType::get(getIntTy(2 * eltSize),
                                             ElementCount::getFixed(numElts)));
      Value *new_dest_vector = createTrunc(
          src_vector,
          VectorType::get(getIntTy(eltSize), ElementCount::getFixed(numElts)));

      if (part == 1) {
        // Have to preserve the lower 64 bits so, read from register and insert
        // to the upper 64 bits
        Value *dest = readFromReg(op0.getReg());
        Value *original_dest_vector = createBitCast(
            dest, VectorType::get(getIntTy(64), ElementCount::getFixed(2)));

        new_dest_vector = createInsertElement(
            original_dest_vector, new_dest_vector, getIntConst(1, 32));
      }

      // Write 64 bit or 128 bit register to output
      // 64 bit will be zero-extended to 128 bit by this function
      updateOutputReg(new_dest_vector);
      break;
    }

    case AArch64::UADALPv8i8_v4i16:
    case AArch64::UADALPv4i16_v2i32:
    case AArch64::UADALPv4i32_v2i64:
    case AArch64::UADALPv16i8_v8i16:
    case AArch64::UADALPv8i16_v4i32:
    case AArch64::UADDLPv4i16_v2i32:
    case AArch64::UADDLPv2i32_v1i64:
    case AArch64::UADDLPv8i8_v4i16:
    case AArch64::UADDLPv8i16_v4i32:
    case AArch64::UADDLPv4i32_v2i64:
    case AArch64::UADDLPv16i8_v8i16:
    case AArch64::UADDLVv8i16v:
    case AArch64::UADDLVv4i32v:
    case AArch64::UADDLVv8i8v:
    case AArch64::UADDLVv4i16v:
    case AArch64::UADDLVv16i8v:
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
    case AArch64::ADDVv4i16v: {
      auto src = readFromOperand(1);
      u_int64_t eltSize, numElts;

      switch (opcode) {
      case AArch64::NEGv1i64:
        eltSize = 64;
        numElts = 1;
        break;
      case AArch64::NEGv4i16:
      case AArch64::UADDLVv4i16v:
      case AArch64::UADDLPv4i16_v2i32:
      case AArch64::UADALPv4i16_v2i32:
      case AArch64::ADDVv4i16v:
        eltSize = 16;
        numElts = 4;
        break;
      case AArch64::NEGv2i32:
      case AArch64::UADDLPv2i32_v1i64:
      case AArch64::UADALPv2i32_v1i64:
        eltSize = 32;
        numElts = 2;
        break;
      case AArch64::NEGv8i16:
      case AArch64::UADDLVv8i16v:
      case AArch64::UADDLPv8i16_v4i32:
      case AArch64::UADALPv8i16_v4i32:
      case AArch64::ADDVv8i16v:
        eltSize = 16;
        numElts = 8;
        break;
      case AArch64::NEGv2i64:
        eltSize = 64;
        numElts = 2;
        break;
      case AArch64::NEGv4i32:
      case AArch64::UADDLVv4i32v:
      case AArch64::UADDLPv4i32_v2i64:
      case AArch64::UADALPv4i32_v2i64:
      case AArch64::ADDVv4i32v:
        eltSize = 32;
        numElts = 4;
        break;
      case AArch64::UADDLVv8i8v:
      case AArch64::UADDLPv8i8_v4i16:
      case AArch64::UADALPv8i8_v4i16:
      case AArch64::NEGv8i8:
      case AArch64::NOTv8i8:
      case AArch64::CNTv8i8:
      case AArch64::ADDVv8i8v:
        eltSize = 8;
        numElts = 8;
        break;
      case AArch64::ADDVv16i8v:
      case AArch64::NEGv16i8:
      case AArch64::UADDLVv16i8v:
      case AArch64::UADDLPv16i8_v8i16:
      case AArch64::UADALPv16i8_v8i16:
      case AArch64::NOTv16i8:
      case AArch64::CNTv16i8:
        eltSize = 8;
        numElts = 16;
        break;
      default:
        *out << "\nError Unknown opcode\n";
        visitError();
        break;
      }

      auto *vTy =
          VectorType::get(getIntTy(eltSize), ElementCount::getFixed(numElts));

      // Perform the operation
      switch (opcode) {

      case AArch64::ADDVv16i8v:
      case AArch64::ADDVv8i16v:
      case AArch64::ADDVv4i32v:
      case AArch64::ADDVv8i8v:
      case AArch64::ADDVv4i16v: {
        auto src_vector = createBitCast(src, vTy);
        Value *sum = getIntConst(0, eltSize);
        for (unsigned i = 0; i < numElts; ++i) {
          auto elt = createExtractElement(src_vector, getIntConst(i, 32));
          sum = createAdd(sum, elt);
        }
        // sum goes into the bottom lane, all others are zeroed out
        auto zero = ConstantVector::getSplat(ElementCount::getFixed(numElts),
                                             getIntConst(0, eltSize));
        auto res = createInsertElement(zero, sum, getIntConst(0, 32));
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
        auto res = addPairs(src_vector, eltSize, numElts);
        updateOutputReg(res);
        break;
      }

      case AArch64::UADALPv4i16_v2i32:
      case AArch64::UADALPv2i32_v1i64:
      case AArch64::UADALPv8i8_v4i16:
      case AArch64::UADALPv8i16_v4i32:
      case AArch64::UADALPv4i32_v2i64:
      case AArch64::UADALPv16i8_v8i16: {
        auto src2 = readFromOperand(2);
        auto src2_vector = createBitCast(src2, vTy);
        auto sum = addPairs(src2_vector, eltSize, numElts);
        auto bigEltTy = getIntTy(2 * eltSize);
        auto *bigTy =
            VectorType::get(bigEltTy, ElementCount::getFixed(numElts / 2));
        Value *res = ConstantVector::getSplat(
            ElementCount::getFixed(numElts / 2), UndefValue::get(bigEltTy));
        auto src_vector = createBitCast(src, bigTy);
        for (unsigned i = 0; i < numElts / 2; ++i) {
          auto elt1 = createExtractElement(src_vector, getIntConst(i, 32));
          auto elt2 = createExtractElement(sum, getIntConst(i, 32));
          auto add = createAdd(elt1, elt2);
          res = createInsertElement(res, add, getIntConst(i, 32));
        }

        updateOutputReg(res);
        break;
      }

      case AArch64::UADDLVv8i16v:
      case AArch64::UADDLVv4i32v:
      case AArch64::UADDLVv8i8v:
      case AArch64::UADDLVv4i16v:
      case AArch64::UADDLVv16i8v: {
        auto src_vector = createBitCast(src, vTy);
        auto bigTy = getIntTy(2 * eltSize);
        Value *sum = getIntConst(0, 2 * eltSize);
        for (unsigned i = 0; i < numElts; ++i) {
          auto elt = createExtractElement(src_vector, getIntConst(i, 32));
          auto ext = createZExt(elt, bigTy);
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

      default: {
        *out << "\nError Unknown opcode\n";
        visitError();
        break;
      }
      }

      break;
    }
    default:
      *out << funcToString(&Fn);
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
   * the idea here is that if a parameter is, for example, 8 bits,
   * then we only want to initialize the lower 8 bits of the register
   * or stack slot, with the remaining bits containing junk, in order
   * to detect cases where the compiler incorrectly emits code
   * depending on that junk. on the other hand, if a parameter is
   * signext or zeroext then we have to actually initialize those
   * higher bits.
   *
   * FIXME -- this code was originally developed for scalar parameters
   * and we're mostly sort of hoping it also works for vectors. this
   * should work fine as long as the only vectors we accept are 64 and
   * 128 bits, which seemed (as of Nov 2023) to be the only ones with
   * a stable ABI
   */
  Value *parameterABIRules(Value *V, bool isSExt, bool isZExt) {
    auto i8 = getIntTy(8);
    auto i32 = getIntTy(32);
    auto argTy = V->getType();
    unsigned targetWidth;

    // these are already sized appropriately for their register or
    // stack slot
    if (argTy->isPointerTy())
      return V;

    if (argTy->isVectorTy() || argTy->isFloatingPointTy()) {
      argTy = getIntTy(getBitWidth(V));
      V = createBitCast(V, argTy);
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

    if (isZExt && getBitWidth(V) < targetWidth) {
      V = createZExt(V, getIntTy(targetWidth));
    }

    return V;
  }

  void printRegs() {
    const string funcName{"printRegs"};
    auto i1 = getIntTy(1);
    auto retTy = Type::getVoidTy(Ctx);
    auto argTy = {i1, i1, i1, i1};
    auto *fTy = FunctionType::get(retTy, argTy, false);
    Function *F = LiftedModule->getFunction(funcName)
                      ?: Function::createWithDefaultAttr(
                             fTy, GlobalValue::LinkageTypes::ExternalLinkage, 0,
                             funcName, LiftedModule);
    auto N = getN();
    auto Z = getZ();
    auto C = getC();
    auto V = getV();
    CallInst::Create(fTy, F, {N, Z, C, V}, "", LLVMBB);
  }

  void printGlobals() {
    for (auto &g : globals) {
      *out << g.first << " = " << g.second << "\n";
    }
  }

  Function *run() {
    auto i8 = getIntTy(8);
    auto i32 = getIntTy(32);
    auto i64 = getIntTy(64);

    auto Fn =
        Function::Create(srcFn.getFunctionType(), GlobalValue::ExternalLinkage,
                         0, srcFn.getName(), LiftedModule);

    // create LLVM-side basic blocks
    vector<pair<BasicBlock *, MCBasicBlock *>> BBs;
    for (auto &mbb : MF.BBs) {
      auto bb = BasicBlock::Create(Ctx, mbb.getName(), Fn);
      BBs.push_back(make_pair(bb, &mbb));
    }

    // default to adding instructions to the entry block
    LLVMBB = BBs[0].first;

    // Create globals found in assembly
    // If you comment this loop out, global tests 0-100 crash since the size
    // of the global cannot be determined...

    // This loop looks through keys in MF.globals (which is populated using
    // emitCommonSymbol and emitELFSize) and creates a global value for each
    for (const auto &[name, size_alignment_pair] : MF.globals) {
      // Gets 2nd argument to ArrayType::get from the size provided by assembly
      auto *AT = ArrayType::get(i8, size_alignment_pair.first);
      auto *g = new GlobalVariable(*LiftedModule, AT, false,
                                   GlobalValue::LinkageTypes::ExternalLinkage,
                                   nullptr, name);
      g->setAlignment(MaybeAlign(size_alignment_pair.second));
      globals[name] = g;
    }

    // Create globals not found in the assembly
    // If you comment this loop out, global tests 101 and above crash since the
    // assembly does not have a global which is reference in the instructions...

    // This loop looks through globals in the source LLVM IR and creates a
    // global value for those that were not found in the assembly (If a global
    // is in the assembly, it would have been created by the first loop)
    for (auto &srcFnGlobal : srcFn.getParent()->globals()) {
      // If the global has not been created yet, create it
      auto name = srcFnGlobal.getName();
      if (globals[name.str()] == nullptr) {
        auto *g = new GlobalVariable(
            *LiftedModule, srcFnGlobal.getValueType(), false,
            GlobalValue::LinkageTypes::ExternalLinkage, nullptr, name);
        g->setAlignment(MaybeAlign(srcFnGlobal.getAlign()));
        globals[srcFnGlobal.getName().str()] = g;
      }
    }

    // number of 8-byte stack slots for paramters
    const int stackSlots = 32;
    // amount of stack available for use by the lifted function, in bytes
    const int localFrame = 1024;

    auto *allocTy = FunctionType::get(PointerType::get(Ctx, 0), {i32}, false);
    auto *myAlloc = Function::Create(allocTy, GlobalValue::ExternalLinkage, 0,
                                     "myalloc", LiftedModule);
    myAlloc->addRetAttr(Attribute::NonNull);
    AttrBuilder B(Ctx);
    B.addAllocKindAttr(AllocFnKind::Alloc);
    B.addAllocSizeAttr(0, {});
    myAlloc->addFnAttrs(B);
    stackMem = CallInst::Create(
        myAlloc, {getIntConst(localFrame + (8 * stackSlots), 32)}, "stack",
        LLVMBB);

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
    // handled in updateOutputReg()
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
    unsigned stackArgNum = 0;

    for (Function::arg_iterator arg = Fn->arg_begin(), E = Fn->arg_end(),
                                srcArg = srcFn.arg_begin();
         arg != E; ++arg, ++srcArg) {
      *out << "  processing arg wtih vecArgNum = " << vecArgNum
           << ", scalarArgNum = " << scalarArgNum
           << ", stackArgNum = " << stackArgNum << "\n";
      auto *argTy = arg->getType();
      auto *val =
          parameterABIRules(arg, srcArg->hasSExtAttr(), srcArg->hasZExtAttr());

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

      {
        // anything else goes onto the stack!
        if (stackArgNum >= stackSlots) {
          *out << "\nERROR: maximum stack slots for parameter values "
                  "exceeded\n\n";
          exit(-1);
        }
        if (argTy->isPointerTy() || argTy->isIntegerTy()) {
          auto addr =
              createGEP(i64, paramBase, {getIntConst(stackArgNum, 64)}, "");
          createStore(val, addr);
          ++stackArgNum;
        } else if (getBitWidth(val) == 128) {
          auto addr =
              createGEP(i64, paramBase, {getIntConst(stackArgNum, 64)}, "");
          createStore(val, addr);
          stackArgNum += 2;
        } else {
          assert(false);
        }
      }

    end:;
    }

    *out << "done with callee-side ABI stuff\n";

    // initialize the frame pointer
    auto initFP = createGEP(i64, paramBase, {getIntConst(stackArgNum, 64)}, "");
    createStore(initFP, RegFile[AArch64::FP]);

    *out << "about to lift the instructions\n";

    for (auto &[llvm_bb, mc_bb] : BBs) {
      *out << "visiting bb: " << mc_bb->getName() << "\n";
      LLVMBB = llvm_bb;
      MCBB = mc_bb;
      auto &mc_instrs = mc_bb->getInstrs();

      for (auto &inst : mc_instrs) {
        *out << "  ";
        inst.dump();
        if (DebugRegs)
          printRegs();
        llvmInstNum = 0;
        mc_visit(inst, *Fn);
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
          auto *dst = getBBByName(*Fn, MCBB->getSuccs()[0]->getName());
          createBranch(dst);
        }
      }
    }
    return Fn;
  }
};

// We're overriding MCStreamerWrapper to generate an MCFunction
// from the arm assembly. MCStreamerWrapper provides callbacks to handle
// different parts of the assembly file. The callbacks that we're
// using right now are all emit* functions.
class MCStreamerWrapper final : public MCStreamer {
  enum ASMLine { none = 0, label = 1, non_term_instr = 2, terminator = 3 };

private:
  // curLabel which is set in emitLabel allows tracking the label being
  // processed allowing the emit* functions to know which label to associate the
  // directive with if the directive does not have an MCSymbol field
  // eg: .*align directive which is parsed using the emitValueToAlignment
  // function.
  MCSymbol *curLabel{nullptr};
  MCBasicBlock *curBB{nullptr};
  unsigned prev_line{0};
  MCInstrAnalysis *IA;
  MCInstPrinter *IP;
  MCRegisterInfo *MRI;
  bool FunctionEnded = false;

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

  void printMCExpr(const MCExpr *Expr) {
    if (Expr) {
      int64_t Res;
      if (Expr->evaluateAsAbsolute(Res)) {
        *out << "  expr = " << Res << "\n";
      } else {
        *out << "  can't evaluate expr as absolute\n";
      }
    } else {
      *out << "  null expr\n";
    }
  }

  virtual bool emitSymbolAttribute(MCSymbol *Symbol,
                                   MCSymbolAttr Attribute) override {
    if (false) {
      *out << "[[emitSymbolAttribute]]\n";
      std::string sss;
      llvm::raw_string_ostream ss(sss);
      Symbol->print(ss, nullptr);
      *out << "  " << sss << "\n";
      *out << "  Common? " << Symbol->isCommon() << "\n";
      *out << "  Varible? " << Symbol->isVariable() << "\n";
      *out << "  Attribute = " << attrName(Attribute) << "\n\n";
    }
    return true;
  }

  virtual void emitCommonSymbol(MCSymbol *Symbol, uint64_t Size,
                                Align ByteAlignment) override {
    auto name = (string)Symbol->getName();
    *out << "[emitCommonSymbol]\n";
    std::string sss;
    llvm::raw_string_ostream ss(sss);
    *out << "  creating " << Size << " byte global ELF object " << name
         << " with " << ByteAlignment.value() << " byte alignment\n";
    MF.globals[name] = make_pair(Size, ByteAlignment.value());
    Symbol->print(ss, nullptr);
    *out << sss << " "
         << "size = " << Size << " Align = " << ByteAlignment.value() << "\n\n";
  }

  virtual void emitZerofill(MCSection *Section, MCSymbol *Symbol = nullptr,
                            uint64_t Size = 0, Align ByteAlignment = Align(1),
                            SMLoc Loc = SMLoc()) override {
    if (false) {
      *out << "[emitZerofill]\n";
      *out << (string)Section->getName() << "\n\n";
    }
  }

  virtual void emitELFSize(MCSymbol *Symbol, const MCExpr *Value) override {
    *out << "[emitELFSize]\n";
    auto name = (string)Symbol->getName();
    int64_t size;
    if (Value && Value->evaluateAsAbsolute(size)) {
      *out << "  creating " << size << " byte global ELF object " << name
           << "\n";
      MF.globals[name] = make_pair(size, 16);
    } else {
      *out << "  can't get ELF size of " << name << "\n";
    }
  }

  virtual void emitValueToAlignment(Align Alignment, int64_t Value = 0,
                                    unsigned int ValueSize = 1,
                                    unsigned int MaxBytesToEmit = 0) override {
    *out << "[emitValueToAlignment]\n";

    if (curLabel) {
      if (MF.globals.contains((string)curLabel->getName())) {
        *out << "  Associating " << Alignment.value() / 8
             << " byte alignment with " << (string)curLabel->getName() << "\n";
        MF.globals[(string)curLabel->getName()].second = Alignment.value() / 8;
      } else {
        *out << "  " << (string)curLabel->getName()
             << " not a part of globals\n";
      }
    } else {
      *out << "No label to associate with alignment"
           << "\n";
    }
  }

  virtual void emitLabel(MCSymbol *Symbol, SMLoc Loc) override {
    curLabel = Symbol;

    string Lab = Symbol->getName().str();
    *out << "[[emitLabel " << Lab << "]]\n";

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

  // Only call after MF with Basicblocks is constructed to generate the
  // successors for each basic block
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
        cur_bb.addSucc(target_bb);
        if (next_bb_ptr)
          cur_bb.addSucc(next_bb_ptr);
      } else if (IA->isUnconditionalBranch(last_mc_instr)) {
        string target = findTargetLabel(last_mc_instr);
        auto target_bb = MF.findBlockByName(target);
        cur_bb.addSucc(target_bb);
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

void reset() {
  static bool initialized = false;

  if (!initialized) {
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
    initialized = true;
  }

  // FIXME this is a pretty error-prone way to reset the state,
  // probably should just encapsulate this in a class
  origRetWidth = 64;
  has_ret_attr = false;
}

pair<Function *, Function *> liftFunc(Module *OrigModule, Module *LiftedModule,
                                      Function *srcFn,
                                      unique_ptr<MemoryBuffer> MB,
                                      bool DebugRegs) {
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

  auto lifted =
      arm2llvm(LiftedModule, Str.MF, *srcFn, IP.get(), DebugRegs).run();

  std::string sss;
  llvm::raw_string_ostream ss(sss);
  if (llvm::verifyModule(*LiftedModule, &ss)) {
    *out << sss << "\n\n";
    out->flush();
    *out << "\nERROR: Lifted module is broken, this should not happen\n";
    exit(-1);
  }

  return make_pair(srcFn, lifted);
}

} // namespace lifter
