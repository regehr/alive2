#pragma once

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCInstPrinter.h"
#include "llvm/MC/MCParser/MCTargetAsmParser.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCTargetOptions.h"
#include "llvm/MC/MCTargetOptionsCommandFlags.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"

#include "backend_tv/lifter.h"
#include "backend_tv/streamerwrapper.h"

#include "aslp/aslp_bridge.h"

namespace lifter {

class MCFunction;
class MCBasicBlock;

class mc2llvm : public aslp::lifter_interface_llvm {
public:
  llvm::SourceMgr SrcMgr;
  llvm::Module *LiftedModule{nullptr};
  llvm::LLVMContext &Ctx;
  llvm::Function *srcFn{nullptr};
  llvm::Function *liftedFn{nullptr};
  MCBasicBlock *MCBB{nullptr};
  llvm::BasicBlock *LLVMBB{nullptr};
  llvm::MCInstPrinter *InstPrinter{nullptr};
  llvm::MCInst *CurInst{nullptr}, *PrevInst{nullptr};
  unsigned armInstNum{0}, llvmInstNum{0};
  std::map<unsigned, llvm::Value *> RegFile;
  llvm::Value *stackMem{nullptr};
  std::unordered_map<std::string, llvm::Constant *> LLVMglobals;
  llvm::Value *initialSP{nullptr}, *initialReg[32]{nullptr};
  llvm::Function *assertDecl{nullptr};
  std::unique_ptr<llvm::MCSubtargetInfo> STI;
  std::unique_ptr<llvm::MCInstrAnalysis> IA;
  const llvm::DataLayout &DL;
  std::unique_ptr<llvm::MCInstrInfo> MCII;
  std::unique_ptr<llvm::MCRegisterInfo> MRI;
  llvm::MCTargetOptions MCOptions;
  std::unique_ptr<llvm::MCAsmInfo> MAI;
  std::unique_ptr<llvm::MCContext> MCCtx;
  std::unique_ptr<llvm::MCCodeEmitter> MCE;
  std::unique_ptr<MCStreamerWrapper> Str;
  std::unique_ptr<llvm::MemoryBuffer> MB;

  mc2llvm(llvm::Function *srcFn, std::unique_ptr<llvm::MemoryBuffer> MB)
      : LiftedModule{new llvm::Module("LiftedModule", srcFn->getContext())},
        Ctx{srcFn->getContext()}, srcFn{srcFn},
        STI{Targ->createMCSubtargetInfo(DefaultTT.getTriple(), DefaultCPU,
                                        DefaultFeatures)},
        DL{srcFn->getParent()->getDataLayout()},
        MCII{Targ->createMCInstrInfo()},
        MRI{Targ->createMCRegInfo(DefaultTT.getTriple())},
        MCOptions{llvm::mc::InitMCTargetOptionsFromFlags()},
        MAI{Targ->createMCAsmInfo(*MRI, DefaultTT.getTriple(), MCOptions)},
        MCCtx{std::make_unique<llvm::MCContext>(
            DefaultTT, MAI.get(), MRI.get(), STI.get(), &SrcMgr, &MCOptions)},
        MCE{Targ->createMCCodeEmitter(*MCII.get(), *MCCtx.get())},
        MB{std::move(MB)} {}

  // these are ones that the backend adds to tgt, even when they don't
  // appear at all in src
  const std::unordered_map<std::string, std::string> intrinsic_names = {
      // memory
      {"memcpy", "llvm.memcpy.p0.p0.i64"},
      {"memset", "llvm.memset.p0.i64"},
      {"memmove", "llvm.memmove.p0.p0.i64"},
      {"__llvm_memset_element_unordered_atomic_16",
       "llvm.memset.element.unordered.atomic.p0.i32"}, // FIXME

      // FP
      {"cosf", "llvm.cos.f32"},
      {"coshf", "llvm.cosh.f32"},
      {"powf", "llvm.pow.f32"},
      {"pow", "llvm.pow.f64"},
      {"expf", "llvm.exp.f32"},
      {"exp", "llvm.exp.f64"},
      {"exp10f", "llvm.exp10.f32"},
      {"exp10", "llvm.exp10.f64"},

      {"ldexpf", "llvm.ldexp.f32.i32"},
      {"sincos", "llvm.cos.f64"},
  };

  // intrinsics get their names mangled by LLVM, we need to reverse
  // that, and there's no way to do it other than hard-coding this
  // mapping
  const std::unordered_map<std::string, llvm::FunctionType *>
      implicit_intrinsics = {
          {"llvm.memset.p0.i64",
           llvm::FunctionType::get(llvm::Type::getVoidTy(Ctx),
                                   {
                                       llvm::PointerType::get(Ctx, 0),
                                       llvm::Type::getInt8Ty(Ctx),
                                       llvm::Type::getInt64Ty(Ctx),
                                       llvm::Type::getInt1Ty(Ctx),
                                   },
                                   false)},
      };

  std::map<std::string, unsigned> encodingCounts;

  // Map of ADRP MCInsts to the string representations of the operand variable
  // names
  std::unordered_map<llvm::MCInst *, std::string> instExprVarMap;

  struct deferredGlobal {
    std::string name;
    llvm::GlobalVariable *val;
  };
  std::vector<deferredGlobal> deferredGlobs;

  int64_t getImm(int idx) {
    return CurInst->getOperand(idx).getImm();
  }

  llvm::Function *copyFunctionToTarget(llvm::Function *f,
                                       const llvm::Twine &name);
  llvm::Constant *lazyAddGlobal(std::string newGlobal);

  // create lifted globals only on demand -- saves time and clutter for
  // large modules
  llvm::Constant *lookupGlobal(const std::string &nm);

  llvm::BasicBlock *getBB(llvm::MCOperand &jmp_tgt) {
    assert(jmp_tgt.isExpr() && "[getBB] expected expression operand");
    assert(
        (jmp_tgt.getExpr()->getKind() == llvm::MCExpr::ExprKind::SymbolRef) &&
        "[getBB] expected symbol ref as jump operand");
    const llvm::MCSymbolRefExpr &SRE =
        cast<llvm::MCSymbolRefExpr>(*jmp_tgt.getExpr());
    const llvm::MCSymbol &Sym = SRE.getSymbol();
    llvm::StringRef name = Sym.getName();
    for (auto &bb : *liftedFn) {
      if (bb.getName() == name)
        return &bb;
    }
    return nullptr;
  }

  // FIXME -- do this without the strings, just keep a map or something
  llvm::BasicBlock *getBBByName(llvm::StringRef name) {
    for (auto &bb : *liftedFn) {
      if (bb.getName() == name)
        return &bb;
    }
    assert(false && "basic block not found in getBBByName()");
  }

  llvm::Constant *getUnsignedIntConst(uint64_t val, uint64_t bits) override {
    return llvm::ConstantInt::get(Ctx, llvm::APInt(bits, val));
  }

  llvm::Constant *getAllOnesConst(uint64_t bits) {
    return llvm::ConstantInt::get(Ctx, llvm::APInt::getAllOnes(bits));
  }

  llvm::Constant *getSignedMaxConst(uint64_t bits) {
    return llvm::ConstantInt::get(Ctx, llvm::APInt::getSignedMaxValue(bits));
  }

  llvm::Constant *getSignedMinConst(uint64_t bits) {
    return llvm::ConstantInt::get(Ctx, llvm::APInt::getSignedMinValue(bits));
  }

  llvm::Constant *getAllOnesConst(llvm::Type *t) {
    int w = -1;
    if (auto *vTy = dyn_cast<llvm::VectorType>(t)) {
      auto *eTy = vTy->getElementType();
      w = getBitWidth(eTy);
    } else {
      w = getBitWidth(t);
    }
    return llvm::ConstantInt::get(t, llvm::APInt::getAllOnes(w));
  }

  llvm::Constant *getBoolConst(bool val) {
    return llvm::ConstantInt::get(Ctx, llvm::APInt(1, val ? 1 : 0));
  }

  llvm::Constant *getSignedIntConst(int64_t val, uint64_t bits) {
    return llvm::ConstantInt::get(Ctx, llvm::APInt(bits, val, /*signed=*/true));
  }

  llvm::VectorType *getVecTy(unsigned eltSize, unsigned numElts,
                             bool isFP = false) override {
    llvm::Type *eTy;
    if (isFP) {
      eTy = getFPType(eltSize);
    } else {
      eTy = getIntTy(eltSize);
    }
    auto ec = llvm::ElementCount::getFixed(numElts);
    return llvm::VectorType::get(eTy, ec);
  }

  llvm::Constant *getUndefVec(unsigned numElts, unsigned eltSize) override {
    auto eTy = getIntTy(eltSize);
    auto ec = llvm::ElementCount::getFixed(numElts);
    return llvm::ConstantVector::getSplat(ec, llvm::UndefValue::get(eTy));
  }

  llvm::Constant *getElemSplat(unsigned numElts, unsigned eltSize, uint64_t val,
                               bool isSigned = false) {
    auto ec = llvm::ElementCount::getFixed(numElts);
    return llvm::ConstantVector::getSplat(
        ec, llvm::ConstantInt::get(Ctx, llvm::APInt(eltSize, val, isSigned)));
  }

  llvm::Constant *getZeroFPVec(unsigned numElts, unsigned eltSize) {
    assert(eltSize == 32 || eltSize == 64);
    auto ec = llvm::ElementCount::getFixed(numElts);
    auto &sem = (eltSize == 64) ? llvm::APFloat::IEEEdouble()
                                : llvm::APFloat::IEEEsingle();
    auto z = llvm::APFloat::getZero(sem);
    return llvm::ConstantVector::getSplat(ec, llvm::ConstantFP::get(Ctx, z));
  }

  llvm::Constant *getZeroIntVec(unsigned numElts, unsigned eltSize) {
    return getElemSplat(numElts, eltSize, 0);
  }

  llvm::Type *getIntTy(unsigned bits) override {
    return llvm::Type::getIntNTy(Ctx, bits);
  }

  llvm::Type *getFPType(unsigned bits) override {
    if (bits == 16)
      return llvm::Type::getHalfTy(Ctx);
    else if (bits == 32)
      return llvm::Type::getFloatTy(Ctx);
    else if (bits == 64)
      return llvm::Type::getDoubleTy(Ctx);
    else
      assert(false && "unsupported floating point type");
  }

  // Create and return a ConstantVector out of the vector of Constant vals
  llvm::Value *getVectorConst(const std::vector<llvm::Constant *> &vals) {
    return llvm::ConstantVector::get(vals);
  }

  // Takes an LLVM Type*, constructs a mask value of this type with
  // mask value = W - 1 where W is the bitwidth of the element type if a vector
  // type or the bitwidth of the type if an integer
  llvm::Value *getMaskByType(llvm::Type *llvm_ty);

  [[noreturn]] void visitError() {
    out->flush();
    std::string str(InstPrinter->getOpcodeName(CurInst->getOpcode()));
    *out << "\nERROR: Unsupported instruction: " << str << "\n";
    out->flush();
    exit(-1);
  }

  void set_bb(llvm::BasicBlock *bb) override {
    LLVMBB = bb;
  }

  llvm::BasicBlock *get_bb() override {
    return LLVMBB;
  }

  llvm::Function &ll_function() override {
    return *liftedFn;
  }

  // lifted instructions are named using the number of the ARM
  // instruction they come from
  std::string nextName() override {
    std::stringstream ss;
    ss << "a" << armInstNum << "_" << llvmInstNum++;
    return ss.str();
  }

  llvm::AllocaInst *createAlloca(llvm::Type *ty, llvm::Value *sz,
                                 const std::string &NameStr) override {
    return new llvm::AllocaInst(ty, 0, sz, NameStr, LLVMBB);
  }

  llvm::GetElementPtrInst *createGEP(llvm::Type *ty, llvm::Value *v,
                                     llvm::ArrayRef<llvm::Value *> idxlist,
                                     const std::string &NameStr) override {
    return llvm::GetElementPtrInst::Create(ty, v, idxlist, NameStr, LLVMBB);
  }

  void createBranch(llvm::Value *c, llvm::BasicBlock *t, llvm::BasicBlock *f) {
    llvm::BranchInst::Create(t, f, c, LLVMBB);
  }

  void createBranch(llvm::Value *c, stmt_t t, stmt_t f) override {
    createBranch(c, t.first, f.first);
  }

  void createBranch(llvm::BasicBlock *dst) {
    llvm::BranchInst::Create(dst, LLVMBB);
  }

  void createBranch(stmt_t dst) override {
    createBranch(dst.first);
  }

  llvm::LoadInst *createLoad(llvm::Type *ty, llvm::Value *ptr) override {
    return new llvm::LoadInst(ty, ptr, nextName(), false, llvm::Align(1),
                              LLVMBB);
  }

  void createStore(llvm::Value *v, llvm::Value *ptr) override {
    new llvm::StoreInst(v, ptr, false, llvm::Align(1), LLVMBB);
  }

  llvm::Value *createTrap() override {
    auto decl = llvm::Intrinsic::getOrInsertDeclaration(LiftedModule,
                                                        llvm::Intrinsic::trap);
    return llvm::CallInst::Create(decl, "", LLVMBB);
  }

  llvm::Value *createUnreachable() {
    return new llvm::UnreachableInst(Ctx, LLVMBB);
  }

  llvm::Value *createSMin(llvm::Value *a, llvm::Value *b) override {
    auto decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::smin, a->getType());
    return llvm::CallInst::Create(decl, {a, b}, nextName(), LLVMBB);
  }

  llvm::Value *createSMax(llvm::Value *a, llvm::Value *b) override {
    auto decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::smax, a->getType());
    return llvm::CallInst::Create(decl, {a, b}, nextName(), LLVMBB);
  }

  llvm::Value *createUMin(llvm::Value *a, llvm::Value *b) override {
    auto decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::umin, a->getType());
    return llvm::CallInst::Create(decl, {a, b}, nextName(), LLVMBB);
  }

  llvm::Value *createUMax(llvm::Value *a, llvm::Value *b) override {
    auto decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::umax, a->getType());
    return llvm::CallInst::Create(decl, {a, b}, nextName(), LLVMBB);
  }

  llvm::Value *createFNeg(llvm::Value *v) override {
    return llvm::UnaryOperator::CreateFNeg(v, nextName(), LLVMBB);
  }

  llvm::Value *createFAbs(llvm::Value *v) override {
    auto fabs_decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::fabs, v->getType());
    return llvm::CallInst::Create(fabs_decl, {v}, nextName(), LLVMBB);
  }

  llvm::CallInst *createSSubOverflow(llvm::Value *a, llvm::Value *b) override {
    auto ssub_decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::ssub_with_overflow, a->getType());
    return llvm::CallInst::Create(ssub_decl, {a, b}, nextName(), LLVMBB);
  }

  llvm::CallInst *createSAddOverflow(llvm::Value *a, llvm::Value *b) override {
    auto sadd_decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::sadd_with_overflow, a->getType());
    return llvm::CallInst::Create(sadd_decl, {a, b}, nextName(), LLVMBB);
  }

  llvm::CallInst *createUSubOverflow(llvm::Value *a, llvm::Value *b) override {
    auto usub_decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::usub_with_overflow, a->getType());
    return llvm::CallInst::Create(usub_decl, {a, b}, nextName(), LLVMBB);
  }

  llvm::CallInst *createUAddOverflow(llvm::Value *a, llvm::Value *b) override {
    auto uadd_decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::uadd_with_overflow, a->getType());
    return llvm::CallInst::Create(uadd_decl, {a, b}, nextName(), LLVMBB);
  }

  llvm::CallInst *createUAddSat(llvm::Value *a, llvm::Value *b) override {
    auto uadd_decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::uadd_sat, a->getType());
    return llvm::CallInst::Create(uadd_decl, {a, b}, nextName(), LLVMBB);
  }

  llvm::CallInst *createUSubSat(llvm::Value *a, llvm::Value *b) override {
    auto usub_decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::usub_sat, a->getType());
    return llvm::CallInst::Create(usub_decl, {a, b}, nextName(), LLVMBB);
  }

  llvm::CallInst *createSAddSat(llvm::Value *a, llvm::Value *b) override {
    auto sadd_decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::sadd_sat, a->getType());
    return llvm::CallInst::Create(sadd_decl, {a, b}, nextName(), LLVMBB);
  }

  llvm::CallInst *createSSubSat(llvm::Value *a, llvm::Value *b) override {
    auto ssub_decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::ssub_sat, a->getType());
    return llvm::CallInst::Create(ssub_decl, {a, b}, nextName(), LLVMBB);
  }

  llvm::CallInst *createCtPop(llvm::Value *v) override {
    auto decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::ctpop, v->getType());
    return llvm::CallInst::Create(decl, {v}, nextName(), LLVMBB);
  }

  // first argument is an i16
  llvm::CallInst *createConvertFromFP16(llvm::Value *v,
                                        llvm::Type *ty) override {
    auto cvt_decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::convert_from_fp16, ty);
    return llvm::CallInst::Create(cvt_decl, {v}, nextName(), LLVMBB);
  }

  llvm::CastInst *createConvertFPToSI(llvm::Value *v, llvm::Type *ty) override {
    return new llvm::FPToSIInst(v, ty, nextName(), LLVMBB);
  }

  llvm::CastInst *createConvertFPToUI(llvm::Value *v, llvm::Type *ty) override {
    return new llvm::FPToUIInst(v, ty, nextName(), LLVMBB);
  }

  llvm::CastInst *createPtrToInt(llvm::Value *v, llvm::Type *ty) override {
    return new llvm::PtrToIntInst(v, ty, nextName(), LLVMBB);
  }

  llvm::InsertElementInst *
  createInsertElement(llvm::Value *vec, llvm::Value *val, int idx) override {
    auto idxv = getUnsignedIntConst(idx, 32);
    return llvm::InsertElementInst::Create(vec, val, idxv, nextName(), LLVMBB);
  }

  llvm::InsertElementInst *createInsertElement(llvm::Value *vec,
                                               llvm::Value *val,
                                               llvm::Value *idx) override {
    return llvm::InsertElementInst::Create(vec, val, idx, nextName(), LLVMBB);
  }

  llvm::ExtractElementInst *createExtractElement(llvm::Value *v,
                                                 llvm::Value *idx) override {
    return llvm::ExtractElementInst::Create(v, idx, nextName(), LLVMBB);
  }

  llvm::ExtractElementInst *createExtractElement(llvm::Value *v,
                                                 int idx) override {
    auto idxv = getUnsignedIntConst(idx, 32);
    return llvm::ExtractElementInst::Create(v, idxv, nextName(), LLVMBB);
  }

  llvm::ShuffleVectorInst *
  createShuffleVector(llvm::Value *v, llvm::ArrayRef<int> mask) override {
    return new llvm::ShuffleVectorInst(v, mask, nextName(), LLVMBB);
  }

  llvm::ShuffleVectorInst *createShuffleVector(llvm::Value *v,
                                               llvm::Value *mask) {
    return new llvm::ShuffleVectorInst(v, mask, nextName(), LLVMBB);
  }

  llvm::ShuffleVectorInst *
  createShuffleVector(llvm::Value *v, llvm::Value *x,
                      llvm::ArrayRef<int> mask) override {
    return new llvm::ShuffleVectorInst(v, x, mask, nextName(), LLVMBB);
  }

  llvm::ShuffleVectorInst *createShuffleVector(llvm::Value *v1, llvm::Value *v2,
                                               llvm::Value *mask) {
    return new llvm::ShuffleVectorInst(v1, v2, mask, nextName(), LLVMBB);
  }

  llvm::ExtractValueInst *
  createExtractValue(llvm::Value *v, llvm::ArrayRef<unsigned> idxs) override {
    return llvm::ExtractValueInst::Create(v, idxs, nextName(), LLVMBB);
  }

  llvm::ReturnInst *createReturn(llvm::Value *v) override {
    return llvm::ReturnInst::Create(Ctx, v, LLVMBB);
  }

  llvm::CallInst *createFShr(llvm::Value *a, llvm::Value *b,
                             llvm::Value *c) override {
    auto *decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::fshr, a->getType());
    return llvm::CallInst::Create(decl, {a, b, c}, nextName(), LLVMBB);
  }

  llvm::CallInst *createFShl(llvm::Value *a, llvm::Value *b,
                             llvm::Value *c) override {
    auto *decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::fshl, a->getType());
    return llvm::CallInst::Create(decl, {a, b, c}, nextName(), LLVMBB);
  }

  llvm::CallInst *createBitReverse(llvm::Value *v) override {
    auto *decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::bitreverse, v->getType());
    return llvm::CallInst::Create(decl, {v}, nextName(), LLVMBB);
  }

  llvm::CallInst *createAbs(llvm::Value *v) override {
    auto *decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::abs, v->getType());
    return llvm::CallInst::Create(decl, {v, getBoolConst(false)}, nextName(),
                                  LLVMBB);
  }

  llvm::CallInst *createCtlz(llvm::Value *v) override {
    auto *decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::ctlz, v->getType());
    return llvm::CallInst::Create(decl, {v, getBoolConst(false)}, nextName(),
                                  LLVMBB);
  }

  llvm::CallInst *createBSwap(llvm::Value *v) override {
    auto *decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::bswap, v->getType());
    return llvm::CallInst::Create(decl, {v}, nextName(), LLVMBB);
  }

  llvm::CallInst *createVectorReduceAdd(llvm::Value *v) override {
    auto *decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::vector_reduce_add, v->getType());
    return llvm::CallInst::Create(decl, {v}, nextName(), LLVMBB);
  }

  llvm::CallInst *createFusedMultiplyAdd(llvm::Value *a, llvm::Value *b,
                                         llvm::Value *c) override {
    auto *decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::fma, a->getType());
    return llvm::CallInst::Create(decl, {a, b, c}, nextName(), LLVMBB);
  }

  llvm::CallInst *createSQRT(llvm::Value *v) override {
    auto *decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::sqrt, v->getType());
    return llvm::CallInst::Create(decl, {v}, nextName(), LLVMBB);
  }

  llvm::CallInst *createRound(llvm::Value *v) override {
    auto *decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::rint, v->getType());
    return llvm::CallInst::Create(decl, {v}, nextName(), LLVMBB);
  }

  llvm::CallInst *createConstrainedRound(llvm::Value *v, llvm::Metadata *md) {
    auto *decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::experimental_constrained_round,
        v->getType());
    return llvm::CallInst::Create(
        decl, {v, llvm::MetadataAsValue::get(Ctx, md)}, nextName(), LLVMBB);
  }

  llvm::CallInst *createConstrainedFloor(llvm::Value *v, llvm::Metadata *md) {
    auto *decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::experimental_constrained_floor,
        v->getType());
    return llvm::CallInst::Create(
        decl, {v, llvm::MetadataAsValue::get(Ctx, md)}, nextName(), LLVMBB);
  }

  llvm::CallInst *createConstrainedCeil(llvm::Value *v, llvm::Metadata *md) {
    auto *decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::experimental_constrained_ceil,
        v->getType());
    return llvm::CallInst::Create(
        decl, {v, llvm::MetadataAsValue::get(Ctx, md)}, nextName(), LLVMBB);
  }

  llvm::CallInst *createConstrainedRound(llvm::Value *v) override {
    return createConstrainedRound(v,
                                  llvm::MDString::get(Ctx, "fpexcept.strict"));
  }

  llvm::CallInst *createConstrainedFloor(llvm::Value *v) override {
    return createConstrainedFloor(v,
                                  llvm::MDString::get(Ctx, "fpexcept.strict"));
  }

  llvm::CallInst *createConstrainedCeil(llvm::Value *v) override {
    return createConstrainedCeil(v,
                                 llvm::MDString::get(Ctx, "fpexcept.strict"));
  }

  llvm::SelectInst *createSelect(llvm::Value *cond, llvm::Value *a,
                                 llvm::Value *b) override {
    return llvm::SelectInst::Create(cond, a, b, nextName(), LLVMBB);
  }

  llvm::ICmpInst *createICmp(llvm::ICmpInst::Predicate p, llvm::Value *a,
                             llvm::Value *b) override {
    return new llvm::ICmpInst(LLVMBB, p, a, b, nextName());
  }

  llvm::FCmpInst *createFCmp(llvm::FCmpInst::Predicate p, llvm::Value *a,
                             llvm::Value *b) override {
    return new llvm::FCmpInst(LLVMBB, p, a, b, nextName());
  }

  llvm::BinaryOperator *createBinop(llvm::Value *a, llvm::Value *b,
                                    llvm::Instruction::BinaryOps op) override {
    return llvm::BinaryOperator::Create(op, a, b, nextName(), LLVMBB);
  }

  llvm::BinaryOperator *createUDiv(llvm::Value *a, llvm::Value *b) override {
    return llvm::BinaryOperator::Create(llvm::Instruction::UDiv, a, b,
                                        nextName(), LLVMBB);
  }

  llvm::BinaryOperator *createSDiv(llvm::Value *a, llvm::Value *b) override {
    return llvm::BinaryOperator::Create(llvm::Instruction::SDiv, a, b,
                                        nextName(), LLVMBB);
  }

  llvm::BinaryOperator *createMul(llvm::Value *a, llvm::Value *b) override {
    return llvm::BinaryOperator::Create(llvm::Instruction::Mul, a, b,
                                        nextName(), LLVMBB);
  }

  llvm::BinaryOperator *createAdd(llvm::Value *a, llvm::Value *b) override {
    return llvm::BinaryOperator::Create(llvm::Instruction::Add, a, b,
                                        nextName(), LLVMBB);
  }

  llvm::BinaryOperator *createFAdd(llvm::Value *a, llvm::Value *b) {
    return llvm::BinaryOperator::Create(llvm::Instruction::FAdd, a, b,
                                        nextName(), LLVMBB);
  }

  llvm::BinaryOperator *createSub(llvm::Value *a, llvm::Value *b) override {
    return llvm::BinaryOperator::Create(llvm::Instruction::Sub, a, b,
                                        nextName(), LLVMBB);
  }

  llvm::BinaryOperator *createFSub(llvm::Value *a, llvm::Value *b) {
    return llvm::BinaryOperator::Create(llvm::Instruction::FSub, a, b,
                                        nextName(), LLVMBB);
  }

  llvm::BinaryOperator *createFMul(llvm::Value *a, llvm::Value *b) {
    return llvm::BinaryOperator::Create(llvm::Instruction::FMul, a, b,
                                        nextName(), LLVMBB);
  }

  llvm::Value *createRawLShr(llvm::Value *a, llvm::Value *b) override {
    return llvm::BinaryOperator::Create(llvm::Instruction::LShr, a, b,
                                        nextName(), LLVMBB);
  }

  llvm::Value *createMaskedLShr(llvm::Value *a, llvm::Value *b) override {
    assert(a->getType() == b->getType() && "Expected values of same type");

    // Get an LLVM mask for b to get shift value less than bit width of a
    // In LLVM shift >= bitwidth -> poison
    auto mask = getMaskByType(a->getType());
    assert(a->getType() == mask->getType() && "Expected values of same type");

    auto masked = llvm::BinaryOperator::Create(llvm::Instruction::And, mask, b,
                                               nextName(), LLVMBB);
    return llvm::BinaryOperator::Create(llvm::Instruction::LShr, a, masked,
                                        nextName(), LLVMBB);
  }

  llvm::Value *createRawAShr(llvm::Value *a, llvm::Value *b) override {
    return llvm::BinaryOperator::Create(llvm::Instruction::AShr, a, b,
                                        nextName(), LLVMBB);
  }

  llvm::Value *createMaskedAShr(llvm::Value *a, llvm::Value *b) override {
    assert(a->getType() == b->getType() && "Expected values of same type");

    // Get an LLVM mask for b to get shift value less than bit width of a
    // In LLVM shift >= bitwidth -> poison
    auto mask = getMaskByType(a->getType());
    assert(a->getType() == mask->getType() && "Expected values of same type");

    auto masked = llvm::BinaryOperator::Create(llvm::Instruction::And, mask, b,
                                               nextName(), LLVMBB);
    return llvm::BinaryOperator::Create(llvm::Instruction::AShr, a, masked,
                                        nextName(), LLVMBB);
  }

  llvm::Value *createRawShl(llvm::Value *a, llvm::Value *b) override {
    return llvm::BinaryOperator::Create(llvm::Instruction::Shl, a, b,
                                        nextName(), LLVMBB);
  }

  llvm::Value *createMaskedShl(llvm::Value *a, llvm::Value *b) override {
    assert(a->getType() == b->getType() && "Expected values of same type");

    // Get an LLVM mask for b to get shift value less than bit width of a
    // In LLVM shift >= bitwidth -> poison
    auto mask = getMaskByType(a->getType());
    assert(a->getType() == mask->getType() && "Expected values of same type");

    auto masked = llvm::BinaryOperator::Create(llvm::Instruction::And, mask, b,
                                               nextName(), LLVMBB);
    return llvm::BinaryOperator::Create(llvm::Instruction::Shl, a, masked,
                                        nextName(), LLVMBB);
  }

  llvm::Value *getLowOnes(int ones, int w) override {
    auto zero = getUnsignedIntConst(0, ones);
    auto one = getUnsignedIntConst(1, ones);
    auto minusOne = createSub(zero, one);
    return createZExt(minusOne, getIntTy(w));
  }

  llvm::Value *createMSL(llvm::Value *a, int b) override {
    auto v = llvm::BinaryOperator::Create(
        llvm::Instruction::Shl, a, getUnsignedIntConst(b, getBitWidth(a)),
        nextName(), LLVMBB);
    auto ones = getLowOnes(b, getBitWidth(a));
    return createOr(v, ones);
  }

  llvm::BinaryOperator *createAnd(llvm::Value *a, llvm::Value *b) override {
    return llvm::BinaryOperator::Create(llvm::Instruction::And, a, b,
                                        nextName(), LLVMBB);
  }

  llvm::BinaryOperator *createOr(llvm::Value *a, llvm::Value *b) override {
    return llvm::BinaryOperator::Create(llvm::Instruction::Or, a, b, nextName(),
                                        LLVMBB);
  }

  llvm::BinaryOperator *createXor(llvm::Value *a, llvm::Value *b) override {
    return llvm::BinaryOperator::Create(llvm::Instruction::Xor, a, b,
                                        nextName(), LLVMBB);
  }

  llvm::BinaryOperator *createNot(llvm::Value *a) override {
    auto NegOne = getAllOnesConst(a->getType());
    return llvm::BinaryOperator::Create(llvm::Instruction::Xor, a, NegOne,
                                        nextName(), LLVMBB);
  }

  llvm::FreezeInst *createFreeze(llvm::Value *v) override {
    return new llvm::FreezeInst(v, nextName(), LLVMBB);
  }

  llvm::Value *createTrunc(llvm::Value *v, llvm::Type *t) override {
    if (v->getType() == t)
      return v;
    return llvm::CastInst::Create(llvm::Instruction::Trunc, v, t, nextName(),
                                  LLVMBB);
  }

  llvm::CastInst *createSExt(llvm::Value *v, llvm::Type *t) override {
    return llvm::CastInst::Create(llvm::Instruction::SExt, v, t, nextName(),
                                  LLVMBB);
  }

  llvm::CastInst *createZExt(llvm::Value *v, llvm::Type *t) override {
    return llvm::CastInst::Create(llvm::Instruction::ZExt, v, t, nextName(),
                                  LLVMBB);
  }

  llvm::CallInst *createFPToUI_sat(llvm::Value *v, llvm::Type *t) override {
    auto decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::fptoui_sat, {t, v->getType()});
    return llvm::CallInst::Create(decl, {v}, nextName(), LLVMBB);
  }

  llvm::CallInst *createFPToSI_sat(llvm::Value *v, llvm::Type *t) override {
    auto decl = llvm::Intrinsic::getOrInsertDeclaration(
        LiftedModule, llvm::Intrinsic::fptosi_sat, {t, v->getType()});
    return llvm::CallInst::Create(decl, {v}, nextName(), LLVMBB);
  }

  llvm::CastInst *createUIToFP(llvm::Value *v, llvm::Type *t) override {
    return llvm::CastInst::Create(llvm::Instruction::UIToFP, v, t, nextName(),
                                  LLVMBB);
  }

  llvm::CastInst *createSIToFP(llvm::Value *v, llvm::Type *t) override {
    return llvm::CastInst::Create(llvm::Instruction::SIToFP, v, t, nextName(),
                                  LLVMBB);
  }

  llvm::CastInst *createFPTrunc(llvm::Value *v, llvm::Type *t) override {
    return llvm::CastInst::Create(llvm::Instruction::FPTrunc, v, t, nextName(),
                                  LLVMBB);
  }

  llvm::CastInst *createFPExt(llvm::Value *v, llvm::Type *t) override {
    return llvm::CastInst::Create(llvm::Instruction::FPExt, v, t, nextName(),
                                  LLVMBB);
  }

  llvm::CastInst *createBitCast(llvm::Value *v, llvm::Type *t) override {
    return llvm::CastInst::Create(llvm::Instruction::BitCast, v, t, nextName(),
                                  LLVMBB);
  }

  llvm::CastInst *createCast(llvm::Value *v, llvm::Type *t,
                             llvm::Instruction::CastOps op) override {
    return llvm::CastInst::Create(op, v, t, nextName(), LLVMBB);
  }

  llvm::Value *splat(llvm::Value *v, unsigned numElts, unsigned eltSize) {
    assert(getBitWidth(v) == eltSize);
    llvm::Value *res = getUndefVec(numElts, eltSize);
    for (unsigned i = 0; i < numElts; ++i)
      res = createInsertElement(res, v, i);
    return res;
  }

  enum class extKind { SExt, ZExt, None };

  llvm::Value *addPairs(llvm::Value *src, unsigned eltSize, unsigned numElts,
                        extKind ext) {
    auto bigEltTy = getIntTy(2 * eltSize);
    llvm::Value *res = getUndefVec(numElts / 2, 2 * eltSize);
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

  unsigned getBitWidth(llvm::Type *ty) {
    if (auto vTy = dyn_cast<llvm::VectorType>(ty)) {
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

  unsigned getBitWidth(llvm::Value *V) {
    return getBitWidth(V->getType());
  }

  std::tuple<std::string, long> getOffset(const std::string &var);
  // Reads an Expr and maps containing string variable to a global variable
  std::string mapExprVar(const llvm::MCExpr *expr);
  std::string demangle(const std::string &name);
  // Reads an Expr and gets the global variable corresponding the containing
  // string variable. Assuming the Expr consists of a single global variable.
  std::pair<llvm::Value *, bool> getExprVar(const llvm::MCExpr *expr);
  // negative shift exponents go the other direction
  llvm::Value *createUSHL(llvm::Value *a, llvm::Value *b);
  // negative shift exponents go the other direction
  llvm::Value *createSSHL(llvm::Value *a, llvm::Value *b);
  llvm::Value *rev(llvm::Value *in, unsigned eltSize, unsigned amt);
  llvm::Value *dupElts(llvm::Value *v, unsigned numElts, unsigned eltSize);
  llvm::Value *concat(llvm::Value *a, llvm::Value *b);
  void assertSame(llvm::Value *a, llvm::Value *b);
  void doDirectCall();
  llvm::Instruction *getCurLLVMInst();
  void liftInst(llvm::MCInst &I);
  void invalidateReg(unsigned Reg, unsigned Width);
  void createRegStorage(unsigned Reg, unsigned Width, const std::string &Name);
  std::pair<llvm::Function *, llvm::Function *> run();

  /*
   * shared with the aslp lifter
   *
   * FIXME -- this should take an MCSymbolRef, not a generic MCExpr
   */
  llvm::Value *lookupExprVar(const llvm::MCExpr &expr) override {
    return lookupGlobal(mapExprVar(&expr));
  }
  void assertTrue(llvm::Value *cond) override;
  void storeToMemoryValOffset(llvm::Value *base, llvm::Value *offset,
                              uint64_t size, llvm::Value *val) override;

  /*
   * per-backend functionality goes here
   */
  virtual void doCall(llvm::FunctionCallee FC, llvm::CallInst *llvmCI,
                      const std::string &calleeName) = 0;
  virtual void lift(llvm::MCInst &I) = 0;
  virtual void platformInit() = 0;
  virtual void doReturn() = 0;
  virtual unsigned branchInst() = 0;
  virtual unsigned sentinelNOP() = 0;
};

} // end namespace lifter
