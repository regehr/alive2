#pragma once

/*
 * primary references for this lifter are the user mode ISA reference:
 *
 * https://drive.google.com/file/d/1uviu1nH-tScFfgrovvFCrj7Omv8tFtkp/view
 *
 * and the ABI document:
 *
 * https://drive.google.com/file/d/1Ja_Tpp_5Me583CGVD-BIZMlgGBnlKU4R/view
 *
 * this thread has some useful details about setting up a RISCV
 * execution environment:
 *
 * https://www.reddit.com/r/RISCV/comments/10k805c/how_can_i_buildrun_riscv_assembly_on_macos/
 */

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
#include "backend_tv/mc2llvm.h"
#include "backend_tv/riscv2llvm.h"
#include "backend_tv/streamerwrapper.h"

#include <cstdint>
#include <map>
#include <optional>
#include <string>

namespace lifter {

class riscv2llvm final : public mc2llvm {
  llvm::Value *initialFPReg[32]{nullptr};

  // RVV state. VLEN, the width of each vector register in bits, is fixed
  // for the whole run at 64 * vscale (LLVM's RVV mapping), and is set in
  // platformInit(). Each of V0-V31 is backed by an iVLEN alloca.
  unsigned VLEN{0};

  // true if the function is riscv_vector_cc, and so follows the psABI's
  // vector calling convention variant, which makes v1-v7 and v24-v31
  // callee-saved. their values on entry are kept here
  bool usesVectorCC{false};
  llvm::Value *initialVReg[32]{nullptr};
  static bool isCalleeSavedVReg(unsigned idx) {
    return (idx >= 1 && idx <= 7) || idx >= 24;
  }

  // a vector register operand names one register, an LMUL>1 group (VnM2,
  // VnM4, VnM8), or a segment tuple; all of these cover a contiguous run
  // of architectural registers starting at `first`
  struct VRegRange {
    unsigned first;
    unsigned count;
  };

  // vtype is tracked statically: most RVV instructions take their element
  // width and register grouping from vtype rather than from their encoding,
  // and the lifter has to know these to pick LLVM types. a program point's
  // vtype is the immediate of the vset{i}vli that reaches it on every path,
  // or unknown (after entry, a call, vsetvl, or a merge of differing
  // settings). vector instructions at a point with unknown vtype are not
  // lifted. see riscv2llvm_vector.cpp
  struct VConfig {
    unsigned vtype;
    unsigned SEW;
    int LMULLog2;
    bool tailAgnostic;
    bool maskAgnostic;
  };
  static constexpr int64_t VTypeUnknown = -1;
  static constexpr int64_t VTypeUnreached = -2;
  std::map<const MCBasicBlock *, int64_t> vtypeIn;
  int64_t curVType{VTypeUnknown};
  const MCBasicBlock *curVTypeBB{nullptr};

  // inactive element accesses are redirected here, so that a vector memory
  // instruction touches memory only for its active elements
  llvm::Value *vScratch{nullptr};

  void updateOutputReg(llvm::Value *V, bool SExt = false) override;

  llvm::Value *makeLoadWithOffset(llvm::Value *base, llvm::Value *offset,
                                  int size) override;

  llvm::Value *getIndexedElement(unsigned idx, unsigned eltSize,
                                 unsigned reg) override;

  void doCall(llvm::FunctionCallee FC, llvm::CallInst *llvmCI,
              const std::string &calleeName) override;
  void doIndirectCall();
  void checkStackAlignment();

  void lift(llvm::MCInst &I) override;
  llvm::Value *liftRoundingToInt(llvm::Value *src, int64_t mode);
  llvm::Value *liftRoundedFP(llvm::Intrinsic::ID id, llvm::Type *ty,
                             llvm::ArrayRef<llvm::Value *> operands,
                             int64_t mode);

  void checkCallingConv(llvm::Function *fn) override;

  void platformInit() override;

  unsigned branchInst() override;

  llvm::Value *enforceSExtZExt(llvm::Value *V, bool isSExt, bool isZExt);
  llvm::Value *checkIntegerABI(llvm::Value *V, llvm::Type *ty, bool isSExt,
                             bool isZExt);

  unsigned sentinelNOP() override;

  llvm::MCRegister stackPointerReg() override;
  llvm::MCRegister framePointerReg() override;
  std::optional<std::pair<int64_t, int64_t>>
  decodeCFAExpression(llvm::StringRef expr) override;

  void doReturn() override;
  void doReturn(llvm::Value *returnAddress);
  bool needsReturnTypeWidening() const override { return false; }
  CCAssigner::Target ccTarget() const override {
    return CCAssigner::Target::RISCV64;
  }

  void checkArgSupport(llvm::Argument &arg) override;
  void checkTypeSupport(llvm::Type *ty) override;

  bool isGOT(uint16_t spec) override {
    assert(false);
  }

  // Returns bitWidth corresponding the registers
  unsigned getRegSize(unsigned Reg);

  unsigned mapRegToBackingReg(unsigned Reg);
  llvm::Value *readFromReg(unsigned Reg, llvm::Type *ty);
  llvm::Value *readFromFPReg(unsigned Reg, llvm::Type *ty);
  llvm::Value *isNaNBoxed(unsigned Reg, unsigned width);
  llvm::Value *readFPABIReg(unsigned Reg, llvm::Type *ty);
  llvm::Value *readFromRegOperand(int idx, llvm::Type *ty);
  llvm::Value *readFromFPRegOperand(int idx, llvm::Type *ty,
                                    bool checkNaNBox = true);
  llvm::Value *readPtrFromRegOperand(int idx);
  llvm::Value *lookupReg(unsigned Reg);
  llvm::Value *lookupFPReg(unsigned Reg);
  void updateReg(llvm::Value *V, uint64_t reg);
  void updateFPReg(llvm::Value *V, uint64_t reg);
  llvm::Value *canonicalizeNaN(llvm::Value *V);
  llvm::Value *readFromImmOperand(int idx, unsigned immed_width,
                                  unsigned result_width);
  std::tuple<llvm::BasicBlock *, llvm::BasicBlock *>
  getBranchTargetsOperand(int op);
  llvm::Value *getPointerFromMCExpr();
  llvm::Value *getPointerOperand();
  std::vector<llvm::Value *> marshallArgs(llvm::FunctionType *fTy,
                                        const llvm::CallInst &llvmCI);

  // RVV register file helpers. Groups are named by the index (0-31) of
  // their first register; EMUL is passed as log2(EMUL), -3 through 3.
  bool isVReg(unsigned Reg);
  VRegRange vregRange(unsigned Reg);
  llvm::Value *lookupVReg(unsigned idx);
  llvm::Value *lookupVCSR(unsigned Reg);
  llvm::Value *getVLENB();
  unsigned numVElts(unsigned EEW, int EMULLog2);
  unsigned numVRegs(int EMULLog2);
  void checkVGroup(unsigned first, unsigned EEW, int EMULLog2);
  llvm::Value *readVRegs(unsigned first, unsigned count, unsigned EEW);
  void writeVRegs(unsigned first, unsigned count, llvm::Value *vec);
  llvm::Value *readVGroup(unsigned first, unsigned EEW, int EMULLog2);
  llvm::Value *readVMask(unsigned idx, unsigned numElts);
  llvm::Value *createUnknownBoolVec(unsigned numElts);
  llvm::Value *createBodyMask(llvm::Value *vl, unsigned numElts);
  void writeVGroup(unsigned first, unsigned EEW, int EMULLog2,
                   llvm::Value *result, llvm::Value *vl, llvm::Value *mask,
                   bool tailAgnostic, bool maskAgnostic,
                   llvm::Value *keep = nullptr,
                   llvm::Value *bodyLen = nullptr);
  void writeVMask(unsigned idx, llvm::Value *result, llvm::Value *vl,
                  llvm::Value *mask, bool maskAgnostic);

  // static vtype tracking and RVV instruction lifting
  // (riscv2llvm_vector.cpp)
  void computeVTypeDataflow();
  int64_t vtypeTransfer(const llvm::MCInst &I, int64_t in);
  std::optional<VConfig> decodeVType(uint64_t vtype);
  void enterVTypeBlock();
  VConfig getVConfig();
  [[noreturn]] void vectorError(const std::string &msg);
  void invalidateVectorState();
  llvm::Value *readVL();
  llvm::Value *computeVL(llvm::Value *avl, unsigned vlmax);
  llvm::Value *createVSplat(llvm::Value *scalar, unsigned numElts);
  llvm::Value *readVMaskOperand(int idx, unsigned numElts);
  void assertVAligned(llvm::Value *ptr, unsigned bytes, llvm::Value *cond);
  void liftVSet(unsigned opcode);
  void liftVWholeRegLoadStore(unsigned nregs, unsigned EEW, bool isStore);

  // single-width integer arithmetic: vd[i] = vs2[i] op src[i]
  enum class VIntOp {
    Add,
    Sub,
    RSub,
    And,
    Or,
    Xor,
    Sll,
    Srl,
    Sra,
    MinU,
    Min,
    MaxU,
    Max,
    Mul,
    MulH,
    MulHU,
    MulHSU,
    DivU,
    Div,
    RemU,
    Rem,
    AndN,
    Rol,
    Ror,
  };
  // a vector register operand, for checking the overlap rules: the first
  // register of its group, its EEW (1 for a mask), and its EMUL
  struct VOperand {
    unsigned first;
    unsigned EEW;
    int EMULLog2;
  };
  std::optional<unsigned> vgroupOperand(int idx);
  VOperand vopAt(int idx, unsigned EEW, int EMULLog2);
  void addVSrc(std::vector<VOperand> &srcs, int idx, unsigned EEW,
               int EMULLog2);
  unsigned vopLast(const VOperand &o);
  bool vopOverlaps(const VOperand &a, const VOperand &b);
  void checkVSources(const std::vector<VOperand> &srcs, bool readsV0Mask);
  bool checkVOverlap(VOperand dest, const std::vector<VOperand> &srcs,
                     bool readsV0Mask);
  void rejectVOverlap(VOperand dest, const std::vector<VOperand> &srcs);
  void checkVWidenable(const VConfig &cfg);
  llvm::Value *createVExt(llvm::Value *v, unsigned bits, bool isSigned);
  llvm::Value *readVOperand(int idx, const VConfig &cfg, bool zextImm);
  llvm::Value *createVIntOp(VIntOp op, llvm::Value *a, llvm::Value *b);
  void liftVIntBinary(VIntOp op);
  void liftVIntCompare(llvm::CmpInst::Predicate pred);
  void liftVMerge();

  // widening add, subtract, and multiply; U is unsigned, SU is signed vs2
  // times unsigned vs1/rs1
  enum class VWideOp { AddU, Add, SubU, Sub, MulU, Mul, MulSU, Sll };
  enum class VMacOp { Macc, Nmsac, Madd, Nmsub };
  void liftVWidenBinary(VWideOp op, bool wideVs2);
  void liftVExtend(unsigned factor, bool isSigned);
  // Zvbb elementwise unary operations
  enum class VUnaryOp { Brev, Brev8, Rev8, Clz, Ctz, Cpop };
  void liftVIntUnary(VUnaryOp op);
  void liftVNarrowShift(bool isArith);
  void liftVMulAdd(VMacOp op);
  void liftVWidenMulAdd(bool s1Signed, bool s2Signed);
  void liftVAdcSbc(bool isSub);
  void liftVMadcMsbc(bool isSub, bool hasCarryIn);

  enum class VAddrMode { Unit, Strided, Indexed };
  void liftVMemAccess(VAddrMode mode, unsigned EEW, unsigned nf, bool isStore,
                      bool ordered);

  // mask, scalar move, slide, gather, and reduction instructions
  enum class VMaskLogicOp { And, Nand, AndN, Xor, Or, Nor, OrN, Xnor };
  enum class VRedOp { Sum, MaxU, Max, MinU, Min, And, Or, Xor };
  llvm::Value *getVLaneIndices(unsigned n);
  llvm::Value *createVDynGather(llvm::Value *src, llvm::Value *idx,
                                llvm::Value *inRange);
  llvm::Value *createZExtOrTrunc64(llvm::Value *v);
  llvm::Value *createActiveMask(llvm::Value *mask, unsigned n);
  void writeVElt0(unsigned vd, llvm::Value *scalar, const VConfig &cfg);
  void liftVMaskLogical(VMaskLogicOp op);
  void liftVMaskScalar(bool isFirst);
  void liftVId();
  void liftVMvXS();
  void liftVMvSX();
  void liftVSlide(bool up);
  void liftVSlide1(bool up);
  void liftVRGather(bool ei16);
  void liftVReduce(VRedOp op);
  void liftVWidenReduce(bool isSigned);
  enum class VSetFirstOp { Before, Including, Only };
  void liftVMaskSetFirst(VSetFirstOp op);
  void liftVIota();
  void liftVCompress();

  bool liftVector(unsigned opcode);

  // vector floating point (riscv2llvm_vector_fp.cpp). elements are held
  // in the register file as integers and bitcast to FP for arithmetic
  enum class VFOp { Add, Sub, RSub, Mul, Div, RDiv, Min, Max, SgnJ, SgnJN,
                    SgnJX };
  enum class VFMacOp { Macc, NMacc, Msac, NMsac, Madd, NMadd, Msub, NMsub };
  static void vfMacFlags(VFMacOp op, bool &negProduct, bool &negAddend,
                         bool &overwritesMultiplicand);
  enum class VFRedOp { OSum, Min, Max };
  enum class VFCvtKind { FToI, FToIRtz, IToF, FToF };
  bool isFPRegOperand(int idx);
  void checkVFWidth(unsigned width);
  VConfig getVFConfig();
  llvm::Value *toVF(llvm::Value *v);
  llvm::Value *toVI(llvm::Value *v);
  llvm::Value *readVFScalar(int idx, unsigned SEW);
  llvm::Value *readVScalarBits(int idx, unsigned SEW);
  llvm::Value *readVFGroup(unsigned first, unsigned EEW, int EMULLog2);
  llvm::Value *readVFOperand(int idx, const VConfig &cfg);
  llvm::Value *createVFOp(VFOp op, llvm::Value *a, llvm::Value *b);
  llvm::Value *createVFConvert(llvm::Value *v, VFCvtKind kind, bool isSigned,
                               llvm::Type *toTy);
  void liftVFBinary(VFOp op);
  void liftVFUnary(bool isClass);
  void liftVFMulAdd(VFMacOp op);
  void liftVFCompare(llvm::CmpInst::Predicate pred);
  void liftVFMvFS();
  void liftVFCvt(VFCvtKind kind, bool isSigned, int widthChange);
  void liftVFWidenBinary(VFOp op, bool wideVs2);
  void liftVFWidenMulAdd(VFMacOp op);
  void liftVFReduce(VFRedOp op, bool widen);
  bool liftVectorFP(unsigned opcode);

  // moving vector arguments and return values between LLVM values and RVV
  // register groups; see backend_tv/abi.h
  llvm::Value *vectorElts(llvm::Value *v);
  llvm::Value *vectorFromElts(llvm::Value *v, llvm::VectorType *ty);
  void writeVectorArg(llvm::Value *arg, const ArgLoc &loc);
  llvm::Value *readVectorRet(llvm::VectorType *ty, const ArgLoc &loc);

public:
  riscv2llvm(llvm::Function *srcFn, std::unique_ptr<llvm::MemoryBuffer> MB,
             std::unordered_map<unsigned, llvm::Instruction *> &lineMap,
             std::ostream *out, const llvm::Target *Targ,
             llvm::Triple DefaultTT, const char *DefaultCPU,
             const char *DefaultFeatures);
};

} // end namespace lifter
