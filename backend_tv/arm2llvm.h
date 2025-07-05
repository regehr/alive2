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

#include "backend_tv/arm2llvm.h"
#include "backend_tv/lifter.h"
#include "backend_tv/mc2llvm.h"
#include "backend_tv/riscv2llvm.h"
#include "backend_tv/streamerwrapper.h"

#include <cmath>
#include <vector>

// avoid collisions with the upstream AArch64 namespace
namespace llvm::AArch64 {
const unsigned N = 100000000;
const unsigned Z = 100000001;
const unsigned C = 100000002;
const unsigned V = 100000003;
} // namespace llvm::AArch64

namespace lifter {

class arm2llvm final : public mc2llvm {
public:
  arm2llvm(llvm::Function *srcFn, std::unique_ptr<llvm::MemoryBuffer> MB,
           std::unordered_map<unsigned, llvm::Instruction *> &lineMap,
           std::ostream *out, const llvm::Target *Targ, llvm::Triple DefaultTT,
           const char *DefaultCPU, const char *DefaultFeatures);

  // Implemented library pseudocode for signed satuaration from A64 ISA manual
  std::tuple<llvm::Value *, bool> SignedSatQ(llvm::Value *i, unsigned bitWidth);

  // Implemented library pseudocode for unsigned satuaration from A64 ISA manual
  std::tuple<llvm::Value *, bool> UnsignedSatQ(llvm::Value *i,
                                               unsigned bitWidth);

  // Implemented library pseudocode for satuaration from A64 ISA manual
  std::tuple<llvm::Value *, bool> SatQ(llvm::Value *i, unsigned bitWidth,
                                       bool isSigned);

  bool isSIMDandFPRegOperand(llvm::MCOperand &op);

  llvm::Value *enforceSExtZExt(llvm::Value *V, bool isSExt, bool isZExt);

  std::tuple<llvm::Value *, int, llvm::Value *> getStoreParams();

  // Creates instructions to store val in memory pointed by base + offset
  // offset and size are in bytes
  void storeToMemoryImmOffset(llvm::Value *base, uint64_t offset, uint64_t size,
                              llvm::Value *val);

  unsigned decodeRegSet(unsigned r);

  llvm::Value *tblHelper2(std::vector<llvm::Value *> &tbl, llvm::Value *idx,
                          unsigned i);

  llvm::Value *tblHelper(std::vector<llvm::Value *> &tbl, llvm::Value *idx);

  std::tuple<llvm::Value *, int> getParamsLoadImmed();

  llvm::Value *makeLoadWithOffset(llvm::Value *base, llvm::Value *offset,
                                  int size) override;

  llvm::Value *makeLoadWithOffset(llvm::Value *base, int offset, unsigned size);

  std::tuple<llvm::Value *, llvm::Value *, llvm::Value *> getParamsStoreReg();

  void doIndirectCall();

  void doReturn() override;

  bool isGOT(uint16_t spec) override;

  std::tuple<llvm::Value *, llvm::Value *, llvm::Value *, llvm::Value *>
  FPCompare(llvm::Value *a, llvm::Value *b);

  std::tuple<llvm::Value *, llvm::Value *, llvm::Value *, llvm::Value *>
  splitImmNZCV(uint64_t imm_flags);

  bool disjoint(const std::set<int> &a, const std::set<int> &b);

  enum ExtendType { SXTB, SXTH, SXTW, SXTX, UXTB, UXTH, UXTW, UXTX };

  // Follows the "Library pseudocode for aarch64/instrs/extendreg/ExtendReg"
  // from ARM manual
  // val is always 64 bits and shiftAmt is always 0-4
  llvm::Value *extendAndShiftValue(llvm::Value *val, enum ExtendType extType,
                                   int shiftAmt);

  std::tuple<llvm::Value *, llvm::Value *> getParamsLoadReg();

  // From https://github.com/agustingianni/retools
  uint64_t VFPExpandImm(uint64_t imm8, unsigned N);

  // From https://github.com/agustingianni/retools
  // Implementation of: bits(64) AdvSIMDExpandImm(bit op, bits(4) cmode, bits(8)
  // imm8)
  uint64_t AdvSIMDExpandImm(unsigned op, unsigned cmode, unsigned imm8);

  std::vector<llvm::Value *> marshallArgs(llvm::FunctionType *fTy);

  void doCall(llvm::FunctionCallee FC, llvm::CallInst *llvmCI,
              const std::string &calleeName) override;

  unsigned branchInst() override;

  llvm::Value *conditionHolds(uint64_t cond);

  std::tuple<llvm::Value *, std::tuple<llvm::Value *, llvm::Value *,
                                       llvm::Value *, llvm::Value *>>
  addWithCarry(llvm::Value *l, llvm::Value *r, llvm::Value *carryIn);

  void setV(llvm::Value *V);

  void setZ(llvm::Value *V);

  void setN(llvm::Value *V);

  void setC(llvm::Value *V);

  void setZUsingResult(llvm::Value *V);

  void setNUsingResult(llvm::Value *V);

  llvm::Value *getV();

  llvm::Value *getZ();

  llvm::Value *getN();

  llvm::Value *getC();

  // Creates LLVM IR instructions which take two values with the same
  // number of bits, bit casting them to vectors of numElts elements
  // of size eltSize and doing an operation on them. In cases where
  // LLVM does not have an appropriate vector instruction, we perform
  // the operation element-wise.
  llvm::Value *
  createVectorOp(std::function<llvm::Value *(llvm::Value *, llvm::Value *)> op,
                 llvm::Value *a, llvm::Value *b, unsigned eltSize,
                 unsigned numElts, bool elementWise, extKind ext,
                 bool splatImm2, bool immShift, bool isUpper,
                 bool operandTypesDiffer);

  llvm::Value *getIndexedElement(unsigned idx, unsigned eltSize,
                                 unsigned reg) override;

  llvm::Value *getIndexedFPElement(unsigned idx, unsigned eltSize,
                                   unsigned reg);

  // Returns bitWidth corresponding the registers
  unsigned getRegSize(unsigned Reg);

  // Maps ARM registers to backing registers
  unsigned mapRegToBackingReg(unsigned Reg);

  // return pointer to the backing store for a register, doing the
  // necessary de-aliasing
  llvm::Value *dealiasReg(unsigned Reg);

  // always does a full-width read
  //
  // TODO eliminate all uses of this and rename readFromRegTyped to readFromReg
  llvm::Value *readFromRegOld(unsigned Reg);

  llvm::Value *readFromRegTyped(unsigned Reg, llvm::Type *ty);

  llvm::Value *readPtrFromReg(unsigned Reg);

  void updateReg(llvm::Value *V, uint64_t reg, bool SExt = false);

  llvm::Value *readInputReg(int idx);

  // FIXME: stop using this -- instructions should know what they're loading
  // FIXME: then remove getInstSize!
  // TODO: make it so that lshr generates code on register lookups
  // some instructions make use of this, and the semantics need to be
  // worked out
  llvm::Value *readFromOperand(int idx, unsigned size = 0);

  llvm::Value *readFromFPOperand(int idx, unsigned size);

  llvm::Value *readFromVecOperand(int idx, unsigned eltSize, unsigned numElts,
                                  bool isUpperHalf = false, bool isFP = false);

  void updateOutputReg(llvm::Value *V, bool SExt = false) override;

  llvm::Value *splatImm(llvm::Value *v, unsigned numElts, unsigned eltSize,
                        bool shift);

  static const std::set<int> s_flag;

  static const std::set<int> instrs_32;

  static const std::set<int> instrs_64;

  static const std::set<int> instrs_128;

  bool has_s(int instr);

  // decodeBitMasks - Decode a logical immediate value in the form
  // "N:immr:imms" (where the immr and imms fields are each 6 bits) into the
  // integer value it represents with regSize bits. Implementation of the
  // DecodeBitMasks function from the ARMv8 manual.
  //
  // WARNING: tmask is untested
  std::pair<uint64_t, uint64_t> decodeBitMasks(uint64_t val, unsigned regSize);

  unsigned getInstSize(int instr);

  // from getShiftType/getShiftValue:
  // https://github.com/llvm/llvm-project/blob/93d1a623cecb6f732db7900baf230a13e6ac6c6a/llvm/lib/Target/AArch64/MCTargetDesc/AArch64AddressingModes.h#L74
  llvm::Value *regShift(llvm::Value *value, int encodedShift);

  llvm::AllocaInst *get_reg(aslp::reg_t regtype, uint64_t num) override;

  void platformInit() override;

  void checkArgSupport(llvm::Argument &arg) override;
  void checkFuncSupport(llvm::Function &func) override;
  void checkTypeSupport(llvm::Type *ty) override;

  unsigned sentinelNOP() override;

  std::optional<aslp::opcode_t> getArmOpcode(const llvm::MCInst &I);

  void lift(llvm::MCInst &I) override;
  void lift_add(unsigned opcode);
  void lift_adc_sbc(unsigned opcode);
  void lift_branch();
  void lift_bcc();
  void lift_mrs();
  void lift_msr();
  void lift_asrv(unsigned opcode);
  void lift_sub(unsigned opcode);
  void lift_csel(unsigned opcode);
  void lift_and(unsigned opcode);
  void lift_madd(unsigned opcode);
  void lift_umadd(unsigned opcode);
  void lift_smaddl();
  void lift_msubl(unsigned opcode);
  void lift_mulh(unsigned opcode);
  void lift_msub();
  void lift_sbfm(unsigned opcode);
  void lift_ccmp(unsigned opcode);
  void lift_eori(unsigned opcode);
  void lift_eorr();
  void lift_ccmn();
  void lift_csinv_csneg(unsigned opcode);
  void lift_csinc(unsigned opcode);
  void lift_movz(unsigned opcode);
  void lift_movn();
  void lift_movk(unsigned opcode);
  void lift_bfm(unsigned opcode);
  void lift_ubfm(unsigned opcode);
  void lift_orn();
  void lift_lsrv();
  void lift_lslv();
  void lift_orri(unsigned opcode);
  void lift_orrr();
  void lift_udiv(unsigned opcode);
  void lift_sdiv(unsigned opcode);
  void lift_eon_bic(unsigned opcode);
  void lift_clz();
  void lift_rev();
  void lift_rbit();
  void lift_ror();
  void lift_extr();
  void lift_rev16(unsigned opcode);
  void lift_rev32(unsigned opcode);
  void lift_ld1r(unsigned opcode);
  void lift_ld1(unsigned opcode);
  void lift_ldr3(unsigned opcode);
  void lift_ldr2(unsigned opcode);
  void lift_ldu1(unsigned opcode);
  void lift_ldr1(unsigned opcode);
  void lift_ld2(unsigned opcode);
  void lift_str_1(unsigned opcode);
  void lift_str_2(unsigned opcode);
  void lift_str_3(unsigned opcode);
  void lift_str_4(unsigned opcode);
  void lift_str_5(unsigned opcode);
  void lift_stp_1(unsigned opcode);
  void lift_stp_2(unsigned opcode);
  void lift_ldp_1(unsigned opcode);
  void lift_ldp_2(unsigned opcode);
  void lift_adrp();
  void lift_cbz();
  void lift_tbz(unsigned opcode);
  void lift_cbnz();
  void lift_ins_gpr(unsigned opcode);
  void lift_ins_lane(unsigned opcode);
  void lift_fneg();
  void lift_vec_fneg(unsigned opcode);
  void lift_fcvt_1(unsigned opcode);
  void lift_fcvt_2(unsigned opcode);
  void lift_fcvt_3();
  void lift_fcvt_4();
  void lift_frint(unsigned opcode);
  void lift_cvtf_1(unsigned opcode);
  void lift_cvtf_2(unsigned opcode);
  void lift_fmov_1();
  void lift_fmov_2(unsigned opcode);
  void lift_fmov_3(unsigned opcode);
  void lift_fabs();
  void lift_fminmax(unsigned opcode);
  void lift_fpbinop(unsigned opcode);
  void lift_fmul_idx(unsigned opcode);
  void lift_fmla_mls(unsigned opcode);
  void lift_fnm(unsigned opcode);
  void lift_fsqrt();
  void lift_vec_fpbinop(unsigned opcode);
  void lift_fcmp(unsigned opcode);
  void lift_fccmp();
  void lift_smov_vi(unsigned opcode);
  void lift_umov_vi(unsigned opcode);
  void lift_mvni(unsigned opcode);
  void lift_mvni_msl(unsigned opcode);
  void lift_movi_msl(unsigned opcode);
  void lift_movi_1();
  void lift_movi_2();
  void lift_movi_3();
  void lift_movi_4();
  void lift_movi_5();
  void lift_movi_6();
  void lift_movi_7();
  void lift_ext_1();
  void lift_ext_2();
  void lift_rev64_1();
  void lift_rev64_2();
  void lift_rev64_3();
  void lift_rev64_4();
  void lift_rev64_5();
  void lift_rev64_6();
  void lift_rev32_1();
  void lift_rev32_2();
  void lift_rev32_3();
  void lift_rev32_4();
  void lift_rev16_1();
  void lift_rev16_2();
  void lift_dup8();
  void lift_dup16();
  void lift_dup32();
  void lift_dup64();
  void lift_dup88();
  void lift_dup168();
  void lift_dup816();
  void lift_dup416();
  void lift_dup432();
  void lift_dup232();
  void lift_dup264();
  void lift_dup232lane();
  void lift_dup264lane();
  void lift_dup416lane();
  void lift_dup432lane();
  void lift_dup88lane();
  void lift_dup816lane();
  void lift_dup168lane();
  void lift_bif1();
  void lift_bif2();
  void lift_bif3();
  void lift_fcm(unsigned opcode);
  void lift_cm(unsigned opcode);
  void lift_tbl(unsigned opcode);
  void lift_xhn(unsigned opcode);
  void lift_vec_binop(unsigned opcode);
  void lift_addp();
  void lift_uzp(unsigned opcode);
  void lift_vec_mul(unsigned opcode);
  void lift_vec_mls(unsigned opcode);
  void lift_vec_mla(unsigned opcode);
  void lift_trn(unsigned opcode);
  void lift_vec_minmax(unsigned upcode);
  void lift_more_vec_binops(unsigned opcode);
  void lift_shll(unsigned opcode);
  void lift_shrn(unsigned opcode);
  void lift_sli_sri(unsigned opcode);
  void lift_mls(unsigned opcode);
  void lift_mla(unsigned opcode);
  void lift_abal_abdl(unsigned opcode);
  void lift_mlal_mlsl_idx(unsigned opcode);
  void lift_mlal_mlsl(unsigned opcode);
  void lift_aba_abd(unsigned opcode);
  void lift_ssra(unsigned opcode);
  void lift_usra(unsigned opcode);
  void lift_zip1(unsigned opcode);
  void lift_zip2(unsigned opcode);
  void lift_addp(unsigned opcode);
  void lift_xtn(unsigned opcode);
  void lift_qxtn(unsigned opcode);
  void lift_unary_vec(unsigned opcode);
  void lift_uqadd(unsigned opcode);
  void lift_uqsub(unsigned opcode);
  void lift_sqadd(unsigned opcode);
  void lift_sqsub(unsigned opcode);
  void lift_mull(unsigned opcode);
};

} // end namespace lifter
