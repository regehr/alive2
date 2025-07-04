#include "backend_tv/arm2llvm.h"

#include "Target/AArch64/MCTargetDesc/AArch64MCAsmInfo.h"

const bool EXTRA_ABI_CHECKS = false;

using namespace std;
using namespace lifter;
using namespace llvm;

unsigned arm2llvm::branchInst() {
  return AArch64::B;
}

unsigned arm2llvm::sentinelNOP() {
  return AArch64::SEH_Nop;
}

arm2llvm::arm2llvm(Function *srcFn, unique_ptr<MemoryBuffer> MB,
                   unordered_map<unsigned, Instruction *> &lineMap,
                   ostream *out, const Target *Targ, Triple DefaultTT,
                   const char *DefaultCPU, const char *DefaultFeatures)
    : mc2llvm(srcFn, std::move(MB), lineMap, out, Targ, DefaultTT, DefaultCPU,
              DefaultFeatures) {
  // sanity checking
  assert(disjoint(instrs_32, instrs_64));
  assert(disjoint(instrs_32, instrs_128));
  assert(disjoint(instrs_64, instrs_128));
  *out << (instrs_32.size() + instrs_64.size() + instrs_128.size())
       << " AArch64 instructions supported\n";
}

// Implemented library pseudocode for signed satuaration from A64 ISA manual
tuple<Value *, bool> arm2llvm::SignedSatQ(Value *i, unsigned bitWidth) {
  auto W = getBitWidth(i);
  assert(bitWidth < W);
  auto max = getSignedIntConst(((uint64_t)1 << (bitWidth - 1)) - 1, W);
  auto min = getSignedIntConst(-((uint64_t)1 << (bitWidth - 1)), W);
  auto *max_bitWidth = getSignedMaxConst(bitWidth);
  auto *min_bitWidth = getSignedMinConst(bitWidth);
  Value *i_bitWidth = createTrunc(i, getIntTy(bitWidth));

  auto sat = createOr(createICmp(ICmpInst::Predicate::ICMP_SGT, i, max),
                      createICmp(ICmpInst::Predicate::ICMP_SLT, i, min));
  auto max_or_min =
      createSelect(createICmp(ICmpInst::Predicate::ICMP_SGT, i, max),
                   max_bitWidth, min_bitWidth);

  auto res = createSelect(sat, max_or_min, i_bitWidth);
  return make_pair(res, sat);
}

// Implemented library pseudocode for unsigned satuaration from A64 ISA manual
tuple<Value *, bool> arm2llvm::UnsignedSatQ(Value *i, unsigned bitWidth) {
  auto W = getBitWidth(i);
  assert(bitWidth < W);
  auto max = getUnsignedIntConst(((uint64_t)1 << bitWidth) - 1, W);
  auto min = getUnsignedIntConst(0, W);
  Value *max_bitWidth = ConstantInt::get(Ctx, APInt::getMaxValue(bitWidth));
  Value *min_bitWidth = ConstantInt::get(Ctx, APInt::getMinValue(bitWidth));
  Value *i_bitWidth = createTrunc(i, getIntTy(bitWidth));

  auto sat = createOr(createICmp(ICmpInst::Predicate::ICMP_UGT, i, max),
                      createICmp(ICmpInst::Predicate::ICMP_ULT, i, min));
  auto max_or_min =
      createSelect(createICmp(ICmpInst::Predicate::ICMP_UGT, i, max),
                   max_bitWidth, min_bitWidth);

  auto res = createSelect(sat, max_or_min, i_bitWidth);
  return make_pair(res, sat);
}

// Implemented library pseudocode for satuaration from A64 ISA manual
tuple<Value *, bool> arm2llvm::SatQ(Value *i, unsigned bitWidth,
                                    bool isSigned) {
  auto [result, sat] =
      isSigned ? SignedSatQ(i, bitWidth) : UnsignedSatQ(i, bitWidth);

  return make_pair(result, sat);
}

bool arm2llvm::isSIMDandFPRegOperand(MCOperand &op) {
  assert(op.isReg() && "[isSIMDandFPRegOperand] expected register operand");
  unsigned reg = op.getReg();
  return (reg >= AArch64::Q0 && reg <= AArch64::Q31) ||
         (reg >= AArch64::D0 && reg <= AArch64::D31) ||
         (reg >= AArch64::S0 && reg <= AArch64::S31);
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
Value *arm2llvm::enforceSExtZExt(Value *V, bool isSExt, bool isZExt) {
  assert(!(isSExt && isZExt));
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
        createRawShl(ext1, getUnsignedIntConst(getBitWidth(V), targetWidth));
    auto ext2 = createZExt(V, getIntTy(targetWidth));
    V = createOr(shifted, ext2);
  }

  return V;
}

tuple<Value *, int, Value *> arm2llvm::getStoreParams() {
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
    return make_tuple(baseAddr, op2.getImm(), readFromRegOld(op0.getReg()));
  } else {
    assert(op2.isExpr());
    auto [globalVar, _] = getExprVar(op2.getExpr());
    return make_tuple(globalVar, 0, readFromRegOld(op0.getReg()));
  }
}

// Creates instructions to store val in memory pointed by base + offset
// offset and size are in bytes
void arm2llvm::storeToMemoryImmOffset(Value *base, uint64_t offset,
                                      uint64_t size, Value *val) {
  // Get offset as a 64-bit LLVM constant
  auto offsetVal = getUnsignedIntConst(offset, 64);

  // Create a GEP instruction based on a byte addressing basis (8 bits)
  // returning pointer to base + offset
  assert(base);
  auto ptr = createGEP(getIntTy(8), base, {offsetVal}, "");

  // Store Value val in the pointer returned by the GEP instruction
  createStore(val, ptr);
}

unsigned arm2llvm::decodeRegSet(unsigned r) {
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

Value *arm2llvm::tblHelper2(vector<Value *> &tbl, Value *idx, unsigned i) {
  if (i == tbl.size())
    return getUnsignedIntConst(0, 8);
  auto cond = createICmp(ICmpInst::Predicate::ICMP_ULT, idx,
                         getUnsignedIntConst((i + 1) * 16, 8));
  auto adjIdx = createSub(idx, getUnsignedIntConst(i * 16, 8));
  auto t = createExtractElement(tbl.at(i), adjIdx);
  auto f = tblHelper2(tbl, idx, i + 1);
  return createSelect(cond, t, f);
}

Value *arm2llvm::tblHelper(vector<Value *> &tbl, Value *idx) {
  return tblHelper2(tbl, idx, 0);
}

tuple<Value *, int> arm2llvm::getParamsLoadImmed() {
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

Value *arm2llvm::makeLoadWithOffset(Value *base, Value *offset, int size) {
  // Create a GEP instruction based on a byte addressing basis (8 bits)
  // returning pointer to base + offset
  assert(base);
  auto ptr = createGEP(getIntTy(8), base, {offset}, "");

  // Load Value val in the pointer returned by the GEP instruction
  Value *loaded = createLoad(getIntTy(8 * size), ptr);

  *out << "[makeLoadWithOffset size = " << size << "]\n";

  // FIXME -- this code should not be needed, the ABI guarantees
  // that loading an i1 should have the rest of the bits clear
  if (false && size == 1) {
    *out << "[it's a 1-byte load]\n";
    if (auto llvmInst = getCurLLVMInst()) {
      if (auto LI = dyn_cast<LoadInst>(llvmInst)) {
        if (LI->getType() == getIntTy(1)) {
          *out << "[following ABI rules for i1]\n";
          loaded = createAnd(loaded, getUnsignedIntConst(1, 8));
        } else {
          *out << "[LLVM inst for this ARM load isn't a load i1]\n";
        }
      } else {
        *out << "[LLVM inst for this ARM load is not a load]\n";
      }
    } else {
      *out << "[can't find an LLVM inst for this ARM load]\n";
    }
  }

  return loaded;
}

Value *arm2llvm::makeLoadWithOffset(Value *base, int offset, unsigned size) {
  return makeLoadWithOffset(base, getUnsignedIntConst(offset, 64), size);
}

tuple<Value *, Value *, Value *> arm2llvm::getParamsStoreReg() {
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
         (offsetReg == AArch64::LR) || (offsetReg == AArch64::FP) ||
         (offsetReg == AArch64::XZR) ||
         (offsetReg >= AArch64::W0 && offsetReg <= AArch64::W30) ||
         (offsetReg == AArch64::WZR));

  int extTyp = 0, shiftAmt;
  if ((offsetReg >= AArch64::W0 && offsetReg <= AArch64::W30) ||
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
  auto offset = extendAndShiftValue(readFromRegOld(offsetReg),
                                    (ExtendType)extTyp, shiftAmt);

  return make_tuple(baseAddr, offset, readFromRegOld(op0.getReg()));
}

void arm2llvm::doIndirectCall() {
  if (auto llvmInst = getCurLLVMInst()) {
    if (auto CI = dyn_cast<CallInst>(llvmInst)) {
      if (!CI->isIndirectCall()) {
        *out << "OOPS: expected BR/BLR to map to an indirect call\n\n";
        exit(-1);
      }
      auto reg = CurInst->getOperand(0).getReg();
      auto fnPtr = readPtrFromReg(reg);
      FunctionCallee FC(CI->getFunctionType(), fnPtr);
      doCall(FC, CI, "");
    } else {
      *out << "OOPS: debuginfo gave us something that's not a callinst\n";
      *out << "Can't process BR/BLR instruction\n\n";
      exit(-1);
    }
  } else {
    *out << "OOPS: no debuginfo mapping exists\n";
    *out << "Can't process BR/BLR instruction\n\n";
    exit(-1);
  }
}

void arm2llvm::doReturn() {
  auto i32 = getIntTy(32);
  auto i64 = getIntTy(64);

  if (EXTRA_ABI_CHECKS) {
    /*
     * ABI stuff: on all return paths, check that callee-saved +
     * other registers have been reset to their previous
     * values. these values were saved at the top of the function so
     * the trivially dominate all returns
     */
    // FIXME: check callee-saved vector registers
    // FIXME: make sure code doesn't touch 16, 17?
    // FIXME: check FP and LR?
    assertSame(initialSP, readFromRegTyped(AArch64::SP, getIntTy(64)));
    for (unsigned r = 19; r <= 28; ++r)
      assertSame(initialReg[r],
                 readFromRegTyped(AArch64::X0 + r, getIntTy(64)));
  }

  auto *retTyp = srcFn->getReturnType();
  if (retTyp->isVoidTy()) {
    createReturn(nullptr);
  } else {
    Value *retVal = nullptr;
    if (retTyp->isVectorTy() || retTyp->isFloatingPointTy()) {
      retVal = readFromRegTyped(AArch64::Q0, retTyp);
    } else {
      retVal = readFromRegOld(AArch64::X0);
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

tuple<Value *, Value *, Value *, Value *> arm2llvm::FPCompare(Value *a,
                                                              Value *b) {
  return {
      createFCmp(FCmpInst::Predicate::FCMP_OLT, a, b),
      createFCmp(FCmpInst::Predicate::FCMP_OEQ, a, b),
      createFCmp(FCmpInst::Predicate::FCMP_UGT, a, b),
      createFCmp(FCmpInst::Predicate::FCMP_UNO, a, b),
  };
}

tuple<Value *, Value *, Value *, Value *>
arm2llvm::splitImmNZCV(uint64_t imm_flags) {
  assert(imm_flags < 16);
  return {
      getBoolConst((imm_flags & 8) != 0),
      getBoolConst((imm_flags & 4) != 0),
      getBoolConst((imm_flags & 2) != 0),
      getBoolConst((imm_flags & 1) != 0),
  };
}

bool arm2llvm::disjoint(const set<int> &a, const set<int> &b) {
  set<int> i;
  set_intersection(a.begin(), a.end(), b.begin(), b.end(),
                   inserter(i, i.begin()));
  return i.empty();
}

// Follows the "Library pseudocode for aarch64/instrs/extendreg/ExtendReg"
// from ARM manual
// val is always 64 bits and shiftAmt is always 0-4
Value *arm2llvm::extendAndShiftValue(Value *val, enum ExtendType extType,
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
    val = createCast(val, ty, isSigned ? Instruction::SExt : Instruction::ZExt);
  }

  // shift may not be there, it may just be the extend
  if (shiftAmt != 0)
    val = createMaskedShl(val, getUnsignedIntConst(shiftAmt, size));

  return val;
}

tuple<Value *, Value *> arm2llvm::getParamsLoadReg() {
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
         (offsetReg == AArch64::LR) || (offsetReg == AArch64::FP) ||
         (offsetReg == AArch64::SP) || (offsetReg == AArch64::XZR) ||
         (offsetReg >= AArch64::W0 && offsetReg <= AArch64::W30) ||
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
  auto offset = extendAndShiftValue(readFromRegTyped(offsetReg, getIntTy(64)),
                                    (ExtendType)extTyp, shiftAmt);

  return make_pair(baseAddr, offset);
}

static uint64_t Replicate32x2(uint64_t bits32) {
  return (bits32 << 32) | bits32;
}

static uint64_t Replicate16x4(uint64_t bits16) {
  return Replicate32x2((bits16 << 16) | bits16);
}

static uint64_t Replicate8x8(uint64_t bits8) {
  return Replicate16x4((bits8 << 8) | bits8);
}

static uint64_t replicateBit(uint64_t bit, int N) {
  if (!bit)
    return 0;
  if (N == 64)
    return 0xffffffffffffffffLL;
  return (1ULL << N) - 1;
}

// From https://github.com/agustingianni/retools
uint64_t arm2llvm::VFPExpandImm(uint64_t imm8, unsigned N) {
  unsigned E = ((N == 32) ? 8 : 11) - 2; // E in {6, 9}
  unsigned F = N - E - 1;                // F in {25, 54}
  uint64_t imm8_6 = (imm8 >> 6) & 1;     // imm8<6>
  uint64_t sign = (imm8 >> 7) & 1;       // imm8<7>
  // NOT(imm8<6>):Replicate(imm8<6>,{5, 8})
  uint64_t exp = ((imm8_6 ^ 1) << (E - 1)) | replicateBit(imm8_6, E - 1);
  // imm8<5:0> : Zeros({19, 48})
  uint64_t frac = ((imm8 & 0x3f) << (F - 6)) | replicateBit(0, F - 6);
  uint64_t res = (sign << (E + F)) | (exp << F) | frac;
  return res;
}

// From https://github.com/agustingianni/retools
// Implementation of: bits(64) AdvSIMDExpandImm(bit op, bits(4) cmode, bits(8)
// imm8)
uint64_t arm2llvm::AdvSIMDExpandImm(unsigned op, unsigned cmode,
                                    unsigned imm8) {
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
                       (replicateBit(imm8_6, 5) << (6 + 19)) | (imm8_50 << 19);
      imm64 = Replicate32x2(imm32);
    }

    if ((cmode & 1) == 1 && op == 1) {
      // imm64 =
      // imm8<7>:NOT(imm8<6>):Replicate(imm8<6>,8):imm8<5:0>:Zeros(48);
      uint64_t imm8_7 = (imm8 >> 7) & 1;
      uint64_t imm8_6 = (imm8 >> 6) & 1;
      uint64_t imm8_50 = imm8 & 63;
      imm64 = (imm8_7 << 63) | ((imm8_6 ^ 1) << 62) |
              (replicateBit(imm8_6, 8) << 54) | (imm8_50 << 48);
    }
    break;
  default:
    abort();
  }

  return imm64;
}

vector<Value *> arm2llvm::marshallArgs(FunctionType *fTy) {
  *out << "entering marshallArgs()\n";
  assert(fTy);
  if (fTy->getReturnType()->isStructTy()) {
    *out << "\nERROR: we don't support structures in return values yet\n\n";
    exit(-1);
  }
  if (fTy->getReturnType()->isArrayTy()) {
    *out << "\nERROR: we don't support arrays in return values yet\n\n";
    exit(-1);
  }
  unsigned vecArgNum = 0;
  unsigned scalarArgNum = 0;
  unsigned stackSlot = 0;
  vector<Value *> args;
  for (auto arg = fTy->param_begin(); arg != fTy->param_end(); ++arg) {
    Type *argTy = *arg;
    assert(argTy);
    *out << "  vecArgNum = " << vecArgNum << " scalarArgNum = " << scalarArgNum
         << "\n";
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
      if (vecArgNum < 8) {
        param = readFromRegTyped(AArch64::Q0 + vecArgNum, argTy);
        ++vecArgNum;
      } else {
        auto sz = getBitWidth(argTy);
        if (sz > 64 && ((stackSlot % 2) != 0)) {
          ++stackSlot;
          *out << "aligning stack slot for large vector parameter\n";
        }
        *out << "vector parameter going on stack with size = " << sz << "\n";
        auto SP = readPtrFromReg(AArch64::SP);
        auto addr = createGEP(getIntTy(64), SP,
                              {getUnsignedIntConst(stackSlot, 64)}, nextName());
        param = createBitCast(createLoad(getIntTy(sz), addr), argTy);
        ++stackSlot;
        if (sz > 64)
          ++stackSlot;
      }
    } else if (argTy->isIntegerTy() || argTy->isPointerTy()) {
      // FIXME check signext and zeroext
      if (scalarArgNum < 8) {
        param = readFromRegTyped(AArch64::X0 + scalarArgNum, getIntTy(64));
        ++scalarArgNum;
      } else {
        auto SP = readPtrFromReg(AArch64::SP);
        auto addr = createGEP(getIntTy(64), SP,
                              {getUnsignedIntConst(stackSlot, 64)}, nextName());
        param = createLoad(getIntTy(64), addr);
        ++stackSlot;
      }
      if (argTy->isPointerTy()) {
        param = new IntToPtrInst(param, PointerType::get(Ctx, 0), "", LLVMBB);
      } else {
        assert(argTy->getIntegerBitWidth() <= 64);
        if (argTy->getIntegerBitWidth() < 64)
          param = createTrunc(param, getIntTy(argTy->getIntegerBitWidth()));
      }
    } else {
      assert(false && "unknown arg type\n");
    }
    args.push_back(param);
  }
  *out << "marshalled up " << args.size() << " arguments\n";
  return args;
}

void arm2llvm::doCall(FunctionCallee FC, CallInst *llvmCI,
                      const string &calleeName) {
  *out << "entering doCall()\n";

  for (auto &arg : FC.getFunctionType()->params()) {
    if (auto vTy = dyn_cast<VectorType>(arg))
      checkVectorTy(vTy);
  }
  if (auto RT = dyn_cast<VectorType>(FC.getFunctionType()->getReturnType()))
    checkVectorTy(RT);

  auto args = marshallArgs(FC.getFunctionType());

  // ugh -- these functions have an LLVM "immediate" as their last
  // argument; this is not present in the assembly at all, we have
  // to provide it by hand
  if (calleeName == "llvm.memset.p0.i64" ||
      calleeName == "llvm.memset.p0.i32" ||
      calleeName == "llvm.memcpy.p0.p0.i64" ||
      calleeName == "llvm.memmove.p0.p0.i64")
    args[3] = getBoolConst(false);

  auto CI = CallInst::Create(FC, args, "", LLVMBB);

  bool sext{false}, zext{false};

  assert(llvmCI);
  if (llvmCI->hasFnAttr(Attribute::NoReturn)) {
    auto a = CI->getAttributes();
    auto a2 = a.addFnAttribute(Ctx, Attribute::NoReturn);
    CI->setAttributes(a2);
  }
  // NB we have to check for both function attributes and call site
  // attributes
  if (llvmCI->hasRetAttr(Attribute::SExt))
    sext = true;
  if (llvmCI->hasRetAttr(Attribute::ZExt))
    zext = true;
  auto calledFn = llvmCI->getCalledFunction();
  if (calledFn) {
    if (calledFn->hasRetAttribute(Attribute::SExt))
      sext = true;
    if (calledFn->hasRetAttribute(Attribute::ZExt))
      zext = true;
  }

  auto RV = enforceSExtZExt(CI, sext, zext);

  // invalidate machine state that is not guaranteed to be preserved across a
  // call
  invalidateReg(AArch64::N, 1);
  invalidateReg(AArch64::Z, 1);
  invalidateReg(AArch64::C, 1);
  invalidateReg(AArch64::V, 1);
  for (unsigned reg = 9; reg <= 15; ++reg)
    invalidateReg(AArch64::X0 + reg, 64);

  auto retTy = FC.getFunctionType()->getReturnType();
  if (retTy->isIntegerTy() || retTy->isPointerTy()) {
    updateReg(RV, AArch64::X0);
  } else if (retTy->isFloatingPointTy() || retTy->isVectorTy()) {
    updateReg(RV, AArch64::Q0);
  } else {
    assert(retTy->isVoidTy());
  }
}

Value *arm2llvm::conditionHolds(uint64_t cond) {
  assert(cond < 16);

  // cond<0> == '1' && cond != '1111'
  auto invert_bit = (cond & 1) && (cond != 15);

  cond >>= 1;

  auto cur_v = getV();
  auto cur_z = getZ();
  auto cur_n = getN();
  auto cur_c = getC();

  auto falseVal = getBoolConst(false);
  auto trueVal = getBoolConst(true);

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
    res = createNot(res);

  return res;
}

tuple<Value *, tuple<Value *, Value *, Value *, Value *>>
arm2llvm::addWithCarry(Value *l, Value *r, Value *carryIn) {
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

  auto zero = getUnsignedIntConst(0, size);
  auto res = createTrunc(unsignedSum, ty);

  auto newN = createICmp(ICmpInst::Predicate::ICMP_SLT, res, zero);
  auto newZ = createICmp(ICmpInst::Predicate::ICMP_EQ, res, zero);
  auto newC = createICmp(ICmpInst::Predicate::ICMP_NE, unsignedSum,
                         createZExt(res, tyPlusOne));
  auto newV = createICmp(ICmpInst::Predicate::ICMP_NE, signedSum,
                         createSExt(res, tyPlusOne));

  return {res, {newN, newZ, newC, newV}};
};

void arm2llvm::setV(Value *V) {
  assert(getBitWidth(V) == 1);
  updateReg(V, AArch64::V);
}

void arm2llvm::setZ(Value *V) {
  assert(getBitWidth(V) == 1);
  updateReg(V, AArch64::Z);
}

void arm2llvm::setN(Value *V) {
  assert(getBitWidth(V) == 1);
  updateReg(V, AArch64::N);
}

void arm2llvm::setC(Value *V) {
  assert(getBitWidth(V) == 1);
  updateReg(V, AArch64::C);
}

void arm2llvm::setZUsingResult(Value *V) {
  auto W = getBitWidth(V);
  auto zero = getUnsignedIntConst(0, W);
  auto z = createICmp(ICmpInst::Predicate::ICMP_EQ, V, zero);
  setZ(z);
}

void arm2llvm::setNUsingResult(Value *V) {
  auto W = getBitWidth(V);
  auto zero = getUnsignedIntConst(0, W);
  auto n = createICmp(ICmpInst::Predicate::ICMP_SLT, V, zero);
  setN(n);
}

Value *arm2llvm::getV() {
  return createLoad(getIntTy(1), dealiasReg(AArch64::V));
}

Value *arm2llvm::getZ() {
  return createLoad(getIntTy(1), dealiasReg(AArch64::Z));
}

Value *arm2llvm::getN() {
  return createLoad(getIntTy(1), dealiasReg(AArch64::N));
}

Value *arm2llvm::getC() {
  return createLoad(getIntTy(1), dealiasReg(AArch64::C));
}

// Creates LLVM IR instructions which take two values with the same
// number of bits, bit casting them to vectors of numElts elements
// of size eltSize and doing an operation on them. In cases where
// LLVM does not have an appropriate vector instruction, we perform
// the operation element-wise.
Value *arm2llvm::createVectorOp(function<Value *(Value *, Value *)> op,
                                Value *a, Value *b, unsigned eltSize,
                                unsigned numElts, bool elementWise, extKind ext,
                                bool splatImm2, bool immShift, bool isUpper,
                                bool operandTypesDiffer) {
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

Value *arm2llvm::getIndexedElement(unsigned idx, unsigned eltSize,
                                   unsigned reg) {
  assert(getRegSize(reg) == 128 && "Expected 128-bit register");
  auto *ty = getVecTy(eltSize, 128 / eltSize);
  auto *r = readFromRegTyped(reg, ty);
  return createExtractElement(r, idx);
}

Value *arm2llvm::getIndexedFPElement(unsigned idx, unsigned eltSize,
                                     unsigned reg) {
  assert(getRegSize(reg) == 128 && "Expected 128-bit register");
  auto *element = getIndexedElement(idx, eltSize, reg);
  return createBitCast(element, getFPType(eltSize));
}

// Returns bitWidth corresponding the registers
unsigned arm2llvm::getRegSize(unsigned Reg) {
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
unsigned arm2llvm::mapRegToBackingReg(unsigned Reg) {
  if (Reg == AArch64::WZR)
    return AArch64::XZR;
  else if (Reg == AArch64::W29)
    return AArch64::FP;
  else if (Reg == AArch64::W30)
    return AArch64::LR;
  else if (Reg == AArch64::WSP)
    return AArch64::SP;
  else if (Reg >= AArch64::W0 && Reg <= AArch64::W30)
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
  assert(RegFile[Reg] && "ERROR: Cannot have a register without a backing store"
                         " register corresponding it.");
  return Reg;
}

// return pointer to the backing store for a register, doing the
// necessary de-aliasing
Value *arm2llvm::dealiasReg(unsigned Reg) {
  auto RegAddr = RegFile[mapRegToBackingReg(Reg)];
  assert(RegAddr);
  return RegAddr;
}

// always does a full-width read
//
// TODO eliminate all uses of this and rename readFromRegTyped to readFromReg
Value *arm2llvm::readFromRegOld(unsigned Reg) {
  auto RegAddr = dealiasReg(Reg);
  return createLoad(getIntTy(getRegSize(mapRegToBackingReg(Reg))), RegAddr);
}

Value *arm2llvm::readFromRegTyped(unsigned Reg, Type *ty) {
  auto RegAddr = dealiasReg(Reg);
  return createLoad(ty, RegAddr);
}

Value *arm2llvm::readPtrFromReg(unsigned Reg) {
  auto RegAddr = dealiasReg(Reg);
  return createLoad(PointerType::get(Ctx, 0), RegAddr);
}

void arm2llvm::updateReg(Value *V, uint64_t reg, bool SExt) {
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

Value *arm2llvm::readInputReg(int idx) {
  auto op = CurInst->getOperand(idx);
  assert(op.isReg());
  auto reg = op.getReg();
  auto regSize = getRegSize(reg);
  return readFromRegTyped(reg, getIntTy(regSize));
}

// FIXME: stop using this -- instructions should know what they're loading
// FIXME: then remove getInstSize!
// TODO: make it so that lshr generates code on register lookups
// some instructions make use of this, and the semantics need to be
// worked out
Value *arm2llvm::readFromOperand(int idx, unsigned size) {
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
    V = getUnsignedIntConst(op.getImm(), size);
  } else if (op.isReg()) {
    V = readFromRegOld(op.getReg());
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

Value *arm2llvm::readFromFPOperand(int idx, unsigned size) {
  assert((size == 16 || size == 32 || size == 64) &&
         "Only 16, 32 and 64 bit FP regs");
  auto val = readFromOperand(idx, size);
  return createBitCast(val, getFPType(size));
}

Value *arm2llvm::readFromVecOperand(int idx, unsigned eltSize, unsigned numElts,
                                    bool isUpperHalf, bool isFP) {
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
    ty = getVecTy(eltSize, numElts / 2, isFP);
  } else {
    ty = getVecTy(eltSize, numElts, isFP);
  }
  return createBitCast(regVal, ty);
}

void arm2llvm::updateOutputReg(Value *V, bool SExt) {
  auto destReg = CurInst->getOperand(0).getReg();
  updateReg(V, destReg, SExt);
}

Value *arm2llvm::splatImm(Value *v, unsigned numElts, unsigned eltSize,
                          bool shift) {
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

const set<int> arm2llvm::s_flag = {
    AArch64::ADDSWri, AArch64::ADDSWrs, AArch64::ADDSWrx, AArch64::ADDSXri,
    AArch64::ADDSXrs, AArch64::ADDSXrx, AArch64::SUBSWri, AArch64::SUBSWrs,
    AArch64::SUBSWrx, AArch64::SUBSXri, AArch64::SUBSXrs, AArch64::SUBSXrx,
    AArch64::ANDSWri, AArch64::ANDSWrr, AArch64::ANDSWrs, AArch64::ANDSXri,
    AArch64::ANDSXrr, AArch64::ANDSXrs, AArch64::BICSWrs, AArch64::BICSXrs,
    AArch64::ADCSWr,  AArch64::ADCSXr,  AArch64::SBCSWr,  AArch64::SBCSXr,
};

const set<int> arm2llvm::instrs_32 = {
    AArch64::TBNZW,
    AArch64::TBZW,
    AArch64::CBZW,
    AArch64::CBNZW,
    AArch64::ADDWrx,
    AArch64::ADDSWrs,
    AArch64::ADDSWri,
    AArch64::ADDWrs,
    AArch64::ADDWri,
    AArch64::ADDSWrx,
    AArch64::ADCWr,
    AArch64::ADCSWr,
    AArch64::ASRVWr,
    AArch64::SBCSWr,
    AArch64::SBCWr,
    AArch64::SUBWri,
    AArch64::SUBWrs,
    AArch64::SUBWrx,
    AArch64::SUBSWrs,
    AArch64::SUBSWri,
    AArch64::SUBSWrx,
    AArch64::SBFMWri,
    AArch64::CSELWr,
    AArch64::ANDWri,
    AArch64::ANDWrr,
    AArch64::ANDWrs,
    AArch64::ANDSWri,
    AArch64::ANDSWrr,
    AArch64::ANDSWrs,
    AArch64::MADDWrrr,
    AArch64::MSUBWrrr,
    AArch64::EORWri,
    AArch64::CSINVWr,
    AArch64::CSINCWr,
    AArch64::MOVZWi,
    AArch64::MOVNWi,
    AArch64::MOVKWi,
    AArch64::LSLVWr,
    AArch64::LSRVWr,
    AArch64::ORNWrs,
    AArch64::UBFMWri,
    AArch64::BFMWri,
    AArch64::ORRWrs,
    AArch64::ORRWri,
    AArch64::SDIVWr,
    AArch64::UDIVWr,
    AArch64::EXTRWrri,
    AArch64::EORWrs,
    AArch64::RORVWr,
    AArch64::RBITWr,
    AArch64::CLZWr,
    AArch64::REVWr,
    AArch64::CSNEGWr,
    AArch64::BICWrs,
    AArch64::BICSWrs,
    AArch64::EONWrs,
    AArch64::REV16Wr,
    AArch64::Bcc,
    AArch64::CCMPWr,
    AArch64::CCMPWi,
    AArch64::LDRWui,
    AArch64::LDRBBroW,
    AArch64::LDRBBroX,
    AArch64::LDRBroW,
    AArch64::LDRBroX,
    AArch64::LDRHHroW,
    AArch64::LDRHHroX,
    AArch64::LDRHroW,
    AArch64::LDRHroX,
    AArch64::LDRWroW,
    AArch64::LDRWroX,
    AArch64::LDRSBWroW,
    AArch64::LDRSBWroX,
    AArch64::LDRSBXroW,
    AArch64::LDRSBXroX,
    AArch64::LDRSHWroW,
    AArch64::LDRSHWroX,
    AArch64::LDRSHXroW,
    AArch64::LDRSHXroX,
    AArch64::LDRSWroW,
    AArch64::LDRSWroX,
    AArch64::LDRSui,
    AArch64::LDRBBui,
    AArch64::LDRBui,
    AArch64::LDRSBWui,
    AArch64::LDRSHWui,
    AArch64::LDRSBWpre,
    AArch64::LDRSHWpre,
    AArch64::LDRSBWpost,
    AArch64::LDRSHWpost,
    AArch64::LDRHHui,
    AArch64::LDRHui,
    AArch64::LDURBi,
    AArch64::LDURBBi,
    AArch64::LDURHi,
    AArch64::LDURHHi,
    AArch64::LDURSi,
    AArch64::LDURWi,
    AArch64::LDURSBWi,
    AArch64::LDURSHWi,
    AArch64::LDRBBpre,
    AArch64::LDRBpre,
    AArch64::LDRHHpre,
    AArch64::LDRHpre,
    AArch64::LDRWpre,
    AArch64::LDRSpre,
    AArch64::LDRBBpost,
    AArch64::LDRBpost,
    AArch64::LDRHHpost,
    AArch64::LDRHpost,
    AArch64::LDRWpost,
    AArch64::LDRSpost,
    AArch64::STRBBpost,
    AArch64::STRBpost,
    AArch64::STRHHpost,
    AArch64::STRHpost,
    AArch64::STRWpost,
    AArch64::STRSpost,
    AArch64::STRWui,
    AArch64::STRBBroW,
    AArch64::STRBBroX,
    AArch64::STRBroW,
    AArch64::STRBroX,
    AArch64::STRHHroW,
    AArch64::STRHHroX,
    AArch64::STRHroW,
    AArch64::STRHroX,
    AArch64::STRWroW,
    AArch64::STRWroX,
    AArch64::CCMNWi,
    AArch64::CCMNWr,
    AArch64::STRBBui,
    AArch64::STRBui,
    AArch64::STPWi,
    AArch64::STPSi,
    AArch64::STPWpre,
    AArch64::STPSpre,
    AArch64::STPWpost,
    AArch64::STPSpost,
    AArch64::STRHHui,
    AArch64::STRHui,
    AArch64::STURBBi,
    AArch64::STURBi,
    AArch64::STURHHi,
    AArch64::STURHi,
    AArch64::STURWi,
    AArch64::STURSi,
    AArch64::STRSui,
    AArch64::LDPWi,
    AArch64::LDPSi,
    AArch64::LDPWpre,
    AArch64::LDPSpre,
    AArch64::LDPWpost,
    AArch64::LDPSpost,
    AArch64::STRBBpre,
    AArch64::STRBpre,
    AArch64::STRHHpre,
    AArch64::STRHpre,
    AArch64::STRWpre,
    AArch64::STRSpre,
    AArch64::FADDSrr,
    AArch64::FSUBSrr,
    AArch64::FCMPSrr,
    AArch64::FCMPESrr,
    AArch64::FCMPSri,
    AArch64::FCMPESri,
    AArch64::FMOVSWr,
    AArch64::INSvi32gpr,
    AArch64::INSvi16gpr,
    AArch64::INSvi8gpr,
    AArch64::FCVTZUUWHr,
    AArch64::FCVTZUUWSr,
    AArch64::FCVTZUUXHr,
    AArch64::FCVTZUUXSr,
    AArch64::FCVTZSUWHr,
    AArch64::FCVTZSUWSr,
    AArch64::FCVTZSUXHr,
    AArch64::FCVTZSUXSr,
    AArch64::FCVTZSv1i32,
    AArch64::FCVTSHr,
    AArch64::FCVTDHr,
    AArch64::FCVTHSr,
    AArch64::FCVTDSr,
    AArch64::FCVTZSUWSr,
    AArch64::FCSELSrrr,
    AArch64::FMULSrr,
    AArch64::FDIVSrr,
    AArch64::FMADDSrrr,
    AArch64::FMSUBSrrr,
    AArch64::FNMADDSrrr,
    AArch64::FNMSUBSrrr,
    AArch64::FNMULSrr,
    AArch64::FSQRTSr,
    AArch64::FMULv1i32_indexed,
    AArch64::FMLAv1i32_indexed,
    AArch64::FMLSv1i32_indexed,
    AArch64::FABSSr,
    AArch64::UQADDv1i32,
    AArch64::SQSUBv1i32,
    AArch64::SQADDv1i32,
    AArch64::FMOVSr,
    AArch64::FNEGSr,
    AArch64::BRK,
    AArch64::UCVTFUWSri,
    AArch64::UCVTFUWDri,
    AArch64::SCVTFUWSri,
    AArch64::SCVTFUWDri,
    AArch64::SCVTFv1i32,
    AArch64::FRINTXSr,
    AArch64::FRINTASr,
    AArch64::FRINTMSr,
    AArch64::FRINTPSr,
    AArch64::UQXTNv1i8,
    AArch64::UQXTNv1i16,
    AArch64::UQXTNv1i32,
    AArch64::SQXTNv1i8,
    AArch64::SQXTNv1i16,
    AArch64::SQXTNv1i32,
    AArch64::FMINSrr,
    AArch64::FMINNMSrr,
    AArch64::FMAXSrr,
    AArch64::FMAXNMSrr,
    AArch64::FCCMPSrr,
};

const set<int> arm2llvm::instrs_64 = {
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
    AArch64::SHLLv8i8,
    AArch64::SHLLv4i16,
    AArch64::SHLLv2i32,
    AArch64::SSHLv8i8,
    AArch64::SSHLv1i64,
    AArch64::SSHLv4i16,
    AArch64::SSHLv2i32,
    AArch64::SUBv1i64,
    AArch64::SUBv2i32,
    AArch64::SUBv4i16,
    AArch64::SUBv8i8,
    AArch64::UABAv8i8,
    AArch64::UABAv4i16,
    AArch64::UABAv2i32,
    AArch64::UABDv8i8,
    AArch64::UABDv4i16,
    AArch64::UABDv2i32,
    AArch64::SABAv8i8,
    AArch64::SABAv4i16,
    AArch64::SABAv2i32,
    AArch64::SABDv8i8,
    AArch64::SABDv4i16,
    AArch64::SABDv2i32,
    AArch64::ADCXr,
    AArch64::ADCSXr,
    AArch64::ASRVXr,
    AArch64::SBCXr,
    AArch64::SBCSXr,
    AArch64::SUBXri,
    AArch64::SUBXrs,
    AArch64::SUBXrx,
    AArch64::SUBXrx64,
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
    AArch64::LDPSWpre,
    AArch64::LDPSWpost,
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
    AArch64::FMLAv1i64_indexed,
    AArch64::FMLSv1i64_indexed,
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
    AArch64::SLIv8i8_shift,
    AArch64::SLIv4i16_shift,
    AArch64::SLIv2i32_shift,
    AArch64::SLId,
    AArch64::SRIv8i8_shift,
    AArch64::SRIv4i16_shift,
    AArch64::SRIv2i32_shift,
    AArch64::SRId,
    AArch64::XTNv8i8,
    AArch64::XTNv4i16,
    AArch64::XTNv2i32,
    AArch64::UQXTNv8i8,
    AArch64::UQXTNv4i16,
    AArch64::UQXTNv2i32,
    AArch64::SQXTNv8i8,
    AArch64::SQXTNv4i16,
    AArch64::SQXTNv2i32,
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
    AArch64::UABALv8i8_v8i16,
    AArch64::UABALv4i16_v4i32,
    AArch64::UABALv2i32_v2i64,
    AArch64::UABDLv8i8_v8i16,
    AArch64::UABDLv4i16_v4i32,
    AArch64::UABDLv2i32_v2i64,
    AArch64::SABALv8i8_v8i16,
    AArch64::SABALv4i16_v4i32,
    AArch64::SABALv2i32_v2i64,
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
    AArch64::SHLd,
    AArch64::SSHRv8i8_shift,
    AArch64::SSHRv4i16_shift,
    AArch64::SSHRv2i32_shift,
    AArch64::SSHRd,
    AArch64::SSRAv8i8_shift,
    AArch64::SSRAv4i16_shift,
    AArch64::SSRAv2i32_shift,
    AArch64::ADDPv8i8,
    AArch64::ADDPv4i16,
    AArch64::ADDPv2i32,
    AArch64::CMEQv2i32,
    AArch64::CMHIv8i8,
    AArch64::CMHIv4i16,
    AArch64::CMHIv1i64,
    AArch64::CMHIv2i32,
    AArch64::CMHIv1i64,
    AArch64::CMGTv1i64,
    AArch64::CMEQv2i32rz,
    AArch64::CMEQv4i16,
    AArch64::CMEQv4i16rz,
    AArch64::CMEQv8i8,
    AArch64::CMEQv8i8rz,
    AArch64::CMGEv2i32,
    AArch64::CMGEv1i64,
    AArch64::CMGEv4i16,
    AArch64::CMGEv8i8,
    AArch64::CMGTv2i32,
    AArch64::CMGTv4i16,
    AArch64::CMGTv8i8,
    AArch64::CMHSv1i64,
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
    AArch64::USHRd,
    AArch64::SMULLv8i8_v8i16,
    AArch64::SMULLv2i32_v2i64,
    AArch64::SMULLv4i16_v4i32,
    AArch64::SMULLv4i16_indexed,
    AArch64::SMULLv2i32_indexed,
    AArch64::USRAv8i8_shift,
    AArch64::USRAv4i16_shift,
    AArch64::USRAv2i32_shift,
    AArch64::USRAd,
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
    AArch64::SMINPv8i8,
    AArch64::SMINPv4i16,
    AArch64::SMINPv2i32,
    AArch64::SMAXPv8i8,
    AArch64::SMAXPv4i16,
    AArch64::SMAXPv2i32,
    AArch64::UMINPv8i8,
    AArch64::UMINPv4i16,
    AArch64::UMINPv2i32,
    AArch64::UMAXPv8i8,
    AArch64::UMAXPv4i16,
    AArch64::UMAXPv2i32,
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
    AArch64::CMGTv1i64rz,
    AArch64::CMGTv2i32rz,
    AArch64::CMGEv8i8rz,
    AArch64::CMLEv1i64rz,
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
    AArch64::URHADDv8i8,
    AArch64::URHADDv4i16,
    AArch64::URHADDv2i32,
    AArch64::UHADDv8i8,
    AArch64::UHADDv4i16,
    AArch64::UHADDv2i32,
    AArch64::SRHADDv8i8,
    AArch64::SRHADDv4i16,
    AArch64::SRHADDv2i32,
    AArch64::SHADDv8i8,
    AArch64::SHADDv4i16,
    AArch64::SHADDv2i32,
    AArch64::UHSUBv8i8,
    AArch64::UHSUBv4i16,
    AArch64::UHSUBv2i32,
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
    AArch64::FRINTXDr,
    AArch64::FRINTADr,
    AArch64::FRINTMDr,
    AArch64::FRINTPDr,
    AArch64::PRFMl,
    AArch64::PRFMroW,
    AArch64::PRFMroX,
    AArch64::PRFMui,
    AArch64::PRFUMi,
    AArch64::PACIASP,
    AArch64::PACIBSP,
    AArch64::AUTIASP,
    AArch64::AUTIBSP,
    AArch64::HINT,
    AArch64::FMINDrr,
    AArch64::FMINNMDrr,
    AArch64::FMAXDrr,
    AArch64::FMAXNMDrr,
    AArch64::FCMEQv2f32,
    AArch64::FCMGTv2f32,
    AArch64::FCMGEv2f32,
    AArch64::FCMEQv2i32rz,
    AArch64::FCMGTv2i32rz,
    AArch64::FCMLTv2i32rz,
    AArch64::FCMLEv2i32rz,
    AArch64::FCMGEv2i32rz,
    AArch64::FADDv2f32,
    AArch64::FSUBv2f32,
    AArch64::FMULv2f32,
    AArch64::FCCMPDrr,
};

const set<int> arm2llvm::instrs_128 = {
    AArch64::DUPv8i8lane,
    AArch64::DUPv4i16lane,
    AArch64::DUPv2i32lane,
    AArch64::RADDHNv2i64_v2i32,
    AArch64::RADDHNv2i64_v4i32,
    AArch64::RADDHNv4i32_v4i16,
    AArch64::RADDHNv4i32_v8i16,
    AArch64::RADDHNv8i16_v16i8,
    AArch64::RADDHNv8i16_v8i8,
    AArch64::ADDHNv2i64_v2i32,
    AArch64::ADDHNv2i64_v4i32,
    AArch64::ADDHNv4i32_v4i16,
    AArch64::ADDHNv4i32_v8i16,
    AArch64::ADDHNv8i16_v16i8,
    AArch64::ADDHNv8i16_v8i8,
    AArch64::RSUBHNv2i64_v2i32,
    AArch64::RSUBHNv2i64_v4i32,
    AArch64::RSUBHNv4i32_v4i16,
    AArch64::RSUBHNv4i32_v8i16,
    AArch64::RSUBHNv8i16_v16i8,
    AArch64::RSUBHNv8i16_v8i8,
    AArch64::SUBHNv2i64_v2i32,
    AArch64::SUBHNv2i64_v4i32,
    AArch64::SUBHNv4i32_v4i16,
    AArch64::SUBHNv4i32_v8i16,
    AArch64::SUBHNv8i16_v16i8,
    AArch64::SUBHNv8i16_v8i8,
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
    AArch64::SHRNv8i8_shift,
    AArch64::SHRNv16i8_shift,
    AArch64::SHRNv4i16_shift,
    AArch64::SHRNv8i16_shift,
    AArch64::SHRNv2i32_shift,
    AArch64::SHRNv4i32_shift,
    AArch64::RSHRNv8i8_shift,
    AArch64::RSHRNv16i8_shift,
    AArch64::RSHRNv4i16_shift,
    AArch64::RSHRNv8i16_shift,
    AArch64::RSHRNv2i32_shift,
    AArch64::RSHRNv4i32_shift,
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
    AArch64::SMINPv16i8,
    AArch64::SMINPv8i16,
    AArch64::SMINPv4i32,
    AArch64::SMAXPv16i8,
    AArch64::SMAXPv8i16,
    AArch64::SMAXPv4i32,
    AArch64::UMINPv16i8,
    AArch64::UMINPv8i16,
    AArch64::UMINPv4i32,
    AArch64::UMAXPv16i8,
    AArch64::UMAXPv8i16,
    AArch64::UMAXPv4i32,
    AArch64::USRAv16i8_shift,
    AArch64::USRAv8i16_shift,
    AArch64::USRAv4i32_shift,
    AArch64::USRAv2i64_shift,
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
    AArch64::SSRAv16i8_shift,
    AArch64::SSRAv8i16_shift,
    AArch64::SSRAv4i32_shift,
    AArch64::SSRAd,
    AArch64::SSRAv2i64_shift,
    AArch64::SHLv16i8_shift,
    AArch64::SHLv8i16_shift,
    AArch64::SHLv4i32_shift,
    AArch64::SHLv2i64_shift,
    AArch64::USHLLv8i16_shift,
    AArch64::USHLLv16i8_shift,
    AArch64::USHLLv4i32_shift,
    AArch64::SLIv16i8_shift,
    AArch64::SLIv8i16_shift,
    AArch64::SLIv4i32_shift,
    AArch64::SLIv2i64_shift,
    AArch64::SRIv16i8_shift,
    AArch64::SRIv8i16_shift,
    AArch64::SRIv4i32_shift,
    AArch64::SRIv2i64_shift,
    AArch64::DUPi8,
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
    AArch64::LD1Rv8b_POST,
    AArch64::LD1Rv16b_POST,
    AArch64::LD1Rv4h_POST,
    AArch64::LD1Rv8h_POST,
    AArch64::LD1Rv2s_POST,
    AArch64::LD1Rv4s_POST,
    AArch64::LD1Rv1d_POST,
    AArch64::LD1Rv2d_POST,
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
    AArch64::UABAv16i8,
    AArch64::UABAv8i16,
    AArch64::UABAv4i32,
    AArch64::UABDv16i8,
    AArch64::UABDv8i16,
    AArch64::UABDv4i32,
    AArch64::SABAv16i8,
    AArch64::SABAv8i16,
    AArch64::SABAv4i32,
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
    AArch64::UMOVvi32_idx0,
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
    AArch64::SHLLv16i8,
    AArch64::SHLLv8i16,
    AArch64::SHLLv4i32,
    AArch64::SSHLv16i8,
    AArch64::SSHLv8i16,
    AArch64::SSHLv4i32,
    AArch64::SSHLv2i64,
    AArch64::URHADDv16i8,
    AArch64::URHADDv8i16,
    AArch64::URHADDv4i32,
    AArch64::UHADDv16i8,
    AArch64::UHADDv8i16,
    AArch64::UHADDv4i32,
    AArch64::SRHADDv16i8,
    AArch64::SRHADDv8i16,
    AArch64::SRHADDv4i32,
    AArch64::SHADDv16i8,
    AArch64::SHADDv8i16,
    AArch64::SHADDv4i32,
    AArch64::UHSUBv16i8,
    AArch64::UHSUBv8i16,
    AArch64::UHSUBv4i32,
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
    AArch64::UABALv16i8_v8i16,
    AArch64::UABALv8i16_v4i32,
    AArch64::UABALv4i32_v2i64,
    AArch64::UABDLv16i8_v8i16,
    AArch64::UABDLv8i16_v4i32,
    AArch64::UABDLv4i32_v2i64,
    AArch64::SABALv16i8_v8i16,
    AArch64::SABALv8i16_v4i32,
    AArch64::SABALv4i32_v2i64,
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
    AArch64::UQXTNv16i8,
    AArch64::UQXTNv8i16,
    AArch64::UQXTNv4i32,
    AArch64::SQXTNv16i8,
    AArch64::SQXTNv8i16,
    AArch64::SQXTNv4i32,
    AArch64::FNEGv4f32,
    AArch64::FNEGv2f64,
    AArch64::FCMEQv4f32,
    AArch64::FCMEQv2f64,
    AArch64::FCMGTv4f32,
    AArch64::FCMGTv2f64,
    AArch64::FCMGEv4f32,
    AArch64::FCMGEv2f64,
    AArch64::FCMLEv2i64rz,
    AArch64::FCMLEv4i32rz,
    AArch64::FCMGEv4i32rz,
    AArch64::FCMGEv2i64rz,
    AArch64::FCMLTv2i64rz,
    AArch64::FCMLTv4i32rz,
    AArch64::FCMGTv4i32rz,
    AArch64::FCMGTv2i64rz,
    AArch64::FCMEQv2i64rz,
    AArch64::FCMEQv4i32rz,
    AArch64::FADDv4f32,
    AArch64::FADDv2f64,
    AArch64::FSUBv4f32,
    AArch64::FSUBv2f64,
    AArch64::FMULv4f32,
    AArch64::FMULv2f64,
};

bool arm2llvm::has_s(int instr) {
  return s_flag.contains(instr);
}

// decodeBitMasks - Decode a logical immediate value in the form
// "N:immr:imms" (where the immr and imms fields are each 6 bits) into the
// integer value it represents with regSize bits. Implementation of the
// DecodeBitMasks function from the ARMv8 manual.
//
// WARNING: tmask is untested
pair<uint64_t, uint64_t> arm2llvm::decodeBitMasks(uint64_t val,
                                                  unsigned regSize) {
  // Extract the N, imms, and immr fields.
  unsigned N = (val >> 12) & 1;
  unsigned immr = (val >> 6) & 0x3f;
  unsigned imms = val & 0x3f;

  assert((regSize == 64 || N == 0) && "undefined logical immediate encoding");
  int len = 31 - llvm::countl_zero((N << 6) | (~imms & 0x3f));
  assert(len >= 0 && len <= 6 && "undefined logical immediate encoding");
  unsigned size = (uint64_t)1 << len;
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

unsigned arm2llvm::getInstSize(int instr) {
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
Value *arm2llvm::regShift(Value *value, int encodedShift) {
  if (encodedShift == 0)
    return value;

  int shift_type = (encodedShift >> 6) & 0x7;
  auto W = getBitWidth(value);
  auto exp = getUnsignedIntConst(encodedShift & 0x3f, W);

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

llvm::AllocaInst *arm2llvm::get_reg(aslp::reg_t regtype, uint64_t num) {
  using reg_t = aslp::reg_t;
  using pstate_t = aslp::pstate_t;

  uint64_t reg = 0;
  if (regtype == reg_t::X) {
    if (num <= 28)
      reg = llvm::AArch64::X0 + num;
    else if (num == 29)
      reg = llvm::AArch64::FP;
    else if (num == 30)
      reg = llvm::AArch64::LR;
    else if (num == 31)
      reg = llvm::AArch64::SP;
    else
      assert(false && "X register out of range");

  } else if (regtype == reg_t::PSTATE) {

    if (num == (int)pstate_t::N)
      reg = llvm::AArch64::N;
    else if (num == (int)pstate_t::Z)
      reg = llvm::AArch64::Z;
    else if (num == (int)pstate_t::C)
      reg = llvm::AArch64::C;
    else if (num == (int)pstate_t::V)
      reg = llvm::AArch64::V;

  } else if (regtype == reg_t::V) {
    reg = llvm::AArch64::Q0 + num;
  }

  assert(reg && "register not mapped");
  return llvm::cast<llvm::AllocaInst>(RegFile.at(reg));
}

optional<aslp::opcode_t> arm2llvm::getArmOpcode(const MCInst &I) {
  SmallVector<MCFixup> Fixups{};
  SmallVector<char> Code{};

  if (I.getOpcode() == sentinelNOP())
    return nullopt;

  MCE->encodeInstruction(I, Code, Fixups, *STI.get());
  for (auto x : Fixups) {
    // std::cerr << "fixup: " << x.getKind() << ' ' << x.getTargetKind() << '
    // ' << x.getOffset() << ' ' << std::flush; x.getValue()->dump();
    // std::cout << std::endl;
    (void)x;
  }

  // do not hand any instructions with relocation fixups to aslp
  if (Fixups.size() != 0)
    return nullopt;

  aslp::opcode_t ret;
  unsigned i = 0;
  for (const char &x : Code) {
    ret.at(i++) = x;
  }
  return ret;
}

void arm2llvm::platformInit() {
  auto i8 = getIntTy(8);
  auto i64 = getIntTy(64);

  // allocate storage for the main register file
  for (unsigned Reg = AArch64::X0; Reg <= AArch64::X28; ++Reg) {
    stringstream Name;
    Name << "X" << Reg - AArch64::X0;
    createRegStorage(Reg, 64, Name.str());
    initialReg[Reg - AArch64::X0] = readFromRegOld(Reg);
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
  auto paramBase =
      createGEP(i8, stackMem, {getUnsignedIntConst(stackBytes, 64)}, "");
  createStore(paramBase, RegFile[AArch64::SP]);
  initialSP = readFromRegOld(AArch64::SP);

  // FP is X29; we'll initialize it later
  createRegStorage(AArch64::FP, 64, "FP");

  // LR is X30; FIXME initialize this
  createRegStorage(AArch64::LR, 64, "LR");

  // initializing to zero makes loads from XZR work; stores are
  // handled in updateReg()
  createRegStorage(AArch64::XZR, 64, "XZR");
  createStore(getUnsignedIntConst(0, 64), RegFile[AArch64::XZR]);

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
                              srcArg = srcFn->arg_begin();
       arg != E; ++arg, ++srcArg) {
    *out << "  processing " << getBitWidth(arg)
         << "-bit arg with vecArgNum = " << vecArgNum
         << ", scalarArgNum = " << scalarArgNum
         << ", stackSlot = " << stackSlot;
    auto *argTy = arg->getType();
    auto *val =
        enforceSExtZExt(arg, srcArg->hasSExtAttr(), srcArg->hasZExtAttr());

    // first 8 integer parameters go in the first 8 integer registers
    if ((argTy->isIntegerTy() || argTy->isPointerTy()) && scalarArgNum < 8) {
      auto Reg = AArch64::X0 + scalarArgNum;
      createStore(val, RegFile[Reg]);
      ++scalarArgNum;
      goto end;
    }

    // first 8 vector/FP parameters go in the first 8 vector registers
    if ((argTy->isVectorTy() || argTy->isFloatingPointTy()) && vecArgNum < 8) {
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

      auto addr =
          createGEP(i64, paramBase, {getUnsignedIntConst(stackSlot, 64)}, "");
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
  auto initFP =
      createGEP(i64, paramBase, {getUnsignedIntConst(stackSlot, 64)}, "");
  createStore(initFP, RegFile[AArch64::FP]);
}

void arm2llvm::checkArgSupport(Argument &arg) {}
void arm2llvm::checkFuncSupport(Function &func) {}
void arm2llvm::checkTypeSupport(Type *ty) {
  if (ty->isFloatingPointTy()) {
    if (!(ty->isFloatTy() || ty->isDoubleTy())) {
      *out << "\nERROR: only float and double supported (not bfloat, half, "
              "fp128, etc.)\n\n";
      exit(-1);
    }
  }
}
