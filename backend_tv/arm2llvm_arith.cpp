#include "backend_tv/arm2llvm.h"

using namespace lifter;
using namespace llvm;

#define GET_INSTRINFO_ENUM
#include "Target/AArch64/AArch64GenInstrInfo.inc"

#define GET_REGINFO_ENUM
#include "Target/AArch64/AArch64GenRegisterInfo.inc"

void arm2llvm::lift_udiv(unsigned opcode) {
  // can't directly lift ARM udiv to LLVM udiv because the latter
  // is UB for divide by zero
  auto size = getInstSize(opcode);
  auto zero = getUnsignedIntConst(0, size);
  auto lhs = readFromOperand(1);
  auto rhs = readFromOperand(2);
  auto result = createCheckedUDiv(lhs, rhs, zero);
  updateOutputReg(result);
}

void arm2llvm::lift_sdiv(unsigned opcode) {
  // can't directly lift ARM sdiv to LLVM sdiv because the latter
  // is UB for divide by zero and INT_MIN / -1
  auto size = getInstSize(opcode);
  auto zero = getUnsignedIntConst(0, size);
  auto lhs = readFromOperand(1);
  auto rhs = readFromOperand(2);
  auto result = createCheckedSDiv(lhs, rhs, zero, zero);
  updateOutputReg(result);
}

void arm2llvm::lift_msub() {
  auto mul_lhs = readFromOperand(1);
  auto mul_rhs = readFromOperand(2);
  auto minuend = readFromOperand(3);
  auto mul = createMul(mul_lhs, mul_rhs);
  auto sub = createSub(minuend, mul);
  updateOutputReg(sub);
}

void arm2llvm::lift_mulh(unsigned opcode) {
  auto i64 = getIntTy(64);
  auto i128 = getIntTy(128);

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
  auto shift = createMaskedLShr(mul, getUnsignedIntConst(64, 128));

  // Truncate to the proper size:
  auto trunc = createTrunc(shift, i64);
  updateOutputReg(trunc);
}

void arm2llvm::lift_msubl(unsigned opcode) {
  auto i32 = getIntTy(32);
  auto i64 = getIntTy(64);

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
}

void arm2llvm::lift_smaddl() {
  auto i32 = getIntTy(32);
  auto i64 = getIntTy(64);

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
}

void arm2llvm::lift_umadd(unsigned opcode) {
  auto size = getInstSize(opcode);
  auto mul_lhs = readFromOperand(1);
  auto mul_rhs = readFromOperand(2);
  auto addend = readFromOperand(3);

  auto lhs_masked = createAnd(mul_lhs, getUnsignedIntConst(0xffffffffUL, size));
  auto rhs_masked = createAnd(mul_rhs, getUnsignedIntConst(0xffffffffUL, size));
  auto mul = createMul(lhs_masked, rhs_masked);
  auto add = createAdd(mul, addend);
  updateOutputReg(add);
}

void arm2llvm::lift_madd(unsigned opcode) {
  auto mul_lhs = readFromOperand(1);
  auto mul_rhs = readFromOperand(2);
  auto addend = readFromOperand(3);

  auto mul = createMul(mul_lhs, mul_rhs);
  auto add = createAdd(mul, addend);
  updateOutputReg(add);
}

void arm2llvm::lift_add(unsigned opcode) {
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
      b = createCast(b, ty, isSigned ? Instruction::SExt : Instruction::ZExt);
    }

    // shift may not be there, it may just be the extend
    if (shift != 0)
      b = createMaskedShl(b, getUnsignedIntConst(shift, size));
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
    return;

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
}

void arm2llvm::lift_adc_sbc(unsigned opcode) {
  auto a = readFromOperand(1);
  auto b = readFromOperand(2);

  switch (opcode) {
  case AArch64::SBCWr:
  case AArch64::SBCXr:
  case AArch64::SBCSWr:
  case AArch64::SBCSXr:
    b = createNot(b);
    return;
  }

  auto [res, flags] = addWithCarry(a, b, getC());
  updateOutputReg(res);

  if (has_s(opcode)) {
    auto [n, z, c, v] = flags;
    setN(n);
    setZ(z);
    setC(c);
    setV(v);
  }
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
void arm2llvm::lift_sub(unsigned opcode) {
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
  case AArch64::SUBXrx64:
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
      b = createCast(b, ty, isSigned ? Instruction::SExt : Instruction::ZExt);
    }

    // shift may not be there, it may just be the extend
    if (shift != 0)
      b = createMaskedShl(b, getUnsignedIntConst(shift, size));
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
}
