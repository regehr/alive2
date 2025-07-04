#include "backend_tv/arm2llvm.h"

#include "Target/AArch64/MCTargetDesc/AArch64MCAsmInfo.h"

#include <vector>

using namespace lifter;
using namespace llvm;
using namespace std;

static uint64_t replicate8to64(uint64_t v) {
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

void arm2llvm::lift_mull(unsigned opcode) {
  int eltSize, numElts1, numElts2;
  if (opcode == AArch64::SMULLv8i16_indexed ||
      opcode == AArch64::UMULLv8i16_indexed) {
    numElts1 = 8;
    numElts2 = 8;
    eltSize = 16;
  } else if (opcode == AArch64::SMULLv4i32_indexed ||
             opcode == AArch64::UMULLv4i32_indexed) {
    numElts1 = 4;
    numElts2 = 4;
    eltSize = 32;
  } else if (opcode == AArch64::SMULLv2i32_indexed ||
             opcode == AArch64::UMULLv2i32_indexed) {
    numElts1 = 2;
    numElts2 = 4;
    eltSize = 32;
  } else if (opcode == AArch64::SMULLv4i16_indexed ||
             opcode == AArch64::UMULLv4i16_indexed) {
    numElts1 = 4;
    numElts2 = 8;
    eltSize = 16;
  } else {
    assert(false);
  }
  auto vTy1 = getVecTy(eltSize, numElts1);
  auto vTy2 = getVecTy(eltSize, numElts2);
  auto vBigTy1 = getVecTy(2 * eltSize, numElts1);
  auto vBigTy2 = getVecTy(2 * eltSize, numElts2);
  Value *a, *b;
  switch (opcode) {
  case AArch64::UMULLv4i16_indexed:
  case AArch64::UMULLv2i32_indexed:
  case AArch64::UMULLv8i16_indexed:
  case AArch64::UMULLv4i32_indexed:
    a = createZExt(createBitCast(readFromOperand(1), vTy1), vBigTy1);
    b = createZExt(createBitCast(readFromOperand(2, 128), vTy2), vBigTy2);
    break;
  case AArch64::SMULLv4i32_indexed:
  case AArch64::SMULLv8i16_indexed:
  case AArch64::SMULLv4i16_indexed:
  case AArch64::SMULLv2i32_indexed:
    a = createSExt(createBitCast(readFromOperand(1), vTy1), vBigTy1);
    b = createSExt(createBitCast(readFromOperand(2, 128), vTy2), vBigTy2);
    break;
  default:
    assert(false);
  }
  auto idx = getImm(3);
  Value *res = getUndefVec(numElts1, 2 * eltSize);
  // offset is nonzero when we're dealing with SMULL2/UMULL2
  int offset = (opcode == AArch64::SMULLv4i32_indexed ||
                opcode == AArch64::SMULLv8i16_indexed ||
                opcode == AArch64::UMULLv4i32_indexed ||
                opcode == AArch64::UMULLv8i16_indexed)
                   ? (numElts1 / 2)
                   : 0;
  auto e2 = createExtractElement(b, idx);
  for (int i = 0; i < numElts1; ++i) {
    auto e1 = createExtractElement(a, i + offset);
    res = createInsertElement(res, createMul(e1, e2), i);
  }
  updateOutputReg(res);
}

void arm2llvm::lift_unary_vec(unsigned opcode) {
  auto src = readFromOperand(1);
  uint64_t eltSize, numElts;

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
    Value *sum = getUnsignedIntConst(0, eltSize);
    for (unsigned i = 0; i < numElts; ++i) {
      auto elt = createExtractElement(src_vector, i);
      sum = createAdd(sum, elt);
    }
    // sum goes into the bottom lane, all others are zeroed out
    auto zero = getZeroIntVec(numElts, eltSize);
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
        opcode == AArch64::SADDLVv4i16v || opcode == AArch64::SADDLVv8i16v ||
        opcode == AArch64::SADDLVv4i32v;
    auto src_vector = createBitCast(src, vTy);
    auto bigTy = getIntTy(2 * eltSize);
    Value *sum = getUnsignedIntConst(0, 2 * eltSize);
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
    updateOutputReg(createNot(src_vector));
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
}

void arm2llvm::lift_qxtn(unsigned opcode) {
  auto i64 = getIntTy(64);
  auto &op0 = CurInst->getOperand(0);

  uint64_t srcReg, part;
  switch (opcode) {
  case AArch64::UQXTNv1i8:
  case AArch64::UQXTNv1i16:
  case AArch64::UQXTNv1i32:
  case AArch64::UQXTNv8i8:
  case AArch64::UQXTNv4i16:
  case AArch64::UQXTNv2i32:
  case AArch64::SQXTNv1i8:
  case AArch64::SQXTNv1i16:
  case AArch64::SQXTNv1i32:
  case AArch64::SQXTNv8i8:
  case AArch64::SQXTNv4i16:
  case AArch64::SQXTNv2i32:
    srcReg = 1;
    part = 0;
    break;
  case AArch64::UQXTNv16i8:
  case AArch64::UQXTNv8i16:
  case AArch64::UQXTNv4i32:
  case AArch64::SQXTNv16i8:
  case AArch64::SQXTNv8i16:
  case AArch64::SQXTNv4i32:
    srcReg = 2;
    part = 1;
    break;
  default:
    *out << "\nError Unknown opcode\n";
    visitError();
    break;
  }

  auto &op1 = CurInst->getOperand(srcReg);
  assert(isSIMDandFPRegOperand(op0) && isSIMDandFPRegOperand(op0));

  Value *src = readFromRegOld(op1.getReg());
  assert(getBitWidth(src) == 128 &&
         "Source value is not a vector with 128 bits");

  uint64_t eltSize, numElts;
  switch (opcode) {
  case AArch64::UQXTNv1i8:
  case AArch64::UQXTNv16i8:
  case AArch64::UQXTNv8i8:
  case AArch64::SQXTNv1i8:
  case AArch64::SQXTNv16i8:
  case AArch64::SQXTNv8i8:
    numElts = 8;
    eltSize = 8;
    break;
  case AArch64::UQXTNv1i16:
  case AArch64::UQXTNv8i16:
  case AArch64::UQXTNv4i16:
  case AArch64::SQXTNv1i16:
  case AArch64::SQXTNv8i16:
  case AArch64::SQXTNv4i16:
    numElts = 4;
    eltSize = 16;
    break;
  case AArch64::UQXTNv1i32:
  case AArch64::UQXTNv4i32:
  case AArch64::UQXTNv2i32:
  case AArch64::SQXTNv1i32:
  case AArch64::SQXTNv2i32:
  case AArch64::SQXTNv4i32:
    numElts = 2;
    eltSize = 32;
    break;
  default:
    *out << "\nError Unknown opcode\n";
    visitError();
    break;
  }

  if (opcode == AArch64::UQXTNv1i8 || opcode == AArch64::UQXTNv1i16 ||
      opcode == AArch64::UQXTNv1i32 || opcode == AArch64::SQXTNv1i8 ||
      opcode == AArch64::SQXTNv1i16 || opcode == AArch64::SQXTNv1i32) {
    numElts = 1;
  }

  bool isSigned = false;
  switch (opcode) {
  case AArch64::SQXTNv1i8:
  case AArch64::SQXTNv1i16:
  case AArch64::SQXTNv1i32:
  case AArch64::SQXTNv8i8:
  case AArch64::SQXTNv4i16:
  case AArch64::SQXTNv2i32:
  case AArch64::SQXTNv16i8:
  case AArch64::SQXTNv8i16:
  case AArch64::SQXTNv4i32:
    isSigned = true;
    break;
  }

  // BitCast src to a vector of numElts x (2*eltSize) for narrowing
  assert(numElts * (2 * eltSize) == 128 && "BitCasting to wrong type");
  Value *src_vector = createBitCast(src, getVecTy(2 * eltSize, numElts));

  Value *final_vector = getUndefVec(numElts, eltSize);
  // Perform element-wise saturating narrow using SatQ
  for (unsigned i = 0; i < numElts; ++i) {
    auto element = createExtractElement(src_vector, i);
    auto [narrowed_element, sat] = SatQ(element, eltSize, isSigned);
    final_vector = createInsertElement(final_vector, narrowed_element, i);
  }

  // For UQXTN2, SQXTN2 - insertion to upper half
  if (part) {
    // Preserve the lower 64 bits so, read from destination register
    // and insert to the upper 64 bits
    Value *dest = readFromRegOld(op0.getReg());
    Value *original_dest_vector = createBitCast(dest, getVecTy(64, 2));

    Value *element = createBitCast(final_vector, i64);

    final_vector = createInsertElement(original_dest_vector, element, 1);
  }

  // Write 64 bits for UQXTN, SQXTN or 128 bits for UQXTN2, SQXTN2 to output
  // register
  updateOutputReg(final_vector);
}

void arm2llvm::lift_xtn(unsigned opcode) {
  auto i64 = getIntTy(64);
  auto &op0 = CurInst->getOperand(0);
  uint64_t srcReg = (opcode == AArch64::XTNv2i32 ||
                     opcode == AArch64::XTNv4i16 || opcode == AArch64::XTNv8i8)
                        ? 1
                        : 2;
  auto &op1 = CurInst->getOperand(srcReg);
  assert(isSIMDandFPRegOperand(op0) && isSIMDandFPRegOperand(op0));

  Value *src = readFromRegOld(op1.getReg());
  assert(getBitWidth(src) == 128 &&
         "Source value is not a vector with 128 bits");

  uint64_t eltSize, numElts, part;
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
  Value *narrowed_vector = createTrunc(src_vector, getVecTy(eltSize, numElts));

  Value *final_vector = narrowed_vector;
  // For XTN2 - insertion to upper half
  if (part) {
    // Preserve the lower 64 bits so, read from destination register
    // and insert to the upper 64 bits
    Value *dest = readFromRegOld(op0.getReg());
    Value *original_dest_vector = createBitCast(dest, getVecTy(64, 2));

    Value *element = createBitCast(narrowed_vector, i64);

    final_vector = createInsertElement(original_dest_vector, element, 1);
  }

  // Write 64 bits for XTN or 128 bits XTN2 to output register
  updateOutputReg(final_vector);
}

void arm2llvm::lift_ssra(unsigned opcode) {
  unsigned numElts = -1, eltSize = -1;
  switch (opcode) {
  case AArch64::SSRAv8i8_shift:
    eltSize = 8;
    numElts = 8;
    break;
  case AArch64::SSRAv16i8_shift:
    eltSize = 8;
    numElts = 16;
    break;
  case AArch64::SSRAv4i16_shift:
    eltSize = 16;
    numElts = 4;
    break;
  case AArch64::SSRAv8i16_shift:
    eltSize = 16;
    numElts = 8;
    break;
  case AArch64::SSRAv2i32_shift:
    eltSize = 32;
    numElts = 2;
    break;
  case AArch64::SSRAv4i32_shift:
    eltSize = 32;
    numElts = 4;
    break;
  case AArch64::SSRAd:
    eltSize = 64;
    numElts = 1;
    break;
  case AArch64::SSRAv2i64_shift:
    eltSize = 64;
    numElts = 2;
    break;
  default:
    assert(false);
  }

  Value *a, *b, *c;
  a = readFromVecOperand(2, eltSize, numElts);
  b = getElemSplat(numElts, eltSize, getImm(3));
  c = readFromVecOperand(1, eltSize, numElts);

  auto shiftedVec = createMaskedAShr(a, b);
  auto res = createAdd(shiftedVec, c);
  updateOutputReg(res);
}

void arm2llvm::lift_uzp(unsigned opcode) {
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
}

void arm2llvm::lift_addp() {
  auto vTy = getVecTy(64, 2);
  auto v = createBitCast(readFromOperand(1), vTy);
  auto res = createAdd(createExtractElement(v, 0), createExtractElement(v, 1));
  updateOutputReg(res);
}

void arm2llvm::lift_vec_binop(unsigned opcode) {
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
  case AArch64::SMINPv8i8:
  case AArch64::SMINPv4i16:
  case AArch64::SMINPv2i32:
  case AArch64::SMINPv16i8:
  case AArch64::SMINPv8i16:
  case AArch64::SMINPv4i32:
    op = [&](Value *a, Value *b) { return createSMin(a, b); };
    break;
  case AArch64::SMAXv8i8:
  case AArch64::SMAXv4i16:
  case AArch64::SMAXv2i32:
  case AArch64::SMAXv16i8:
  case AArch64::SMAXv8i16:
  case AArch64::SMAXv4i32:
  case AArch64::SMAXPv8i8:
  case AArch64::SMAXPv4i16:
  case AArch64::SMAXPv2i32:
  case AArch64::SMAXPv16i8:
  case AArch64::SMAXPv8i16:
  case AArch64::SMAXPv4i32:
    op = [&](Value *a, Value *b) { return createSMax(a, b); };
    break;
  case AArch64::UMINv8i8:
  case AArch64::UMINv4i16:
  case AArch64::UMINv2i32:
  case AArch64::UMINv16i8:
  case AArch64::UMINv8i16:
  case AArch64::UMINv4i32:
  case AArch64::UMINPv8i8:
  case AArch64::UMINPv4i16:
  case AArch64::UMINPv2i32:
  case AArch64::UMINPv16i8:
  case AArch64::UMINPv8i16:
  case AArch64::UMINPv4i32:
    op = [&](Value *a, Value *b) { return createUMin(a, b); };
    break;
  case AArch64::UMAXv8i8:
  case AArch64::UMAXv4i16:
  case AArch64::UMAXv2i32:
  case AArch64::UMAXv16i8:
  case AArch64::UMAXv8i16:
  case AArch64::UMAXv4i32:
  case AArch64::UMAXPv8i8:
  case AArch64::UMAXPv4i16:
  case AArch64::UMAXPv2i32:
  case AArch64::UMAXPv16i8:
  case AArch64::UMAXPv8i16:
  case AArch64::UMAXPv4i32:
    op = [&](Value *a, Value *b) { return createUMax(a, b); };
    break;
  case AArch64::UMULLv16i8_v8i16:
  case AArch64::UMULLv8i16_v4i32:
  case AArch64::UMULLv4i32_v2i64:
    // these three cases are UMULL2
    a = createRawLShr(a, getUnsignedIntConst(64, 128));
    b = createRawLShr(b, getUnsignedIntConst(64, 128));
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
    a = createRawLShr(a, getUnsignedIntConst(64, 128));
    b = createRawLShr(b, getUnsignedIntConst(64, 128));
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
  case AArch64::USHRd:
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
  case AArch64::SSHRv8i8_shift:
  case AArch64::SSHRv16i8_shift:
  case AArch64::SSHRv4i16_shift:
  case AArch64::SSHRv8i16_shift:
  case AArch64::SSHRv2i32_shift:
  case AArch64::SSHRv4i32_shift:
  case AArch64::SSHRd:
  case AArch64::SSHRv2i64_shift:
    splatImm2 = true;
    op = [&](Value *a, Value *b) { return createMaskedAShr(a, b); };
    break;
  case AArch64::SHLv16i8_shift:
  case AArch64::SHLv8i16_shift:
  case AArch64::SHLv4i32_shift:
  case AArch64::SHLv2i64_shift:
  case AArch64::SHLv8i8_shift:
  case AArch64::SHLv4i16_shift:
  case AArch64::SHLd:
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
  case AArch64::SUBv1i64:
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
    a = createRawLShr(a, getUnsignedIntConst(64, 128));
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
    a = createRawLShr(a, getUnsignedIntConst(64, 128));
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

  unsigned eltSize, numElts;
  switch (opcode) {
  case AArch64::USHLv1i64:
  case AArch64::USHRd:
  case AArch64::SSHRd:
  case AArch64::SSHLv1i64:
  case AArch64::SHLd:
  case AArch64::ADDv1i64:
  case AArch64::SUBv1i64:
    numElts = 1;
    eltSize = 64;
    break;
  case AArch64::ORRv2i32:
  case AArch64::UMULLv2i32_v2i64:
  case AArch64::SMINv2i32:
  case AArch64::SMAXv2i32:
  case AArch64::UMINv2i32:
  case AArch64::UMAXv2i32:
  case AArch64::SMINPv2i32:
  case AArch64::SMAXPv2i32:
  case AArch64::UMINPv2i32:
  case AArch64::UMAXPv2i32:
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
  case AArch64::SMINPv4i16:
  case AArch64::SMAXPv4i16:
  case AArch64::UMINPv4i16:
  case AArch64::UMAXPv4i16:
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
  case AArch64::SMINPv4i32:
  case AArch64::SMAXPv4i32:
  case AArch64::UMINPv4i32:
  case AArch64::UMAXPv4i32:
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
  case AArch64::SMINPv8i8:
  case AArch64::SMAXPv8i8:
  case AArch64::UMINPv8i8:
  case AArch64::UMAXPv8i8:
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
  case AArch64::SMINPv8i16:
  case AArch64::SMAXPv8i16:
  case AArch64::UMINPv8i16:
  case AArch64::UMAXPv8i16:
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
  case AArch64::SMINPv16i8:
  case AArch64::SMAXPv16i8:
  case AArch64::UMINPv16i8:
  case AArch64::UMAXPv16i8:
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

  // Preprocessing the two operands
  switch (opcode) {
  case AArch64::SMINPv8i8:
  case AArch64::SMINPv4i16:
  case AArch64::SMINPv2i32:
  case AArch64::SMINPv16i8:
  case AArch64::SMINPv8i16:
  case AArch64::SMINPv4i32:
  case AArch64::SMAXPv8i8:
  case AArch64::SMAXPv4i16:
  case AArch64::SMAXPv2i32:
  case AArch64::SMAXPv16i8:
  case AArch64::SMAXPv8i16:
  case AArch64::SMAXPv4i32:
  case AArch64::UMINPv8i8:
  case AArch64::UMINPv4i16:
  case AArch64::UMINPv2i32:
  case AArch64::UMINPv16i8:
  case AArch64::UMINPv8i16:
  case AArch64::UMINPv4i32:
  case AArch64::UMAXPv8i8:
  case AArch64::UMAXPv4i16:
  case AArch64::UMAXPv2i32:
  case AArch64::UMAXPv16i8:
  case AArch64::UMAXPv8i16:
  case AArch64::UMAXPv4i32: {
    vector<int> mask1(numElts), mask2(numElts);
    for (unsigned i = 0; i < numElts; i++) {
      mask1[i] = 2 * i;
      mask2[i] = 2 * i + 1;
    }
    auto vector_a = createBitCast(a, getVecTy(eltSize, numElts));
    auto vector_b = createBitCast(b, getVecTy(eltSize, numElts));

    auto new_a = createShuffleVector(vector_b, vector_a, mask1);
    auto new_b = createShuffleVector(vector_b, vector_a, mask2);

    a = createBitCast(new_a, getIntTy(eltSize * numElts));
    b = createBitCast(new_b, getIntTy(eltSize * numElts));
    break;
  }
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

  auto res = createVectorOp(op, a, b, eltSize, numElts, elementWise, ext,
                            splatImm2, immShift, isUpper, operandTypesDiffer);
  updateOutputReg(res);
}

void arm2llvm::lift_xhn(unsigned opcode) {
  auto i64 = getIntTy(64);
  unsigned eltSize = -1, numElts = -1;
  switch (opcode) {
  case AArch64::RADDHNv2i64_v2i32:
  case AArch64::RADDHNv2i64_v4i32:
  case AArch64::ADDHNv2i64_v2i32:
  case AArch64::ADDHNv2i64_v4i32:
  case AArch64::RSUBHNv2i64_v2i32:
  case AArch64::RSUBHNv2i64_v4i32:
  case AArch64::SUBHNv2i64_v2i32:
  case AArch64::SUBHNv2i64_v4i32:
    eltSize = 32;
    numElts = 2;
    break;
  case AArch64::RADDHNv4i32_v4i16:
  case AArch64::RADDHNv4i32_v8i16:
  case AArch64::ADDHNv4i32_v4i16:
  case AArch64::ADDHNv4i32_v8i16:
  case AArch64::RSUBHNv4i32_v4i16:
  case AArch64::RSUBHNv4i32_v8i16:
  case AArch64::SUBHNv4i32_v4i16:
  case AArch64::SUBHNv4i32_v8i16:
    eltSize = 16;
    numElts = 4;
    break;
  case AArch64::RADDHNv8i16_v8i8:
  case AArch64::RADDHNv8i16_v16i8:
  case AArch64::ADDHNv8i16_v8i8:
  case AArch64::ADDHNv8i16_v16i8:
  case AArch64::RSUBHNv8i16_v8i8:
  case AArch64::RSUBHNv8i16_v16i8:
  case AArch64::SUBHNv8i16_v8i8:
  case AArch64::SUBHNv8i16_v16i8:
    eltSize = 8;
    numElts = 8;
    break;
  default:
    assert(false);
    break;
  }

  Instruction::BinaryOps op;
  bool addRoundingConst = false;
  switch (opcode) {
  case AArch64::RADDHNv2i64_v2i32:
  case AArch64::RADDHNv4i32_v4i16:
  case AArch64::RADDHNv8i16_v8i8:
  case AArch64::RADDHNv2i64_v4i32:
  case AArch64::RADDHNv4i32_v8i16:
  case AArch64::RADDHNv8i16_v16i8:
    addRoundingConst = true;
  case AArch64::ADDHNv2i64_v2i32:
  case AArch64::ADDHNv4i32_v4i16:
  case AArch64::ADDHNv8i16_v8i8:
  case AArch64::ADDHNv2i64_v4i32:
  case AArch64::ADDHNv4i32_v8i16:
  case AArch64::ADDHNv8i16_v16i8:
    op = Instruction::BinaryOps::Add;
    break;
  case AArch64::RSUBHNv2i64_v2i32:
  case AArch64::RSUBHNv4i32_v4i16:
  case AArch64::RSUBHNv8i16_v8i8:
  case AArch64::RSUBHNv2i64_v4i32:
  case AArch64::RSUBHNv4i32_v8i16:
  case AArch64::RSUBHNv8i16_v16i8:
    addRoundingConst = true;
  case AArch64::SUBHNv2i64_v2i32:
  case AArch64::SUBHNv4i32_v4i16:
  case AArch64::SUBHNv8i16_v8i8:
  case AArch64::SUBHNv2i64_v4i32:
  case AArch64::SUBHNv4i32_v8i16:
  case AArch64::SUBHNv8i16_v16i8:
    op = Instruction::BinaryOps::Sub;
    break;
  default:
    assert(false);
    break;
  }

  bool isUpper = false;
  switch (opcode) {
  // RADDHN2 variants
  case AArch64::RADDHNv2i64_v4i32:
  case AArch64::RADDHNv4i32_v8i16:
  case AArch64::RADDHNv8i16_v16i8:
  // ADDHN2 variants
  case AArch64::ADDHNv2i64_v4i32:
  case AArch64::ADDHNv4i32_v8i16:
  case AArch64::ADDHNv8i16_v16i8:
  // RSUBHN2 variants
  case AArch64::RSUBHNv2i64_v4i32:
  case AArch64::RSUBHNv4i32_v8i16:
  case AArch64::RSUBHNv8i16_v16i8:
  // SUBHN2 variants
  case AArch64::SUBHNv2i64_v4i32:
  case AArch64::SUBHNv4i32_v8i16:
  case AArch64::SUBHNv8i16_v16i8:
    isUpper = true;
    break;
  default:
    break;
  }

  auto a = readFromVecOperand(isUpper ? 2 : 1, 2 * eltSize, numElts);
  auto b = readFromVecOperand(isUpper ? 3 : 2, 2 * eltSize, numElts);

  Value *res = createBinop(a, b, op);

  if (addRoundingConst) {
    auto roundingConst =
        getElemSplat(numElts, 2 * eltSize, (uint64_t)1 << (eltSize - 1));
    res = createBinop(res, roundingConst, Instruction::BinaryOps::Add);
  }

  Value *shifted =
      createRawLShr(res, getElemSplat(numElts, 2 * eltSize, eltSize));
  res = createTrunc(shifted, getVecTy(eltSize, numElts));
  if (isUpper) {
    // Preserve the lower 64 bits so, read from destination register
    // and insert to the upper 64 bits
    Value *dest = readFromVecOperand(1, 64, 2);
    Value *element = createBitCast(res, i64);
    res = createInsertElement(dest, element, 1);
  }
  updateOutputReg(res);
}

void arm2llvm::lift_tbl(unsigned opcode) {
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
    regs.push_back(createBitCast(readFromRegOld(baseReg), fullTy));
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
}

void arm2llvm::lift_cm(unsigned opcode) {
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
  case AArch64::CMGTv1i64rz:
  case AArch64::CMGTv2i32rz:
  case AArch64::CMGEv8i8rz:
  case AArch64::CMLEv1i64rz:
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
    b = getUnsignedIntConst(0, getInstSize(opcode));
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
  case AArch64::CMGEv1i64:
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
  case AArch64::CMHSv1i64:
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
  case AArch64::CMLEv1i64rz:
  case AArch64::CMLTv1i64rz:
  case AArch64::CMHIv1i64:
  case AArch64::CMGTv1i64:
  case AArch64::CMGTv1i64rz:
  case AArch64::CMEQv1i64:
  case AArch64::CMGEv1i64:
  case AArch64::CMHSv1i64:
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
  case AArch64::CMLEv1i64rz:
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
  case AArch64::CMGEv1i64:
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
  case AArch64::CMGTv1i64rz:
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
  case AArch64::CMHSv1i64:
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
    auto *zero =
        createBitCast(getUnsignedIntConst(0, getInstSize(opcode)), vTy);
    res = createICmp(ICmpInst::Predicate::ICMP_NE, tmp, zero);
    break;
  }
  default:
    assert(false);
  }

  updateOutputReg(createSExt(res, vTy));
}

void arm2llvm::lift_bif1() {
  auto op1 = readFromOperand(1);
  auto op4 = readFromOperand(2);
  auto op3 = createNot(readFromOperand(3));
  auto res = createXor(op1, createAnd(createXor(op1, op4), op3));
  updateOutputReg(res);
}

void arm2llvm::lift_bif2() {
  auto op4 = readFromOperand(2);
  auto op1 = readFromOperand(1);
  auto op3 = readFromOperand(3);
  auto res = createXor(op1, createAnd(createXor(op1, op4), op3));
  updateOutputReg(res);
}

void arm2llvm::lift_bif3() {
  auto op1 = readFromOperand(3);
  auto op4 = readFromOperand(2);
  auto op3 = readFromOperand(1);
  auto res = createXor(op1, createAnd(createXor(op1, op4), op3));
  updateOutputReg(res);
}

void arm2llvm::lift_dup88() {
  auto i8 = getIntTy(8);
  auto t = createTrunc(readFromOperand(1), i8);
  updateOutputReg(dupElts(t, 8, 8));
}

void arm2llvm::lift_dup168() {
  auto i8 = getIntTy(8);
  auto t = createTrunc(readFromOperand(1), i8);
  updateOutputReg(dupElts(t, 16, 8));
}

void arm2llvm::lift_dup816() {
  auto i16 = getIntTy(16);
  auto t = createTrunc(readFromOperand(1), i16);
  updateOutputReg(dupElts(t, 8, 16));
}

void arm2llvm::lift_dup416() {
  auto i16 = getIntTy(16);
  auto t = createTrunc(readFromOperand(1), i16);
  updateOutputReg(dupElts(t, 4, 16));
}

void arm2llvm::lift_dup432() {
  auto i32 = getIntTy(32);
  auto t = createTrunc(readFromOperand(1), i32);
  updateOutputReg(dupElts(t, 4, 32));
}

void arm2llvm::lift_dup232() {
  auto i32 = getIntTy(32);
  auto t = createTrunc(readFromOperand(1), i32);
  updateOutputReg(dupElts(t, 2, 32));
}

void arm2llvm::lift_dup264() {
  updateOutputReg(dupElts(readFromOperand(1), 2, 64));
}

void arm2llvm::lift_dup232lane() {
  auto in = readFromVecOperand(1, 32, 4);
  auto ext = createExtractElement(in, getImm(2));
  updateOutputReg(dupElts(ext, 2, 32));
}

void arm2llvm::lift_dup264lane() {
  auto in = readFromVecOperand(1, 64, 2);
  auto ext = createExtractElement(in, getImm(2));
  updateOutputReg(dupElts(ext, 2, 64));
}

void arm2llvm::lift_dup416lane() {
  auto in = readFromVecOperand(1, 16, 8);
  auto ext = createExtractElement(in, getImm(2));
  updateOutputReg(dupElts(ext, 4, 16));
}

void arm2llvm::lift_dup432lane() {
  auto in = readFromVecOperand(1, 32, 4);
  auto ext = createExtractElement(in, getImm(2));
  updateOutputReg(dupElts(ext, 4, 32));
}

void arm2llvm::lift_dup88lane() {
  auto in = readFromVecOperand(1, 8, 16);
  auto ext = createExtractElement(in, getImm(2));
  updateOutputReg(dupElts(ext, 8, 8));
}

void arm2llvm::lift_dup816lane() {
  auto in = readFromVecOperand(1, 16, 8);
  auto ext = createExtractElement(in, getImm(2));
  updateOutputReg(dupElts(ext, 8, 16));
}

void arm2llvm::lift_dup168lane() {
  auto in = readFromVecOperand(1, 8, 16);
  auto ext = createExtractElement(in, getImm(2));
  updateOutputReg(dupElts(ext, 16, 8));
}

void arm2llvm::lift_dup8() {
  auto in = readFromVecOperand(1, 8, 16);
  auto ext = createExtractElement(in, getImm(2));
  updateOutputReg(ext);
}

void arm2llvm::lift_dup16() {
  auto in = readFromVecOperand(1, 16, 8);
  auto ext = createExtractElement(in, getImm(2));
  updateOutputReg(ext);
}

void arm2llvm::lift_dup32() {
  auto in = readFromVecOperand(1, 32, 4);
  auto ext = createExtractElement(in, getImm(2));
  updateOutputReg(ext);
}

void arm2llvm::lift_dup64() {
  auto in = readFromVecOperand(1, 64, 2);
  auto ext = createExtractElement(in, getImm(2));
  updateOutputReg(ext);
}

void arm2llvm::lift_ext_2() {
  auto i128 = getIntTy(128);
  auto a = readFromOperand(1);
  auto b = readFromOperand(2);
  auto imm = getImm(3);
  auto both = concat(b, a);
  auto shifted = createRawLShr(both, getUnsignedIntConst(8 * imm, 256));
  updateOutputReg(createTrunc(shifted, i128));
}

void arm2llvm::lift_ext_1() {
  auto i64 = getIntTy(64);
  auto a = readFromOperand(1);
  auto b = readFromOperand(2);
  auto imm = getImm(3);
  auto both = concat(b, a);
  auto shifted = createRawLShr(both, getUnsignedIntConst(8 * imm, 128));
  updateOutputReg(createTrunc(shifted, i64));
}

void arm2llvm::lift_movi_7() {
  auto imm1 = getImm(1);
  auto imm2 = getImm(2);
  auto val = getUnsignedIntConst(imm1 << imm2, 32);
  updateOutputReg(dupElts(val, 4, 32));
}

void arm2llvm::lift_movi_6() {
  auto imm1 = getImm(1);
  auto imm2 = getImm(2);
  auto val = getUnsignedIntConst(imm1 << imm2, 32);
  updateOutputReg(dupElts(val, 2, 32));
}

void arm2llvm::lift_movi_5() {
  auto imm1 = getImm(1);
  auto imm2 = getImm(2);
  auto val = getUnsignedIntConst(imm1 << imm2, 16);
  updateOutputReg(dupElts(val, 8, 16));
}

void arm2llvm::lift_movi_4() {
  auto imm1 = getImm(1);
  auto imm2 = getImm(2);
  auto val = getUnsignedIntConst(imm1 << imm2, 16);
  updateOutputReg(dupElts(val, 4, 16));
}

void arm2llvm::lift_movi_3() {
  auto v = getUnsignedIntConst(getImm(1), 8);
  updateOutputReg(dupElts(v, 16, 8));
}

void arm2llvm::lift_movi_2() {
  auto v = getUnsignedIntConst(getImm(1), 8);
  updateOutputReg(dupElts(v, 8, 8));
}

void arm2llvm::lift_movi_1() {
  auto imm = getUnsignedIntConst(replicate8to64(getImm(1)), 64);
  updateOutputReg(dupElts(imm, 2, 64));
}

void arm2llvm::lift_movi_msl(unsigned opcode) {
  auto imm1 = getUnsignedIntConst(getImm(1), 32);
  auto imm2 = getImm(2) & ~0x100;
  auto v = createMSL(imm1, imm2);
  int numElts = (opcode == AArch64::MOVIv2s_msl) ? 2 : 4;
  updateOutputReg(dupElts(v, numElts, 32));
}

void arm2llvm::lift_mvni_msl(unsigned opcode) {
  auto imm1 = getUnsignedIntConst(getImm(1), 32);
  auto imm2 = getImm(2) & ~0x100;
  auto v = createNot(createMSL(imm1, imm2));
  int numElts = (opcode == AArch64::MVNIv2s_msl) ? 2 : 4;
  updateOutputReg(dupElts(v, numElts, 32));
}

void arm2llvm::lift_mvni(unsigned opcode) {
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
  auto imm1 = getUnsignedIntConst(getImm(1), eltSize);
  auto imm2 = getUnsignedIntConst(getImm(2), eltSize);
  auto v = createNot(createRawShl(imm1, imm2));
  updateOutputReg(dupElts(v, numElts, eltSize));
}

void arm2llvm::lift_umov_vi(unsigned opcode) {
  unsigned sz;
  if (opcode == AArch64::UMOVvi8 || opcode == AArch64::UMOVvi8_idx0) {
    sz = 8;
  } else if (opcode == AArch64::UMOVvi16 || opcode == AArch64::UMOVvi16_idx0) {
    sz = 16;
  } else if (opcode == AArch64::UMOVvi32 || opcode == AArch64::UMOVvi32_idx0) {
    sz = 32;
  } else if (opcode == AArch64::UMOVvi64) {
    sz = 64;
  } else {
    assert(false);
  }
  unsigned idx;
  if (opcode == AArch64::UMOVvi8_idx0 || opcode == AArch64::UMOVvi16_idx0 ||
      opcode == AArch64::UMOVvi32_idx0) {
    idx = 0;
  } else {
    idx = getImm(2);
  }
  auto vTy = getVecTy(sz, 128 / sz);
  auto reg = createBitCast(readFromOperand(1), vTy);
  auto val = createExtractElement(reg, idx);
  updateOutputReg(val);
}

void arm2llvm::lift_smov_vi(unsigned opcode) {
  auto i8 = getIntTy(8);
  auto i16 = getIntTy(16);
  auto i32 = getIntTy(32);
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
  default:
    assert(false && "error");
  }
  auto shiftAmt = getUnsignedIntConst(index << eltSizeLog2, 128);
  auto shifted = createRawLShr(val, shiftAmt);
  auto trunced = createTrunc(shifted, truncSize);
  updateOutputReg(trunced, true);
}

void arm2llvm::lift_ins_lane(unsigned opcode) {
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
}

void arm2llvm::lift_ins_gpr(unsigned opcode) {
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
  auto ty = getVecTy(w, 128 / w);
  auto vec = readFromRegTyped(CurInst->getOperand(1).getReg(), ty);
  auto inserted = createInsertElement(vec, val, lane);
  updateOutputReg(inserted);
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

void arm2llvm::lift_vec_mul(unsigned opcode) {
  unsigned eltSize, numElts;
  GET_SIZES4(MUL, _indexed);
  auto a = readFromVecOperand(1, eltSize, numElts);
  auto e2 =
      getIndexedElement(getImm(3), eltSize, CurInst->getOperand(2).getReg());
  Value *res = getUndefVec(numElts, eltSize);
  for (unsigned i = 0; i < numElts; ++i) {
    auto e1 = createExtractElement(a, i);
    res = createInsertElement(res, createMul(e1, e2), i);
  }
  updateOutputReg(res);
}

void arm2llvm::lift_vec_mls(unsigned opcode) {
  int numElts, eltSize;
  GET_SIZES4(MLS, _indexed);
  auto vTy = getVecTy(eltSize, numElts);
  auto a = createBitCast(readFromOperand(1), vTy);
  auto b = createBitCast(readFromOperand(2), vTy);
  auto e =
      getIndexedElement(getImm(4), eltSize, CurInst->getOperand(3).getReg());
  auto spl = splat(e, numElts, eltSize);
  auto mul = createMul(b, spl);
  auto sum = createSub(a, mul);
  updateOutputReg(sum);
}

void arm2llvm::lift_vec_mla(unsigned opcode) {
  int numElts, eltSize;
  GET_SIZES4(MLA, _indexed);
  auto vTy = getVecTy(eltSize, numElts);
  auto a = createBitCast(readFromOperand(1), vTy);
  auto b = createBitCast(readFromOperand(2), vTy);
  auto e =
      getIndexedElement(getImm(4), eltSize, CurInst->getOperand(3).getReg());
  auto spl = splat(e, numElts, eltSize);
  auto mul = createMul(b, spl);
  auto sum = createAdd(mul, a);
  updateOutputReg(sum);
}

#undef GET_SIZES4

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

void arm2llvm::lift_trn(unsigned opcode) {
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
}

void arm2llvm::lift_vec_minmax(unsigned opcode) {
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
}

#undef GET_SIZES5

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

void arm2llvm::lift_shll(unsigned opcode) {
  unsigned eltSize = -1, numElts = -1;
  GET_SIZES6(SHLL, )

  bool isUpper = false;
  switch (opcode) {
  case AArch64::SHLLv16i8:
  case AArch64::SHLLv8i16:
  case AArch64::SHLLv4i32:
    isUpper = true;
    break;
  }

  auto a = readFromVecOperand(1, eltSize, numElts, isUpper);
  Value *extended_a, *b;
  if (isUpper) {
    numElts /= 2;
    extended_a = createZExt(a, getVecTy(eltSize * 2, numElts));
    b = getElemSplat(numElts, 2 * eltSize, eltSize);
  } else {
    extended_a = createZExt(a, getVecTy(eltSize * 2, numElts));
    b = getElemSplat(numElts, 2 * eltSize, eltSize);
  }

  auto res = createMaskedShl(extended_a, b);
  updateOutputReg(res);
}

void arm2llvm::lift_shrn(unsigned opcode) {
  auto i64 = getIntTy(64);
  unsigned numElts = -1, eltSize = -1;
  GET_SIZES6(SHRN, _shift)
  GET_SIZES6(RSHRN, _shift)

  bool isUpper = false;
  switch (opcode) {
  case AArch64::SHRNv16i8_shift:
  case AArch64::SHRNv8i16_shift:
  case AArch64::SHRNv4i32_shift:
  case AArch64::RSHRNv16i8_shift:
  case AArch64::RSHRNv8i16_shift:
  case AArch64::RSHRNv4i32_shift:
    isUpper = true;
    break;
  }

  bool addRoundingConst = false;
  switch (opcode) {
  case AArch64::RSHRNv8i8_shift:
  case AArch64::RSHRNv16i8_shift:
  case AArch64::RSHRNv4i16_shift:
  case AArch64::RSHRNv8i16_shift:
  case AArch64::RSHRNv2i32_shift:
  case AArch64::RSHRNv4i32_shift:
    addRoundingConst = true;
    break;
  }

  Value *a, *b, *roundingConst;
  if (isUpper) {
    numElts /= 2;
    a = readFromVecOperand(2, eltSize * 2, numElts);
    roundingConst =
        getElemSplat(numElts, eltSize * 2, (uint64_t)1 << (getImm(3) - 1));
    b = getElemSplat(numElts, eltSize * 2, getImm(3));
  } else {
    a = readFromVecOperand(1, eltSize * 2, numElts);
    roundingConst =
        getElemSplat(numElts, eltSize * 2, (uint64_t)1 << (getImm(2) - 1));
    b = getElemSplat(numElts, eltSize * 2, getImm(2));
  }

  if (addRoundingConst) {
    a = createAdd(a, roundingConst);
  }

  auto shiftedVec = createMaskedLShr(a, b);
  Value *res = createTrunc(shiftedVec, getVecTy(eltSize, numElts));
  if (isUpper) {
    Value *dest = readFromVecOperand(1, 64, 2);
    Value *element = createBitCast(res, i64);
    res = createInsertElement(dest, element, 1);
  }

  updateOutputReg(res);
}

void arm2llvm::lift_sli_sri(unsigned opcode) {
  unsigned numElts = -1, eltSize = -1;
  switch (opcode) {
  case AArch64::SLIv8i8_shift:
  case AArch64::SRIv8i8_shift:
    numElts = 8;
    eltSize = 8;
    break;
  case AArch64::SLIv16i8_shift:
  case AArch64::SRIv16i8_shift:
    numElts = 16;
    eltSize = 8;
    break;
  case AArch64::SLIv4i16_shift:
  case AArch64::SRIv4i16_shift:
    numElts = 4;
    eltSize = 16;
    break;
  case AArch64::SLIv8i16_shift:
  case AArch64::SRIv8i16_shift:
    numElts = 8;
    eltSize = 16;
    break;
  case AArch64::SLIv2i32_shift:
  case AArch64::SRIv2i32_shift:
    numElts = 2;
    eltSize = 32;
    break;
  case AArch64::SLIv4i32_shift:
  case AArch64::SRIv4i32_shift:
    numElts = 4;
    eltSize = 32;
    break;
  case AArch64::SLId:
  case AArch64::SRId:
    numElts = 1;
    eltSize = 64;
    break;
  case AArch64::SLIv2i64_shift:
  case AArch64::SRIv2i64_shift:
    numElts = 2;
    eltSize = 64;
    break;
  default:
    assert(false && "Invalid opcode for SLI");
    break;
  }

  auto shiftAmt = getImm(3);
  auto a = readFromVecOperand(2, eltSize, numElts);
  auto shiftVec = getElemSplat(numElts, eltSize, shiftAmt);
  Value *shifted_a;
  uint64_t maskAmt;
  switch (opcode) {
  case AArch64::SLIv8i8_shift:
  case AArch64::SLIv16i8_shift:
  case AArch64::SLIv4i16_shift:
  case AArch64::SLIv8i16_shift:
  case AArch64::SLIv2i32_shift:
  case AArch64::SLIv4i32_shift:
  case AArch64::SLId:
  case AArch64::SLIv2i64_shift:
    shifted_a = createMaskedShl(a, shiftVec);
    maskAmt = (1ULL << shiftAmt) - 1;
    break;
  case AArch64::SRIv8i8_shift:
  case AArch64::SRIv16i8_shift:
  case AArch64::SRIv4i16_shift:
  case AArch64::SRIv8i16_shift:
  case AArch64::SRIv2i32_shift:
  case AArch64::SRIv4i32_shift:
  case AArch64::SRId:
  case AArch64::SRIv2i64_shift:
    shifted_a = createMaskedLShr(a, shiftVec);
    maskAmt = ((1ULL << shiftAmt) - 1) << (eltSize - shiftAmt);
    break;
  default:
    assert(false && "Invalid opcode for SLI");
    break;
  }

  auto res = readFromVecOperand(1, eltSize, numElts);
  auto mask = getElemSplat(numElts, eltSize, maskAmt);
  auto masked_res = createAnd(res, mask);

  updateOutputReg(createOr(masked_res, shifted_a));
}

void arm2llvm::lift_mls(unsigned opcode) {
  unsigned numElts = -1, eltSize = -1;
  GET_SIZES6(MLS, )
  auto a = readFromVecOperand(1, eltSize, numElts);
  auto b = readFromVecOperand(2, eltSize, numElts);
  auto c = readFromVecOperand(3, eltSize, numElts);
  auto mul = createMul(b, c);
  auto sum = createSub(a, mul);
  updateOutputReg(sum);
}

void arm2llvm::lift_mla(unsigned opcode) {
  unsigned numElts = -1, eltSize = -1;
  GET_SIZES6(MLA, )
  auto a = readFromVecOperand(1, eltSize, numElts);
  auto b = readFromVecOperand(2, eltSize, numElts);
  auto c = readFromVecOperand(3, eltSize, numElts);
  auto mul = createMul(b, c);
  auto sum = createAdd(mul, a);
  updateOutputReg(sum);
}

void arm2llvm::lift_abal_abdl(unsigned opcode) {
  unsigned numElts, eltSize;
  bool isUpper = false;
  switch (opcode) {
  case AArch64::UABALv8i8_v8i16:
  case AArch64::UABDLv8i8_v8i16:
  case AArch64::SABALv8i8_v8i16:
  case AArch64::SABDLv8i8_v8i16:
    numElts = 8;
    eltSize = 8;
    break;
  case AArch64::UABALv16i8_v8i16:
  case AArch64::UABDLv16i8_v8i16:
  case AArch64::SABALv16i8_v8i16:
  case AArch64::SABDLv16i8_v8i16:
    numElts = 16;
    eltSize = 8;
    isUpper = true;
    break;
  case AArch64::UABALv4i16_v4i32:
  case AArch64::UABDLv4i16_v4i32:
  case AArch64::SABALv4i16_v4i32:
  case AArch64::SABDLv4i16_v4i32:
    numElts = 4;
    eltSize = 16;
    break;
  case AArch64::UABALv8i16_v4i32:
  case AArch64::UABDLv8i16_v4i32:
  case AArch64::SABALv8i16_v4i32:
  case AArch64::SABDLv8i16_v4i32:
    numElts = 8;
    eltSize = 16;
    isUpper = true;
    break;
  case AArch64::UABALv2i32_v2i64:
  case AArch64::UABDLv2i32_v2i64:
  case AArch64::SABALv2i32_v2i64:
  case AArch64::SABDLv2i32_v2i64:
    numElts = 2;
    eltSize = 32;
    break;
  case AArch64::UABALv4i32_v2i64:
  case AArch64::UABDLv4i32_v2i64:
  case AArch64::SABALv4i32_v2i64:
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
  case AArch64::SABALv8i8_v8i16:
  case AArch64::SABALv16i8_v8i16:
  case AArch64::SABALv4i16_v4i32:
  case AArch64::SABALv8i16_v4i32:
  case AArch64::SABALv2i32_v2i64:
  case AArch64::SABALv4i32_v2i64:
  case AArch64::SABDLv8i8_v8i16:
  case AArch64::SABDLv16i8_v8i16:
  case AArch64::SABDLv4i16_v4i32:
  case AArch64::SABDLv8i16_v4i32:
  case AArch64::SABDLv2i32_v2i64:
  case AArch64::SABDLv4i32_v2i64:
    isSigned = true;
    break;
  }

  bool accumulate = false;
  switch (opcode) {
  case AArch64::UABALv8i8_v8i16:
  case AArch64::UABALv16i8_v8i16:
  case AArch64::UABALv4i16_v4i32:
  case AArch64::UABALv8i16_v4i32:
  case AArch64::UABALv2i32_v2i64:
  case AArch64::UABALv4i32_v2i64:
  case AArch64::SABALv8i8_v8i16:
  case AArch64::SABALv16i8_v8i16:
  case AArch64::SABALv4i16_v4i32:
  case AArch64::SABALv8i16_v4i32:
  case AArch64::SABALv2i32_v2i64:
  case AArch64::SABALv4i32_v2i64:
    accumulate = true;
    break;
  }

  auto a = readFromVecOperand(accumulate ? 2 : 1, eltSize, numElts, isUpper);
  auto b = readFromVecOperand(accumulate ? 3 : 2, eltSize, numElts, isUpper);

  Value *extended_a, *extended_b;

  if (isSigned) {
    extended_a =
        createSExt(a, getVecTy(2 * eltSize, isUpper ? numElts / 2 : numElts));
    extended_b =
        createSExt(b, getVecTy(2 * eltSize, isUpper ? numElts / 2 : numElts));
  } else {
    extended_a =
        createZExt(a, getVecTy(2 * eltSize, isUpper ? numElts / 2 : numElts));
    extended_b =
        createZExt(b, getVecTy(2 * eltSize, isUpper ? numElts / 2 : numElts));
  }

  auto difference = createSub(extended_a, extended_b);
  Value *res = createAbs(difference);

  if (accumulate) {
    auto c =
        readFromVecOperand(1, 2 * eltSize, isUpper ? numElts / 2 : numElts);
    res = createAdd(c, res);
  }

  updateOutputReg(res);
}

void arm2llvm::lift_mlal_mlsl_idx(unsigned opcode) {
  unsigned numElts, eltSize;
  switch (opcode) {
  case AArch64::UMLALv2i32_indexed:
  case AArch64::UMLSLv2i32_indexed:
  case AArch64::SMLALv2i32_indexed:
  case AArch64::SMLSLv2i32_indexed:
    numElts = 2;
    eltSize = 32;
    break;
  case AArch64::UMLALv4i16_indexed:
  case AArch64::UMLSLv4i16_indexed:
  case AArch64::SMLALv4i16_indexed:
  case AArch64::SMLSLv4i16_indexed:
    numElts = 4;
    eltSize = 16;
    break;
  case AArch64::UMLALv4i32_indexed:
  case AArch64::UMLSLv4i32_indexed:
  case AArch64::SMLALv4i32_indexed:
  case AArch64::SMLSLv4i32_indexed:
    numElts = 4;
    eltSize = 32;
    break;
  case AArch64::UMLALv8i16_indexed:
  case AArch64::UMLSLv8i16_indexed:
  case AArch64::SMLALv8i16_indexed:
  case AArch64::SMLSLv8i16_indexed:
    numElts = 8;
    eltSize = 16;
    break;
  default:
    assert(false);
  }
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
         CurInst->getOperand(0).getReg() == CurInst->getOperand(1).getReg());
  assert(CurInst->getOperand(4).isImm());

  auto numEltsAdj = isUpper ? numElts / 2 : numElts;

  auto destReg = readFromVecOperand(1, 2 * eltSize, numEltsAdj);
  auto a = readFromVecOperand(2, eltSize, numElts, isUpper);
  auto element =
      getIndexedElement(getImm(4), eltSize, CurInst->getOperand(3).getReg());
  auto splatElement = splat(element, numEltsAdj, eltSize);
  auto *eTy = getVecTy(2 * eltSize, numEltsAdj);
  auto extended_a = isSigned ? createSExt(a, eTy) : createZExt(a, eTy);
  auto extended_b =
      isSigned ? createSExt(splatElement, eTy) : createZExt(splatElement, eTy);
  auto mul = createMul(extended_a, extended_b);
  auto sum = isSub ? createSub(destReg, mul) : createAdd(destReg, mul);

  updateOutputReg(sum);
}

void arm2llvm::lift_mlal_mlsl(unsigned opcode) {
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
         CurInst->getOperand(0).getReg() == CurInst->getOperand(1).getReg());
  auto destReg =
      readFromVecOperand(1, 2 * eltSize, isUpper ? numElts / 2 : numElts);
  auto a = readFromVecOperand(2, eltSize, numElts, isUpper);
  auto b = readFromVecOperand(3, eltSize, numElts, isUpper);

  auto extended_a =
      isSigned
          ? createSExt(a,
                       getVecTy(2 * eltSize, isUpper ? numElts / 2 : numElts))
          : createZExt(a,
                       getVecTy(2 * eltSize, isUpper ? numElts / 2 : numElts));
  auto extended_b =
      isSigned
          ? createSExt(b,
                       getVecTy(2 * eltSize, isUpper ? numElts / 2 : numElts))
          : createZExt(b,
                       getVecTy(2 * eltSize, isUpper ? numElts / 2 : numElts));

  auto mul = createMul(extended_a, extended_b);
  auto res = isSub ? createSub(destReg, mul) : createAdd(destReg, mul);

  updateOutputReg(res);
}

void arm2llvm::lift_aba_abd(unsigned opcode) {
  unsigned numElts = -1, eltSize = -1;
  GET_SIZES6(UABA, )
  GET_SIZES6(UABD, )
  GET_SIZES6(SABA, )
  GET_SIZES6(SABD, )

  bool isSigned = false;
  switch (opcode) {
  case AArch64::SABAv8i8:
  case AArch64::SABAv16i8:
  case AArch64::SABAv4i16:
  case AArch64::SABAv8i16:
  case AArch64::SABAv2i32:
  case AArch64::SABAv4i32:
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

  bool accumulate = false;
  switch (opcode) {
  case AArch64::UABAv8i8:
  case AArch64::UABAv16i8:
  case AArch64::UABAv4i16:
  case AArch64::UABAv8i16:
  case AArch64::UABAv2i32:
  case AArch64::UABAv4i32:
  case AArch64::SABAv8i8:
  case AArch64::SABAv16i8:
  case AArch64::SABAv4i16:
  case AArch64::SABAv8i16:
  case AArch64::SABAv2i32:
  case AArch64::SABAv4i32:
    accumulate = true;
    break;
  }

  auto a = readFromVecOperand(accumulate ? 2 : 1, eltSize, numElts);
  auto b = readFromVecOperand(accumulate ? 3 : 2, eltSize, numElts);

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
  Value *res = createTrunc(abs, getVecTy(eltSize, numElts));
  if (accumulate) {
    auto c = readFromVecOperand(1, eltSize, numElts);
    res = createAdd(c, res);
  }

  updateOutputReg(res);
}

void arm2llvm::lift_more_vec_binops(unsigned opcode) {
  unsigned numElts = -1, eltSize = -1;
  GET_SIZES6(URHADD, )
  GET_SIZES6(UHADD, )
  GET_SIZES6(SRHADD, )
  GET_SIZES6(SHADD, )
  GET_SIZES6(UHSUB, )
  GET_SIZES6(SHSUB, )

  bool isSigned = false;
  switch (opcode) {
  case AArch64::SRHADDv8i8:
  case AArch64::SRHADDv16i8:
  case AArch64::SRHADDv4i16:
  case AArch64::SRHADDv8i16:
  case AArch64::SRHADDv2i32:
  case AArch64::SRHADDv4i32:
  case AArch64::SHADDv8i8:
  case AArch64::SHADDv16i8:
  case AArch64::SHADDv4i16:
  case AArch64::SHADDv8i16:
  case AArch64::SHADDv2i32:
  case AArch64::SHADDv4i32:
  case AArch64::SHSUBv8i8:
  case AArch64::SHSUBv16i8:
  case AArch64::SHSUBv4i16:
  case AArch64::SHSUBv8i16:
  case AArch64::SHSUBv2i32:
  case AArch64::SHSUBv4i32:
    isSigned = true;
    break;
  }

  Instruction::BinaryOps op;
  bool addRoundingConst = false;
  switch (opcode) {
  case AArch64::URHADDv8i8:
  case AArch64::URHADDv16i8:
  case AArch64::URHADDv4i16:
  case AArch64::URHADDv8i16:
  case AArch64::URHADDv2i32:
  case AArch64::URHADDv4i32:
  case AArch64::SRHADDv8i8:
  case AArch64::SRHADDv16i8:
  case AArch64::SRHADDv4i16:
  case AArch64::SRHADDv8i16:
  case AArch64::SRHADDv2i32:
  case AArch64::SRHADDv4i32:
    addRoundingConst = true;
  case AArch64::UHADDv8i8:
  case AArch64::UHADDv16i8:
  case AArch64::UHADDv4i16:
  case AArch64::UHADDv8i16:
  case AArch64::UHADDv2i32:
  case AArch64::UHADDv4i32:
  case AArch64::SHADDv8i8:
  case AArch64::SHADDv16i8:
  case AArch64::SHADDv4i16:
  case AArch64::SHADDv8i16:
  case AArch64::SHADDv2i32:
  case AArch64::SHADDv4i32:
    op = Instruction::Add;
    break;
  case AArch64::UHSUBv8i8:
  case AArch64::UHSUBv16i8:
  case AArch64::UHSUBv4i16:
  case AArch64::UHSUBv8i16:
  case AArch64::UHSUBv2i32:
  case AArch64::UHSUBv4i32:
  case AArch64::SHSUBv8i8:
  case AArch64::SHSUBv16i8:
  case AArch64::SHSUBv4i16:
  case AArch64::SHSUBv8i16:
  case AArch64::SHSUBv2i32:
  case AArch64::SHSUBv4i32:
    op = Instruction::Sub;
    break;
  default:
    assert(false);
  }

  Type *vecTy = getVecTy(eltSize, numElts);
  Type *vecTyDoubledEltSize = getVecTy(2 * eltSize, numElts);

  auto a = isSigned ? createSExt(readFromVecOperand(1, eltSize, numElts),
                                 vecTyDoubledEltSize)
                    : createZExt(readFromVecOperand(1, eltSize, numElts),
                                 vecTyDoubledEltSize);
  auto b = isSigned ? createSExt(readFromVecOperand(2, eltSize, numElts),
                                 vecTyDoubledEltSize)
                    : createZExt(readFromVecOperand(2, eltSize, numElts),
                                 vecTyDoubledEltSize);

  auto res = createBinop(a, b, op);
  if (addRoundingConst) {
    auto roundingConst = getElemSplat(numElts, 2 * eltSize, 1);
    res = createAdd(res, roundingConst);
  }

  auto onesVec = getElemSplat(numElts, 2 * eltSize, 1);
  auto shifted = createRawAShr(res, onesVec);

  updateOutputReg(createTrunc(shifted, vecTy));
}

#undef GET_SIZES6

#define GET_SIZES7(INSN, SUFF)                                                 \
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
  }

void arm2llvm::lift_usra(unsigned opcode) {
  unsigned numElts = 999, eltSize = 999;
  GET_SIZES7(USRA, _shift);
  if (opcode == AArch64::USRAd) {
    numElts = 1;
    eltSize = 64;
  }
  assert(numElts != 999 && eltSize != 999);
  auto a = readFromVecOperand(1, eltSize, numElts);
  auto b = readFromVecOperand(2, eltSize, numElts);
  auto exp = getImm(3);
  Value *res = getUndefVec(numElts, eltSize);
  for (unsigned i = 0; i < numElts; ++i) {
    auto e1 = createExtractElement(a, i);
    auto e2 = createExtractElement(b, i);
    auto shift = createMaskedLShr(e2, getUnsignedIntConst(exp, eltSize));
    auto sum = createAdd(e1, shift);
    res = createInsertElement(res, sum, i);
  }
  updateOutputReg(res);
}

void arm2llvm::lift_zip1(unsigned opcode) {
  unsigned numElts = -1, eltSize = -1;
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
}

void arm2llvm::lift_zip2(unsigned opcode) {
  unsigned numElts = -1, eltSize = -1;
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
}

void arm2llvm::lift_addp(unsigned opcode) {
  unsigned numElts = -1, eltSize = -1;
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
}

#undef GET_SIZES7

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
void arm2llvm::lift_uqadd(unsigned opcode) {
  GET_SIZES9(UQADD, );
  auto x = readFromVecOperand(1, eltSize, numElts);
  auto y = readFromVecOperand(2, eltSize, numElts);
  auto res = createUAddSat(x, y);
  updateOutputReg(res);
}

void arm2llvm::lift_uqsub(unsigned opcode) {
  GET_SIZES9(UQSUB, );
  auto x = readFromVecOperand(1, eltSize, numElts);
  auto y = readFromVecOperand(2, eltSize, numElts);
  auto res = createUSubSat(x, y);
  updateOutputReg(res);
}

void arm2llvm::lift_sqadd(unsigned opcode) {
  GET_SIZES9(SQADD, );
  auto x = readFromVecOperand(1, eltSize, numElts);
  auto y = readFromVecOperand(2, eltSize, numElts);
  auto res = createSAddSat(x, y);
  updateOutputReg(res);
}

void arm2llvm::lift_sqsub(unsigned opcode) {
  GET_SIZES9(SQSUB, );
  auto x = readFromVecOperand(1, eltSize, numElts);
  auto y = readFromVecOperand(2, eltSize, numElts);
  auto res = createSSubSat(x, y);
  updateOutputReg(res);
}

#undef GET_SIZES9
