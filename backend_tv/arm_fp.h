#pragma once

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"

#include <cassert>
#include <cstdint>

namespace lifter {

// Arm A64 ISA 2026-06: FPMin/FPMax, FPMinNum/FPMaxNum, and FPProcessNaNs.
// Model the default FPCR (AH=DN=FZ=FZ16=0); exception flags are not modeled.
// LLVM's minnum/maxnum allow more outcomes than these instructions, both
// for signaling NaNs and for the returned NaN's sign and payload.
inline llvm::Value *createArmFPMinMax(llvm::IRBuilder<> &B, llvm::Value *a,
                                     llvm::Value *b, bool min, bool number) {
  auto *ty = a->getType();
  assert(ty == b->getType() && ty->isFloatingPointTy());
  unsigned width = ty->getScalarSizeInBits();
  assert(width == 16 || width == 32 || width == 64);
  unsigned quietBit = width == 16 ? 9 : width == 32 ? 22 : 51;
  auto *intTy = B.getIntNTy(width);
  auto *quiet = llvm::ConstantInt::get(intTy, uint64_t(1) << quietBit);
  auto *abits = B.CreateBitCast(a, intTy);
  auto *bbits = B.CreateBitCast(b, intTy);
  auto *an = B.CreateFCmpUNO(a, a);
  auto *bn = B.CreateFCmpUNO(b, b);
  auto *as = B.CreateAnd(an, B.CreateICmpEQ(B.CreateAnd(abits, quiet),
                                          llvm::ConstantInt::get(intTy, 0)));
  auto *bs = B.CreateAnd(bn, B.CreateICmpEQ(B.CreateAnd(bbits, quiet),
                                          llvm::ConstantInt::get(intTy, 0)));

  // The first signaling NaN wins, otherwise the first quiet NaN wins.
  auto *nan = B.CreateSelect(as, abits,
                             B.CreateSelect(bs, bbits,
                                            B.CreateSelect(an, abits, bbits)));
  nan = B.CreateBitCast(B.CreateOr(nan, quiet), ty);
  auto *propagate = number ? B.CreateOr(B.CreateOr(as, bs), B.CreateAnd(an, bn))
                           : B.CreateOr(an, bn);
  auto id = min ? llvm::Intrinsic::minimumnum : llvm::Intrinsic::maximumnum;
  auto *numeric = B.CreateBinaryIntrinsic(id, a, b);
  return B.CreateSelect(propagate, nan, numeric);
}

} // namespace lifter
