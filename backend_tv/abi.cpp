#include "backend_tv/abi.h"

#include "llvm/Support/MathExtras.h"

#include <cassert>
#include <string>

using namespace llvm;

namespace lifter {

std::string toString(const ArgLoc &loc) {
  std::string s;
  switch (loc.kind) {
  case ArgLoc::None:
    return "NONE";
  case ArgLoc::Unsupported:
    return "UNSUPPORTED";
  case ArgLoc::Direct:
    s = "DIRECT";
    break;
  case ArgLoc::Indirect:
    s = "INDIRECT";
    break;
  case ArgLoc::Vector:
    s = "VECTOR";
    break;
  }
  for (auto &limb : loc.limbs) {
    s += " ";
    if (!limb.inReg)
      s += "s" + std::to_string(limb.stackOffset);
    else if (loc.kind == ArgLoc::Vector)
      s += "v" + std::to_string(limb.reg);
    else if (loc.kind == ArgLoc::Indirect &&
             limb.reg == CCAssigner::AArch64IndirectResultReg)
      s += "X8";
    else
      s += "r" + std::to_string(limb.reg);
  }
  return s;
}

bool canPlace(const ArgLoc &loc) {
  return loc.kind != ArgLoc::Unsupported && loc.kind != ArgLoc::Indirect;
}

CCAssigner::CCAssigner(Target target, const DataLayout &DL, Type *retTy)
    : target{target}, DL{DL} {
  ret = assignRet(retTy);
  /*
   * RV64 returns anything wider than two registers through a buffer the
   * caller allocates, and the backend passes that buffer's address as a
   * prepended first argument -- so it consumes a0 and shifts every real
   * argument down by one register. AArch64 uses x8 for the same job,
   * which is not an argument register and shifts nothing.
   */
  if (target == Target::RISCV64 && ret.kind == ArgLoc::Indirect)
    ++nextGPR;
}

LimbLoc CCAssigner::takeStack(uint64_t size, uint64_t align) {
  stackOff = alignTo(stackOff, align);
  LimbLoc loc{false, 0, stackOff};
  stackOff += size;
  return loc;
}

LimbLoc CCAssigner::takeGPROrStack() {
  if (nextGPR < numArgGPRs)
    return LimbLoc{true, nextGPR++, 0};
  return takeStack(limbBits() / 8, limbBits() / 8);
}

/*
 * AArch64AllocateSelectionDAG assigns each legalized i64 part in turn. the
 * first part of a multi-part value carries the IsSplit flag -- see
 * SelectionDAGBuilder, which sets it only on part 0 -- and matches this
 * rule from AArch64CallingConvention.td:
 *
 *   CCIfType<[i64], CCIfSplit<CCAssignToRegWithShadow<[X0, X2, X4, X6],
 *                                                     [X0, X1, X3, X5]>>>,
 *   CCIfType<[i64], CCIfSplit<CCAssignToStackWithShadow<8, 16, [X7]>>>,
 *
 * so the first limb of a multi-limb value lands in an even-numbered
 * register, burning the odd register it skipped over, and every later limb
 * takes the ordinary consecutive-register rule. when no even register is
 * left the whole value starts on the stack at 16-byte alignment and x7 is
 * burned as well.
 */
ArgLoc CCAssigner::assignArgAArch64(Type *ty) {
  ArgLoc loc;
  loc.bitWidth = DL.getTypeSizeInBits(ty);
  loc.limbBits = limbBits();

  if (ty->isVectorTy() || ty->isFloatingPointTy()) {
    loc.kind = ArgLoc::Vector;
    if (nextVec < numArgVecRegs) {
      loc.limbs.push_back(LimbLoc{true, nextVec++, 0});
    } else {
      uint64_t size = alignTo(loc.bitWidth, limbBits()) / 8;
      loc.limbs.push_back(takeStack(size, loc.bitWidth > limbBits() ? 16 : 8));
    }
    return loc;
  }

  if (!ty->isIntegerTy() && !ty->isPointerTy()) {
    loc.kind = ArgLoc::Unsupported;
    return loc;
  }
  loc.kind = ArgLoc::Direct;
  unsigned n = numLimbs(loc.bitWidth);
  loc.topLimbBits = loc.bitWidth - (n - 1) * limbBits();

  if (n == 1) {
    loc.limbs.push_back(takeGPROrStack());
    return loc;
  }

  // limb 0 needs an even-numbered register
  unsigned even = alignTo(nextGPR, 2u);
  if (even < numArgGPRs) {
    loc.limbs.push_back(LimbLoc{true, even, 0});
    nextGPR = even + 1;
  } else {
    loc.limbs.push_back(takeStack(limbBits() / 8, 16));
    // the shadow on the stack rule burns x7, so nothing later gets a
    // register either
    nextGPR = numArgGPRs;
  }

  for (unsigned i = 1; i != n; ++i)
    loc.limbs.push_back(takeGPROrStack());

  return loc;
}

/*
 * RV64 has no even-register requirement, and CC_RISCVAssign2XLen will
 * happily split a two-limb value between the last argument register and
 * the stack. anything wider than two limbs is passed by reference:
 * "Scalars wider than 2*XLEN bits are passed by reference and are replaced
 * in the argument list with the address" (riscv-elf-psabi-doc).
 */
ArgLoc CCAssigner::assignArgRISCV(Type *ty) {
  ArgLoc loc;
  loc.bitWidth = DL.getTypeSizeInBits(ty);
  loc.limbBits = limbBits();

  if (ty->isVectorTy()) {
    loc.kind = ArgLoc::Unsupported;
    return loc;
  }

  if (ty->isFloatingPointTy()) {
    if (nextVec < numArgVecRegs) {
      loc.kind = ArgLoc::Vector;
      loc.limbs.push_back(LimbLoc{true, nextVec++, 0});
      return loc;
    }
    // with the FP registers gone, a float is passed like an integer of the
    // same width
    loc.kind = ArgLoc::Direct;
    loc.topLimbBits = loc.bitWidth;
    loc.limbs.push_back(takeGPROrStack());
    return loc;
  }

  if (!ty->isIntegerTy() && !ty->isPointerTy()) {
    // RV64 vectors, in particular, are not modelled here
    loc.kind = ArgLoc::Unsupported;
    return loc;
  }
  unsigned n = numLimbs(loc.bitWidth);

  if (n > 2) {
    loc.kind = ArgLoc::Indirect;
    loc.topLimbBits = limbBits();
    loc.limbs.push_back(takeGPROrStack());
    return loc;
  }

  loc.kind = ArgLoc::Direct;
  loc.topLimbBits = loc.bitWidth - (n - 1) * limbBits();

  if (n == 1) {
    loc.limbs.push_back(takeGPROrStack());
    return loc;
  }

  if (nextGPR < numArgGPRs) {
    // at least the low limb goes in a register; the high limb follows it
    // into the next register, or onto the stack if there isn't one
    loc.limbs.push_back(LimbLoc{true, nextGPR++, 0});
    loc.limbs.push_back(takeGPROrStack());
  } else {
    // both limbs on the stack, the first at the type's own alignment
    uint64_t align =
        std::max<uint64_t>(limbBits() / 8, DL.getABITypeAlign(ty).value());
    loc.limbs.push_back(takeStack(limbBits() / 8, align));
    loc.limbs.push_back(takeStack(limbBits() / 8, limbBits() / 8));
  }
  return loc;
}

ArgLoc CCAssigner::assignRet(Type *ty) {
  ArgLoc loc;
  if (ty->isVoidTy())
    return loc;

  loc.bitWidth = DL.getTypeSizeInBits(ty);
  loc.limbBits = limbBits();

  if (ty->isVectorTy() && target == Target::RISCV64) {
    // RV64 vectors are not modelled here
    loc.kind = ArgLoc::Unsupported;
    return loc;
  }

  if (ty->isVectorTy() || ty->isFloatingPointTy()) {
    loc.kind = ArgLoc::Vector;
    loc.limbs.push_back(LimbLoc{true, 0, 0});
    return loc;
  }

  if (!ty->isIntegerTy() && !ty->isPointerTy()) {
    loc.kind = ArgLoc::Unsupported;
    return loc;
  }
  unsigned n = numLimbs(loc.bitWidth);

  /*
   * AArch64 returns a value in x0-x7 when it fits, so everything up to
   * i512 comes back in registers. RV64 refuses any return split into more
   * than two parts -- see CC_RISCV_Impl's "IsRet && ValNo > 1" -- so the
   * limit there is 2*XLEN.
   */
  unsigned maxRetLimbs = target == Target::AArch64 ? numArgGPRs : 2;

  if (n > maxRetLimbs) {
    loc.kind = ArgLoc::Indirect;
    loc.topLimbBits = limbBits();
    unsigned reg = target == Target::AArch64 ? AArch64IndirectResultReg : 0;
    loc.limbs.push_back(LimbLoc{true, reg, 0});
    return loc;
  }

  loc.kind = ArgLoc::Direct;
  loc.topLimbBits = loc.bitWidth - (n - 1) * limbBits();
  for (unsigned i = 0; i != n; ++i)
    loc.limbs.push_back(LimbLoc{true, i, 0});
  return loc;
}

ArgLoc CCAssigner::assignArg(Type *ty) {
  switch (target) {
  case Target::AArch64:
    return assignArgAArch64(ty);
  case Target::RISCV64:
    return assignArgRISCV(ty);
  }
  llvm_unreachable("bad target");
}

} // end namespace lifter
