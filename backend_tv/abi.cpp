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
  case ArgLoc::VectorGroup:
    s = "VGROUP";
    break;
  }
  for (auto &limb : loc.limbs) {
    s += " ";
    if (!limb.inReg)
      s += "s" + std::to_string(limb.stackOffset);
    else if (loc.kind == ArgLoc::Vector || loc.kind == ArgLoc::VectorGroup)
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
 * a vector goes in an RVV register group, following the psABI's "standard
 * vector calling convention variant" (riscv-cc.adoc) and allocateRVVReg()
 * in LLVM's RISCVCallingConv.cpp. each argument takes the first group of
 * LMUL registers in v8-v23 that starts at a multiple of LMUL and holds no
 * earlier argument -- the search starts at v8 every time, so a later
 * argument can land below an earlier one. a vector return value uses the
 * group starting at v8. LMUL is:
 *
 * - for a scalable vector, its known-minimum size over 64 bits
 *   (RVVBitsPerBlock), treated as 1 when that is fractional
 * - for a fixed-length vector, its size rounded up to a power of two, over
 *   128 bits, again treated as 1 when fractional. 128 is the minimum VLEN
 *   that RVA23U64's Zvl128b guarantees, which is what LLVM sizes the
 *   scalable container of a fixed-length vector by. the value is in the
 *   low bits of the group; on a machine with a longer VLEN, the rest of
 *   the group is unused
 *
 * masks (vectors of i1, one bit per element) are the exception: the first
 * mask argument goes in v0, and later ones are placed like LMUL = 1 data.
 * a mask return value goes in v0.
 *
 * not modeled, and so Unsupported: tuples, types LLVM promotes, widens, or
 * splits rather than mapping directly onto one group, and arguments
 * passed by reference once v8-v23 run out.
 */
ArgLoc CCAssigner::assignRVVGroup(VectorType *ty, bool isRet) {
  ArgLoc loc;
  loc.limbBits = limbBits();
  auto *eltTy = ty->getElementType();
  bool scalable = isa<ScalableVectorType>(ty);
  uint64_t elts = ty->getElementCount().getKnownMinValue();
  auto unsupported = [&]() {
    loc = ArgLoc{};
    loc.kind = ArgLoc::Unsupported;
    return loc;
  };

  if (eltTy->isIntegerTy(1)) {
    // the legal mask types, each of which fits in one register: nxv1i1
    // through nxv64i1, and fixed-length masks of up to 128 elements
    if (scalable ? !isPowerOf2_64(elts) || elts > 64 : elts > 128)
      return unsupported();
    loc.bitWidth = elts;
    loc.kind = ArgLoc::VectorGroup;
    if (isRet || !v0Used) {
      if (!isRet)
        v0Used = true;
      loc.limbs.push_back(LimbLoc{true, 0, 0});
      return loc;
    }
    unsigned i = 0;
    while (i < 16 && (vregsUsed & (1u << i)))
      ++i;
    if (i == 16)
      return unsupported();
    vregsUsed |= 1u << i;
    loc.limbs.push_back(LimbLoc{true, 8 + i, 0});
    return loc;
  }

  // f16 vectors are legal types under Zvfhmin, which RVA23U64 includes;
  // bfloat ones would need Zvfbfmin
  bool eltOK = eltTy->isHalfTy() || eltTy->isFloatTy() || eltTy->isDoubleTy();
  if (auto *intTy = dyn_cast<IntegerType>(eltTy)) {
    unsigned w = intTy->getBitWidth();
    eltOK = w == 8 || w == 16 || w == 32 || w == 64;
  }
  if (!eltOK)
    return unsupported();
  uint64_t bits = eltTy->getScalarSizeInBits() * elts;
  unsigned nregs;
  if (scalable) {
    if (!isPowerOf2_64(elts) || bits > 512)
      return unsupported();
    nregs = std::max<uint64_t>(1, bits / 64);
  } else {
    uint64_t rounded = PowerOf2Ceil(bits);
    if (rounded > 8 * fixedABIVLEN)
      return unsupported();
    nregs = std::max<uint64_t>(1, rounded / fixedABIVLEN);
  }
  // for a scalable vector, the known-minimum size; the real size is this
  // times vscale
  loc.bitWidth = bits;

  unsigned first = 0;
  if (!isRet) {
    uint32_t group = (1u << nregs) - 1;
    while (first + nregs <= 16 && (vregsUsed & (group << first)))
      first += nregs;
    if (first + nregs > 16)
      return unsupported();
    vregsUsed |= group << first;
  }
  loc.kind = ArgLoc::VectorGroup;
  for (unsigned i = 0; i < nregs; ++i)
    loc.limbs.push_back(LimbLoc{true, 8 + first + i, 0});
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
  if (auto *vTy = dyn_cast<VectorType>(ty))
    return assignRVVGroup(vTy, /*isRet=*/false);

  ArgLoc loc;
  loc.bitWidth = DL.getTypeSizeInBits(ty);
  loc.limbBits = limbBits();

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

  if (auto *vTy = dyn_cast<VectorType>(ty); vTy && target == Target::RISCV64)
    return assignRVVGroup(vTy, /*isRet=*/true);
  if (isa<ScalableVectorType>(ty)) {
    // AArch64 SVE is not modeled
    loc.kind = ArgLoc::Unsupported;
    return loc;
  }

  loc.bitWidth = DL.getTypeSizeInBits(ty);
  loc.limbBits = limbBits();

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
    // SVE is not modeled
    if (isa<ScalableVectorType>(ty))
      return ArgLoc{ArgLoc::Unsupported};
    return assignArgAArch64(ty);
  case Target::RISCV64:
    return assignArgRISCV(ty);
  }
  llvm_unreachable("bad target");
}

} // end namespace lifter
