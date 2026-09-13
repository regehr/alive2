#pragma once

#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"

#include <cstdint>
#include <string>

namespace lifter {

/*
 * target-independent description of where a backend places a function
 * argument or return value.
 *
 * both supported targets legalize an integer wider than one register into
 * ceil(N / XLEN) register-sized limbs, least significant limb first, and
 * then place those limbs one at a time. a single value can therefore
 * occupy some registers and some stack -- AArch64 splits an i1024 across
 * x0-x7 and 64 bytes of stack, and RV64 splits an i128 between a7 and
 * 0(sp) -- so a location is a list of per-limb locations, not one place.
 *
 * the non-significant high bits of the most significant limb are left
 * unspecified by both ABIs. topLimbBits records how many bits of that
 * limb actually belong to the value; a lifter must supply unknown bits
 * above them when it writes a value into the register file, and must
 * ignore them when it reads one back out.
 */
struct LimbLoc {
  // true: the limb is in argument register number `reg`. false: it is in
  // memory, `stackOffset` bytes from the base of the incoming argument area.
  bool inReg;
  unsigned reg;
  uint64_t stackOffset;
};

struct ArgLoc {
  enum Kind {
    // a void return value: `limbs` is empty
    None,
    // the value itself occupies `limbs`
    Direct,
    // `limbs` holds exactly one register-sized value: the address of a
    // copy of the value that the caller allocated
    Indirect,
    // the value occupies one vector / floating-point register, or one
    // stack slot; `limbs` has exactly one entry, and when it is in a
    // register `reg` indexes the vector registers rather than the GPRs
    Vector,
  };

  Kind kind{None};
  llvm::SmallVector<LimbLoc, 4> limbs;
  // width of the original LLVM type
  unsigned bitWidth{0};
  // bits per limb, i.e. the target's register width
  unsigned limbBits{0};
  // Direct only: significant bits in the most significant limb, in
  // 1..limbBits. bits above this within that limb are unspecified.
  unsigned topLimbBits{0};
};

/*
 * renders a location the way backend_tv/scripts/abi-difftest.py expects
 * to read it: a kind, then one token per limb -- rN for argument register
 * N, vN for vector register N, sN for byte offset N into the incoming
 * argument area, and X8 for the AArch64 indirect result register.
 */
std::string toString(const ArgLoc &loc);

/*
 * assigns locations to a function's arguments in order, mirroring what
 * the LLVM backend's CCAssignFn does for the default C calling
 * convention. construct one per call site or function entry, ask for the
 * return location, then call assignArg() once per argument, in order.
 */
class CCAssigner {
public:
  enum class Target { AArch64, RISCV64 };

  CCAssigner(Target target, const llvm::DataLayout &DL, llvm::Type *retTy);

  const ArgLoc &retLoc() const {
    return ret;
  }

  ArgLoc assignArg(llvm::Type *ty);

  // total size of the incoming argument area, in bytes
  uint64_t stackSize() const {
    return stackOff;
  }

  // AArch64 returns a value too large for x0-x7 through a buffer whose
  // address the caller passes in x8. x8 is not an argument register, so
  // this number is deliberately outside the argument register range.
  static constexpr unsigned AArch64IndirectResultReg = 8;

  // both targets pass the first 8 integer and the first 8 vector / FP
  // arguments in registers
  static constexpr unsigned numArgGPRs = 8;
  static constexpr unsigned numArgVecRegs = 8;

private:
  Target target;
  const llvm::DataLayout &DL;
  unsigned nextGPR{0};
  unsigned nextVec{0};
  uint64_t stackOff{0};
  ArgLoc ret;

  unsigned limbBits() const {
    // both supported targets are 64-bit
    return 64;
  }

  unsigned numLimbs(unsigned bits) const {
    return (bits + limbBits() - 1) / limbBits();
  }

  LimbLoc takeStack(uint64_t size, uint64_t align);
  LimbLoc takeGPROrStack();

  ArgLoc assignArgAArch64(llvm::Type *ty);
  ArgLoc assignArgRISCV(llvm::Type *ty);
  ArgLoc assignRet(llvm::Type *ty);
};

} // end namespace lifter
