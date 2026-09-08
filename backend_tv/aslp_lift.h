#pragma once

/*
 * The seam between the AArch64 lifter and ASLP.
 *
 * arm2llvm::lift() calls tryLiftWithASLP() before falling back to its own
 * semantics. That is the entire coupling: this header names ASLP but does
 * not depend on it, so arm2llvm_insns.cpp compiles identically with and
 * without ASLP support, and every translation unit that actually talks to
 * ASLP lives behind this declaration in aslp_adapter.cpp.
 */

#include "llvm/ADT/StringRef.h"

namespace llvm {
class MCInst;
}

namespace lifter {

class arm2llvm;

/*
 * Try to lift I using ASLP's formally-derived semantics.
 *
 * Returns true if ASLP handled the instruction, in which case code has been
 * emitted and L.LLVMBB has been advanced past it. Returns false if the
 * caller must fall back to the classic lifter -- which happens routinely,
 * not just exceptionally: ASLP refuses every branch, call and return, and
 * we withhold anything carrying a relocation. In builds without ASLP this
 * is a stub that always returns false.
 */
#ifdef ALIVE_NO_ASLP
inline bool tryLiftWithASLP(arm2llvm &, llvm::MCInst &, llvm::StringRef) {
  return false;
}
#else
bool tryLiftWithASLP(arm2llvm &L, llvm::MCInst &I, llvm::StringRef instStr);
#endif

} // end namespace lifter
