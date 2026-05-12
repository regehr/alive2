#pragma once

#include "backend_tv/lifter.h"
#include "llvm/IR/Function.h"
#include <ostream>

namespace lifter {

struct InlineAsmLiftOptions {
  TargetCtx tctx;
  std::ostream *out;
};

bool tryLiftInlineAsm(llvm::Function &F, std::ostream *out);

// Lift every `call asm` in F to pure IR via the backend-tv pipeline
// (generateAsm + mc2llvm lift) and inline the result.
// Returns the number of sites lifted, or -1 on error.
// Known semantic gaps in the round-trip:
//   - `~{memory}` clobbers and other compiler-fence-style side effects
//     are not represented in the lifted IR; surrounding code may
//     legally be reordered across the lifted region in ways the
//     original asm forbade
int liftInlineAsmCalls(llvm::Function &F, const InlineAsmLiftOptions &opts);

} // namespace lifter
