#pragma once

#include <ostream>
#include <string>

#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/TargetParser/Triple.h"

namespace llvm {
class Constant;
class Function;
class VectorType;
} // namespace llvm

namespace lifter {

struct TargetCtx {
  llvm::Triple TT;
  const char *DL;
  const char *CPU;
  const char *Features;
  const llvm::Target *Targ;
};

// Init target backend (aarch64/riscv64) from `TT`. `DL` may be nullptr to use
// the per-arch default datalayout. Calls exit(-1) on unsupported arch.
TargetCtx initTargetFromTriple(const llvm::Triple &TT, const char *DL,
                               std::ostream *out);

/*
 * add debug into to an IR file that will help the lifter figure out
 * which LLVM instruction each asm instruction came from
 */
void addDebugInfo(llvm::Function *srcFn,
                  std::unordered_map<unsigned, llvm::Instruction *> &lineMap);

/*
 * lower LLVM IR to textual assembly
 */
std::unique_ptr<llvm::MemoryBuffer>
generateAsm(llvm::Module &, const llvm::Target *Targ, llvm::Triple DefaultTT,
            const char *DefaultCPU, const char *DefaultFeatures);

/*
 * lift textual assembly to LLVM IR. the target arguments should be
 * the same as those used to generate assembly, and lineMap should
 * come from addDebugInfo(). it's fine to pass an empty
 * lineMap. return value is a source/target pair that is ready for an
 * Alive2 refinement check. the source function is returned because it
 * has been rewritten, for example to implement ABI checks.
 */
std::pair<llvm::Function *, llvm::Function *>
liftFunc(llvm::Function *, std::unique_ptr<llvm::MemoryBuffer>,
         std::unordered_map<unsigned, llvm::Instruction *> &lineMap,
         std::string optimize_tgt, std::ostream *out, const llvm::Target *Targ,
         llvm::Triple DefaultTT, const char *DefaultCPU,
         const char *DefaultFeatures);

/*
 * random utility function
 */
inline std::string moduleToString(llvm::Module *M) {
  std::string sss;
  llvm::raw_string_ostream ss(sss);
  M->print(ss, nullptr);
  return sss;
}

} // namespace lifter
