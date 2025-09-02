#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCInstPrinter.h"
#include "llvm/MC/MCParser/MCTargetAsmParser.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCTargetOptions.h"
#include "llvm/MC/MCTargetOptionsCommandFlags.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"

#include "backend_tv/arm2llvm.h"
#include "backend_tv/lifter.h"
#include "backend_tv/mc2llvm.h"
#include "backend_tv/riscv2llvm.h"
#include "backend_tv/streamerwrapper.h"
#include "llvm_util/llvm_optimizer.h"

using namespace std;
using namespace llvm;
using namespace lifter;

// do not delete this line, it's not as dead as it looks
mc::RegisterMCTargetOptionsFlags MOF;

namespace lifter {

void addDebugInfo(Function *srcFn,
                  unordered_map<unsigned, Instruction *> &lineMap) {
  auto &M = *srcFn->getParent();

  // start with a clean slate
  StripDebugInfo(M);

  M.setModuleFlag(Module::Warning, "Dwarf Version", dwarf::DWARF_VERSION);
  M.setModuleFlag(Module::Warning, "Debug Info Version",
                  DEBUG_METADATA_VERSION);

  auto &Ctx = srcFn->getContext();

  DIBuilder DBuilder(M);
  auto DIF = DBuilder.createFile("foo.ll", ".");
  auto CU = DBuilder.createCompileUnit(dwarf::DW_LANG_C, DIF, "backend-tv",
                                       false, "", 0);
  auto Ty = DBuilder.createSubroutineType(DBuilder.getOrCreateTypeArray({}));
  auto SP = DBuilder.createFunction(CU, srcFn->getName(), StringRef(), DIF, 0,
                                    Ty, 0, DINode::FlagPrototyped,
                                    DISubprogram::SPFlagDefinition);
  srcFn->setSubprogram(SP);
  unsigned line = 0;
  for (auto &bb : *srcFn) {
    for (auto &i : bb) {
      lineMap[line] = &i;
      i.setDebugLoc(DILocation::get(Ctx, line, 0, SP));
      ++line;
    }
  }

#if 0
    M.dump();
    *out << "\n\n\n";
#endif

  DBuilder.finalize();
  verifyModule(M);
}

pair<Function *, Function *>
liftFunc(Function *srcFn, unique_ptr<MemoryBuffer> MB,
         std::unordered_map<unsigned, llvm::Instruction *> &lineMap,
         std::string optimize_tgt, std::ostream *out, const Target *Targ,
         llvm::Triple DefaultTT, const char *DefaultCPU,
         const char *DefaultFeatures) {
  string backend{Targ->getName()};
  unique_ptr<mc2llvm> lifter;
  if (backend == "aarch64") {
    lifter = make_unique<arm2llvm>(srcFn, std::move(MB), lineMap, out, Targ,
                                   DefaultTT, DefaultCPU, DefaultFeatures);
  } else if (backend == "riscv64") {
    lifter = make_unique<riscv2llvm>(srcFn, std::move(MB), lineMap, out, Targ,
                                     DefaultTT, DefaultCPU, DefaultFeatures);
  } else {
    *out << "ERROR: Nonexistent backend\n";
    exit(-1);
  }

  auto [adjustedSrc, tgtFn] = lifter->run();

  auto tgtModule = tgtFn->getParent();

  *out << "\n\nabout to optimize lifted code:\n\n";
  *out << moduleToString(tgtModule) << std::endl;

  auto err = llvm_util::optimize_module(*tgtModule, optimize_tgt);
  if (!err.empty()) {
    *out << "\n\nERROR running LLVM optimizations:\n";
    *out << err << "\n";
    exit(-1);
  }

  lifter->fixupOptimizedTgt(tgtFn);

  *out << "\n\noptimized lifted code:\n\n";
  *out << moduleToString(tgtModule) << std::endl;

  return make_pair(adjustedSrc, tgtFn);
}

} // namespace lifter
