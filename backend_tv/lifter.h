#include <utility>
#include <vector>

// FIXME get rid of these eventually
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/SourceMgr.h"

#include "ir/function.h"

namespace llvm {
class Function;
class Module;
class VectorType;
} // namespace llvm

namespace lifter {

void checkArguments(llvm::CallInst *ci, llvm::Value *val);
void checkVectorTy(llvm::VectorType *Ty);

extern std::ostream *out;

inline std::string moduleToString(llvm::Module *M) {
  std::string sss;
  llvm::raw_string_ostream ss(sss);
  M->print(ss, nullptr);
  return sss;
}

inline std::string funcToString(llvm::Function *F) {
  std::string sss;
  llvm::raw_string_ostream ss(sss);
  F->print(ss, nullptr);
  return sss;
}

// FIXME we'd rather not have these globals shared across files

// details from rewriting the src function
extern unsigned origRetWidth;
extern bool has_ret_attr;
extern const llvm::Target *Targ;

// TODO -- make expose these to the command line, probably
inline const char *TripleName = "aarch64-unknown-linux-gnu";
inline const char *CPU = "generic";

extern std::unordered_map<unsigned, llvm::Instruction *> lineMap;

void init();

llvm::Function *adjustSrcReturn(llvm::Function *srcFn);
void addDebugInfo(llvm::Function *srcFn);
void checkSupport(llvm::Function *srcFn);

std::unique_ptr<llvm::MemoryBuffer> generateAsm(llvm::Module &OrigModule);

std::pair<llvm::Function *, llvm::Function *>
liftFunc(llvm::Module *OrigModule, llvm::Module *LiftedModule,
         llvm::Function *srcFnLLVM, std::unique_ptr<llvm::MemoryBuffer> MB);

} // namespace lifter
