#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/MC/TargetRegistry.h"

namespace llvm {
class Constant;
class Function;
class VectorType;
} // namespace llvm

namespace lifter {

// amount of stack available for use by the lifted function, in bytes
const int stackBytes = 1024;

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
extern llvm::Function *myAlloc;
extern llvm::Constant *stackSize;

// TODO -- expose these to the command line
inline const llvm::Triple DefaultTT("aarch64-unknown-linux-gnu");
inline const char *CPU = "generic";

extern std::unordered_map<unsigned, llvm::Instruction *> lineMap;

void init();

void nameGlobals(llvm::Module *);
llvm::Function *adjustSrc(llvm::Function *);
void addDebugInfo(llvm::Function *);
void checkSupport(llvm::Function *);
void fixupOptimizedTgt(llvm::Function *);
bool tryReplaceRoundTrip(llvm::IntToPtrInst *);

std::unique_ptr<llvm::MemoryBuffer> generateAsm(llvm::Module &);

std::pair<llvm::Function *, llvm::Function *>
liftFunc(llvm::Module *, llvm::Module *, llvm::Function *,
         std::unique_ptr<llvm::MemoryBuffer>);

} // namespace lifter
