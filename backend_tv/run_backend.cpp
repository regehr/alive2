#include "backend_tv/lifter.h"

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Pass.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/Utils/Cloning.h"

#include <iostream>

using namespace std;
using namespace llvm;
using namespace lifter;

namespace lifter {

SmallString<1024> Asm;

unique_ptr<MemoryBuffer> generateAsm(Module &M) {
  TargetOptions Opt;
  auto RM = optional<Reloc::Model>();
  unique_ptr<TargetMachine> TM(
      Targ->createTargetMachine(TripleName, CPU, "", Opt, RM));

  Asm = "";
  raw_svector_ostream os(Asm);

  legacy::PassManager pass;
  if (TM->addPassesToEmitFile(pass, os, nullptr, CodeGenFileType::AssemblyFile,
                              false)) {
    cerr << "\nERROR: Failed to add pass to generate assembly\n\n";
    exit(-1);
  }
  /*
   * sigh... running these passes changes the module, and some of
   * these changes are non-trivial refinements
   */
  auto MClone = CloneModule(M);
  MClone->setDataLayout(TM->createDataLayout());
  pass.run(*MClone.get());
  return MemoryBuffer::getMemBuffer(Asm.c_str());
}

} // namespace lifter
