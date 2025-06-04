#include "backend_tv/lifter.h"

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Pass.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/Utils/Cloning.h"

#include <cassert>
#include <iostream>
#include <memory>

using namespace std;
using namespace llvm;
using namespace lifter;

SmallString<1024> Asm;

void appendTargetFeatures(std::unique_ptr<llvm::Module> &MClone) {
  for (llvm::Function &F : *MClone) {
    if (F.hasFnAttribute("target-features")) {
      std::string current_features =
          F.getFnAttribute("target-features").getValueAsString().str();
      F.addFnAttr("target-features", current_features + "," + DefaultFeatures);
    }
  }
}

unique_ptr<MemoryBuffer> lifter::generateAsm(Module &M) {
  assert(DefaultFeatures != NULL &&
         "[generateAsm] DefaultFeatures must be set");
  TargetOptions Opt;
  auto RM = optional<Reloc::Model>();
  unique_ptr<TargetMachine> TM(Targ->createTargetMachine(
      DefaultTT, DefaultCPU, DefaultFeatures, Opt, RM));

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

  if (DefaultFeatures[0] != '\0')
    appendTargetFeatures(MClone);

  pass.run(*MClone.get());
  return MemoryBuffer::getMemBuffer(Asm.c_str());
}
