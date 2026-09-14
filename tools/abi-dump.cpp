// Copyright (c) 2018-present The Alive2 Authors.
// Distributed under the MIT license that can be found in the LICENSE file.

/*
 * prints where backend_tv's CCAssigner thinks each argument and return
 * value of each function in an LLVM file is passed. this exists so that
 * backend_tv/scripts/abi-difftest.py can diff our model of the calling
 * convention against what the LLVM backend actually does; it is a
 * debugging aid, not part of the translation validator.
 */

#include "backend_tv/abi.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"

#include <string>

using namespace llvm;
using namespace lifter;

namespace {

cl::OptionCategory AbiDumpCat("abi-dump options");

cl::opt<std::string> InputFile(cl::Positional, cl::desc("<input .ll file>"),
                               cl::Required, cl::cat(AbiDumpCat));

cl::opt<std::string>
    TargetName("target", cl::desc("aarch64 or riscv64"),
               cl::init("aarch64"), cl::cat(AbiDumpCat));

void printLoc(raw_ostream &os, StringRef fn, StringRef what,
              const ArgLoc &loc) {
  os << fn << " " << what << " " << toString(loc) << "\n";
}

} // namespace

int main(int argc, char **argv) {
  InitLLVM X(argc, argv);
  cl::HideUnrelatedOptions(AbiDumpCat);
  cl::ParseCommandLineOptions(argc, argv);

  CCAssigner::Target target;
  if (TargetName == "aarch64") {
    target = CCAssigner::Target::AArch64;
  } else if (TargetName == "riscv64") {
    target = CCAssigner::Target::RISCV64;
  } else {
    errs() << "abi-dump: unknown target '" << TargetName << "'\n";
    return 1;
  }

  LLVMContext Ctx;
  SMDiagnostic Diag;
  auto M = parseIRFile(InputFile, Diag, Ctx);
  if (!M) {
    Diag.print(argv[0], errs());
    return 1;
  }

  for (auto &F : *M) {
    if (F.isDeclaration())
      continue;
    CCAssigner CC(target, M->getDataLayout(), F.getReturnType());
    printLoc(outs(), F.getName(), "ret", CC.retLoc());
    unsigned i = 0;
    for (auto &arg : F.args())
      printLoc(outs(), F.getName(), "arg" + std::to_string(i++),
               CC.assignArg(arg.getType()));
  }
  return 0;
}
