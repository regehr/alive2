// Copyright (c) 2018-present The Alive2 Authors.
// Distributed under the MIT license that can be found in the LICENSE file.

#include "backend_tv/lifter.h"
#include "cache/cache.h"
#include "llvm_util/compare.h"
#include "llvm_util/llvm2alive.h"
#include "llvm_util/llvm_optimizer.h"
#include "llvm_util/utils.h"
#include "smt/smt.h"
#include "tools/transform.h"
#include "util/version.h"

#include "llvm/ADT/StringExtras.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/InitializePasses.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/TargetParser/Triple.h"
#include "llvm/Transforms/IPO/GlobalDCE.h"
#include "llvm/Transforms/IPO/Internalize.h"
#include "llvm/Transforms/Utils/Cloning.h"

#include <fstream>
#include <iostream>
#include <sstream>
#include <utility>

using namespace tools;
using namespace util;
using namespace std;
using namespace llvm_util;

#define LLVM_ARGS_PREFIX ""
#define ARGS_SRC_TGT
#define ARGS_REFINEMENT
#include "llvm_util/cmd_args_list.h"

namespace {

llvm::cl::opt<string> opt_file(llvm::cl::Positional,
                               llvm::cl::desc("bitcode_file"),
                               llvm::cl::Required,
                               llvm::cl::value_desc("filename"),
                               llvm::cl::cat(alive_cmdargs));

llvm::cl::opt<std::string>
    opt_fn(LLVM_ARGS_PREFIX "fn",
           llvm::cl::desc("Name of function to verify, without @ (default "
                          "= first function in the module)"),
           llvm::cl::cat(alive_cmdargs));

llvm::cl::opt<string> opt_optimize_tgt(
    LLVM_ARGS_PREFIX "optimize-tgt",
    llvm::cl::desc("Optimize lifted code before performing translation "
                   "validation (default=O3)"),
    llvm::cl::cat(alive_cmdargs), llvm::cl::init("O3"));

llvm::cl::opt<bool> opt_skip_verification(
    LLVM_ARGS_PREFIX "skip-verification",
    llvm::cl::desc(
        "Perform lifting but skip the refinement check (default=false)"),
    llvm::cl::cat(alive_cmdargs), llvm::cl::init(false));

llvm::cl::opt<bool> opt_use_debuginfo(
    LLVM_ARGS_PREFIX "use-debug-info",
    llvm::cl::desc(
        "Try to improve results by using LLVM debuginfo to provide a mapping "
        "between LLVM and ARM instructions (default=true)"),
    llvm::cl::cat(alive_cmdargs), llvm::cl::init(true));

// FIXME -- this needs to be turned off by default
llvm::cl::opt<bool> opt_internalize(
    LLVM_ARGS_PREFIX "internalize",
    llvm::cl::desc(
        "Internalize the module before performing TV, to allow removing more "
        "clutter. WARNING: this sometimes changes the generated code, which "
        "can invalidate TV results, generally do not use (default=false"),
    llvm::cl::cat(alive_cmdargs), llvm::cl::init(false));

// FIXME support opt_asm_only and opt_asm_input

llvm::cl::opt<bool> opt_asm_only(
    "asm-only",
    llvm::cl::desc("Only generate assembly and exit (default=false)"),
    llvm::cl::init(false), llvm::cl::cat(alive_cmdargs));

llvm::cl::opt<bool> save_lifted_ir(
    "save-lifted-ir",
    llvm::cl::desc("Save lifted LLVM IR to file (default=false)"),
    llvm::cl::init(false), llvm::cl::cat(alive_cmdargs));

llvm::cl::opt<string> opt_asm_input(
    "asm-input",
    llvm::cl::desc("Use the provied file as lifted assembly, instead of "
                   "lifting the LLVM IR. This is only for testing. "
                   "(default=no asm input)"),
    llvm::cl::cat(alive_cmdargs));

llvm::ExitOnError ExitOnErr;

void doit(llvm::Module *M1, llvm::Function *srcFn, Verifier &verifier,
          llvm::TargetLibraryInfoWrapperPass &TLI) {
  assert(lifter::out);

  // sigh... do this check earlier to stop an assertion in LLVM from
  // firing
  if (srcFn->isVarArg()) {
    *out << "\nERROR: varargs not supported\n\n";
    exit(-1);
  }

  {
    // let's not even bother if Alive2 can't process our function
    auto fn = llvm2alive(*srcFn, TLI.getTLI(*srcFn), /*isSrc=*/true);
    if (!fn) {
      *out << "Fatal error, exiting\n";
      exit(-1);
    }
  }

  // nuke the rest of the functions in the module -- no need to
  // generate and then parse assembly that we don't care about
  for (auto &F : *M1) {
    if (&F != srcFn && !F.isDeclaration())
      F.deleteBody();
  }

  if (opt_internalize) {
    // nuke everything not reachable from the target function; this is
    // useful for removing clutter but should never be used when you
    // want to trust the results, since changing linkage can change
    // codegen

    srcFn->setLinkage(llvm::GlobalValue::LinkageTypes::ExternalLinkage);

    llvm::LoopAnalysisManager LAM;
    llvm::FunctionAnalysisManager FAM;
    llvm::CGSCCAnalysisManager CGAM;
    llvm::ModuleAnalysisManager MAM;
    llvm::PassBuilder PB;
    llvm::ModulePassManager MPM;

    PB.registerModuleAnalyses(MAM);
    PB.registerCGSCCAnalyses(CGAM);
    PB.registerFunctionAnalyses(FAM);
    PB.registerLoopAnalyses(LAM);
    PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

    auto preserve = [srcFn](const llvm::GlobalValue &GV) {
      return &GV == srcFn;
    };

    MPM.addPass(llvm::InternalizePass(preserve));
    MPM.addPass(llvm::GlobalDCEPass());
    MPM.run(*M1, MAM);
  }

  lifter::init();
  lifter::checkSupport(srcFn);
  lifter::nameGlobals(M1);

  if (opt_use_debuginfo)
    lifter::addDebugInfo(srcFn);

  auto AsmBuffer = (opt_asm_input != "")
                       ? ExitOnErr(llvm::errorOrToExpected(
                             llvm::MemoryBuffer::getFile(opt_asm_input)))
                       : lifter::generateAsm(*M1);

  *out << "\n\n------------ AArch64 Assembly: ------------\n\n";
  for (auto it = AsmBuffer->getBuffer().begin();
       it != AsmBuffer->getBuffer().end(); ++it) {
    *out << *it;
  }
  *out << "-------------" << std::endl;

  if (opt_asm_only)
    exit(0);

  srcFn = lifter::adjustSrc(srcFn);

  std::unique_ptr<llvm::Module> M2 =
      std::make_unique<llvm::Module>("M2", M1->getContext());
  // M2->setDataLayout(M1->getDataLayout());
  // M2->setTargetTriple(M1->getTargetTriple());

  auto [F1, F2] = lifter::liftFunc(M1, M2.get(), srcFn, std::move(AsmBuffer));

  *out << "\n\nabout to optimize lifted code:\n\n";
  *out << lifter::moduleToString(M2.get()) << std::endl;

  auto err = optimize_module(M2.get(), opt_optimize_tgt);
  if (!err.empty()) {
    *out << "\n\nERROR running LLVM optimizations\n\n";
    exit(-1);
  }

  // these attributes can be soundly removed, and a good thing too
  // since they cause spurious TV failures in ASM memory mode
  for (auto arg = F2->arg_begin(); arg != F2->arg_end(); ++arg) {
    arg->removeAttr(llvm::Attribute::NoCapture);
    arg->removeAttr(llvm::Attribute::ReadNone);
    arg->removeAttr(llvm::Attribute::ReadOnly);
    arg->removeAttr(llvm::Attribute::WriteOnly);
  }

  auto lifted = lifter::moduleToString(M2.get());
  if (save_lifted_ir) {
    std::filesystem::path p{(string)opt_file};
    p.replace_extension(".lifted.ll");
    ofstream of(p);
    of << lifted;
    of.close();
  }

  *out << "\n\nafter optimization:\n\n";
  *out << lifted;
  *out << "\n";
  out->flush();

  if (!opt_skip_verification)
    verifier.compareFunctions(*F1, *F2);

  *out << "done comparing functions\n";
  out->flush();
}

} // namespace

unique_ptr<Cache> cache;

int main(int argc, char **argv) {

  if (true) {
    // FIXME remove when done debugging
    for (int i = 0; i < argc; ++i)
      cout << "'" << argv[i] << "' ";
    cout << endl;
  }

  llvm::InitLLVM X(argc, argv);
  llvm::EnableDebugBuffering = true;
  llvm::LLVMContext Context;

  std::string Usage =
      R"EOF(Alive2 stand-alone translation validator for the AArch64 backend:
version )EOF";
  Usage += alive_version;

  llvm::cl::HideUnrelatedOptions(alive_cmdargs);
  llvm::cl::ParseCommandLineOptions(argc, argv, Usage);

  auto M1 = openInputFile(Context, opt_file);
  if (!M1.get()) {
    cerr << "Could not read bitcode from '" << opt_file << "'\n";
    return -1;
  }

#define ARGS_MODULE_VAR M1
#include "llvm_util/cmd_args_def.h"

  cerr << "report file: " << report_filename << '\n';

  // if src is always UB we end up with weird effects such as targets
  // that never reach a return instruction. let's just weed these out
  // here.
  config::fail_if_src_is_ub = true;

  // turn on Alive2's asm-level memory model for the target; this
  // helps Alive2 deal more gracefully with the fact that integers and
  // pointers are freely mixed at the asm level, unlike in LLVM IR in
  // general
  config::tgt_is_asm = true;

  // undef is going away, we don't want to see bugs about it
  config::disable_undef_input = true;

  // for now let's just avoid bugs coming from poison inputs;
  // separately, this flag means that memory passed to us will be
  // frozen
  config::disable_poison_input = true;

  // FIXME: we should avoid hard-coding these
  M1.get()->setTargetTriple("aarch64-linux-gnu");
  M1.get()->setDataLayout(
      "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32");

  lifter::out = out;

  auto &DL = M1.get()->getDataLayout();
  llvm::Triple targetTriple(M1.get()->getTargetTriple());
  llvm::TargetLibraryInfoWrapperPass TLI(targetTriple);

  llvm_util::initializer llvm_util_init(*out, DL);
  smt::smt_initializer smt_init;
  Verifier verifier(TLI, smt_init, *out);
  verifier.quiet = opt_quiet;
  verifier.always_verify = opt_always_verify;
  verifier.print_dot = opt_print_dot;
  verifier.bidirectional = opt_bidirectional;

  if (opt_fn != "") {
    auto *srcFn = findFunction(*M1, opt_fn);
    if (srcFn == nullptr) {
      *out << "ERROR: Couldn't find function to verify\n";
      exit(-1);
    }
    doit(M1.get(), srcFn, verifier, TLI);
  } else {
    for (auto &srcFn : *M1.get()) {
      if (srcFn.isDeclaration())
        continue;
      doit(M1.get(), &srcFn, verifier, TLI);
      break;
    }
  }

  *out << "Summary:\n"
          "  "
       << verifier.num_correct
       << " correct transformations\n"
          "  "
       << verifier.num_unsound
       << " incorrect transformations\n"
          "  "
       << verifier.num_failed
       << " failed-to-prove transformations\n"
          "  "
       << verifier.num_errors << " Alive2 errors\n";

  if (opt_smt_stats)
    smt::solver_print_stats(*out);

  smt_init.reset();

  if (opt_alias_stats)
    IR::Memory::printAliasStats(*out);

  return verifier.num_errors > 0;
}
