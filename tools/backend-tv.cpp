// Copyright (c) 2018-present The Alive2 Authors.
// Distributed under the MIT license that can be found in the LICENSE file.

#include "backend_tv/lifter.h"
#include "cache/cache.h"
#include "llvm_util/compare.h"
#include "llvm_util/llvm2alive.h"
#include "llvm_util/utils.h"
#include "smt/smt.h"
#include "tools/transform.h"
#include "util/version.h"

#include "Target/AArch64/AArch64Subtarget.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_os_ostream.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/TargetParser/Triple.h"
#include "llvm/Transforms/IPO/GlobalDCE.h"
#include "llvm/Transforms/IPO/Internalize.h"
#include "llvm/Transforms/Utils/Cloning.h"

#include <fstream>
#include <iostream>
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

llvm::cl::opt<string>
    opt_backend(LLVM_ARGS_PREFIX "backend",
                llvm::cl::desc("Backend to validate (default=aarch64)"),
                llvm::cl::cat(alive_cmdargs), llvm::cl::init("aarch64"));

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

llvm::cl::opt<string> opt_asm_output("asm-output",
                                     llvm::cl::desc("Save assembly to file "
                                                    "(default=no asm output)"),
                                     llvm::cl::cat(alive_cmdargs));

llvm::cl::opt<bool> save_lifted_ir(
    "save-lifted-ir",
    llvm::cl::desc("Save lifted LLVM IR to file (default=false)"),
    llvm::cl::init(false), llvm::cl::cat(alive_cmdargs));

llvm::cl::opt<bool> run_replace_ptrtoint(
    "run-replace-ptrtoint",
    llvm::cl::desc(
        "Replace ptr-int round trips with single GEP (default=false)"),
    llvm::cl::init(true), llvm::cl::cat(alive_cmdargs));

llvm::cl::opt<bool> test_replace_ptrtoint(
    "test-replace-ptrtoint",
    llvm::cl::desc("Test run-replace-ptrtoint flag by running the pass on "
                   "source (default=false)"),
    llvm::cl::init(false), llvm::cl::cat(alive_cmdargs));

llvm::cl::opt<string> opt_asm_input(
    "asm-input",
    llvm::cl::desc("Use the provied file as lifted assembly, instead of "
                   "lifting the LLVM IR. This is only for testing. "
                   "(default=no asm input)"),
    llvm::cl::cat(alive_cmdargs));

llvm::ExitOnError ExitOnErr;
llvm::Triple DefaultTT;
const char *DefaultDL;
const char *DefaultCPU;
const char *DefaultFeatures;

// Given an intToPtr instruction, checks for a round trip
// from a ptrToInt instruction, then replaces with a single GEP instruction.
bool tryReplaceRoundTrip(llvm::IntToPtrInst *intToPtr) {
  assert(intToPtr);

  // we only understand add instructions in this context
  auto *op_inst = dyn_cast<llvm::Instruction>(intToPtr->getOperand(0));
  if (!op_inst || op_inst->getOpcode() != llvm::Instruction::Add ||
      op_inst->getNumUses() > 1)
    return false;

  // keep track of (ptr + int) vs (int + ptr)
  bool ptrOnLeft = true;
  auto *ptrToInt = dyn_cast<llvm::PtrToIntInst>(op_inst->getOperand(0));

  if (!ptrToInt) {
    ptrOnLeft = false;
    ptrToInt = dyn_cast<llvm::PtrToIntInst>(op_inst->getOperand(1));
  }

  if (!ptrToInt || ptrToInt->getNumUses() > 1)
    return false;

  llvm::IRBuilder<> B(intToPtr);

  llvm::Value *gep = B.CreateGEP(B.getInt8Ty(), ptrToInt->getOperand(0),
                                 {op_inst->getOperand(ptrOnLeft ? 1 : 0)}, "");

  intToPtr->replaceAllUsesWith(gep);
  intToPtr->eraseFromParent();
  op_inst->eraseFromParent();
  ptrToInt->eraseFromParent();

  return true;
}

// find and collapse sequences of the form ptrToInt, add, intToPtr
// into a single GEP instruction. Possible performance benefits.
void tryReplacePtrtoInt(llvm::Function *fn) {
  // apparently iterating in this way is stable
  for (auto it = instructions(*fn).begin(), end = instructions(*fn).end();
       it != end;) {
    llvm::Instruction &Inst = *it++;
    if (auto *intToPtr = dyn_cast<llvm::IntToPtrInst>(&Inst)) {
      tryReplaceRoundTrip(intToPtr);
    }
  }
}

void doit(llvm::Module *srcModule, llvm::Function *srcFn, Verifier &verifier,
          llvm::TargetLibraryInfoWrapperPass &TLI) {

  // sigh... do this check earlier to stop an assertion in LLVM from
  // firing
  if (srcFn->isVarArg()) {
    *out << "\nERROR: varargs not supported\n\n";
    exit(-1);
  }

  {
    // let's not even bother if Alive2 can't process our function

    auto Pred = [](unsigned MDKind, llvm::MDNode *Node) {
      return MDKind == llvm::LLVMContext::MD_alias_scope ||
             MDKind == llvm::LLVMContext::MD_noalias ||
             MDKind == llvm::LLVMContext::MD_tbaa;
    };
    for (auto &bb : *srcFn)
      for (auto &i : bb)
        i.eraseMetadataIf(Pred);

    auto fn = llvm2alive(*srcFn, TLI.getTLI(*srcFn), /*isSrc=*/true);
    if (!fn) {
      *out << "Fatal error, exiting\n";
      exit(-1);
    }
  }

  // nuke the rest of the functions in the module -- no need to
  // generate and then parse assembly that we don't care about
  for (auto &F : *srcModule) {
    if (&F != srcFn && !F.isDeclaration())
      F.deleteBody();
  }

  if (test_replace_ptrtoint) {
    llvm::raw_os_ostream streamWrapper(*out);
    tryReplacePtrtoInt(srcFn);
    srcFn->print(streamWrapper);
    streamWrapper.flush();
    exit(0);
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
    MPM.run(*srcModule, MAM);
  }

  string Error;
  const auto *Targ = llvm::TargetRegistry::lookupTarget(DefaultTT, Error);
  if (!Targ) {
    *out << "Can't lookup target\n";
    *out << Error;
    exit(-1);
  }

  unique_ptr<llvm::MemoryBuffer> AsmBuffer;
  std::unordered_map<unsigned, llvm::Instruction *> lineMap;
  if (opt_asm_input == "") {
    lifter::addDebugInfo(srcFn, lineMap);
    AsmBuffer = lifter::generateAsm(*srcModule, Targ, DefaultTT, DefaultCPU,
                                    DefaultFeatures);
  } else {
    AsmBuffer = ExitOnErr(
        llvm::errorOrToExpected(llvm::MemoryBuffer::getFile(opt_asm_input)));
  }

  if (opt_asm_output != "") {
    std::error_code EC;
    llvm::raw_fd_ostream asm_file(opt_asm_output, EC);
    if (EC)
      llvm::report_fatal_error("Couldn't open output file, exiting");
    for (auto it = AsmBuffer->getBuffer().begin();
         it != AsmBuffer->getBuffer().end(); ++it) {
      asm_file << *it;
    }
  }

  *out << "\n\n------------ Assembly: ------------\n\n";
  for (auto it = AsmBuffer->getBuffer().begin();
       it != AsmBuffer->getBuffer().end(); ++it) {
    *out << *it;
  }
  *out << "-------------" << std::endl;

  if (opt_asm_only)
    exit(0);

  auto [F1, F2] =
      lifter::liftFunc(srcFn, std::move(AsmBuffer), lineMap, opt_optimize_tgt,
                       out, Targ, DefaultTT, DefaultCPU, DefaultFeatures);

  auto lifted = lifter::moduleToString(F2->getParent());
  if (save_lifted_ir) {
    std::filesystem::path p{(string)opt_file};
    p.replace_extension(".lifted.ll");
    ofstream of(p);
    of << lifted;
    of.close();
  }

  if (run_replace_ptrtoint) {
    tryReplacePtrtoInt(F2);
  }

  if (!opt_skip_verification)
    verifier.compareFunctions(*F1, *F2);

  *out << "done comparing functions\n";
  out->flush();
}

} // anonymous namespace

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
      R"EOF(Alive2-based backend translation validator:
version )EOF";
  Usage += alive_version;

  llvm::cl::HideUnrelatedOptions(alive_cmdargs);
  llvm::cl::ParseCommandLineOptions(argc, argv, Usage);

  auto srcModule = openInputFile(Context, opt_file);
  if (!srcModule.get()) {
    cerr << "Could not read bitcode from '" << opt_file << "'\n";
    return -1;
  }

#define ARGS_MODULE_VAR srcModule
#include "llvm_util/cmd_args_def.h"

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
  if (opt_backend == "aarch64") {
    DefaultTT = llvm::Triple("aarch64-unknown-linux-gnu");
    DefaultDL = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32";
    DefaultCPU = "generic";
    DefaultFeatures = "";
    LLVMInitializeAArch64TargetInfo();
    LLVMInitializeAArch64Target();
    LLVMInitializeAArch64TargetMC();
    LLVMInitializeAArch64AsmParser();
    LLVMInitializeAArch64AsmPrinter();
  } else if (opt_backend == "riscv64") {
    DefaultTT = llvm::Triple("riscv64-unknown-linux-gnu");
    DefaultDL = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128";
    DefaultCPU = "generic";
    // +m includes zmmul
    // +b = +zba,+zbb,+zbs
    DefaultFeatures = "+c,+m,+b,+f,+d,+q,+zfh";
    LLVMInitializeRISCVTargetInfo();
    LLVMInitializeRISCVTarget();
    LLVMInitializeRISCVTargetMC();
    LLVMInitializeRISCVAsmParser();
    LLVMInitializeRISCVAsmPrinter();
  } else {
    *out << "ERROR: Only aarch64 or riscv64 are supported\n";
    exit(-1);
  }

  srcModule.get()->setTargetTriple(DefaultTT);
  srcModule.get()->setDataLayout(DefaultDL);

  auto &DL = srcModule.get()->getDataLayout();
  llvm::Triple targetTriple(srcModule.get()->getTargetTriple());
  llvm::TargetLibraryInfoWrapperPass TLI(targetTriple);

  llvm_util::initializer llvm_util_init(*out, DL);
  smt::smt_initializer smt_init;
  Verifier verifier(TLI, smt_init, *out);
  verifier.always_verify = opt_always_verify;
  verifier.print_dot = opt_print_dot;
  verifier.bidirectional = opt_bidirectional;

  if (opt_fn != "") {
    auto *srcFn = findFunction(*srcModule, opt_fn);
    if (srcFn == nullptr) {
      *out << "ERROR: Couldn't find function to verify\n";
      exit(-1);
    }
    doit(srcModule.get(), srcFn, verifier, TLI);
  } else {
    for (auto &srcFn : *srcModule.get()) {
      if (srcFn.isDeclaration())
        continue;
      doit(srcModule.get(), &srcFn, verifier, TLI);
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
