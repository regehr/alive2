#include "llvm/IR/BasicBlock.h"
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

using namespace std;
using namespace llvm;
using namespace lifter;

// do not delete this line
mc::RegisterMCTargetOptionsFlags MOF;

namespace lifter {

// FIXME get rid of these globals
std::ostream *out;
unsigned origRetWidth;
bool has_ret_attr;
const Target *Targ;
Function *myAlloc;
Constant *stackSize;
std::string DefaultBackend;
llvm::Triple DefaultTT;
const char *DefaultDL;
const char *DefaultCPU;
const char *DefaultFeatures;

void init(std::string &backend) {
  DefaultBackend = backend;
  auto TripleStr = DefaultTT.getTriple();
  assert(TripleStr == Triple::normalize(TripleStr));
  /*
   * FIXME we probably want to ask the client to run these
   * initializers
   */
  if (DefaultBackend == "aarch64") {
    LLVMInitializeAArch64TargetInfo();
    LLVMInitializeAArch64Target();
    LLVMInitializeAArch64TargetMC();
    LLVMInitializeAArch64AsmParser();
    LLVMInitializeAArch64AsmPrinter();
  } else if (DefaultBackend == "riscv64") {
    LLVMInitializeRISCVTargetInfo();
    LLVMInitializeRISCVTarget();
    LLVMInitializeRISCVTargetMC();
    LLVMInitializeRISCVAsmParser();
    LLVMInitializeRISCVAsmPrinter();
  } else {
    assert(false);
  }
  string Error;
  Targ = TargetRegistry::lookupTarget(DefaultTT, Error);
  if (!Targ) {
    *out << Error;
    exit(-1);
  }
  origRetWidth = 64;
  has_ret_attr = false;
}

pair<Function *, Function *> liftFunc(Function *srcFn,
                                      unique_ptr<MemoryBuffer> MB) {
  unique_ptr<mc2llvm> lifter;
  if (DefaultBackend == "aarch64") {
    lifter = make_unique<arm2llvm>(srcFn, std::move(MB));
  } else if (DefaultBackend == "riscv64") {
    lifter = make_unique<riscv2llvm>(srcFn, std::move(MB));
  } else {
    *out << "ERROR: Nonexistent backend\n";
    exit(-1);
  }

  return lifter->run();
}

std::string moduleToString(llvm::Module *M) {
  std::string sss;
  llvm::raw_string_ostream ss(sss);
  M->print(ss, nullptr);
  return sss;
}

std::string funcToString(llvm::Function *F) {
  std::string sss;
  llvm::raw_string_ostream ss(sss);
  F->print(ss, nullptr);
  return sss;
}

// Some of these are from https://github.com/agustingianni/retools

uint64_t replicate8to64(uint64_t v) {
  uint64_t ret = 0;
  for (int i = 0; i < 8; ++i) {
    bool b = (v & 128) != 0;
    ret <<= 8;
    if (b)
      ret |= 0xff;
    v <<= 1;
  }
  return ret;
}

uint64_t Replicate(uint64_t bit, int N) {
  if (!bit)
    return 0;
  if (N == 64)
    return 0xffffffffffffffffLL;
  return (1ULL << N) - 1;
}

uint64_t Replicate32x2(uint64_t bits32) {
  return (bits32 << 32) | bits32;
}

uint64_t Replicate16x4(uint64_t bits16) {
  return Replicate32x2((bits16 << 16) | bits16);
}

uint64_t Replicate8x8(uint64_t bits8) {
  return Replicate16x4((bits8 << 8) | bits8);
}

} // namespace lifter
