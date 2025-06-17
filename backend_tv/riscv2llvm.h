#pragma once

/*
 * primary references for this lifter are the user mode ISA reference:
 *
 * https://drive.google.com/file/d/1uviu1nH-tScFfgrovvFCrj7Omv8tFtkp/view
 *
 * and the ABI document:
 *
 * https://drive.google.com/file/d/1Ja_Tpp_5Me583CGVD-BIZMlgGBnlKU4R/view
 *
 * this thread has some useful details about setting up a RISCV
 * execution environment:
 *
 * https://www.reddit.com/r/RISCV/comments/10k805c/how_can_i_buildrun_riscv_assembly_on_macos/
 */

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

#include "backend_tv/lifter.h"
#include "backend_tv/mc2llvm.h"
#include "backend_tv/riscv2llvm.h"
#include "backend_tv/streamerwrapper.h"

#include <string>

namespace lifter {

class riscv2llvm final : public mc2llvm {
  llvm::AllocaInst *get_reg(aslp::reg_t regtype, uint64_t num) override;

  void updateOutputReg(llvm::Value *V, bool SExt = false) override;

  llvm::Value *makeLoadWithOffset(llvm::Value *base, llvm::Value *offset,
                                  int size) override;

  llvm::Value *getIndexedElement(unsigned idx, unsigned eltSize,
                                 unsigned reg) override;

  void doCall(llvm::FunctionCallee FC, llvm::CallInst *llvmCI,
              const std::string &calleeName) override;

  void lift(llvm::MCInst &I) override;

  void platformInit() override;

  unsigned branchInst() override;

  llvm::Value *enforceSExtZExt(llvm::Value *V, bool isSExt, bool isZExt);

  unsigned sentinelNOP() override;

  void doReturn() override;

  unsigned mapRegToBackingReg(unsigned Reg);
  llvm::Value *readFromReg(unsigned Reg, llvm::Type *ty);
  llvm::Value *readFromRegOperand(int idx, llvm::Type *ty);
  llvm::Value *readPtrFromRegOperand(int idx);
  llvm::Value *lookupReg(unsigned Reg);
  void updateReg(llvm::Value *V, uint64_t reg);
  llvm::Value *readFromImmOperand(int idx, unsigned immed_width,
                                  unsigned result_width);
  std::tuple<llvm::BasicBlock *, llvm::BasicBlock *>
  getBranchTargetsOperand(int op);
  llvm::Value *getPointerFromMCExpr();
  llvm::Value *getPointerOperand();

public:
  riscv2llvm(llvm::Function *srcFn, std::unique_ptr<llvm::MemoryBuffer> MB,
             std::unordered_map<unsigned, llvm::Instruction *> &lineMap);
};

} // end namespace lifter
