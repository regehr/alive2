#pragma once

#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>


class ir_interface {

  // perform possibly non-atomic memory load/store  
  virtual llvm::Value* mem_load(llvm::Value* addr, llvm::Value* size) = 0;
  virtual void mem_store(llvm::Value* addr, llvm::Value* size, llvm::Value* rhs) = 0;

  virtual llvm::Value* neg_bits(llvm::Value*) = 0; // two's complement negation
  virtual llvm::Value* add_bits(llvm::Value*, llvm::Value*) = 0; // with overflow wrapping
  virtual llvm::Value* sub_bits(llvm::Value*, llvm::Value*) = 0;
  virtual llvm::Value* mul_bits(llvm::Value*, llvm::Value*) = 0;
  virtual llvm::Value* sdiv_bits(llvm::Value*, llvm::Value*) = 0; // rounding towards zero, divide by zero is undefined

  virtual llvm::Value* not_bits(llvm::Value*) = 0;
  virtual llvm::Value* or_bits(llvm::Value*, llvm::Value*) = 0;
  virtual llvm::Value* xor_bits(llvm::Value*, llvm::Value*) = 0;
  virtual llvm::Value* and_bits(llvm::Value*, llvm::Value*) = 0;

  virtual llvm::Value* eq_bits(llvm::Value*, llvm::Value*) = 0;
  virtual llvm::Value* ne_bits(llvm::Value*, llvm::Value*) = 0;
  virtual llvm::Value* slt_bits(llvm::Value*, llvm::Value*) = 0;
  virtual llvm::Value* sle_bits(llvm::Value*, llvm::Value*) = 0;

  virtual llvm::Value* lsl_bits(llvm::Value*, llvm::Value* shift) = 0;
  virtual llvm::Value* lsr_bits(llvm::Value*, llvm::Value* shift) = 0;
  virtual llvm::Value* asr_bits(llvm::Value*, llvm::Value* shift) = 0;

  virtual llvm::Value* replicate_bits(llvm::Value* val, uint64_t count) = 0;
  virtual llvm::Value* append_bits(llvm::Value* high, llvm::Value* low) = 0;

  virtual llvm::Value* zero_extend(llvm::Value* val, uint64_t finalWidth) = 0;
  virtual llvm::Value* sign_extend(llvm::Value* val, uint64_t finalWidth) = 0;

};

