#pragma once

#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>

namespace aslp {

enum struct pstate_t : uint64_t {
  N = 0, Z, C, V
};

enum struct reg_t {
  X,
  V,
  PSTATE,
};

class lifter_interface {
public:

  virtual ~lifter_interface() = default;

  // perform possibly non-atomic memory load/store  
  // virtual llvm::Value* mem_load(llvm::Value* addr, llvm::Value* size) = 0;
  // virtual void mem_store(llvm::Value* addr, llvm::Value* size, llvm::Value* rhs) = 0;

  // should return a ptr type value suitable for load and store
  virtual llvm::AllocaInst* get_reg(reg_t, uint64_t num) = 0;

  // XXX: callback for `aslt_visitor` to inform arm2llvm of changed basic blocks,
  // so create* functions create instructions in the right place.
  virtual void set_bb(llvm::BasicBlock*) = 0;
  virtual llvm::BasicBlock* get_bb() = 0;

  virtual llvm::Function& ll_function() = 0;

  // lifted instructions are named using the number of the ARM
  // instruction they come from
  virtual std::string nextName() = 0;

  virtual llvm::AllocaInst *createAlloca(llvm::Type *ty, llvm::Value *sz, const std::string &NameStr) = 0;

  virtual llvm::GetElementPtrInst *createGEP(llvm::Type *ty, llvm::Value *v, llvm::ArrayRef<llvm::Value *> idxlist,
                               const std::string &NameStr) = 0;

  virtual void createBranch(llvm::Value *c, llvm::BasicBlock *t, llvm::BasicBlock *f) = 0;

  virtual void createBranch(llvm::BasicBlock *dst) = 0;

  virtual llvm::LoadInst *createLoad(llvm::Type *ty, llvm::Value *ptr) = 0;

  virtual void createStore(llvm::Value *v, llvm::Value *ptr) = 0;

  virtual llvm::Value *createTrap() = 0;

  virtual llvm::Value *createSMin(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::Value *createSMax(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::Value *createUMin(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::Value *createUMax(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::Value *createFNeg(llvm::Value *v) = 0;

  virtual llvm::Value *createFAbs(llvm::Value *v) = 0;

  virtual llvm::CallInst *createSSubOverflow(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::CallInst *createSAddOverflow(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::CallInst *createUSubOverflow(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::CallInst *createUAddOverflow(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::CallInst *createUAddSat(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::CallInst *createUSubSat(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::CallInst *createSAddSat(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::CallInst *createSSubSat(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::CallInst *createCtPop(llvm::Value *v) = 0;

  // first argument is an i16
  virtual llvm::CallInst *createConvertFromFP16(llvm::Value *v, llvm::Type *ty) = 0;

  virtual llvm::CastInst *createConvertFPToSI(llvm::Value *v, llvm::Type *ty) = 0;

  virtual llvm::CastInst *createPtrToInt(llvm::Value *v, llvm::Type *ty) = 0;

  virtual llvm::InsertElementInst *createInsertElement(llvm::Value *vec, llvm::Value *val, int idx) = 0;

  virtual llvm::ExtractElementInst *createExtractElement(llvm::Value *v, llvm::Value *idx) = 0;

  virtual llvm::ExtractElementInst *createExtractElement(llvm::Value *v, int idx) = 0;

  virtual llvm::ShuffleVectorInst *createShuffleVector(llvm::Value *v, llvm::ArrayRef<int> mask) = 0;

  virtual llvm::Value *getIndexedElement(unsigned idx, unsigned eltSize, unsigned reg) = 0;

  virtual llvm::ExtractValueInst *createExtractValue(llvm::Value *v, llvm::ArrayRef<unsigned> idxs) = 0;

  virtual llvm::ReturnInst *createReturn(llvm::Value *v) = 0;

  virtual llvm::CallInst *createFShr(llvm::Value *a, llvm::Value *b, llvm::Value *c) = 0;

  virtual llvm::CallInst *createFShl(llvm::Value *a, llvm::Value *b, llvm::Value *c) = 0;

  virtual llvm::CallInst *createBitReverse(llvm::Value *v) = 0;

  virtual llvm::CallInst *createAbs(llvm::Value *v) = 0;

  virtual llvm::CallInst *createCtlz(llvm::Value *v) = 0;

  virtual llvm::CallInst *createBSwap(llvm::Value *v) = 0;

  virtual llvm::CallInst *createVectorReduceAdd(llvm::Value *v) = 0;

  virtual llvm::SelectInst *createSelect(llvm::Value *cond, llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::ICmpInst *createICmp(llvm::ICmpInst::Predicate p, llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::FCmpInst *createFCmp(llvm::FCmpInst::Predicate p, llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::BinaryOperator *createBinop(llvm::Value *a, llvm::Value *b, llvm::Instruction::BinaryOps op) = 0;

  virtual llvm::BinaryOperator *createUDiv(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::BinaryOperator *createSDiv(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::BinaryOperator *createMul(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::BinaryOperator *createAdd(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::BinaryOperator *createSub(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::Value *createRawLShr(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::Value *createMaskedLShr(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::Value *createRawAShr(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::Value *createMaskedAShr(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::Value *createRawShl(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::Value *createMaskedShl(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::Value *getLowOnes(int ones, int w) = 0;

  virtual llvm::Value *createMSL(llvm::Value *a, int b) = 0;

  virtual llvm::BinaryOperator *createAnd(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::BinaryOperator *createOr(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::BinaryOperator *createXor(llvm::Value *a, llvm::Value *b) = 0;

  virtual llvm::BinaryOperator *createNot(llvm::Value *a) = 0;

  virtual llvm::FreezeInst *createFreeze(llvm::Value *v) = 0;

  virtual llvm::CastInst *createTrunc(llvm::Value *v, llvm::Type *t) = 0;

  virtual llvm::CastInst *createSExt(llvm::Value *v, llvm::Type *t) = 0;

  virtual llvm::CastInst *createZExt(llvm::Value *v, llvm::Type *t) = 0;

  virtual llvm::CastInst *createBitCast(llvm::Value *v, llvm::Type *t) = 0;

  virtual llvm::CastInst *createCast(llvm::Value *v, llvm::Type *t, llvm::Instruction::CastOps op) = 0;

};

}
