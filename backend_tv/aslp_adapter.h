#pragma once

/*
 * ASLP is a semantics server that gives us formally-derived AArch64
 * instruction semantics. It is an accelerator for one backend, not a
 * lifter in its own right: bridge::run refuses every branch, call, and
 * return, and getArmOpcode withholds anything carrying a relocation, so
 * a complete classic lifter always has to be underneath it.
 *
 * mc2llvm therefore does not implement aslp::lifter_interface_llvm --
 * doing so put ~100 virtuals on every backend, and made RISC-V patches
 * edit an ARM semantics contract. Instead this adapter presents an
 * existing lifter to ASLP. It holds a reference, adds no state of its
 * own, and every method below is a forwarder, so ASLP and the classic
 * lifter share one register file and one insertion point.
 */

#include "backend_tv/arm2llvm.h"

#include "aslp/interface.h"

#include <optional>

namespace lifter {

class arm_aslp_adapter final : public aslp::lifter_interface_llvm {
  arm2llvm &L;

public:
  explicit arm_aslp_adapter(arm2llvm &L) : L{L} {}

  // maps an ASLP register reference onto the lifter's register file
  lexpr_t get_reg(aslp::reg_t regtype, uint64_t num) override;

  // encode I back to bytes for ASLP, which addresses semantics by opcode.
  // returns nullopt for the sentinel NOP and for anything carrying a
  // relocation fixup, neither of which ASLP can be asked about
  std::optional<aslp::opcode_t> getArmOpcode(const llvm::MCInst &I);

  void set_bb(llvm::BasicBlock *a0) override {
    L.LLVMBB = a0;
  }

  llvm::BasicBlock *get_bb() override {
    return L.LLVMBB;
  }

  llvm::Function &ll_function() override {
    return *L.liftedFn;
  }

  std::string nextName() override {
    return L.nextName();
  }

  bool isGOT(uint16_t spec) override {
    return L.isGOT(spec);
  }

  std::pair<std::string, uint16_t>
  MCExprToName(const llvm::MCExpr *expr) override {
    return L.MCExprToName(expr);
  }

  expr_t lookupExprVar(const llvm::MCExpr &a0) override {
    return L.lookupExprVar(a0);
  }

  void updateOutputReg(expr_t V, bool SExt = false) override {
    L.updateOutputReg(V, SExt);
  }

  expr_t getUnsignedIntConst(uint64_t val, u_int64_t bits) override {
    return L.getUnsignedIntConst(val, bits);
  }

  type_t getIntTy(unsigned bits) override {
    return L.getIntTy(bits);
  }

  type_t getFPType(unsigned bits) override {
    return L.getFPType(bits);
  }

  type_t getVecTy(unsigned eltSize, unsigned numElts,
                  bool isFP = false) override {
    return L.getVecTy(eltSize, numElts, isFP);
  }

  expr_t getUndefVec(unsigned numElts, unsigned eltSize) override {
    return L.getUndefVec(numElts, eltSize);
  }

  void assertTrue(expr_t cond) override {
    L.assertTrue(cond);
  }

  expr_t makeLoadWithOffset(expr_t base, expr_t offset, int size) override {
    return L.makeLoadWithOffset(base, offset, size);
  }

  void storeToMemoryValOffset(expr_t base, expr_t offset, u_int64_t size,
                              expr_t val) override {
    L.storeToMemoryValOffset(base, offset, size, val);
  }

  lexpr_t createAlloca(type_t ty, expr_t sz,
                       const std::string &NameStr) override {
    return L.createAlloca(ty, sz, NameStr);
  }

  expr_t createGEP(type_t ty, expr_t v, llvm::ArrayRef<expr_t> idxlist,
                   const std::string &NameStr) override {
    return L.createGEP(ty, v, idxlist, NameStr);
  }

  void createBranch(expr_t c, stmt_t t, stmt_t f) override {
    L.createBranch(c, t.first, f.first);
  }

  void createBranch(stmt_t dst) override {
    L.createBranch(dst.first);
  }

  expr_t createLoad(type_t ty, expr_t ptr) override {
    return L.createLoad(ty, ptr);
  }

  void createStore(expr_t v, expr_t ptr) override {
    L.createStore(v, ptr);
  }

  expr_t createInsertElement(expr_t vec, expr_t val, expr_t idx) override {
    return L.createInsertElement(vec, val, idx);
  }

  expr_t createInsertElement(expr_t vec, expr_t val, int idx) override {
    return L.createInsertElement(vec, val, idx);
  }

  expr_t createExtractElement(expr_t v, expr_t idx) override {
    return L.createExtractElement(v, idx);
  }

  expr_t createExtractElement(expr_t v, int idx) override {
    return L.createExtractElement(v, idx);
  }

  expr_t createShuffleVector(expr_t v, llvm::ArrayRef<int> mask) override {
    return L.createShuffleVector(v, mask);
  }

  expr_t createShuffleVector(expr_t v, expr_t v2,
                             llvm::ArrayRef<int> mask) override {
    return L.createShuffleVector(v, v2, mask);
  }

  expr_t createVectorReduceAdd(expr_t v) override {
    return L.createVectorReduceAdd(v);
  }

  expr_t createFusedMultiplyAdd(expr_t a, expr_t b, expr_t c) override {
    return L.createFusedMultiplyAdd(a, b, c);
  }

  expr_t createSelect(expr_t cond, expr_t a, expr_t b) override {
    return L.createSelect(cond, a, b);
  }

  expr_t createICmp(llvm::ICmpInst::Predicate p, expr_t a, expr_t b) override {
    return L.createICmp(p, a, b);
  }

  expr_t createFCmp(llvm::FCmpInst::Predicate p, expr_t a, expr_t b) override {
    return L.createFCmp(p, a, b);
  }

  expr_t createBinop(expr_t a, expr_t b,
                     llvm::Instruction::BinaryOps op) override {
    return L.createBinop(a, b, op);
  }

  expr_t createSDiv(expr_t a, expr_t b) override {
    return L.createSDiv(a, b);
  }

  expr_t createMul(expr_t a, expr_t b) override {
    return L.createMul(a, b);
  }

  expr_t createAdd(expr_t a, expr_t b) override {
    return L.createAdd(a, b);
  }

  expr_t createSub(expr_t a, expr_t b) override {
    return L.createSub(a, b);
  }

  expr_t createRawLShr(expr_t a, expr_t b) override {
    return L.createRawLShr(a, b);
  }

  expr_t createRawAShr(expr_t a, expr_t b) override {
    return L.createRawAShr(a, b);
  }

  expr_t createRawShl(expr_t a, expr_t b) override {
    return L.createRawShl(a, b);
  }

  expr_t createAnd(expr_t a, expr_t b) override {
    return L.createAnd(a, b);
  }

  expr_t createOr(expr_t a, expr_t b) override {
    return L.createOr(a, b);
  }

  expr_t createNot(expr_t a) override {
    return L.createNot(a);
  }

  expr_t createTrunc(expr_t v, type_t t) override {
    return L.createTrunc(v, t);
  }

  expr_t createSExt(expr_t v, type_t t) override {
    return L.createSExt(v, t);
  }

  expr_t createZExt(expr_t v, type_t t) override {
    return L.createZExt(v, t);
  }

  expr_t createUIToFP(expr_t v, type_t t) override {
    return L.createUIToFP(v, t);
  }

  expr_t createSIToFP(expr_t v, type_t t) override {
    return L.createSIToFP(v, t);
  }

  expr_t createBitCast(expr_t v, type_t t) override {
    return L.createBitCast(v, t);
  }

  expr_t createFPTrunc(expr_t v, type_t t) override {
    return L.createFPTrunc(v, t);
  }

  expr_t createFPExt(expr_t v, type_t t) override {
    return L.createFPExt(v, t);
  }

  expr_t createSQRT(expr_t v) override {
    return L.createSQRT(v);
  }

  expr_t createRound(expr_t v) override {
    return L.createRound(v);
  }

  expr_t createConstrainedRound(expr_t v) override {
    return L.createConstrainedRound(v);
  }

  expr_t createConstrainedFloor(expr_t v) override {
    return L.createConstrainedFloor(v);
  }

  expr_t createConstrainedCeil(expr_t v) override {
    return L.createConstrainedCeil(v);
  }

  expr_t createFPToSI_sat(expr_t v, type_t t) override {
    return L.createFPToSI_sat(v, t);
  }

  expr_t createFPToUI_sat(expr_t v, type_t t) override {
    return L.createFPToUI_sat(v, t);
  }
};

} // end namespace lifter
