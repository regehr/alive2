#pragma once

#include <any>
#include <functional>
#include <map>

#include <llvm/IR/Value.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <type_traits>

#include "SemanticsBaseVisitor.h"

#include "interface.hpp"

class aslt_visitor : public aslt::SemanticsBaseVisitor { 
public:
  using super = aslt::SemanticsBaseVisitor;
  using type_t = llvm::Type *;
  using expr_t = llvm::Value *;
  using stmt_t = std::pair<llvm::BasicBlock *, llvm::BasicBlock *>;
  using var_t = llvm::AllocaInst *;

private:
  llvm::LLVMContext &context;
  llvm::Function &func;
  lifter_interface &iface;

  var_t x0;
  uint64_t depth = 0;
  llvm::BasicBlock *bb = nullptr;

  std::map<std::string, var_t> locals{};

  // stmt_t statements_init(llvm::Function &func) {
  //   llvm::BasicBlock *bb = llvm::BasicBlock::Create(context, "", &func);
  //   return std::make_pair(bb, bb);
  // }

public:
  aslt_visitor(llvm::Function &func, lifter_interface &iface) : 
    context{func.getContext()},
    func{func},
    iface{iface}, x0{iface.get_reg(reg_t::X, 0)} {
    assert(x0);
  }
protected:
  std::ostream& log() const& {
    return std::cerr << std::string(depth, ' ');
  }

  virtual type_t type(aslt::SemanticsParser::TypeContext* ctx) {
    depth++;
    auto x = visitType(ctx);
    depth--;
    return std::any_cast<type_t>(x);
  }

  virtual expr_t expr(aslt::SemanticsParser::ExprContext* ctx) {
    depth++;
    auto x = visitExpr(ctx);
    depth--;
    // std::cout << "expr cast" << '\n' << x.type().name() << '\n';
    return std::any_cast<expr_t>(x);
  }

  virtual int64_t lit_int(aslt::SemanticsParser::ExprContext* ctx) {
    assert(dynamic_cast<aslt::SemanticsParser::Expr_Context*>(ctx->expr_()) && "non-literal found where a ExprLitInt was expected");
    auto x = expr(ctx);
    auto i = llvm::cast<llvm::ConstantInt>(x);
    return i->getSExtValue();
  }

  virtual stmt_t stmt(aslt::SemanticsParser::StmtContext* ctx) {
    depth++;
    auto x = visitStmt(ctx);
    depth--;
    // std::cout << "stmt cast" << '\n';
    return std::any_cast<stmt_t>(x);
  }

  virtual stmt_t new_stmt() {
    auto newbb = llvm::BasicBlock::Create(context, "stmt", &func);
    bb = newbb;
    iface.update_bb(newbb);
    return std::make_pair(newbb, newbb);
  }

  stmt_t link(stmt_t head, stmt_t tail) {
    llvm::BranchInst::Create(tail.first, head.second);
    bb = tail.second;
    assert(bb);
    iface.update_bb(bb);
    return std::make_pair(head.first, tail.second);
  }

  virtual var_t get_local(std::string s) const& {
    return locals.at(s);
  }

  virtual void add_local(std::string s, var_t v) {
    assert(!locals.contains(s) && "local variable already exists in aslt!");
    locals.emplace(s, v);
  }

  template<std::ranges::range It, typename Ctx, typename U>
  std::vector<U> map(const It &xs, U(aslt_visitor::*f)(Ctx)) {
    std::vector<U> out{};
    for (const auto& x : xs)
      out.push_back((this->*f)(x));
    return out;
  }

  template<std::ranges::range It, typename F, typename U = std::invoke_result_t<F, std::iter_value_t<It>>>
  std::vector<U> map(const It &xs, F f) {
    std::vector<U> out{};
    for (const auto& x : xs)
      out.push_back(f(x));
    return out;
  }
public:
  virtual std::any visitStmt(aslt::SemanticsParser::StmtContext *ctx) override;
  virtual std::any visitStmts(aslt::SemanticsParser::StmtsContext *ctx) override;
  virtual std::any visitAssign(aslt::SemanticsParser::AssignContext *ctx) override;
  virtual std::any visitConstDecl(aslt::SemanticsParser::ConstDeclContext *ctx) override;
  virtual std::any visitVarDecl(aslt::SemanticsParser::VarDeclContext *ctx) override;
  virtual std::any visitVarDeclsNoInit(aslt::SemanticsParser::VarDeclsNoInitContext *ctx) override;
  virtual std::any visitAssert(aslt::SemanticsParser::AssertContext *ctx) override;
  virtual std::any visitCall_stmt(aslt::SemanticsParser::Call_stmtContext *ctx) override;
  virtual std::any visitConditional_stmt(aslt::SemanticsParser::Conditional_stmtContext *ctx) override;
  virtual std::any visitType(aslt::SemanticsParser::TypeContext *ctx) override;
  virtual std::any visitLExprVar(aslt::SemanticsParser::LExprVarContext *ctx) override;
  virtual std::any visitLExprField(aslt::SemanticsParser::LExprFieldContext *ctx) override;
  virtual std::any visitLExprArray(aslt::SemanticsParser::LExprArrayContext *ctx) override;
  virtual std::any visitExprVar(aslt::SemanticsParser::ExprVarContext *ctx) override;
  virtual std::any visitExprTApply(aslt::SemanticsParser::ExprTApplyContext *ctx) override;
  virtual std::any visitExprSlices(aslt::SemanticsParser::ExprSlicesContext *ctx) override;
  virtual std::any visitExprField(aslt::SemanticsParser::ExprFieldContext *ctx) override;
  virtual std::any visitExprArray(aslt::SemanticsParser::ExprArrayContext *ctx) override;
  virtual std::any visitExprLitInt(aslt::SemanticsParser::ExprLitIntContext *ctx) override;
  virtual std::any visitExprLitHex(aslt::SemanticsParser::ExprLitHexContext *ctx) override;
  virtual std::any visitExprLitBits(aslt::SemanticsParser::ExprLitBitsContext *ctx) override;
  virtual std::any visitExprLitMask(aslt::SemanticsParser::ExprLitMaskContext *ctx) override;
  virtual std::any visitExprLitString(aslt::SemanticsParser::ExprLitStringContext *ctx) override;
  virtual std::any visitTargs(aslt::SemanticsParser::TargsContext *ctx) override;
  virtual std::any visitSlice_expr(aslt::SemanticsParser::Slice_exprContext *ctx) override;
  virtual std::any visitUuid(aslt::SemanticsParser::UuidContext *ctx) override;

  virtual std::any defaultResult() override {
    return std::any{};
  }
  virtual std::any aggregateResult(std::any vec, std::any nextResult) override {
    return nextResult;
  }

  virtual ~aslt_visitor() override = default;
};
