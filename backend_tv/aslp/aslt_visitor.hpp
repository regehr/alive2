#pragma once

#include <any>
#include <map>

#include <llvm/IR/Value.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/Support/Casting.h>

#include "SemanticsBaseVisitor.h"

class aslt_visitor : public aslt::SemanticsBaseVisitor { 
public:
  using super = aslt::SemanticsBaseVisitor;
  using type_t = llvm::Type *;
  using expr_t = llvm::Value *;
  using stmt_t = std::pair<llvm::BasicBlock *, llvm::BasicBlock *>;

private:
  llvm::LLVMContext &context;
  llvm::Function &func;

  std::map<std::string, llvm::AllocaInst*> locals{};
  stmt_t statements;

  stmt_t statements_init(llvm::Function &func) {
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(context, "", &func);
    return std::make_pair(bb, bb);
  }

public:
  aslt_visitor(llvm::Function &func) : 
    context{func.getContext()},
    func{func},
    statements{statements_init(func)} {}

  virtual type_t type(aslt::SemanticsParser::TypeContext* ctx) {
    auto x = visitType(ctx);
    return std::any_cast<type_t>(x);
  }
  virtual expr_t expr(aslt::SemanticsParser::ExprContext* ctx) {
    auto x = visitExpr(ctx);
    return std::any_cast<expr_t>(x);
  }
  virtual stmt_t stmt(aslt::SemanticsParser::StmtContext* ctx) {
    return std::any_cast<stmt_t>(visitStmt(ctx));
  }

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
};
