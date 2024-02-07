#pragma once

#include <llvm/IR/Value.h>
#include <llvm/IR/BasicBlock.h>

#include "SemanticsBaseVisitor.h"

class aslt_visitor : public aslt::SemanticsBaseVisitor { 
public:
  using expr_t = llvm::Value;
  using stmt_t = std::pair<llvm::BasicBlock, llvm::BasicBlock>;

  virtual std::any visitConditional_stmt(aslt::SemanticsParser::Conditional_stmtContext *ctx) override;
  virtual std::any visitStmt(aslt::SemanticsParser::StmtContext *context) override;
  virtual std::any visitStmts(aslt::SemanticsParser::StmtsContext *ctx) override;

  virtual std::any defaultResult() override {
    return std::any{};
  }
  virtual std::any aggregateResult(std::any vec, std::any nextResult) override {
    return nextResult;
  }
};
