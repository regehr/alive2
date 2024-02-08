#include "aslt_visitor.hpp"
#include "tree/TerminalNode.h"
#include "llvm/IR/Instructions.h"

#include <llvm/IR/Constants.h>
#include <llvm/Support/Casting.h>

#include <ranges>

using namespace aslt;

namespace {
  antlr4::tree::TerminalNode* OR(antlr4::tree::TerminalNode* x, antlr4::tree::TerminalNode* y) {
    return x ? x : y;
  }
}

std::any aslt_visitor::visitStmt(SemanticsParser::StmtContext *ctx) {
  // log() << "visitStmt: " << ctx->getText() << '\n';
  return std::any_cast<stmt_t>(super::visitStmt(ctx));
}
std::any aslt_visitor::visitStmts(SemanticsParser::StmtsContext *ctx) {
  log() << "visitStmts" << '\n';
  auto ctxs = ctx->stmt();
  assert(!ctxs.empty() && "statement list must not be empty!");

  stmt_t s = stmt(ctxs.at(0));
  for (auto& s2 : ctx->stmt() | std::ranges::views::drop(1)) {
    s = link(s, stmt(s2));
  }
  return s;
}
std::any aslt_visitor::visitAssign(SemanticsParser::AssignContext *ctx) {
  log() << "visitAssign: " << ctx->getText() << '\n';
  return super::visitAssign(ctx);
}
std::any aslt_visitor::visitConstDecl(SemanticsParser::ConstDeclContext *ctx) {
  log() << "visitConstDecl" << '\n';
  auto s = new_stmt();
  type_t ty = type(ctx->type());
  auto rhs = expr(ctx->expr());

  auto name = ctx->METHOD()->getText();
  log() << name << '\n';

  auto v = new llvm::AllocaInst(ty, 0, name, s.second);
  new llvm::StoreInst(rhs, v, s.second);

  add_local(name, v);
  return s;
}
std::any aslt_visitor::visitVarDecl(SemanticsParser::VarDeclContext *ctx) {
  log() << "visitVarDecl" << '\n';
  type_t ty = type(ctx->type());
  auto rhs = expr(ctx->expr());

  auto name = ctx->METHOD()->getText();
  log() << name << '\n';

  auto s = new_stmt();
  auto v = new llvm::AllocaInst(ty, 0, name, s.second);
  new llvm::StoreInst(rhs, v, s.second);

  add_local(name, v);
  return s;
}
std::any aslt_visitor::visitVarDeclsNoInit(SemanticsParser::VarDeclsNoInitContext *ctx) {
  log() << "visitVarDeclsNoInit" << '\n';
  type_t ty = type(ctx->type());
  auto names = ctx->METHOD();
  auto s = new_stmt();

  for (auto namectx : names) {
    auto name = namectx->getText();
    log() << name << '\n';
    auto v = new llvm::AllocaInst(ty, 0, name, s.second);
    add_local(name, v);
  }

  return s;
}
std::any aslt_visitor::visitAssert(SemanticsParser::AssertContext *ctx) {
  log() << "TODO visitAssert" << '\n';
  return super::visitAssert(ctx);
}
std::any aslt_visitor::visitCall_stmt(SemanticsParser::Call_stmtContext *ctx) {
  log() << "visitCall_stmt" << '\n';
  return super::visitCall_stmt(ctx);
}
std::any aslt_visitor::visitConditional_stmt(SemanticsParser::Conditional_stmtContext *ctx) {
  log() << "visitConditional_stmt" << '\n';
  return super::visitConditional_stmt(ctx);
}
std::any aslt_visitor::visitType(SemanticsParser::TypeContext *ctx) {
  log() << "visitType" << ' ' << ctx->getText() << '\n';
  auto typeWidth = expr(ctx->expr());
  auto x = llvm::cast<llvm::ConstantInt>(typeWidth); // widths of types must be constant.
  return (type_t)llvm::Type::getIntNTy(context, x->getSExtValue());
}
std::any aslt_visitor::visitLExprVar(SemanticsParser::LExprVarContext *ctx) {
  log() << "visitLExprVar" << '\n';
  auto name = OR(ctx->METHOD(), ctx->SSYMBOL())->getText();

  return super::visitLExprVar(ctx);
}
std::any aslt_visitor::visitLExprField(SemanticsParser::LExprFieldContext *ctx) {
  log() << "visitLExprField" << '\n';
  return super::visitLExprField(ctx);
}
std::any aslt_visitor::visitLExprArray(SemanticsParser::LExprArrayContext *ctx) {
  log() << "visitLExprArray" << '\n';
  return super::visitLExprArray(ctx);
}
std::any aslt_visitor::visitExprVar(SemanticsParser::ExprVarContext *ctx) {
  log() << "visitExprVar " << ctx->getText() << '\n';
  auto _name = OR(ctx->METHOD(), ctx->SSYMBOL());
  auto name = _name->getText();
  auto var = locals.find(name);

  if (var != locals.end())
    return static_cast<expr_t>(var->second);
  if (name == "_R")
    return static_cast<expr_t>(iface.get_reg(reg_t::X, 0));

  log() << name << '\n';
  assert(false && "unsupported or undefined variable!");

}
std::any aslt_visitor::visitExprTApply(SemanticsParser::ExprTApplyContext *ctx) {
  auto name = ctx->METHOD()->getText();
  log() << "visitExprTApply " << name << '\n';

  auto _args = ctx->expr();
  std::vector<expr_t> args;
  for (auto a : _args) {
    args.push_back(expr(a));
  }


  // two-argument functions
  if (args.size() == 2) {
    auto x = args[0], y = args[1];
    if (name == "add_bits.0") {
      return static_cast<expr_t>(iface.createAdd(x, y));
    }
  }
  assert(false && "unsupported TAPPLY");
}
std::any aslt_visitor::visitExprSlices(SemanticsParser::ExprSlicesContext *ctx) {
  log() << "visitExprSlices" << '\n';
  return super::visitExprSlices(ctx);
}
std::any aslt_visitor::visitExprField(SemanticsParser::ExprFieldContext *ctx) {
  log() << "visitExprField" << '\n';
  return super::visitExprField(ctx);
}
std::any aslt_visitor::visitExprArray(SemanticsParser::ExprArrayContext *ctx) {
  // an array read is used for registers
  log() << "visitExprArray" << '\n';
  auto base = expr(ctx->base);
  assert(ctx->indices.size() == 1 && "array access has multiple indices");

  auto index = expr(ctx->indices.at(0));
  auto idx = llvm::cast<llvm::ConstantInt>(index);
  if (base == x0) {
    auto reg = iface.get_reg(reg_t::X, idx->getSExtValue());
    auto load = iface.createLoad(reg->getAllocatedType(), reg);
    return static_cast<expr_t>(load);
  }

  assert(false && "expr array unsup");
}
std::any aslt_visitor::visitExprLitInt(SemanticsParser::ExprLitIntContext *ctx) {
  log() << "visitExprLitInt >" << ctx->getText() << "<\n";
  // XXX: it is an error for LitInt to appear outside of types.
  auto i100 = llvm::Type::getIntNTy(context, 100);
  auto node = OR(ctx->DEC(), ctx->BINARY());
  return static_cast<expr_t>(llvm::ConstantInt::get(i100, node->getText(), 10));
}
std::any aslt_visitor::visitExprLitHex(SemanticsParser::ExprLitHexContext *ctx) {
  log() << "visitExprLitHex" << '\n';
  assert(0);
  return super::visitExprLitHex(ctx);
}
std::any aslt_visitor::visitExprLitBits(SemanticsParser::ExprLitBitsContext *ctx) {
  log() << "visitExprLitBits" << '\n';
  return super::visitExprLitBits(ctx);
}
std::any aslt_visitor::visitExprLitMask(SemanticsParser::ExprLitMaskContext *ctx) {
  log() << "visitExprLitMask" << '\n';
  return super::visitExprLitMask(ctx);
}
std::any aslt_visitor::visitExprLitString(SemanticsParser::ExprLitStringContext *ctx) {
  log() << "visitExprLitString" << '\n';
  return super::visitExprLitString(ctx);
}
std::any aslt_visitor::visitTargs(SemanticsParser::TargsContext *ctx) {
  // log() << "visitTargs" << '\n';
  return visitExpr(ctx->expr());
}
std::any aslt_visitor::visitSlice_expr(SemanticsParser::Slice_exprContext *ctx) {
  log() << "visitSlice_expr" << '\n';
  return super::visitSlice_expr(ctx);
}
std::any aslt_visitor::visitUuid(SemanticsParser::UuidContext *ctx) {
  log() << "visitUuid" << '\n';
  return super::visitUuid(ctx);
}

