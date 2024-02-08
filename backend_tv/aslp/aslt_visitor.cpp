#include "aslt_visitor.hpp"
#include "tree/TerminalNode.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Instructions.h"

#include <format>
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
  log() << "visitAssign TODO: " << ctx->getText() << '\n';
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
  log() << "TODO visitCall_stmt" << '\n';
  return super::visitCall_stmt(ctx);
}
std::any aslt_visitor::visitConditional_stmt(SemanticsParser::Conditional_stmtContext *ctx) {
  log() << "TODO visitConditional_stmt" << '\n';
  return super::visitConditional_stmt(ctx);
}
std::any aslt_visitor::visitType(SemanticsParser::TypeContext *ctx) {
  log() << "visitType" << ' ' << ctx->getText() << '\n';
  auto typeWidth = expr(ctx->expr());
  auto x = llvm::cast<llvm::ConstantInt>(typeWidth); // widths of types must be constant.
  return (type_t)llvm::Type::getIntNTy(context, x->getSExtValue());
}
std::any aslt_visitor::visitLExprVar(SemanticsParser::LExprVarContext *ctx) {
  log() << "TODO visitLExprVar" << '\n';
  auto name = OR(ctx->METHOD(), ctx->SSYMBOL())->getText();

  return super::visitLExprVar(ctx);
}
std::any aslt_visitor::visitLExprField(SemanticsParser::LExprFieldContext *ctx) {
  log() << "TODO visitLExprField" << '\n';
  return super::visitLExprField(ctx);
}
std::any aslt_visitor::visitLExprArray(SemanticsParser::LExprArrayContext *ctx) {
  log() << "TODO visitLExprArray" << '\n';
  return super::visitLExprArray(ctx);
}
std::any aslt_visitor::visitExprVar(SemanticsParser::ExprVarContext *ctx) {
  log() << "visitExprVar " << ctx->getText() << '\n';
  auto _name = OR(ctx->METHOD(), ctx->SSYMBOL());
  auto name = _name->getText();

  expr_t var;
  if (locals.contains(name))
    var = locals.at(name);
  else if (name == "_R")
    var = iface.get_reg(reg_t::X, 0);
  else
    assert(false && "unsupported or undefined variable!");

  auto ptr = llvm::cast<llvm::AllocaInst>(var);
  return static_cast<expr_t>(iface.createLoad(ptr->getAllocatedType(), ptr));
}
std::any aslt_visitor::visitExprTApply(SemanticsParser::ExprTApplyContext *ctx) {
  auto name = ctx->METHOD()->getText();
  log() 
    << "visitExprTApply " 
    << std::format("{} [{}] ({})", name, ctx->targs().size(), ctx->expr().size())
    << std::endl;

  auto args = map(ctx->expr(), &aslt_visitor::expr);
  auto targctxs = map(ctx->targs(), [](auto x) { return x->expr(); });
  auto targs = map(targctxs, &aslt_visitor::lit_int);

  if (args.size() == 1) {
    auto x = args[0];
    if (name == "cvt_bool_bv.0") {
      assert(x->getType()->getIntegerBitWidth() == 1 && "size mismatch in cvt bv bool");
      return x;

    } else if (name == "not_bits.0") {
      return static_cast<expr_t>(iface.createNot(x));

    }
  } else if (args.size() == 2) {
    auto x = args[0], y = args[1];
    if (name == "SignExtend.0" && targs.size() == 2) {
      type_t finalty = llvm::Type::getIntNTy(context, targs[1]);
      return static_cast<expr_t>(iface.createSExt(x, finalty));

    } else if (name == "ZeroExtend.0" && targs.size() == 2) {
      type_t finalty = llvm::Type::getIntNTy(context, targs[1]);
      return static_cast<expr_t>(iface.createZExt(x, finalty));

    } else if (name == "eq_bits.0") {
      return static_cast<expr_t>(iface.createICmp(llvm::CmpInst::Predicate::ICMP_EQ, x, y));

    } else if (name == "add_bits.0") {
      return static_cast<expr_t>(iface.createAdd(x, y));
    }
  }
  assert(false && "unsupported TAPPLY");
}
std::any aslt_visitor::visitExprSlices(SemanticsParser::ExprSlicesContext *ctx) {
  log() << "TODO visitExprSlices" << '\n';
  return super::visitExprSlices(ctx);
}
std::any aslt_visitor::visitExprField(SemanticsParser::ExprFieldContext *ctx) {
  log() << "TODO visitExprField" << '\n';
  return super::visitExprField(ctx);
}
std::any aslt_visitor::visitExprArray(SemanticsParser::ExprArrayContext *ctx) {
  // an array read is used for registers
  log() << "visitExprArray" << '\n';
  auto _base = expr(ctx->base);

  // XXX: HACK! since ExprVar are realised as LoadInst, this is incorrect in an array.
  // hence, we undo the load to obtain the actual register.
  auto load = llvm::cast<llvm::LoadInst>(_base);
  auto base = load->getPointerOperand();
  assert(load->isSafeToRemove());
  load->eraseFromParent();


  assert(ctx->indices.size() == 1 && "array access has multiple indices");
  auto index = lit_int(ctx->indices.at(0));

  if (base == x0) {
    auto reg = iface.get_reg(reg_t::X, index);
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
  log() << "TODO visitExprLitHex" << '\n';
  assert(0);
  return super::visitExprLitHex(ctx);
}
std::any aslt_visitor::visitExprLitBits(SemanticsParser::ExprLitBitsContext *ctx) {
  log() << "TODO visitExprLitBits" << '\n';
  return super::visitExprLitBits(ctx);
}
std::any aslt_visitor::visitExprLitMask(SemanticsParser::ExprLitMaskContext *ctx) {
  log() << "TODO visitExprLitMask" << '\n';
  return super::visitExprLitMask(ctx);
}
std::any aslt_visitor::visitExprLitString(SemanticsParser::ExprLitStringContext *ctx) {
  log() << "TODO visitExprLitString" << '\n';
  return super::visitExprLitString(ctx);
}
std::any aslt_visitor::visitTargs(SemanticsParser::TargsContext *ctx) {
  // log() << "visitTargs" << '\n';
  return visitExpr(ctx->expr());
}
std::any aslt_visitor::visitSlice_expr(SemanticsParser::Slice_exprContext *ctx) {
  log() << "TODO visitSlice_expr" << '\n';
  return super::visitSlice_expr(ctx);
}
std::any aslt_visitor::visitUuid(SemanticsParser::UuidContext *ctx) {
  log() << "TODO visitUuid" << '\n';
  return super::visitUuid(ctx);
}

