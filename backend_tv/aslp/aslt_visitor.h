#pragma once

#include <any>
#include <format>
#include <map>

#include <llvm/IR/Value.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <type_traits>

#include "SemanticsBaseVisitor.h"

#include "SemanticsParser.h"
#include "interface.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"

namespace aslp {

using type_t = lifter_interface_llvm::type_t;
using expr_t = lifter_interface_llvm::expr_t;
using lexpr_t = lifter_interface_llvm::lexpr_t;
using stmt_t = lifter_interface_llvm::stmt_t;

class aslt_visitor : public aslt::SemanticsBaseVisitor {
public:
  using super = aslt::SemanticsBaseVisitor;
  lifter_interface_llvm &iface;

  enum class unify_mode { SEXT, ZEXT, EXACT, EXACT_VECTOR };

private:
  bool debug;
  llvm::Function &func;  // needed to create basic blocks in here
  llvm::LLVMContext &context;
  std::string block_prefix;

  lexpr_t xreg_sentinel;
  lexpr_t vreg_sentinel;
  lexpr_t pstate_sentinel;
  uint64_t depth = 0;

  std::map<std::string, lexpr_t> locals{};
  std::map<std::string, expr_t> constants{};
  std::map<lexpr_t, lexpr_t> ptrs{};
  std::map<unsigned, unsigned> stmt_counts{};

  // stmt_t statements_init(llvm::Function &func) {
  //   llvm::BasicBlock *bb = llvm::BasicBlock::Create(context, "", &func);
  //   return std::make_pair(bb, bb);
  // }

public:
  aslt_visitor(lifter_interface_llvm &iface, bool debug) :
    iface{iface},
    debug{debug},
    func{iface.ll_function()},
    context{func.getContext()},
    block_prefix{"aslp_" + iface.nextName()},
    xreg_sentinel{iface.get_reg(reg_t::X, 0)},
    vreg_sentinel{iface.get_reg(reg_t::V, 0)},
    pstate_sentinel{iface.get_reg(reg_t::PSTATE, (int)pstate_t::N)}
  {
    assert(xreg_sentinel);
  }
public:
  std::ostream& log() const& {
    static std::ostream nullstream(nullptr);
    if (debug)
      return std::cerr << std::string(depth, '|');
    else
      return nullstream;
  }

  virtual type_t type(aslt::SemanticsParser::TypeContext* ctx) {
    depth++;
    auto x = ctx->accept(this);
    depth--;
    return std::any_cast<type_t>(x);
  }

  virtual expr_t expr(aslt::SemanticsParser::ExprContext* ctx) {
    depth++;
    auto x = ctx->accept(this);
    depth--;
    auto result = std::any_cast<expr_t>(x);
    if (auto inst = llvm::dyn_cast<llvm::Instruction>(result); inst) {
      inst->setMetadata("aslp.expr",
        llvm::MDTuple::get(context, {llvm::MDString::get(context, ctx->getText())}));
    }
    return result;
  }

  virtual lexpr_t lexpr(aslt::SemanticsParser::LexprContext* ctx) {
    depth++;
    auto x = ctx->accept(this);
    // std::cout << x.type().name() << std::endl;
    depth--;
    return std::any_cast<lexpr_t>(x);
  }

  virtual lifter_interface_llvm::slice_t slice(aslt::SemanticsParser::Slice_exprContext* ctx) {
    depth++;
    auto x = visitSlice_expr(ctx);
    depth--;
    return std::any_cast<lifter_interface_llvm::slice_t>(x);
  }

  virtual int64_t integer(aslt::SemanticsParser::IntegerContext* ctx) {
    return std::any_cast<int64_t>(visitInteger(ctx));
  }

  virtual int64_t lit_int(aslt::SemanticsParser::ExprContext* ctx) {
    auto x = expr(ctx);
    auto i = llvm::cast<llvm::ConstantInt>(x);
    return i->getSExtValue();
  }

  virtual std::string ident(aslt::SemanticsParser::IdentContext* ctx) {
    return std::any_cast<std::string>(visitIdent(ctx));
  }

  /**
   * Intelligently coerce the given expression to the given LLVM type.
   * Where possible, exploits structure within the expression to improve precision.
   */
  virtual expr_t coerce(expr_t e, llvm::Type* ty) {
    if (e->getType() == ty) return e;
    if (auto load = llvm::dyn_cast<llvm::LoadInst>(e)) {
      return iface.createLoad(ty, load->getPointerOperand());
    } else if (auto bcast = llvm::dyn_cast<llvm::BitCastInst>(e)) {
      return coerce(bcast->getOperand(0), ty);
    }
    // } else if (auto trunc = llvm::dyn_cast<llvm::TruncInst>(e)) {
    //   return coerce(trunc->getOperand(0), ty);  // XXX: are truncs always guaranteed to obtain the lowest bytes?? seems susipcious
    // }
    // if (llvm::dyn_cast<llvm::PointerType>(ty)) {
    //   return llvm::IntToPtrInst::Create(e, ty);
    // }

    // log() << "fallback aslp bitcast: " << std::flush;
    // e->print(llvm::outs());
    // log() << " to " << std::flush;
    // ty->print(llvm::outs());
    // log() << std::endl;
    return iface.createBitCast(e, ty);
  }

  virtual expr_t coerce_to_int(expr_t e) {
    if (e->getType()->isIntegerTy()) {
      return e;
    }
    auto intty = iface.getIntTy(e->getType()->getPrimitiveSizeInBits());
    return iface.createBitCast(e, intty);
  }

  virtual expr_t ptr_expr(expr_t x);
  virtual std::pair<expr_t, expr_t> unify_sizes(expr_t x, expr_t y, unify_mode mode = unify_mode::EXACT);

  virtual lexpr_t ref_expr(expr_t expr) {
    // XXX: HACK! since ExprVar are realised as LoadInst, this is incorrect in an array.
    // hence, we undo the load to obtain the actual register.

    auto load = llvm::cast<llvm::LoadInst>(expr);
    auto base = llvm::dyn_cast<llvm::AllocaInst>(load->getPointerOperand());

    assert(base && "expr_var: attempt to reference non-allocainst in a lexpr context");
    // assert(load->isSafeToRemove() && "surely not");
    // load->eraseFromParent();

    // assert(base == xreg_sentinel || base == pstate_sentinel);

    return llvm::cast<llvm::AllocaInst>(base);
  }

  virtual lexpr_t expr_var(aslt::SemanticsParser::ExprContext* ctx) {
    auto x = expr(ctx);
    return ref_expr(x);
  }

  virtual stmt_t stmt(aslt::SemanticsParser::StmtContext* ctx) {
    depth++;
    auto x = visitStmt(ctx);
    depth--;
    // std::cout << "stmt cast" << '\n';
    auto result = std::any_cast<stmt_t>(x);
    result.first->begin()->setMetadata("aslp.stmt",
      llvm::MDTuple::get(context, {llvm::MDString::get(context, ctx->getText())}));
    return result;
  }

  virtual stmt_t new_stmt(const std::string_view& name) {
    auto count = stmt_counts[depth]++;
    std::string s = std::format("{}__{}_{}_", block_prefix, depth, count);
    s += name;
    s += '_';
    auto newbb = llvm::BasicBlock::Create(context, s, &func);
    iface.set_bb(newbb);
    return {newbb, newbb};
  }

  stmt_t link(stmt_t head, stmt_t tail) {
    llvm::BranchInst::Create(tail.first, head.second);
    auto bb = tail.second;
    assert(bb);
    iface.set_bb(bb);
    return {head.first, tail.second};
  }

  virtual void add_local(std::string s, lexpr_t v) {
    assert(!locals.contains(s) && "local variable already exists in aslt!");
    // XXX aslp will emit duplicated local variable names when a variable is declared within
    // a for loop.  https://github.com/UQ-PAC/aslp/issues/43
    locals.insert_or_assign(s, v);
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
  virtual stmt_t visit_stmt(const std::vector<aslt::SemanticsParser::StmtContext*>& stmts);
  virtual std::any visitStmts(aslt::SemanticsParser::StmtsContext *ctx) override;
  virtual std::any visitStmt_lines(aslt::SemanticsParser::Stmt_linesContext *ctx) override;
  virtual std::any visitAssign(aslt::SemanticsParser::AssignContext *ctx) override;
  virtual std::any visitConstDecl(aslt::SemanticsParser::ConstDeclContext *ctx) override;
  virtual std::any visitVarDecl(aslt::SemanticsParser::VarDeclContext *ctx) override;
  virtual std::any visitVarDeclsNoInit(aslt::SemanticsParser::VarDeclsNoInitContext *ctx) override;
  virtual std::any visitAssert(aslt::SemanticsParser::AssertContext *ctx) override;
  virtual std::any visitThrow(aslt::SemanticsParser::ThrowContext *ctx) override;
  virtual std::any visitCall_stmt(aslt::SemanticsParser::Call_stmtContext *ctx) override;
  virtual std::any visitConditionalStmt(aslt::SemanticsParser::ConditionalStmtContext *ctx) override;
  virtual std::any visitLoopStmt(aslt::SemanticsParser::LoopStmtContext *ctx) override;
  virtual std::any visitTypeBits(aslt::SemanticsParser::TypeBitsContext *context) override;
  virtual std::any visitTypeBoolean(aslt::SemanticsParser::TypeBooleanContext *context) override;
  virtual std::any visitTypeRegister(aslt::SemanticsParser::TypeRegisterContext *context) override;
  virtual std::any visitTypeConstructor(aslt::SemanticsParser::TypeConstructorContext *context) override;
  virtual std::any visitLExprVar(aslt::SemanticsParser::LExprVarContext *ctx) override;
  virtual std::any visitLExprField(aslt::SemanticsParser::LExprFieldContext *ctx) override;
  virtual std::any visitLExprArray(aslt::SemanticsParser::LExprArrayContext *ctx) override;
  virtual std::any visitExprVar(aslt::SemanticsParser::ExprVarContext *ctx) override;
  virtual std::any visitExprTApply(aslt::SemanticsParser::ExprTApplyContext *ctx) override;
  virtual std::any visitExprSlices(aslt::SemanticsParser::ExprSlicesContext *ctx) override;
  virtual std::any visitExprField(aslt::SemanticsParser::ExprFieldContext *ctx) override;
  virtual std::any visitExprArray(aslt::SemanticsParser::ExprArrayContext *ctx) override;
  virtual std::any visitExprLitInt(aslt::SemanticsParser::ExprLitIntContext *ctx) override;
  // virtual std::any visitExprLitHex(aslt::SemanticsParser::ExprLitHexContext *ctx) override;
  virtual std::any visitExprLitBits(aslt::SemanticsParser::ExprLitBitsContext *ctx) override;
  virtual std::any visitExprParen(aslt::SemanticsParser::ExprParenContext *ctx) override;
  // virtual std::any visitExprLitMask(aslt::SemanticsParser::ExprLitMaskContext *ctx) override;
  // virtual std::any visitExprLitString(aslt::SemanticsParser::ExprLitStringContext *ctx) override;
  virtual std::any visitTargs(aslt::SemanticsParser::TargsContext *ctx) override;
  virtual std::any visitSlice_expr(aslt::SemanticsParser::Slice_exprContext *ctx) override;
  // virtual std::any visitUuid(aslt::SemanticsParser::UuidContext *ctx) override;
  virtual std::any visitInteger(aslt::SemanticsParser::IntegerContext *ctx) override;
  virtual std::any visitBits(aslt::SemanticsParser::BitsContext *ctx) override;
  virtual std::any visitIdent(aslt::SemanticsParser::IdentContext *ctx) override;

  virtual std::any defaultResult() override {
    return std::any{};
  }
  virtual std::any aggregateResult(std::any vec, std::any nextResult) override {
    // std::cerr << vec.type().name() << " | " << nextResult.type().name() << std::endl;
    return nextResult;
  }

  virtual ~aslt_visitor() override = default;
};

} // namespace aslp
