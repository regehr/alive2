#include "aslt_visitor.hpp"
#include "aslp/interface.hpp"
#include "tree/TerminalNode.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/MDBuilder.h"

#include <format>
#include <llvm/IR/Constants.h>
#include <llvm/Support/Casting.h>

#include <ranges>

using namespace aslt;

namespace {
  void require(bool cond, const std::string_view& str, llvm::Value* val = nullptr) {
   if (cond)
     return;

   std::string msg{"Aslp assertion failure! "};
   msg += str;
   if (val) {
     std::string s;
     llvm::raw_string_ostream os{s};
     val->print(os, true);
     std::cerr << '\n' << s << '\n' << std::endl;
   }
   std::cerr << std::flush;
   std::cout << std::flush;
   llvm::outs().flush();
   llvm::errs().flush();
   throw std::runtime_error(msg);
  }

  [[noreturn]] void die(const std::string_view& str, llvm::Value* val = nullptr) {
    require(false, str, val);
    assert(false && "unreachable");
  }
}

#define require_(x) require(x, #x)
#undef assert

namespace aslp {


std::pair<expr_t, expr_t> aslt_visitor::ptr_expr(llvm::Value* x, llvm::Instruction* before) {
  lexpr_t base = nullptr;
  expr_t offset = iface.getIntConst(0, x->getType()->getIntegerBitWidth());

  // experimenting: this code does not convert via GEP and assumes dereferenceable.
  // x = new llvm::IntToPtrInst(x, llvm::PointerType::get(context, 0), "", iface.get_bb());
  // llvm::cast<llvm::IntToPtrInst>(x)->setMetadata("dereferenceable", 
  //     llvm::MDNode::get(context, {(llvm::ConstantAsMetadata::get(iface.getIntConst(99, 64)))}));
  // x->dump();
  // return {x, offset};

  auto Add = llvm::BinaryOperator::BinaryOps::Add;
  if (auto add = llvm::dyn_cast<llvm::BinaryOperator>(x); add && add->getOpcode() == Add) {
    // undo the add instruction into a GEP operation
    auto _base = add->getOperand(0);
    offset = add->getOperand(1);

    // add->eraseFromParent();
    base = ref_expr(_base);

  } else if (auto load = llvm::dyn_cast<llvm::LoadInst>(x); load) {
    auto wd = load->getType()->getIntegerBitWidth();
    base = ref_expr(load);
    offset = iface.getIntConst(0, wd);
  }

  require(base && offset, "unable to coerce to pointer", x);
  auto load = iface.createLoad(base->getAllocatedType(), base);
  auto ptr = new llvm::IntToPtrInst(load, llvm::PointerType::get(context, 0), "", iface.get_bb());
  return {ptr, offset};
}

std::pair<llvm::Value*, llvm::Value*> aslt_visitor::unify_sizes(llvm::Value* x, llvm::Value* y, bool sign) {
  auto ty1 = x->getType(), ty2 = y->getType();
  auto wd1 = ty1->getIntegerBitWidth(), wd2 = ty2->getIntegerBitWidth();

  auto sext = &lifter_interface_llvm::createSExt;
  auto zext = &lifter_interface_llvm::createZExt;
  const decltype(sext) extend = sign ? sext : zext;

  if (wd1 < wd2) {
    x = (iface.*extend)(x, ty2);
  } else if (wd2 < wd1) {
    y = (iface.*extend)(y, ty1);
  }
  return std::make_pair(x, y);
}

std::any aslt_visitor::visitStmt(SemanticsParser::StmtContext *ctx) {
  return std::any_cast<stmt_t>(super::visitStmt(ctx));
}

stmt_t aslt_visitor::visit_stmt(const std::vector<aslt::SemanticsParser::StmtContext *>& stmts) {
  log() << "visitStmts " << '\n';

  // store and reset current bb to support nesting statements nicely
  auto bb = iface.get_bb();

  if (stmts.empty()) {
    auto empty = new_stmt("stmtlist_empty");
    iface.set_bb(bb);
    return empty;
  }

  require(!stmts.empty(), "statement list must not be empty!");

  stmt_counts[depth+1] = 0;
  stmt_t s = stmt(stmts.at(0));
  s.second = iface.get_bb(); // XXX: this is a hack to support expressions which add basic blocks
  for (auto& s2 : stmts | std::ranges::views::drop(1)) {
    s = link(s, stmt(s2));
  }

  iface.set_bb(bb);
  return s;
}

std::any aslt_visitor::visitStmts(SemanticsParser::StmtsContext *ctx) {
  static constexpr std::vector<aslt::SemanticsParser::StmtContext *> empty;
  return visit_stmt(ctx ? ctx->stmt() : empty);
}

std::any aslt_visitor::visitStmt_lines(aslt::SemanticsParser::Stmt_linesContext *ctx) {
  static constexpr std::vector<aslt::SemanticsParser::StmtContext *> empty;
  return visit_stmt(ctx ? ctx->stmt() : empty);
}

std::any aslt_visitor::visitAssign(SemanticsParser::AssignContext *ctx) {
  log() << "visitAssign : " << ctx->getText() << '\n';

  stmt_t s = new_stmt("assign");
  auto lhs = lexpr(ctx->lexpr());
  auto rhs = expr(ctx->expr());

  iface.createStore(rhs, lhs);
  return s;
}

std::any aslt_visitor::visitConstDecl(SemanticsParser::ConstDeclContext *ctx) {
  log() << "visitConstDecl" << '\n';

  auto s = new_stmt("constdecl");
  type_t ty = type(ctx->type());
  auto rhs = expr(ctx->expr());

  auto name = ident(ctx->ident());
  log() << name << '\n';

  auto v = new llvm::AllocaInst(ty, 0, name, s.second);
  new llvm::StoreInst(rhs, v, s.second);

  add_local(name, v);
  return s;
}

std::any aslt_visitor::visitVarDecl(SemanticsParser::VarDeclContext *ctx) {
  log() << "visitVarDecl" << '\n';

  auto s = new_stmt("vardecl");
  type_t ty = type(ctx->type());
  auto rhs = expr(ctx->expr());

  auto name = ident(ctx->ident());
  log() << name << '\n';

  auto v = new llvm::AllocaInst(ty, 0, name, s.second);
  new llvm::StoreInst(rhs, v, s.second);

  add_local(name, v);
  return s;
}

std::any aslt_visitor::visitVarDeclsNoInit(SemanticsParser::VarDeclsNoInitContext *ctx) {
  log() << "visitVarDeclsNoInit" << '\n';

  auto s = new_stmt("vardeclnoinit");
  type_t ty = type(ctx->type());
  auto names = map(ctx->ident(), &aslt_visitor::ident);

  for (auto name : names) {
    log() << name << '\n';
    auto v = new llvm::AllocaInst(ty, 0, name, s.second);
    add_local(name, v);
  }

  return s;
}

std::any aslt_visitor::visitAssert(SemanticsParser::AssertContext *ctx) {
  log() << "visitAssert" << '\n';

  auto s = new_stmt("assert");
  auto c = expr(ctx->expr());
  iface.assertTrue(c);
  return s;
}

std::any aslt_visitor::visitThrow(aslt::SemanticsParser::ThrowContext *ctx) {
  auto s = new_stmt("throw");
  iface.assertTrue(iface.getIntConst(0, 1));
  return s;
}

std::any aslt_visitor::visitCall_stmt(SemanticsParser::Call_stmtContext *ctx) {
  auto name = ident(ctx->ident());
  log() << "visitCall_stmt" << '\n';

  auto s = new_stmt("call");

  auto args = map(ctx->expr(), &aslt_visitor::expr);
  auto targctxs = map(ctx->targs(), [](auto x) { return x->expr(); });
  auto targs = map(targctxs, &aslt_visitor::lit_int);

  if (args.size() == 4) {
    if (name == "Mem.set.0") {
      // Mem[] - assignment (write) form
      // ===============================
      // Perform a write of 'size' bytes. The byte order is reversed for a big-endian access.
      // Mem[bits(64) address, integer size, AccType acctype] = bits(size*8) value
      auto addr = args[0], bytes = args[1], acctype = args[2], val = args[3];
      (void)acctype;

      auto size = llvm::cast<llvm::ConstantInt>(bytes)->getSExtValue();
      auto [ptr, offset] = ptr_expr(addr);
      // iface.createStore(val, ptr);
      iface.storeToMemoryValOffset(ptr, offset, size, val);
      return s;
    }
  }

  die("call stmt unsup: " + name);
}

std::any aslt_visitor::visitConditionalStmt(SemanticsParser::ConditionalStmtContext *ctx) {
  log() << "visitConditional_stmt" << '\n';

  auto entry = new_stmt("conditional");
  
  auto cond = expr(ctx->expr());
  require(cond->getType()->getIntegerBitWidth() == 1, "condition must have type i1", cond);

  // NOTE! visitStmts will restore the bb after it concludes, i.e. reset to 'entry'
  auto tstmts = std::any_cast<stmt_t>(visitStmts(ctx->tcase));
  auto fstmts = std::any_cast<stmt_t>(visitStmts(ctx->fcase));

  iface.createBranch(cond, tstmts, fstmts);

  auto join = new_stmt("conditional_join");
  link(tstmts, join);
  link(fstmts, join);

  return stmt_t{entry.first, join.second};
}

std::any aslt_visitor::visitLoopStmt(SemanticsParser::LoopStmtContext *ctx) {
  log() << "visitLoop_stmt" << '\n';

  bool inc = ctx->direction()->getText() == "Direction_Up";

  /* Loop index type is an integer, which seems to always be 100 bits wide */
  int width = 100;
  type_t ty = iface.getIntTy(width);

  /* for_header:
   *  var := start
   *  goto body
   *
   * inc_body:
   *  stmts
   *  var := var + 1
   *  if var > end then exit else body
   *
   * dec_body:
   *  stmts
   *  var := var - 1
   *  if var < end then exit else body
   *
   * for_exit:
   */

  auto exit = new_stmt("for_exit");
  auto entry = new_stmt("for_header");

  /* Allocate and init the loop index */
  auto start = expr(ctx->start_index);
  auto name = ident(ctx->ident());
  auto v = new llvm::AllocaInst(ty, 0, name, entry.second);
  new llvm::StoreInst(start, v, entry.second);
  add_local(name, v);

  /* Process loop body */
  auto body = std::any_cast<stmt_t>(visitStmts(ctx->stmts()));
  iface.set_bb(body.second); // TODO: Not sure if this is necessary

  /* Increment/Decrement counter */
  auto v2 = iface.createLoad(ty, v);
  auto rhs2 = inc ? iface.createAdd(v2, iface.getIntConst(1, width)) : iface.createSub(v2, iface.getIntConst(1, width));
  new llvm::StoreInst(rhs2, v, body.second);

  /* Test stop, branching to exit or back to body entry */
  auto stop = expr(ctx->stop_index);
  auto cond = iface.createICmp(inc ? llvm::ICmpInst::Predicate::ICMP_SLT :  llvm::ICmpInst::Predicate::ICMP_SGT, stop, rhs2);
  iface.createBranch(cond, exit, body);

  /* Fallthrough for entry */
  link(entry,body);

  return stmt_t{entry.first, exit.second};
}

std::any aslt_visitor::visitTypeBits(SemanticsParser::TypeBitsContext *ctx) {
  log() << "visitTypeBits" << ' ' << ctx->getText() << '\n';
  auto bits = lit_int(ctx->expr());
  return (type_t)iface.getIntTy(bits);
}

std::any aslt_visitor::visitTypeRegister(aslt::SemanticsParser::TypeRegisterContext *context) {
  auto bits = integer(context->integer());
  return (type_t)iface.getIntTy(bits);
}

std::any aslt_visitor::visitTypeConstructor(aslt::SemanticsParser::TypeConstructorContext *context) {
  auto name = context->name->getText();
  if (name == "FPRounding")
    return (type_t)iface.getIntTy(2);
  if (name == "\"boolean\"")
    return iface.getIntTy(1);
  die("type constructor unsup: " + name);
}

std::any aslt_visitor::visitTypeBoolean(SemanticsParser::TypeBooleanContext *ctx) {
  log() << "visitTypeBoolean" << ' ' << ctx->getText() << '\n';
  return (type_t)iface.getIntTy(1);
}

std::any aslt_visitor::visitLExprVar(SemanticsParser::LExprVarContext *ctx) {
  log() << "visitLExprVar" << '\n';
  auto name = ident(ctx->ident());

  if (locals.contains(name)) {
    return locals.at(name);
  } else if (name == "_R") {
    return xreg_sentinel;
  } else if (name == "PSTATE") {
    return pstate_sentinel;
  } else if (name == "_Z") {
    return vreg_sentinel;
  } else if (name == "SP_EL0") {
    return iface.get_reg(reg_t::X, 31);
  } else if (name == "FPSR") {
    // XXX following classic lifter's ignoring of FPSR
    return iface.createAlloca(iface.getIntTy(64), nullptr, "FPSR_void");
  }
  die("lexprvar unsup: " + name);
}

std::any aslt_visitor::visitLExprField(SemanticsParser::LExprFieldContext *ctx) {
  log() << "visitLExprField" << '\n';

  lexpr_t base = lexpr(ctx->lexpr());
  std::string field = ident(ctx->ident());

  if (base == pstate_sentinel) {
    if (field == "N") return iface.get_reg(reg_t::PSTATE, (int)pstate_t::N);
    if (field == "Z") return iface.get_reg(reg_t::PSTATE, (int)pstate_t::Z);
    if (field == "C") return iface.get_reg(reg_t::PSTATE, (int)pstate_t::C);
    if (field == "V") return iface.get_reg(reg_t::PSTATE, (int)pstate_t::V);
    die("pstate unexpect");
  }
  die("lexpr field unsup: " + field, base);
}

std::any aslt_visitor::visitLExprArray(SemanticsParser::LExprArrayContext *ctx) {
  log() << "visitLExprArray" << '\n';
  auto lhs = lexpr(ctx->lexpr());
  auto index = map(ctx->expr(), &aslt_visitor::lit_int);
  require(index.size() == 1, "array index sizes != 1");

  if (lhs == xreg_sentinel) {
    return iface.get_reg(reg_t::X, index.at(0));
  } else if (lhs == vreg_sentinel) {
    return iface.get_reg(reg_t::V, index.at(0));
  }

  die("lexpr array unsup");
}

std::any aslt_visitor::visitExprVar(SemanticsParser::ExprVarContext *ctx) {
  log() << "visitExprVar " << ctx->getText() << '\n';
  auto name = ident(ctx->ident());

  expr_t var;
  if (locals.contains(name))
    var = locals.at(name);
  else if (name == "_R")
    var = xreg_sentinel; // return X0 as a sentinel for all X registers
  else if (name == "_Z")
    var = vreg_sentinel;
  else if (name == "PSTATE")
    var = pstate_sentinel;
  else if (name == "SP_EL0")
    var = iface.get_reg(reg_t::X, 31);
  else if (name == "TRUE" || name == "FALSE")
    return static_cast<expr_t>(iface.getIntConst(name == "TRUE", 1));
  else if (name == "FPSR")
    // XXX following classic lifter's ignoring of FPSR
    return static_cast<expr_t>(iface.getIntConst(0, 64));
  else if (name == "FPCR")
    // XXX do not support FPCR-dependent behaviour
    return static_cast<expr_t>(llvm::UndefValue::get(iface.getIntTy(32)));
  else 
    die("unsupported or undefined variable: " + name);

  auto ptr = llvm::cast<llvm::AllocaInst>(var);
  return static_cast<expr_t>(iface.createLoad(ptr->getAllocatedType(), ptr));
}

std::any aslt_visitor::visitExprTApply(SemanticsParser::ExprTApplyContext *ctx) {
  auto name = ident(ctx->ident());
  log() 
    << "visitExprTApply " 
    << std::format("{} [{} targs] ({} args)", name, ctx->targs().size(), ctx->expr().size())
    << std::endl;

  auto args = map(ctx->expr(), &aslt_visitor::expr);
  auto targctxs = map(ctx->targs(), [](auto x) { return x->expr(); });
  auto targs = map(targctxs, &aslt_visitor::lit_int);

  if (args.size() == 1) {
    auto x = args[0];
    if (name == "cvt_bool_bv.0") {
      require(x->getType()->getIntegerBitWidth() == 1, "size mismatch in cvt bv bool");
      return x;

    } else if (name == "cvt_bits_uint.0") {
      return x;

    } else if (name == "not_bits.0" || name == "not_bool.0") {
      return static_cast<expr_t>(iface.createNot(x));
    } 
  } else if (args.size() == 2) {
    auto x = args[0], y = args[1];
    if (name == "SignExtend.0" && targs.size() == 2) {
      type_t finalty = llvm::Type::getIntNTy(context, targs[1]);
      require_(finalty != x->getType()); // it is undef to sext to the same size
      return static_cast<expr_t>(iface.createSExt(x, finalty));

    } else if (name == "ZeroExtend.0" && targs.size() == 2) {
      type_t finalty = llvm::Type::getIntNTy(context, targs[1]);
      require_(finalty != x->getType());
      return static_cast<expr_t>(iface.createZExt(x, finalty));

    } else if (name == "cvt_int_bits.0") {
      /* Needed to convert loop index into bv */
      type_t finalty = llvm::Type::getIntNTy(context, targs[0]);
      return static_cast<expr_t>(iface.createTrunc(x, finalty));

    } else if (name == "eq_bits.0") {
      return static_cast<expr_t>(iface.createICmp(llvm::CmpInst::Predicate::ICMP_EQ, x, y));

    } else if (name == "ne_bits.0") {
      return static_cast<expr_t>(iface.createICmp(llvm::CmpInst::Predicate::ICMP_NE, x, y));

    } else if (name == "add_bits.0") {
      return static_cast<expr_t>(iface.createAdd(x, y));

    } else if (name == "sub_bits.0") {
      return static_cast<expr_t>(iface.createSub(x, y));

    } else if (name == "eor_bits.0") {
      return static_cast<expr_t>(iface.createBinop(x, y, llvm::BinaryOperator::BinaryOps::Xor));

    } else if (name == "and_bits.0" || name == "and_bool.0") {
      return static_cast<expr_t>(iface.createAnd(x, y));

    } else if (name == "or_bits.0" || name == "or_bool.0") {
      return static_cast<expr_t>(iface.createOr(x, y));

    } else if (name == "mul_bits.0") {
      return static_cast<expr_t>(iface.createMul(x, y));

    } else if (name == "sdiv_bits.0") {
      unsigned long wd = x->getType()->getIntegerBitWidth();
      auto result = iface.createAlloca(x->getType(), iface.getIntConst(1, 64), "sdiv_result");

      auto overflowing = iface.createAnd(
          iface.createICmp(llvm::ICmpInst::ICMP_EQ, x, llvm::ConstantInt::get(context, llvm::APInt::getSignedMinValue(wd))),
          iface.createICmp(llvm::ICmpInst::ICMP_EQ, y, llvm::ConstantInt::get(context, --llvm::APInt::getZero(wd))));

      auto oldbb = iface.get_bb();
      auto overflowbb = new_stmt("sdiv_overflowing");
      auto okbb = new_stmt("sdiv_non_overflowing");
      auto contbb = new_stmt("sdiv_continuation");

      iface.set_bb(oldbb);
      iface.createBranch(overflowing, overflowbb, okbb);

      iface.set_bb(overflowbb.second);
      iface.createStore(iface.getIntConst(0, wd), result);
      link(overflowbb, contbb);

      iface.set_bb(okbb.second);
      iface.createStore(iface.createSDiv(x, y), result);
      link(okbb, contbb);

      iface.set_bb(contbb.first);
      return static_cast<expr_t>(iface.createLoad(x->getType(), result));

    } else if (name == "slt_bits.0") {
      return static_cast<expr_t>(iface.createICmp(llvm::ICmpInst::Predicate::ICMP_SLT, x, y));

    } else if (name == "sle_bits.0") {
      return static_cast<expr_t>(iface.createICmp(llvm::ICmpInst::Predicate::ICMP_SLE, x, y));

    } else if (name == "lsl_bits.0") {
      std::tie(x, y) = unify_sizes(x, y);
      auto wd = x->getType()->getIntegerBitWidth();
      if (std::has_single_bit(wd))
        return static_cast<expr_t>(iface.createMaskedShl(x, y));
      auto max = iface.getIntConst(wd - 1, wd);
      auto ok = iface.createICmp(llvm::ICmpInst::Predicate::ICMP_ULE, y, max);
      auto shift = iface.createRawShl(x, y);
      auto zero = iface.getIntConst(0, wd);
      return static_cast<expr_t>(iface.createSelect(ok, shift, zero));

    } else if (name == "lsr_bits.0") {
      std::tie(x, y) = unify_sizes(x, y);
      auto wd = x->getType()->getIntegerBitWidth();
      if (std::has_single_bit(wd))
        return static_cast<expr_t>(iface.createMaskedLShr(x, y));
      auto max = iface.getIntConst(wd - 1, wd);
      auto ok = iface.createICmp(llvm::ICmpInst::Predicate::ICMP_ULE, y, max);
      auto shift = iface.createRawLShr(x, y);
      auto zero = iface.getIntConst(0, wd);
      return static_cast<expr_t>(iface.createSelect(ok, shift, zero));

    } else if (name == "asr_bits.0") {
      std::tie(x, y) = unify_sizes(x, y);
      auto wd = x->getType()->getIntegerBitWidth();
      if (std::has_single_bit(wd))
        return static_cast<expr_t>(iface.createMaskedAShr(x, y));
      auto max = iface.getIntConst(wd - 1, wd);
      auto ok = iface.createICmp(llvm::ICmpInst::Predicate::ICMP_ULE, y, max);
      auto shift = iface.createRawAShr(x, y);
      auto zero = iface.getIntConst(0, wd);
      return static_cast<expr_t>(iface.createSelect(ok, shift, zero));

    } else if (name == "append_bits.0") {
      auto upper = x, lower = y;

      auto upperwd = upper->getType()->getIntegerBitWidth();
      auto lowerwd = lower->getType()->getIntegerBitWidth();
      require_(upperwd > 0);
      require_(lowerwd > 0);

      if (upperwd == lowerwd) {
        auto valueToStore = iface.getUndefVec(2, upperwd);
        valueToStore = iface.createInsertElement(valueToStore, x, 1);
        valueToStore = iface.createInsertElement(valueToStore, y, 0);
        return coerce(valueToStore, iface.getIntTy(upperwd * 2));
      }

      auto finalty = iface.getIntTy(upperwd + lowerwd);

      upper = iface.createZExt(upper, finalty);
      upper = iface.createRawShl(upper, iface.getIntConst(lowerwd, upperwd + lowerwd));

      lower = iface.createZExt(lower, finalty);

      return static_cast<expr_t>(iface.createOr(upper, lower));

    } else if (name == "replicate_bits.0") {
      auto count = llvm::cast<llvm::ConstantInt>(y)->getSExtValue();
      auto basewd = x->getType()->getIntegerBitWidth() ;
      auto valueToStore = iface.getUndefVec(count, basewd);
      for (unsigned i = 0; i < count; ++i) {
        valueToStore = iface.createInsertElement(valueToStore, x, i);
      }
      return coerce(valueToStore, iface.getIntTy( basewd * count));

    } else if (name == "select_vec.0") {
      // bits(W * N) select_vec (M, N, W) (bits(W * M) x, bits(32 * N) sel)
      auto x_vector = coerce(args[0], iface.getVecTy(targs[2], targs[0]));
      auto count = targs[1];
      std::vector<int> mask(count);
      auto sel = llvm::cast<llvm::ConstantInt>(y)->getValue();
      for (unsigned i = 0; i < count; ++i) {
        mask.at(i) = sel.extractBitsAsZExtValue(32, i * 32);
      }
      auto res = iface.createShuffleVector(x_vector, {mask}) ;
      return coerce(res, iface.getIntTy(count * targs[2]));

    } else if (name == "reduce_add.0") {
      auto x_vector = coerce(args[0], iface.getVecTy(targs[1], targs[0]));
      auto sum = iface.createVectorReduceAdd(x_vector);
      return iface.createAdd(sum, args[1]);

    }

  } else if (args.size() == 3) {
    auto x = args[0], y = args[1], z = args[2];
    (void)z;

    if (name == "Mem.read.0") {
      // Perform a read of 'size' bytes. The access byte order is reversed for a big-endian access.
      // Instruction fetches would call AArch64.MemSingle directly.
      // bits(size*8) Mem[bits(64) address, integer size, AccType acctype]

      auto size = llvm::cast<llvm::ConstantInt>(y);
      auto [ptr, offset] = ptr_expr(x);
      auto load = iface.makeLoadWithOffset(ptr, offset, size->getSExtValue());
      return static_cast<expr_t>(load);

    } else if (name == "Elem.read.0") {
      // bits(size) Elem[bits(N) vector, integer e, integer size]
      auto elem_size = llvm::cast<llvm::ConstantInt>(z)->getSExtValue();
      auto vec_size = x->getType()->getIntegerBitWidth();
      auto elems = vec_size / elem_size;
      auto vector = coerce(x, iface.getVecTy(elem_size, elems));
      return iface.createExtractElement(vector, y);

    } else if (name == "add_vec.0") {
      // bits(W * N) add_vec (N, W) (bits(W * N) x, bits(W * N) y, integer N)
      auto x_vector = coerce(x, iface.getVecTy(targs[1], targs[0]));
      auto y_vector = coerce(y, iface.getVecTy(targs[1], targs[0]));
      auto cast = iface.createAdd(x_vector, y_vector);
      return coerce(cast, x->getType());

    } else if (name == "sub_vec.0") {
      // bits(W * N) sub_vec (N, W) (bits(W * N) x, bits(W * N) y, integer N)
      auto x_vector = coerce(x, iface.getVecTy(targs[1], targs[0]));
      auto y_vector = coerce(y, iface.getVecTy(targs[1], targs[0]));
      auto cast = iface.createSub(x_vector, y_vector);
      return coerce(cast, x->getType());

    } else if (name == "mul_vec.0") {
      // bits(W * N) mul_vec (N, W) (bits(W * N) x, bits(W * N) y, integer N)
      auto x_vector = coerce(x, iface.getVecTy(targs[1], targs[0]));
      auto y_vector = coerce(y, iface.getVecTy(targs[1], targs[0]));
      auto cast = iface.createMul(x_vector, y_vector);
      return coerce(cast, x->getType());

    } else if (name == "scast_vec.0") {
      // bits(NW * N) scast_vec (N, NW, W) (bits(W * N) x, integer N, integer NW)
      auto x_vector = coerce(x, iface.getVecTy(targs[2], targs[0]));
      auto cast = iface.createSExt(x_vector, iface.getVecTy(targs[1], targs[0]));
      return coerce(cast, iface.getIntTy(targs[1] * targs[0]));

    } else if (name == "zcast_vec.0") {
      // bits(NW * N) zcast_vec (N, NW, W) (bits(W * N) x, integer N, integer NW)
      auto x_vector = coerce(x, iface.getVecTy(targs[2], targs[0]));
      auto cast = iface.createZExt(x_vector, iface.getVecTy(targs[1], targs[0]));
      return coerce(cast, iface.getIntTy(targs[1] * targs[0]));

    } else if (name == "trunc_vec.0") {
      // bits(NW * N) trunc_vec (N, NW, W) (bits(W * N) x, integer N, integer NW)
      auto x_vector = coerce(args[0], iface.getVecTy(targs[2], targs[0]));
      auto trunced = iface.createTrunc(x_vector, iface.getVecTy(targs[1], targs[0]));
      return coerce(trunced, iface.getIntTy(targs[1] * targs[0]));

    // TODO: This is not sound due to large shifts
    } else if (name == "asr_vec.0") {
      // bits(W * N) asr_vec (N, W) (bits(W * N) x, bits(W * N) y, integer N)
      auto x_vector = coerce(x, iface.getVecTy(targs[1], targs[0]));
      auto y_vector = coerce(y, iface.getVecTy(targs[1], targs[0]));
      auto shift = iface.createRawAShr(x_vector, y_vector);
      return coerce(shift, iface.getIntTy(targs[1] * targs[0]));

    // TODO: This is not sound due to large shifts
    } else if (name == "lsr_vec.0") {
      // bits(W * N) lsr_vec (N, W) (bits(W * N) x, bits(W * N) y, integer N)
      auto x_vector = coerce(x, iface.getVecTy(targs[1], targs[0]));
      auto y_vector = coerce(y, iface.getVecTy(targs[1], targs[0]));
      auto shift = iface.createRawLShr(x_vector, y_vector);
      return coerce(shift, iface.getIntTy(targs[1] * targs[0]));

    // TODO: This is not sound due to large shifts
    } else if (name == "lsl_vec.0") {
      // bits(W * N) lsr_vec (N, W) (bits(W * N) x, bits(W * N) y, integer N)
      auto x_vector = coerce(x, iface.getVecTy(targs[1], targs[0]));
      auto y_vector = coerce(y, iface.getVecTy(targs[1], targs[0]));
      auto shift = iface.createRawShl(x_vector, y_vector);
      return coerce(shift, iface.getIntTy(targs[1] * targs[0]));

    } else if (name == "sle_vec.0") {
      // bits(N) sle_vec (N, W) (bits(W * N) x, bits(W * N) y, integer N)
      auto x_vector = coerce(x, iface.getVecTy(targs[1], targs[0]));
      auto y_vector = coerce(y, iface.getVecTy(targs[1], targs[0]));
      auto cmp = iface.createICmp(llvm::ICmpInst::Predicate::ICMP_SLE, x_vector, y_vector);
      return coerce(cmp, iface.getIntTy(targs[0]));

    } else if (name == "slt_vec.0") {
      // bits(N) slt_vec (N, W) (bits(W * N) x, bits(W * N) y, integer N)
      auto x_vector = coerce(x, iface.getVecTy(targs[1], targs[0]));
      auto y_vector = coerce(y, iface.getVecTy(targs[1], targs[0]));
      auto cmp = iface.createICmp(llvm::ICmpInst::Predicate::ICMP_SLT, x_vector, y_vector);
      return coerce(cmp, iface.getIntTy(targs[0]));

    } else if (name == "eq_vec.0") {
      // bits(N) eq_vec (N, W) (bits(W * N) x, bits(W * N) y, integer N)
      auto x_vector = coerce(x, iface.getVecTy(targs[1], targs[0]));
      auto y_vector = coerce(y, iface.getVecTy(targs[1], targs[0]));
      auto cmp = iface.createICmp(llvm::ICmpInst::Predicate::ICMP_EQ, x_vector, y_vector);
      return coerce(cmp, iface.getIntTy(targs[0]));

    } else if (name == "shuffle_vec.0") {
      // bits(W * N) shuffle_vec (M, N, W) (bits(W * M) x, bits(W * M) y, bits(32 * N) sel)
      auto x_vector = coerce(args[0], iface.getVecTy(targs[2], targs[0]));
      auto y_vector = coerce(args[1], iface.getVecTy(targs[2], targs[0]));
      auto count = targs[1];
      std::vector<int> mask(count);
      auto sel = llvm::cast<llvm::ConstantInt>(z)->getValue();
      for (unsigned i = 0; i < count; ++i) {
        mask.at(i) = sel.extractBitsAsZExtValue(32, i * 32);
      }
      auto res = iface.createShuffleVector(y_vector, x_vector, {mask}) ;
      return coerce(res, iface.getIntTy(count * targs[2]));

    } else if ((name == "FPAdd.0" || name == "FPSub.0" || name == "FPMul.0") && args.size() == 3) {
      // bits(N) FPAdd(bits(N) op1, bits(N) op2, FPCRType fpcr)
      x = coerce(x, iface.getFPType(x->getType()->getIntegerBitWidth()));
      y = coerce(y, iface.getFPType(y->getType()->getIntegerBitWidth()));

      auto op = llvm::BinaryOperator::BinaryOps::BinaryOpsEnd;
      if (name == "FPAdd.0")
        op = llvm::BinaryOperator::BinaryOps::FAdd;
      else if (name == "FPSub.0")
        op = llvm::BinaryOperator::BinaryOps::FSub;
      else if (name == "FPMul.0")
        op = llvm::BinaryOperator::BinaryOps::FMul;
      return static_cast<expr_t>(iface.createBinop(x, y, op));

    } else if (name == "ite.0") {
      // bits(W) ite_vec ( W ) (bits(1) c, bits(W) x, bits(W) y)
      return iface.createSelect(args[0], args[1], args[2]);

    }

  } else if (args.size() == 4) {
    if (name == "Elem.set.0") {
      // bits(N) Elem[bits(N) vector, integer e, integer size, bits(size) value]
      auto bv_size = args[0]->getType()->getIntegerBitWidth();
      auto elem_size = llvm::cast<llvm::ConstantInt>(args[2])->getSExtValue();
      auto elems = bv_size / elem_size;
      auto vector = coerce(args[0], iface.getVecTy(elem_size, elems));
      auto insert = iface.createInsertElement(vector, args[3], args[1]);
      return coerce(insert, iface.getIntTy(bv_size));

    } else if (name == "ite_vec.0") {
      // bits(W * N) ite_vec ( N , W ) (bits(N) c, bits(W * N) x, bits(W * N) y, integer N)
      auto cond = coerce(args[0], iface.getVecTy(1, targs[0]));
      auto x_vec = coerce(args[1], iface.getVecTy(targs[1], targs[0]));
      auto y_vec = coerce(args[2], iface.getVecTy(targs[1], targs[0]));
      auto res = iface.createSelect(cond, x_vec, y_vec);
      return coerce(res, iface.getIntTy(targs[1] * targs[0]));

    } else if (name == "FPCompare.0") {
      // bits(4) FPCompare(bits(N) op1, bits(N) op2, boolean signal_nans, FPCRType fpcr)
      
      // return value:
      // PSTATE . V = result [ 0 +: 1 ] ;
      // PSTATE . C = result [ 1 +: 1 ] ;
      // PSTATE . Z = result [ 2 +: 1 ] ;
      // PSTATE . N = result [ 3 +: 1 ] ;

      auto a = args[0], b = args[1], signalnan = args[2], fpcr = args[3];
      (void)fpcr;

      // XXX we do not support signalling nans
      iface.assertTrue(iface.createICmp(llvm::ICmpInst::Predicate::ICMP_EQ, signalnan, llvm::ConstantInt::get(signalnan->getType(), 0)));

      a = coerce(a, iface.getFPType(a->getType()->getIntegerBitWidth()));
      b = coerce(b, iface.getFPType(b->getType()->getIntegerBitWidth()));

      expr_t ret = iface.getIntConst(0, 4);
      auto set = [&](unsigned shift, llvm::Value* cond) {

        require(shift < 4, "");
        auto flag = iface.createSelect(
            cond, iface.getIntConst(1U << shift, 4), iface.getIntConst(0, 4));
        ret = iface.createOr(ret, flag);

      };

      set(3, iface.createFCmp(llvm::FCmpInst::Predicate::FCMP_OLT, a, b));
      set(2, iface.createFCmp(llvm::FCmpInst::Predicate::FCMP_OEQ, a, b));
      set(1, iface.createFCmp(llvm::FCmpInst::Predicate::FCMP_UGT, a, b));
      set(0, iface.createFCmp(llvm::FCmpInst::Predicate::FCMP_UNO, a, b));
      return ret;
    }

  } else if (args.size() == 5) {
    if (name == "FixedToFP.0" && targs.size() == 2) {
      // bits(N) FixedToFP(bits(M) op, integer fbits, boolean unsigned, FPCRType fpcr, FPRounding rounding)
      auto wdin = targs[0], wdout = targs[1];
      (void)wdin;
      auto val = args[0], fbits = args[1], unsign = args[2], fpcr = args[3], rounding = args[4];
      (void)fpcr, (void)rounding;

      // XXX with zero fractional bits, this is a simple cast. otherwise, we would need to consider rounding.
      iface.assertTrue(iface.createICmp(llvm::ICmpInst::Predicate::ICMP_EQ, fbits, llvm::ConstantInt::get(fbits->getType(), 0)));

      // XXX do we need to bitcast back to int? that will be done most of the time when storing in a local.
      return static_cast<expr_t>(
          iface.createSelect(
            unsign,
            iface.createUIToFP(val, iface.getFPType(wdout)),
            iface.createSIToFP(val, iface.getFPType(wdout))));

    } if (name == "FPToFixed.0" && targs.size() == 2) {
      // bits(M) FPToFixed(bits(N) op, integer fbits, boolean unsigned, FPCRType fpcr, FPRounding rounding)
      auto wdout = targs[0], wdin = targs[1];
      auto val = args[0], fbits = args[1], unsign = args[2], fpcr = args[3], rounding = args[4];
      (void)fpcr, (void)rounding;
      // XXX same problems as FixedToFP.
      iface.assertTrue(iface.createICmp(llvm::ICmpInst::Predicate::ICMP_EQ, fbits, llvm::ConstantInt::get(fbits->getType(), 0)));

      val = coerce(val, iface.getFPType(wdin));
      return static_cast<expr_t>(
          iface.createSelect(
            unsign,
            iface.createConvertFPToUI(val, iface.getIntTy(wdout)),
            iface.createConvertFPToSI(val, iface.getIntTy(wdout))));
    }
  }

  die("unsupported TAPPLY: " + name);
}

std::any aslt_visitor::visitExprSlices(SemanticsParser::ExprSlicesContext *ctx) {
  log() << "visitExprSlices" << '\n';
  auto base = expr(ctx->expr());
  auto sl = slice(ctx->slice_expr());
  // a slice is done by right shifting by the "low" value,
  // then, truncating to the "width" value
  auto lo = llvm::ConstantInt::get(base->getType(), sl.lo);
  auto wdty = llvm::Type::getIntNTy(context, sl.wd);
  auto vec_size = base->getType()->getIntegerBitWidth();


  if (lo->isZeroValue()) {
    // Just trunc
    auto trunced = iface.createTrunc(base, wdty);
    return static_cast<expr_t>(trunced);
  } else if (sl.lo % sl.wd == 0 && vec_size % sl.wd == 0) {
    // Vector access
    auto elem_size = sl.wd;
    auto elems = vec_size / elem_size;
    auto vector = coerce(base, iface.getVecTy(elem_size, elems));
    auto pos = llvm::ConstantInt::get(base->getType(), sl.lo / sl.wd);
    return iface.createExtractElement(vector, pos);
  } else {
    // raw shift ok, since slice must be within bounds.
    auto shifted = !lo->isZeroValue() ? iface.createRawLShr(base, lo) : base;
    auto trunced = iface.createTrunc(shifted, wdty);
    return static_cast<expr_t>(trunced);
  }
}

std::any aslt_visitor::visitExprField(SemanticsParser::ExprFieldContext *ctx) {
  log() << "visitExprField" << '\n';

  const static std::map<uint8_t, pstate_t> pstate_map{
    {'N', pstate_t::N}, {'Z', pstate_t::Z}, {'C', pstate_t::C}, {'V', pstate_t::V}, 
  };

  auto base = expr_var(ctx->expr());
  auto field = ident(ctx->ident());
  if (base == pstate_sentinel) {
    require(field.length() == 1, "pstate field name length unexpect");
    auto c = field.at(0);
    auto reg = iface.get_reg(reg_t::PSTATE, (unsigned)pstate_map.at(c));
    auto load = iface.createLoad(reg->getAllocatedType(), reg);
    return static_cast<expr_t>(load);
  }

  die("expr field unsup", base);
}

std::any aslt_visitor::visitExprArray(SemanticsParser::ExprArrayContext *ctx) {
  // an array read is used for registers
  log() << "visitExprArray" << '\n';
  auto base = expr_var(ctx->base);

  require(ctx->indices.size() == 1, "array access has multiple indices");
  auto index = lit_int(ctx->indices.at(0));

  lexpr_t reg = nullptr;
  if (base == xreg_sentinel) {
    reg = iface.get_reg(reg_t::X, index);
  } else if (base == vreg_sentinel) {
    reg = iface.get_reg(reg_t::V, index);
  }
  require(reg, "expr base array unsup");

  auto load = iface.createLoad(reg->getAllocatedType(), reg);
  return static_cast<expr_t>(load);
}
std::any aslt_visitor::visitExprLitInt(SemanticsParser::ExprLitIntContext *ctx) {
  log() << "visitExprLitInt >" << ctx->getText() << "<\n";
  // XXX: it is an error for LitInt to appear outside of types.
  auto node = integer(ctx->integer());
  return static_cast<expr_t>(iface.getIntConst(node, 100));
}

// std::any aslt_visitor::visitExprLitHex(SemanticsParser::ExprLitHexContext *ctx) {
//   log() << "TODO visitExprLitHex" << '\n';
//   assert(0);
//   return super::visitExprLitHex(ctx);
// }

std::any aslt_visitor::visitExprLitBits(SemanticsParser::ExprLitBitsContext *ctx) {
  log() << "visitExprLitBits" << '\n';

  std::string s = std::any_cast<std::string>(visitBits(ctx->bits()));

  auto ty = llvm::Type::getIntNTy(context, s.length());
  return static_cast<expr_t>(llvm::ConstantInt::get(ty, s, 2));
}

std::any aslt_visitor::visitExprParen(aslt::SemanticsParser::ExprParenContext *ctx) {
  return expr(ctx->expr());
}

// std::any aslt_visitor::visitExprLitMask(SemanticsParser::ExprLitMaskContext *ctx) {
//   log() << "TODO visitExprLitMask" << '\n';
//   return super::visitExprLitMask(ctx);
// }

// std::any aslt_visitor::visitExprLitString(SemanticsParser::ExprLitStringContext *ctx) {
//   log() << "TODO visitExprLitString" << '\n';
//   return super::visitExprLitString(ctx);
// }

std::any aslt_visitor::visitTargs(SemanticsParser::TargsContext *ctx) {
  // log() << "visitTargs" << '\n';
  return visitChildren(ctx->expr());
}

std::any aslt_visitor::visitSlice_expr(SemanticsParser::Slice_exprContext *ctx) {
  log() << "visitSlice_expr" << '\n';
  auto exprs = map(ctx->expr(), &aslt_visitor::lit_int);
  require(exprs.size() == 2, "surely not...!");
  return lifter_interface_llvm::slice_t(exprs.at(0), exprs.at(1)); // implicit integer cast
}

// std::any aslt_visitor::visitUuid(SemanticsParser::UuidContext *ctx) {
//   log() << "TODO visitUuid" << '\n';
//   return super::visitUuid(ctx);
// }

std::any aslt_visitor::visitInteger(aslt::SemanticsParser::IntegerContext *ctx) {
  auto s = ctx->DECIMAL()->getText();
  return (int64_t)std::stoll(s);
}

std::any aslt_visitor::visitBits(aslt::SemanticsParser::BitsContext *ctx) {
  auto isbit = [](char x) { return x == '1' || x == '0'; };
  auto filtered = ctx->BINARY()->getText() | std::ranges::views::filter(isbit);

  std::string s;
  std::ranges::copy(filtered, std::back_inserter(s));

  return static_cast<std::string>(s);
}

std::any aslt_visitor::visitIdent(aslt::SemanticsParser::IdentContext *ctx) {
  return static_cast<std::string>(ctx->ID()->getText());
}


}
