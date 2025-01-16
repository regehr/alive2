#include "aslt_visitor.h"
#include "aslp/interface.h"
#include "tree/TerminalNode.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/MDBuilder.h"

#include <format>
#include <llvm/IR/Constants.h>
#include <llvm/Support/Casting.h>

#include <ostream>
#include <ranges>
#include <sstream>

using namespace aslt;

namespace {
  std::string dump(llvm::Value* val) {
    if (!val) {
      return "(null)";
    }
    std::string s;
    llvm::raw_string_ostream os{s};
    val->print(os, true);
    return s;
  }

  void require(bool cond, const std::string_view& str, llvm::Value* val = nullptr) {
   if (cond)
     return;

   std::string msg{"Aslp assertion failure! "};
   msg += str;
   if (val) {
     std::cerr << '\n' << dump(val) << '\n' << std::endl;
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

  template<typename T>
  [[noreturn]] T undefined(const std::string_view& str, llvm::Value* val = nullptr) {
    require(false, str, val);
    assert(false && "unreachable");
  }
}

#define require_(x) require(x, #x)
#undef assert

namespace aslp {

/**
 * Applies the FPRounding as defined in ASL.
 */
llvm::Value* apply_fprounding(lifter_interface_llvm& iface, llvm::Value* val, llvm::Value* fproundingval, llvm::Value* exactval) {
  auto exactconst = llvm::cast<llvm::ConstantInt>(exactval);
  bool exact = 1 == exactconst->getZExtValue();
  // require(exact, "apply_fprounding only supports exact mode"); // all of the llvm functions may throw on inexact.
  (void)exact;

  auto roundingconst = llvm::dyn_cast<llvm::ConstantInt>(fproundingval);
  if (!roundingconst) {
    // XXX:if this is FPCR-dependent, we should use llvm.experimental.constrained.rint instead, but this is unsupported
    val = iface.createRound(val); // actually rint
  } else {
    uint64_t rounding = roundingconst->getZExtValue();
    if (rounding == 0) {
      die("apply_fprounding tie-to-even unsupported", fproundingval); // tie-to-even
    } else if (rounding == 1) {
      val = iface.createConstrainedCeil(val); // ceiling to posinf
    } else if (rounding == 2) {
      val = iface.createConstrainedFloor(val); // flooring to neginf
    } else if (rounding == 3) {
      die("apply_fprounding towards zero unsupported", fproundingval); // towards zero
    } else if (rounding == 4) {
      val = iface.createConstrainedRound(val); // tie-away
    } else if (rounding == 5) {
      die("apply_fprounding tie-to-odd unsupported", fproundingval); // towards odd
    } else {
      die("unknown constant rounding mode", fproundingval);
    }
  }

  return val;
}

/**
 * Wraps the shift operations to define shifts by >= bitwidth as zero. Supports scalars and vectors.
 */
llvm::Value* safe_shift(lifter_interface_llvm& iface, llvm::Value* x, llvm::Instruction::BinaryOps op, llvm::Value* shift) {
  using llvm::Instruction::BinaryOps::Shl, llvm::Instruction::BinaryOps::AShr, llvm::Instruction::BinaryOps::LShr;

  auto vecty = llvm::dyn_cast<llvm::VectorType>(x->getType());
  auto elemty = vecty ? vecty->getElementType() : x->getType();
  auto elemwd = elemty->getIntegerBitWidth();

  expr_t (lifter_interface_llvm::*createrawshift)(expr_t, expr_t) =
    op == Shl ? &lifter_interface_llvm::createRawShl :
    op == AShr ? &lifter_interface_llvm::createRawAShr :
    op == LShr ? &lifter_interface_llvm::createRawLShr :
    undefined<decltype(createrawshift)>("invalid safe_shift op");

  if (std::has_single_bit(elemwd)) {
    auto mask = llvm::ConstantInt::get(x->getType(), elemwd - 1);
    return (iface.*createrawshift)(x, iface.createAnd(shift, mask));
  }

  auto max = llvm::ConstantInt::get(x->getType(), elemwd - 1);
  auto ok = iface.createICmp(llvm::ICmpInst::Predicate::ICMP_ULE, shift, max);
  auto result = (iface.*createrawshift)(x, shift);
  auto zeros = llvm::ConstantInt::get(x->getType(), 0);
  return static_cast<expr_t>(iface.createSelect(ok, result, zeros));
}

/**
 * Wraps the sdiv operation to define (INT_MIN / -1) as INT_MIN and (x / 0) as zeros. Supports scalars and vectors.
 */
llvm::Value* safe_sdiv(aslt_visitor& vis, llvm::Value* numerator, llvm::Value* denominator) {
  auto& iface = vis.iface;

  auto numty = numerator->getType();
  auto vecty = llvm::dyn_cast<llvm::VectorType>(numty);
  auto elemty = vecty ? vecty->getElementType() : numty;
  auto elemwd = elemty->getIntegerBitWidth();

  auto int_min = llvm::ConstantInt::get(numty, llvm::APInt::getSignedMinValue(elemwd));
  auto minus_one = llvm::ConstantInt::get(numty, --llvm::APInt::getZero(elemwd));
  auto one = llvm::ConstantInt::get(numty, ++llvm::APInt::getZero(elemwd));
  // auto zero = llvm::ConstantInt::get(numty, llvm::APInt::getZero(elemwd));

  auto overflowing = iface.createAnd(
    iface.createICmp(llvm::ICmpInst::ICMP_EQ, numerator, int_min),
    iface.createICmp(llvm::ICmpInst::ICMP_EQ, denominator, minus_one));

  auto vector_reduce_or = vecty ? llvm::Intrinsic::getOrInsertDeclaration(iface.ll_function().getParent(), llvm::Intrinsic::vector_reduce_or, { overflowing->getType() }) : nullptr;
  auto any_overflowing = vecty ? llvm::CallInst::Create(vector_reduce_or, { overflowing }, "", iface.get_bb()) : overflowing;

  // auto divbyzero = iface.createICmp(llvm::ICmpInst::ICMP_EQ, denominator, zero);
  auto oldbb = iface.get_bb();

  auto result = iface.createAlloca(numty, iface.getUnsignedIntConst(1, 64), iface.nextName() + "_sdiv_result");

  auto safe_stmt = vis.new_stmt("sdiv_is_safe");
  auto overflow_stmt = vis.new_stmt("sdiv_is_overflow");
  auto continuation = vis.new_stmt("sdiv_continuation");

  llvm::BranchInst::Create(overflow_stmt.first, safe_stmt.first, any_overflowing, oldbb);

  // if overflowing, replace numerator with int_min and denominator with 1 to force a int_min result.
  iface.set_bb(overflow_stmt.second);
  auto numerator2 = iface.createSelect(overflowing, int_min, numerator);
  auto denominator2 = iface.createSelect(overflowing, one, denominator);

  // numerator = iface.createSelect(divbyzero, zero, numerator);
  // denominator = iface.createSelect(divbyzero, one, denominator);

  iface.createStore(iface.createSDiv(numerator2, denominator2), result);
  vis.link(overflow_stmt, continuation);

  iface.set_bb(safe_stmt.second);
  iface.createStore(iface.createSDiv(numerator, denominator), result);
  vis.link(safe_stmt, continuation);

  iface.set_bb(continuation.second);
  return static_cast<expr_t>(iface.createLoad(numty, result));
}

// coerce the given x into a pointer as best we can by examining its structure, recursively.
// (similar to coerce(), which works on normal scalars and vectors).
expr_t aslt_visitor::ptr_expr(llvm::Value* x) {
  log() << "coercing to pointer: " << dump(x) << std::endl;

  // experimenting: this code does not convert via GEP and assumes dereferenceable.
  // x = new llvm::IntToPtrInst(x, llvm::PointerType::get(context, 0), "", iface.get_bb());
  // llvm::cast<llvm::IntToPtrInst>(x)->setMetadata("dereferenceable",
  //     llvm::MDNode::get(context, {(llvm::ConstantAsMetadata::get(iface.getUnsignedIntConst(99, 64)))}));
  // x->dump();
  // return {x, offset};

  if (x->getType()->isPointerTy()) {
    return x;
  }

  auto Add = llvm::BinaryOperator::BinaryOps::Add;
  if (auto add = llvm::dyn_cast<llvm::BinaryOperator>(x); add && add->getOpcode() == Add) {
    // undo the add instruction into a GEP operation
    auto base = ptr_expr(add->getOperand(0));
    auto offset = add->getOperand(1);
    return iface.createGEP(iface.getIntTy(8), base, {offset}, "");

  } else if (auto load = llvm::dyn_cast<llvm::LoadInst>(x); load) {

    // NOTE: if we loaded from a variable and it has a unique dominating store, then replace it.
    // this happens often, e.g. in aslp-generated CSE constants.
    if (auto alloc = llvm::dyn_cast<llvm::AllocaInst>(load->getPointerOperand()); alloc) {
      // std::cerr << "USES of ALLOC ";
      // alloc->dump();
      // std::cerr << "=";

      llvm::DominatorTree dt{iface.ll_function()};
      llvm::PostDominatorTree postdt{iface.ll_function()};

      llvm::StoreInst* uniqueStore{nullptr};
      for (const auto& x : alloc->uses()) {
        auto user = x.getUser();
        log() << "user: " << dump(user);

        if (llvm::isa<llvm::LoadInst>(user)) continue;

        if (auto store = llvm::dyn_cast<llvm::StoreInst>(user); store) {

          // NOTE: if this is a potential dominating store, record it
          // if it post-dominates the existing recorded store.
          if (dt.dominates(store, load)) {
            if (uniqueStore != nullptr) {
              if (postdt.dominates(store, uniqueStore)) {
                uniqueStore = store;
                continue;
              } else if (postdt.dominates(uniqueStore, store)) {
                continue;
              }
              uniqueStore = nullptr;
              log() << "break, too many stores";
              break;
            }
            uniqueStore = store;
            log() << "set";
            continue;
          }
          // NOTE: if this store occurs after the load, disregard it.
          if (dt.dominates(load, store)) {
            continue;
          }
          log() << "not dominated";
        }
        uniqueStore = nullptr;
        break;
      }

      auto uniqueStoredValue = uniqueStore ? uniqueStore->getValueOperand() : nullptr;
      log() << '\n';
      log() << "unique stored value: " << dump(uniqueStoredValue) << std::endl;
      if (uniqueStoredValue) {
        return ptr_expr(uniqueStoredValue);
      }
    }

  }

  // die("unable to coerce to pointer", x);
  log() << "FALLBACK POINTER coerce of " << dump(x) << '\n';
  return new llvm::IntToPtrInst(x, llvm::PointerType::get(context, 0), iface.nextName(), iface.get_bb());
}

std::pair<llvm::Value*, llvm::Value*> aslt_visitor::unify_sizes(llvm::Value* x1, llvm::Value* x2, unify_mode mode) {
  auto ty1 = x1->getType(), ty2 = x2->getType();
  auto wd1 = ty1->getPrimitiveSizeInBits(), wd2 = ty2->getPrimitiveSizeInBits();

  if (mode == unify_mode::EXACT || mode == unify_mode::EXACT_VECTOR) {
    require(wd1 == wd2, "operands must have same size", x1);
  }

  auto vec1 = llvm::dyn_cast<llvm::VectorType>(ty1), vec2 = llvm::dyn_cast<llvm::VectorType>(ty2);
  if ((vec1 || vec2) && mode == unify_mode::EXACT_VECTOR) {
    if (vec1 && !vec2) {
      x2 = coerce(x2, ty1);

    } else if (!vec1 && vec2) {
      x1 = coerce(x1, ty2);

    } else if (vec1->getScalarSizeInBits() > vec2->getScalarSizeInBits()) {
      x1 = coerce(x1, ty2); // it is probably easier to split vector elements than fuse them

    } else if (vec1->getScalarSizeInBits() < vec2->getScalarSizeInBits()) {
      x2 = coerce(x2, ty1);
    }
    return std::make_pair(x1, x2);
  }

  x1 = coerce_to_int(x1), x2 = coerce_to_int(x2);

  auto sext = &lifter_interface_llvm::createSExt;
  auto zext = &lifter_interface_llvm::createZExt;
  const decltype(sext) extend = mode == unify_mode::SEXT ? sext : zext;

  if (wd1 < wd2) {
    x1 = (iface.*extend)(x1, ty2);
  } else if (wd2 < wd1) {
    x2 = (iface.*extend)(x2, ty1);
  }
  return std::make_pair(x1, x2);
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
    auto stmt2 = stmt(s2);
    stmt2.second = iface.get_bb();
    s = link(s, stmt2);
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
  v->setAlignment(llvm::Align(1));
  iface.createStore(rhs, v);

  require_(!constants.contains(name));
  require_(!locals.contains(name));
  constants.insert({name, rhs});
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
  v->setAlignment(llvm::Align(1));
  iface.createStore(rhs, v);

  require_(!constants.contains(name));
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
    v->setAlignment(llvm::Align(1));
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
  iface.assertTrue(iface.getUnsignedIntConst(0, 1));
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
      auto ptr = ptr_expr(addr);
      auto offset = iface.getUnsignedIntConst(0, 64);
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
  v->setAlignment(llvm::Align(1));
  iface.createStore(start, v);
  add_local(name, v);

  /* Process loop body */
  auto body = std::any_cast<stmt_t>(visitStmts(ctx->stmts()));
  iface.set_bb(body.second); // TODO: Not sure if this is necessary

  /* Increment/Decrement counter */
  auto v2 = iface.createLoad(ty, v);
  auto rhs2 = inc ? iface.createAdd(v2, iface.getUnsignedIntConst(1, width)) : iface.createSub(v2, iface.getUnsignedIntConst(1, width));
  iface.createStore(rhs2, v);

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
  if (constants.contains(name))
    var = constants.at(name);
  else if (locals.contains(name))
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
    return static_cast<expr_t>(iface.getUnsignedIntConst(name == "TRUE", 1));
  else if (name == "FPSR")
    // XXX following classic lifter's ignoring of FPSR
    return static_cast<expr_t>(iface.getUnsignedIntConst(0, 64));
  else if (name == "FPCR")
    // XXX do not support FPCR-dependent behaviour
    return static_cast<expr_t>(llvm::UndefValue::get(iface.getIntTy(32)));
  else
    die("unsupported or undefined variable: " + name);

  if (auto ptr = llvm::dyn_cast<llvm::AllocaInst>(var); ptr) {
    var = iface.createLoad(ptr->getAllocatedType(), ptr);
  }
  return var;
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
      x = coerce_to_int(x);
      type_t finalty = llvm::Type::getIntNTy(context, targs[1]);
      require_(finalty != x->getType()); // it is undef to sext to the same size
      return static_cast<expr_t>(iface.createSExt(x, finalty));

    } else if (name == "ZeroExtend.0" && targs.size() == 2) {
      expr_t zero =
          llvm::Constant::getNullValue(iface.getVecTy(x->getType()->getPrimitiveSizeInBits(), 1));
      expr_t vector = x->getType()->isVectorTy() ? x :
            iface.createInsertElement(zero, coerce_to_int(x), iface.getUnsignedIntConst(0, 2));
      auto vty = llvm::cast<llvm::VectorType>(vector->getType());

      auto elemty = vty->getElementType();
      auto elemwd = elemty->getScalarSizeInBits();
      auto elemcnt = vty->getElementCount().getFixedValue();
      if (targs[1] % elemwd == 0) {
        auto finalelemcnt = targs[1] / elemwd;
        expr_t result = llvm::Constant::getNullValue(llvm::VectorType::get(elemty, finalelemcnt, false));
        for (unsigned i = 0; i < elemcnt; i++) {
          auto ii = iface.getUnsignedIntConst(i, 100);
          auto val = iface.createExtractElement(vector, ii);
          result = iface.createInsertElement(result, val, ii);
        }
        return result;
      }
      type_t finalty = llvm::Type::getIntNTy(context, targs[1]);
      require_(finalty != x->getType());
      return static_cast<expr_t>(iface.createZExt(x, finalty));

    } else if (name == "cvt_int_bits.0") {
      /* Needed to convert loop index into bv */
      type_t finalty = llvm::Type::getIntNTy(context, targs[0]);
      return static_cast<expr_t>(iface.createTrunc(x, finalty));

    } else if (name == "eq_bits.0") {
      std::tie(x, y) = unify_sizes(x, y, unify_mode::EXACT);
      return static_cast<expr_t>(iface.createICmp(llvm::CmpInst::Predicate::ICMP_EQ, x, y));

    } else if (name == "ne_bits.0") {
      std::tie(x, y) = unify_sizes(x, y, unify_mode::EXACT);
      return static_cast<expr_t>(iface.createICmp(llvm::CmpInst::Predicate::ICMP_NE, x, y));

    } else if (name == "add_bits.0") {
      std::tie(x, y) = unify_sizes(x, y, unify_mode::EXACT);
      return static_cast<expr_t>(iface.createAdd(x, y));

    } else if (name == "sub_bits.0") {
      std::tie(x, y) = unify_sizes(x, y, unify_mode::EXACT);
      return static_cast<expr_t>(iface.createSub(x, y));

    } else if (name == "eor_bits.0") {
      std::tie(x, y) = unify_sizes(x, y, unify_mode::EXACT_VECTOR);
      return static_cast<expr_t>(iface.createBinop(x, y, llvm::BinaryOperator::BinaryOps::Xor));

    } else if (name == "and_bits.0" || name == "and_bool.0") {
      std::tie(x, y) = unify_sizes(x, y, unify_mode::EXACT_VECTOR);
      return static_cast<expr_t>(iface.createAnd(x, y));

    } else if (name == "or_bits.0" || name == "or_bool.0") {
      std::tie(x, y) = unify_sizes(x, y, unify_mode::EXACT_VECTOR);
      return static_cast<expr_t>(iface.createOr(x, y));

    } else if (name == "mul_bits.0") {
      std::tie(x, y) = unify_sizes(x, y, unify_mode::EXACT);
      return static_cast<expr_t>(iface.createMul(x, y));

    } else if (name == "sdiv_bits.0") {
      std::tie(x, y) = unify_sizes(x, y, unify_mode::EXACT);
      return static_cast<expr_t>(safe_sdiv(*this, x, y));

    } else if (name == "slt_bits.0") {
      std::tie(x, y) = unify_sizes(x, y, unify_mode::EXACT);
      return static_cast<expr_t>(iface.createICmp(llvm::ICmpInst::Predicate::ICMP_SLT, x, y));

    } else if (name == "sle_bits.0") {
      std::tie(x, y) = unify_sizes(x, y, unify_mode::EXACT);
      return static_cast<expr_t>(iface.createICmp(llvm::ICmpInst::Predicate::ICMP_SLE, x, y));

    } else if (name == "lsl_bits.0") {
      std::tie(x, y) = unify_sizes(x, y, unify_mode::ZEXT);
      return static_cast<expr_t>(safe_shift(iface, x, llvm::Instruction::BinaryOps::Shl, y));

    } else if (name == "lsr_bits.0") {
      std::tie(x, y) = unify_sizes(x, y, unify_mode::ZEXT);
      return static_cast<expr_t>(safe_shift(iface, x, llvm::Instruction::BinaryOps::LShr, y));

    } else if (name == "asr_bits.0") {
      std::tie(x, y) = unify_sizes(x, y, unify_mode::ZEXT);
      return static_cast<expr_t>(safe_shift(iface, x, llvm::Instruction::BinaryOps::AShr, y));

    } else if (name == "append_bits.0") {
      x = coerce_to_int(x); y = coerce_to_int(y);
      auto upper = x, lower = y;

      auto upperwd = upper->getType()->getIntegerBitWidth();
      auto lowerwd = lower->getType()->getIntegerBitWidth();
      require_(upperwd > 0);
      require_(lowerwd > 0);

      if (upperwd == lowerwd) {
        auto valueToStore = iface.getUndefVec(2, upperwd);
        valueToStore = iface.createInsertElement(valueToStore, x, 1);
        valueToStore = iface.createInsertElement(valueToStore, y, 0);
        return valueToStore;
      }

      auto finalty = iface.getIntTy(upperwd + lowerwd);

      upper = iface.createZExt(upper, finalty);
      upper = iface.createRawShl(upper, iface.getUnsignedIntConst(lowerwd, upperwd + lowerwd));

      lower = iface.createZExt(lower, finalty);

      return static_cast<expr_t>(iface.createOr(upper, lower));

    } else if (name == "replicate_bits.0") {
      auto count = llvm::cast<llvm::ConstantInt>(y)->getSExtValue();
      x = coerce_to_int(x);
      auto basewd = x->getType()->getIntegerBitWidth();
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

    } else if (name == "FPSqrt.0") {
      // bits(N) FPSqrt(bits(N) op, FPCRType fpcr)
      auto x = coerce(args[0], iface.getFPType(args[0]->getType()->getIntegerBitWidth()));
      return iface.createSQRT(x);
    }

  } else if (args.size() == 3) {
    auto x = args[0], y = args[1], z = args[2];
    (void)z;

    if (name == "Mem.read.0") {
      // Perform a read of 'size' bytes. The access byte order is reversed for a big-endian access.
      // Instruction fetches would call AArch64.MemSingle directly.
      // bits(size*8) Mem[bits(64) address, integer size, AccType acctype]

      auto size = llvm::cast<llvm::ConstantInt>(y);
      auto ptr = ptr_expr(x);
      auto offset = iface.getUnsignedIntConst(0, 64);
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
      return cast;

    } else if (name == "sub_vec.0") {
      // bits(W * N) sub_vec (N, W) (bits(W * N) x, bits(W * N) y, integer N)
      auto x_vector = coerce(x, iface.getVecTy(targs[1], targs[0]));
      auto y_vector = coerce(y, iface.getVecTy(targs[1], targs[0]));
      auto cast = iface.createSub(x_vector, y_vector);
      return cast;

    } else if (name == "mul_vec.0") {
      // bits(W * N) mul_vec (N, W) (bits(W * N) x, bits(W * N) y, integer N)
      auto x_vector = coerce(x, iface.getVecTy(targs[1], targs[0]));
      auto y_vector = coerce(y, iface.getVecTy(targs[1], targs[0]));
      auto cast = iface.createMul(x_vector, y_vector);
      return cast;

    } else if (name == "sdiv_vec.0") {
      // bits(W * N) sdiv_vec (N, W) (bits(W * N) x, bits(W * N) y, integer N)
      auto x_vector = coerce(x, iface.getVecTy(targs[1], targs[0]));
      auto y_vector = coerce(y, iface.getVecTy(targs[1], targs[0]));
      auto cast = safe_sdiv(*this, x_vector, y_vector);
      return cast;

    } else if (name == "scast_vec.0") {
      // bits(NW * N) scast_vec (N, NW, W) (bits(W * N) x, integer N, integer NW)
      auto x_vector = coerce(x, iface.getVecTy(targs[2], targs[0]));
      auto cast = iface.createSExt(x_vector, iface.getVecTy(targs[1], targs[0]));
      return cast;

    } else if (name == "zcast_vec.0") {
      // bits(NW * N) zcast_vec (N, NW, W) (bits(W * N) x, integer N, integer NW)
      auto x_vector = coerce(x, iface.getVecTy(targs[2], targs[0]));
      auto cast = iface.createZExt(x_vector, iface.getVecTy(targs[1], targs[0]));
      return cast;

    } else if (name == "trunc_vec.0") {
      // bits(NW * N) trunc_vec (N, NW, W) (bits(W * N) x, integer N, integer NW)
      auto x_vector = coerce(args[0], iface.getVecTy(targs[2], targs[0]));
      auto trunced = iface.createTrunc(x_vector, iface.getVecTy(targs[1], targs[0]));
      return trunced;

    } else if (name == "asr_vec.0") {
      // bits(W * N) asr_vec (N, W) (bits(W * N) x, bits(W * N) y, integer N)
      auto x_vector = coerce(x, iface.getVecTy(targs[1], targs[0]));
      auto y_vector = coerce(y, iface.getVecTy(targs[1], targs[0]));
      return safe_shift(iface, x_vector, llvm::Instruction::BinaryOps::AShr, y_vector);

    } else if (name == "lsr_vec.0") {
      // bits(W * N) lsr_vec (N, W) (bits(W * N) x, bits(W * N) y, integer N)
      auto x_vector = coerce(x, iface.getVecTy(targs[1], targs[0]));
      auto y_vector = coerce(y, iface.getVecTy(targs[1], targs[0]));
      return safe_shift(iface, x_vector, llvm::Instruction::BinaryOps::LShr, y_vector);

    } else if (name == "lsl_vec.0") {
      // bits(W * N) lsr_vec (N, W) (bits(W * N) x, bits(W * N) y, integer N)
      auto x_vector = coerce(x, iface.getVecTy(targs[1], targs[0]));
      auto y_vector = coerce(y, iface.getVecTy(targs[1], targs[0]));
      return safe_shift(iface, x_vector, llvm::Instruction::BinaryOps::Shl, y_vector);

    } else if (name == "sle_vec.0") {
      // bits(N) sle_vec (N, W) (bits(W * N) x, bits(W * N) y, integer N)
      auto x_vector = coerce(x, iface.getVecTy(targs[1], targs[0]));
      auto y_vector = coerce(y, iface.getVecTy(targs[1], targs[0]));
      auto cmp = iface.createICmp(llvm::ICmpInst::Predicate::ICMP_SLE, x_vector, y_vector);
      return cmp;

    } else if (name == "slt_vec.0") {
      // bits(N) slt_vec (N, W) (bits(W * N) x, bits(W * N) y, integer N)
      auto x_vector = coerce(x, iface.getVecTy(targs[1], targs[0]));
      auto y_vector = coerce(y, iface.getVecTy(targs[1], targs[0]));
      auto cmp = iface.createICmp(llvm::ICmpInst::Predicate::ICMP_SLT, x_vector, y_vector);
      return cmp;

    } else if (name == "eq_vec.0") {
      // bits(N) eq_vec (N, W) (bits(W * N) x, bits(W * N) y, integer N)
      auto x_vector = coerce(x, iface.getVecTy(targs[1], targs[0]));
      auto y_vector = coerce(y, iface.getVecTy(targs[1], targs[0]));
      auto cmp = iface.createICmp(llvm::ICmpInst::Predicate::ICMP_EQ, x_vector, y_vector);
      return cmp;

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
      return res;

    } else if (name == "FPAdd.0" || name == "FPSub.0" || name == "FPMul.0" || name == "FPDiv.0") {
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
      else if (name == "FPDiv.0")
        op = llvm::BinaryOperator::BinaryOps::FDiv;
      return static_cast<expr_t>(iface.createBinop(x, y, op));

    } else if (name == "FPCompareEQ.0" || name == "FPCompareGE.0" || name == "FPCompareGT.0") {
      // boolean FPCompareEQ(bits(N) op1, bits(N) op2, FPCRType fpcr)
      x = coerce(x, iface.getFPType(x->getType()->getIntegerBitWidth()));
      y = coerce(y, iface.getFPType(y->getType()->getIntegerBitWidth()));

      auto op = llvm::FCmpInst::Predicate::BAD_FCMP_PREDICATE;
      if (name == "FPCompareEQ.0")
        op = llvm::FCmpInst::Predicate::FCMP_OEQ;
      else if (name == "FPCompareGT.0")
        op = llvm::FCmpInst::Predicate::FCMP_OGT;
      else if (name == "FPCompareGE.0")
        op = llvm::FCmpInst::Predicate::FCMP_OGE;
      return static_cast<expr_t>(iface.createFCmp(op, x, y));

    } else if (name == "FPMax.0" || name == "FPMin.0" || name == "FPMaxNum.0" || name == "FPMinNum.0") {
      // bits(N) FPMax(bits(N) op1, bits(N) op2, FPCRType fpcr)
      // bits(N) FPMaxNum(bits(N) op1, bits(N) op2, FPCRType fpcr)
      x = coerce(x, iface.getFPType(x->getType()->getIntegerBitWidth()));
      y = coerce(y, iface.getFPType(y->getType()->getIntegerBitWidth()));

      auto module = iface.ll_function().getParent();
      auto op = llvm::Intrinsic::num_intrinsics;
      if (name == "FPMax.0")
        op = llvm::Intrinsic::maximum;
      else if (name == "FPMin.0")
        op = llvm::Intrinsic::minimum;
      else if (name == "FPMaxNum.0")
        op = llvm::Intrinsic::maxnum;
      else if (name == "FPMinNum.0")
        op = llvm::Intrinsic::minnum;

      auto decl = llvm::Intrinsic::getOrInsertDeclaration(module, op, x->getType());
      expr_t res = llvm::CallInst::Create(decl, {x, y}, iface.nextName(), iface.get_bb());
      return res;

    } else if (name == "FPConvert.0") {
      // Convert floating point OP with N-bit precision to M-bit precision,
      // with rounding controlled by ROUNDING.
      // This is used by the FP-to-FP conversion instructions and so for
      // half-precision data ignores FZ16, but observes AHP.

      // bits(M) FPConvert(bits(N) op, FPCRType fpcr, FPRounding rounding)
      auto srcwd = x->getType()->getIntegerBitWidth();
      auto dstwd = targs[0];

      x = coerce(x, iface.getFPType(srcwd));
      auto dstty = iface.getFPType(dstwd);

      auto result =
        dstwd < srcwd ? iface.createFPTrunc(x, dstty) :
        dstwd > srcwd ? iface.createFPExt(x, dstty) :
        x;

      return result;

    } else if (name == "ite.0") {
      // bits(W) ite_vec ( W ) (bits(1) c, bits(W) x, bits(W) y)
      return iface.createSelect(args[0], args[1], args[2]);

    }

  } else if (args.size() == 4) {
    if (name == "Elem.set.0") {
      // bits(N) Elem[bits(N) vector, integer e, integer size, bits(size) value]
      expr_t vector;
      if (args[0]->getType()->isVectorTy()) {
        vector = args[0];
      } else {
        auto bv_size = args[0]->getType()->getIntegerBitWidth();
        auto elem_size = llvm::cast<llvm::ConstantInt>(args[2])->getSExtValue();
        auto elems = bv_size / elem_size;
        vector = coerce(args[0], iface.getVecTy(elem_size, elems));
      }
      auto val = coerce(args[3], vector->getType()->getScalarType());
      auto insert = iface.createInsertElement(vector, val, args[1]);
      return insert;

    } else if (name == "ite_vec.0") {
      // bits(W * N) ite_vec ( N , W ) (bits(N) c, bits(W * N) x, bits(W * N) y, integer N)
      auto cond = coerce(args[0], iface.getVecTy(1, targs[0]));
      auto x_vec = coerce(args[1], iface.getVecTy(targs[1], targs[0]));
      auto y_vec = coerce(args[2], iface.getVecTy(targs[1], targs[0]));
      auto res = iface.createSelect(cond, x_vec, y_vec);
      return res;

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
      // iface.assertTrue(iface.createICmp(llvm::ICmpInst::Predicate::ICMP_EQ, signalnan, llvm::ConstantInt::get(signalnan->getType(), 0)));
      (void)signalnan; // XXX: assume non-signalling for now

      a = coerce(a, iface.getFPType(a->getType()->getIntegerBitWidth()));
      b = coerce(b, iface.getFPType(b->getType()->getIntegerBitWidth()));

      auto i1x4 = iface.getVecTy(1, 4);
      expr_t ret = llvm::PoisonValue::get(i1x4);
      ret = iface.createInsertElement(ret, iface.createFCmp(llvm::FCmpInst::Predicate::FCMP_OLT, a, b), 3);
      ret = iface.createInsertElement(ret, iface.createFCmp(llvm::FCmpInst::Predicate::FCMP_OEQ, a, b), 2);
      ret = iface.createInsertElement(ret, iface.createFCmp(llvm::FCmpInst::Predicate::FCMP_UGT, a, b), 1);
      ret = iface.createInsertElement(ret, iface.createFCmp(llvm::FCmpInst::Predicate::FCMP_UNO, a, b), 0);
      return ret;

    } else if (name == "FPMulAdd.0") {
      // bits(N) FPMulAdd(bits(N) addend, bits(N) op1, bits(N) op2, FPCRType fpcr)
      auto addend = args[0], x = args[1], y = args[2], fpcr = args[3];
      (void)fpcr;

      x = coerce(x, iface.getFPType(x->getType()->getIntegerBitWidth()));
      y = coerce(y, iface.getFPType(y->getType()->getIntegerBitWidth()));
      addend = coerce(addend, iface.getFPType(addend->getType()->getIntegerBitWidth()));
      return static_cast<expr_t>(iface.createFusedMultiplyAdd(x, y, addend));

    } else if (name == "FPRoundInt.0") {
      // enumeration FPRounding  {FPRounding_TIEEVEN, FPRounding_POSINF,
      //                          FPRounding_NEGINF,  FPRounding_ZERO,
      //                          FPRounding_TIEAWAY, FPRounding_ODD};

      // FPRoundInt()
      // ============
      // Round OP to nearest integral floating point value using rounding mode ROUNDING.
      // If EXACT is TRUE, set FPSR.IXC if result is not numerically equal to OP.
      //
      // bits(N) FPRoundInt(bits(N) op, FPCRType fpcr, FPRounding rounding, boolean exact)


      auto a = args[0], fpcr = args[1], fprounding = args[2], exact = args[3];
      (void)fpcr;

      a = coerce(a, iface.getFPType(a->getType()->getIntegerBitWidth()));

      return apply_fprounding(iface, a, fprounding, exact);
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
      auto val = args[0], fbits = args[1], unsign = args[2], fpcr = args[3], fprounding = args[4];
      (void)fpcr, (void)fprounding;
      // XXX same problems as FixedToFP.
      iface.assertTrue(iface.createICmp(llvm::ICmpInst::Predicate::ICMP_EQ, fbits, llvm::ConstantInt::get(fbits->getType(), 0)));

      val = coerce(val, iface.getFPType(wdin));

      auto roundingconst = llvm::dyn_cast<llvm::ConstantInt>(fprounding);
      if (roundingconst && roundingconst->getZExtValue() == 3) {
        // rounding towards zero is handled by the FPToUI instructions.
        ;
      } else {
        val = apply_fprounding(iface, val, fprounding, iface.getUnsignedIntConst(1, 1));
      }

      return static_cast<expr_t>(
          iface.createSelect(
            unsign,
            iface.createFPToUI_sat(val, iface.getIntTy(wdout)),
            iface.createFPToSI_sat(val, iface.getIntTy(wdout))));
    }
  }

  std::ostringstream ss;
  ss << "unsupported TAPPLY: " << name << " with " << args.size() << " args, " << targs.size() << " targs";
  die(ss.view());
}

std::any aslt_visitor::visitExprSlices(SemanticsParser::ExprSlicesContext *ctx) {
  log() << "visitExprSlices" << '\n';
  auto base = expr(ctx->expr());
  auto sl = slice(ctx->slice_expr());
  auto vec_size = base->getType()->getPrimitiveSizeInBits();

  if (sl.lo % sl.wd == 0 && vec_size % sl.wd == 0) {
    // Vector access
    auto elem_size = sl.wd;
    auto elems = vec_size / elem_size;
    auto vector = coerce(base, iface.getVecTy(elem_size, elems));
    auto pos = iface.getUnsignedIntConst(sl.lo / sl.wd, 100);
    return iface.createExtractElement(vector, pos);

  } else {
    // fallback to slice of scalar
    base = coerce_to_int(base);

    // a slice is done by right shifting by the "low" value,
    // then, truncating to the "width" value
    auto lo = llvm::ConstantInt::get(base->getType(), sl.lo);
    auto wdty = llvm::Type::getIntNTy(context, sl.wd);
    if (lo->isZeroValue()) {
      // Just trunc
      auto trunced = iface.createTrunc(base, wdty);
      return static_cast<expr_t>(trunced);
    } else {
      // raw shift ok, since slice must be within bounds.
      auto shifted = !lo->isZeroValue() ? iface.createRawLShr(base, lo) : base;
      auto trunced = iface.createTrunc(shifted, wdty);
      return static_cast<expr_t>(trunced);
    }

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
  return static_cast<expr_t>(iface.getUnsignedIntConst(node, 100));
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
