#include "backend_tv/arm2llvm.h"

#include "Target/AArch64/MCTargetDesc/AArch64MCAsmInfo.h"

using namespace std;
using namespace lifter;
using namespace llvm;

void arm2llvm::lift_tbz(unsigned opcode) {
  auto i1 = getIntTy(1);
  auto size = getInstSize(opcode);
  auto operand = readFromOperand(0);
  assert(operand != nullptr && "operand is null");
  auto bit_pos = getImm(1);
  auto shift = createMaskedLShr(operand, getUnsignedIntConst(bit_pos, size));
  auto cond_val = createTrunc(shift, i1);

  auto &jmp_tgt_op = CurInst->getOperand(2);
  assert(jmp_tgt_op.isExpr() && "expected expression");
  assert((jmp_tgt_op.getExpr()->getKind() == MCExpr::ExprKind::SymbolRef) &&
         "expected symbol ref as bcc operand");
  const MCSymbolRefExpr &SRE = cast<MCSymbolRefExpr>(*jmp_tgt_op.getExpr());
  const MCSymbol &Sym = SRE.getSymbol(); // FIXME refactor this into a function
  auto *dst_false = getBBByName(Sym.getName());

  assert(MCBB->getSuccs().size() == 1 || MCBB->getSuccs().size() == 2);

  const string *dst_true_name = nullptr;
  for (auto &succ : MCBB->getSuccs()) {
    if (succ->getName() != Sym.getName()) {
      dst_true_name = &succ->getName();
      break;
    }
  }
  auto *dst_true = getBBByName(dst_true_name ? *dst_true_name : Sym.getName());

  if (opcode == AArch64::TBNZW || opcode == AArch64::TBNZX)
    createBranch(cond_val, dst_false, dst_true);
  else
    createBranch(cond_val, dst_true, dst_false);
}

void arm2llvm::lift_cbnz() {
  auto operand = readFromOperand(0);
  assert(operand != nullptr && "operand is null");
  auto cond_val = createICmp(ICmpInst::Predicate::ICMP_NE, operand,
                             getUnsignedIntConst(0, getBitWidth(operand)));

  auto dst_true = getBB(CurInst->getOperand(1));
  assert(dst_true);
  auto succs = MCBB->getSuccs().size();

  if (succs == 2) {
    const string *dst_false_name = nullptr;
    for (auto &succ : MCBB->getSuccs()) {
      if (succ->getName() != dst_true->getName()) {
        dst_false_name = &succ->getName();
        break;
      }
    }
    assert(dst_false_name != nullptr);
    auto *dst_false = getBBByName(*dst_false_name);
    createBranch(cond_val, dst_true, dst_false);
  } else {
    assert(succs == 1);
  }
}

void arm2llvm::lift_cbz() {
  auto operand = readFromOperand(0);
  assert(operand != nullptr && "operand is null");
  auto cond_val = createICmp(ICmpInst::Predicate::ICMP_EQ, operand,
                             getUnsignedIntConst(0, getBitWidth(operand)));
  auto dst_true = getBB(CurInst->getOperand(1));
  assert(dst_true);
  assert(MCBB->getSuccs().size() == 1 || MCBB->getSuccs().size() == 2);

  BasicBlock *dst_false{nullptr};
  if (MCBB->getSuccs().size() == 1) {
    dst_false = dst_true;
  } else {
    const string *dst_false_name = nullptr;
    for (auto &succ : MCBB->getSuccs()) {
      if (succ->getName() != dst_true->getName()) {
        dst_false_name = &succ->getName();
        break;
      }
    }
    assert(dst_false_name != nullptr);
    dst_false = getBBByName(*dst_false_name);
  }
  createBranch(cond_val, dst_true, dst_false);
}

void arm2llvm::lift_branch() {
  BasicBlock *dst{nullptr};
  // JDR: I don't understand this
  if (CurInst->getOperand(0).isImm()) {
    // handles the case when we add an entry block with no predecessors
    auto &dst_name = Str->MF.BBs[getImm(0)].getName();
    dst = getBBByName(dst_name);
  } else {
    dst = getBB(CurInst->getOperand(0));
  }
  if (dst) {
    createBranch(dst);
  } else {
    // ok, if we don't have a destination block then we left this
    // dangling on purpose, with the assumption that it's a tail
    // call
    doDirectCall();
    doReturn();
  }
}

void arm2llvm::lift_bcc() {
  auto cond_val_imm = getImm(0);
  auto cond_val = conditionHolds(cond_val_imm);

  auto &jmp_tgt_op = CurInst->getOperand(1);
  assert(jmp_tgt_op.isExpr() && "expected expression");
  assert((jmp_tgt_op.getExpr()->getKind() == MCExpr::ExprKind::SymbolRef) &&
         "expected symbol ref as bcc operand");
  const MCSymbolRefExpr &SRE = cast<MCSymbolRefExpr>(*jmp_tgt_op.getExpr());
  const MCSymbol &Sym = SRE.getSymbol();

  auto *dst_true = getBBByName(Sym.getName());

  assert(MCBB->getSuccs().size() == 1 || MCBB->getSuccs().size() == 2);
  const string *dst_false_name = nullptr;
  for (auto &succ : MCBB->getSuccs()) {
    if (succ->getName() != Sym.getName()) {
      dst_false_name = &succ->getName();
      break;
    }
  }
  auto *dst_false =
      getBBByName(dst_false_name ? *dst_false_name : Sym.getName());

  createBranch(cond_val, dst_true, dst_false);
}
