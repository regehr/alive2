#include "backend_tv/arm2llvm.h"

#include "Target/AArch64/MCTargetDesc/AArch64MCAsmInfo.h"

using namespace lifter;
using namespace llvm;

void arm2llvm::lift_csinc(unsigned opcode) {
  auto size = getInstSize(opcode);
  assert(CurInst->getOperand(1).isReg() && CurInst->getOperand(2).isReg());
  assert(CurInst->getOperand(3).isImm());

  auto a = readFromOperand(1);
  auto b = readFromOperand(2);

  auto cond_val_imm = getImm(3);
  auto cond_val = conditionHolds(cond_val_imm);

  auto inc = createAdd(b, getUnsignedIntConst(1, size));
  auto sel = createSelect(cond_val, a, inc);

  updateOutputReg(sel);
}

void arm2llvm::lift_csinv_csneg(unsigned opcode) {
  auto size = getInstSize(opcode);
  // csinv dst, a, b, cond
  // if (cond) a else ~b
  assert(CurInst->getNumOperands() == 4); // dst, lhs, rhs, cond
  // TODO decode condition and find the approprate cond val
  assert(CurInst->getOperand(1).isReg() && CurInst->getOperand(2).isReg());
  assert(CurInst->getOperand(3).isImm());

  auto a = readFromOperand(1);
  auto b = readFromOperand(2);
  if (!a || !b)
    visitError();

  auto cond_val_imm = getImm(3);
  auto cond_val = conditionHolds(cond_val_imm);

  auto inverted_b = createNot(b);

  if (opcode == AArch64::CSNEGWr || opcode == AArch64::CSNEGXr) {
    auto negated_b = createAdd(inverted_b, getUnsignedIntConst(1, size));
    auto ret = createSelect(cond_val, a, negated_b);
    updateOutputReg(ret);
  } else {
    auto ret = createSelect(cond_val, a, inverted_b);
    updateOutputReg(ret);
  }
}

void arm2llvm::lift_ccmn() {
  auto a = readFromOperand(0);
  auto b = readFromOperand(1);
  auto nzcv = getImm(2);
  auto cond_val_imm = getImm(3);

  auto zero = getBoolConst(false);
  auto one = getBoolConst(true);

  auto [res, flags] = addWithCarry(a, b, zero);
  auto [n, z, c, v] = flags;

  auto cond = conditionHolds(cond_val_imm);
  setN(createSelect(cond, n, (nzcv & 8) ? one : zero));
  setZ(createSelect(cond, z, (nzcv & 4) ? one : zero));
  setC(createSelect(cond, c, (nzcv & 2) ? one : zero));
  setV(createSelect(cond, v, (nzcv & 1) ? one : zero));
}

void arm2llvm::lift_ccmp(unsigned opcode) {
  assert(CurInst->getNumOperands() == 4);

  auto lhs = readFromOperand(0);
  auto imm_rhs = readFromOperand(1);

  if (!lhs || !imm_rhs)
    visitError();

  auto [imm_n, imm_z, imm_c, imm_v] = splitImmNZCV(getImm(2));

  auto cond_val_imm = getImm(3);
  auto cond_val = conditionHolds(cond_val_imm);

  auto ssub = createSSubOverflow(lhs, imm_rhs);
  auto result = createExtractValue(ssub, {0});
  auto zero_val = getUnsignedIntConst(0, getBitWidth(result));

  auto new_n = createICmp(ICmpInst::Predicate::ICMP_SLT, result, zero_val);
  auto new_z = createICmp(ICmpInst::Predicate::ICMP_EQ, lhs, imm_rhs);
  auto new_c = createICmp(ICmpInst::Predicate::ICMP_UGE, lhs, imm_rhs);
  auto new_v = createExtractValue(ssub, {1});

  auto new_n_flag = createSelect(cond_val, new_n, imm_n);
  auto new_z_flag = createSelect(cond_val, new_z, imm_z);
  auto new_c_flag = createSelect(cond_val, new_c, imm_c);
  auto new_v_flag = createSelect(cond_val, new_v, imm_v);

  setN(new_n_flag);
  setZ(new_z_flag);
  setC(new_c_flag);
  setV(new_v_flag);
}

void arm2llvm::lift_csel(unsigned opcode) {
  assert(CurInst->getNumOperands() == 4); // dst, lhs, rhs, cond
  // TODO decode condition and find the approprate cond val
  assert(CurInst->getOperand(1).isReg() && CurInst->getOperand(2).isReg());
  assert(CurInst->getOperand(3).isImm());

  auto a = readFromOperand(1);
  auto b = readFromOperand(2);
  if (!a || !b)
    visitError();

  auto cond_val_imm = getImm(3);
  auto cond_val = conditionHolds(cond_val_imm);

  auto result = createSelect(cond_val, a, b);
  updateOutputReg(result);
}
