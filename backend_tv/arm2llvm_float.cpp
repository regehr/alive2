#include "backend_tv/arm2llvm.h"

#include "Target/AArch64/MCTargetDesc/AArch64MCAsmInfo.h"

using namespace lifter;
using namespace llvm;

void arm2llvm::lift_fccmp() {
  auto operandSize = getRegSize(CurInst->getOperand(0).getReg());
  auto a = readFromFPOperand(0, operandSize);
  auto b = readFromFPOperand(1, operandSize);

  auto [imm_n, imm_z, imm_c, imm_v] = splitImmNZCV(getImm(2));
  auto [n, z, c, v] = FPCompare(a, b);

  auto cond = conditionHolds(getImm(3));

  auto new_n = createSelect(cond, n, imm_n);
  auto new_z = createSelect(cond, z, imm_z);
  auto new_c = createSelect(cond, c, imm_c);
  auto new_v = createSelect(cond, v, imm_v);

  setN(new_n);
  setZ(new_z);
  setC(new_c);
  setV(new_v);
}

void arm2llvm::lift_fcmp(unsigned opcode) {
  auto operandSize = getRegSize(CurInst->getOperand(0).getReg());
  auto a = readFromFPOperand(0, operandSize);
  Value *b;
  if (opcode == AArch64::FCMPSri || opcode == AArch64::FCMPDri ||
      opcode == AArch64::FCMPESri || opcode == AArch64::FCMPEDri) {
    b = ConstantFP::get(getFPType(operandSize), 0.0);
  } else {
    b = readFromFPOperand(1, operandSize);
  }
  auto [n, z, c, v] = FPCompare(a, b);
  setN(n);
  setZ(z);
  setC(c);
  setV(v);
}

void arm2llvm::lift_fsqrt() {
  auto operandSize = getRegSize(CurInst->getOperand(1).getReg());
  auto a = readFromFPOperand(1, operandSize);
  auto res = createSQRT(a);
  updateOutputReg(res);
}

void arm2llvm::lift_fnm(unsigned opcode) {
  auto &op0 = CurInst->getOperand(0);
  auto &op1 = CurInst->getOperand(1);
  auto &op2 = CurInst->getOperand(2);
  auto &op3 = CurInst->getOperand(3);
  assert(op0.isReg() && op1.isReg() && op2.isReg() && op3.isReg());

  bool negateProduct = false, negateAddend = false;
  switch (opcode) {
  case AArch64::FMSUBSrrr:
  case AArch64::FMSUBDrrr: {
    negateProduct = true;
    break;
  }
  case AArch64::FNMADDSrrr:
  case AArch64::FNMADDDrrr: {
    negateAddend = true;
    negateProduct = true;
    break;
  }
  case AArch64::FNMSUBSrrr:
  case AArch64::FNMSUBDrrr: {
    negateAddend = true;
    break;
  }
  }

  auto a = readFromFPOperand(1, getRegSize(op1.getReg()));
  auto b = readFromFPOperand(2, getRegSize(op2.getReg()));
  auto c = readFromFPOperand(3, getRegSize(op3.getReg()));

  if (negateProduct) {
    a = createFNeg(a);
  }
  if (negateAddend) {
    c = createFNeg(c);
  }

  auto res = createFusedMultiplyAdd(a, b, c);

  updateOutputReg(res);
}

void arm2llvm::lift_fmla_mls(unsigned opcode) {
  auto &op1 = CurInst->getOperand(1);
  auto &op2 = CurInst->getOperand(2);
  auto &op3 = CurInst->getOperand(3);
  auto &op4 = CurInst->getOperand(4);
  assert(op1.isReg() && op2.isReg() && op3.isReg() && op4.isImm());

  bool negateProduct = false;
  switch (opcode) {
  case AArch64::FMLSv1i32_indexed:
  case AArch64::FMLSv1i64_indexed: {
    negateProduct = true;
    break;
  }
  }

  auto eltSize = getRegSize(op2.getReg());
  auto c = readFromFPOperand(1, getRegSize(op1.getReg()));
  auto a = readFromFPOperand(2, eltSize);
  auto b = getIndexedFPElement(op4.getImm(), eltSize, op3.getReg());

  if (negateProduct)
    a = createFNeg(a);
  auto res = createFusedMultiplyAdd(a, b, c);
  updateOutputReg(res);
}

void arm2llvm::lift_fmul_idx(unsigned opcode) {
  auto &op0 = CurInst->getOperand(0);
  auto &op1 = CurInst->getOperand(1);
  auto &op2 = CurInst->getOperand(2);
  auto &op3 = CurInst->getOperand(3);
  assert(op0.isReg() && op1.isReg() && op2.isReg() && op3.isImm());
  Instruction::BinaryOps op;
  switch (opcode) {
  case AArch64::FMULv1i32_indexed:
  case AArch64::FMULv1i64_indexed: {
    op = Instruction::FMul;
    break;
  }
  default: {
    assert(false && "missed a case");
  }
  }

  auto eltSize = getRegSize(op1.getReg());
  auto a = readFromFPOperand(1, eltSize);
  auto b = getIndexedFPElement(op3.getImm(), eltSize, op2.getReg());
  auto res = createBinop(a, b, op);
  updateOutputReg(res);
}

void arm2llvm::lift_fpbinop(unsigned opcode) {
  Instruction::BinaryOps op;
  if (opcode == AArch64::FADDSrr || opcode == AArch64::FADDDrr) {
    op = Instruction::FAdd;
  } else if (opcode == AArch64::FMULSrr || opcode == AArch64::FMULDrr ||
             opcode == AArch64::FNMULSrr || opcode == AArch64::FNMULDrr) {
    op = Instruction::FMul;
  } else if (opcode == AArch64::FDIVSrr || opcode == AArch64::FDIVDrr) {
    op = Instruction::FDiv;
  } else if (opcode == AArch64::FSUBSrr || opcode == AArch64::FSUBDrr) {
    op = Instruction::FSub;
  } else {
    assert(false && "missed a case");
  }

  auto a = readFromFPOperand(1, getRegSize(CurInst->getOperand(1).getReg()));
  auto b = readFromFPOperand(2, getRegSize(CurInst->getOperand(2).getReg()));

  Value *res = createBinop(a, b, op);
  if (opcode == AArch64::FNMULSrr || opcode == AArch64::FNMULDrr) {
    res = createFNeg(res);
  }
  updateOutputReg(res);
}

void arm2llvm::lift_fminmax(unsigned opcode) {
  auto a = readFromFPOperand(1, getRegSize(CurInst->getOperand(1).getReg()));
  auto b = readFromFPOperand(2, getRegSize(CurInst->getOperand(2).getReg()));

  Function *decl{nullptr};
  switch (opcode) {
  case AArch64::FMINSrr:
  case AArch64::FMINDrr:
    decl = Intrinsic::getOrInsertDeclaration(LiftedModule, Intrinsic::minimum,
                                             a->getType());
    break;
  case AArch64::FMAXSrr:
  case AArch64::FMAXDrr:
    decl = Intrinsic::getOrInsertDeclaration(LiftedModule, Intrinsic::maximum,
                                             a->getType());
    break;
  case AArch64::FMINNMSrr:
  case AArch64::FMINNMDrr:
    decl = Intrinsic::getOrInsertDeclaration(LiftedModule, Intrinsic::minnum,
                                             a->getType());
    break;
  case AArch64::FMAXNMSrr:
  case AArch64::FMAXNMDrr:
    decl = Intrinsic::getOrInsertDeclaration(LiftedModule, Intrinsic::maxnum,
                                             a->getType());
    break;
  default:
    assert(false);
  }
  assert(decl);

  Value *res = CallInst::Create(decl, {a, b}, nextName(), LLVMBB);
  updateOutputReg(res);
}

void arm2llvm::lift_fabs() {
  auto a = readFromFPOperand(1, getRegSize(CurInst->getOperand(1).getReg()));
  auto res = createFAbs(a);
  updateOutputReg(res);
}

void arm2llvm::lift_fmov_2(unsigned opcode) {
  auto imm = getImm(1);
  assert(imm <= 256);
  int w = (opcode == AArch64::FMOVSi) ? 32 : 64;
  auto floatVal = getUnsignedIntConst(VFPExpandImm(imm, w), 64);
  updateOutputReg(floatVal);
}

void arm2llvm::lift_fmov_1() {
  auto v = readFromOperand(1);
  updateOutputReg(v);
}

void arm2llvm::lift_cvtf_1(unsigned opcode) {
  auto &op0 = CurInst->getOperand(0);
  auto &op1 = CurInst->getOperand(1);
  assert(op0.isReg() && op1.isReg());

  auto isSigned =
      opcode == AArch64::SCVTFUWSri || opcode == AArch64::SCVTFUWDri ||
      opcode == AArch64::SCVTFUXSri || opcode == AArch64::SCVTFUXDri;

  auto fTy = getFPType(getRegSize(op0.getReg()));
  auto val = readFromOperand(1, getRegSize(op1.getReg()));
  auto converted = isSigned ? createSIToFP(val, fTy) : createUIToFP(val, fTy);

  updateOutputReg(converted);
}

void arm2llvm::lift_frint(unsigned opcode) {
  auto &op1 = CurInst->getOperand(1);
  assert(op1.isReg());
  auto *v = readFromFPOperand(1, getRegSize(op1.getReg()));
  auto md = MDString::get(Ctx, "fpexcept.strict");
  Value *converted{nullptr};
  switch (opcode) {
  case AArch64::FRINTXSr:
  case AArch64::FRINTXDr: {
    converted = createRound(v);
    break;
  }
  case AArch64::FRINTASr:
  case AArch64::FRINTADr: {
    converted = createConstrainedRound(v, md);
    break;
  }
  case AArch64::FRINTMSr:
  case AArch64::FRINTMDr: {
    converted = createConstrainedFloor(v, md);
    break;
  }
  case AArch64::FRINTPSr:
  case AArch64::FRINTPDr: {
    converted = createConstrainedCeil(v, md);
    break;
  }
  default:
    assert(false);
  }
  updateOutputReg(converted);
}

void arm2llvm::lift_fcvt_4() {
  auto &op0 = CurInst->getOperand(0);
  auto &op1 = CurInst->getOperand(1);
  assert(op0.isReg() && op1.isReg());

  auto op0Size = getRegSize(op0.getReg());
  auto op1Size = getRegSize(op1.getReg());

  auto fTy = getFPType(op0Size);
  auto fp_val = readFromFPOperand(1, op1Size);

  auto converted =
      op0Size < op1Size ? createFPTrunc(fp_val, fTy) : createFPExt(fp_val, fTy);

  updateOutputReg(converted);
}

void arm2llvm::lift_fcvt_3() {
  // TODO
  *out << "\nERROR: only float and double supported (not bfloat, half, "
          "fp128, etc.)\n\n";
  exit(-1);
}

void arm2llvm::lift_fcvt_1(unsigned opcode) {
  auto &op0 = CurInst->getOperand(0);
  auto &op1 = CurInst->getOperand(1);
  assert(op0.isReg() && op1.isReg());

  auto isSigned =
      opcode == AArch64::FCVTZSUWHr || opcode == AArch64::FCVTZSUWSr ||
      opcode == AArch64::FCVTZSUWDr || opcode == AArch64::FCVTZSUXHr ||
      opcode == AArch64::FCVTZSUXSr || opcode == AArch64::FCVTZSUXDr;

  auto op0Size = getRegSize(op0.getReg());
  auto op1Size = getRegSize(op1.getReg());

  auto fp_val = readFromFPOperand(1, op1Size);
  auto converted = isSigned ? createFPToSI_sat(fp_val, getIntTy(op0Size))
                            : createFPToUI_sat(fp_val, getIntTy(op0Size));
  updateOutputReg(converted);
}

void arm2llvm::lift_fneg() {
  auto operandSize = getRegSize(CurInst->getOperand(1).getReg());
  auto fVal = readFromFPOperand(1, operandSize);
  auto res = createFNeg(fVal);
  updateOutputReg(res);
}
