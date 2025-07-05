#include "backend_tv/arm2llvm.h"

#include "Target/AArch64/MCTargetDesc/AArch64MCAsmInfo.h"

#include <vector>

using namespace lifter;
using namespace llvm;
using namespace std;

void arm2llvm::lift_ldp_2(unsigned opcode) {
  auto i64 = getIntTy(64);
  unsigned scale;
  switch (opcode) {
  case AArch64::LDPSWpre:
  case AArch64::LDPWpre:
  case AArch64::LDPSpre:
  case AArch64::LDPSWpost:
  case AArch64::LDPWpost:
  case AArch64::LDPSpost: {
    scale = 2;
    break;
  }
  case AArch64::LDPXpre:
  case AArch64::LDPDpre:
  case AArch64::LDPXpost:
  case AArch64::LDPDpost: {
    scale = 3;
    break;
  }
  case AArch64::LDPQpre:
  case AArch64::LDPQpost: {
    scale = 4;
    break;
  }
  default: {
    *out << "\nError Unknown opcode\n";
    visitError();
  }
  }

  bool sExt = false;
  switch (opcode) {
  case AArch64::LDPSWpre:
  case AArch64::LDPSWpost:
    sExt = true;
    break;
  default:
    sExt = false;
    break;
  }
  unsigned size = pow(2, scale);
  auto &op0 = CurInst->getOperand(0);
  auto &op1 = CurInst->getOperand(1);
  auto &op2 = CurInst->getOperand(2);
  auto &op3 = CurInst->getOperand(3);
  auto &op4 = CurInst->getOperand(4);
  assert(op0.isReg() && op1.isReg() && op2.isReg() && op3.isReg());
  assert(op0.getReg() == op3.getReg());
  assert(op4.isImm());

  auto destReg1 = op1.getReg();
  auto destReg2 = op2.getReg();
  auto baseReg = op3.getReg();
  auto imm = op4.getImm();
  assert((baseReg >= AArch64::X0 && baseReg <= AArch64::X28) ||
         (baseReg == AArch64::SP) || (baseReg == AArch64::LR) ||
         (baseReg == AArch64::FP) || (baseReg == AArch64::XZR));
  auto base = readPtrFromReg(baseReg);
  auto baseAddr = createPtrToInt(base, i64);

  // Start offset as 7-bit signed integer
  assert(imm <= 63 && imm >= -64);
  auto offset = getSignedIntConst(imm, 7);
  Value *offsetVal1 =
      createMaskedShl(createSExt(offset, i64), getUnsignedIntConst(scale, 64));
  Value *offsetVal2 = createAdd(offsetVal1, getUnsignedIntConst(size, 64));

  bool isPre = opcode == AArch64::LDPWpre || opcode == AArch64::LDPSpre ||
               opcode == AArch64::LDPXpre || opcode == AArch64::LDPDpre ||
               opcode == AArch64::LDPQpre;

  Value *loaded1, *loaded2;
  if (isPre) {
    loaded1 = makeLoadWithOffset(base, offsetVal1, size);
    loaded2 = makeLoadWithOffset(base, offsetVal2, size);
  } else {
    loaded1 = makeLoadWithOffset(base, getUnsignedIntConst(0, 64), size);
    loaded2 = makeLoadWithOffset(base, getUnsignedIntConst(size, 64), size);
  }
  updateReg(loaded1, destReg1, sExt);
  updateReg(loaded2, destReg2, sExt);

  auto added = createAdd(baseAddr, offsetVal1);
  updateOutputReg(added);
}

void arm2llvm::lift_ldp_1(unsigned opcode) {
  auto &op0 = CurInst->getOperand(0);
  auto &op1 = CurInst->getOperand(1);
  auto &op2 = CurInst->getOperand(2);
  auto &op3 = CurInst->getOperand(3);
  assert(op0.isReg() && op1.isReg() && op2.isReg());
  assert(op3.isImm());

  auto baseReg = op2.getReg();
  assert((baseReg >= AArch64::X0 && baseReg <= AArch64::X28) ||
         (baseReg == AArch64::SP) || (baseReg == AArch64::LR) ||
         (baseReg == AArch64::XZR) || (baseReg == AArch64::FP));
  auto baseAddr = readPtrFromReg(baseReg);

  int size = 0;
  switch (opcode) {
  case AArch64::LDPSWi:
  case AArch64::LDPWi:
  case AArch64::LDPSi: {
    size = 4;
    break;
  }
  case AArch64::LDPXi:
  case AArch64::LDPDi: {
    size = 8;
    break;
  }
  case AArch64::LDPQi: {
    size = 16;
    break;
  }
  default: {
    *out << "\nError Unknown opcode\n";
    visitError();
  }
  }
  assert(size != 0);

  bool SExt = opcode == AArch64::LDPSWi;

  auto imm = op3.getImm();
  auto out1 = op0.getReg();
  auto out2 = op1.getReg();

  updateReg(makeLoadWithOffset(baseAddr, imm * size, size), out1, SExt);
  updateReg(makeLoadWithOffset(baseAddr, (imm + 1) * size, size), out2, SExt);
}

void arm2llvm::lift_ld2(unsigned opcode) {
  auto i64 = getIntTy(64);
  unsigned numElts, eltSize;
  bool fullWidth;
  switch (opcode) {
  case AArch64::LD1Onev8b:
  case AArch64::LD1Onev8b_POST:
  case AArch64::LD1Twov8b:
  case AArch64::LD1Twov8b_POST:
  case AArch64::LD1Threev8b:
  case AArch64::LD1Threev8b_POST:
  case AArch64::LD1Fourv8b:
  case AArch64::LD1Fourv8b_POST:
  case AArch64::LD2Twov8b:
  case AArch64::LD2Twov8b_POST:
  case AArch64::LD3Threev8b:
  case AArch64::LD3Threev8b_POST:
  case AArch64::LD4Fourv8b:
  case AArch64::LD4Fourv8b_POST:
    numElts = 8;
    eltSize = 8;
    fullWidth = false;
    break;
  case AArch64::LD1Onev16b:
  case AArch64::LD1Onev16b_POST:
  case AArch64::LD1Twov16b:
  case AArch64::LD1Twov16b_POST:
  case AArch64::LD1Threev16b:
  case AArch64::LD1Threev16b_POST:
  case AArch64::LD1Fourv16b:
  case AArch64::LD1Fourv16b_POST:
  case AArch64::LD2Twov16b:
  case AArch64::LD2Twov16b_POST:
  case AArch64::LD3Threev16b:
  case AArch64::LD3Threev16b_POST:
  case AArch64::LD4Fourv16b:
  case AArch64::LD4Fourv16b_POST:
    numElts = 16;
    eltSize = 8;
    fullWidth = true;
    break;
  case AArch64::LD1Onev4h:
  case AArch64::LD1Onev4h_POST:
  case AArch64::LD1Twov4h:
  case AArch64::LD1Twov4h_POST:
  case AArch64::LD1Threev4h:
  case AArch64::LD1Threev4h_POST:
  case AArch64::LD1Fourv4h:
  case AArch64::LD1Fourv4h_POST:
  case AArch64::LD2Twov4h:
  case AArch64::LD2Twov4h_POST:
  case AArch64::LD3Threev4h:
  case AArch64::LD3Threev4h_POST:
  case AArch64::LD4Fourv4h:
  case AArch64::LD4Fourv4h_POST:
    numElts = 4;
    eltSize = 16;
    fullWidth = false;
    break;
  case AArch64::LD1Onev8h:
  case AArch64::LD1Onev8h_POST:
  case AArch64::LD1Twov8h:
  case AArch64::LD1Twov8h_POST:
  case AArch64::LD1Threev8h:
  case AArch64::LD1Threev8h_POST:
  case AArch64::LD1Fourv8h:
  case AArch64::LD1Fourv8h_POST:
  case AArch64::LD2Twov8h:
  case AArch64::LD2Twov8h_POST:
  case AArch64::LD3Threev8h:
  case AArch64::LD3Threev8h_POST:
  case AArch64::LD4Fourv8h:
  case AArch64::LD4Fourv8h_POST:
    numElts = 8;
    eltSize = 16;
    fullWidth = true;
    break;
  case AArch64::LD1Onev2s:
  case AArch64::LD1Onev2s_POST:
  case AArch64::LD1Twov2s:
  case AArch64::LD1Twov2s_POST:
  case AArch64::LD1Threev2s:
  case AArch64::LD1Threev2s_POST:
  case AArch64::LD1Fourv2s:
  case AArch64::LD1Fourv2s_POST:
  case AArch64::LD2Twov2s:
  case AArch64::LD2Twov2s_POST:
  case AArch64::LD3Threev2s:
  case AArch64::LD3Threev2s_POST:
  case AArch64::LD4Fourv2s:
  case AArch64::LD4Fourv2s_POST:
    numElts = 2;
    eltSize = 32;
    fullWidth = false;
    break;
  case AArch64::LD1Onev4s:
  case AArch64::LD1Onev4s_POST:
  case AArch64::LD1Twov4s:
  case AArch64::LD1Twov4s_POST:
  case AArch64::LD1Threev4s:
  case AArch64::LD1Threev4s_POST:
  case AArch64::LD1Fourv4s:
  case AArch64::LD1Fourv4s_POST:
  case AArch64::LD2Twov4s:
  case AArch64::LD2Twov4s_POST:
  case AArch64::LD3Threev4s:
  case AArch64::LD3Threev4s_POST:
  case AArch64::LD4Fourv4s:
  case AArch64::LD4Fourv4s_POST:
    numElts = 4;
    eltSize = 32;
    fullWidth = true;
    break;
  case AArch64::LD1Onev1d:
  case AArch64::LD1Onev1d_POST:
  case AArch64::LD1Twov1d:
  case AArch64::LD1Twov1d_POST:
  case AArch64::LD1Threev1d:
  case AArch64::LD1Threev1d_POST:
  case AArch64::LD1Fourv1d:
  case AArch64::LD1Fourv1d_POST:
    numElts = 1;
    eltSize = 64;
    fullWidth = false;
    break;
  case AArch64::LD1Onev2d:
  case AArch64::LD1Onev2d_POST:
  case AArch64::LD1Twov2d:
  case AArch64::LD1Twov2d_POST:
  case AArch64::LD1Threev2d:
  case AArch64::LD1Threev2d_POST:
  case AArch64::LD1Fourv2d:
  case AArch64::LD1Fourv2d_POST:
  case AArch64::LD2Twov2d:
  case AArch64::LD2Twov2d_POST:
  case AArch64::LD3Threev2d:
  case AArch64::LD3Threev2d_POST:
  case AArch64::LD4Fourv2d:
  case AArch64::LD4Fourv2d_POST:
    numElts = 2;
    eltSize = 64;
    fullWidth = true;
    break;
  default:
    assert(false);
    break;
  }

  unsigned nregs;
  switch (opcode) {
  case AArch64::LD1Onev8b:
  case AArch64::LD1Onev16b:
  case AArch64::LD1Onev4h:
  case AArch64::LD1Onev8h:
  case AArch64::LD1Onev2s:
  case AArch64::LD1Onev4s:
  case AArch64::LD1Onev1d:
  case AArch64::LD1Onev2d:
  case AArch64::LD1Onev8b_POST:
  case AArch64::LD1Onev16b_POST:
  case AArch64::LD1Onev4h_POST:
  case AArch64::LD1Onev8h_POST:
  case AArch64::LD1Onev2s_POST:
  case AArch64::LD1Onev4s_POST:
  case AArch64::LD1Onev1d_POST:
  case AArch64::LD1Onev2d_POST:
    nregs = 1;
    break;
  case AArch64::LD1Twov8b:
  case AArch64::LD1Twov16b:
  case AArch64::LD1Twov4h:
  case AArch64::LD1Twov8h:
  case AArch64::LD1Twov2s:
  case AArch64::LD1Twov4s:
  case AArch64::LD1Twov1d:
  case AArch64::LD1Twov2d:
  case AArch64::LD1Twov8b_POST:
  case AArch64::LD1Twov16b_POST:
  case AArch64::LD1Twov4h_POST:
  case AArch64::LD1Twov8h_POST:
  case AArch64::LD1Twov2s_POST:
  case AArch64::LD1Twov4s_POST:
  case AArch64::LD1Twov1d_POST:
  case AArch64::LD1Twov2d_POST:
  case AArch64::LD2Twov8b:
  case AArch64::LD2Twov16b:
  case AArch64::LD2Twov4h:
  case AArch64::LD2Twov8h:
  case AArch64::LD2Twov2s:
  case AArch64::LD2Twov4s:
  case AArch64::LD2Twov2d:
  case AArch64::LD2Twov8b_POST:
  case AArch64::LD2Twov16b_POST:
  case AArch64::LD2Twov4h_POST:
  case AArch64::LD2Twov8h_POST:
  case AArch64::LD2Twov2s_POST:
  case AArch64::LD2Twov4s_POST:
  case AArch64::LD2Twov2d_POST:
    nregs = 2;
    break;
  case AArch64::LD1Threev8b:
  case AArch64::LD1Threev16b:
  case AArch64::LD1Threev4h:
  case AArch64::LD1Threev8h:
  case AArch64::LD1Threev2s:
  case AArch64::LD1Threev4s:
  case AArch64::LD1Threev1d:
  case AArch64::LD1Threev2d:
  case AArch64::LD1Threev8b_POST:
  case AArch64::LD1Threev16b_POST:
  case AArch64::LD1Threev4h_POST:
  case AArch64::LD1Threev8h_POST:
  case AArch64::LD1Threev2s_POST:
  case AArch64::LD1Threev4s_POST:
  case AArch64::LD1Threev1d_POST:
  case AArch64::LD1Threev2d_POST:
  case AArch64::LD3Threev8b:
  case AArch64::LD3Threev16b:
  case AArch64::LD3Threev4h:
  case AArch64::LD3Threev8h:
  case AArch64::LD3Threev2s:
  case AArch64::LD3Threev4s:
  case AArch64::LD3Threev2d:
  case AArch64::LD3Threev8b_POST:
  case AArch64::LD3Threev16b_POST:
  case AArch64::LD3Threev4h_POST:
  case AArch64::LD3Threev8h_POST:
  case AArch64::LD3Threev2s_POST:
  case AArch64::LD3Threev4s_POST:
  case AArch64::LD3Threev2d_POST:
    nregs = 3;
    break;
  case AArch64::LD1Fourv8b:
  case AArch64::LD1Fourv16b:
  case AArch64::LD1Fourv4h:
  case AArch64::LD1Fourv8h:
  case AArch64::LD1Fourv2s:
  case AArch64::LD1Fourv4s:
  case AArch64::LD1Fourv1d:
  case AArch64::LD1Fourv2d:
  case AArch64::LD1Fourv8b_POST:
  case AArch64::LD1Fourv16b_POST:
  case AArch64::LD1Fourv4h_POST:
  case AArch64::LD1Fourv8h_POST:
  case AArch64::LD1Fourv2s_POST:
  case AArch64::LD1Fourv4s_POST:
  case AArch64::LD1Fourv1d_POST:
  case AArch64::LD1Fourv2d_POST:
  case AArch64::LD4Fourv8b:
  case AArch64::LD4Fourv16b:
  case AArch64::LD4Fourv4h:
  case AArch64::LD4Fourv8h:
  case AArch64::LD4Fourv2s:
  case AArch64::LD4Fourv4s:
  case AArch64::LD4Fourv2d:
  case AArch64::LD4Fourv8b_POST:
  case AArch64::LD4Fourv16b_POST:
  case AArch64::LD4Fourv4h_POST:
  case AArch64::LD4Fourv8h_POST:
  case AArch64::LD4Fourv2s_POST:
  case AArch64::LD4Fourv4s_POST:
  case AArch64::LD4Fourv2d_POST:
    nregs = 4;
    break;
  default:
    assert(false);
    break;
  }
  bool isPost =
      opcode == AArch64::LD1Onev8b_POST || opcode == AArch64::LD1Onev16b_POST ||
      opcode == AArch64::LD1Onev4h_POST || opcode == AArch64::LD1Onev8h_POST ||
      opcode == AArch64::LD1Onev2s_POST || opcode == AArch64::LD1Onev4s_POST ||
      opcode == AArch64::LD1Onev1d_POST || opcode == AArch64::LD1Onev2d_POST ||
      opcode == AArch64::LD1Twov8b_POST || opcode == AArch64::LD1Twov16b_POST ||
      opcode == AArch64::LD1Twov4h_POST || opcode == AArch64::LD1Twov8h_POST ||
      opcode == AArch64::LD1Twov2s_POST || opcode == AArch64::LD1Twov4s_POST ||
      opcode == AArch64::LD1Twov1d_POST || opcode == AArch64::LD1Twov2d_POST ||
      opcode == AArch64::LD1Threev8b_POST ||
      opcode == AArch64::LD1Threev16b_POST ||
      opcode == AArch64::LD1Threev4h_POST ||
      opcode == AArch64::LD1Threev8h_POST ||
      opcode == AArch64::LD1Threev2s_POST ||
      opcode == AArch64::LD1Threev4s_POST ||
      opcode == AArch64::LD1Threev1d_POST ||
      opcode == AArch64::LD1Threev2d_POST ||
      opcode == AArch64::LD1Fourv8b_POST ||
      opcode == AArch64::LD1Fourv16b_POST ||
      opcode == AArch64::LD1Fourv4h_POST ||
      opcode == AArch64::LD1Fourv8h_POST ||
      opcode == AArch64::LD1Fourv2s_POST ||
      opcode == AArch64::LD1Fourv4s_POST ||
      opcode == AArch64::LD1Fourv1d_POST ||
      opcode == AArch64::LD1Fourv2d_POST || opcode == AArch64::LD2Twov8b_POST ||
      opcode == AArch64::LD2Twov16b_POST || opcode == AArch64::LD2Twov4h_POST ||
      opcode == AArch64::LD2Twov8h_POST || opcode == AArch64::LD2Twov2s_POST ||
      opcode == AArch64::LD2Twov4s_POST || opcode == AArch64::LD2Twov2d_POST ||
      opcode == AArch64::LD3Threev8b_POST ||
      opcode == AArch64::LD3Threev16b_POST ||
      opcode == AArch64::LD3Threev4h_POST ||
      opcode == AArch64::LD3Threev8h_POST ||
      opcode == AArch64::LD3Threev2s_POST ||
      opcode == AArch64::LD3Threev4s_POST ||
      opcode == AArch64::LD3Threev2d_POST ||
      opcode == AArch64::LD4Fourv8b_POST ||
      opcode == AArch64::LD4Fourv16b_POST ||
      opcode == AArch64::LD4Fourv4h_POST ||
      opcode == AArch64::LD4Fourv8h_POST ||
      opcode == AArch64::LD4Fourv2s_POST ||
      opcode == AArch64::LD4Fourv4s_POST || opcode == AArch64::LD4Fourv2d_POST;

  auto regCounter = decodeRegSet(CurInst->getOperand(isPost ? 1 : 0).getReg());
  auto baseReg = CurInst->getOperand(isPost ? 2 : 1).getReg();
  assert((baseReg >= AArch64::X0 && baseReg <= AArch64::X28) ||
         (baseReg == AArch64::SP) || (baseReg == AArch64::LR) ||
         (baseReg == AArch64::FP) || (baseReg == AArch64::XZR));
  assert((regCounter >= AArch64::Q0 && regCounter <= AArch64::Q31) ||
         (regCounter >= AArch64::D0 && regCounter <= AArch64::D31));
  Value *offset = nullptr;
  if (isPost) {
    if (CurInst->getOperand(3).isReg() &&
        CurInst->getOperand(3).getReg() != AArch64::XZR) {
      offset = readFromRegOld(CurInst->getOperand(3).getReg());
    } else {
      offset = getUnsignedIntConst(nregs * numElts * (eltSize / 8), 64);
    }
  }

  // Load the entire memory block to be stored in all the registers at once
  // [nregs * numElts * (eltSize / 8)] bytes
  auto base = readPtrFromReg(baseReg);
  auto baseAddr = createPtrToInt(base, i64);
  auto loaded = makeLoadWithOffset(base, 0, nregs * numElts * (eltSize / 8));
  auto casted = createBitCast(loaded, getVecTy(eltSize, nregs * numElts));

  // Deinterleave the loaded vector into nregs vectors and store them in the
  // registers
  vector<int> mask(numElts);
  Value *res;

  // Outer loop control for register to store into
  for (unsigned j = 0; j < nregs; j++) {
    for (unsigned i = 0, maskVal = j; i < numElts; i++, maskVal += nregs) {
      assert(maskVal < nregs * numElts);
      mask[i] = maskVal;
    }
    res = createShuffleVector(casted, mask);
    updateReg(res, regCounter);

    regCounter++;
    if (fullWidth && regCounter > AArch64::Q31)
      regCounter = AArch64::Q0;
    else if (!fullWidth && regCounter > AArch64::D31)
      regCounter = AArch64::D0;
  }

  if (isPost) {
    auto added = createAdd(baseAddr, offset);
    updateOutputReg(added);
  }
}

void arm2llvm::lift_ld1r(unsigned opcode) {
  unsigned numElts, eltSize;
  switch (opcode) {
  case AArch64::LD1Rv8b:
  case AArch64::LD1Rv8b_POST:
    numElts = 8;
    eltSize = 8;
    break;
  case AArch64::LD1Rv16b:
  case AArch64::LD1Rv16b_POST:
    numElts = 16;
    eltSize = 8;
    break;
  case AArch64::LD1Rv4h:
  case AArch64::LD1Rv4h_POST:
    numElts = 4;
    eltSize = 16;
    break;
  case AArch64::LD1Rv8h:
  case AArch64::LD1Rv8h_POST:
    numElts = 8;
    eltSize = 16;
    break;
  case AArch64::LD1Rv2s:
  case AArch64::LD1Rv2s_POST:
    numElts = 2;
    eltSize = 32;
    break;
  case AArch64::LD1Rv4s:
  case AArch64::LD1Rv4s_POST:
    numElts = 4;
    eltSize = 32;
    break;
  case AArch64::LD1Rv1d:
  case AArch64::LD1Rv1d_POST:
    numElts = 1;
    eltSize = 64;
    break;
  case AArch64::LD1Rv2d:
  case AArch64::LD1Rv2d_POST:
    numElts = 2;
    eltSize = 64;
    break;
  default:
    *out << "\nError Unknown opcode\n";
    visitError();
    break;
  }

  bool isPost = false;
  switch (opcode) {
  case AArch64::LD1Rv8b_POST:
  case AArch64::LD1Rv16b_POST:
  case AArch64::LD1Rv4h_POST:
  case AArch64::LD1Rv8h_POST:
  case AArch64::LD1Rv2s_POST:
  case AArch64::LD1Rv4s_POST:
  case AArch64::LD1Rv1d_POST:
  case AArch64::LD1Rv2d_POST:
    isPost = true;
    break;
  default:
    isPost = false;
    break;
  }
  auto dst = readFromVecOperand(isPost ? 1 : 0, eltSize, numElts);
  auto baseReg = CurInst->getOperand(isPost ? 2 : 1).getReg();
  assert((baseReg >= AArch64::X0 && baseReg <= AArch64::X28) ||
         (baseReg == AArch64::SP) || (baseReg == AArch64::LR) ||
         (baseReg == AArch64::FP) || (baseReg == AArch64::XZR));

  Value *offset = nullptr;
  if (isPost) {
    if (CurInst->getOperand(3).isReg() &&
        CurInst->getOperand(3).getReg() != AArch64::XZR) {
      offset = readFromRegOld(CurInst->getOperand(3).getReg());
    } else {
      offset = getUnsignedIntConst((eltSize / 8), 64);
    }
  }

  auto i64 = getIntTy(64);
  auto base = readPtrFromReg(baseReg);
  auto baseAddr = createPtrToInt(base, i64);
  auto loaded = makeLoadWithOffset(base, 0, eltSize / 8);
  auto single_inserted = createInsertElement(dst, loaded, 0);
  auto shuffled =
      createShuffleVector(single_inserted, getZeroIntVec(numElts, 32));

  if (isPost) {
    updateReg(shuffled, CurInst->getOperand(1).getReg());
    auto added = createAdd(baseAddr, offset);
    updateOutputReg(added);
  } else {
    updateOutputReg(shuffled);
  }
}

void arm2llvm::lift_ld1(unsigned opcode) {
  unsigned numElts, eltSize;

  switch (opcode) {
  case AArch64::LD1i8:
  case AArch64::LD1i8_POST:
    numElts = 16;
    eltSize = 8;
    break;
  case AArch64::LD1i16:
  case AArch64::LD1i16_POST:
    numElts = 8;
    eltSize = 16;
    break;
  case AArch64::LD1i32:
  case AArch64::LD1i32_POST:
    numElts = 4;
    eltSize = 32;
    break;
  case AArch64::LD1i64:
  case AArch64::LD1i64_POST:
    numElts = 2;
    eltSize = 64;
    break;
  default:
    assert(false);
  }
  unsigned nregs = 1;
  bool isPost =
      opcode == AArch64::LD1i8_POST || opcode == AArch64::LD1i16_POST ||
      opcode == AArch64::LD1i32_POST || opcode == AArch64::LD1i64_POST;

  auto regCounter = decodeRegSet(CurInst->getOperand(isPost ? 2 : 1).getReg());
  auto index = getImm(isPost ? 3 : 2);
  auto baseReg = CurInst->getOperand(isPost ? 4 : 3).getReg();
  assert((baseReg >= AArch64::X0 && baseReg <= AArch64::X28) ||
         (baseReg == AArch64::SP) || (baseReg == AArch64::LR) ||
         (baseReg == AArch64::FP) || (baseReg == AArch64::XZR));
  assert((regCounter >= AArch64::Q0 && regCounter <= AArch64::Q31));
  Value *totalOffset{nullptr};
  if (isPost) {
    if (CurInst->getOperand(5).isReg() &&
        CurInst->getOperand(5).getReg() != AArch64::XZR) {
      totalOffset = readFromRegOld(CurInst->getOperand(5).getReg());
    } else {
      totalOffset = getUnsignedIntConst(nregs * (eltSize / 8), 64);
    }
  }

  auto base = readPtrFromReg(baseReg);
  auto i64 = getIntTy(64);
  auto baseAddr = createPtrToInt(base, i64);

  auto offset = 0;
  for (unsigned i = 0; i < nregs; i++, offset += eltSize / 8) {
    auto loaded = makeLoadWithOffset(base, offset, eltSize / 8);
    auto dst = readFromRegTyped(regCounter, getVecTy(eltSize, numElts));
    auto updated = createInsertElement(dst, loaded, index);
    updateReg(updated, regCounter);

    regCounter++;
    if (regCounter > AArch64::Q31)
      regCounter = AArch64::Q0;
  }

  if (isPost) {
    auto added = createAdd(baseAddr, totalOffset);
    updateOutputReg(added);
  }
}

void arm2llvm::lift_ldr3(unsigned opcode) {
  unsigned size;

  switch (opcode) {
  case AArch64::LDRBBroW:
  case AArch64::LDRBBroX:
  case AArch64::LDRBroW:
  case AArch64::LDRBroX:
  case AArch64::LDRSBWroW:
  case AArch64::LDRSBWroX:
  case AArch64::LDRSBXroW:
  case AArch64::LDRSBXroX:
    size = 1;
    break;
  case AArch64::LDRHHroW:
  case AArch64::LDRHHroX:
  case AArch64::LDRHroW:
  case AArch64::LDRHroX:
  case AArch64::LDRSHWroW:
  case AArch64::LDRSHWroX:
  case AArch64::LDRSHXroW:
  case AArch64::LDRSHXroX:
    size = 2;
    break;
  case AArch64::LDRWroX:
  case AArch64::LDRWroW:
  case AArch64::LDRSroW:
  case AArch64::LDRSroX:
  case AArch64::LDRSWroW:
  case AArch64::LDRSWroX:
    size = 4;
    break;
  case AArch64::LDRXroW:
  case AArch64::LDRXroX:
  case AArch64::LDRDroW:
  case AArch64::LDRDroX:
    size = 8;
    break;
  case AArch64::LDRQroW:
  case AArch64::LDRQroX:
    size = 16;
    break;
  default:
    *out << "\nError Unknown opcode\n";
    visitError();
    break;
  }

  bool sExt = opcode == AArch64::LDRSBWroW || opcode == AArch64::LDRSBWroX ||
              opcode == AArch64::LDRSBXroW || opcode == AArch64::LDRSBXroX ||
              opcode == AArch64::LDRSHWroW || opcode == AArch64::LDRSHWroX ||
              opcode == AArch64::LDRSHXroW || opcode == AArch64::LDRSHXroX ||
              opcode == AArch64::LDRSWroW || opcode == AArch64::LDRSWroX;

  auto [base, offset] = getParamsLoadReg();
  auto loaded = makeLoadWithOffset(base, offset, size);
  updateOutputReg(loaded, sExt);
}

void arm2llvm::lift_ldr2(unsigned opcode) {
  unsigned size;
  switch (opcode) {
  case AArch64::LDRSBWpre:
  case AArch64::LDRSBXpre:
  case AArch64::LDRBBpre:
  case AArch64::LDRBpre:
  case AArch64::LDRSBWpost:
  case AArch64::LDRSBXpost:
  case AArch64::LDRBBpost:
  case AArch64::LDRBpost:
    size = 1;
    break;
  case AArch64::LDRSHWpre:
  case AArch64::LDRSHXpre:
  case AArch64::LDRHHpre:
  case AArch64::LDRHpre:
  case AArch64::LDRSHWpost:
  case AArch64::LDRSHXpost:
  case AArch64::LDRHHpost:
  case AArch64::LDRHpost:
    size = 2;
    break;
  case AArch64::LDRSWpre:
  case AArch64::LDRWpre:
  case AArch64::LDRSpre:
  case AArch64::LDRSWpost:
  case AArch64::LDRWpost:
  case AArch64::LDRSpost:
    size = 4;
    break;
  case AArch64::LDRXpre:
  case AArch64::LDRDpre:
  case AArch64::LDRXpost:
  case AArch64::LDRDpost:
    size = 8;
    break;
  case AArch64::LDRQpre:
  case AArch64::LDRQpost:
    size = 16;
    break;
  default:
    assert(false);
  }
  bool sExt = opcode == AArch64::LDRSBWpre || opcode == AArch64::LDRSBXpre ||
              opcode == AArch64::LDRSHWpre || opcode == AArch64::LDRSHXpre ||
              opcode == AArch64::LDRSWpre || opcode == AArch64::LDRSBWpost ||
              opcode == AArch64::LDRSBXpost || opcode == AArch64::LDRSHWpost ||
              opcode == AArch64::LDRSHXpost || opcode == AArch64::LDRSWpost;
  auto &op0 = CurInst->getOperand(0);
  auto &op1 = CurInst->getOperand(1);
  auto &op2 = CurInst->getOperand(2);
  auto &op3 = CurInst->getOperand(3);
  assert(op0.isReg() && op1.isReg() && op2.isReg());
  assert(op0.getReg() == op2.getReg());
  assert(op3.isImm());

  // For pre and post index memory instructions, the destination register
  // is at position 1
  auto destReg = op1.getReg();
  auto baseReg = op2.getReg();
  auto imm = op3.getImm();
  assert((baseReg >= AArch64::X0 && baseReg <= AArch64::X28) ||
         (baseReg == AArch64::SP) || (baseReg == AArch64::LR) ||
         (baseReg == AArch64::FP) || (baseReg == AArch64::XZR));
  auto base = readPtrFromReg(baseReg);
  auto i64 = getIntTy(64);
  auto baseAddr = createPtrToInt(base, i64);

  // Start offset as a 9-bit signed integer
  assert(imm <= 255 && imm >= -256);
  auto offset = getSignedIntConst(imm, 9);
  Value *offsetVal = createSExt(offset, i64);
  Value *zeroVal = getUnsignedIntConst(0, 64);

  bool isPre = opcode == AArch64::LDRBBpre || opcode == AArch64::LDRBpre ||
               opcode == AArch64::LDRHHpre || opcode == AArch64::LDRHpre ||
               opcode == AArch64::LDRWpre || opcode == AArch64::LDRSpre ||
               opcode == AArch64::LDRXpre || opcode == AArch64::LDRDpre ||
               opcode == AArch64::LDRQpre || opcode == AArch64::LDRSBWpre ||
               opcode == AArch64::LDRSBXpre || opcode == AArch64::LDRSHWpre ||
               opcode == AArch64::LDRSHXpre || opcode == AArch64::LDRSWpre;

  auto loaded = makeLoadWithOffset(base, isPre ? offsetVal : zeroVal, size);
  updateReg(loaded, destReg, sExt);

  auto added = createAdd(baseAddr, offsetVal);
  updateOutputReg(added, sExt);
}

void arm2llvm::lift_ldu1(unsigned opcode) {
  unsigned size;
  if (opcode == AArch64::LDURBBi || opcode == AArch64::LDURBi ||
      opcode == AArch64::LDURSBWi || opcode == AArch64::LDURSBXi)
    size = 1;
  else if (opcode == AArch64::LDURHHi || opcode == AArch64::LDURHi ||
           opcode == AArch64::LDURSHWi || opcode == AArch64::LDURSHXi)
    size = 2;
  else if (opcode == AArch64::LDURWi || opcode == AArch64::LDURSi ||
           opcode == AArch64::LDURSWi)
    size = 4;
  else if (opcode == AArch64::LDURXi || opcode == AArch64::LDURDi)
    size = 8;
  else if (opcode == AArch64::LDURQi)
    size = 16;
  else
    assert(false);

  bool sExt = opcode == AArch64::LDURSBWi || opcode == AArch64::LDURSBXi ||
              opcode == AArch64::LDURSHWi || opcode == AArch64::LDURSHXi ||
              opcode == AArch64::LDURSWi;
  auto [base, imm] = getParamsLoadImmed();
  // Start offset as a 9-bit signed integer
  assert(imm <= 255 && imm >= -256);
  auto offset = getSignedIntConst(imm, 9);
  auto i64 = getIntTy(64);
  Value *offsetVal = createSExt(offset, i64);
  auto loaded = makeLoadWithOffset(base, offsetVal, size);
  updateOutputReg(loaded, sExt);
}

void arm2llvm::lift_ldr1(unsigned opcode) {
  unsigned size;
  switch (opcode) {
  case AArch64::LDRBBui:
  case AArch64::LDRBui:
  case AArch64::LDRSBXui:
  case AArch64::LDRSBWui:
    size = 1;
    break;
  case AArch64::LDRHHui:
  case AArch64::LDRHui:
  case AArch64::LDRSHXui:
  case AArch64::LDRSHWui:
    size = 2;
    break;
  case AArch64::LDRWui:
  case AArch64::LDRSui:
  case AArch64::LDRSWui:
    size = 4;
    break;
  case AArch64::LDRXui:
  case AArch64::LDRDui:
    size = 8;
    break;
  case AArch64::LDRQui:
    size = 16;
    break;
  default:
    assert(false);
  }

  bool sExt = opcode == AArch64::LDRSBXui || opcode == AArch64::LDRSBWui ||
              opcode == AArch64::LDRSHXui || opcode == AArch64::LDRSHWui ||
              opcode == AArch64::LDRSWui;

  MCOperand &op2 = CurInst->getOperand(2);
  if (op2.isExpr()) {
    *out << "[operand 2 is expr]\n";
    auto [globalVar, specifier] = getExprVar(op2.getExpr());
    if (isGOT(specifier)) {
      Value *ptrToInt = createPtrToInt(globalVar, getIntTy(size * 8));
      updateOutputReg(ptrToInt, sExt);
    } else {
      auto loaded = makeLoadWithOffset(globalVar, 0, size);
      updateOutputReg(loaded, sExt);
    }
  } else {
    *out << "[operand 2 is not expr]\n";
    auto [base, imm] = getParamsLoadImmed();
    auto loaded = makeLoadWithOffset(base, imm * size, size);
    updateOutputReg(loaded, sExt);
  }
}
