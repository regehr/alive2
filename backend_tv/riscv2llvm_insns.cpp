#include "backend_tv/riscv2llvm.h"

#include "Target/RISCV/MCTargetDesc/RISCVMCExpr.h"
#include "llvm/BinaryFormat/ELF.h"

#include <cmath>
#include <vector>

#define GET_INSTRINFO_ENUM
#include "Target/RISCV/RISCVGenInstrInfo.inc"

#define GET_REGINFO_ENUM
#include "Target/RISCV/RISCVGenRegisterInfo.inc"

using namespace std;
using namespace lifter;
using namespace llvm;

void riscv2llvm::lift(MCInst &I) {
  auto opcode = I.getOpcode();
  // StringRef instStr = InstPrinter->getOpcodeName(opcode);
  auto newbb = BasicBlock::Create(Ctx, "lifter_" + nextName(), liftedFn);
  createBranch(newbb);
  LLVMBB = newbb;

  // auto i1ty = getIntTy(1);
  auto i8ty = getIntTy(8);
  auto i16ty = getIntTy(16);
  auto i32ty = getIntTy(32);
  auto i64ty = getIntTy(64);
  // auto i128ty = getIntTy(128);
  auto ptrTy = llvm::PointerType::get(Ctx, 0);

  switch (opcode) {

  case RISCV::C_NOP_HINT:
    break;

  case RISCV::C_J: {
    /*
     * copied from ARM -- maybe move to portable code
     */
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
    break;
  }

  case RISCV::C_BNEZ: {
    auto a = readFromRegOperand(0, i64ty);
    auto [dst_true, dst_false] = getBranchTargetsOperand(1);
    Value *zero = getUnsignedIntConst(0, 64);
    Value *cond = createICmp(ICmpInst::Predicate::ICMP_NE, a, zero);
    createBranch(cond, dst_true, dst_false);
    break;
  }

  case RISCV::BLT:
  case RISCV::BLTU:
  case RISCV::BNE: {
    auto a = readFromRegOperand(0, i64ty);
    auto b = readFromRegOperand(1, i64ty);
    auto [dst_true, dst_false] = getBranchTargetsOperand(2);
    Value *cond;
    switch (opcode) {
    case RISCV::BLT:
      cond = createICmp(ICmpInst::Predicate::ICMP_SLT, a, b);
      break;
    case RISCV::BLTU:
      cond = createICmp(ICmpInst::Predicate::ICMP_ULT, a, b);
      break;
    case RISCV::BNE:
      cond = createICmp(ICmpInst::Predicate::ICMP_NE, a, b);
      break;
    default:
      assert(false);
    }
    createBranch(cond, dst_true, dst_false);
    break;
  }

  case RISCV::MUL:
  case RISCV::C_ADD:
  case RISCV::ADD:
  case RISCV::C_SUB:
  case RISCV::SUB:
  case RISCV::C_OR:
  case RISCV::OR: {
    auto a = readFromRegOperand(1, i64ty);
    auto b = readFromRegOperand(2, i64ty);
    Value *res;
    switch (opcode) {
    case RISCV::ADD:
    case RISCV::C_ADD:
      res = createAdd(a, b);
      break;
    case RISCV::SUB:
    case RISCV::C_SUB:
      res = createSub(a, b);
      break;
    case RISCV::C_OR:
    case RISCV::OR:
      res = createOr(a, b);
      break;
    case RISCV::MUL:
      res = createMul(a, b);
      break;
    default:
      assert(false);
    }
    updateOutputReg(res);
    break;
  }

  case RISCV::C_ADDW:
  case RISCV::ADDW:
  case RISCV::C_SUBW:
  case RISCV::SUBW: {
    auto a = readFromRegOperand(1, i64ty);
    auto b = readFromRegOperand(2, i64ty);
    auto a32 = createTrunc(a, i32ty);
    auto b32 = createTrunc(b, i32ty);
    Value *res;
    switch (opcode) {
    case RISCV::C_ADDW:
    case RISCV::ADDW:
      res = createAdd(a32, b32);
      break;
    case RISCV::C_SUBW:
    case RISCV::SUBW:
      res = createSub(a32, b32);
      break;
    default:
      assert(false);
    }
    auto resExt = createSExt(res, i64ty);
    updateOutputReg(resExt);
    break;
  }

  case RISCV::C_LUI:
  case RISCV::LUI: {
    auto op1 = CurInst->getOperand(1);
    if (op1.isImm()) {
      auto imm = readFromImmOperand(1, 20, 64);
      auto amt = getUnsignedIntConst(12, 64);
      auto immShifted = createRawShl(imm, amt);
      updateOutputReg(immShifted);
    } else if (op1.isExpr()) {
      auto expr = op1.getExpr();
      auto rvExpr = dyn_cast<RISCVMCExpr>(expr);
      assert(rvExpr);
      auto specifier = rvExpr->getSpecifier();
      switch (specifier) {
      case ELF::R_RISCV_HI20:
        // FIXME: this is loading the high 20 bits of an address. we
        // (unsoundly) ignore this for now -- but we'll want to
        // connect this up with the lo part that comes (sometimes a
        // number of instructions) later. this will be easy as long as
        // the pair are always in the same basic block.
        break;
      default:
        *out << "unknown specifier: "
             << (string)RISCVMCExpr::getSpecifierName(specifier) << "\n";
        exit(-1);
      }
    } else {
      *out << "unhandled lui case\n";
      exit(-1);
    }
    break;
  }

  case RISCV::LH:
  case RISCV::LHU:
  case RISCV::LW:
  case RISCV::LWU:
  case RISCV::LD: {
    bool sExt;
    switch (opcode) {
    case RISCV::LH:
    case RISCV::LW:
    case RISCV::LD:
      sExt = true;
      break;
    case RISCV::LHU:
    case RISCV::LWU:
      sExt = false;
      break;
    default:
      assert(false);
    }
    Type *size{nullptr};
    switch (opcode) {
    case RISCV::LH:
    case RISCV::LHU:
      size = i16ty;
      break;
    case RISCV::LW:
    case RISCV::LWU:
      size = i32ty;
      break;
    case RISCV::LD:
    default:
      size = i64ty;
      break;
      assert(false);
    }
    Value *ptr{nullptr};
    if (CurInst->getOperand(2).isImm()) {
      auto imm = readFromImmOperand(2, 12, 64);
      ptr = createGEP(i8ty, readFromRegOperand(1, ptrTy), {imm}, nextName());
    } else {
      ptr = getPointerOperand();
    }
    Value *loaded = createLoad(size, ptr);
    if (size != i64ty)
      loaded = sExt ? createSExt(loaded, i64ty) : createZExt(loaded, i64ty);
    updateOutputReg(loaded);
    break;
  }

  case RISCV::C_SRAI:
  case RISCV::SRAI:
  case RISCV::C_SRLI:
  case RISCV::SRLI:
  case RISCV::C_SLLI:
  case RISCV::SLLI:
  case RISCV::C_ANDI:
  case RISCV::ANDI:
  case RISCV::XORI:
  case RISCV::ORI:
  case RISCV::C_ADDI:
  case RISCV::ADDI: {
    auto a = readFromRegOperand(1, i64ty);
    auto b = readFromImmOperand(2, 12, 64);
    Value *res;
    switch (opcode) {
    case RISCV::C_ADDI:
    case RISCV::ADDI:
      res = createAdd(a, b);
      break;
    case RISCV::C_ANDI:
    case RISCV::ANDI:
      res = createAnd(a, b);
      break;
    case RISCV::XORI:
      res = createXor(a, b);
      break;
    case RISCV::ORI:
      res = createOr(a, b);
      break;
    case RISCV::C_SLLI:
    case RISCV::SLLI:
      res = createMaskedShl(a, b);
      break;
    case RISCV::C_SRAI:
    case RISCV::SRAI:
      res = createMaskedAShr(a, b);
      break;
    case RISCV::C_SRLI:
    case RISCV::SRLI:
      res = createMaskedLShr(a, b);
      break;
    default:
      assert(false);
    }
    updateOutputReg(res);
    break;
  }

  case RISCV::C_ADDIW:
  case RISCV::ADDIW: {
    auto a = readFromRegOperand(1, i64ty);
    auto b = readFromImmOperand(2, 12, 32);
    auto a32 = createTrunc(a, i32ty);
    auto res = createAdd(a32, b);
    auto resExt = createSExt(res, i64ty);
    updateOutputReg(resExt);
    break;
  }

  case RISCV::C_MV: {
    auto a = readFromRegOperand(1, i64ty);
    updateOutputReg(a);
    break;
  }

  case RISCV::C_LI: {
    auto a = readFromImmOperand(1, 12, 64);
    updateOutputReg(a);
    break;
  }

  case RISCV::SLTU:
  case RISCV::SLT: {
    auto a = readFromRegOperand(1, i64ty);
    auto b = readFromRegOperand(2, i64ty);
    Value *res;
    switch (opcode) {
    case RISCV::SLT:
      res = createICmp(ICmpInst::Predicate::ICMP_SLT, a, b);
      break;
    case RISCV::SLTU:
      res = createICmp(ICmpInst::Predicate::ICMP_ULT, a, b);
      break;
    default:
      assert(false);
    }
    auto resExt = createZExt(res, i64ty);
    updateOutputReg(resExt);
    break;
  }

  case RISCV::SLTI:
  case RISCV::SLTIU: {
    auto a = readFromRegOperand(1, i64ty);
    auto b = readFromImmOperand(2, 12, 64);
    Value *res;
    switch (opcode) {
    case RISCV::SLTI:
      res = createICmp(ICmpInst::Predicate::ICMP_SLT, a, b);
      break;
    case RISCV::SLTIU:
      res = createICmp(ICmpInst::Predicate::ICMP_ULT, a, b);
      break;
    default:
      assert(false);
    }
    auto resExt = createZExt(res, i64ty);
    updateOutputReg(resExt);
    break;
  }

  case RISCV::C_JR: {
    doReturn();
    break;
  }

  case RISCV::JALR: {
    assert(CurInst->getOperand(0).getReg() == RISCV::X0);
    assert(CurInst->getOperand(1).getReg() == RISCV::X1);
    assert(CurInst->getOperand(2).getImm() == 0);
    doReturn();
    break;
  }

  default:
    *out << "unhandled instruction\n";
    exit(-1);
  }
}
