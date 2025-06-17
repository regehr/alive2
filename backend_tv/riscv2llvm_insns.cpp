#include "backend_tv/mc2llvm.h"
#include "backend_tv/riscv2llvm.h"

#include "llvm/BinaryFormat/ELF.h"
#include "llvm/IR/Value.h"
#include "llvm/MC/MCAsmInfo.h"

#include <cassert>
#include <cmath>
#include <cstdint>
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

    /*
     * Standard instructions
     */

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

  case RISCV::C_BNEZ:
  case RISCV::C_BEQZ: {
    auto a = readFromRegOperand(0, i64ty);
    auto [dst_true, dst_false] = getBranchTargetsOperand(1);
    Value *zero = getUnsignedIntConst(0, 64);
    Value *cond;
    switch (opcode) {
    case RISCV::C_BNEZ:
      cond = createICmp(ICmpInst::Predicate::ICMP_NE, a, zero);
      break;
    case RISCV::C_BEQZ:
      cond = createICmp(ICmpInst::Predicate::ICMP_EQ, a, zero);
      break;
    default:
      assert(false);
    }
    createBranch(cond, dst_true, dst_false);
    break;
  }

  case RISCV::BLT:
  case RISCV::BLTU:
  case RISCV::BNE:
  case RISCV::BEQ:
  case RISCV::BGE:
  case RISCV::BGEU: {
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
    case RISCV::BEQ:
      cond = createICmp(ICmpInst::Predicate::ICMP_EQ, a, b);
      break;
    case RISCV::BGE:
      cond = createICmp(ICmpInst::Predicate::ICMP_SGE, a, b);
      break;
    case RISCV::BGEU:
      cond = createICmp(ICmpInst::Predicate::ICMP_UGE, a, b);
      break;
    default:
      assert(false);
    }
    createBranch(cond, dst_true, dst_false);
    break;
  }

  case RISCV::C_ADD:
  case RISCV::ADD:
  case RISCV::C_SUB:
  case RISCV::SUB:
  case RISCV::C_AND:
  case RISCV::AND:
  case RISCV::C_OR:
  case RISCV::OR:
  case RISCV::C_XOR:
  case RISCV::XOR:
  case RISCV::SRL:
  case RISCV::SLL: {
    auto a = readFromRegOperand(1, i64ty);
    auto b = readFromRegOperand(2, i64ty);
    Value *res;
    switch (opcode) {
    case RISCV::C_ADD:
    case RISCV::ADD:
      res = createAdd(a, b);
      break;
    case RISCV::C_SUB:
    case RISCV::SUB:
      res = createSub(a, b);
      break;
    case RISCV::C_OR:
    case RISCV::OR:
      res = createOr(a, b);
      break;
    case RISCV::C_AND:
    case RISCV::AND:
      res = createAnd(a, b);
      break;
    case RISCV::C_XOR:
    case RISCV::XOR:
      res = createXor(a, b);
      break;
    case RISCV::SRL:
      res = createMaskedLShr(a, b);
      break;
    case RISCV::SLL:
      res = createMaskedShl(a, b);
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
  case RISCV::SUBW:
  case RISCV::SRLW:
  case RISCV::SLLW:
  case RISCV::SRAW: {
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
    case RISCV::SRLW:
      res = createMaskedLShr(a32, b32);
      break;
    case RISCV::SLLW:
      res = createMaskedShl(a32, b32);
      break;
    case RISCV::SRAW:
      res = createMaskedAShr(a32, b32);
      break;
    default:
      assert(false);
    }
    auto resExt = createSExt(res, i64ty);
    updateOutputReg(resExt);
    break;
  }

  case RISCV::C_LI: {
    auto imm = readFromImmOperand(1, 12, 64);
    updateOutputReg(imm);
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
      auto rvExpr = dyn_cast<MCSpecifierExpr>(expr);
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
             << (string)MAI->getSpecifierName(specifier) << "\n";
        exit(-1);
      }
    } else {
      *out << "unhandled lui case\n";
      exit(-1);
    }
    break;
  }

  case RISCV::LB:
  case RISCV::LBU:
  case RISCV::LH:
  case RISCV::LHU:
  case RISCV::C_LW:
  case RISCV::LW:
  case RISCV::LWU:
  case RISCV::C_LD:
  case RISCV::LD: {
    bool sExt;
    switch (opcode) {
    case RISCV::LB:
    case RISCV::LH:
    case RISCV::C_LW:
    case RISCV::LW:
    case RISCV::C_LD:
    case RISCV::LD:
      sExt = true;
      break;
    case RISCV::LBU:
    case RISCV::LHU:
    case RISCV::LWU:
      sExt = false;
      break;
    default:
      assert(false);
    }
    Type *size{nullptr};
    switch (opcode) {
    case RISCV::LB:
    case RISCV::LBU:
      size = i8ty;
      break;
    case RISCV::LH:
    case RISCV::LHU:
      size = i16ty;
      break;
    case RISCV::C_LW:
    case RISCV::LW:
    case RISCV::LWU:
      size = i32ty;
      break;
    case RISCV::C_LD:
    case RISCV::LD:
    default:
      size = i64ty;
      break;
      assert(false);
    }
    Value *ptr = getPointerOperand();
    Value *loaded = createLoad(size, ptr);
    if (size != i64ty)
      loaded = sExt ? createSExt(loaded, i64ty) : createZExt(loaded, i64ty);
    updateOutputReg(loaded);
    break;
  }

  case RISCV::C_SB:
  case RISCV::SB:
  case RISCV::C_SH:
  case RISCV::SH:
  case RISCV::C_SW:
  case RISCV::SW:
  case RISCV::C_SD:
  case RISCV::SD: {
    Type *size{nullptr};
    switch (opcode) {
    case RISCV::C_SB:
    case RISCV::SB:
      size = i8ty;
      break;
    case RISCV::C_SH:
    case RISCV::SH:
      size = i16ty;
      break;
    case RISCV::C_SW:
    case RISCV::SW:
      size = i32ty;
      break;
    case RISCV::C_SD:
    case RISCV::SD:
      size = i64ty;
      break;
    default:
      assert(false);
    }
    auto value = readFromRegOperand(0, size);
    auto ptr = getPointerOperand();
    createStore(value, ptr);
    break;
  }

  case RISCV::C_LDSP:
  case RISCV::C_SDSP: {
    auto op = CurInst->getOperand(2);
    assert(op.isImm());
    auto imm_int = op.getImm() & ((1U << 12) - 1);
    auto imm = getUnsignedIntConst(imm_int, 12);
    auto imm_ext = createZExt(imm, i64ty);
    auto amt = getSignedIntConst(3, 64);
    auto imm_scaled = createMaskedShl(imm_ext, amt);
    auto sp = readFromRegOperand(1, ptrTy);
    auto addr = createGEP(i8ty, sp, {imm_scaled}, nextName());
    switch (opcode) {
    case RISCV::C_LDSP:
      updateOutputReg(createLoad(i64ty, addr));
      break;
    case RISCV::C_SDSP:
      createStore(readFromRegOperand(0, i64ty), addr);
      break;
    default:
      assert(false);
    }
    break;
  }

  case RISCV::C_ADDI16SP:
  case RISCV::C_ADDI:
  case RISCV::ADDI: {
    if (CurInst->getOperand(2).isImm()) {
      auto a = readFromRegOperand(1, i64ty);
      auto imm = readFromImmOperand(2, 12, 64);
      if (opcode == RISCV::C_ADDI16SP) {
        auto scaled = createMaskedShl(imm, getUnsignedIntConst(4, 64));
        updateOutputReg(createAdd(a, scaled));
        break;
      }
      updateOutputReg(createAdd(a, imm));
    } else {
      Value *ptr = getPointerFromMCExpr();
      updateOutputReg(ptr);
    }
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
  case RISCV::ORI: {
    auto a = readFromRegOperand(1, i64ty);
    auto b = readFromImmOperand(2, 12, 64);
    Value *res;
    switch (opcode) {
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

  case RISCV::SRLIW: {
    auto a = readFromRegOperand(1, i64ty);
    auto a32 = createTrunc(a, i32ty);
    auto imm_op = CurInst->getOperand(2);
    auto imm_int = imm_op.getImm() & ((1U << 5) - 1);
    auto imm = getUnsignedIntConst(imm_int, 32);
    auto res = createMaskedLShr(a32, imm);
    auto resExt = createSExt(res, i64ty);
    updateOutputReg(resExt);
    break;
  }

  case RISCV::C_MV: {
    auto a = readFromRegOperand(1, i64ty);
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

    /*
     * M extension instructions
     */

  case RISCV::C_MUL:
  case RISCV::MUL:
  case RISCV::MULW: {
    auto a = readFromRegOperand(1, i64ty);
    auto b = readFromRegOperand(2, i64ty);
    Value *res;
    switch (opcode) {
    case RISCV::C_MUL:
    case RISCV::MUL:
      res = createMul(a, b);
      break;
    case RISCV::MULW: {
      auto a32 = createTrunc(a, i32ty);
      auto b32 = createTrunc(b, i32ty);
      res = createSExt(createMul(a32, b32), i64ty);
      break;
    }
    default:
      assert(false);
    }
    updateOutputReg(res);
    break;
  }

  case RISCV::DIV:
  case RISCV::DIVU:
  case RISCV::DIVW:
  case RISCV::DIVUW:
  case RISCV::REM:
  case RISCV::REMU:
  case RISCV::REMW:
  case RISCV::REMUW: {
    auto a = readFromRegOperand(1, i64ty);
    auto b = readFromRegOperand(2, i64ty);
    Value *lhs;
    Value *rhs;
    uint64_t size;
    switch (opcode) {
    case RISCV::DIV:
    case RISCV::DIVU:
    case RISCV::REM:
    case RISCV::REMU:
      size = 64;
      lhs = a;
      rhs = b;
      break;
    case RISCV::DIVW:
    case RISCV::DIVUW:
    case RISCV::REMW:
    case RISCV::REMUW:
      size = 32;
      lhs = createTrunc(a, i32ty);
      rhs = createTrunc(b, i32ty);
      break;
    default:
      assert(false);
    }

    auto allOnes = getAllOnesConst(size);
    auto intMin = createMaskedShl(getUnsignedIntConst(1, size),
                                  getUnsignedIntConst(size - 1, size));

    Value *res;
    switch (opcode) {
    case RISCV::DIV:
    case RISCV::DIVW:
      res = createCheckedSDiv(lhs, rhs, allOnes, intMin);
      break;
    case RISCV::DIVU:
    case RISCV::DIVUW:
      res = createCheckedUDiv(lhs, rhs, allOnes);
      break;
    case RISCV::REM:
    case RISCV::REMW:
      res = createCheckedSRem(lhs, rhs, lhs, getUnsignedIntConst(0, size));
      break;
    case RISCV::REMU:
    case RISCV::REMUW:
      res = createCheckedURem(lhs, rhs, lhs);
      break;
    default:
      assert(false);
    }
    updateOutputReg(res);
    break;
  }

  default:
    visitError();
  }
}
