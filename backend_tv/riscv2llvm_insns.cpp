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
  auto i128ty = getIntTy(128);
  auto ptrTy = llvm::PointerType::get(Ctx, 0);

  switch (opcode) {

    /*
     * Standard instructions
     */

  case RISCV::C_NOP_HINT:
    break;

  // JDR: I don't understand why JAL is getting generated sometimes
  // for simple direct branches. here we're ignoring the register
  // update.
  case RISCV::JAL:
  case RISCV::C_JAL:
  case RISCV::C_J: {
    /*
     * copied from ARM -- maybe move to portable code
     */
    int operand = 0;
    if (opcode == RISCV::JAL || opcode == RISCV::C_JAL)
      operand = 1;
    BasicBlock *dst{nullptr};
    // JDR: I don't understand this
    if (CurInst->getOperand(operand).isImm()) {
      // handles the case when we add an entry block with no predecessors
      auto &dst_name = Str->MF.BBs[getImm(operand)].getName();
      dst = getBBByName(dst_name);
    } else {
      dst = getBB(CurInst->getOperand(operand));
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
    ICmpInst::Predicate pred = ICmpInst::Predicate::BAD_ICMP_PREDICATE;
    switch (opcode) {
    case RISCV::C_BNEZ:
      pred = ICmpInst::Predicate::ICMP_NE;
      break;
    case RISCV::C_BEQZ:
      pred = ICmpInst::Predicate::ICMP_EQ;
      break;
    default:
      assert(false);
    }
    auto cond = createICmp(pred, a, zero);
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
    ICmpInst::Predicate pred = ICmpInst::Predicate::BAD_ICMP_PREDICATE;
    switch (opcode) {
    case RISCV::BLT:
      pred = ICmpInst::Predicate::ICMP_SLT;
      break;
    case RISCV::BLTU:
      pred = ICmpInst::Predicate::ICMP_ULT;
      break;
    case RISCV::BNE:
      pred = ICmpInst::Predicate::ICMP_NE;
      break;
    case RISCV::BEQ:
      pred = ICmpInst::Predicate::ICMP_EQ;
      break;
    case RISCV::BGE:
      pred = ICmpInst::Predicate::ICMP_SGE;
      break;
    case RISCV::BGEU:
      pred = ICmpInst::Predicate::ICMP_UGE;
      break;
    default:
      assert(false);
    }
    auto cond = createICmp(pred, a, b);
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
  case RISCV::SRA:
  case RISCV::SLL: {
    auto a = readFromRegOperand(1, i64ty);
    auto b = readFromRegOperand(2, i64ty);
    Value *res{nullptr};
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
    case RISCV::SRA:
      res = createMaskedAShr(a, b);
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
    Value *res{nullptr};
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
    updateOutputReg(res, /*SExt=*/true);
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
      } else {
        updateOutputReg(createAdd(a, imm));
      }
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
    Value *res{nullptr};
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
    updateOutputReg(res, /*SExt=*/true);
    break;
  }

  case RISCV::SLLIW:
  case RISCV::SRAIW:
  case RISCV::SRLIW: {
    auto a = readFromRegOperand(1, i64ty);
    auto a32 = createTrunc(a, i32ty);
    auto imm_op = CurInst->getOperand(2);
    auto imm_int = imm_op.getImm() & ((1U << 5) - 1);
    auto imm = getUnsignedIntConst(imm_int, 32);
    Value *res{nullptr};
    switch (opcode) {
    case RISCV::SLLIW:
      res = createMaskedShl(a32, imm);
      break;
    case RISCV::SRAIW:
      res = createMaskedAShr(a32, imm);
      break;
    case RISCV::SRLIW:
      res = createMaskedLShr(a32, imm);
      break;
    default:
      assert(false);
    }
    updateOutputReg(res, /*SExt=*/true);
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
    ICmpInst::Predicate pred = ICmpInst::Predicate::BAD_ICMP_PREDICATE;
    switch (opcode) {
    case RISCV::SLT:
      pred = ICmpInst::Predicate::ICMP_SLT;
      break;
    case RISCV::SLTU:
      pred = ICmpInst::Predicate::ICMP_ULT;
      break;
    default:
      assert(false);
    }
    auto res = createICmp(pred, a, b);
    auto resExt = createZExt(res, i64ty);
    updateOutputReg(resExt);
    break;
  }

  case RISCV::SLTI:
  case RISCV::SLTIU: {
    auto a = readFromRegOperand(1, i64ty);
    auto b = readFromImmOperand(2, 12, 64);
    ICmpInst::Predicate pred = ICmpInst::Predicate::BAD_ICMP_PREDICATE;
    switch (opcode) {
    case RISCV::SLTI:
      pred = ICmpInst::Predicate::ICMP_SLT;
      break;
    case RISCV::SLTIU:
      pred = ICmpInst::Predicate::ICMP_ULT;
      break;
    default:
      assert(false);
    }
    auto res = createICmp(pred, a, b);
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

  case RISCV::MULH:
  case RISCV::MULHSU:
  case RISCV::MULHU: {
    auto a = readFromRegOperand(1, i64ty);
    auto b = readFromRegOperand(2, i64ty);
    Value *ax{nullptr}, *bx{nullptr};
    if (opcode == RISCV::MULHU)
      ax = createZExt(a, i128ty);
    else
      ax = createSExt(a, i128ty);
    if (opcode == RISCV::MULH)
      bx = createSExt(b, i128ty);
    else
      bx = createZExt(b, i128ty);
    auto res = createMul(ax, bx);
    Value *resShift{nullptr};
    if (opcode == RISCV::MULHU)
      resShift = createRawLShr(res, getUnsignedIntConst(64, 128));
    else
      resShift = createRawAShr(res, getUnsignedIntConst(64, 128));
    updateOutputReg(createTrunc(resShift, i64ty));
    break;
  }

  case RISCV::C_MUL:
  case RISCV::MUL:
  case RISCV::MULW: {
    auto a = readFromRegOperand(1, i64ty);
    auto b = readFromRegOperand(2, i64ty);
    Value *res{nullptr};
    switch (opcode) {
    case RISCV::C_MUL:
    case RISCV::MUL:
      res = createMul(a, b);
      break;
    case RISCV::MULW: {
      auto a32 = createTrunc(a, i32ty);
      auto b32 = createTrunc(b, i32ty);
      res = createMul(a32, b32);
      break;
    }
    default:
      assert(false);
    }
    updateOutputReg(res, /*SExt=*/true);
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
    Value *lhs{nullptr}, *rhs{nullptr};
    uint64_t size{-1UL};
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
    auto intMin = getSignedMinConst(size);

    Value *res{nullptr};
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
    updateOutputReg(res, /*SExt=*/true);
    break;
  }

    /*
     * B extension instructions
     */

  case RISCV::ADD_UW: {
    auto a = readFromRegOperand(1, i64ty);
    auto b = readFromRegOperand(2, i64ty);
    auto a32 = createTrunc(a, i32ty);
    auto ax = createZExt(a32, i64ty);
    auto res = createAdd(b, ax);
    updateOutputReg(res);
    break;
  }

  case RISCV::CLZ:
  case RISCV::CLZW: {
    auto a = readFromRegOperand(1, i64ty);
    auto res = createCtlz(opcode == RISCV::CLZ ? a : createTrunc(a, i32ty));
    updateOutputReg(res, /*SExt=*/true);
    break;
  }

  case RISCV::CPOP:
  case RISCV::CPOPW: {
    auto a = readFromRegOperand(1, i64ty);
    auto res = createCtPop(opcode == RISCV::CPOP ? a : createTrunc(a, i32ty));
    updateOutputReg(res, /*SExt=*/true);
    break;
  }

  case RISCV::CTZ:
  case RISCV::CTZW: {
    auto a = readFromRegOperand(1, i64ty);
    auto res = createCttz(opcode == RISCV::CTZ ? a : createTrunc(a, i32ty));
    updateOutputReg(res, /*SExt=*/true);
    break;
  }

  case RISCV::BSET:
  case RISCV::ROL:
  case RISCV::ROR:
  case RISCV::BCLR:
  case RISCV::BEXT:
  case RISCV::BINV: {
    auto a = readFromRegOperand(1, i64ty);
    auto b = readFromRegOperand(2, i64ty);
    auto mask = getUnsignedIntConst(0b111111, 64);
    auto shamt = createAnd(b, mask);
    auto bit = createRawShl(getUnsignedIntConst(1, 64), shamt);
    Value *res{nullptr};
    switch (opcode) {
    case RISCV::BSET:
      res = createOr(a, bit);
      break;
    case RISCV::ROL: {
      auto shamt2 = createSub(getUnsignedIntConst(64, 64), shamt);
      auto lo = createMaskedLShr(a, shamt2);
      auto hi = createMaskedShl(a, shamt);
      res = createOr(lo, hi);
      break;
    }
    case RISCV::ROR: {
      auto shamt2 = createSub(getUnsignedIntConst(64, 64), shamt);
      auto lo = createMaskedLShr(a, shamt);
      auto hi = createMaskedShl(a, shamt2);
      res = createOr(lo, hi);
      break;
    }
    case RISCV::BCLR:
      res = createAnd(a, createNot(bit));
      break;
    case RISCV::BEXT: {
      auto shifted = createMaskedLShr(a, shamt);
      res = createAnd(shifted, getUnsignedIntConst(1, 64));
      break;
    }
    case RISCV::BINV:
      res = createXor(a, bit);
      break;
    default:
      assert(false);
    }
    updateOutputReg(res);
    break;
  }

  case RISCV::BSETI:
  case RISCV::RORI:
  case RISCV::BCLRI:
  case RISCV::BEXTI:
  case RISCV::BINVI:
  case RISCV::SLLI_UW: {
    // instructions with a 6-bit, unsigned immediate
    auto a = readFromRegOperand(1, i64ty);
    auto op = CurInst->getOperand(2);
    assert(op.isImm());
    auto shamt = op.getImm() & 0b111111;

    Value *res{nullptr};
    switch (opcode) {
    case RISCV::BSETI:
      res = createOr(a, getUnsignedIntConst(1UL << shamt, 64));
      break;
    case RISCV::RORI: {
      auto lo = createMaskedLShr(a, getUnsignedIntConst(shamt, 64));
      auto hi = createMaskedShl(a, getUnsignedIntConst(64 - shamt, 64));
      res = createOr(lo, hi);
      break;
    }
    case RISCV::BCLRI:
      res = createAnd(a, getUnsignedIntConst(~(1UL << shamt), 64));
      break;
    case RISCV::BEXTI: {
      auto shifted = createMaskedLShr(a, getUnsignedIntConst(shamt, 64));
      res = createAnd(shifted, getUnsignedIntConst(1, 64));
      break;
    }
    case RISCV::BINVI:
      res = createXor(a, getUnsignedIntConst(1UL << shamt, 64));
      break;
    case RISCV::SLLI_UW: {
      auto a32 = createTrunc(a, i32ty);
      auto ax = createZExt(a32, i64ty);
      res = createMaskedShl(ax, getUnsignedIntConst(shamt, 64));
      break;
    }
    default:
      assert(false);
    }
    updateOutputReg(res);
    break;
  }

  case RISCV::REV8_RV64:
  case RISCV::SEXT_B:
  case RISCV::SEXT_H:
  case RISCV::ZEXT_H_RV64: {
    auto a = readFromRegOperand(1, i64ty);

    Value *res{nullptr};
    switch (opcode) {
    case RISCV::REV8_RV64:
      res = createBSwap(a);
      break;
    case RISCV::SEXT_B:
      res = createTrunc(a, i8ty);
      break;
    case RISCV::SEXT_H:
      res = createTrunc(a, i16ty);
      break;
    case RISCV::ZEXT_H_RV64:
      res = createZExt(createTrunc(a, i16ty), i64ty);
      break;
    default:
      assert(false);
    }
    updateOutputReg(res, /*SExt=*/true);
    break;
  }

  case RISCV::MIN:
  case RISCV::MINU:
  case RISCV::MAX:
  case RISCV::MAXU: {
    auto a = readFromRegOperand(1, i64ty);
    auto b = readFromRegOperand(2, i64ty);
    Value *res{nullptr};
    switch (opcode) {
    case RISCV::MIN:
      res = createSMin(a, b);
      break;
    case RISCV::MINU:
      res = createUMin(a, b);
      break;
    case RISCV::MAX:
      res = createSMax(a, b);
      break;
    case RISCV::MAXU:
      res = createUMax(a, b);
      break;
    default:
      assert(false);
    }
    updateOutputReg(res);
    break;
  }

  case RISCV::ANDN:
  case RISCV::ORN: {
    auto a = readFromRegOperand(1, i64ty);
    auto b = readFromRegOperand(2, i64ty);
    auto notb = createNot(b);

    Value *res{nullptr};
    switch (opcode) {
    case RISCV::ANDN:
      res = createAnd(a, notb);
      break;
    case RISCV::ORN:
      res = createOr(a, notb);
      break;
    default:
      assert(false);
    }
    updateOutputReg(res);
    break;
  }

  case RISCV::XNOR: {
    auto a = readFromRegOperand(1, i64ty);
    auto b = readFromRegOperand(2, i64ty);
    auto res = createNot(createXor(a, b));
    updateOutputReg(res);
    break;
  }

  case RISCV::SH1ADD:
  case RISCV::SH1ADD_UW:
  case RISCV::SH2ADD:
  case RISCV::SH2ADD_UW:
  case RISCV::SH3ADD:
  case RISCV::SH3ADD_UW: {
    auto a = readFromRegOperand(1, i64ty);
    auto b = readFromRegOperand(2, i64ty);

    uint64_t shamt = 0;
    switch (opcode) {
    case RISCV::SH1ADD:
    case RISCV::SH1ADD_UW:
      shamt = 1;
      break;
    case RISCV::SH2ADD:
    case RISCV::SH2ADD_UW:
      shamt = 2;
      break;
    case RISCV::SH3ADD:
    case RISCV::SH3ADD_UW:
      shamt = 3;
      break;
    default:
      assert(false);
    }
    assert(shamt != 0);
    auto shamt_val = getUnsignedIntConst(shamt, 64);

    Value *index{nullptr};
    switch (opcode) {
    case RISCV::SH1ADD:
    case RISCV::SH2ADD:
    case RISCV::SH3ADD:
      index = a;
      break;
    case RISCV::SH1ADD_UW:
    case RISCV::SH2ADD_UW:
    case RISCV::SH3ADD_UW:
      index = createZExt(createTrunc(a, i32ty), i64ty);
      break;
    default:
      assert(false);
    }
    assert(index);

    auto a_sh1 = createMaskedShl(index, shamt_val);
    auto res = createAdd(a_sh1, b);
    updateOutputReg(res);
    break;
  }

  default:
    visitError();
  }
}
