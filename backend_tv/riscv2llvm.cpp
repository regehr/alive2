#include "backend_tv/riscv2llvm.h"

#include "Target/RISCV/MCTargetDesc/RISCVMCAsmInfo.h"
#include "llvm/ADT/APInt.h"
#include "llvm/BinaryFormat/ELF.h"

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

riscv2llvm::riscv2llvm(
    Function *srcFn, unique_ptr<MemoryBuffer> MB,
    std::unordered_map<unsigned, llvm::Instruction *> &lineMap,
    std::ostream *out, const llvm::Target *Targ, llvm::Triple DefaultTT,
    const char *DefaultCPU, const char *DefaultFeatures)
    : mc2llvm(srcFn, std::move(MB), lineMap, out, Targ, DefaultTT, DefaultCPU,
              DefaultFeatures) {}

// TODO -- move this up to mc2llvm so the ARM lifter can use it too
tuple<BasicBlock *, BasicBlock *> riscv2llvm::getBranchTargetsOperand(int op) {
  auto &jmp_tgt_op = CurInst->getOperand(op);
  assert(jmp_tgt_op.isExpr() && "expected expression");
  assert((jmp_tgt_op.getExpr()->getKind() == MCExpr::ExprKind::SymbolRef) &&
         "expected symbol ref as bcc operand");
  const MCSymbolRefExpr &SRE = cast<MCSymbolRefExpr>(*jmp_tgt_op.getExpr());
  const MCSymbol &Sym = SRE.getSymbol();
  auto dst_true = getBBByName(Sym.getName());
  assert(MCBB->getSuccs().size() == 1 || MCBB->getSuccs().size() == 2);
  const string *dst_false_name = nullptr;
  for (auto &succ : MCBB->getSuccs()) {
    if (succ->getName() != Sym.getName()) {
      dst_false_name = &succ->getName();
      break;
    }
  }
  auto dst_false =
      getBBByName(dst_false_name ? *dst_false_name : Sym.getName());
  return make_pair(dst_true, dst_false);
}

unsigned riscv2llvm::branchInst() {
  return RISCV::C_J;
}

unsigned riscv2llvm::sentinelNOP() {
  return RISCV::C_NOP_HINT;
}

Value *riscv2llvm::enforceSExtZExt(Value *V, bool isSExt, bool isZExt) {
  auto argTy = V->getType();
  unsigned targetWidth = 64;

  // no work needed
  if (argTy->isPointerTy() || argTy->isVoidTy() || argTy->isFloatingPointTy())
    return V;

  if (argTy->isVectorTy() /* || argTy->isFloatingPointTy()*/) {
    assert(false && "vectors not supported yet");
  }

  assert(argTy->isIntegerTy());

  if (isZExt && getBitWidth(V) < targetWidth)
    V = createZExt(V, getIntTy(targetWidth));

  if (isSExt && getBitWidth(V) < targetWidth)
    V = createSExt(V, getIntTy(targetWidth));

  // FIXME -- zext i1 to i8

  // finally, pad out any remaining bits with junk (frozen poisons)
  auto junkBits = targetWidth - getBitWidth(V);
  if (junkBits > 0) {
    auto junk = createFreeze(PoisonValue::get(getIntTy(junkBits)));
    auto ext1 = createZExt(junk, getIntTy(targetWidth));
    auto shifted =
        createRawShl(ext1, getUnsignedIntConst(getBitWidth(V), targetWidth));
    auto ext2 = createZExt(V, getIntTy(targetWidth));
    V = createOr(shifted, ext2);
  }

  return V;
}

llvm::AllocaInst *riscv2llvm::get_reg(aslp::reg_t regtype, uint64_t num) {
  assert(false);
  return nullptr;
}

Value *riscv2llvm::lookupReg(unsigned Reg) {
  assert(Reg >= RISCV::X0 && Reg <= RISCV::X31);
  return RegFile[Reg];
}

Value *riscv2llvm::lookupFPReg(unsigned Reg) {
  // for some reason the order in the enum is D, F, H, Q
  assert((Reg >= RISCV::F0_D && Reg <= RISCV::F31_Q) || Reg == RISCV::FCSR);

  // there's only one file for all FP widths
  if (Reg >= RISCV::F0_D && Reg <= RISCV::F31_D) {
    Reg = Reg - RISCV::F0_D + RISCV::F0_Q;
  } else if (Reg >= RISCV::F0_F && Reg <= RISCV::F31_F) {
    Reg = Reg - RISCV::F0_F + RISCV::F0_Q;
  } else if (Reg >= RISCV::F0_H && Reg <= RISCV::F31_H) {
    Reg = Reg - RISCV::F0_H + RISCV::F0_Q;
  }
  return RegFile[Reg];
}

void riscv2llvm::updateReg(Value *V, uint64_t Reg) {
  // important -- squash updates to the zero register
  if (Reg == RISCV::X0)
    return;
  createStore(V, lookupReg(Reg));
}

void riscv2llvm::updateFPReg(Value *V, uint64_t Reg) {
  auto W = getBitWidth(V);
  if (W == 128)
    createStore(V, lookupFPReg(Reg));

  // NaN-box smaller FP types
  auto bits = createBitCast(V, getIntTy(W));
  auto extended = createZExt(bits, getIntTy(128));
  auto maskAP = llvm::APInt::getHighBitsSet(128, 128 - W);
  auto mask = llvm::ConstantInt::get(Ctx, maskAP);
  auto nanBoxed = createOr(mask, extended);
  createStore(nanBoxed, lookupFPReg(Reg));
}

unsigned riscv2llvm::getRegSize(unsigned Reg) {
  if (Reg >= RISCV::X0 && Reg <= RISCV::X31)
    return /*XLen=*/64;
  if (Reg >= RISCV::F0_D && Reg <= RISCV::F31_D)
    return 64;
  if (Reg >= RISCV::F0_F && Reg <= RISCV::F31_F)
    return 32;
  if (Reg >= RISCV::F0_H && Reg <= RISCV::F31_H)
    return 16;
  if (Reg >= RISCV::F0_Q && Reg <= RISCV::F31_Q)
    return 128;
  if (Reg == RISCV::FCSR)
    return 32;
  assert(false && "unhandled register");
}

void riscv2llvm::updateOutputReg(Value *V, bool SExt) {
  auto W = getBitWidth(V);
  auto outputReg = CurInst->getOperand(0).getReg();
  if (V->getType()->isFloatingPointTy()) {
    updateFPReg(V, outputReg);
  } else {
    if (SExt) {
      if (W < 64)
        V = createSExt(V, getIntTy(64));
    } else {
      assert(W == 64);
    }
    updateReg(V, outputReg);
  }
}

Value *riscv2llvm::makeLoadWithOffset(Value *base, Value *offset, int size) {
  assert(false);
  return nullptr;
}

Value *riscv2llvm::getIndexedElement(unsigned idx, unsigned eltSize,
                                     unsigned reg) {
  assert(false);
  return nullptr;
}

vector<Value *> riscv2llvm::marshallArgs(FunctionType *fTy) {
  *out << "entering marshallArgs()\n";
  assert(fTy);
  if (fTy->getReturnType()->isStructTy()) {
    *out << "\nERROR: we don't support structures in return values yet\n\n";
    exit(-1);
  }
  if (fTy->getReturnType()->isArrayTy()) {
    *out << "\nERROR: we don't support arrays in return values yet\n\n";
    exit(-1);
  }
  unsigned vecArgNum = 0;
  unsigned scalarArgNum = 0;
  unsigned floatArgNum = 0;
  // unsigned stackSlot = 0;
  vector<Value *> args;
  for (auto arg = fTy->param_begin(); arg != fTy->param_end(); ++arg) {
    Type *argTy = *arg;
    assert(argTy);
    *out << "  vecArgNum = " << vecArgNum << " scalarArgNum = " << scalarArgNum
         << "\n";
    if (argTy->isStructTy()) {
      *out << "\nERROR: we don't support structures in arguments yet\n\n";
      exit(-1);
    }
    if (argTy->isArrayTy()) {
      *out << "\nERROR: we don't support arrays in arguments yet\n\n";
      exit(-1);
    }
    Value *param{nullptr};
    if (argTy->isVectorTy()) {
      assert(false && "Error: calling function with vector args is not supported yet\n\n");
#if 0
      if (vecArgNum < 8) {
        param = readFromReg(AArch64::Q0 + vecArgNum, argTy);
        ++vecArgNum;
      } else {
        auto sz = getBitWidth(argTy);
        if (sz > 64 && ((stackSlot % 2) != 0)) {
          ++stackSlot;
          *out << "aligning stack slot for large vector parameter\n";
        }
        *out << "vector parameter going on stack with size = " << sz << "\n";
        auto SP = readPtrFromReg(AArch64::SP);
        auto addr = createGEP(getIntTy(64), SP,
                              {getUnsignedIntConst(stackSlot, 64)}, nextName());
        param = createBitCast(createLoad(getIntTy(sz), addr), argTy);
        ++stackSlot;
        if (sz > 64)
          ++stackSlot;
      }
#endif
    } else if (argTy->isIntegerTy() || argTy->isPointerTy()) {
      // FIXME check signext and zeroext
      if (scalarArgNum < 8) {
        param = readFromReg(RISCV::X10 + scalarArgNum, getIntTy(64));
        ++scalarArgNum;
      } else {
        assert(false);
#if 0
        auto SP = readPtrFromReg(AArch64::SP);
        auto addr = createGEP(getIntTy(64), SP,
                              {getUnsignedIntConst(stackSlot, 64)}, nextName());
        param = createLoad(getIntTy(64), addr);
        ++stackSlot;
#endif
      }
      if (argTy->isPointerTy()) {
        param = new IntToPtrInst(param, PointerType::get(Ctx, 0), "", LLVMBB);
      } else {
        assert(argTy->getIntegerBitWidth() <= 64);
        if (argTy->getIntegerBitWidth() < 64)
          param = createTrunc(param, getIntTy(argTy->getIntegerBitWidth()));
      }
    } else if (argTy->isFloatingPointTy()) {
      if (floatArgNum < 8) {
        param = readFromFPReg(RISCV::F10_Q + floatArgNum, argTy);
        ++floatArgNum;
      } else {
        assert(false);
      }
    } else {
      assert(false && "unknown arg type\n");
    }
    args.push_back(param);
  }
  *out << "marshalled up " << args.size() << " arguments\n";
  return args;
}

void riscv2llvm::doCall(FunctionCallee FC, CallInst *llvmCI,
                        const string &calleeName) {
  *out << "entering doCall()\n";

  for (auto &arg : FC.getFunctionType()->params()) {
    if (auto vTy = dyn_cast<VectorType>(arg))
      checkVectorTy(vTy);
  }
  if (auto RT = dyn_cast<VectorType>(FC.getFunctionType()->getReturnType()))
    checkVectorTy(RT);

  auto args = marshallArgs(FC.getFunctionType());

  // ugh -- these functions have an LLVM "immediate" as their last
  // argument; this is not present in the assembly at all, we have
  // to provide it by hand
  if (calleeName == "llvm.memset.p0.i64" ||
      calleeName == "llvm.memset.p0.i32" ||
      calleeName == "llvm.memcpy.p0.p0.i64" ||
      calleeName == "llvm.memmove.p0.p0.i64") {
    *out << "adding constant Boolean as args[3]\n";
    args[3] = getBoolConst(false);
  }

  auto CI = CallInst::Create(FC, args, "", LLVMBB);

  bool sext{false}, zext{false};

  assert(llvmCI);
  if (llvmCI->hasFnAttr(Attribute::NoReturn)) {
    auto a = CI->getAttributes();
    auto a2 = a.addFnAttribute(Ctx, Attribute::NoReturn);
    CI->setAttributes(a2);
  }
  // NB we have to check for both function attributes and call site
  // attributes
  if (llvmCI->hasRetAttr(Attribute::SExt))
    sext = true;
  if (llvmCI->hasRetAttr(Attribute::ZExt))
    zext = true;
  auto calledFn = llvmCI->getCalledFunction();
  if (calledFn) {
    if (calledFn->hasRetAttribute(Attribute::SExt))
      sext = true;
    if (calledFn->hasRetAttribute(Attribute::ZExt))
      zext = true;
  }

  auto RV = enforceSExtZExt(CI, sext, zext);

  // invalidate machine state that is not guaranteed to be preserved across a
  // call
  for (unsigned reg = 5; reg <= 7; ++reg)
    invalidateReg(RISCV::X0 + reg, 64);
  for (unsigned reg = 28; reg <= 31; ++reg)
    invalidateReg(RISCV::X0 + reg, 64);

#if 0
  // invalidate argument regs?
  for (unsigned reg = 10; reg <= 17; ++reg)
    invalidateReg(RISCV::X0 + reg, 64);
#endif

  auto retTy = FC.getFunctionType()->getReturnType();
  if (retTy->isIntegerTy() || retTy->isPointerTy()) {
    updateReg(RV, RISCV::X10);
  } else if (retTy->isFloatingPointTy()) {
    updateFPReg(RV, RISCV::F10_Q);
  } else if (retTy->isVectorTy()) {
    assert(false);
    // updateReg(RV, AArch64::Q0);
  } else {
    assert(retTy->isVoidTy());
  }
}

Value *riscv2llvm::readFromRegOperand(int idx, Type *ty) {
  auto op = CurInst->getOperand(idx);
  assert(op.isReg());
  return readFromReg(op.getReg(), ty);
}

Value *riscv2llvm::readFromFPRegOperand(int idx, Type *ty) {
  auto op = CurInst->getOperand(idx);
  assert(op.isReg());
  return readFromFPReg(op.getReg(), ty);
}

Value *riscv2llvm::readPtrFromRegOperand(int idx) {
  auto ptrTy = llvm::PointerType::get(Ctx, 0);
  return readFromRegOperand(idx, ptrTy);
}

Value *riscv2llvm::readFromImmOperand(int idx, unsigned immed_width,
                                      unsigned result_width) {
  assert(immed_width <= 20);
  assert(result_width >= immed_width);
  auto op = CurInst->getOperand(idx);
  assert(op.isImm());
  auto imm_int = op.getImm() & ((1U << immed_width) - 1);
  Value *imm = getUnsignedIntConst(imm_int, immed_width);
  if (result_width > immed_width)
    imm = createSExt(imm, getIntTy(result_width));
  return imm;
}

Value *riscv2llvm::readFromReg(unsigned Reg, Type *ty) {
  assert(ty->isIntOrPtrTy());
  auto addr = lookupReg(Reg);
  return createLoad(ty, addr);
}

Value *riscv2llvm::readFromFPReg(unsigned Reg, Type *ty) {
  assert(ty->isFloatingPointTy());
  auto addr = lookupFPReg(Reg);
  return createLoad(ty, addr);
}

void riscv2llvm::doReturn() {
  auto i32ty = getIntTy(32);
  auto i64ty = getIntTy(64);

  // FIXME add ABI checks -- this function needs a bunch of work!

  auto *retTyp = srcFn->getReturnType();
  if (retTyp->isVoidTy()) {
    createReturn(nullptr);
  } else if (retTyp->isFloatingPointTy()) {
    createReturn(readFromFPReg(RISCV::F10_Q, retTyp));
  } else {
    Value *retVal{nullptr};
    // FIXME handle vectors
    retVal = readFromReg(RISCV::X10, i64ty);
    if (retTyp->isPointerTy()) {
      retVal = new IntToPtrInst(retVal, PointerType::get(Ctx, 0), "", LLVMBB);
    } else {
      auto retWidth = DL.getTypeSizeInBits(retTyp);
      auto retValWidth = DL.getTypeSizeInBits(retVal->getType());

      if (retWidth < retValWidth)
        retVal = createTrunc(retVal, getIntTy(retWidth));

      // mask off any don't-care bits
      if (has_ret_attr && (origRetWidth < 32)) {
        assert(retWidth >= origRetWidth);
        assert(retWidth == 64);
        auto trunc = createTrunc(retVal, i32ty);
        retVal = createZExt(trunc, i64ty);
      }
    }
    createReturn(retVal);
  }
}

void riscv2llvm::platformInit() {
  auto i8ty = getIntTy(8);
  auto i64ty = getIntTy(64);

  // allocate storage for the main register file
  for (unsigned Reg = RISCV::X0; Reg <= RISCV::X31; ++Reg) {
    stringstream Name;
    Name << "X" << Reg - RISCV::X0;
    createRegStorage(Reg, 64, Name.str());
    // initialReg[Reg - RISCV::X0] = readFromReg(Reg, i64ty);
  }

  // allocate storage for the float register file
  for (unsigned Reg = RISCV::F0_Q; Reg <= RISCV::F31_Q; ++Reg) {
    stringstream Name;
    Name << "F" << Reg - RISCV::F0_Q;
    createRegStorage(Reg, 128, Name.str());
  }

  // allocate floating-point control and status register
  createRegStorage(RISCV::FCSR, 32, "FCSR");

  *out << "created scalar registers\n";

  // TODO vector registers

  auto paramBase =
      createGEP(i8ty, stackMem, {getUnsignedIntConst(stackBytes, 64)}, "");
  createStore(paramBase, RegFile[RISCV::X2]);
  initialSP = readFromReg(RISCV::X2, i64ty);

  // initializing to zero makes loads from XZR work; stores are
  // handled in updateReg()
  createStore(getUnsignedIntConst(0, 64), RegFile[RISCV::X0]);

  *out << "about to do callee-side ABI stuff\n";

  // implement the callee side of the ABI; FIXME -- this code only
  // supports integer parameters <= 64 bits and will require
  // significant generalization to handle large parameters
  unsigned vecArgNum = 0;
  unsigned scalarArgNum = 0;
  unsigned floatArgNum = 0;
  unsigned stackSlot = 0;

  for (Function::arg_iterator arg = liftedFn->arg_begin(),
                              E = liftedFn->arg_end(),
                              srcArg = srcFn->arg_begin();
       arg != E; ++arg, ++srcArg) {
    *out << "  processing " << getBitWidth(arg)
         << "-bit arg with vecArgNum = " << vecArgNum
         << ", scalarArgNum = " << scalarArgNum
         << ", stackSlot = " << stackSlot;
    auto *argTy = arg->getType();

    // FIXME -- this isn't correct for RISC-V, but since it's on the
    // caller side, let's wait and see if we can find a false alarm
    // before fixing it
    auto *val =
        enforceSExtZExt(arg, srcArg->hasSExtAttr(), srcArg->hasZExtAttr());

    // first 8 integer parameters go in integer registers starting at X10
    if ((argTy->isIntegerTy() || argTy->isPointerTy()) && scalarArgNum < 8) {
      auto Reg = RISCV::X10 + scalarArgNum;
      createStore(val, RegFile[Reg]);
      ++scalarArgNum;
      goto end;
    }

    // TODO: support args > 64 bits (possibly just remove check in mc2llvm.cpp)
    if (argTy->isFloatingPointTy()) {
      if (floatArgNum < 8) {
        auto Reg = RISCV::F10_Q + floatArgNum;
        createStore(val, lookupFPReg(Reg));
        ++floatArgNum;
        goto end;
      }
      // Otherwise pass the argument by GPR.
      if (scalarArgNum < 8) {
        auto Reg = RISCV::X10 + scalarArgNum;
        unsigned bitWidth = getBitWidth(val);
        Value *intVal = createBitCast(val, getIntTy(bitWidth));
        // 1-extended (NaN-boxed) to FLEN bits.
        if (bitWidth < 64) {
          intVal = createOr(
              createZExt(intVal, getIntTy(64)),
              ConstantInt::get(getIntTy(64),
                               APInt::getHighBitsSet(64, 64 - bitWidth)));
        }
        createStore(intVal, RegFile[Reg]);
        ++scalarArgNum;
        goto end;
      }
    }

#if 0
    // first 8 vector/FP parameters go in the first 8 vector registers
    if ((argTy->isVectorTy() || argTy->isFloatingPointTy()) && vecArgNum < 8) {
      auto Reg = AArch64::Q0 + vecArgNum;
      createStore(val, RegFile[Reg]);
      ++vecArgNum;
      goto end;
    }
#endif

    // everything else goes onto the stack

    {
#if 0
      // 128-bit alignment required for 128-bit arguments
      if ((getBitWidth(val) == 128) && ((stackSlot % 2) != 0)) {
        ++stackSlot;
        *out << " (actual stack slot = " << stackSlot << ")";
      }
#endif

      if (stackSlot >= numStackSlots) {
        *out << "\nERROR: maximum stack slots for parameter values "
                "exceeded\n\n";
        exit(-1);
      }

      auto addr =
          createGEP(i64ty, paramBase, {getUnsignedIntConst(stackSlot, 64)}, "");
      createStore(val, addr);

      if (getBitWidth(val) == 64) {
        stackSlot += 1;
#if 0
      } else if (getBitWidth(val) == 128) {
        stackSlot += 2;
#endif
      } else {
        assert(false);
      }
    }
  end:
    *out << "\n";
  }

  *out << "done with callee-side ABI stuff\n";
}

/*
 * RISC-V memory operations are of the form:
 *
 *   ld/st src/dst reg, %lo(var)(addr_reg)
 *
 * this function returns an LLVM pointer that is the lifted equivalent
 * of the composite second operand
 *
 * FIXME -- we need to verify that the hi part is actually present in
 * the specified register. but for now we'll just assume that the
 * backend got this right.
 *
 * FIXME -- we should be dealing with the offset computation in
 * portable code, not in RISC-V code. there's even some portable code
 * in mc2llvm.cpp that kind of almost handles this already
 */
Value *riscv2llvm::getPointerFromMCExpr() {
  auto op1 = CurInst->getOperand(1);
  auto op2 = CurInst->getOperand(2);
  assert(op1.isReg());
  assert(op2.isExpr());
  auto rvExpr = dyn_cast<MCSpecifierExpr>(op2.getExpr());
  assert(rvExpr);
  auto specifier = rvExpr->getSpecifier();
  assert(specifier == RISCV::S_LO);
  auto addrExpr = rvExpr->getSubExpr();
  assert(addrExpr);
  if (auto binaryExpr = dyn_cast<MCBinaryExpr>(addrExpr)) {
    auto opc = binaryExpr->getOpcode();
    if (opc == MCBinaryExpr::Add || opc == MCBinaryExpr::Sub) {
      auto LHS = binaryExpr->getLHS();
      auto RHS = binaryExpr->getRHS();
      assert(LHS && LHS->getKind() == MCExpr::SymbolRef);
      auto ptr = lookupExprVar(*LHS);
      assert(RHS);
      auto CE = dyn_cast<MCConstantExpr>(RHS);
      assert(CE);
      auto offset = CE->getValue();
      auto i8ty = getIntTy(8);
      Value *offsetVal = getSignedIntConst(offset, 64);
      auto zero = getSignedIntConst(0, 64);
      if (opc == MCBinaryExpr::Sub)
        offsetVal = createSub(zero, offsetVal);
      ptr = createGEP(i8ty, ptr, {offsetVal}, nextName());
      return ptr;
    } else {
      assert(false && "unhandled MCBinaryExpr");
    }
  } else {
    assert(addrExpr->getKind() == MCExpr::SymbolRef);
    return lookupExprVar(*addrExpr);
  }
}

Value *riscv2llvm::getPointerOperand() {
  if (CurInst->getOperand(2).isImm()) {
    auto imm = readFromImmOperand(2, 12, 64);
    return createGEP(getIntTy(8),
                     readFromRegOperand(1, PointerType::get(Ctx, 0)), {imm},
                     nextName());
  }

  return getPointerFromMCExpr();
}

void riscv2llvm::checkArgSupport(Argument &arg) {}
void riscv2llvm::checkFuncSupport(Function &func) {}
void riscv2llvm::checkTypeSupport(Type *ty) {
  if (ty->isVectorTy()) {
    *out << "\nERROR: vectors not yet supported\n\n";
    exit(-1);
  }
}

void riscv2llvm::checkCallingConv(Function *fn) {
  if (fn->getCallingConv() != CallingConv::C) {
    *out << "\nERROR: Only the C calling convention is supported\n\n";
    exit(-1);
  }
}
