#include "backend_tv/riscv2llvm.h"
#include "backend_tv/abi.h"

#include "Target/RISCV/MCTargetDesc/RISCVMCAsmInfo.h"
#include "llvm/ADT/APFloat.h"
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

// The integer and LP64D floating-point ABIs use the same saved register indices:
// s0-s11 and fs0-fs11, respectively.
static bool isCalleeSavedReg(unsigned reg) {
  return reg == 8 || reg == 9 || (reg >= 18 && reg <= 27);
}

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

  // no work needed
  if (argTy->isPointerTy() || argTy->isVoidTy() || argTy->isFloatingPointTy())
    return V;

  if (argTy->isVectorTy() /* || argTy->isFloatingPointTy()*/) {
    assert(false && "vectors not supported yet");
  }

  assert(argTy->isIntegerTy());

  // an integer wider than XLEN is legalized into ceil(N/XLEN) limbs, so
  // its ABI location is that many registers wide and any extension
  // obligation reaches across the whole thing
  unsigned targetWidth = alignTo(getBitWidth(argTy), 64u);

  if (isZExt && getBitWidth(V) < targetWidth)
    V = createZExt(V, getIntTy(targetWidth));

  if (isSExt && getBitWidth(V) < targetWidth)
    V = createSExt(V, getIntTy(targetWidth));

  // FIXME -- zext i1 to i8

  // finally, pad out any remaining bits with unknown values
  auto junkBits = targetWidth - getBitWidth(V);
  if (junkBits > 0) {
    auto junk = createUnknownInt(junkBits);
    auto ext1 = createZExt(junk, getIntTy(targetWidth));
    auto shifted =
        createRawShl(ext1, getUnsignedIntConst(getBitWidth(V), targetWidth));
    auto ext2 = createZExt(V, getIntTy(targetWidth));
    V = createOr(shifted, ext2);
  }

  return V;
}

Value *riscv2llvm::checkIntegerABI(Value *V, Type *ty, bool isSExt,
                                 bool isZExt) {
  assert(ty->isIntegerTy());
  assert(!(isSExt && isZExt));
  unsigned width = getBitWidth(ty);
  // the ABI location is a whole number of registers wide
  unsigned regWidth = alignTo(width, 64u);
  assert(V->getType()->isIntegerTy(regWidth));
  auto value = width < regWidth ? createTrunc(V, ty) : V;
  // LLVM's RV64 signext/zeroext contracts extend to the full ABI location.
  // In particular, signext i32 requires bits 63:32 as well as the low word
  // to be correct. Unattributed integers leave their excess register bits
  // unspecified.
  if (!(isSExt || isZExt) || width == regWidth)
    return value;

  auto valid = createICmp(ICmpInst::ICMP_EQ, V,
                         enforceSExtZExt(value, isSExt, isZExt));
  return guardIntegerABI(value, valid);
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
  // The standard RV64 ABI requires SP to remain 16-byte aligned throughout
  // execution, including temporary updates that are restored before return.
  if (Reg == RISCV::X2)
    checkStackAlignment();
}

void riscv2llvm::checkStackAlignment() {
  auto sp = readFromReg(RISCV::X2, getIntTy(64));
  auto lowBits = createAnd(sp, getUnsignedIntConst(15, 64));
  // Use the opaque assertion so optimization cannot discard an intermediate
  // misaligned SP merely because the register is subsequently overwritten.
  assertTrue(createICmp(ICmpInst::ICMP_EQ, lowBits,
                        getUnsignedIntConst(0, 64)));
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

Value *riscv2llvm::canonicalizeNaN(Value *V) {
  assert(V->getType()->isFloatingPointTy());

  // RISC-V computational FP instructions produce the canonical NaN. Keep this
  // separate from updateFPReg: transfers and sign injection preserve payloads.
  auto isNaN = createIsFPClass(V, fcNan);
  auto canonical = APFloat::getQNaN(V->getType()->getFltSemantics());
  return createSelect(isNaN, ConstantFP::get(Ctx, canonical), V);
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

vector<Value *> riscv2llvm::marshallArgs(FunctionType *fTy,
                                       const CallInst &llvmCI) {
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
  // CCAssigner tells us where the assembly was obliged to leave each
  // argument; see backend_tv/abi.h
  CCAssigner CC(CCAssigner::Target::RISCV64, DL, fTy->getReturnType());
  vector<Value *> args;
  for (auto arg = fTy->param_begin(); arg != fTy->param_end(); ++arg) {
    Type *argTy = *arg;
    assert(argTy);
    if (argTy->isStructTy()) {
      *out << "\nERROR: we don't support structures in arguments yet\n\n";
      exit(-1);
    }
    if (argTy->isArrayTy()) {
      *out << "\nERROR: we don't support arrays in arguments yet\n\n";
      exit(-1);
    }
    auto loc = CC.assignArg(argTy);
    *out << "  " << getBitWidth(argTy) << "-bit arg at " << toString(loc)
         << "\n";
    if (!canPlace(loc)) {
      *out << "\nERROR: we don't support a call argument passed at "
           << toString(loc) << " yet\n\n";
      exit(-1);
    }
    auto &limb = loc.limbs[0];

    Value *param{nullptr};
    if (argTy->isVectorTy()) {
      assert(false && "Error: calling function with vector args is not supported yet\n\n");
    } else if (argTy->isIntegerTy() || argTy->isPointerTy()) {
      // a value wider than XLEN arrives in several limbs, least
      // significant first, and the ones that did not fit in registers sit
      // in the outgoing argument area at the current SP
      vector<Value *> limbs;
      for (auto &l : loc.limbs) {
        if (l.inReg) {
          limbs.push_back(readFromReg(RISCV::X10 + l.reg, getIntTy(64)));
        } else {
          auto SP = readFromReg(RISCV::X2, PointerType::get(Ctx, 0));
          auto addr = createGEP(getIntTy(8), SP,
                                {getUnsignedIntConst(l.stackOffset, 64)},
                                nextName());
          limbs.push_back(createLoad(getIntTy(64), addr));
        }
      }
      param = concatLimbs(limbs);
      if (argTy->isPointerTy()) {
        param = new IntToPtrInst(param, PointerType::get(Ctx, 0), "", LLVMBB);
      } else {
        // Use the parameter index, not its GPR index: FP arguments have their
        // own register sequence. paramHasAttr checks both call and declaration.
        unsigned argIdx = args.size();
        param = checkIntegerABI(param, argTy,
                                llvmCI.paramHasAttr(argIdx, Attribute::SExt),
                                llvmCI.paramHasAttr(argIdx, Attribute::ZExt));
      }
    } else if (argTy->isFloatingPointTy()) {
      // FIXME -- once the FP registers run out, CCAssigner says a float
      // travels in a GPR (and then on the stack). the callee side
      // implements that; this side does not yet.
      if (loc.kind != ArgLoc::Vector) {
        *out << "\nERROR: we don't support call arguments past the FP "
                "registers yet\n\n";
        exit(-1);
      }
      param = readFPABIReg(RISCV::F10_Q + limb.reg, argTy);
    } else {
      assert(false && "unknown arg type\n");
    }
    args.push_back(param);
  }
  *out << "marshalled up " << args.size() << " arguments\n";
  return args;
}

void riscv2llvm::doIndirectCall() {
  *out << "in doIndirectCall\n";

  auto llvmInst = getCurLLVMInst();
  if (!llvmInst) {
    *out << "\nERROR: no debuginfo mapping exists for an indirect call\n\n";
    exit(-1);
  }
  auto llvmCI = dyn_cast<CallInst>(llvmInst);
  if (!llvmCI) {
    *out << "\nERROR: debuginfo for an indirect call gave us something "
            "that's not a call instruction\n\n";
    exit(-1);
  }
  // A call to a known function is lifted from PseudoCALL or PseudoTAIL, so
  // anything arriving here should be a call through a pointer. Ask whether
  // the callee is a Function rather than using isIndirectCall(), which also
  // rejects a constant callee such as null or an inttoptr.
  if (llvmCI->getCalledFunction()) {
    *out << "\nERROR: expected an indirect JALR to map to an indirect "
            "call\n\n";
    exit(-1);
  }

  // An indirect call site carries its own signature; there is no separate
  // declaration that can disagree with it about arity or parameter types.
  auto *FnTy = llvmCI->getFunctionType();
  if (FnTy->isVarArg()) {
    *out << "\nERROR: varargs not supported\n\n";
    exit(-1);
  }

  // JALR transfers control to (rs1 + imm) & ~1. Only the imm == 0 form is
  // generated for a call; a nonzero offset would name something other than
  // the source-level callee.
  if (CurInst->getOperand(2).getImm() != 0) {
    *out << "\nERROR: indirect call through JALR with a nonzero offset is "
            "not supported\n\n";
    exit(-1);
  }

  // The bit that JALR discards is deliberately not masked off: the lifted
  // callee is compared against the source function pointer, and masking would
  // make an equal pointer compare unequal. A callee with bit 0 set is not
  // modeled.
  auto fnPtr = readPtrFromRegOperand(1);
  FunctionCallee FC(FnTy, fnPtr);
  doCall(FC, llvmCI, "");
}

void riscv2llvm::doCall(FunctionCallee FC, CallInst *llvmCI,
                        const string &calleeName) {
  *out << "entering doCall()\n";
  assert(llvmCI);

  // Establish the ABI precondition before abstracting either a normal or a
  // tail call. Restoring SP later does not make a misaligned call valid.
  checkStackAlignment();

  for (auto &arg : FC.getFunctionType()->params()) {
    if (auto vTy = dyn_cast<VectorType>(arg))
      checkVectorTy(vTy);
  }
  if (auto RT = dyn_cast<VectorType>(FC.getFunctionType()->getReturnType()))
    checkVectorTy(RT);

  auto args = marshallArgs(FC.getFunctionType(), *llvmCI);

  // ugh -- these functions have an LLVM "immediate" as their last
  // argument; this is not present in the assembly at all, we have
  // to provide it by hand
  bool returnsDestPtr = false;
  if (calleeName == "llvm.memset.p0.i64" ||
      calleeName == "llvm.memset.p0.i32" ||
      calleeName == "llvm.memcpy.p0.p0.i64" ||
      calleeName == "llvm.memmove.p0.p0.i64") {
    *out << "adding constant Boolean as args[3]\n";
    args[3] = getBoolConst(false);
    // the assembly called libc memset/memcpy/memmove, which return their
    // destination argument; we model them with LLVM intrinsics that return
    // void, so a0 has to be installed by hand below
    returnsDestPtr = true;
  }

  auto CI = CallInst::Create(FC, args, "", LLVMBB);

  bool sext{false}, zext{false};

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

  // Calls overwrite ra; its precise link address is not modeled yet.
  invalidateReg(RISCV::X1, 64);
  // Invalidate t0-t6 and a0-a7 before installing the return value below.
  for (unsigned reg = 5; reg <= 31; ++reg)
    if (!isCalleeSavedReg(reg))
      invalidateReg(RISCV::X0 + reg, 64);

  // The supported LP64D ABI preserves fs0-fs11, but not ft0-ft11 or fa0-fa7.
  // Only 64 bits are architectural; keep the unused backing bits NaN-boxed.
  for (unsigned reg = 0; reg < 32; ++reg) {
    if (!isCalleeSavedReg(reg)) {
      auto value = createBitCast(createUnknownInt(64), getFPType(64));
      updateFPReg(value, RISCV::F0_Q + reg);
    }
  }

  if (returnsDestPtr) {
    updateReg(args[0], RISCV::X10);
    return;
  }

  auto retTy = FC.getFunctionType()->getReturnType();
  auto retLoc = CCAssigner(CCAssigner::Target::RISCV64, DL, retTy).retLoc();
  if (retTy->isIntegerTy() || retTy->isPointerTy()) {
    assert(retLoc.kind == ArgLoc::Direct &&
           "indirect returns not supported yet");
    auto limbs = retLoc.limbs.size() > 1
                     ? splitIntoLimbs(RV, retLoc.limbBits)
                     : vector<Value *>{RV};
    for (unsigned i = 0, n = retLoc.limbs.size(); i != n; ++i) {
      assert(retLoc.limbs[i].inReg);
      updateReg(limbs[i], RISCV::X10 + retLoc.limbs[i].reg);
    }
  } else if (retTy->isFloatingPointTy()) {
    updateFPReg(RV, RISCV::F10_Q + retLoc.limbs[0].reg);
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

Value *riscv2llvm::readFromFPRegOperand(int idx, Type *ty, bool checkNaNBox) {
  auto op = CurInst->getOperand(idx);
  assert(op.isReg());
  auto value = readFromFPReg(op.getReg(), ty);
  auto width = getBitWidth(value);
  if (!checkNaNBox || width == 128)
    return value;

  // Non-transfer instructions treat improperly boxed inputs as canonical NaNs.
  auto boxed = isNaNBoxed(op.getReg(), width);
  auto nan = ConstantFP::get(Ctx, APFloat::getQNaN(ty->getFltSemantics()));
  return createSelect(boxed, value, nan);
}

Value *riscv2llvm::isNaNBoxed(unsigned Reg, unsigned width) {
  assert(width == 16 || width == 32 || width == 64 || width == 128);
  if (width == 128)
    return getBoolConst(true);

  // updateFPReg boxes every narrower write to the 128-bit backing register,
  // including the unused upper bits when the architectural FLEN is smaller.
  auto bits = createLoad(getIntTy(128), lookupFPReg(Reg));
  auto mask = ConstantInt::get(Ctx, APInt::getHighBitsSet(128, 128 - width));
  return createICmp(ICmpInst::ICMP_EQ, createAnd(bits, mask), mask);
}

Value *riscv2llvm::readFPABIReg(unsigned Reg, Type *ty) {
  // Call arguments and return values must satisfy the ABI's NaN-boxing rule.
  // Assert the representation rather than replacing an invalid box with a NaN.
  assertTrue(isNaNBoxed(Reg, getBitWidth(ty)));
  return readFromFPReg(Reg, ty);
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
  doReturn(readFromReg(RISCV::X1, getIntTy(64)));
}

void riscv2llvm::doReturn(Value *returnAddress) {
  auto i64ty = getIntTy(64);

  // The ABI requires SP to be restored on every return path.
  assertSame(initialSP, readFromReg(RISCV::X2, i64ty));
  for (unsigned reg = 0; reg < 32; ++reg) {
    // gp and tp are fixed registers, in addition to the callee-saved GPRs.
    if (reg == 3 || reg == 4 || isCalleeSavedReg(reg))
      assertSame(initialReg[reg], readFromReg(RISCV::X0 + reg, i64ty));
    if (isCalleeSavedReg(reg)) {
      // Compare the LP64D-preserved bits, including NaN payloads and signs.
      auto bits = createLoad(i64ty, lookupFPReg(RISCV::F0_Q + reg));
      assertSame(initialFPReg[reg], bits);
    }
  }
  // JALR clears bit 0 of its target. Compare effective destinations so a
  // change to that bit alone does not invalidate an otherwise correct return.
  auto mask = getSignedIntConst(-2, 64);
  assertSame(createAnd(initialReg[1], mask), createAnd(returnAddress, mask));

  auto *retTyp = srcFn->getReturnType();
  // where the assembly was obliged to leave the result; see
  // backend_tv/abi.h
  auto retLoc = CCAssigner(CCAssigner::Target::RISCV64, DL, retTyp).retLoc();

  if (retTyp->isVoidTy()) {
    createReturn(nullptr);
  } else if (retTyp->isFloatingPointTy()) {
    createReturn(readFPABIReg(RISCV::F10_Q + retLoc.limbs[0].reg, retTyp));
  } else {
    assert(retLoc.kind == ArgLoc::Direct &&
           "indirect returns not supported yet");
    // FIXME handle vectors
    // a value wider than XLEN comes back in several registers, least
    // significant first; checkIntegerABI narrows it back down and
    // discards the top limb's unspecified high bits
    vector<Value *> limbs;
    for (auto &limb : retLoc.limbs) {
      assert(limb.inReg);
      limbs.push_back(readFromReg(RISCV::X10 + limb.reg, i64ty));
    }
    Value *retVal = concatLimbs(limbs);
    if (retTyp->isPointerTy()) {
      retVal = new IntToPtrInst(retVal, PointerType::get(Ctx, 0), "", LLVMBB);
    } else {
      retVal = checkIntegerABI(retVal, retTyp,
                               srcFn->hasRetAttribute(Attribute::SExt),
                               srcFn->hasRetAttribute(Attribute::ZExt));
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
    initialReg[Reg - RISCV::X0] = readFromReg(Reg, i64ty);
  }

  // allocate storage for the float register file
  for (unsigned Reg = RISCV::F0_Q; Reg <= RISCV::F31_Q; ++Reg) {
    stringstream Name;
    Name << "F" << Reg - RISCV::F0_Q;
    createRegStorage(Reg, 128, Name.str());
    if (isCalleeSavedReg(Reg - RISCV::F0_Q))
      initialFPReg[Reg - RISCV::F0_Q] = createLoad(i64ty, lookupFPReg(Reg));
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

  // implement the callee side of the ABI: put each of the source
  // function's arguments where a caller would have left it. CCAssigner
  // works out where that is; see backend_tv/abi.h.
  //
  // FIXME -- the placement below only handles values that fit in a single
  // register or stack slot. wider integers are rejected by checkSupport()
  // for now, so every location we see here has exactly one limb.
  CCAssigner CC(CCAssigner::Target::RISCV64, DL, srcFn->getReturnType());

  for (Function::arg_iterator arg = liftedFn->arg_begin(),
                              E = liftedFn->arg_end(),
                              srcArg = srcFn->arg_begin();
       arg != E; ++arg, ++srcArg) {
    auto loc = CC.assignArg(arg->getType());
    *out << "  processing " << getBitWidth(arg) << "-bit arg at "
         << toString(loc);
    auto *argTy = arg->getType();

    // FIXME -- this isn't correct for RISC-V, but since it's on the
    // caller side, let's wait and see if we can find a false alarm
    // before fixing it
    auto *val =
        enforceSExtZExt(arg, srcArg->hasSExtAttr(), srcArg->hasZExtAttr());

    if (loc.kind == ArgLoc::Vector) {
      assert(loc.limbs[0].inReg);
      updateFPReg(val, RISCV::F10_Q + loc.limbs[0].reg);
    } else {
      if (argTy->isFloatingPointTy() && loc.limbs[0].inReg) {
        // a float that ran out of FP registers travels in a GPR. the
        // integer convention leaves the excess register bits unspecified;
        // NaN-boxing applies only to arguments passed in FP registers.
        val = createBitCast(val, getIntTy(getBitWidth(val)));
        val = enforceSExtZExt(val, false, false);
      }

      // enforceSExtZExt has widened val to a whole number of
      // register-sized limbs, filling the high bits of the top limb with
      // unknown values because the ABI leaves them unspecified
      auto limbVals = loc.limbs.size() > 1
                          ? splitIntoLimbs(val, loc.limbBits)
                          : vector<Value *>{val};
      assert(limbVals.size() == loc.limbs.size());

      for (unsigned i = 0, n = loc.limbs.size(); i != n; ++i) {
        auto &limb = loc.limbs[i];
        if (limb.inReg) {
          createStore(limbVals[i], RegFile[RISCV::X10 + limb.reg]);
        } else {
          if (limb.stackOffset / 8 >= numStackSlots) {
            *out << "\nERROR: maximum stack slots for parameter values "
                    "exceeded\n\n";
            exit(-1);
          }
          auto addr =
              createGEP(i8ty, paramBase,
                        {getUnsignedIntConst(limb.stackOffset, 64)}, "");
          createStore(limbVals[i], addr);
        }
      }
    }
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
void riscv2llvm::checkTypeSupport(Type *ty) {
  if (ty->isVectorTy()) {
    *out << "\nERROR: vectors not yet supported\n\n";
    exit(-1);
  }
  // RVA22U64 gives us Zfhmin (so f16 arithmetic is promoted to f32) but
  // neither Q nor bfloat, and those get turned into libcalls such as
  // __addtf3 that we cannot lift; reject them up front instead
  if (ty->isFloatingPointTy() &&
      !(ty->isHalfTy() || ty->isFloatTy() || ty->isDoubleTy())) {
    *out << "\nERROR: only half, float, and double supported (not bfloat, "
            "fp128, etc.)\n\n";
    exit(-1);
  }
}

void riscv2llvm::checkCallingConv(Function *fn) {
  if (fn->getCallingConv() != CallingConv::C) {
    *out << "\nERROR: Only the C calling convention is supported\n\n";
    exit(-1);
  }
}
