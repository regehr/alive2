/*
 * lifting for the RVV vector extension, as included in RVA23U64
 *
 * the vector register file and the element-policy helpers (writeVGroup and
 * friends) live in riscv2llvm.cpp. this file holds the static tracking of
 * vtype and the lifting of individual vector instructions.
 *
 * reference: src/unpriv/vector-common.adoc in the RISC-V ISA manual
 */

#include "backend_tv/mc2llvm.h"
#include "backend_tv/riscv2llvm.h"

#include <cassert>
#include <cstdint>
#include <vector>

#define GET_INSTRINFO_ENUM
#include "Target/RISCV/RISCVGenInstrInfo.inc"

#define GET_REGINFO_ENUM
#include "Target/RISCV/RISCVGenRegisterInfo.inc"

using namespace std;
using namespace lifter;
using namespace llvm;

// CSR numbers of the read-only vector CSRs
static constexpr int64_t CSR_VL = 0xC20;
static constexpr int64_t CSR_VTYPE = 0xC21;
static constexpr int64_t CSR_VLENB = 0xC22;

void riscv2llvm::vectorError(const string &msg) {
  out->flush();
  *out << "\nERROR: " << msg << "\n\n";
  out->flush();
  exit(-1);
}

/*
 * static vtype tracking
 */

// the effect of one instruction on the statically known vtype. this has to
// agree with what the instruction handlers do to curVType while lifting:
// vset{i}vli set it, and doCall() forgets it
int64_t riscv2llvm::vtypeTransfer(const MCInst &I, int64_t in) {
  switch (I.getOpcode()) {
  case RISCV::VSETVLI:
  case RISCV::VSETIVLI:
    return I.getOperand(2).getImm();
  case RISCV::VSETVL:
    // the new vtype comes from a register
    return VTypeUnknown;
  case RISCV::PseudoCALL:
  case RISCV::C_JALR:
    // calls clobber vtype under the psABI
    return VTypeUnknown;
  case RISCV::JALR:
    // jalr ra, rs is an indirect call; the other forms end the function
    return I.getOperand(0).getReg() == RISCV::X1 ? VTypeUnknown : in;
  default:
    return in;
  }
}

// a forward dataflow over the assembly CFG, computing the vtype on entry
// to each block. the lattice is unreached < a particular vtype < unknown,
// so this terminates quickly
void riscv2llvm::computeVTypeDataflow() {
  auto &BBs = Str->MF.BBs;
  vtypeIn.clear();
  if (BBs.empty())
    return;
  for (auto &bb : BBs)
    vtypeIn[&bb] = VTypeUnreached;
  // vtype is unspecified on entry to a function
  vtypeIn[&BBs[0]] = VTypeUnknown;

  auto join = [](int64_t a, int64_t b) {
    if (a == VTypeUnreached)
      return b;
    if (b == VTypeUnreached)
      return a;
    return a == b ? a : VTypeUnknown;
  };

  vector<MCBasicBlock *> worklist{&BBs[0]};
  while (!worklist.empty()) {
    auto *bb = worklist.back();
    worklist.pop_back();
    auto state = vtypeIn[bb];
    for (auto &I : bb->getInstrs())
      state = vtypeTransfer(I, state);
    for (auto *succ : bb->getSuccs()) {
      auto old = vtypeIn[succ];
      auto merged = join(old, state);
      if (merged != old) {
        vtypeIn[succ] = merged;
        worklist.push_back(succ);
      }
    }
  }
}

// called before each instruction is lifted; picks up the dataflow result
// when lifting moves into a new block
void riscv2llvm::enterVTypeBlock() {
  if (MCBB == curVTypeBB)
    return;
  curVTypeBB = MCBB;
  auto it = vtypeIn.find(MCBB);
  if (it == vtypeIn.end() || it->second == VTypeUnreached)
    curVType = VTypeUnknown;
  else
    curVType = it->second;
}

// decode a vtype value, accepting only the settings that RVA23U64 is
// required to support. everything else is either reserved or
// implementation-defined (it might set vill), and we don't lift it
optional<riscv2llvm::VConfig> riscv2llvm::decodeVType(uint64_t vtype) {
  // vill, the Zvfbfa altfmt bit, and the reserved bits must all be clear
  if (vtype >> 8)
    return nullopt;
  unsigned vsew = (vtype >> 3) & 7;
  if (vsew > 3)
    return nullopt;
  unsigned vlmul = vtype & 7;
  if (vlmul == 4)
    return nullopt;
  int LMULLog2 = vlmul < 4 ? int(vlmul) : int(vlmul) - 8;
  unsigned SEW = 8u << vsew;
  // with ELEN = 64, a fractional LMUL only has to support SEW up to
  // LMUL * ELEN
  if (LMULLog2 < 0 && SEW > (64u >> -LMULLog2))
    return nullopt;
  return VConfig{unsigned(vtype), SEW, LMULLog2, (vtype & 0x40) != 0,
                 (vtype & 0x80) != 0};
}

// the vtype in effect for the current instruction, which must be known
riscv2llvm::VConfig riscv2llvm::getVConfig() {
  if (curVType < 0)
    vectorError("the vtype for this vector instruction is not statically "
                "known; it needs a vset{i}vli on every path to it from the "
                "function entry or the most recent call");
  auto cfg = decodeVType(curVType);
  if (!cfg)
    vectorError("vtype " + to_string(curVType) +
                " is not a setting that RVA23U64 is required to support");
  return *cfg;
}

// the psABI's standard calling convention preserves none of the vector
// state; called from doCall()
void riscv2llvm::invalidateVectorState() {
  for (unsigned i = 0; i < 32; ++i)
    invalidateReg(RISCV::V0 + i, VLEN);
  invalidateReg(RISCV::VL, 64);
  invalidateReg(RISCV::VTYPE, 64);
  invalidateReg(RISCV::VXRM, 2);
  invalidateReg(RISCV::VXSAT, 1);
  curVType = VTypeUnknown;
}

/*
 * helpers
 */

Value *riscv2llvm::readVL() {
  return createLoad(getIntTy(64), lookupVCSR(RISCV::VL));
}

/*
 * the vl chosen by vset{i}vli for a given AVL:
 *
 * - AVL <= VLMAX: vl = AVL
 * - AVL >= 2 * VLMAX: vl = VLMAX
 * - otherwise any value in [ceil(AVL / 2), VLMAX]
 *
 * the spec also requires the choice in the last case to be deterministic
 * for a given AVL and VLMAX on a given implementation. that isn't modeled:
 * each vset{i}vli picks independently, which allows more behaviors than
 * real hardware and so cannot hide a bug
 */
Value *riscv2llvm::computeVL(Value *avl, unsigned vlmax) {
  auto i64ty = getIntTy(64);
  auto VLMAX = getUnsignedIntConst(vlmax, 64);
  auto atMost = createICmp(ICmpInst::ICMP_ULE, avl, VLMAX);
  auto atLeastTwice = createICmp(ICmpInst::ICMP_UGE, avl,
                                 getUnsignedIntConst(2 * uint64_t(vlmax), 64));
  // ceil(AVL / 2); this wraps for AVL = 2^64 - 1, but that case is
  // covered by atLeastTwice and the value is not used
  auto half = createRawLShr(createAdd(avl, getUnsignedIntConst(1, 64)),
                            getUnsignedIntConst(1, 64));
  Value *pick = createUnknownInt(64);
  auto inRange = createAnd(createICmp(ICmpInst::ICMP_UGE, pick, half),
                           createICmp(ICmpInst::ICMP_ULE, pick, VLMAX));
  Value *between = createSelect(inRange, pick, VLMAX);
  Value *res = createSelect(atLeastTwice, VLMAX, between);
  res = createSelect(atMost, avl, res);
  assert(res->getType() == i64ty);
  return res;
}

Value *riscv2llvm::createVSplat(Value *scalar, unsigned numElts) {
  auto vecTy = FixedVectorType::get(scalar->getType(), numElts);
  Value *v = createInsertElement(PoisonValue::get(vecTy), scalar, 0);
  return createShuffleVector(v, SmallVector<int>(numElts, 0));
}

// the vm operand of a maskable instruction: null if unmasked, otherwise
// the mask in v0
Value *riscv2llvm::readVMaskOperand(int idx, unsigned numElts) {
  auto &op = CurInst->getOperand(idx);
  assert(op.isReg());
  if (op.getReg() == RISCV::NoRegister)
    return nullptr;
  assert(op.getReg() == RISCV::V0);
  return readVMask(0, numElts);
}

// require `ptr` to be aligned to `bytes` whenever `cond` holds
void riscv2llvm::assertVAligned(Value *ptr, unsigned bytes, Value *cond) {
  if (bytes <= 1)
    return;
  auto addr = createPtrToInt(ptr, getIntTy(64));
  auto low = createAnd(addr, getUnsignedIntConst(bytes - 1, 64));
  auto aligned = createICmp(ICmpInst::ICMP_EQ, low, getUnsignedIntConst(0, 64));
  assertTrue(createSelect(cond, aligned, getBoolConst(true)));
}

/*
 * instructions
 */

// vsetvli rd, rs1, vtypei / vsetivli rd, uimm, vtypei
void riscv2llvm::liftVSet(unsigned opcode) {
  auto i64ty = getIntTy(64);
  if (opcode == RISCV::VSETVL)
    vectorError("vsetvl takes vtype from a register, so the vtype of the "
                "instructions after it is not statically known");

  int64_t vtypei = CurInst->getOperand(2).getImm();
  auto cfg = decodeVType(vtypei);
  if (!cfg)
    vectorError("vtype " + to_string(vtypei) +
                " is not a setting that RVA23U64 is required to support");
  unsigned vlmax = numVElts(cfg->SEW, cfg->LMULLog2);
  auto rd = CurInst->getOperand(0).getReg();

  Value *vl;
  if (opcode == RISCV::VSETIVLI) {
    auto avl = CurInst->getOperand(1).getImm();
    vl = computeVL(getUnsignedIntConst(avl, 64), vlmax);
  } else {
    auto rs1 = CurInst->getOperand(1).getReg();
    if (rs1 != RISCV::X0) {
      vl = computeVL(readFromReg(rs1, i64ty), vlmax);
    } else if (rd != RISCV::X0) {
      // AVL = ~0, so vl = VLMAX
      vl = getUnsignedIntConst(vlmax, 64);
    } else {
      // vsetvli zero, zero keeps the current vl. this form is reserved if
      // it changes VLMAX or if vill was already set, so the previous vtype
      // has to be known and has to have the same SEW/LMUL ratio
      if (curVType < 0)
        vectorError("vsetvli zero, zero, ... needs the previous vtype to be "
                    "statically known");
      auto prev = decodeVType(curVType);
      if (!prev)
        vectorError("vsetvli zero, zero, ... follows an unsupported vtype");
      if (numVElts(prev->SEW, prev->LMULLog2) != vlmax)
        vectorError("vsetvli zero, zero, ... changes VLMAX, which is "
                    "reserved");
      vl = readVL();
    }
  }

  createStore(vl, lookupVCSR(RISCV::VL));
  createStore(getUnsignedIntConst(vtypei, 64), lookupVCSR(RISCV::VTYPE));
  updateReg(vl, rd);
  curVType = vtypei;
}

// vle<eew>.v vd, (rs1), vm / vse<eew>.v vs3, (rs1), vm
//
// EEW is encoded in the instruction, and EMUL = (EEW / SEW) * LMUL, so the
// element count is still VLMAX. only active elements access memory, which
// is modeled by redirecting each inactive element to a scratch location
void riscv2llvm::liftVUnitStride(unsigned EEW, bool isStore) {
  auto cfg = getVConfig();
  int EMULLog2 = cfg.LMULLog2 + int(Log2_32(EEW)) - int(Log2_32(cfg.SEW));
  if (EMULLog2 < -3 || EMULLog2 > 3)
    vectorError("unit-stride access with EMUL out of range, which is "
                "reserved");
  auto first = vregRange(CurInst->getOperand(0).getReg()).first;
  checkVGroup(first, EEW, EMULLog2);
  unsigned n = numVElts(EEW, EMULLog2);

  auto mask = readVMaskOperand(2, n);
  // for a load, a masked destination may not overlap v0; for a store, v0
  // would be read with two different EEWs. both are reserved
  if (mask && first == 0)
    vectorError("masked vector memory access whose data group includes v0, "
                "which is reserved");

  auto vl = readVL();
  Value *active = createBodyMask(vl, n);
  if (mask)
    active = createSelect(active, mask,
                          Constant::getNullValue(getVecTy(1, n)));

  // LLVM only emits element-aligned vector accesses unless it is told the
  // hardware handles misaligned ones, which RVA23U64 does not promise, so
  // a misaligned active element is treated as a fault. all elements of a
  // unit-stride access share the base address's alignment
  auto base = readFromRegOperand(1, PointerType::get(Ctx, 0));
  auto anyActive = createICmp(ICmpInst::ICMP_NE,
                              createBitCast(active, getIntTy(n)),
                              getUnsignedIntConst(0, n));
  assertVAligned(base, EEW / 8, anyActive);

  auto eltTy = getIntTy(EEW);
  auto i8ty = getIntTy(8);
  Value *data = isStore ? readVGroup(first, EEW, EMULLog2)
                        : PoisonValue::get(getVecTy(EEW, n));
  for (unsigned i = 0; i < n; ++i) {
    auto addr = createGEP(i8ty, base,
                          {getUnsignedIntConst(i * (EEW / 8), 64)}, nextName());
    auto isActive = createExtractElement(active, i);
    auto ptr = createSelect(isActive, addr, vScratch);
    if (isStore) {
      createStore(createExtractElement(data, i), ptr);
    } else {
      // an inactive element's value is never used, but leaving it as
      // whatever the scratch slot holds lets LLVM turn it into undef, which
      // makes verification much slower
      auto loaded = createSelect(isActive, createLoad(eltTy, ptr),
                                 getUnsignedIntConst(0, EEW));
      data = createInsertElement(data, loaded, i);
    }
  }

  if (!isStore)
    writeVGroup(first, EEW, EMULLog2, data, vl, mask, cfg.tailAgnostic,
                cfg.maskAgnostic);
}

// vl<nregs>re<eew>.v vd, (rs1) / vs<nregs>r.v vs3, (rs1)
//
// these move whole registers and depend on neither vl nor vtype. they may
// raise a misaligned exception unless the address is aligned to the
// encoded EEW (always 8 for the stores)
void riscv2llvm::liftVWholeRegLoadStore(unsigned nregs, unsigned EEW,
                                        bool isStore) {
  auto first = vregRange(CurInst->getOperand(0).getReg()).first;
  if (first % nregs != 0)
    vectorError("misaligned whole-register group, which is reserved");
  auto base = readFromRegOperand(1, PointerType::get(Ctx, 0));
  assertVAligned(base, EEW / 8, getBoolConst(true));
  // the in-memory and in-register byte layouts are identical, and the
  // lowest-numbered register is at the lowest address
  auto regTy = getIntTy(VLEN);
  for (unsigned i = 0; i < nregs; ++i) {
    auto addr = createGEP(getIntTy(8), base,
                          {getUnsignedIntConst(i * (VLEN / 8), 64)},
                          nextName());
    if (isStore)
      createStore(createLoad(regTy, lookupVReg(first + i)), addr);
    else
      createStore(createLoad(regTy, addr), lookupVReg(first + i));
  }
}

/*
 * integer arithmetic
 *
 * these all have the operand layout (vd, vs2, vs1 | rs1 | imm, vm), and
 * every source and the destination have EEW = SEW and EMUL = LMUL, except
 * that a compare's destination is a mask
 */

// the lowest-numbered register of a vector register group operand, or
// nullopt if the operand is a scalar register or an immediate
optional<unsigned> riscv2llvm::vgroupOperand(int idx) {
  auto &op = CurInst->getOperand(idx);
  if (!op.isReg() || !isVReg(op.getReg()))
    return nullopt;
  return vregRange(op.getReg()).first;
}

// a masked instruction reads v0 as a mask, with EEW = 1. also reading it
// as a data source is reserved, because one register would be read with two
// EEWs. so is writing a data result to a group that includes it. groups are
// aligned, so a group includes v0 exactly when it starts there
void riscv2llvm::rejectMaskedV0(unsigned first, bool isDest) {
  if (first != 0)
    return;
  if (isDest)
    vectorError("masked vector instruction whose destination group includes "
                "v0, which is reserved");
  vectorError("masked vector instruction that also reads v0 as data, which "
              "is reserved");
}

// operand `idx` as a vector of VLMAX SEW-bit elements. it is a register
// group, an x register whose low SEW bits are used (XLEN >= SEW), or a
// 5-bit immediate. the immediate is sign-extended to SEW bits unless
// `zextImm` is set; the MC layer already gives a uimm5 as 0-31 and a simm5
// as -16-15
Value *riscv2llvm::readVOperand(int idx, const VConfig &cfg, bool zextImm) {
  auto n = numVElts(cfg.SEW, cfg.LMULLog2);
  auto &op = CurInst->getOperand(idx);
  if (op.isImm())
    return getElemSplat(n, cfg.SEW, op.getImm(), /*isSigned=*/!zextImm);
  if (auto first = vgroupOperand(idx))
    return readVGroup(*first, cfg.SEW, cfg.LMULLog2);
  auto x = readFromRegOperand(idx, getIntTy(64));
  return createVSplat(createTrunc(x, getIntTy(cfg.SEW)), n);
}

// elementwise `a op b`, where `a` comes from vs2. every lane is computed,
// including inactive and tail lanes whose results writeVGroup() discards,
// so nothing here may be UB for any input
Value *riscv2llvm::createVIntOp(VIntOp op, Value *a, Value *b) {
  auto *vecTy = cast<FixedVectorType>(a->getType());
  assert(b->getType() == vecTy);
  unsigned SEW = getBitWidth(vecTy->getElementType());
  unsigned n = vecTy->getNumElements();

  switch (op) {
  case VIntOp::Add:
    return createAdd(a, b);
  case VIntOp::Sub:
    return createSub(a, b);
  case VIntOp::RSub:
    return createSub(b, a);
  case VIntOp::And:
    return createAnd(a, b);
  case VIntOp::Or:
    return createOr(a, b);
  case VIntOp::Xor:
    return createXor(a, b);

  // only the low lg2(SEW) bits of the shift amount are used
  case VIntOp::Sll:
    return createMaskedShl(a, b);
  case VIntOp::Srl:
    return createMaskedLShr(a, b);
  case VIntOp::Sra:
    return createMaskedAShr(a, b);

  case VIntOp::MinU:
    return createUMin(a, b);
  case VIntOp::Min:
    return createSMin(a, b);
  case VIntOp::MaxU:
    return createUMax(a, b);
  case VIntOp::Max:
    return createSMax(a, b);

  case VIntOp::Mul:
    return createMul(a, b);

  // the high half of the 2*SEW-bit product. for vmulhsu, vs2 is signed
  // and the other operand is unsigned
  case VIntOp::MulH:
  case VIntOp::MulHU:
  case VIntOp::MulHSU: {
    auto wideTy = getVecTy(2 * SEW, n);
    bool aSigned = op != VIntOp::MulHU;
    bool bSigned = op == VIntOp::MulH;
    Value *wa = aSigned ? createSExt(a, wideTy) : createZExt(a, wideTy);
    Value *wb = bSigned ? createSExt(b, wideTy) : createZExt(b, wideTy);
    auto hi = createRawLShr(createMul(wa, wb), ConstantInt::get(wideTy, SEW));
    return createTrunc(hi, vecTy);
  }

  // these never trap and behave like the scalar M-extension divides: the
  // quotient of division by zero is all ones and the remainder is the
  // dividend; on signed overflow (INT_MIN / -1) the quotient is INT_MIN
  // and the remainder is 0. LLVM's divides are UB in exactly these lanes,
  // so their divisor is replaced by 1 there. dividing by 1 already gives
  // the right overflow results, and the divide-by-zero lanes are fixed up
  // afterwards
  case VIntOp::DivU:
  case VIntOp::Div:
  case VIntOp::RemU:
  case VIntOp::Rem: {
    bool isSigned = op == VIntOp::Div || op == VIntOp::Rem;
    bool isDiv = op == VIntOp::DivU || op == VIntOp::Div;
    auto byZero =
        createICmp(ICmpInst::ICMP_EQ, b, Constant::getNullValue(vecTy));
    Value *unsafe = byZero;
    if (isSigned) {
      auto isMin = createICmp(
          ICmpInst::ICMP_EQ, a,
          ConstantInt::get(vecTy, APInt::getSignedMinValue(SEW)));
      auto isMinusOne =
          createICmp(ICmpInst::ICMP_EQ, b, getAllOnesConst(vecTy));
      unsafe = createOr(byZero, createAnd(isMin, isMinusOne));
    }
    auto safeB = createSelect(unsafe, ConstantInt::get(vecTy, 1), b);
    Value *res;
    if (op == VIntOp::DivU)
      res = createUDiv(a, safeB);
    else if (op == VIntOp::Div)
      res = createSDiv(a, safeB);
    else if (op == VIntOp::RemU)
      res = createURem(a, safeB);
    else
      res = createSRem(a, safeB);
    Value *ifZero = isDiv ? static_cast<Value *>(getAllOnesConst(vecTy)) : a;
    return createSelect(byZero, ifZero, res);
  }
  }
  assert(false && "unhandled VIntOp");
  return nullptr;
}

// vop.vv vd, vs2, vs1, vm / vop.vx vd, vs2, rs1, vm / vop.vi vd, vs2, imm, vm
void riscv2llvm::liftVIntBinary(VIntOp op) {
  auto cfg = getVConfig();
  auto n = numVElts(cfg.SEW, cfg.LMULLog2);
  auto vd = vregRange(CurInst->getOperand(0).getReg()).first;
  auto vs2 = vregRange(CurInst->getOperand(1).getReg()).first;
  auto mask = readVMaskOperand(3, n);
  if (mask) {
    rejectMaskedV0(vd, /*isDest=*/true);
    rejectMaskedV0(vs2, /*isDest=*/false);
    if (auto vs1 = vgroupOperand(2))
      rejectMaskedV0(*vs1, /*isDest=*/false);
  }
  // all the operands have the same EEW, so any overlap between the
  // destination and the sources is allowed
  bool zextImm =
      op == VIntOp::Sll || op == VIntOp::Srl || op == VIntOp::Sra;
  auto a = readVGroup(vs2, cfg.SEW, cfg.LMULLog2);
  auto b = readVOperand(2, cfg, zextImm);
  writeVGroup(vd, cfg.SEW, cfg.LMULLog2, createVIntOp(op, a, b), readVL(),
              mask, cfg.tailAgnostic, cfg.maskAgnostic);
}

// vmsCC.vv vd, vs2, vs1, vm / .vx vd, vs2, rs1, vm / .vi vd, vs2, simm5, vm
//
// the result is a mask, so the destination is a single register and is
// always tail agnostic (writeVMask() handles that). its EEW is 1, narrower
// than the sources, so it may overlap a source group only at that group's
// lowest-numbered register, and in that case the instruction is also mask
// agnostic whatever vma says. unlike other masked instructions, a compare
// may write v0. the immediate is sign-extended even for the unsigned
// compares
void riscv2llvm::liftVIntCompare(CmpInst::Predicate pred) {
  auto cfg = getVConfig();
  auto n = numVElts(cfg.SEW, cfg.LMULLog2);
  auto vd = vregRange(CurInst->getOperand(0).getReg()).first;
  auto vs2 = vregRange(CurInst->getOperand(1).getReg()).first;
  auto vs1 = vgroupOperand(2);
  auto mask = readVMaskOperand(3, n);
  if (mask) {
    rejectMaskedV0(vs2, /*isDest=*/false);
    if (vs1)
      rejectMaskedV0(*vs1, /*isDest=*/false);
  }

  bool overlap = false;
  unsigned groupRegs = numVRegs(cfg.LMULLog2);
  auto checkOverlap = [&](unsigned first) {
    if (vd < first || vd >= first + groupRegs)
      return;
    if (vd != first)
      vectorError("compare whose destination overlaps a source group other "
                  "than at its lowest-numbered register, which is reserved");
    overlap = true;
  };
  checkOverlap(vs2);
  if (vs1)
    checkOverlap(*vs1);

  auto a = readVGroup(vs2, cfg.SEW, cfg.LMULLog2);
  auto b = readVOperand(2, cfg, /*zextImm=*/false);
  writeVMask(vd, createICmp(pred, a, b), readVL(), mask,
             cfg.maskAgnostic || overlap);
}

// vmerge.vvm vd, vs2, vs1, v0 / .vxm vd, vs2, rs1, v0 / .vim vd, vs2, imm, v0
//
// vd[i] = v0.mask[i] ? src[i] : vs2[i] for every body element. this is
// encoded as a masked instruction, so the usual v0 restrictions apply, but
// no body element is inactive: only the tail policy matters
void riscv2llvm::liftVMerge() {
  auto cfg = getVConfig();
  auto n = numVElts(cfg.SEW, cfg.LMULLog2);
  auto vd = vregRange(CurInst->getOperand(0).getReg()).first;
  auto vs2 = vregRange(CurInst->getOperand(1).getReg()).first;
  assert(CurInst->getOperand(3).getReg() == RISCV::V0);
  rejectMaskedV0(vd, /*isDest=*/true);
  rejectMaskedV0(vs2, /*isDest=*/false);
  if (auto vs1 = vgroupOperand(2))
    rejectMaskedV0(*vs1, /*isDest=*/false);
  auto sel = readVMask(0, n);
  auto a = readVGroup(vs2, cfg.SEW, cfg.LMULLog2);
  auto b = readVOperand(2, cfg, /*zextImm=*/false);
  writeVGroup(vd, cfg.SEW, cfg.LMULLog2, createSelect(sel, b, a), readVL(),
              nullptr, cfg.tailAgnostic, cfg.maskAgnostic);
}

// returns false if the opcode is not one of ours
bool riscv2llvm::liftVector(unsigned opcode) {
  switch (opcode) {
  case RISCV::VSETVLI:
  case RISCV::VSETIVLI:
  case RISCV::VSETVL:
    liftVSet(opcode);
    return true;

  // csrr rd, csr; only the read-only vector CSRs are supported for now
  case RISCV::CSRRS: {
    if (CurInst->getOperand(2).getReg() != RISCV::X0)
      return false;
    auto csr = CurInst->getOperand(1).getImm();
    Value *val;
    if (csr == CSR_VL)
      val = readVL();
    else if (csr == CSR_VTYPE)
      val = createLoad(getIntTy(64), lookupVCSR(RISCV::VTYPE));
    else if (csr == CSR_VLENB)
      val = getVLENB();
    else
      return false;
    updateReg(val, CurInst->getOperand(0).getReg());
    return true;
  }

  // vmv.v.i vd, simm5 / vmv.v.x vd, rs1 / vmv.v.v vd, vs1: unmasked
  case RISCV::VMV_V_I:
  case RISCV::VMV_V_X:
  case RISCV::VMV_V_V: {
    auto cfg = getVConfig();
    auto n = numVElts(cfg.SEW, cfg.LMULLog2);
    auto vd = vregRange(CurInst->getOperand(0).getReg()).first;
    Value *res;
    if (opcode == RISCV::VMV_V_I) {
      // the immediate is already sign-extended
      res = getElemSplat(n, cfg.SEW, CurInst->getOperand(1).getImm(),
                         /*isSigned=*/true);
    } else if (opcode == RISCV::VMV_V_X) {
      // SEW <= XLEN, so the low SEW bits of the register are used
      auto x = readFromRegOperand(1, getIntTy(64));
      res = createVSplat(createTrunc(x, getIntTy(cfg.SEW)), n);
    } else {
      auto vs1 = vregRange(CurInst->getOperand(1).getReg()).first;
      res = readVGroup(vs1, cfg.SEW, cfg.LMULLog2);
    }
    writeVGroup(vd, cfg.SEW, cfg.LMULLog2, res, readVL(), nullptr,
                cfg.tailAgnostic, cfg.maskAgnostic);
    return true;
  }

#define VINT_VV(NAME, OP)                                                      \
  case RISCV::NAME##_VV:                                                       \
    liftVIntBinary(VIntOp::OP);                                                \
    return true;
#define VINT_VX(NAME, OP)                                                      \
  case RISCV::NAME##_VX:                                                       \
    liftVIntBinary(VIntOp::OP);                                                \
    return true;
#define VINT_VI(NAME, OP)                                                      \
  case RISCV::NAME##_VI:                                                       \
    liftVIntBinary(VIntOp::OP);                                                \
    return true;
#define VINT_VV_VX(NAME, OP) VINT_VV(NAME, OP) VINT_VX(NAME, OP)
#define VINT_VV_VX_VI(NAME, OP) VINT_VV_VX(NAME, OP) VINT_VI(NAME, OP)
    VINT_VV_VX_VI(VADD, Add)
    VINT_VV_VX(VSUB, Sub)
    VINT_VX(VRSUB, RSub)
    VINT_VI(VRSUB, RSub)
    VINT_VV_VX_VI(VAND, And)
    VINT_VV_VX_VI(VOR, Or)
    VINT_VV_VX_VI(VXOR, Xor)
    VINT_VV_VX_VI(VSLL, Sll)
    VINT_VV_VX_VI(VSRL, Srl)
    VINT_VV_VX_VI(VSRA, Sra)
    VINT_VV_VX(VMINU, MinU)
    VINT_VV_VX(VMIN, Min)
    VINT_VV_VX(VMAXU, MaxU)
    VINT_VV_VX(VMAX, Max)
    VINT_VV_VX(VMUL, Mul)
    VINT_VV_VX(VMULH, MulH)
    VINT_VV_VX(VMULHU, MulHU)
    VINT_VV_VX(VMULHSU, MulHSU)
    VINT_VV_VX(VDIVU, DivU)
    VINT_VV_VX(VDIV, Div)
    VINT_VV_VX(VREMU, RemU)
    VINT_VV_VX(VREM, Rem)
#undef VINT_VV_VX_VI
#undef VINT_VV_VX
#undef VINT_VI
#undef VINT_VX
#undef VINT_VV

#define VCMP(NAME, PRED)                                                       \
  case RISCV::NAME:                                                            \
    liftVIntCompare(ICmpInst::PRED);                                           \
    return true;
    VCMP(VMSEQ_VV, ICMP_EQ)
    VCMP(VMSEQ_VX, ICMP_EQ)
    VCMP(VMSEQ_VI, ICMP_EQ)
    VCMP(VMSNE_VV, ICMP_NE)
    VCMP(VMSNE_VX, ICMP_NE)
    VCMP(VMSNE_VI, ICMP_NE)
    VCMP(VMSLTU_VV, ICMP_ULT)
    VCMP(VMSLTU_VX, ICMP_ULT)
    VCMP(VMSLT_VV, ICMP_SLT)
    VCMP(VMSLT_VX, ICMP_SLT)
    VCMP(VMSLEU_VV, ICMP_ULE)
    VCMP(VMSLEU_VX, ICMP_ULE)
    VCMP(VMSLEU_VI, ICMP_ULE)
    VCMP(VMSLE_VV, ICMP_SLE)
    VCMP(VMSLE_VX, ICMP_SLE)
    VCMP(VMSLE_VI, ICMP_SLE)
    VCMP(VMSGTU_VX, ICMP_UGT)
    VCMP(VMSGTU_VI, ICMP_UGT)
    VCMP(VMSGT_VX, ICMP_SGT)
    VCMP(VMSGT_VI, ICMP_SGT)
#undef VCMP

  case RISCV::VMERGE_VVM:
  case RISCV::VMERGE_VXM:
  case RISCV::VMERGE_VIM:
    liftVMerge();
    return true;

  case RISCV::VLE8_V:
    liftVUnitStride(8, false);
    return true;
  case RISCV::VLE16_V:
    liftVUnitStride(16, false);
    return true;
  case RISCV::VLE32_V:
    liftVUnitStride(32, false);
    return true;
  case RISCV::VLE64_V:
    liftVUnitStride(64, false);
    return true;
  case RISCV::VSE8_V:
    liftVUnitStride(8, true);
    return true;
  case RISCV::VSE16_V:
    liftVUnitStride(16, true);
    return true;
  case RISCV::VSE32_V:
    liftVUnitStride(32, true);
    return true;
  case RISCV::VSE64_V:
    liftVUnitStride(64, true);
    return true;

#define WHOLE_REG_LOAD(N, E)                                                   \
  case RISCV::VL##N##RE##E##_V:                                                \
    liftVWholeRegLoadStore(N, E, false);                                       \
    return true;
#define WHOLE_REG_LOADS(N)                                                     \
  WHOLE_REG_LOAD(N, 8)                                                         \
  WHOLE_REG_LOAD(N, 16)                                                        \
  WHOLE_REG_LOAD(N, 32)                                                        \
  WHOLE_REG_LOAD(N, 64)
    WHOLE_REG_LOADS(1)
    WHOLE_REG_LOADS(2)
    WHOLE_REG_LOADS(4)
    WHOLE_REG_LOADS(8)
#undef WHOLE_REG_LOADS
#undef WHOLE_REG_LOAD

  case RISCV::VS1R_V:
    liftVWholeRegLoadStore(1, 8, true);
    return true;
  case RISCV::VS2R_V:
    liftVWholeRegLoadStore(2, 8, true);
    return true;
  case RISCV::VS4R_V:
    liftVWholeRegLoadStore(4, 8, true);
    return true;
  case RISCV::VS8R_V:
    liftVWholeRegLoadStore(8, 8, true);
    return true;

  // vmv<nr>r.v vd, vs2: copies whole registers, ignoring vl. unlike the
  // whole-register loads and stores, the spec does not exempt these from
  // depending on vtype (an instruction that depends on vtype traps when
  // vill is set), so we require vtype to be statically known here too
  case RISCV::VMV1R_V:
  case RISCV::VMV2R_V:
  case RISCV::VMV4R_V:
  case RISCV::VMV8R_V: {
    getVConfig();
    unsigned nregs = opcode == RISCV::VMV1R_V   ? 1
                     : opcode == RISCV::VMV2R_V ? 2
                     : opcode == RISCV::VMV4R_V ? 4
                                                : 8;
    auto vd = vregRange(CurInst->getOperand(0).getReg()).first;
    auto vs2 = vregRange(CurInst->getOperand(1).getReg()).first;
    if (vd % nregs != 0 || vs2 % nregs != 0)
      vectorError("misaligned whole-register move, which is reserved");
    // vd and vs2 are either identical or disjoint, given the alignment
    for (unsigned i = 0; i < nregs; ++i)
      createStore(createLoad(getIntTy(VLEN), lookupVReg(vs2 + i)),
                  lookupVReg(vd + i));
    return true;
  }

  default:
    return false;
  }
}
