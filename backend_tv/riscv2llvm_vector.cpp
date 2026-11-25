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

/*
 * check the register group constraints of an instruction that writes `dest`
 * and reads `srcs`, plus the mask in v0 if `readsV0Mask`. a mask operand
 * has EEW = 1 and occupies one register. the rules are in "Vector
 * Operands" in the ISA manual, and breaking any of them is reserved:
 *
 * - each data group must be aligned and well formed (checkVGroup)
 * - a masked instruction's destination may overlap v0 only if it is a mask
 * - no register may be read with two different EEWs, counting the v0 mask
 *   as EEW = 1
 * - a destination may overlap a source with a different EEW only if the
 *   destination is narrower and both start at the same register, or the
 *   destination is wider, the source's EMUL is at least 1, and both end at
 *   the same register
 *
 * returns true if the destination does overlap a source with a different
 * EEW. the instruction is then mask and tail agnostic, whatever vtype says
 */
bool riscv2llvm::checkVOverlap(VOperand dest, const vector<VOperand> &srcs,
                               bool readsV0Mask) {
  auto lastReg = [&](const VOperand &o) {
    return o.first + (o.EEW == 1 ? 1 : numVRegs(o.EMULLog2)) - 1;
  };
  auto overlaps = [&](const VOperand &a, const VOperand &b) {
    return a.first <= lastReg(b) && b.first <= lastReg(a);
  };

  if (dest.EEW != 1)
    checkVGroup(dest.first, dest.EEW, dest.EMULLog2);
  for (auto &s : srcs)
    if (s.EEW != 1)
      checkVGroup(s.first, s.EEW, s.EMULLog2);

  if (readsV0Mask) {
    VOperand v0{0, 1, 0};
    if (dest.EEW != 1 && overlaps(dest, v0))
      vectorError("masked vector instruction whose destination group "
                  "includes v0, which is reserved");
    for (auto &s : srcs)
      if (s.EEW != 1 && overlaps(s, v0))
        vectorError("masked vector instruction that also reads v0 as data, "
                    "which is reserved");
  }

  for (size_t i = 0; i < srcs.size(); ++i)
    for (size_t j = i + 1; j < srcs.size(); ++j)
      if (srcs[i].EEW != srcs[j].EEW && overlaps(srcs[i], srcs[j]))
        vectorError("vector register read with two different EEWs, which is "
                    "reserved");

  bool forcedAgnostic = false;
  for (auto &s : srcs) {
    if (s.EEW == dest.EEW || !overlaps(dest, s))
      continue;
    bool allowed = dest.EEW < s.EEW
                       ? dest.first == s.first
                       : s.EMULLog2 >= 0 && lastReg(dest) == lastReg(s);
    if (!allowed)
      vectorError("destination group overlaps a source group with a "
                  "different EEW in a way that is reserved");
    forcedAgnostic = true;
  }
  return forcedAgnostic;
}

// widening and narrowing instructions have an operand with EEW = 2 * SEW
// and EMUL = 2 * LMUL, which has to be a legal width and group size
void riscv2llvm::checkVWidenable(const VConfig &cfg) {
  if (cfg.SEW >= 64)
    vectorError("widening or narrowing instruction with SEW = 64, which is "
                "reserved");
  if (cfg.LMULLog2 >= 3)
    vectorError("widening or narrowing instruction with LMUL = 8, which is "
                "reserved");
}

// zero- or sign-extend each element of an integer vector to `bits`
Value *riscv2llvm::createVExt(Value *v, unsigned bits, bool isSigned) {
  auto n = cast<FixedVectorType>(v->getType())->getNumElements();
  auto ty = getVecTy(bits, n);
  return isSigned ? static_cast<Value *>(createSExt(v, ty))
                  : static_cast<Value *>(createZExt(v, ty));
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
    auto wa = createVExt(a, 2 * SEW, /*isSigned=*/op != VIntOp::MulHU);
    auto wb = createVExt(b, 2 * SEW, /*isSigned=*/op == VIntOp::MulH);
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

// operand `idx`, a vector register group, with the given EEW and EMUL
riscv2llvm::VOperand riscv2llvm::vopAt(int idx, unsigned EEW, int EMULLog2) {
  return {vregRange(CurInst->getOperand(idx).getReg()).first, EEW, EMULLog2};
}

// add operand `idx` to `srcs` if it is a vector register group rather than
// a scalar register or an immediate
void riscv2llvm::addVSrc(vector<VOperand> &srcs, int idx, unsigned EEW,
                         int EMULLog2) {
  if (auto first = vgroupOperand(idx))
    srcs.push_back({*first, EEW, EMULLog2});
}

// vop.vv vd, vs2, vs1, vm / vop.vx vd, vs2, rs1, vm / vop.vi vd, vs2, imm, vm
void riscv2llvm::liftVIntBinary(VIntOp op) {
  auto cfg = getVConfig();
  auto n = numVElts(cfg.SEW, cfg.LMULLog2);
  auto S = cfg.SEW;
  auto L = cfg.LMULLog2;
  auto mask = readVMaskOperand(3, n);
  vector<VOperand> srcs{vopAt(1, S, L)};
  addVSrc(srcs, 2, S, L);
  // every operand has EEW = SEW, so no overlap forces agnostic
  checkVOverlap(vopAt(0, S, L), srcs, mask != nullptr);
  bool zextImm =
      op == VIntOp::Sll || op == VIntOp::Srl || op == VIntOp::Sra;
  auto a = readVGroup(srcs[0].first, S, L);
  auto b = readVOperand(2, cfg, zextImm);
  writeVGroup(vopAt(0, S, L).first, S, L, createVIntOp(op, a, b), readVL(),
              mask, cfg.tailAgnostic, cfg.maskAgnostic);
}

// vmsCC.vv vd, vs2, vs1, vm / .vx vd, vs2, rs1, vm / .vi vd, vs2, simm5, vm
//
// the result is a mask, so the destination is a single register and is
// always tail agnostic (writeVMask() handles that). unlike other masked
// instructions, a compare may write v0. the immediate is sign-extended
// even for the unsigned compares
void riscv2llvm::liftVIntCompare(CmpInst::Predicate pred) {
  auto cfg = getVConfig();
  auto n = numVElts(cfg.SEW, cfg.LMULLog2);
  auto S = cfg.SEW;
  auto L = cfg.LMULLog2;
  auto mask = readVMaskOperand(3, n);
  vector<VOperand> srcs{vopAt(1, S, L)};
  addVSrc(srcs, 2, S, L);
  auto vd = vopAt(0, 1, 0);
  bool agnostic = checkVOverlap(vd, srcs, mask != nullptr);
  auto a = readVGroup(srcs[0].first, S, L);
  auto b = readVOperand(2, cfg, /*zextImm=*/false);
  writeVMask(vd.first, createICmp(pred, a, b), readVL(), mask,
             cfg.maskAgnostic || agnostic);
}

// vmerge.vvm vd, vs2, vs1, v0 / .vxm vd, vs2, rs1, v0 / .vim vd, vs2, imm, v0
//
// vd[i] = v0.mask[i] ? src[i] : vs2[i] for every body element. this is
// encoded as a masked instruction, so the usual v0 restrictions apply, but
// no body element is inactive: only the tail policy matters
void riscv2llvm::liftVMerge() {
  auto cfg = getVConfig();
  auto n = numVElts(cfg.SEW, cfg.LMULLog2);
  auto S = cfg.SEW;
  auto L = cfg.LMULLog2;
  assert(CurInst->getOperand(3).getReg() == RISCV::V0);
  vector<VOperand> srcs{vopAt(1, S, L)};
  addVSrc(srcs, 2, S, L);
  checkVOverlap(vopAt(0, S, L), srcs, /*readsV0Mask=*/true);
  auto sel = readVMask(0, n);
  auto a = readVGroup(srcs[0].first, S, L);
  auto b = readVOperand(2, cfg, /*zextImm=*/false);
  writeVGroup(vopAt(0, S, L).first, S, L, createSelect(sel, b, a), readVL(),
              nullptr, cfg.tailAgnostic, cfg.maskAgnostic);
}

/*
 * widening, narrowing, and extending integer arithmetic. "wide" below
 * means EEW = 2 * SEW and EMUL = 2 * LMUL
 */

// vwop.vv vd, vs2, vs1, vm / vwop.vx vd, vs2, rs1, vm: wide = SEW op SEW
// vwop.wv vd, vs2, vs1, vm / vwop.wx vd, vs2, rs1, vm: wide = wide op SEW
//
// the narrow operands are zero- or sign-extended first; for vwmulsu, vs2 is
// signed and the other operand unsigned. a .vx scalar contributes its low
// SEW bits, which are then extended like a vector operand
void riscv2llvm::liftVWidenBinary(VWideOp op, bool wideVs2) {
  auto cfg = getVConfig();
  checkVWidenable(cfg);
  auto n = numVElts(cfg.SEW, cfg.LMULLog2);
  auto S = cfg.SEW, W = 2 * cfg.SEW;
  auto L = cfg.LMULLog2, WL = cfg.LMULLog2 + 1;
  auto mask = readVMaskOperand(3, n);
  vector<VOperand> srcs{wideVs2 ? vopAt(1, W, WL) : vopAt(1, S, L)};
  addVSrc(srcs, 2, S, L);
  auto vd = vopAt(0, W, WL);
  bool agnostic = checkVOverlap(vd, srcs, mask != nullptr);

  bool aSigned = op == VWideOp::Add || op == VWideOp::Sub ||
                 op == VWideOp::Mul || op == VWideOp::MulSU;
  bool bSigned =
      op == VWideOp::Add || op == VWideOp::Sub || op == VWideOp::Mul;
  Value *a = wideVs2 ? readVGroup(srcs[0].first, W, WL)
                     : createVExt(readVGroup(srcs[0].first, S, L), W, aSigned);
  Value *b = createVExt(readVOperand(2, cfg, /*zextImm=*/false), W, bSigned);
  Value *res = nullptr;
  switch (op) {
  case VWideOp::AddU:
  case VWideOp::Add:
    res = createAdd(a, b);
    break;
  case VWideOp::SubU:
  case VWideOp::Sub:
    res = createSub(a, b);
    break;
  case VWideOp::MulU:
  case VWideOp::Mul:
  case VWideOp::MulSU:
    res = createMul(a, b);
    break;
  }
  writeVGroup(vd.first, W, WL, res, readVL(), mask,
              cfg.tailAgnostic || agnostic, cfg.maskAgnostic || agnostic);
}

// vzext.vf<factor> vd, vs2, vm / vsext.vf<factor> vd, vs2, vm
//
// the source has EEW = SEW / factor and EMUL = LMUL / factor, and the
// encoding is reserved if either is out of range
void riscv2llvm::liftVExtend(unsigned factor, bool isSigned) {
  auto cfg = getVConfig();
  auto n = numVElts(cfg.SEW, cfg.LMULLog2);
  auto S = cfg.SEW;
  auto L = cfg.LMULLog2;
  unsigned EEW = S / factor;
  int EMULLog2 = L - int(Log2_32(factor));
  if (EEW < 8)
    vectorError("integer extension whose source EEW is below 8, which is "
                "reserved");
  if (EMULLog2 < -3)
    vectorError("integer extension whose source EMUL is below 1/8, which is "
                "reserved");
  auto mask = readVMaskOperand(2, n);
  auto vs2 = vopAt(1, EEW, EMULLog2);
  auto vd = vopAt(0, S, L);
  bool agnostic = checkVOverlap(vd, {vs2}, mask != nullptr);
  auto res = createVExt(readVGroup(vs2.first, EEW, EMULLog2), S, isSigned);
  writeVGroup(vd.first, S, L, res, readVL(), mask,
              cfg.tailAgnostic || agnostic, cfg.maskAgnostic || agnostic);
}

// vnsrl.wv vd, vs2, vs1, vm / .wx vd, vs2, rs1, vm / .wi vd, vs2, uimm, vm
// (and vnsra): SEW = wide >> SEW, using the low lg2(2 * SEW) bits of the
// shift amount
void riscv2llvm::liftVNarrowShift(bool isArith) {
  auto cfg = getVConfig();
  checkVWidenable(cfg);
  auto n = numVElts(cfg.SEW, cfg.LMULLog2);
  auto S = cfg.SEW, W = 2 * cfg.SEW;
  auto L = cfg.LMULLog2, WL = cfg.LMULLog2 + 1;
  auto mask = readVMaskOperand(3, n);
  vector<VOperand> srcs{vopAt(1, W, WL)};
  addVSrc(srcs, 2, S, L);
  auto vd = vopAt(0, S, L);
  bool agnostic = checkVOverlap(vd, srcs, mask != nullptr);
  auto a = readVGroup(srcs[0].first, W, WL);
  auto amt = createVExt(readVOperand(2, cfg, /*zextImm=*/true), W,
                        /*isSigned=*/false);
  auto shifted = isArith ? createMaskedAShr(a, amt) : createMaskedLShr(a, amt);
  auto res = createTrunc(shifted, getVecTy(S, n));
  writeVGroup(vd.first, S, L, res, readVL(), mask,
              cfg.tailAgnostic || agnostic, cfg.maskAgnostic || agnostic);
}

/*
 * multiply-add. the operand layout is (vd, vd, vs1 | rs1, vs2, vm): the
 * destination also appears as a tied source, and the assembly order of
 * vs1 and vs2 is the reverse of the other arithmetic instructions
 */

// vmacc:  vd[i] = +(vs1[i] * vs2[i]) + vd[i]
// vnmsac: vd[i] = -(vs1[i] * vs2[i]) + vd[i]
// vmadd:  vd[i] = (vs1[i] * vd[i]) + vs2[i]
// vnmsub: vd[i] = -(vs1[i] * vd[i]) + vs2[i]
void riscv2llvm::liftVMulAdd(VMacOp op) {
  auto cfg = getVConfig();
  auto n = numVElts(cfg.SEW, cfg.LMULLog2);
  auto S = cfg.SEW;
  auto L = cfg.LMULLog2;
  auto mask = readVMaskOperand(4, n);
  auto vd = vopAt(0, S, L);
  vector<VOperand> srcs{vd, vopAt(3, S, L)};
  addVSrc(srcs, 2, S, L);
  checkVOverlap(vd, srcs, mask != nullptr);
  auto d = readVGroup(vd.first, S, L);
  auto s1 = readVOperand(2, cfg, /*zextImm=*/false);
  auto s2 = readVGroup(srcs[1].first, S, L);
  Value *res = nullptr;
  switch (op) {
  case VMacOp::Macc:
    res = createAdd(createMul(s1, s2), d);
    break;
  case VMacOp::Nmsac:
    res = createSub(d, createMul(s1, s2));
    break;
  case VMacOp::Madd:
    res = createAdd(createMul(s1, d), s2);
    break;
  case VMacOp::Nmsub:
    res = createSub(s2, createMul(s1, d));
    break;
  }
  writeVGroup(vd.first, S, L, res, readVL(), mask, cfg.tailAgnostic,
              cfg.maskAgnostic);
}

// vwmacc{u,su,us}.vv vd, vs1, vs2, vm / .vx vd, rs1, vs2, vm:
// wide vd[i] += ext(vs1[i] or rs1) * ext(vs2[i]), with the signedness of
// the two multiplicands given separately
void riscv2llvm::liftVWidenMulAdd(bool s1Signed, bool s2Signed) {
  auto cfg = getVConfig();
  checkVWidenable(cfg);
  auto n = numVElts(cfg.SEW, cfg.LMULLog2);
  auto S = cfg.SEW, W = 2 * cfg.SEW;
  auto L = cfg.LMULLog2, WL = cfg.LMULLog2 + 1;
  auto mask = readVMaskOperand(4, n);
  auto vd = vopAt(0, W, WL);
  vector<VOperand> srcs{vd, vopAt(3, S, L)};
  addVSrc(srcs, 2, S, L);
  bool agnostic = checkVOverlap(vd, srcs, mask != nullptr);
  auto d = readVGroup(vd.first, W, WL);
  auto s1 = createVExt(readVOperand(2, cfg, /*zextImm=*/false), W, s1Signed);
  auto s2 = createVExt(readVGroup(srcs[1].first, S, L), W, s2Signed);
  auto res = createAdd(createMul(s1, s2), d);
  writeVGroup(vd.first, W, WL, res, readVL(), mask,
              cfg.tailAgnostic || agnostic, cfg.maskAgnostic || agnostic);
}

/*
 * add-with-carry and subtract-with-borrow. the carry or borrow input is
 * always the mask in v0, and all body elements are written whether or not
 * the encoding is masked
 */

// vadc.v{v,x,i}m vd, vs2, src, v0: vd[i] = vs2[i] + src[i] + v0.mask[i]
// vsbc.v{v,x}m vd, vs2, src, v0:   vd[i] = vs2[i] - src[i] - v0.mask[i]
void riscv2llvm::liftVAdcSbc(bool isSub) {
  auto cfg = getVConfig();
  auto n = numVElts(cfg.SEW, cfg.LMULLog2);
  auto S = cfg.SEW;
  auto L = cfg.LMULLog2;
  assert(CurInst->getOperand(3).getReg() == RISCV::V0);
  vector<VOperand> srcs{vopAt(1, S, L)};
  addVSrc(srcs, 2, S, L);
  // this also rejects vd = v0, which the spec singles out as reserved
  checkVOverlap(vopAt(0, S, L), srcs, /*readsV0Mask=*/true);
  auto a = readVGroup(srcs[0].first, S, L);
  auto b = readVOperand(2, cfg, /*zextImm=*/false);
  auto c = createVExt(readVMask(0, n), S, /*isSigned=*/false);
  auto res = isSub ? createSub(createSub(a, b), c)
                   : createAdd(createAdd(a, b), c);
  writeVGroup(vopAt(0, S, L).first, S, L, res, readVL(), nullptr,
              cfg.tailAgnostic, cfg.maskAgnostic);
}

// vmadc.v{v,x,i}m vd, vs2, src, v0 / vmadc.v{v,x,i} vd, vs2, src:
//   vd.mask[i] = carry_out(vs2[i] + src[i] (+ v0.mask[i]))
// vmsbc.v{v,x}m vd, vs2, src, v0 / vmsbc.v{v,x} vd, vs2, src:
//   vd.mask[i] = borrow_out(vs2[i] - src[i] (- v0.mask[i]))
//
// the forms with the v0 operand take a carry or borrow input, the others
// do not. the borrow is 1 iff the untruncated difference is negative.
// the result is a mask, so it may be written to v0
void riscv2llvm::liftVMadcMsbc(bool isSub, bool hasCarryIn) {
  auto cfg = getVConfig();
  auto n = numVElts(cfg.SEW, cfg.LMULLog2);
  auto S = cfg.SEW;
  auto L = cfg.LMULLog2;
  if (hasCarryIn)
    assert(CurInst->getOperand(3).getReg() == RISCV::V0);
  vector<VOperand> srcs{vopAt(1, S, L)};
  addVSrc(srcs, 2, S, L);
  auto vd = vopAt(0, 1, 0);
  checkVOverlap(vd, srcs, hasCarryIn);
  // SEW + 1 bits hold the full sum, and the full subtrahend
  unsigned X = S + 1;
  auto a = createVExt(readVGroup(srcs[0].first, S, L), X, /*isSigned=*/false);
  auto b = createVExt(readVOperand(2, cfg, /*zextImm=*/false), X,
                      /*isSigned=*/false);
  Value *c = hasCarryIn
                 ? createVExt(readVMask(0, n), X, /*isSigned=*/false)
                 : static_cast<Value *>(Constant::getNullValue(getVecTy(X, n)));
  Value *out;
  if (isSub) {
    out = createICmp(ICmpInst::ICMP_ULT, a, createAdd(b, c));
  } else {
    auto sum = createAdd(createAdd(a, b), c);
    out = createICmp(ICmpInst::ICMP_UGE, sum,
                     ConstantInt::get(getVecTy(X, n),
                                      APInt::getOneBitSet(X, S)));
  }
  // every body element is active, so the mask policy never applies
  writeVMask(vd.first, out, readVL(), nullptr, /*maskAgnostic=*/false);
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

#define VWIDE(NAME, OP, WIDEVS2)                                               \
  case RISCV::NAME:                                                            \
    liftVWidenBinary(VWideOp::OP, WIDEVS2);                                    \
    return true;
    VWIDE(VWADDU_VV, AddU, false)
    VWIDE(VWADDU_VX, AddU, false)
    VWIDE(VWADD_VV, Add, false)
    VWIDE(VWADD_VX, Add, false)
    VWIDE(VWSUBU_VV, SubU, false)
    VWIDE(VWSUBU_VX, SubU, false)
    VWIDE(VWSUB_VV, Sub, false)
    VWIDE(VWSUB_VX, Sub, false)
    VWIDE(VWADDU_WV, AddU, true)
    VWIDE(VWADDU_WX, AddU, true)
    VWIDE(VWADD_WV, Add, true)
    VWIDE(VWADD_WX, Add, true)
    VWIDE(VWSUBU_WV, SubU, true)
    VWIDE(VWSUBU_WX, SubU, true)
    VWIDE(VWSUB_WV, Sub, true)
    VWIDE(VWSUB_WX, Sub, true)
    VWIDE(VWMULU_VV, MulU, false)
    VWIDE(VWMULU_VX, MulU, false)
    VWIDE(VWMUL_VV, Mul, false)
    VWIDE(VWMUL_VX, Mul, false)
    VWIDE(VWMULSU_VV, MulSU, false)
    VWIDE(VWMULSU_VX, MulSU, false)
#undef VWIDE

  case RISCV::VZEXT_VF2:
    liftVExtend(2, false);
    return true;
  case RISCV::VZEXT_VF4:
    liftVExtend(4, false);
    return true;
  case RISCV::VZEXT_VF8:
    liftVExtend(8, false);
    return true;
  case RISCV::VSEXT_VF2:
    liftVExtend(2, true);
    return true;
  case RISCV::VSEXT_VF4:
    liftVExtend(4, true);
    return true;
  case RISCV::VSEXT_VF8:
    liftVExtend(8, true);
    return true;

  case RISCV::VNSRL_WV:
  case RISCV::VNSRL_WX:
  case RISCV::VNSRL_WI:
    liftVNarrowShift(false);
    return true;
  case RISCV::VNSRA_WV:
  case RISCV::VNSRA_WX:
  case RISCV::VNSRA_WI:
    liftVNarrowShift(true);
    return true;

#define VMAC(NAME, OP)                                                         \
  case RISCV::NAME##_VV:                                                       \
  case RISCV::NAME##_VX:                                                       \
    liftVMulAdd(VMacOp::OP);                                                   \
    return true;
    VMAC(VMACC, Macc)
    VMAC(VNMSAC, Nmsac)
    VMAC(VMADD, Madd)
    VMAC(VNMSUB, Nmsub)
#undef VMAC

  case RISCV::VWMACCU_VV:
  case RISCV::VWMACCU_VX:
    liftVWidenMulAdd(false, false);
    return true;
  case RISCV::VWMACC_VV:
  case RISCV::VWMACC_VX:
    liftVWidenMulAdd(true, true);
    return true;
  case RISCV::VWMACCSU_VV:
  case RISCV::VWMACCSU_VX:
    liftVWidenMulAdd(true, false);
    return true;
  case RISCV::VWMACCUS_VX:
    liftVWidenMulAdd(false, true);
    return true;

  case RISCV::VADC_VVM:
  case RISCV::VADC_VXM:
  case RISCV::VADC_VIM:
    liftVAdcSbc(false);
    return true;
  case RISCV::VSBC_VVM:
  case RISCV::VSBC_VXM:
    liftVAdcSbc(true);
    return true;
  case RISCV::VMADC_VVM:
  case RISCV::VMADC_VXM:
  case RISCV::VMADC_VIM:
    liftVMadcMsbc(false, true);
    return true;
  case RISCV::VMADC_VV:
  case RISCV::VMADC_VX:
  case RISCV::VMADC_VI:
    liftVMadcMsbc(false, false);
    return true;
  case RISCV::VMSBC_VVM:
  case RISCV::VMSBC_VXM:
    liftVMadcMsbc(true, true);
    return true;
  case RISCV::VMSBC_VV:
  case RISCV::VMSBC_VX:
    liftVMadcMsbc(true, false);
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
