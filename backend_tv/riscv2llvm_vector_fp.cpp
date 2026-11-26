/*
 * lifting for the floating-point instructions of the RVV vector
 * extension, as included in RVA23U64: V gives f32 and f64 arithmetic, and
 * Zvfhmin adds only the conversions between f16 and f32
 *
 * the register file holds every element as an integer, so FP elements
 * are bitcast to FP for arithmetic and back to integers to be written.
 * like the scalar FP lifting:
 *
 * - the dynamic rounding mode (frm) is assumed to be round to nearest,
 *   ties to even, so the lifted code uses plain LLVM FP operations
 * - fflags are not modeled
 * - computational instructions return the canonical NaN, while moves,
 *   merges, slides, and sign injection preserve NaN payloads
 * - a scalar f register operand narrower than FLEN that is not properly
 *   NaN-boxed reads as the canonical NaN
 *
 * reference: src/unpriv/vector-floating-point.adoc and
 * src/unpriv/vector-reductions.adoc in the RISC-V ISA manual
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

// RISC-V rounding-mode encodings used with liftRoundingToInt()
static constexpr int64_t RoundTowardZero = 1;
static constexpr int64_t RoundDynamic = 7;

/*
 * helpers
 */

bool riscv2llvm::isFPRegOperand(int idx) {
  auto &op = CurInst->getOperand(idx);
  return op.isReg() && op.getReg() >= RISCV::F0_D &&
         op.getReg() <= RISCV::F31_Q;
}

// the element width of an FP operand has to name a format that RVA23U64
// does arithmetic on. the f16 conversions of Zvfhmin check for themselves
void riscv2llvm::checkVFWidth(unsigned width) {
  if (width == 16)
    vectorError("vector half-precision arithmetic needs Zvfh, which "
                "RVA23U64 does not include");
  if (width != 32 && width != 64)
    vectorError("vector floating-point instruction with an element width of " +
                to_string(width) + " bits, which names no FP format");
}

// the vtype for an instruction whose SEW-bit elements are floats
riscv2llvm::VConfig riscv2llvm::getVFConfig() {
  auto cfg = getVConfig();
  checkVFWidth(cfg.SEW);
  return cfg;
}

// reinterpret a vector of integers as floats of the same width, and back
Value *riscv2llvm::toVF(Value *v) {
  auto *ty = cast<FixedVectorType>(v->getType());
  return createBitCast(v, getVecTy(getBitWidth(ty->getElementType()),
                                   ty->getNumElements(), /*isFP=*/true));
}

Value *riscv2llvm::toVI(Value *v) {
  return createBitCast(v, VectorType::getInteger(cast<VectorType>(
                              v->getType())));
}

// the f register operand `idx` as an SEW-bit float, NaN-boxing checked
Value *riscv2llvm::readVFScalar(int idx, unsigned SEW) {
  assert(isFPRegOperand(idx));
  return readFromFPRegOperand(idx, getFPType(SEW));
}

Value *riscv2llvm::readVFGroup(unsigned first, unsigned EEW, int EMULLog2) {
  return toVF(readVGroup(first, EEW, EMULLog2));
}

// operand `idx` as a vector of VLMAX SEW-bit floats: a register group,
// or an f register splatted to every lane
Value *riscv2llvm::readVFOperand(int idx, const VConfig &cfg) {
  if (auto first = vgroupOperand(idx))
    return readVFGroup(*first, cfg.SEW, cfg.LMULLog2);
  return createVSplat(readVFScalar(idx, cfg.SEW),
                      numVElts(cfg.SEW, cfg.LMULLog2));
}

// elementwise `a op b` on FP vectors, where `a` comes from vs2
Value *riscv2llvm::createVFOp(VFOp op, Value *a, Value *b) {
  switch (op) {
  case VFOp::Add:
    return canonicalizeNaN(createFAdd(a, b));
  case VFOp::Sub:
    return canonicalizeNaN(createFSub(a, b));
  case VFOp::RSub:
    return canonicalizeNaN(createFSub(b, a));
  case VFOp::Mul:
    return canonicalizeNaN(createFMul(a, b));
  case VFOp::Div:
    return canonicalizeNaN(createFDiv(a, b));
  case VFOp::RDiv:
    return canonicalizeNaN(createFDiv(b, a));
  // IEEE 754-2019 minimumNumber and maximumNumber, as for the scalar
  // fmin and fmax: a NaN operand is ignored unless both are NaN
  case VFOp::Min:
    return canonicalizeNaN(createMinimumNum(a, b));
  case VFOp::Max:
    return canonicalizeNaN(createMaximumNum(a, b));

  // sign injection works on the bits, so NaN payloads are kept
  case VFOp::SgnJ:
  case VFOp::SgnJN:
  case VFOp::SgnJX: {
    auto ai = toVI(a), bi = toVI(b);
    auto *ty = cast<FixedVectorType>(ai->getType());
    unsigned w = getBitWidth(ty->getElementType());
    auto sign = ConstantInt::get(ty, APInt::getSignMask(w));
    auto rest = ConstantInt::get(ty, APInt::getSignedMaxValue(w));
    Value *signSrc = op == VFOp::SgnJ    ? bi
                     : op == VFOp::SgnJN ? createXor(bi, sign)
                                         : createXor(ai, bi);
    return toVF(createOr(createAnd(ai, rest), createAnd(signSrc, sign)));
  }
  }
  assert(false && "unhandled VFOp");
  return nullptr;
}

// convert each element of `v` to the vector type `toTy`
Value *riscv2llvm::createVFConvert(Value *v, VFCvtKind kind, bool isSigned,
                                   Type *toTy) {
  switch (kind) {
  // FP to integer saturates. a NaN becomes the largest integer, which
  // LLVM's saturating conversions would map to 0
  case VFCvtKind::FToI:
  case VFCvtKind::FToIRtz: {
    auto rounded = liftRoundingToInt(
        v, kind == VFCvtKind::FToIRtz ? RoundTowardZero : RoundDynamic);
    auto *intTy = cast<FixedVectorType>(toTy);
    unsigned w = getBitWidth(intTy->getElementType());
    Value *res = isSigned ? createFPToSI_sat(rounded, toTy)
                          : createFPToUI_sat(rounded, toTy);
    auto nanVal = ConstantInt::get(intTy, isSigned ? APInt::getSignedMaxValue(w)
                                                   : APInt::getAllOnes(w));
    return createSelect(createIsFPClass(v, fcNan), nanVal, res);
  }
  case VFCvtKind::IToF:
    return isSigned ? static_cast<Value *>(createSIToFP(v, toTy))
                    : static_cast<Value *>(createUIToFP(v, toTy));
  case VFCvtKind::FToF: {
    unsigned from = getBitWidth(v->getType()->getScalarType());
    unsigned to = getBitWidth(toTy->getScalarType());
    Value *res = to > from ? static_cast<Value *>(createFPExt(v, toTy))
                           : static_cast<Value *>(createFPTrunc(v, toTy));
    return canonicalizeNaN(res);
  }
  }
  assert(false && "unhandled VFCvtKind");
  return nullptr;
}

/*
 * single-width arithmetic. the operand layouts are those of the integer
 * instructions: (vd, vs2, vs1 | fs1, vm) for binary operations,
 * (vd, vs2, vm) for unary ones
 */

// vfop.vv vd, vs2, vs1, vm / vfop.vf vd, vs2, fs1, vm
void riscv2llvm::liftVFBinary(VFOp op) {
  auto cfg = getVFConfig();
  auto n = numVElts(cfg.SEW, cfg.LMULLog2);
  auto S = cfg.SEW;
  auto L = cfg.LMULLog2;
  auto mask = readVMaskOperand(3, n);
  vector<VOperand> srcs{vopAt(1, S, L)};
  addVSrc(srcs, 2, S, L);
  checkVOverlap(vopAt(0, S, L), srcs, mask != nullptr);
  auto a = readVFGroup(srcs[0].first, S, L);
  auto b = readVFOperand(2, cfg);
  writeVGroup(vopAt(0, S, L).first, S, L, toVI(createVFOp(op, a, b)),
              readVL(), mask, cfg.tailAgnostic, cfg.maskAgnostic);
}

// vfsqrt.v vd, vs2, vm
// vfclass.v vd, vs2, vm: an SEW-bit integer with the bit for vs2[i]'s
// class set, numbered as for the scalar fclass
void riscv2llvm::liftVFUnary(bool isClass) {
  auto cfg = getVFConfig();
  auto n = numVElts(cfg.SEW, cfg.LMULLog2);
  auto S = cfg.SEW;
  auto L = cfg.LMULLog2;
  auto mask = readVMaskOperand(2, n);
  auto vs2 = vopAt(1, S, L);
  auto vd = vopAt(0, S, L);
  checkVOverlap(vd, {vs2}, mask != nullptr);
  auto a = readVFGroup(vs2.first, S, L);
  Value *res;
  if (isClass) {
    res = getElemSplat(n, S, 1); // negative infinity
    auto mark = [&](FPClassTest test, unsigned bit) {
      res = createSelect(createIsFPClass(a, test),
                         getElemSplat(n, S, 1ULL << bit), res);
    };
    mark(fcNegNormal, 1);
    mark(fcNegSubnormal, 2);
    mark(fcNegZero, 3);
    mark(fcPosZero, 4);
    mark(fcPosSubnormal, 5);
    mark(fcPosNormal, 6);
    mark(fcPosInf, 7);
    mark(fcSNan, 8);
    mark(fcQNan, 9);
  } else {
    res = toVI(canonicalizeNaN(createSQRT(a)));
  }
  writeVGroup(vd.first, S, L, res, readVL(), mask, cfg.tailAgnostic,
              cfg.maskAgnostic);
}

/*
 * fused multiply-add, with the operand layout (vd, vd, vs1 | fs1, vs2, vm)
 * of the integer multiply-adds. as for the scalar instructions, negations
 * apply to the inputs, so the sign of an exact zero result is right:
 *
 *   vfmacc:  vd = +(vs1 * vs2) + vd     vfmadd:  vd = +(vs1 * vd) + vs2
 *   vfnmacc: vd = -(vs1 * vs2) - vd     vfnmadd: vd = -(vs1 * vd) - vs2
 *   vfmsac:  vd = +(vs1 * vs2) - vd     vfmsub:  vd = +(vs1 * vd) - vs2
 *   vfnmsac: vd = -(vs1 * vs2) + vd     vfnmsub: vd = -(vs1 * vd) + vs2
 */
void riscv2llvm::vfMacFlags(VFMacOp op, bool &negProduct, bool &negAddend,
                            bool &overwritesMultiplicand) {
  using Op = VFMacOp;
  negProduct = op == Op::NMacc || op == Op::NMsac || op == Op::NMadd ||
               op == Op::NMsub;
  negAddend = op == Op::NMacc || op == Op::Msac || op == Op::NMadd ||
              op == Op::Msub;
  overwritesMultiplicand =
      op == Op::Madd || op == Op::NMadd || op == Op::Msub || op == Op::NMsub;
}

void riscv2llvm::liftVFMulAdd(VFMacOp op) {
  auto cfg = getVFConfig();
  auto n = numVElts(cfg.SEW, cfg.LMULLog2);
  auto S = cfg.SEW;
  auto L = cfg.LMULLog2;
  auto mask = readVMaskOperand(4, n);
  auto vd = vopAt(0, S, L);
  vector<VOperand> srcs{vd, vopAt(3, S, L)};
  addVSrc(srcs, 2, S, L);
  checkVOverlap(vd, srcs, mask != nullptr);
  auto d = readVFGroup(vd.first, S, L);
  auto s1 = readVFOperand(2, cfg);
  auto s2 = readVFGroup(srcs[1].first, S, L);
  bool negProduct, negAddend, ofMultiplicand;
  vfMacFlags(op, negProduct, negAddend, ofMultiplicand);
  Value *other = ofMultiplicand ? d : s2;
  Value *addend = ofMultiplicand ? s2 : d;
  Value *m1 = negProduct ? createFNeg(s1) : s1;
  Value *c = negAddend ? createFNeg(addend) : addend;
  auto res = canonicalizeNaN(createFusedMultiplyAdd(m1, other, c));
  writeVGroup(vd.first, S, L, toVI(res), readVL(), mask, cfg.tailAgnostic,
              cfg.maskAgnostic);
}

// vmfCC.vv vd, vs2, vs1, vm / vmfCC.vf vd, vs2, fs1, vm. vmfne is true for
// unordered operands and the others are false. the destination is a mask,
// as for the integer compares
void riscv2llvm::liftVFCompare(CmpInst::Predicate pred) {
  auto cfg = getVFConfig();
  auto n = numVElts(cfg.SEW, cfg.LMULLog2);
  auto S = cfg.SEW;
  auto L = cfg.LMULLog2;
  auto mask = readVMaskOperand(3, n);
  vector<VOperand> srcs{vopAt(1, S, L)};
  addVSrc(srcs, 2, S, L);
  auto vd = vopAt(0, 1, 0);
  bool agnostic = checkVOverlap(vd, srcs, mask != nullptr);
  auto a = readVFGroup(srcs[0].first, S, L);
  auto b = readVFOperand(2, cfg);
  writeVMask(vd.first, createFCmp(pred, a, b), readVL(), mask,
             cfg.maskAgnostic || agnostic);
}

// vfmv.f.s fd, vs2: fd = vs2[0], NaN-boxed. like vmv.x.s, this ignores vl
void riscv2llvm::liftVFMvFS() {
  auto cfg = getVFConfig();
  auto vs2 = vregRange(CurInst->getOperand(1).getReg()).first;
  auto reg = createLoad(getIntTy(VLEN), lookupVReg(vs2));
  auto elt = createBitCast(createTrunc(reg, getIntTy(cfg.SEW)),
                           getFPType(cfg.SEW));
  updateFPReg(elt, CurInst->getOperand(0).getReg());
}

/*
 * conversions, (vd, vs2, vm). `widthChange` is 0 for the single-width
 * vfcvt, 1 for the widening vfwcvt (vd has EEW = 2 * SEW), and -1 for the
 * narrowing vfncvt (vs2 has EEW = 2 * SEW). the integer side of a
 * conversion may have any element width, and the FP side has to be f32
 * or f64, except that vfwcvt.f.f.v and vfncvt.f.f.w also convert between
 * f16 and f32 (Zvfhmin)
 */
void riscv2llvm::liftVFCvt(VFCvtKind kind, bool isSigned, int widthChange) {
  auto cfg = getVConfig();
  if (widthChange != 0)
    checkVWidenable(cfg);
  auto n = numVElts(cfg.SEW, cfg.LMULLog2);
  auto S = cfg.SEW;
  auto L = cfg.LMULLog2;
  unsigned srcW = widthChange < 0 ? 2 * S : S;
  int srcL = widthChange < 0 ? L + 1 : L;
  unsigned dstW = widthChange > 0 ? 2 * S : S;
  int dstL = widthChange > 0 ? L + 1 : L;
  bool srcFP = kind != VFCvtKind::IToF;
  bool dstFP = kind == VFCvtKind::IToF || kind == VFCvtKind::FToF;
  auto checkFP = [&](unsigned w) {
    if (!(kind == VFCvtKind::FToF && w == 16))
      checkVFWidth(w);
  };
  if (srcFP)
    checkFP(srcW);
  if (dstFP)
    checkFP(dstW);

  auto mask = readVMaskOperand(2, n);
  auto vs2 = vopAt(1, srcW, srcL);
  auto vd = vopAt(0, dstW, dstL);
  bool agnostic = checkVOverlap(vd, {vs2}, mask != nullptr);
  Value *src = readVGroup(vs2.first, srcW, srcL);
  if (srcFP)
    src = toVF(src);
  auto res = createVFConvert(src, kind, isSigned, getVecTy(dstW, n, dstFP));
  if (dstFP)
    res = toVI(res);
  writeVGroup(vd.first, dstW, dstL, res, readVL(), mask,
              cfg.tailAgnostic || agnostic, cfg.maskAgnostic || agnostic);
}

/*
 * widening arithmetic: the SEW-bit operands are converted to 2 * SEW bits,
 * which is exact, and the operation is done at the wider precision
 */

// vfwop.vv vd, vs2, vs1, vm / .vf vd, vs2, fs1, vm: wide = SEW op SEW
// vfwop.wv vd, vs2, vs1, vm / .wf vd, vs2, fs1, vm: wide = wide op SEW
void riscv2llvm::liftVFWidenBinary(VFOp op, bool wideVs2) {
  auto cfg = getVFConfig();
  checkVWidenable(cfg);
  auto n = numVElts(cfg.SEW, cfg.LMULLog2);
  auto S = cfg.SEW, W = 2 * cfg.SEW;
  auto L = cfg.LMULLog2, WL = cfg.LMULLog2 + 1;
  auto mask = readVMaskOperand(3, n);
  vector<VOperand> srcs{wideVs2 ? vopAt(1, W, WL) : vopAt(1, S, L)};
  addVSrc(srcs, 2, S, L);
  auto vd = vopAt(0, W, WL);
  bool agnostic = checkVOverlap(vd, srcs, mask != nullptr);
  auto wideTy = getVecTy(W, n, /*isFP=*/true);
  Value *a = wideVs2 ? readVFGroup(srcs[0].first, W, WL)
                     : createFPExt(readVFGroup(srcs[0].first, S, L), wideTy);
  Value *b = createFPExt(readVFOperand(2, cfg), wideTy);
  writeVGroup(vd.first, W, WL, toVI(createVFOp(op, a, b)), readVL(), mask,
              cfg.tailAgnostic || agnostic, cfg.maskAgnostic || agnostic);
}

// vfw{n}macc / vfw{n}msac .vv vd, vs1, vs2, vm / .vf vd, fs1, vs2, vm:
// the wide accumulator vd is updated as for the single-width forms
void riscv2llvm::liftVFWidenMulAdd(VFMacOp op) {
  auto cfg = getVFConfig();
  checkVWidenable(cfg);
  auto n = numVElts(cfg.SEW, cfg.LMULLog2);
  auto S = cfg.SEW, W = 2 * cfg.SEW;
  auto L = cfg.LMULLog2, WL = cfg.LMULLog2 + 1;
  auto mask = readVMaskOperand(4, n);
  auto vd = vopAt(0, W, WL);
  vector<VOperand> srcs{vd, vopAt(3, S, L)};
  addVSrc(srcs, 2, S, L);
  bool agnostic = checkVOverlap(vd, srcs, mask != nullptr);
  auto wideTy = getVecTy(W, n, /*isFP=*/true);
  auto d = readVFGroup(vd.first, W, WL);
  auto s1 = createFPExt(readVFOperand(2, cfg), wideTy);
  auto s2 = createFPExt(readVFGroup(srcs[1].first, S, L), wideTy);
  bool negProduct, negAddend, ofMultiplicand;
  vfMacFlags(op, negProduct, negAddend, ofMultiplicand);
  assert(!ofMultiplicand);
  Value *m1 = negProduct ? createFNeg(s1) : s1;
  Value *c = negAddend ? createFNeg(d) : d;
  auto res = canonicalizeNaN(createFusedMultiplyAdd(m1, s2, c));
  writeVGroup(vd.first, W, WL, toVI(res), readVL(), mask,
              cfg.tailAgnostic || agnostic, cfg.maskAgnostic || agnostic);
}

/*
 * reductions, (vd, vs2, vs1, vm): vd[0] = vs1[0] combined with each active
 * element of vs2. the ordered sum adds the elements in increasing order,
 * and min and max follow vfmin and vfmax, whose result doesn't depend on
 * the order. if no element is active, the result is vs1[0]. for the
 * widening ordered sum, vs1[0] and vd[0] have 2 * SEW bits
 *
 * the unordered sums (vfredusum, vfwredusum) may combine the elements in
 * any order, which can change the rounded result; they aren't lifted
 */
void riscv2llvm::liftVFReduce(VFRedOp op, bool widen) {
  auto cfg = getVFConfig();
  auto n = numVElts(cfg.SEW, cfg.LMULLog2);
  auto S = cfg.SEW, W = widen ? 2 * cfg.SEW : cfg.SEW;
  auto L = cfg.LMULLog2;
  if (widen && S >= 64)
    vectorError("widening reduction with SEW = 64, which is reserved");
  auto mask = readVMaskOperand(3, n);
  auto vs2 = vopAt(1, S, L);
  auto vs1 = vopAt(2, W, 0);
  checkVSources({vs2, vs1}, mask != nullptr);
  auto vd = vregRange(CurInst->getOperand(0).getReg()).first;

  Value *src = readVFGroup(vs2.first, S, L);
  if (widen)
    src = createFPExt(src, getVecTy(W, n, /*isFP=*/true));
  auto active = createActiveMask(mask, n);
  Value *start = createBitCast(
      createTrunc(createLoad(getIntTy(VLEN), lookupVReg(vs1.first)),
                  getIntTy(W)),
      getFPType(W));
  Value *acc = start;
  Value *any = getBoolConst(false);
  for (unsigned i = 0; i < n; ++i) {
    auto e = createExtractElement(src, i);
    auto on = createExtractElement(active, i);
    Value *next = op == VFRedOp::OSum  ? createFAdd(acc, e)
                  : op == VFRedOp::Min ? createMinimumNum(acc, e)
                                       : createMaximumNum(acc, e);
    acc = createSelect(on, next, acc);
    any = createOr(any, on);
  }
  auto res = createSelect(any, canonicalizeNaN(acc), start);
  writeVElt0(vd, createBitCast(res, getIntTy(W)), cfg);
}

bool riscv2llvm::liftVectorFP(unsigned opcode) {
  switch (opcode) {
#define VF_VV_VF(NAME, OP)                                                     \
  case RISCV::NAME##_VV:                                                       \
  case RISCV::NAME##_VF:                                                       \
    liftVFBinary(VFOp::OP);                                                    \
    return true;
    VF_VV_VF(VFADD, Add)
    VF_VV_VF(VFSUB, Sub)
    VF_VV_VF(VFMUL, Mul)
    VF_VV_VF(VFDIV, Div)
    VF_VV_VF(VFMIN, Min)
    VF_VV_VF(VFMAX, Max)
    VF_VV_VF(VFSGNJ, SgnJ)
    VF_VV_VF(VFSGNJN, SgnJN)
    VF_VV_VF(VFSGNJX, SgnJX)
#undef VF_VV_VF
  case RISCV::VFRSUB_VF:
    liftVFBinary(VFOp::RSub);
    return true;
  case RISCV::VFRDIV_VF:
    liftVFBinary(VFOp::RDiv);
    return true;

  case RISCV::VFSQRT_V:
    liftVFUnary(/*isClass=*/false);
    return true;
  case RISCV::VFCLASS_V:
    liftVFUnary(/*isClass=*/true);
    return true;

#define VFMAC(NAME, OP)                                                        \
  case RISCV::NAME##_VV:                                                       \
  case RISCV::NAME##_VF:                                                       \
    liftVFMulAdd(VFMacOp::OP);                                                 \
    return true;
    VFMAC(VFMACC, Macc)
    VFMAC(VFNMACC, NMacc)
    VFMAC(VFMSAC, Msac)
    VFMAC(VFNMSAC, NMsac)
    VFMAC(VFMADD, Madd)
    VFMAC(VFNMADD, NMadd)
    VFMAC(VFMSUB, Msub)
    VFMAC(VFNMSUB, NMsub)
#undef VFMAC

#define VFCMP(NAME, PRED)                                                      \
  case RISCV::NAME:                                                            \
    liftVFCompare(FCmpInst::PRED);                                             \
    return true;
    VFCMP(VMFEQ_VV, FCMP_OEQ)
    VFCMP(VMFEQ_VF, FCMP_OEQ)
    VFCMP(VMFNE_VV, FCMP_UNE)
    VFCMP(VMFNE_VF, FCMP_UNE)
    VFCMP(VMFLT_VV, FCMP_OLT)
    VFCMP(VMFLT_VF, FCMP_OLT)
    VFCMP(VMFLE_VV, FCMP_OLE)
    VFCMP(VMFLE_VF, FCMP_OLE)
    VFCMP(VMFGT_VF, FCMP_OGT)
    VFCMP(VMFGE_VF, FCMP_OGE)
#undef VFCMP

  // vfmv.v.f vd, fs1: unmasked, like vmv.v.x
  case RISCV::VFMV_V_F: {
    auto cfg = getVFConfig();
    auto n = numVElts(cfg.SEW, cfg.LMULLog2);
    auto vd = vopAt(0, cfg.SEW, cfg.LMULLog2);
    checkVOverlap(vd, {}, /*readsV0Mask=*/false);
    writeVGroup(vd.first, cfg.SEW, cfg.LMULLog2,
                createVSplat(readVScalarBits(1, cfg.SEW), n), readVL(),
                nullptr, cfg.tailAgnostic, cfg.maskAgnostic);
    return true;
  }
  // vfmv.s.f vd, fs1, with the operands (vd, vd, fs1), like vmv.s.x
  case RISCV::VFMV_S_F: {
    auto cfg = getVFConfig();
    auto vd = vregRange(CurInst->getOperand(0).getReg()).first;
    writeVElt0(vd, readVScalarBits(2, cfg.SEW), cfg);
    return true;
  }
  case RISCV::VFMV_F_S:
    liftVFMvFS();
    return true;
  // these are the integer instructions with an f register operand, which
  // readVOperand() and liftVSlide1() read
  case RISCV::VFMERGE_VFM:
    checkVFWidth(getVConfig().SEW);
    liftVMerge();
    return true;
  case RISCV::VFSLIDE1UP_VF:
    liftVSlide1(true);
    return true;
  case RISCV::VFSLIDE1DOWN_VF:
    liftVSlide1(false);
    return true;

#define VFCVT(NAME, KIND, SIGNED, WIDTH)                                       \
  case RISCV::NAME:                                                            \
    liftVFCvt(VFCvtKind::KIND, SIGNED, WIDTH);                                 \
    return true;
    VFCVT(VFCVT_XU_F_V, FToI, false, 0)
    VFCVT(VFCVT_X_F_V, FToI, true, 0)
    VFCVT(VFCVT_RTZ_XU_F_V, FToIRtz, false, 0)
    VFCVT(VFCVT_RTZ_X_F_V, FToIRtz, true, 0)
    VFCVT(VFCVT_F_XU_V, IToF, false, 0)
    VFCVT(VFCVT_F_X_V, IToF, true, 0)
    VFCVT(VFWCVT_XU_F_V, FToI, false, 1)
    VFCVT(VFWCVT_X_F_V, FToI, true, 1)
    VFCVT(VFWCVT_RTZ_XU_F_V, FToIRtz, false, 1)
    VFCVT(VFWCVT_RTZ_X_F_V, FToIRtz, true, 1)
    VFCVT(VFWCVT_F_XU_V, IToF, false, 1)
    VFCVT(VFWCVT_F_X_V, IToF, true, 1)
    VFCVT(VFWCVT_F_F_V, FToF, false, 1)
    VFCVT(VFNCVT_XU_F_W, FToI, false, -1)
    VFCVT(VFNCVT_X_F_W, FToI, true, -1)
    VFCVT(VFNCVT_RTZ_XU_F_W, FToIRtz, false, -1)
    VFCVT(VFNCVT_RTZ_X_F_W, FToIRtz, true, -1)
    VFCVT(VFNCVT_F_XU_W, IToF, false, -1)
    VFCVT(VFNCVT_F_X_W, IToF, true, -1)
    VFCVT(VFNCVT_F_F_W, FToF, false, -1)
#undef VFCVT

#define VFWIDE(NAME, OP, WIDEVS2)                                              \
  case RISCV::NAME:                                                            \
    liftVFWidenBinary(VFOp::OP, WIDEVS2);                                      \
    return true;
    VFWIDE(VFWADD_VV, Add, false)
    VFWIDE(VFWADD_VF, Add, false)
    VFWIDE(VFWSUB_VV, Sub, false)
    VFWIDE(VFWSUB_VF, Sub, false)
    VFWIDE(VFWADD_WV, Add, true)
    VFWIDE(VFWADD_WF, Add, true)
    VFWIDE(VFWSUB_WV, Sub, true)
    VFWIDE(VFWSUB_WF, Sub, true)
    VFWIDE(VFWMUL_VV, Mul, false)
    VFWIDE(VFWMUL_VF, Mul, false)
#undef VFWIDE

#define VFWMAC(NAME, OP)                                                       \
  case RISCV::NAME##_VV:                                                       \
  case RISCV::NAME##_VF:                                                       \
    liftVFWidenMulAdd(VFMacOp::OP);                                            \
    return true;
    VFWMAC(VFWMACC, Macc)
    VFWMAC(VFWNMACC, NMacc)
    VFWMAC(VFWMSAC, Msac)
    VFWMAC(VFWNMSAC, NMsac)
#undef VFWMAC

  case RISCV::VFREDOSUM_VS:
    liftVFReduce(VFRedOp::OSum, /*widen=*/false);
    return true;
  case RISCV::VFREDMIN_VS:
    liftVFReduce(VFRedOp::Min, /*widen=*/false);
    return true;
  case RISCV::VFREDMAX_VS:
    liftVFReduce(VFRedOp::Max, /*widen=*/false);
    return true;
  case RISCV::VFWREDOSUM_VS:
    liftVFReduce(VFRedOp::OSum, /*widen=*/true);
    return true;
  case RISCV::VFREDUSUM_VS:
  case RISCV::VFWREDUSUM_VS:
    vectorError("unordered floating-point reductions are not supported: "
                "their rounded result depends on an unspecified order of "
                "evaluation");

  default:
    return false;
  }
}
