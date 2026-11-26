#include "backend_tv/riscv2llvm.h"
#include "backend_tv/abi.h"

#include "Target/RISCV/MCTargetDesc/RISCVMCAsmInfo.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/BinaryFormat/ELF.h"
#include "util/config.h"

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
  if (W == 128) {
    createStore(V, lookupFPReg(Reg));
  } else {
    // NaN-box smaller FP types
    auto bits = createBitCast(V, getIntTy(W));
    auto extended = createZExt(bits, getIntTy(128));
    auto maskAP = llvm::APInt::getHighBitsSet(128, 128 - W);
    auto mask = llvm::ConstantInt::get(Ctx, maskAP);
    auto nanBoxed = createOr(mask, extended);
    createStore(nanBoxed, lookupFPReg(Reg));
  }
}

Value *riscv2llvm::canonicalizeNaN(Value *V) {
  assert(V->getType()->isFPOrFPVectorTy());

  // RISC-V computational FP instructions produce the canonical NaN. Keep this
  // separate from updateFPReg: transfers and sign injection preserve payloads.
  // for a vector, this applies to each element
  auto isNaN = createIsFPClass(V, fcNan);
  auto canonical =
      APFloat::getQNaN(V->getType()->getScalarType()->getFltSemantics());
  return createSelect(isNaN, ConstantFP::get(V->getType(), canonical), V);
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
  if (Reg == RISCV::VL || Reg == RISCV::VTYPE || Reg == RISCV::VLENB)
    return 64;
  if (Reg == RISCV::VXRM)
    return 2;
  if (Reg == RISCV::VXSAT)
    return 1;
  if (isVReg(Reg))
    return vregRange(Reg).count * VLEN;
  assert(false && "unhandled register");
}

/*
 * RVV register file
 *
 * each of V0-V31 is an iVLEN alloca. elements are packed starting at the
 * least significant bit, and an LMUL>1 group continues into the next
 * higher-numbered register, so a group's contents are the concatenation
 * of its registers with the lowest-numbered one in the low bits. LLVM's
 * little-endian bitcasts between iN and <K x iM> put element 0 in the low
 * bits too, so the typed views below are plain bitcasts.
 *
 * vstart is not modeled: the psABI guarantees it is zero on entry and
 * after calls, compiled code does not write it, and every lifted vector
 * instruction assumes it is zero.
 */

bool riscv2llvm::isVReg(unsigned Reg) {
  if (Reg >= RISCV::V0 && Reg <= RISCV::V31)
    return true;
  for (MCRegister Sub : MRI->subregs(Reg))
    if (Sub.id() >= RISCV::V0 && Sub.id() <= RISCV::V31)
      return true;
  return false;
}

riscv2llvm::VRegRange riscv2llvm::vregRange(unsigned Reg) {
  if (Reg >= RISCV::V0 && Reg <= RISCV::V31)
    return {Reg - RISCV::V0, 1};
  // groups and tuples: find the architectural registers they cover,
  // rather than depending on the order of the generated enum
  unsigned lo = 32, hi = 0, n = 0;
  for (MCRegister Sub : MRI->subregs(Reg)) {
    unsigned id = Sub.id();
    if (id < RISCV::V0 || id > RISCV::V31)
      continue;
    unsigned idx = id - RISCV::V0;
    lo = std::min(lo, idx);
    hi = std::max(hi, idx);
    ++n;
  }
  assert(n > 0 && "not a vector register");
  assert(hi - lo + 1 == n && "vector register group is not contiguous");
  return {lo, n};
}

Value *riscv2llvm::lookupVReg(unsigned idx) {
  assert(idx < 32);
  return RegFile[RISCV::V0 + idx];
}

Value *riscv2llvm::lookupVCSR(unsigned Reg) {
  assert(Reg == RISCV::VL || Reg == RISCV::VTYPE || Reg == RISCV::VXRM ||
         Reg == RISCV::VXSAT);
  return RegFile[Reg];
}

// vlenb is a read-only design-time constant, so it has no storage
Value *riscv2llvm::getVLENB() {
  return getUnsignedIntConst(VLEN / 8, 64);
}

unsigned riscv2llvm::numVRegs(int EMULLog2) {
  return EMULLog2 > 0 ? 1u << EMULLog2 : 1u;
}

// the number of EEW-bit elements in a group with the given EMUL: this is
// VLMAX when EEW = SEW and EMUL = LMUL. it is zero when a fractional
// group is too small to hold even one element
unsigned riscv2llvm::numVElts(unsigned EEW, int EMULLog2) {
  unsigned bits = EMULLog2 >= 0 ? VLEN << EMULLog2 : VLEN >> -EMULLog2;
  return bits / EEW;
}

// reject the register-group shapes that the spec makes reserved or that
// RVA23U64 does not support, rather than modeling them
void riscv2llvm::checkVGroup(unsigned first, unsigned EEW, int EMULLog2) {
  auto fail = [&](const char *why) {
    *out << "\nERROR: unsupported vector register group: " << why << "\n\n";
    exit(-1);
  };
  if (EEW != 8 && EEW != 16 && EEW != 32 && EEW != 64)
    fail("element width must be 8, 16, 32, or 64 bits");
  if (EMULLog2 < -3 || EMULLog2 > 3)
    fail("EMUL must be between 1/8 and 8");
  if (first >= 32 || first % numVRegs(EMULLog2) != 0)
    fail("register group is misaligned");
  if (numVElts(EEW, EMULLog2) == 0)
    fail("fractional group holds no whole element");
}

// `count` consecutive registers as one vector of iEEW elements. registers
// are combined with shufflevectors rather than wide integer arithmetic,
// which is much easier on the solver
Value *riscv2llvm::readVRegs(unsigned first, unsigned count, unsigned EEW) {
  assert(count >= 1 && first + count <= 32);
  assert(VLEN % EEW == 0);
  auto perReg = getVecTy(EEW, VLEN / EEW);
  vector<Value *> parts;
  for (unsigned i = 0; i < count; ++i)
    parts.push_back(createBitCast(
        createLoad(getIntTy(VLEN), lookupVReg(first + i)), perReg));
  // count is a power of two, so this concatenates equal-sized halves
  while (parts.size() > 1) {
    vector<Value *> next;
    auto len = cast<FixedVectorType>(parts[0]->getType())->getNumElements();
    SmallVector<int> concat;
    for (unsigned j = 0; j < 2 * len; ++j)
      concat.push_back(j);
    for (unsigned i = 0; i < parts.size(); i += 2)
      next.push_back(createShuffleVector(parts[i], parts[i + 1], concat));
    parts = std::move(next);
  }
  return parts[0];
}

// the inverse of readVRegs(); `vec` is any vector of integers that is
// exactly `count` registers wide
void riscv2llvm::writeVRegs(unsigned first, unsigned count, Value *vec) {
  assert(count >= 1 && first + count <= 32);
  auto *vecTy = cast<FixedVectorType>(vec->getType());
  auto eltW = getBitWidth(vecTy->getElementType());
  unsigned len = vecTy->getNumElements();
  assert(eltW * len == count * VLEN);
  unsigned perReg = len / count;
  for (unsigned i = 0; i < count; ++i) {
    Value *slice = vec;
    if (count > 1) {
      SmallVector<int> idx;
      for (unsigned j = 0; j < perReg; ++j)
        idx.push_back(i * perReg + j);
      slice = createShuffleVector(vec, idx);
    }
    createStore(createBitCast(slice, getIntTy(VLEN)), lookupVReg(first + i));
  }
}

// a group as a vector of numVElts(EEW, EMUL) iEEW elements; a fractional
// group is the low elements of one register
Value *riscv2llvm::readVGroup(unsigned first, unsigned EEW, int EMULLog2) {
  checkVGroup(first, EEW, EMULLog2);
  auto n = numVElts(EEW, EMULLog2);
  auto regs = numVRegs(EMULLog2);
  auto vec = readVRegs(first, regs, EEW);
  if (n == regs * VLEN / EEW)
    return vec;
  SmallVector<int> low;
  for (unsigned i = 0; i < n; ++i)
    low.push_back(i);
  return createShuffleVector(vec, low);
}

// a mask is always held in one register, with bit i belonging to element i
Value *riscv2llvm::readVMask(unsigned idx, unsigned numElts) {
  assert(numElts >= 1 && numElts <= VLEN);
  Value *bits = createLoad(getIntTy(VLEN), lookupVReg(idx));
  bits = createTrunc(bits, getIntTy(numElts));
  return createBitCast(bits, getVecTy(1, numElts));
}

// an arbitrary, independently chosen boolean per element
Value *riscv2llvm::createUnknownBoolVec(unsigned numElts) {
  return createBitCast(createUnknownInt(numElts), getVecTy(1, numElts));
}

// lane i is a body element iff i < vl. vstart is assumed to be zero, so
// there are no prestart elements
Value *riscv2llvm::createBodyMask(Value *vl, unsigned numElts) {
  assert(vl->getType()->isIntegerTy(64));
  vector<Constant *> lanes;
  for (unsigned i = 0; i < numElts; ++i)
    lanes.push_back(getUnsignedIntConst(i, 64));
  auto laneIdx = ConstantVector::get(lanes);
  auto vecTy = getVecTy(64, numElts);
  Value *vlVec = createInsertElement(PoisonValue::get(vecTy), vl, 0);
  vlVec = createShuffleVector(vlVec, SmallVector<int>(numElts, 0));
  return createICmp(ICmpInst::ICMP_ULT, laneIdx, vlVec);
}

/*
 * write `result`, a vector of numVElts(EEW, EMUL) elements, to a group,
 * applying the RVV element policies:
 *
 * - active elements (body, and enabled in `mask` if there is one) get the
 *   new value
 * - inactive elements follow the mask policy, tail elements follow the
 *   tail policy; with a fractional EMUL, the rest of the register past
 *   VLMAX is also tail
 * - undisturbed elements keep their old value; agnostic elements either
 *   keep their old value or become all ones, chosen independently for each
 *   element. in the target this is a nondeterministic choice, so the
 *   lifted code has to be correct for every combination
 * - if vl = 0, nothing is written, not even agnostic tail elements
 *
 * `mask` is a vector of numVElts(EEW, EMUL) i1s, or null if unmasked.
 * `keep`, if not null, is another such vector: body elements where it is
 * set keep their old value whatever the mask and the policies say, the way
 * vslideup leaves the elements below its offset unchanged. `bodyLen`, if
 * not null, ends the body there instead of at vl, as vcompress needs; the
 * nothing-is-written rule still depends on vl
 */
void riscv2llvm::writeVGroup(unsigned first, unsigned EEW, int EMULLog2,
                             Value *result, Value *vl, Value *mask,
                             bool tailAgnostic, bool maskAgnostic,
                             Value *keep, Value *bodyLen) {
  checkVGroup(first, EEW, EMULLog2);
  auto n = numVElts(EEW, EMULLog2);
  auto regs = numVRegs(EMULLog2);
  auto total = regs * VLEN / EEW;
  auto vecTy = getVecTy(EEW, total);
  assert(result->getType() == getVecTy(EEW, n));
  assert(!mask || mask->getType() == getVecTy(1, n));
  assert(!keep || keep->getType() == getVecTy(1, n));

  auto old = readVRegs(first, regs, EEW);

  // widen to cover the whole register; the extra lanes are all past VLMAX,
  // and so past vl, which means they are never selected from these values
  if (total != n) {
    SmallVector<int> widen;
    for (unsigned i = 0; i < total; ++i)
      widen.push_back(i < n ? int(i) : PoisonMaskElem);
    result = createShuffleVector(result, widen);
    if (mask)
      mask = createShuffleVector(mask, widen);
    if (keep)
      keep = createShuffleVector(keep, widen);
  }

  auto inBody = createBodyMask(bodyLen ? bodyLen : vl, total);
  // select rather than and, so the poison lanes of a widened mask are
  // never looked at
  Value *active =
      mask ? createSelect(inBody, mask,
                          Constant::getNullValue(getVecTy(1, total)))
           : inBody;

  Value *agnostic = nullptr;
  if (tailAgnostic || (mask && maskAgnostic))
    agnostic = createSelect(createUnknownBoolVec(total), old,
                            getAllOnesConst(vecTy));
  Value *tailVal = tailAgnostic ? agnostic : old;
  Value *notActive = tailVal;
  if (mask) {
    Value *inactiveVal = maskAgnostic ? agnostic : old;
    notActive = createSelect(inBody, inactiveVal, tailVal);
  }
  Value *merged = createSelect(active, result, notActive);
  if (keep) {
    // select on inBody first, so the poison lanes of a widened `keep` are
    // never looked at
    auto kept = createSelect(inBody, keep,
                             Constant::getNullValue(getVecTy(1, total)));
    merged = createSelect(kept, old, merged);
  }

  auto vlIsZero =
      createICmp(ICmpInst::ICMP_EQ, vl, getUnsignedIntConst(0, 64));
  writeVRegs(first, regs, createSelect(vlIsZero, old, merged));
}

/*
 * write `result`, a vector of VLMAX i1s, to the mask register `idx`
 *
 * mask destinations are always tail agnostic, and the spec gives their
 * tail more freedom than other agnostic elements: it may also receive the
 * value the instruction would have computed with vl = VLMAX, and for some
 * instructions any bit of the register may be overwritten. rather than
 * model those cases one by one, a tail bit here is entirely arbitrary,
 * which covers all of them. inactive body bits follow the mask policy as
 * usual, and nothing is written when vl = 0
 */
void riscv2llvm::writeVMask(unsigned idx, Value *result, Value *vl,
                            Value *mask, bool maskAgnostic) {
  auto *resTy = cast<FixedVectorType>(result->getType());
  assert(resTy->getElementType()->isIntegerTy(1));
  unsigned n = resTy->getNumElements();
  assert(n >= 1 && n <= VLEN);
  assert(!mask || mask->getType() == resTy);

  auto oldBits = createLoad(getIntTy(VLEN), lookupVReg(idx));
  auto boolsTy = getVecTy(1, VLEN);
  auto old = createBitCast(oldBits, boolsTy);

  if (n != VLEN) {
    SmallVector<int> widen;
    for (unsigned i = 0; i < VLEN; ++i)
      widen.push_back(i < n ? int(i) : PoisonMaskElem);
    result = createShuffleVector(result, widen);
    if (mask)
      mask = createShuffleVector(mask, widen);
  }

  auto inBody = createBodyMask(vl, VLEN);
  Value *active =
      mask ? createSelect(inBody, mask, Constant::getNullValue(boolsTy))
           : inBody;

  Value *notActive = createUnknownBoolVec(VLEN);
  if (mask) {
    // a one-bit agnostic element either keeps its old value or becomes 1
    Value *inactiveVal =
        maskAgnostic ? createSelect(createUnknownBoolVec(VLEN), old,
                                    getAllOnesConst(boolsTy))
                     : static_cast<Value *>(old);
    notActive = createSelect(inBody, inactiveVal, notActive);
  }
  auto merged = createSelect(active, result, notActive);
  Value *newBits = createBitCast(merged, oldBits->getType());

  auto vlIsZero =
      createICmp(ICmpInst::ICMP_EQ, vl, getUnsignedIntConst(0, 64));
  createStore(createSelect(vlIsZero, oldBits, newBits), lookupVReg(idx));
}

void riscv2llvm::updateOutputReg(Value *V, bool SExt) {
  auto W = getBitWidth(V);
  auto outputReg = CurInst->getOperand(0).getReg();
  assert(!isVReg(outputReg) &&
         "vector results are written with writeVGroup or writeVMask");
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
  debug() << "entering marshallArgs()\n";
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
    debug() << "  " << getBitWidth(argTy) << "-bit arg at " << toString(loc)
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
        // The assembly's callee and the source call site can disagree about
        // the argument list: a call site whose signature differs from the
        // declaration, or debuginfo pointing at a different call than the one
        // the assembly makes. Either way there is no call-site argument here
        // to take the extension attributes from.
        if (argIdx >= llvmCI.arg_size()) {
          *out << "\nERROR: the source call site passes " << llvmCI.arg_size()
               << " argument(s) but the callee the assembly names takes "
               << fTy->getNumParams() << "\n\n";
          exit(-1);
        }
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
  debug() << "marshalled up " << args.size() << " arguments\n";
  return args;
}

void riscv2llvm::doIndirectCall() {
  debug() << "in doIndirectCall\n";

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
  debug() << "entering doCall()\n";
  assert(llvmCI);

  // Establish the ABI precondition before abstracting either a normal or a
  // tail call. Restoring SP later does not make a misaligned call valid.
  checkStackAlignment();

  auto *calleeTy = FC.getFunctionType();
  bool vectors = calleeTy->getReturnType()->isVectorTy();
  for (auto *arg : calleeTy->params())
    vectors |= arg->isVectorTy();
  if (vectors) {
    *out << "\nERROR: calls that pass or return vectors are not supported "
            "yet\n\n";
    exit(-1);
  }
  for (auto &arg : calleeTy->params()) {
    if (auto vTy = dyn_cast<VectorType>(arg))
      checkVectorTy(vTy);
  }
  if (auto RT = dyn_cast<VectorType>(calleeTy->getReturnType()))
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
    debug() << "adding constant Boolean as args[3]\n";
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
  // a call-site memory attribute (e.g. "call @f() readnone") is a fact
  // about this call, so it holds for the lifted call as well; without it
  // the target call may write memory the source call can't, which
  // Alive2's call-state matching doesn't handle
  auto memAttr = llvmCI->getAttributes().getFnAttr(Attribute::Memory);
  if (memAttr.isValid())
    CI->addFnAttr(memAttr);
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

  // nor does it preserve any of the vector state
  invalidateVectorState();

  if (returnsDestPtr) {
    updateReg(args[0], RISCV::X10);
    return;
  }

  auto retTy = FC.getFunctionType()->getReturnType();
  auto retLoc = CCAssigner(CCAssigner::Target::RISCV64, DL, retTy).retLoc();
  if (retTy->isIntegerTy() || retTy->isPointerTy()) {
    // checkSupport() vets the lifted function's own return type, but nothing
    // vets the ones it calls, so a callee returning a value too wide for the
    // registers arrives here.
    if (!canPlace(retLoc)) {
      *out << "\nERROR: Unsupported Function Return: a " << getBitWidth(retTy)
           << "-bit value would be returned at " << toString(retLoc)
           << " by a callee, which we don't support yet\n\n";
      exit(-1);
    }
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
  // the vector calling convention variant preserves v1-v7 and v24-v31
  if (usesVectorCC)
    for (unsigned i = 0; i < 32; ++i)
      if (isCalleeSavedVReg(i))
        assertSame(initialVReg[i], createLoad(getIntTy(VLEN), lookupVReg(i)));
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
  } else if (retLoc.kind == ArgLoc::VectorGroup) {
    createReturn(readVectorRet(cast<VectorType>(retTyp), retLoc));
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

/*
 * vectors at the ABI boundary; see backend_tv/abi.h. alive2 checks a
 * single vscale, at which a <vscale x N x T> has exactly N * vscale
 * elements, and VLEN is 64 * vscale, so such a value fills its register
 * group exactly -- or, for a fractional-LMUL type, the low part of one
 * register. a fixed-length vector is in the low bits of its group, which
 * was sized for VLEN = 128. scalable values are converted element by
 * element, because alive2 handles neither scalable allocas nor
 * llvm.vector.extract/insert. the indices are constants that may be past
 * the type's known minimum, which LLVM does not fold to poison for
 * scalable vectors
 */

// the elements of a vector argument or return value, as a fixed vector
Value *riscv2llvm::vectorElts(Value *v) {
  auto *ty = dyn_cast<ScalableVectorType>(v->getType());
  if (!ty)
    return v;
  unsigned n = ty->getMinNumElements() * util::config::vscale_value;
  Value *res = PoisonValue::get(FixedVectorType::get(ty->getElementType(), n));
  for (unsigned i = 0; i < n; ++i)
    res = createInsertElement(res, createExtractElement(v, i), i);
  return res;
}

// the inverse of vectorElts()
Value *riscv2llvm::vectorFromElts(Value *v, VectorType *ty) {
  auto *svTy = dyn_cast<ScalableVectorType>(ty);
  if (!svTy) {
    assert(v->getType() == ty);
    return v;
  }
  auto n = cast<FixedVectorType>(v->getType())->getNumElements();
  assert(n == svTy->getMinNumElements() * util::config::vscale_value);
  Value *res = PoisonValue::get(svTy);
  for (unsigned i = 0; i < n; ++i)
    res = createInsertElement(res, createExtractElement(v, i), i);
  return res;
}

// put an incoming argument where the caller would have: in the low bits of
// its register group, with any bits of the group above it left unknown. a
// mask is handled the same way, with EEW = 1: bit i of its register is
// element i
void riscv2llvm::writeVectorArg(Value *arg, const ArgLoc &loc) {
  auto elts = vectorElts(arg);
  // the register file holds integers; FP elements move as their bits
  if (elts->getType()->isFPOrFPVectorTy())
    elts = createBitCast(
        elts, VectorType::getInteger(cast<VectorType>(elts->getType())));
  auto *eltsTy = cast<FixedVectorType>(elts->getType());
  unsigned EEW = getBitWidth(eltsTy->getElementType());
  unsigned n = eltsTy->getNumElements();
  unsigned nregs = loc.limbs.size();
  unsigned first = loc.limbs[0].reg;
  unsigned full = nregs * VLEN / EEW;
  assert(n <= full);
  if (n < full) {
    // widen to the whole group, then fill the new lanes from `rest`
    SmallVector<int> widen;
    for (unsigned i = 0; i < full; ++i)
      widen.push_back(i < n ? int(i) : PoisonMaskElem);
    auto wide = createShuffleVector(elts, widen);
    auto rest =
        createBitCast(createUnknownInt(nregs * VLEN), getVecTy(EEW, full));
    SmallVector<int> idx;
    for (unsigned i = 0; i < full; ++i)
      idx.push_back(i < n ? int(i) : int(full + i));
    elts = createShuffleVector(wide, rest, idx);
  }
  writeVRegs(first, nregs, elts);
}

// the value a function returns in a register group, which is in its low
// bits; the rest of the group is ignored
Value *riscv2llvm::readVectorRet(VectorType *ty, const ArgLoc &loc) {
  unsigned EEW = getBitWidth(ty->getElementType());
  unsigned n = ty->getElementCount().getKnownMinValue();
  if (isa<ScalableVectorType>(ty))
    n *= util::config::vscale_value;
  auto vec = readVRegs(loc.limbs[0].reg, loc.limbs.size(), EEW);
  if (cast<FixedVectorType>(vec->getType())->getNumElements() != n) {
    SmallVector<int> low;
    for (unsigned i = 0; i < n; ++i)
      low.push_back(i);
    vec = createShuffleVector(vec, low);
  }
  if (ty->getElementType()->isFloatingPointTy())
    vec = createBitCast(vec, getVecTy(EEW, n, /*isFP=*/true));
  return vectorFromElts(vec, ty);
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

  debug() << "created scalar registers\n";

  // RVV. VLEN is fixed for this run at 64 * vscale, matching LLVM's
  // mapping of scalable types onto RVV. RVA23U64 includes Zvl128b, and the
  // architecture caps VLEN at 2^16
  VLEN = 64 * util::config::vscale_value;
  if (VLEN < 128 || VLEN > 65536) {
    *out << "\nERROR: RVA23U64 needs 128 <= VLEN <= 65536, that is, a vscale "
            "between 2 and 1024, but vscale is "
         << util::config::vscale_value << "\n\n";
    exit(-1);
  }

  // the vector registers; the psABI makes them all caller-saved, so their
  // values on entry are arbitrary
  for (unsigned i = 0; i < 32; ++i)
    createRegStorage(RISCV::V0 + i, VLEN, "V" + to_string(i));

  // ...except under the vector calling convention variant. The psABI
  // says any function passing vectors in vector registers follows the
  // variant, but LLVM applies it only to riscv_vector_cc functions: a C
  // calling convention function takes vector arguments in v8-v23 yet
  // preserves no vector registers (see test_vector_std in LLVM's
  // test/CodeGen/RISCV/rvv/callee-saved-regs.ll). We follow LLVM.
  usesVectorCC = srcFn->getCallingConv() == CallingConv::RISCV_VectorCall;
  if (usesVectorCC)
    for (unsigned i = 0; i < 32; ++i)
      if (isCalleeSavedVReg(i))
        initialVReg[i] = createLoad(getIntTy(VLEN), lookupVReg(i));

  // vl, vtype, vxrm, and vxsat are unspecified on entry. vlenb is a
  // constant (see getVLENB()) and vstart is not modeled
  createRegStorage(RISCV::VL, 64, "VL");
  createRegStorage(RISCV::VTYPE, 64, "VTYPE");
  createRegStorage(RISCV::VXRM, 2, "VXRM");
  createRegStorage(RISCV::VXSAT, 1, "VXSAT");

  vScratch = createAlloca(getIntTy(64), getUnsignedIntConst(1, 64), "vscratch");

  computeVTypeDataflow();

  debug() << "created vector registers, VLEN = " << VLEN << "\n";

  auto paramBase =
      createGEP(i8ty, stackMem, {getUnsignedIntConst(stackBytes, 64)}, "");
  createStore(paramBase, RegFile[RISCV::X2]);
  initialSP = readFromReg(RISCV::X2, i64ty);

  // initializing to zero makes loads from XZR work; stores are
  // handled in updateReg()
  createStore(getUnsignedIntConst(0, 64), RegFile[RISCV::X0]);

  debug() << "about to do callee-side ABI stuff\n";

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
    if (loc.kind == ArgLoc::VectorGroup) {
      debug() << "  processing vector arg at " << toString(loc) << "\n";
      writeVectorArg(&*arg, loc);
      continue;
    }
    debug() << "  processing " << getBitWidth(arg) << "-bit arg at "
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
    debug() << "\n";
  }

  debug() << "done with callee-side ABI stuff\n";
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

// vectors can be passed and returned in RVV register groups (see
// backend_tv/abi.h). RVA23U64 has vector f32 and f64 arithmetic (V) and
// conversions to and from f16 (Zvfhmin), but no vector bfloat
static void checkVectorElt(VectorType *ty, std::ostream *out) {
  auto *elt = ty->getElementType();
  if (!elt->isIntegerTy() && !elt->isHalfTy() && !elt->isFloatTy() &&
      !elt->isDoubleTy()) {
    *out << "\nERROR: only vectors of integers, half, float, and double are "
            "supported\n\n";
    exit(-1);
  }
}

void riscv2llvm::checkArgSupport(Argument &arg) {
  if (auto *vTy = dyn_cast<VectorType>(arg.getType()))
    checkVectorElt(vTy, out);
}

void riscv2llvm::checkTypeSupport(Type *ty) {
  if (auto *vTy = dyn_cast<VectorType>(ty)) {
    checkVectorElt(vTy, out);
    return;
  }
  // RVA23U64 gives us Zfhmin (so f16 arithmetic is promoted to f32) but
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
  // riscv_vector_cc differs from C only in making v1-v7 and v24-v31
  // callee-saved. For the lifted function, doReturn() checks that; for a
  // callee, doCall() clobbering all vector state is a sound
  // over-approximation
  if (fn->getCallingConv() != CallingConv::C &&
      fn->getCallingConv() != CallingConv::RISCV_VectorCall) {
    *out << "\nERROR: Only the C and riscv_vector_cc calling conventions "
            "are supported\n\n";
    exit(-1);
  }
}
