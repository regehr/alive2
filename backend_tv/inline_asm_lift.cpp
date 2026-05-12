#include "backend_tv/inline_asm_lift.h"
#include "backend_tv/lifter.h"

#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/TargetParser/Triple.h"
#include "llvm/Transforms/Utils/Cloning.h"

#include <iostream>
#include <sstream>
#include <vector>

using namespace std;
using namespace llvm;

namespace lifter {

// Max bit-width of a flattened multi-output struct return. mc2llvm's
// checkSupport rejects scalar returns wider than 64 bits.
static constexpr unsigned kMaxFlattenedReturnBits = 64;

// All scalar wrapper-name construction goes through this so the wrapper
// and the matching stub stay in sync.
static std::string asmWrapName(size_t idx) {
  return "__asm_wrap_" + std::to_string(idx);
}

static bool isStructReturn(Type *ty) {
  return ty->isStructTy();
}

// Multi-output asm round-trips only when every field is a plain integer
// and the packed width fits in `kMaxFlattenedReturnBits`. Other shapes
// (pointer/float/vector/nested aggregate fields, arrays) need work that
// we don't do yet
static bool canFlattenStructReturn(StructType *st, const DataLayout &DL,
                                   std::ostream *out) {
  unsigned totalBits = 0;
  for (unsigned i = 0; i < st->getNumElements(); ++i) {
    Type *fieldTy = st->getElementType(i);
    if (!fieldTy->isIntegerTy()) {
      *out << "ERROR: multi-output inline asm with non-integer field "
              "type at index "
           << i << " not supported\n";
      return false;
    }
    totalBits += DL.getTypeSizeInBits(fieldTy);
  }
  if (totalBits == 0 || totalBits > kMaxFlattenedReturnBits) {
    *out << "ERROR: multi-output inline asm with " << totalBits
         << "-bit packed return not supported (max "
         << kMaxFlattenedReturnBits << ")\n";
    return false;
  }
  return true;
}

static Type *getFlattenedReturnType(StructType *st, LLVMContext &Ctx,
                                    const DataLayout &DL) {
  unsigned totalBits = 0;
  for (unsigned i = 0; i < st->getNumElements(); ++i) {
    totalBits += DL.getTypeSizeInBits(st->getElementType(i));
  }
  return Type::getIntNTy(Ctx, totalBits);
}

static void copyTargetAttrs(const Function &from, Function &to) {
  for (StringRef attrName : {"target-features", "target-cpu", "tune-cpu"}) {
    if (from.hasFnAttribute(attrName))
      to.addFnAttr(attrName,
                   from.getFnAttribute(attrName).getValueAsString());
  }
}

// One-call wrapper around `CB`. Struct returns get packed into a wide int.
static Function *buildAsmWrapper(CallBase &CB, Module &wrapMod, size_t idx) {
  auto *iasm = cast<InlineAsm>(CB.getCalledOperand());
  auto &Ctx = wrapMod.getContext();
  auto &DL = wrapMod.getDataLayout();

  vector<Type *> argTypes;
  for (unsigned i = 0; i < CB.arg_size(); ++i)
    argTypes.push_back(CB.getArgOperand(i)->getType());

  Type *origRetTy = CB.getType();
  Type *wrapRetTy = origRetTy;

  bool flattenStruct = isStructReturn(origRetTy);
  StructType *structTy = nullptr;
  if (flattenStruct) {
    structTy = cast<StructType>(origRetTy);
    wrapRetTy = getFlattenedReturnType(structTy, Ctx, DL);
  }

  FunctionType *wrapFTy = FunctionType::get(wrapRetTy, argTypes, false);

  Function *wrapFn = Function::Create(wrapFTy, GlobalValue::ExternalLinkage,
                                      asmWrapName(idx), &wrapMod);
  if (const Function *origFn = CB.getFunction())
    copyTargetAttrs(*origFn, *wrapFn);

  BasicBlock *BB = BasicBlock::Create(Ctx, "entry", wrapFn);
  IRBuilder<> Builder(BB);

  vector<Value *> args;
  for (auto &arg : wrapFn->args())
    args.push_back(&arg);

  InlineAsm *newAsm = InlineAsm::get(
      cast<FunctionType>(iasm->getFunctionType()), iasm->getAsmString(),
      iasm->getConstraintString(), iasm->hasSideEffects(),
      iasm->isAlignStack(), iasm->getDialect());

  CallInst *asmCall = Builder.CreateCall(newAsm, args);
  // preserve call-site attrs (e.g. elementtype for indirect-mem constraints)
  asmCall->setAttributes(CB.getAttributes());

  Value *result = asmCall;

  if (origRetTy->isVoidTy()) {
    Builder.CreateRetVoid();
  } else if (flattenStruct) {
    Value *packed = ConstantInt::get(wrapRetTy, 0);
    unsigned offset = 0;
    for (unsigned i = 0; i < structTy->getNumElements(); ++i) {
      Value *field = Builder.CreateExtractValue(result, {i});
      unsigned fieldBits = DL.getTypeSizeInBits(structTy->getElementType(i));
      Value *ext = Builder.CreateZExt(field, wrapRetTy);
      if (offset > 0)
        ext = Builder.CreateShl(ext, offset);
      packed = Builder.CreateOr(packed, ext);
      offset += fieldBits;
    }
    Builder.CreateRet(packed);
  } else {
    Builder.CreateRet(result);
  }

  return wrapFn;
}

int liftInlineAsmCalls(Function &F, const InlineAsmLiftOptions &opts) {
  assert(opts.out && "InlineAsmLiftOptions::out must be non-null");

  vector<CallBase *> asmCalls;
  const DataLayout &moduleDL = F.getParent()->getDataLayout();

  for (auto &BB : F) {
    for (auto &I : BB) {
      auto *CB = dyn_cast<CallBase>(&I);
      if (!CB || !isa<InlineAsm>(CB->getCalledOperand()))
        continue;
      if (isa<CallBrInst>(CB)) {
        *opts.out << "ERROR: callbr (asm-goto) not supported for "
                     "inline asm lifting\n";
        return -1;
      }
      if (CB->hasOperandBundles()) {
        *opts.out << "ERROR: inline asm with operand bundles not "
                     "supported\n";
        return -1;
      }
      Type *retTy = CB->getType();
      if (retTy->isArrayTy()) {
        *opts.out << "ERROR: inline asm with array return type not "
                     "supported\n";
        return -1;
      }
      if (auto *st = dyn_cast<StructType>(retTy)) {
        if (!canFlattenStructReturn(st, moduleDL, opts.out))
          return -1;
      }
      asmCalls.push_back(CB);
    }
  }

  if (asmCalls.empty())
    return 0;

  *opts.out << "Found " << asmCalls.size()
            << " inline asm call(s) to lift\n";

  auto &tctx = opts.tctx;
  int lifted = 0;

  for (size_t idx = 0; idx < asmCalls.size(); ++idx) {
    CallBase *CB = asmCalls[idx];

    *opts.out << "\n--- Lifting inline asm site " << idx << " ---\n";

    LLVMContext &Ctx = F.getContext();

    auto wrapMod = make_unique<Module>("AsmWrapModule", Ctx);
    wrapMod->setTargetTriple(tctx.TT);
    wrapMod->setDataLayout(tctx.DL);

    Function *wrapFn = buildAsmWrapper(*CB, *wrapMod, idx);

    string verifyErr;
    raw_string_ostream verifyOS(verifyErr);
    if (verifyFunction(*wrapFn, &verifyOS)) {
      *opts.out << "ERROR: wrapper function verification failed: "
                << verifyErr << "\n";
      return -1;
    }

    *opts.out << "Wrapper module:\n" << moduleToString(wrapMod.get()) << "\n";

    auto asmBuffer =
        generateAsm(*wrapMod, tctx.Targ, tctx.TT, tctx.CPU, tctx.Features);

    *opts.out << "Generated assembly:\n"
              << asmBuffer->getBuffer().str() << "\n";

    auto stubMod = make_unique<Module>("StubModule", Ctx);
    stubMod->setTargetTriple(tctx.TT);
    stubMod->setDataLayout(tctx.DL);

    // Use the wrapper's function type (which has flattened struct returns)
    // and the same symbol name so mc2llvm matches the asm function.
    FunctionType *fTy = wrapFn->getFunctionType();
    Function *stubFn = Function::Create(fTy, GlobalValue::ExternalLinkage,
                                        asmWrapName(idx), stubMod.get());

    BasicBlock *stubBB = BasicBlock::Create(Ctx, "entry", stubFn);
    Type *retTy = fTy->getReturnType();
    if (retTy->isVoidTy())
      ReturnInst::Create(Ctx, stubBB);
    else
      ReturnInst::Create(Ctx, UndefValue::get(retTy), stubBB);

    unordered_map<unsigned, Instruction *> lineMap;
    addDebugInfo(stubFn, lineMap);

    auto [adjSrc, liftedTgt] =
        liftFunc(stubFn, std::move(asmBuffer), lineMap, "O1", opts.out,
                 tctx.Targ, tctx.TT, tctx.CPU, tctx.Features);

    *opts.out << "Lifted function:\n"
              << moduleToString(liftedTgt->getParent()) << "\n";

    Module *parentMod = F.getParent();

    Function *importedFn = Function::Create(
        liftedTgt->getFunctionType(), GlobalValue::InternalLinkage,
        liftedTgt->getName().str() + "_imported", parentMod);

    ValueToValueMapTy VMap;
    for (auto srcArg = liftedTgt->arg_begin(),
              dstArg = importedFn->arg_begin();
         srcArg != liftedTgt->arg_end(); ++srcArg, ++dstArg) {
      VMap[&*srcArg] = &*dstArg;
      dstArg->setName(srcArg->getName());
    }

    SmallVector<ReturnInst *, 4> Returns;
    CloneFunctionInto(importedFn, liftedTgt, VMap,
                      CloneFunctionChangeType::DifferentModule, Returns);

    IRBuilder<> Builder(CB);
    Builder.SetCurrentDebugLocation(CB->getDebugLoc());

    vector<Value *> callArgs;
    for (unsigned i = 0; i < CB->arg_size(); ++i)
      callArgs.push_back(CB->getArgOperand(i));

    CallInst *newCI = Builder.CreateCall(importedFn, callArgs);
    Value *newResult = newCI;

    Type *origRetTy = CB->getType();
    if (!origRetTy->isVoidTy()) {
      if (isStructReturn(origRetTy)) {
        auto *structTy = cast<StructType>(origRetTy);
        Value *packed = newCI;
        Value *unpacked = UndefValue::get(origRetTy);
        unsigned offset = 0;
        for (unsigned i = 0; i < structTy->getNumElements(); ++i) {
          Type *elemTy = structTy->getElementType(i);
          unsigned fieldBits = moduleDL.getTypeSizeInBits(elemTy);
          Value *shifted = packed;
          if (offset > 0)
            shifted = Builder.CreateLShr(packed, offset);
          Value *trunced = Builder.CreateTrunc(shifted, elemTy);
          unpacked = Builder.CreateInsertValue(unpacked, trunced, {i});
          offset += fieldBits;
        }
        newResult = unpacked;
      }
      CB->replaceAllUsesWith(newResult);
    }
    CB->eraseFromParent();

    InlineFunctionInfo IFI;
    InlineResult inlineRes = InlineFunction(*newCI, IFI);
    if (!inlineRes.isSuccess()) {
      *opts.out << "ERROR: failed to inline lifted asm body: "
                << inlineRes.getFailureReason() << "\n";
      return -1;
    }
    if (importedFn->use_empty())
      importedFn->eraseFromParent();

    ++lifted;
    *opts.out << "Successfully lifted inline asm site " << idx << "\n";
  }

  return lifted;
}

static bool hasInlineAsm(Function &F) {
  for (auto &BB : F)
    for (auto &I : BB)
      if (auto *CB = dyn_cast<CallBase>(&I))
        if (isa<InlineAsm>(CB->getCalledOperand()))
          return true;
  return false;
}

bool tryLiftInlineAsm(Function &F, std::ostream *out) {
  if (!hasInlineAsm(F))
    return true;

  static bool targetInited = false;
  static TargetCtx tctx;
  if (!targetInited) {
    auto *M = F.getParent();
    Triple TT(M->getTargetTriple());
    if (TT.getArch() == Triple::UnknownArch) {
      *out << "WARNING: module has no target triple; defaulting to "
              "aarch64-unknown-linux-gnu for inline asm lifting. Set "
              "`target triple` in the IR to silence this and ensure "
              "correct codegen.\n";
      TT = Triple("aarch64-unknown-linux-gnu");
    }
    const auto &dlStr = M->getDataLayoutStr();
    tctx = initTargetFromTriple(
        TT, dlStr.empty() ? nullptr : dlStr.c_str(), out);
    targetInited = true;
  }

  InlineAsmLiftOptions opts{tctx, out};
  int result = liftInlineAsmCalls(F, opts);
  if (result < 0) {
    *out << "ERROR: failed to lift inline asm in @" << F.getName().str()
         << "\n";
    return false;
  }
  *out << "Lifted " << result << " inline asm call(s) in @"
       << F.getName().str() << "\n";
  return true;
}

} // namespace lifter
