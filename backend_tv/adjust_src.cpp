#include "backend_tv/lifter.h"

#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

#include <iostream>

using namespace std;
using namespace llvm;
using namespace lifter;

long totalAllocas;

namespace {

void checkCallingConv(Function *fn) {
  if (fn->getCallingConv() != CallingConv::C &&
      fn->getCallingConv() != CallingConv::Fast) {
    *out
        << "\nERROR: Only the C and fast calling conventions are supported\n\n";
    exit(-1);
  }
}

void avoidArgMD(CallInst *ci, const string &str) {
  auto &Ctx = ci->getContext();
  auto val = MetadataAsValue::get(Ctx, MDString::get(Ctx, str));
  for (auto &arg : ci->args()) {
    if (arg.get() == val) {
      *out << "\nERROR: " << val->getNameOrAsOperand() << " not supported\n\n";
      exit(-1);
    }
  }
}

void checkSupportHelper(Instruction &i, const DataLayout &DL,
                        set<Type *> &typeSet) {
  typeSet.insert(i.getType());
  for (auto &op : i.operands()) {
    auto *ty = op.get()->getType();
    typeSet.insert(ty);
    if (auto *vty = dyn_cast<VectorType>(ty)) {
      typeSet.insert(vty->getElementType());
    }
    if (auto *pty = dyn_cast<PointerType>(ty)) {
      if (pty->getAddressSpace() != 0) {
        *out << "\nERROR: address spaces other than 0 are unsupported\n\n";
        exit(-1);
      }
    }
  }
  if (i.isVolatile()) {
    *out << "\nERROR: volatiles not supported\n\n";
    exit(-1);
  }
  if (i.isAtomic()) {
    *out << "\nERROR: atomics not supported yet\n\n";
    exit(-1);
  }
  if (isa<VAArgInst>(&i)) {
    *out << "\nERROR: va_arg instructions not supported\n\n";
    exit(-1);
  }
  if (isa<InvokeInst>(&i)) {
    *out << "\nERROR: invoke instructions not supported\n\n";
    exit(-1);
  }
  if (auto *ai = dyn_cast<AllocaInst>(&i)) {
    if (!ai->isStaticAlloca()) {
      *out << "\nERROR: only static allocas supported for now\n\n";
      exit(-1);
    }
    auto allocSize = ai->getAllocationSize(DL);
    if (allocSize)
      totalAllocas += allocSize->getFixedValue();
  }
  if (auto *cb = dyn_cast<CallBase>(&i)) {
    if (isa<InlineAsm>(cb->getCalledOperand())) {
      *out << "\nERROR: inline assembly not supported\n\n";
      exit(-1);
    }
  }
  if (auto *ci = dyn_cast<CallInst>(&i)) {

    // non-null as a callsite attribute is problematic for us and we
    // don't think it happens much, just get rid of it
    ci->removeRetAttr(Attribute::NonNull);
    ci->removeRetAttr(Attribute::NoAlias);

    if (auto callee = ci->getCalledFunction()) {
      checkCallingConv(callee);

      if (callee->isIntrinsic()) {
        const auto &name = callee->getName();
        if (callee->isConstrainedFPIntrinsic() ||
            name.contains("llvm.fptrunc.round")) {
          // FIXME we should be able to support these, except round.dynamic
          avoidArgMD(ci, "round.dynamic");
          avoidArgMD(ci, "round.downward");
          avoidArgMD(ci, "round.upward");
          avoidArgMD(ci, "round.towardzero");
          avoidArgMD(ci, "round.tonearestaway");
        }
      } else {
        for (auto arg = callee->arg_begin(); arg != callee->arg_end(); ++arg) {

          if (auto *vTy = dyn_cast<VectorType>(arg->getType()))
            checkVectorTy(vTy);

          if (arg->hasByValAttr()) {
            *out << "\nERROR: we don't support the byval parameter attribute "
                    "yet\n\n";
            exit(-1);
          }
        }
      }

      if (callee->isVarArg()) {
        *out << "\nERROR: varargs not supported\n\n";
        exit(-1);
      }

      auto name = (string)callee->getName();
      if (name.find("llvm.memcpy.element.unordered.atomic") != string::npos) {
        *out << "\nERROR: atomic instrinsics not supported\n\n";
        exit(-1);
      }

      if (name.find("llvm.objc") != string::npos) {
        *out << "\nERROR: llvm.objc instrinsics not supported\n\n";
        exit(-1);
      }

      if (name.find("llvm.thread") != string::npos) {
        *out << "\nERROR: llvm.thread instrinsics not supported\n\n";
        exit(-1);
      }

      if ((name.find("llvm.experimental.gc") != string::npos) ||
          (name.find("llvm.experimental.stackmap") != string::npos)) {
        *out << "\nERROR: llvm GC instrinsics not supported\n\n";
        exit(-1);
      }
    } else {
      // indirect call or signature mismatch
      auto co = ci->getCalledOperand();
      assert(co);
      if (auto cf = dyn_cast<Function>(co)) {
        if (cf->isVarArg()) {
          *out << "\nERROR: varargs not supported\n\n";
          exit(-1);
        }
      } else {
        // FIXME -- do we need to handle this case?
      }
    }
  }
}

} // namespace

namespace lifter {

void checkVectorTy(VectorType *Ty) {
  auto *EltTy = Ty->getElementType();
  int Width = -1;
  if (auto *IntTy = dyn_cast<IntegerType>(EltTy)) {
    Width = IntTy->getBitWidth();
    if (Width != 8 && Width != 16 && Width != 32 && Width != 64)
      goto vec_error;
  } else if (EltTy->isFloatTy()) {
    Width = 32;
  } else if (EltTy->isDoubleTy()) {
    Width = 64;
  } else {
    *out << "\nERROR: Only vectors of integer, f32, f64, supported for now\n\n";
    exit(-1);
  }
  {
    auto Count = Ty->getElementCount().getFixedValue();
    auto VecSize = (Count * Width) / 8;
    if (VecSize != 8 && VecSize != 16)
      goto vec_error;
    return;
  }
vec_error:
  *out << "\nERROR: Only short vectors 8 and 16 bytes long are supported, "
          "in parameters and return values; please see Section 5.4 of "
          "AAPCS64 for more details\n\n";
  exit(-1);
}

std::unique_ptr<DIBuilder> DBuilder;
std::unordered_map<unsigned, llvm::Instruction *> lineMap;

void addDebugInfo(Function *srcFn) {
  auto &M = *srcFn->getParent();

  // start with a clean slate
  StripDebugInfo(M);

  M.setModuleFlag(Module::Warning, "Dwarf Version", dwarf::DWARF_VERSION);
  M.setModuleFlag(Module::Warning, "Debug Info Version",
                  DEBUG_METADATA_VERSION);

  auto &Ctx = srcFn->getContext();

  DBuilder = std::make_unique<DIBuilder>(M);
  auto DIF = DBuilder->createFile("foo.ll", ".");
  auto CU = DBuilder->createCompileUnit(dwarf::DW_LANG_C, DIF, "arm-tv", false,
                                        "", 0);
  auto Ty = DBuilder->createSubroutineType(DBuilder->getOrCreateTypeArray({}));
  auto SP = DBuilder->createFunction(CU, srcFn->getName(), StringRef(), DIF, 0,
                                     Ty, 0, DINode::FlagPrototyped,
                                     DISubprogram::SPFlagDefinition);
  srcFn->setSubprogram(SP);
  unsigned line = 0;
  for (auto &bb : *srcFn) {
    for (auto &i : bb) {
      lineMap[line] = &i;
      i.setDebugLoc(DILocation::get(Ctx, line, 0, SP));
      ++line;
    }
  }

  if (false) {
    M.dump();
    *out << "\n\n\n";
  }

  DBuilder->finalize();
  verifyModule(M);
  *out << "\n\n\n";
}

void checkSupport(Function *srcFn) {
  checkCallingConv(srcFn);
  if (srcFn->getLinkage() ==
      GlobalValue::LinkageTypes::AvailableExternallyLinkage) {
    *out << "\nERROR: function has externally_available linkage type and won't "
            "be codegenned\n\n";
    exit(-1);
  }

  if (srcFn->hasPersonalityFn()) {
    *out << "\nERROR: personality functions not supported\n\n";
    exit(-1);
  }

  for (auto &arg : srcFn->args()) {
    if (arg.hasByValAttr()) {
      *out << "\nERROR: we don't support the byval parameter attribute yet\n\n";
      exit(-1);
    }
    auto *ty = arg.getType();
    if (ty->isStructTy()) {
      *out << "\nERROR: we don't support structures in arguments yet\n\n";
      exit(-1);
    }
    if (ty->isArrayTy()) {
      *out << "\nERROR: we don't support arrays in arguments yet\n\n";
      exit(-1);
    }
    auto &DL = srcFn->getParent()->getDataLayout();
    auto orig_width = DL.getTypeSizeInBits(ty);
    if (auto vTy = dyn_cast<VectorType>(ty)) {
      checkVectorTy(vTy);
      if (orig_width > 128) {
        *out << "\nERROR: Vector arguments >128 bits not supported\n\n";
        exit(-1);
      }
    } else {
      if (orig_width > 64) {
        *out << "\nERROR: Unsupported function argument: Only integer / "
                "pointer parameters 64 bits or smaller supported for now\n\n";
        exit(-1);
      }
    }
  }

  if (auto RT = dyn_cast<VectorType>(srcFn->getReturnType()))
    checkVectorTy(RT);

  if (srcFn->getReturnType()->isStructTy()) {
    *out << "\nERROR: we don't support structures in return values yet\n\n";
    exit(-1);
  }
  if (srcFn->getReturnType()->isArrayTy()) {
    *out << "\nERROR: we don't support arrays in return values yet\n\n";
    exit(-1);
  }
  set<Type *> typeSet;
  auto &DL = srcFn->getParent()->getDataLayout();
  unsigned llvmInstCount = 0;

  totalAllocas = 0;
  for (auto &bb : *srcFn) {
    for (auto &i : bb) {
      checkSupportHelper(i, DL, typeSet);
      ++llvmInstCount;
    }
  }
  if (totalAllocas >= stackBytes) {
    *out << "ERROR: ARM stack frame too large, consider increasing "
            "stackBytes\n\n";
    exit(-1);
  }

  for (auto ty : typeSet) {
    if (ty->isFloatingPointTy()) {
      if (!(ty->isFloatTy() || ty->isDoubleTy())) {
        *out << "\nERROR: only float and double supported (not  bfloat, half, "
                "fp128, etc.)\n\n";
        exit(-1);
      }
    }
  }

  *out << llvmInstCount << " LLVM instructions in source function\n";
}

void nameGlobals(Module *M) {
  int num = 0;
  for (auto G = M->global_begin(); G != M->global_end(); ++G) {
    if (G->hasName())
      continue;
    G->setName("g" + std::to_string(num++));
  }
  num = 0;
  for (auto &F : *M) {
    if (F.hasName())
      continue;
    F.setName("f" + std::to_string(num++));
  }
}

/*
 * a function that have the sext or zext attribute on its return value
 * is awkward: this obligates the function to sign- or zero-extend the
 * return value. we want to check this, which requires making the
 * function return a wider type, which requires cloning the function.
 *
 * cloning a function is an awkward case and we'd like to thoroughly
 * test that sort of code, so we just unconditionally do that even
 * when we don't actually need to.
 */
Function *adjustSrc(Function *srcFn) {
  auto *origRetTy = srcFn->getReturnType();

  if (!(origRetTy->isIntegerTy() || origRetTy->isVectorTy() ||
        origRetTy->isPointerTy() || origRetTy->isVoidTy() ||
        origRetTy->isFloatingPointTy())) {
    *out << "\nERROR: Unsupported Function Return Type: Only int, ptr, vec, "
            "float, and void supported for now\n\n";
    exit(-1);
  }

  auto &DL = srcFn->getParent()->getDataLayout();
  origRetWidth = origRetTy->isVoidTy() ? 0 : DL.getTypeSizeInBits(origRetTy);

  if (origRetTy->isVectorTy()) {
    if (origRetWidth > 128) {
      *out << "\nERROR: Unsupported Function Return: vector > 128 bits\n\n";
      exit(-1);
    }
  } else {
    if (origRetWidth > 64) {
      *out << "\nERROR: Scalar return values larger than 64 bits are not "
              "supported\n\n";
      exit(-1);
    }
  }

  has_ret_attr = srcFn->hasRetAttribute(Attribute::SExt) ||
                 srcFn->hasRetAttribute(Attribute::ZExt);

  Type *actualRetTy = nullptr;
  if (has_ret_attr && origRetWidth != 32 && origRetWidth != 64) {
    auto *i32 = Type::getIntNTy(srcFn->getContext(), 32);
    auto *i64 = Type::getIntNTy(srcFn->getContext(), 64);

    // build this first to avoid iterator invalidation
    vector<ReturnInst *> RIs;
    for (auto &BB : *srcFn)
      for (auto &I : BB)
        if (auto *RI = dyn_cast<ReturnInst>(&I))
          RIs.push_back(RI);

    for (auto RI : RIs) {
      BasicBlock::iterator it{RI};
      auto retVal = RI->getReturnValue();
      auto Name = retVal->getName();
      if (origRetTy->isVectorTy()) {
        retVal = new BitCastInst(
            retVal, Type::getIntNTy(srcFn->getContext(), origRetWidth),
            Name + "_bitcast", it);
      }
      if (srcFn->hasRetAttribute(Attribute::ZExt)) {
        retVal = new ZExtInst(retVal, i64, Name + "_zext", it);
      } else {
        if (origRetWidth < 32) {
          auto sext = new SExtInst(retVal, i32, Name + "_sext", it);
          retVal = new ZExtInst(sext, i64, Name + "_zext", it);
        } else {
          retVal = new SExtInst(retVal, i64, Name + "_sext", it);
        }
      }
      ReturnInst::Create(srcFn->getContext(), retVal, it);
      RI->eraseFromParent();
    }
    actualRetTy = i64;
  } else {
    actualRetTy = origRetTy;
  }

  FunctionType *NFTy =
      FunctionType::get(actualRetTy, srcFn->getFunctionType()->params(), false);
  Function *NF =
      Function::Create(NFTy, srcFn->getLinkage(), srcFn->getAddressSpace(),
                       srcFn->getName(), srcFn->getParent());

  // TODO -- it's probably not ok to copy all flags this way, we need
  // to be picky and think hard about this
  NF->copyAttributesFrom(srcFn);

  NF->splice(NF->begin(), srcFn);
  NF->takeName(srcFn);
  srcFn->replaceAllUsesWith(NF);

  for (Function::arg_iterator I = srcFn->arg_begin(), E = srcFn->arg_end(),
                              I2 = NF->arg_begin();
       I != E; ++I, ++I2) {
    I->replaceAllUsesWith(&*I2);
  }

  srcFn->eraseFromParent();
  return NF;
}

void fixupOptimizedTgt(llvm::Function *tgt) {
  /*
   * these attributes can be soundly removed, and a good thing too
   * since they cause spurious TV failures in ASM memory mode
   */
  for (auto arg = tgt->arg_begin(); arg != tgt->arg_end(); ++arg) {
    arg->removeAttr(llvm::Attribute::Captures);
    arg->removeAttr(llvm::Attribute::ReadNone);
    arg->removeAttr(llvm::Attribute::ReadOnly);
    arg->removeAttr(llvm::Attribute::WriteOnly);
  }

  /*
   * when we originally generated the target function, we allocated
   * its stack memory using a custom allocation function; this is to
   * keep LLVM from making unwarranted assumptions about that memory
   * and optimizing it in undesirable ways. however, Alive doesn't
   * want to see the custom allocator. so, here, before passing target
   * to Alive, we replace it with a regular old alloc
   */
  Instruction *myAllocCall{nullptr};
  for (auto &bb : *tgt) {
    for (auto &i : bb) {
      if (auto *ci = dyn_cast<CallInst>(&i)) {
        if (auto callee = ci->getCalledFunction()) {
          if (callee == myAlloc) {
            IRBuilder<> B(&i);
            auto *i8Ty = Type::getInt8Ty(ci->getContext());
            auto *alloca = B.CreateAlloca(i8Ty, 0, stackSize, "stack");
            alloca->setAlignment(Align(16));
            i.replaceAllUsesWith(alloca);
            assert(myAllocCall == nullptr);
            myAllocCall = &i;
          }
        }
      }
    }
  }
  if (myAllocCall)
    myAllocCall->eraseFromParent();
}

/**
 * Given an intToPtr instruction, checks for a round trip 
 * from a ptrToInt instruction, then replaces with a single GEP instruction.
 * This translation can be verified with physical pointers.
 */
bool tryReplaceRoundTrip(llvm::IntToPtrInst *intToPtr) {
  assert(intToPtr);

  // we only understand add instructions in this context
  auto *op_inst = dyn_cast<llvm::Instruction>(intToPtr->getOperand(0));
  if (!op_inst || op_inst->getOpcode() != llvm::Instruction::Add || op_inst->getNumUses() > 1)
      return false;

  // keep track of (ptr + int) vs (int + ptr)
  bool ptrOnLeft = true;
  auto *ptrToInt = dyn_cast<PtrToIntInst>(op_inst->getOperand(0));

  if (!ptrToInt) {
      ptrOnLeft = false;
      ptrToInt = dyn_cast<PtrToIntInst>(op_inst->getOperand(1));
  }

  if (!ptrToInt || ptrToInt->getNumUses() > 1)
      return false;

  llvm::IRBuilder<> B(intToPtr);

  llvm::Value *gep = B.CreateGEP(B.getInt8Ty(),
                                 ptrToInt->getOperand(0),
                                 {op_inst->getOperand(ptrOnLeft ? 1 : 0)},
                                 "");
  
  intToPtr->replaceAllUsesWith(gep);
  intToPtr->eraseFromParent();
  op_inst->eraseFromParent();
  ptrToInt->eraseFromParent();

  return true;
}

} // namespace lifter
