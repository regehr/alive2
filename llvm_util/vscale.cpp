// Copyright (c) 2018-present The Alive2 Authors.
// Distributed under the MIT license that can be found in the LICENSE file.

#include "llvm_util/vscale.h"
#include "ir/type.h"
#include "tools/transform.h"

#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Operator.h"
#include <algorithm>

using namespace std;

namespace {

bool referencesVScale(const llvm::AttributeList &attrs) {
  for (auto set : attrs) {
    for (auto attr : set) {
      if (attr.hasAttribute(llvm::Attribute::VScaleRange) ||
          (attr.isTypeAttribute() && attr.getValueAsType()->isScalableTy()))
        return true;
    }
  }
  return false;
}

}

namespace llvm_util {

bool referencesVScale(const llvm::Function &F) {
  vector<const llvm::Value *> worklist { &F };
  for (auto &BB : F)
    for (auto &I : BB)
      worklist.push_back(&I);

  llvm::SmallPtrSet<const llvm::Value *, 32> visited;
  while (!worklist.empty()) {
    auto V = worklist.back();
    worklist.pop_back();
    if (!visited.insert(V).second)
      continue;
    if (V->getType()->isScalableTy())
      return true;

    if (auto fn = llvm::dyn_cast<llvm::Function>(V)) {
      if (fn->getIntrinsicID() == llvm::Intrinsic::vscale ||
          ::referencesVScale(fn->getAttributes()))
        return true;
      for (auto ty : fn->getFunctionType()->subtypes())
        if (ty->isScalableTy())
          return true;
      // Calls are not inlined by the validator.
      continue;
    }
    if (auto gep = llvm::dyn_cast<llvm::GEPOperator>(V)) {
      if (gep->getSourceElementType()->isScalableTy())
        return true;
    }
    if (auto alloc = llvm::dyn_cast<llvm::AllocaInst>(V)) {
      if (alloc->getAllocatedType()->isScalableTy())
        return true;
    }
    if (auto call = llvm::dyn_cast<llvm::CallBase>(V)) {
      if (::referencesVScale(call->getAttributes()))
        return true;
    }
    if (auto user = llvm::dyn_cast<llvm::User>(V))
      for (auto &op : user->operands())
        worklist.push_back(op.get());
  }
  return false;
}

bool allowsVScale(const llvm::Function &F, unsigned scale) {
  auto attr = F.getFnAttribute(llvm::Attribute::VScaleRange);
  if (!attr.isValid())
    return true;
  auto max = attr.getVScaleRangeMax();
  return scale >= attr.getVScaleRangeMin() && (!max || scale <= *max);
}

optional<vector<unsigned>> getVScales(unsigned max_vscale) {
  auto vscale = IR::Type::vscale(max_vscale);
  tools::TypingAssignments types(vscale.isPowerOf2() && vscale.ule(max_vscale));
  vector<unsigned> scales;
  for (; types; ++types)
    scales.push_back(types.getUInt(vscale));
  if (types.hasError())
    return {};
  sort(scales.begin(), scales.end());
  return scales;
}

}
