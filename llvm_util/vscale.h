#pragma once

// Copyright (c) 2018-present The Alive2 Authors.
// Distributed under the MIT license that can be found in the LICENSE file.

#include "llvm/IR/Function.h"

#include <optional>
#include <vector>

namespace llvm_util {

/// True if anything reachable from F makes its semantics depend on vscale:
/// a scalable type, a vscale_range attribute, or llvm.vscale.
bool referencesVScale(const llvm::Function &F);

/// True if F's vscale_range attribute, if it has one, admits scale.
bool allowsVScale(const llvm::Function &F, unsigned scale);

/// Enumerate the type solver's power-of-two assignments up to max_vscale,
/// sorted in ascending order. No solver objects survive this call, so callers
/// can reset the SMT context while verifying each concrete assignment.
/// Returns nullopt if enumeration fails.
std::optional<std::vector<unsigned>> getVScales(unsigned max_vscale);

}
