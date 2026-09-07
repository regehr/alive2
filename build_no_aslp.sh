#!/bin/bash

# Builds alive-tv, backend-tv, and friends without ASLP.
#
# ASLP supplies formally-derived ARM semantics that backend-tv can use as an
# alternative to our own AArch64 semantics. It costs us a dependency on ANTLR,
# on aslp-cpp (fetched from GitHub at configure time), and on a running
# aslp-server; it does nothing at all for RISC-V. This script cuts all of that
# out, so the build needs no network access and no Nix -- just LLVM and Z3.
#
# Point this at an LLVM build or install tree with either:
#   LLVM_ROOT=/path/to/llvm  ./build_no_aslp.sh
#   LOCAL_LLVM=/path/to/llvm ./build_no_aslp.sh   # same thing, matches build.sh
# If neither is set we use llvm-config from your PATH, or failing that, Nix to
# fetch the same LLVM that build.sh uses. Extra arguments are passed through to
# cmake, which builds Release unless you ask for something else.

set -e -o pipefail

export CXXFLAGS
export CFLAGS
cd "$(dirname "$0")"

BUILD_DIR="${BUILD_DIR:-build}"

# accept either spelling; LLVM_ROOT wins so it can override a stale LOCAL_LLVM
# inherited from a shell profile
LLVM="${LLVM_ROOT:-$LOCAL_LLVM}"

if [[ -n "$LLVM" ]]; then
  if ! [[ -d "$LLVM/lib/cmake/llvm" ]]; then
    echo "$0: no lib/cmake/llvm under '$LLVM' -- is that an LLVM build or install tree?" >&2
    exit 1
  fi
  LLVM_CMAKE_DIR="$(cd "$LLVM/lib/cmake/llvm" && pwd)"
elif [[ -d "$BUILD_DIR/llvm-dev" ]]; then
  LLVM_CMAKE_DIR="$(cd "$BUILD_DIR/llvm-dev/lib/cmake/llvm" && pwd)"
elif command -v llvm-config &>/dev/null; then
  LLVM_CMAKE_DIR="$(llvm-config --cmakedir)"
elif command -v nix &>/dev/null; then
  mkdir -p "$BUILD_DIR"
  ( cd "$BUILD_DIR" && nix build 'github:katrinafyi/pac-nix#llvm-custom-git.libllvm^dev' -o llvm-dev )
  LLVM_CMAKE_DIR="$(cd "$BUILD_DIR/llvm-dev/lib/cmake/llvm" && pwd)"
else
  echo "$0: don't know where to find LLVM." >&2
  echo "  set LLVM_ROOT (or LOCAL_LLVM) to an LLVM build or install tree, or put" >&2
  echo "  llvm-config on your PATH, or install Nix." >&2
  exit 1
fi

echo "$0: using LLVM from $LLVM_CMAKE_DIR" >&2

if command -v clang++ &>/dev/null && [[ -z "$CXX" ]]; then
  export CXX=$(which clang++)
fi

cmake -G Ninja -B "$BUILD_DIR" -DBUILD_TV=1 \
  -DENABLE_ASLP=OFF \
  -DLLVM_DIR="$LLVM_CMAKE_DIR" \
  "$@"

cmake --build "$BUILD_DIR" -j"$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)"

# HACK: inherited from build.sh -- when building against an LLVM whose clang
# lives elsewhere, alivecc/alive++ come out non-functional. Remove them rather
# than let them fail the test suite.
if ! "$BUILD_DIR"/alivecc --version >/dev/null 2>&1; then
  echo "$0: removing incorrectly-built alivecc/alive++..." >&2
  rm -f "$BUILD_DIR"/alivecc "$BUILD_DIR"/alive++
fi
