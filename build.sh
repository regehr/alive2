#!/bin/bash

set -e -o pipefail


export CXXFLAGS
export CFLAGS
cd "$(dirname "$0")"

mkdir -p build && cd build
if ! [[ -f antlr-jar ]]; then
  nix build 'nixpkgs/655a58a72a6601292512670343087c2d75d859c1#antlr.src' -o antlr-jar
fi
if ! [[ -d antlr-dev ]]; then
  nix build 'nixpkgs/655a58a72a6601292512670343087c2d75d859c1#antlr.runtime.cpp^dev' -o antlr
fi
if [[ -n "$LLVM_ROOT" ]]; then
  ln -s "$LLVM_ROOT" ./llvm-dev
elif ! [[ -d llvm-dev ]]; then
  nix build 'github:katrinafyi/pac-nix/ac6bd13f242a782e1c69da6cdab2fe1322871bd3#llvm-custom-git.libllvm^dev' -o llvm
fi

if [[ -d aslp ]]; then :
elif command -v aslp-server &>/dev/null; then
  mkdir -p aslp/bin; ln -s $(which aslp-server) ./aslp/bin
else
  nix build 'github:katrinafyi/pac-nix#aslp-server' -o aslp
fi

if ! [[ -d varnish ]]; then
  nix build 'nixpkgs/655a58a72a6601292512670343087c2d75d859c1#varnish' -o varnish
fi

cd ..

#if command -v clang++ &>/dev/null && [[ -z "$CXX" ]]; then
#  export CXX=$(which clang++)
#fi

if [ ! -z "$LOCAL_LLVM" ]; then
  cmake -B build -DBUILD_TV=1 \
    -DCMAKE_PREFIX_PATH="$(realpath build/antlr-dev)" \
    -DANTLR4_JAR_LOCATION="$(realpath build/antlr-jar)" \
    -DLLVM_DIR=$LOCAL_LLVM/lib/cmake/llvm/ \
    -G Ninja \
    -DCMAKE_BUILD_TYPE=Release \
    "$@"
  cmake --build build
else
  # -DCMAKE_BUILD_TYPE=Release \
  cmake -B build -DBUILD_TV=1 \
    -DCMAKE_PREFIX_PATH="$(realpath build/antlr-dev);$(realpath build/llvm-dev)" \
    -DANTLR4_JAR_LOCATION="$(realpath build/antlr-jar)" \
    "$@"
    # -DLLVM_DIR=~/progs/llvm-regehr/build/lib/cmake/llvm/ \
    # -DFETCHCONTENT_SOURCE_DIR_ASLP-CPP=~/progs/aslp \
    # -DCMAKE_VERBOSE_MAKEFILE=TRUE \
  cmake --build build -j12
fi

# HACK: when building with LLVM from Nix, the alivecc/alive++ will be non-functional,
# due to an incorrect clang path.
# simply remove it to prevent test failure.
if ! build/alivecc --version; then
  echo 'build.sh: removing incorrectly-built alivecc/alive++...' >&2
  rm -f build/alivecc build/alive++
fi
