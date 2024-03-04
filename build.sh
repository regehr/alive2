#!/bin/bash

set -e -o pipefail


export CXXFLAGS
export CFLAGS
cd "$(dirname "$0")"

if ! [[ -f antlr-jar ]]; then
  nix build 'nixpkgs#antlr.src' -o antlr-jar
fi
if ! [[ -d antlr-dev ]]; then
  nix build 'nixpkgs#antlr.runtime.cpp^dev' -o antlr
fi
if ! [[ -d llvm-dev ]]; then
  nix build 'github:katrinafyi/pac-nix#llvm-custom-git.libllvm^dev' -o llvm
fi

cmake -B build -DBUILD_TV=1 \
  -DCMAKE_PREFIX_PATH=$(realpath antlr-dev)';'$(realpath llvm-dev) \
  -DANTLR4_JAR_LOCATION=$(realpath antlr-jar) \
  "$@"
  # -DLLVM_DIR=~/progs/llvm-regehr/llvm/build/lib/cmake/llvm/ \
  # -DFETCHCONTENT_SOURCE_DIR_ASLP-CPP=~/progs/aslp \
cmake --build build -j10 -t backend-tv
