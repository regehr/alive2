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

cmake -B build -DBUILD_TV=1 -DCMAKE_PREFIX_PATH=$(realpath antlr-dev) -DLLVM_DIR=~/progs/llvm-regehr/llvm/build/lib/cmake//llvm/ -DANTLR4_JAR_LOCATION=$(realpath antlr-jar) -DFETCHCONTENT_SOURCE_DIR_ASLP-CPP=~/progs/aslp "$@"
cmake --build build -j10 -t backend-tv
