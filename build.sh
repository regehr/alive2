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
if ! [[ -d llvm-dev ]]; then
  nix build 'github:katrinafyi/pac-nix/llvm-for-alive2#llvm-custom-git.libllvm^dev' -o llvm
fi
if ! ( [[ -d aslp ]] || command -v aslp-server &>/dev/null ); then
  nix build 'github:katrinafyi/pac-nix#aslp' -o aslp
fi
cd ..

if command -v clang++ &>/dev/null && [[ -z "$CXX" ]]; then
  export CXX=$(which clang++)
fi

  # -DCMAKE_BUILD_TYPE=Release \
cmake -B build -DBUILD_TV=1 \
  -DCMAKE_PREFIX_PATH="$(realpath build/antlr-dev);$(realpath build/llvm-dev)" \
  -DANTLR4_JAR_LOCATION="$(realpath build/antlr-jar)" \
  "$@"
  # -DLLVM_DIR=~/progs/llvm-regehr/build/lib/cmake/llvm/ \
  # -DFETCHCONTENT_SOURCE_DIR_ASLP-CPP=~/progs/aslp \
cmake --build build -j12
