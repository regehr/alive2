#!/bin/bash

set -e -o pipefail


export CXXFLAGS
export CFLAGS
cd "$(dirname "$0")"

mkdir -p build && cd build
if ! [[ -f antlr-jar ]]; then
  nix build 'nixpkgs#antlr.src' -o antlr-jar
fi
if ! [[ -d antlr-dev ]]; then
  nix build 'nixpkgs#antlr.runtime.cpp^dev' -o antlr
fi
if ! [[ -d llvm-dev ]]; then
  nix build 'github:katrinafyi/pac-nix#llvm-custom-git.libllvm^dev' -o llvm
fi
if ! ( [[ -d aslp ]] || command -v aslp-server &>/dev/null ); then
  nix build 'github:katrinafyi/pac-nix#aslp' -o aslp
fi
cd ..

  # -DCMAKE_BUILD_TYPE=Release \
cmake -B build -DBUILD_TV=1 \
  -DCMAKE_PREFIX_PATH="$(pwd)/build/antlr-dev;$(pwd)/build/llvm-dev" \
  -DANTLR4_JAR_LOCATION="$(pwd)/build/antlr-jar" \
  "$@"
  # -DLLVM_DIR=~/progs/llvm-regehr/llvm/build/lib/cmake/llvm/ \
  # -DFETCHCONTENT_SOURCE_DIR_ASLP-CPP=~/progs/aslp \
cmake --build build -j10 -t backend-tv
