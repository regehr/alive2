#!/bin/bash

set -e -o pipefail

cd "$(dirname "$0")"
cmake -B build -DBUILD_TV=1 -DCMAKE_PREFIX_PATH=./antlr-dev/. -DLLVM_DIR=~/progs/llvm-regehr/llvm/build/lib/cmake//llvm/ -DANTLR4_JAR_LOCATION=$(cat antlr-jar) -DFETCHCONTENT_SOURCE_DIR_ASLP-CPP=~/progs/aslp 
cmake --build build -j10 -t backend-tv
