#!/bin/bash

pwd="$(pwd)"
cd $(dirname "$0")
./build.sh

mkdir -p out

arg="$1"
x=$(basename $arg)
extension="${x##*.}"
filename="${x%.*}"
shift
ASLP=1 build/backend-tv "$pwd/$arg" "$@" > out/$filename.aslp.$extension 2>&1
ASLP=0 build/backend-tv "$pwd/$arg" "$@" > out/$filename.classic.$extension 2>&1
