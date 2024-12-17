#!/bin/bash

pwd="$(pwd)"
cd $(dirname "$0")
out="$(pwd)/out"
./build.sh

mkdir -p "$out"

if [[ "$#" != 1 ]]; then
  echo "$0: requires one argument, .aarch64.ll test file" >&2
  exit 1
fi

arg="$1"
x=$(basename $arg)
extension="${x##*.}"
filename="${x%.*}"
shift
aslp=$out/$filename.aslp.$extension 
classic=$out/$filename.classic.$extension 
diff=$out/$filename.$extension.diff

ASLP=1 ASLP_DEBUG=0 build/backend-tv "$arg" "$@" >$aslp 2>&1
aslp_status=$?
ASLP=0 ASLP_DEBUG=0 build/backend-tv "$arg" "$@" >$classic 2>&1
classic_status=$?

diff -u --color=auto $classic $aslp
diff -u $classic $aslp >$diff

printf '%s\n' '' \
  "finished! backend-tv '$arg'" \
  "  aslp (status $aslp_status): $aslp" \
  "  classic (status $classic_status): $classic" \
  "  diff: $diff" \
  >&2
