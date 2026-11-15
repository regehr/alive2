#!/bin/sh
# Run the AArch64 lit tests. Works from any directory.
#   ./arm-tests.sh              all of tests/arm-tv
#   ./arm-tests.sh vectors      just tests/arm-tv/vectors
#   ./arm-tests.sh -v vectors   extra lit flags go before the subdirectory
set -e
repo=$(cd "$(dirname "$0")" && pwd)
dir=""
flags=""
for arg in "$@"; do
  case "$arg" in
    -*) flags="$flags $arg" ;;
    *) dir="$arg" ;;
  esac
done
cd "$repo/build"
exec uv run --no-project --python 3.12 --with psutil \
  python ../tests/lit/lit.py -s --timeout=60 $flags "../tests/arm-tv/$dir"
