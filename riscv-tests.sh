#!/bin/sh
# Run the RISC-V lit tests. Works from any directory.
#   ./riscv-tests.sh            all of tests/riscv-tv
#   ./riscv-tests.sh loads      just tests/riscv-tv/loads
#   ./riscv-tests.sh -v loads   extra lit flags go before the subdirectory
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
  python ../tests/lit/lit.py -s --timeout=60 $flags "../tests/riscv-tv/$dir"
