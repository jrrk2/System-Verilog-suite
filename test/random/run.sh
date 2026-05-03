#!/bin/bash
# Wrapper for random_sv_gen — runs N constrained-random cases through
# the Verilator↔Verible miter and reports a summary of failure
# categories. Default is a quick 50-case sweep; pass an integer for a
# bigger run.
#
# Usage:
#   ./test/random/run.sh           # 50 cases starting at seed 1
#   ./test/random/run.sh 500       # 500 cases starting at seed 1
#   ./test/random/run.sh 200 1000  # 200 cases starting at seed 1000

set -e
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
n=${1:-50}
seed=${2:-1}
out=${OUT:-/tmp/random_sv}

# Make sure both the generator and the miter are built.
( cd "$repo" && dune build random_sv_gen.exe test_verilator_vs_verible.exe ) \
    > /dev/null 2>&1

rm -rf "$out"
"$repo/_build/default/random_sv_gen.exe" --seed "$seed" --n "$n" --out "$out" \
  | grep -v "^  ❌"  # suppress per-failure noise; summary is at the end

if [ -s "$out/found/INDEX" ]; then
    echo
    echo "=== failure categories ==="
    sort "$out/found/INDEX" | sed 's/seed=[0-9]* //' | sort | uniq -c | sort -rn
    echo
    echo "Failing seeds:"
    grep -oE "seed=[0-9]+" "$out/found/INDEX" | tr '\n' ' '
    echo
    echo
    echo "Stashed cases: $out/found/"
fi
