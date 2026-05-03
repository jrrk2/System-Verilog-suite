#!/bin/bash
# Run our three runners across the chipsalliance/sv-tests corpus and
# report a one-line per-runner pass-rate. Builds the OCaml exes first
# if they're missing.
#
# Usage:
#   bash test/sv_tests/run.sh                 # default: just decompiler_miter
#   bash test/sv_tests/run.sh ALL             # all 3 decompiler runners
#   bash test/sv_tests/run.sh chapter         # one chapter for triage
#   SV_TESTS_DIR=~/sv-tests bash …            # override clone location

set -e
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
sv_tests=${SV_TESTS_DIR:-$HOME/sv-tests}

mode=${1:-miter}    # miter | all | chapter | <chapter-N>

if [ ! -d "$sv_tests" ]; then
    echo "==> sv-tests not found at $sv_tests; running install.sh"
    bash "$here/install.sh"
fi

# Build the exes our runners depend on.
echo "==> building decompiler exes"
( cd "$repo" && eval "$(opam env --switch=5.3.0 --set-switch 2>/dev/null)" \
    && dune build test_verilator_vs_verible.exe test_verible_to_bir.exe \
                  test_verilator_behavioral.exe ) > /dev/null

case "$mode" in
    miter)   runners="Decompiler_Miter" ;;
    all)     runners="Decompiler_Verible_Parse Decompiler_Verilator_Parse Decompiler_Miter" ;;
    chapter) runners="Decompiler_Miter"
             tests_dir="$sv_tests/tests/chapter-5" ;;
    chapter-*) runners="Decompiler_Miter"
               tests_dir="$sv_tests/tests/$mode" ;;
    *)       runners="$mode" ;;   # treat $1 as a runner name
esac

cd "$sv_tests"

# sv-tests' Makefile takes the runners as RUNNERS=… and reports per-test
# logs under out/logs/<runner>/. We pin job count to nproc/2 so the OCaml
# exes don't drown the box (each can spawn verilator + z3).
nproc=$(( $(nproc) / 2 ))
[ $nproc -lt 1 ] && nproc=1
echo "==> sweep (runners=$runners, jobs=$nproc)"
OUT_DIR=$PWD/out CONF_DIR=$PWD/conf TESTS_DIR=$PWD/tests \
RUNNERS_DIR=$PWD/tools/runners THIRD_PARTY_DIR=$PWD/third_party \
make -j"$nproc" tests RUNNERS="$runners" \
    ${tests_dir:+TESTS="$tests_dir"} \
    > /tmp/sv_tests_run.log 2>&1 || true

echo
echo "==== per-runner pass-rate (synthesisable only) ===="
for r in $runners; do
    all=$(find "out/logs/$r" -name '*.log' 2>/dev/null | wc -l)
    # Skipped = empty log file (get_mode returned None for non-synth tests).
    skipped=$(find "out/logs/$r" -name '*.log' -size 0 2>/dev/null | wc -l)
    in_scope=$(( all - skipped ))
    pass=$(find "out/logs/$r" -name '*.log' ! -size 0 2>/dev/null \
           | xargs grep -l '^rc: 0' 2>/dev/null | wc -l)
    if [ "$in_scope" -gt 0 ]; then
        pct=$(( pass * 100 / in_scope ))
    else
        pct=0
    fi
    printf "  %-32s  %4d / %4d  (%d%%)  [skipped %d non-synth]\n" \
           "$r" "$pass" "$in_scope" "$pct" "$skipped"
done

echo
echo "Logs under: $sv_tests/out/logs/<runner>/"
echo "Build dashboard with: cd $sv_tests && make report"
