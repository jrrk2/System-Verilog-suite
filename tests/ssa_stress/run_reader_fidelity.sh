#!/usr/bin/env bash
# Sweep: yosys RTLIL ↔ yosys Verilog reader-fidelity miter.
# Same seed-driven random_sv_gen ssa_stress as run_equiv.sh, but
# uses test_yosys_rtlil_vs_verilog which compares the two yosys
# output paths via their respective readers (Rtlil_to_behavioral
# vs Verible_to_behavioral).  Disagreement = reader bug, not
# random-gen/source-SV problem.
set -u
cd "$(dirname "${BASH_SOURCE[0]}")/../.."
eval "$(opam env --switch=5.3.0)"
GEN=./_build/default/random_sv_gen.exe
MITER=./_build/default/test_yosys_rtlil_vs_verilog.exe
[ -x "$GEN"   ] || dune build ./random_sv_gen.exe                  >/dev/null
[ -x "$MITER" ] || dune build ./test_yosys_rtlil_vs_verilog.exe    >/dev/null

SEED=1; N=50
while [ $# -gt 0 ]; do
  case "$1" in
    --seed) SEED=$2; shift 2;;
    --n)    N=$2;    shift 2;;
    *) echo "unknown arg: $1"; exit 2;;
  esac
done

OUT=/tmp/reader_fidelity
WORK=$OUT/work
FOUND=$OUT/found
mkdir -p "$WORK" "$FOUND"
rm -f "$FOUND/INDEX"

pass=0; fail=0; err=0
for ((s = SEED; s < SEED + N; s++)); do
  NAME="rand_ssa_stress_$s"
  SV="$WORK/$NAME.sv"
  $GEN --features ssa_stress --emit-only --seed $s > "$SV"
  PAT=$(head -1 "$SV" | grep -oE "ssa_stress/[a-z_]+" | sed 's|ssa_stress/||')
  LOG="$WORK/$NAME.log"

  if $MITER "$NAME" "$SV" >"$LOG" 2>&1; then
    if grep -q "FORMALLY EQUIVALENT" "$LOG"; then pass=$((pass+1)); continue; fi
  fi
  fail=$((fail+1))
  cp "$SV"  "$FOUND/${NAME}.sv"
  cp "$LOG" "$FOUND/${NAME}.log"
  last=$(tail -30 "$LOG" | grep -E "NOT EQUIV|differ|Fatal|Error|reader divergence" | head -1)
  [ -z "$last" ] && last="(see ${NAME}.log)"
  printf "seed=%d %-12s FAIL  %s\n" $s "$PAT" "$last" >> "$FOUND/INDEX"
  printf "  ❌ seed=%d %s — %s\n" $s "$PAT" "$last"
done

echo
echo "── $pass passed / $fail failed / $err gen-error / $N total ──"
[ -s "$FOUND/INDEX" ] && echo "    saved to $FOUND/"
[ "$fail" = 0 ] && [ "$err" = 0 ]
