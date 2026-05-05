#!/usr/bin/env bash
# Smoke regressions for the gate-level Liberty-expansion Z3 miter.
#
# Each line is a (top, behavioral, gate, expected) tuple. The driver
# (test_gate_z3_miter.exe) parses both sides into BIR, expands Liberty
# cell instances on the gate side, and runs Z3 to prove formal
# equivalence. Liberty defaults to simcells.lib in hardcaml-lua.
#
# The counter case (yosys bit-blasts a 4-bit bus FF into four 1-bit
# FFs) used to be expected-fail; #74 added the FF-pack pass
# (Behavioral_ffpack) which collapses bit-FFs back into a bus FF
# before the miter's ffrip runs. Now expected to pass.

set -u
cd "$(dirname "$0")/../.."

EXE=_build/default/test_gate_z3_miter.exe
[ -x "$EXE" ] || { echo "build $EXE first (dune build)"; exit 2; }

D=test/gate_miter
pass=0 fail=0 expected_fail=0
failures=()

run() {
  local top=$1 beh=$2 gate=$3 want=$4
  local out
  out=$("$EXE" "$top" "$D/$beh" "$D/$gate" 2>&1)
  if echo "$out" | grep -q "FORMALLY EQUIVALENT"; then
    if [ "$want" = "ok" ]; then
      printf '  ✅ %-30s\n' "$top"
      pass=$((pass+1))
    else
      printf '  ⚠  %-30s (passed but expected fail)\n' "$top"
      fail=$((fail+1))
    fi
  else
    if [ "$want" = "fail" ]; then
      printf '  ⚠  %-30s (expected fail, still fails)\n' "$top"
      expected_fail=$((expected_fail+1))
    else
      printf '  ❌ %-30s\n' "$top"
      failures+=("$top")
      fail=$((fail+1))
    fi
  fi
}

echo "Gate-level miter regressions (simcells.lib)"
run and2       and2_beh.sv      and2_gate.sv      ok
run full_adder full_adder.sv    full_adder_gate.v ok
run and8       and8.sv          and8_gate.v       ok
run dff1       dff1.sv          dff1_gate.v       ok
run counter    counter.sv       counter_gate.v    ok

echo "─────"
echo "  pass=$pass  fail=$fail  expected-fail=$expected_fail"
if [ ${#failures[@]} -gt 0 ]; then
  echo "  unexpected failures: ${failures[*]}"
  exit 1
fi
