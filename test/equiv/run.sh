#!/bin/bash
# Self-test for the equivalence workbench (equiv_core.ml + gui_equiv.ml),
# driven through the headless verb `sv_suite equiv`.
#
# Every case here is one the GUI puts in front of a user, so each is checked
# by its VERDICT AND its exit code — an INCONCLUSIVE that exits 0 is the way a
# checker starts lying, and case 3 is the deliberate control that shows the
# register-matching step is doing the work rather than the miter getting lucky.
#
#   1  self-miter                      EQUIVALENT (0)
#   2  renamed registers, sim matching EQUIVALENT (0)
#   3  renamed registers, NO matching  DIFFER (1) + "NO register matched"   [control]
#   4  injected bug, per-cone          DIFFER (1), localised to ovf_q__D
#   5  counterexample explanation      first divergence, reproduced in simulation
#   6  manual overrides from a project EQUIVALENT (0) with matching OFF
#   7  hierarchical (bottom-up)        EQUIVALENT (0)
#   8  GUI window construction         builds headless under xvfb (if available)
#   9  tool scan                       reports what each frontend needs and found
#  10  missing tool                    named early, with the fix, exit 2 (not a crash)
#  11  tool selection                  --tool persists and the scan then uses it
#  12  a DISCOVERED tool really runs   verible vs the first external SV frontend
#                                      found, under a wall-clock bound
#
# Exit 0 iff every case passes.

set -u
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
cd "$repo"

eval "$(opam env --switch=5.3.0)" 2>/dev/null || true
SV=./_build/default/sv_suite.exe
[ -x "$SV" ] || dune build ./_build/default/sv_suite.exe || exit 2

A=test/equiv/acc_a.sv
BR=test/equiv/acc_b_renamed.sv
BB=test/equiv/acc_b_bug.sv
pass=0; fail=0
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

check () { # name expected_exit pattern… ; reads the run's output from $tmp/out
  local name=$1 want=$2 got=$3; shift 3
  local ok=1
  [ "$got" = "$want" ] || { ok=0; echo "    exit $got, expected $want"; }
  for pat in "$@"; do
    grep -qF -- "$pat" "$tmp/out" || { ok=0; echo "    missing: $pat"; }
  done
  if [ $ok = 1 ]; then echo "  PASS  $name"; pass=$((pass+1))
  else echo "  FAIL  $name"; fail=$((fail+1)); sed 's/^/        /' "$tmp/out" | tail -25; fi
}

echo "== equivalence workbench self-test =="

$SV equiv --a verible,acc,$A --b verible,acc,$A >"$tmp/out" 2>&1; got=$?
check "1 self-miter" 0 $got "VERDICT: EQUIVALENT" "registers matched  : 2"

$SV equiv --a verible,acc,$A --b verible,acc,$BR >"$tmp/out" 2>&1; got=$?
check "2 renamed registers, simulation matching" 0 $got \
  "VERDICT: EQUIVALENT" "2 by simulation"

$SV equiv --a verible,acc,$A --b verible,acc,$BR --no-sim >"$tmp/out" 2>&1; got=$?
check "3 renamed registers, matching OFF (control)" 1 $got \
  "VERDICT: DIFFER" "NO register matched"

$SV equiv --a verible,acc,$A --b verible,acc,$BB --mode per-cone --scan \
  >"$tmp/out" 2>&1; got=$?
check "4 injected bug, per-cone localisation" 1 $got \
  "VERDICT: DIFFER" "Differing cones (1): ovf_q__D"

$SV equiv --a verible,acc,$A --b verible,acc,$BB --explain ovf_q__D \
  >"$tmp/out" 2>&1; got=$?
check "5 counterexample on the failing cone" 1 $got \
  "First divergence" "ovf_q__D" "reproduced in simulation"

$SV equiv --a verible,acc,$A --b verible,acc,$BR \
  --save-project "$tmp/p.json" >"$tmp/out" 2>&1
python3 - "$tmp/p.json" <<'PY'
import json, sys
p = sys.argv[1]
j = json.load(open(p))
# the names a manual override must use are the RIPPED names (`--list-regs`),
# not the RTL's: prep_for_z3 rewrites `_reg` suffixes and bit-splits scalars.
j["overrides"] = [{"a": "acc_q", "b": "n42_reg"}, {"a": "ovf_q", "b": "n43__b0"}]
json.dump(j, open(p, "w"), indent=2)
PY
$SV equiv "$tmp/p.json" --no-sim --list-regs >"$tmp/out" 2>&1; got=$?
check "6 manual overrides from a project file" 0 $got \
  "VERDICT: EQUIVALENT" "2 manual"

$SV equiv --a verible,dff_top,test/hier/dff_hier.sv \
          --b verible,dff_top,test/hier/dff_flat.sv --mode hier \
  >"$tmp/out" 2>&1; got=$?
check "7 hierarchical (bottom-up) miter" 0 $got \
  "VERDICT: EQUIVALENT (hierarchical, assume-guarantee)" "Per-module verdicts"

if command -v xvfb-run >/dev/null 2>&1; then
  dune build ./_build/default/sv_gui.exe >/dev/null 2>&1
  # =tools also constructs the external-tool picker
  xvfb-run -a env SV_GUI_EQUIV=tools SV_GUI_EXIT_AFTER_LOAD=1 \
    ./_build/default/sv_gui.exe >"$tmp/out" 2>&1; got=$?
  # GTK criticals mean a widget was built wrong even when the process exits 0
  if [ $got = 0 ] && ! grep -qE "Gtk-(CRITICAL|WARNING)|Gtk:ERROR" "$tmp/out"; then
    echo "  PASS  8 workbench window constructs (xvfb)"; pass=$((pass+1))
  else
    echo "  FAIL  8 workbench window constructs (xvfb) — exit $got"; fail=$((fail+1))
    sed 's/^/        /' "$tmp/out" | tail -20
  fi
else
  echo "  SKIP  8 workbench window constructs (no xvfb-run)"
fi

$SV equiv --tools >"$tmp/out" 2>&1; got=$?
check "9 external-tool scan" 0 $got \
  "frontend" "verible" "built-in" "frontends usable:"

# A frontend whose tool is absent must be named BEFORE the parse, with the fix
# in the message.  Made deterministic by hiding every place the scan looks:
# sv-parser only lives under $HOME or on $PATH.
mkdir -p "$tmp/home" "$tmp/cfg"
env HOME="$tmp/home" XDG_CONFIG_HOME="$tmp/cfg" PATH=/nonexistent \
  $SV equiv --a sv-parser,acc,$A --b verible,acc,$A >"$tmp/out" 2>&1; got=$?
check "10 missing tool named early, with the fix" 2 $got \
  "needs 'parse_sv'" "SV_PARSER_BIN" "--tool sv-parser=/path"

# Selecting a binary persists it and the next scan uses it.  /bin/true stands
# in for a tool we do not have: the point under test is the picker, not the
# parse.
env HOME="$tmp/home" XDG_CONFIG_HOME="$tmp/cfg" PATH=/nonexistent \
  $SV equiv --tool sv-parser=/bin/true --tools >"$tmp/out" 2>&1; got=$?
check "11 tool selection persists" 0 $got \
  "selected sv-parser → /bin/true" "/bin/true" "selected"
grep -q '"sv-parser": "/bin/true"' "$tmp/cfg/sv_suite/tools.json" \
  || { echo "  FAIL  11b selection written to tools.json"; fail=$((fail+1)); }

# The scan is only worth anything if what it offers actually runs.  Take the
# first external SV frontend it reports usable and prove the same equivalence
# through it.  The timeout is part of the test: exporting a path into
# VERILATOR_BIN (verilator's OWN variable, naming verilator_bin for its perl
# driver) made the wrapper re-exec itself without end, which shows up here as
# 124 rather than as a wrong answer.
usable=$($SV equiv --tools 2>/dev/null | sed -n 's/^ *[0-9]* of [0-9]* frontends usable: //p')
pick=""
for fe in slang verilator synlig; do
  case ", $usable," in *", $fe,"*) pick=$fe; break;; esac
done
if [ -n "$pick" ]; then
  timeout 180 $SV equiv --a verible,acc,$A --b "$pick",acc,$BR >"$tmp/out" 2>&1; got=$?
  [ "$got" = 124 ] && echo "    TIMED OUT — a frontend that never terminates"
  check "12 discovered tool actually runs ($pick)" 0 $got \
    "VERDICT: EQUIVALENT" "2 by simulation"
else
  echo "  SKIP  12 discovered tool actually runs (no external SV frontend found)"
fi

echo "== pass=$pass fail=$fail =="
[ $fail = 0 ]
