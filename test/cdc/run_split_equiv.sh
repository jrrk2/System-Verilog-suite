#!/bin/bash
# Prove the per-clock-domain split PRESERVES the design.
#
# The checker in Behavioral_cdc_check says the cut is safe to time; it says
# nothing about whether the split still computes what the original computed.
# Moving a combinational cone into a neighbouring module, replicating one into
# two, electing which copy drives the top-level net -- every one of those is a
# rewrite that can silently drop a driver.  So: SAT-equivalence, gold (the
# unsplit BIR) against gate (the split), not a simulation sample.
#
#   test/cdc/run_split_equiv.sh          # uses deps/yosys or $YOSYS
#
# Exit 0 iff every case proves equivalent.
set -e
cd "$(dirname "$0")/../.."
eval "$(opam env)" 2>/dev/null || true

OUT=/tmp/domain_split
# The pinned deps/yosys (this file lives in deps/System-Verilog-suite, so the
# sibling is ../yosys) before anything on PATH: the tree pins a fork, and a
# stray system yosys is not the one the rest of the flow is proven against.
YOSYS=${YOSYS:-$( [ -x ../yosys/yosys ] && echo ../yosys/yosys || command -v yosys )}
[ -x "$YOSYS" ] || { echo "no yosys at deps/yosys/yosys -- set YOSYS=/path/to/yosys" >&2; exit 1; }

dune build ./_build/default/sv_suite.exe
mkdir -p $OUT
./_build/default/sv_suite.exe script recipes/domain_split_selftest.lua >$OUT/selftest.log 2>&1
grep -E '^(PASS|FAIL|==)' $OUT/selftest.log || true
grep -q '== pass=[0-9]* fail=0 ==' $OUT/selftest.log || { echo "SPLIT SELFTEST FAIL"; exit 1; }

fail=0
equiv () {
  local top=$1; shift
  echo "--- $top: gold (unsplit) vs gate (split)"
  # -flatten on both sides: the split's whole point is that the hierarchy
  # differs, so the comparison has to be of the logic, not of the boundaries.
  "$YOSYS" -q -p "
    read_verilog -sv $OUT/${top}_gold.v
    prep -top $top -flatten
    design -stash gold
    read_verilog -sv $*
    prep -top $top -flatten
    design -stash gate
    design -copy-from gold -as gold $top
    design -copy-from gate -as gate $top
    equiv_make gold gate equiv
    prep -top equiv
    equiv_simple
    equiv_induct
    equiv_status -assert
  " && echo "EQUIVALENT  $top" || { echo "NOT EQUIVALENT  $top"; fail=1; }
}

equiv s_two    "$OUT/s_two_split_top.v $OUT/s_two_clk_a.v $OUT/s_two_clk_b.v"
equiv s_shared "$OUT/s_shared_split_top.v $OUT/s_shared_clk_a.v $OUT/s_shared_clk_b.v"

# The faster-domain rule, checked on the emitted RTL rather than on the log
# line that claims it: s_shared's parity cone feeds flops on both clocks, and
# with clk_a at 8 ns against clk_b's 40 it must be emitted into the clk_a
# module ONLY.  Equivalence alone cannot see this -- both placements compute
# the same function, and only one of them is timed under the constraint that
# actually binds.
if grep -q 'parity' $OUT/s_shared_clk_a.v && ! grep -q 'parity =' $OUT/s_shared_clk_b.v; then
  echo "POLICY OK   shared cone went to the faster domain (clk_a)"
else
  echo "POLICY FAIL shared cone is not in the faster domain alone"; fail=1
fi

[ $fail = 0 ] && { echo "SPLIT EQUIV PASS"; exit 0; } || { echo "SPLIT EQUIV FAIL"; exit 1; }
