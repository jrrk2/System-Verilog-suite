#!/bin/bash
# Writer roundtrip oracle: a front end's tree, written out as behavioural
# Verilog (svd.write_verilog) and read back by verible, must miter EQUIVALENT
# to the tree it came from.  Proves the diagnostic Verilog says what the IR
# says -- the precondition for using that Verilog as the "source" of any
# other check (gl_cosim.sh on a verilator-elaborated design does exactly that).
#   test/qor/roundtrip.sh <frontend> <top> <file.sv>...     (frontend: verible|yosys|verilator|slang)
#   RT_OUT=<dir>  SV_SUITE=<exe>  Z3_MITER_TIMEOUT_MS  RT_TIMEOUT=<s, whole run, default 1800>
# Prints `ROUNDTRIP <frontend> <top>: EQUIVALENT|DIFFER ...`; exit 0 iff EQUIVALENT.
set -u
HERE=$(cd "$(dirname "$0")" && pwd); ROOT=$(cd "$HERE/../.." && pwd)
FE=$1; TOP=$2; shift 2
OUT=${RT_OUT:-$HERE/out_rt/$FE/$TOP}; mkdir -p "$OUT"
files=$(for f in "$@"; do printf '"%s",' "$(cd "$(dirname "$f")" && pwd)/$(basename "$f")"; done); files=${files%,}
export YOSYS_BIN=${YOSYS_BIN:-${YOSYS:-$HOME/OpenROAD-flow-scripts/tools/install/yosys/bin/yosys}}
cat > "$OUT/rt.lua" <<LUA
a = svd.parse("$FE", "$TOP", {$files})
svd.write_verilog(a, "$OUT/rt.v")
b = svd.parse("verible", "$TOP", {"$OUT/rt.v"})
print("ROUNDTRIP $FE $TOP: " .. svd.miter_hier(a, b, "$TOP"))
LUA
(cd "$ROOT" && Z3_MITER_TIMEOUT_MS=${Z3_MITER_TIMEOUT_MS:-120000} timeout "${RT_TIMEOUT:-1800}" "${SV_SUITE:-./_build/default/sv_suite.exe}" script "$OUT/rt.lua") > "$OUT/rt.log" 2>&1
[ ${PIPESTATUS[0]:-0} = 124 ] && echo "ROUNDTRIP $FE $TOP: TIMEOUT after ${RT_TIMEOUT:-1800}s" >> "$OUT/rt.log"
r="ROUNDTRIP $FE $TOP: $(grep -m1 "^HIER-SUMMARY" "$OUT/rt.log" | sed "s/HIER-SUMMARY //")"; grep -q "^HIER-SUMMARY" "$OUT/rt.log" || r=""
[ -z "$r" ] && r="ROUNDTRIP $FE $TOP: FAILED ($(grep -m1 -E 'lua:|Error|error' "$OUT/rt.log" | cut -c1-160))"
echo "$r"
case "$r" in *"whole design EQUIVALENT"*) exit 0;; *) exit 1;; esac
