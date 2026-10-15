#!/bin/bash
# DSP48E1 inference self-test: unsigned, signed and multiply-accumulate 4-bit
# multipliers, each proven (Z3) at two levels against the plain source:
#   DSP-BIR  : the BIR after dsp_infer (DSP48E1 instance + behavioural model)
#   MAPPED   : the LUT/DSP netlist gate_map produced from it
# Small widths on purpose: Z3 proves 4x4 in seconds, 8x8 takes minutes.
set -u
HERE=$(cd "$(dirname "$0")" && pwd); ROOT=$(cd "$HERE/../.." && pwd)
LUA=$(mktemp /tmp/dsp_selftest_XXXX.lua)
cat > "$LUA" <<LUA
F = {"$HERE/designs/mul4.sv"}
tops = {"mul4u", "mul4s", "macc4"}
i = 1
while tops[i] do
  t = tops[i]
  src = svd.parse("verible", t, F)
  prog = svd.parse("verible", t, F)
  prog = svd.unroll(prog); prog = svd.inline(prog); prog = svd.iflift(prog); prog = svd.blocking_subst(prog)
  prog = svd.meminfer(prog); prog = svd.memlower(prog); prog = svd.dsp_infer(prog)
  mapped = svd.gate_map(svd.pick(prog, t), 6, 0)
  sp = svd.augment_xil_models(svd.mapped_to_prog(mapped))
  print("DSP-BIR " .. t .. ": " .. svd.miter(svd.pick(src, t), svd.pick(svd.augment_xil_models(prog), t)))
  print("MAPPED  " .. t .. ": " .. svd.miter(svd.pick(src, t), svd.pick(sp, t)))
  i = i + 1
end
LUA
export Z3_MITER_TIMEOUT_MS=${Z3_MITER_TIMEOUT_MS:-120000}
(cd "$ROOT" && ./_build/default/sv_suite.exe script "$LUA") 2>&1 | grep -E "^(DSP-BIR|MAPPED) " | tee /dev/stderr | grep -vq "EQUIVALENT$" 
rc=$?; rm -f "$LUA"
# grep -v found a non-EQUIVALENT line => rc 0 => fail
if [ $rc -eq 0 ]; then echo "dsp_selftest: FAIL"; exit 1; else echo "dsp_selftest: all 6 proofs EQUIVALENT"; fi
