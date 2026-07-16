#!/bin/bash
# Synthesis-path self-test / regression suite.
#
# Runs recipes/xil_models_selftest.lua, which Z3-miters the Xilinx primitive
# models (LUT/FF/SRL/CARRY) and the srl_infer pass against behavioral specs, and
# guards the bignum (BConst.value : Z.t) fixes with wide-constant regressions:
#   - LUT2/LUT6/FDRE/FDSE primitive models
#   - SRL16E / SRLC32E functional models (tap 15 / tap 6 / tap 31 + Q31)
#   - srl_infer end-to-end (FF-chain == inferred SRL cells)
#   - 128-bit constant bit-exact through behavioral optimize (Z.to_int guard)
#   - wide gate_map 128b/96b + cellmapped write (signal_of_z / initial_values,
#     write_cellmapped_v completeness guard positive path)
#
# Exit 0 iff every case proves EQUIVALENT / completes (fail=0).
set -e
cd "$(dirname "$0")/.."
eval "$(opam env)" 2>/dev/null || true
dune build _build/default/sv_suite.exe
# FPGA_LEC_NAMES makes of_circuit name FF Q nets after the RTL register, which the
# sequential FF-state-alignment cases need; it's inert for the combinational ones.
OUT=$(FPGA_LEC_NAMES=1 ./_build/default/sv_suite.exe script recipes/xil_models_selftest.lua 2>&1)
echo "$OUT" | grep -E '\->|pass=' || true
if echo "$OUT" | grep -qE '== pass=[0-9]+ fail=0 =='; then
  echo "SELFTEST PASS"
  exit 0
else
  echo "SELFTEST FAIL"
  exit 1
fi
