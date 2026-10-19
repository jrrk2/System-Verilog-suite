#!/bin/bash
# The module the svs stage stopped on: common_cells rr_arb_tree, 5 inputs, as
# CVA6's AXI crossbar instantiates it (there with an AXI channel struct as
# DataType).  Maps it through the same pipeline at a few data widths and prints
# time / peak memory per width, under an SVS_MAX_MEM_MB cap.
#   test/cva6_litex/repro_rr_arb_tree.sh [widths...]      (default: 8 32 128)
set -u
HERE=$(cd "$(dirname "$0")" && pwd); ROOT=$(cd "$HERE/../.." && pwd)
SV=${SV_SUITE:-$ROOT/_build/default/sv_suite.exe}
CC=${CC_DIR:-$HOME/xc7-bitstream-tools/litex-deps/pythondata-cpu-cva6/pythondata_cpu_cva6/system_verilog/vendor/pulp-platform/common_cells}
OUT=${OUT:-$HOME/svs-gatesim/cva6-litex/repro}; mkdir -p "$OUT"
widths=("$@"); [ ${#widths[@]} -eq 0 ] && widths=(8 32 128)
for W in "${widths[@]}"; do
  verilator --json-only --json-only-output "$OUT/a$W.json" -Wno-fatal -Wno-lint -Wno-style \
    +define+SYNTHESIS=1 -GW=$W --top-module a -I"$CC/include" \
    "$CC/src/cf_math_pkg.sv" "$CC/src/lzc.sv" "$CC/src/rr_arb_tree.sv" "$HERE/repro_rr_arb_tree.sv" \
    > "$OUT/a$W.vlt.log" 2>&1 || { echo "W=$W: verilator failed ($OUT/a$W.vlt.log)"; continue; }
  cat > "$OUT/a$W.lua" <<LUA
prog = svd.parse("verilator", "a", {"$OUT/a$W.json"})
prog = svd.unroll(prog); prog = svd.inline(prog); prog = svd.iflift(prog)
prog = svd.blocking_subst(prog); prog = svd.meminfer(prog); prog = svd.memlower(prog)
m = svd.gate_map(svd.pick(prog, "rr_arb_tree__N5_Tz1_Az2_Lz2"), 6, 0)
print("mapped")
LUA
  (cd "$ROOT" && /usr/bin/time -f "W=$W %e s %M KB" env SVS_MAX_MEM_MB=${SVS_MAX_MEM_MB:-8000} \
     timeout 600 "$SV" script "$OUT/a$W.lua") 2>&1 | grep -E '^mapped|^W=|mem-limit'
done
