#!/bin/bash
# Large-benchmark QoR: the OCaml bytecode VM (~4k lines, one module, 96-state
# machine, 333 BRAM tiles).  No yosys reader takes the source and a whole-
# module Z3 vote does not finish, so this maps it and reports numbers only:
# cell counts, logic depth, and the registers the LUTs feed (attrib.py).
#
#   test/qor/run_vm.sh                 Verible front-end, default profile
#   VM_FRONTEND=verilator test/qor/run_vm.sh
#   VM_SRC=<path>  VM_OUT=<dir>  plus any GATE_MAP_* / CARRY4_* / DSP_SPLIT /
#   SVS_ABC_ROUNDS / LUTCOVER_BACKEND override (see test/qor/README.md)
set -u
HERE=$(cd "$(dirname "$0")" && pwd); ROOT=$(cd "$HERE/../.." && pwd)
SRC=${VM_SRC:-$HOME/bytecode/ocaml4142_vm_rtl.sv}
FE=${VM_FRONTEND:-verible}
OUT=${VM_OUT:-$HERE/out_vm}; mkdir -p "$OUT"
[ -f "$SRC" ] || { echo "run_vm: missing $SRC"; exit 2; }
SV_SUITE=${SV_SUITE:-$ROOT/_build/default/sv_suite.exe}
[ -x "$SV_SUITE" ] || { echo "build first: dune build sv_suite.exe"; exit 2; }
{
  echo "prog = svd.parse(\"$FE\", \"ocaml4142_vm_rtl\", {\"$SRC\"})"
  for p in unroll inline flatten iflift blocking_subst meminfer memlower srl_infer dsp_infer; do
    echo "prog = svd.$p(prog)"; done
  echo 'print("opts: " .. svd.fpga_opts())'
  echo 'mapped = svd.gate_map(svd.pick(prog, "ocaml4142_vm_rtl"), 6, 0)'
  echo "svd.write_mapped_json(mapped, \"$OUT/vm.json\")"
  echo 'print("done")'
} > "$OUT/vm.lua"
t0=$(date +%s)
(cd "$ROOT" && "$SV_SUITE" script "$OUT/vm.lua") > "$OUT/vm.log" 2>&1
wall=$(( $(date +%s) - t0 ))
[ -f "$OUT/vm.json" ] || { echo "run_vm: mapping FAILED: $(grep -m1 -i 'error\|exception' "$OUT/vm.log")"; exit 1; }
echo "OCaml VM ($FE): $(python3 -I "$HERE/count.py" "$OUT/vm.json")  $(python3 -I "$HERE/depth.py" "$OUT/vm.json" | sed 's/.*: //')  wall ${wall}s"
python3 -I "$HERE/attrib.py" "$OUT/vm.json" 15 | tee "$OUT/attrib.txt"
