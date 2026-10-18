#!/bin/bash
# Bounded co-simulation of the gate_map passes on the OCaml VM: the BIR that
# goes INTO gate_map against the LUT netlist that comes OUT, driven by
# identical random inputs for VM_COSIM_CYCLES cycles (svd.bmc_sim).  The VM
# is far beyond a Z3 proof, so this is the check its restructuring passes get.
#
# The memories are shrunk (STACK_AW=4, HEAP_AW=5, GLOBALS_AW=4) so the copy
# simulates quickly; every state, opcode arm and datapath is unchanged.
# meminfer/memlower are left out, so the tiny memories reach gate_map as
# arrays and come out as FFs and muxes: bmc_sim cuts every black box into
# free inputs / compared outputs, and a RAMB36E1 tile's ports are not wired
# alike on the two sides (`DIADI width 36 vs 1`), so no RAM primitive may be
# left in the comparison.  The DSP48E1s from dsp_infer are cut identically
# on both sides (the DSP-vs-multiply step is dsp_selftest.sh's proof).
set -u
HERE=$(cd "$(dirname "$0")" && pwd); ROOT=$(cd "$HERE/../.." && pwd)
SRC=${VM_SRC:-$HOME/bytecode/ocaml4142_vm_rtl.sv}
OUT=${VM_OUT:-$HERE/out_vm_cosim}; mkdir -p "$OUT"
[ -f "$SRC" ] || { echo "vm_cosim: missing $SRC"; exit 2; }
sed -e 's/parameter int STACK_AW *= *[0-9]*,/parameter int STACK_AW = 4,/' \
    -e 's/parameter int HEAP_AW *= *[0-9]*,/parameter int HEAP_AW = 5,/' \
    -e 's/parameter int GLOBALS_AW *= *[0-9]*,/parameter int GLOBALS_AW = 4,/' "$SRC" > "$OUT/vm_small.sv"
for h in "$(dirname "$SRC")"/*.h "$(dirname "$SRC")"/*.svh; do [ -f "$h" ] && ln -sf "$h" "$OUT/"; done
cat > "$OUT/cosim.lua" <<LUA
F = {"$OUT/vm_small.sv"}
prog = svd.parse("verible", "ocaml4142_vm_rtl", F)
prog = svd.unroll(prog); prog = svd.inline(prog); prog = svd.flatten(prog); prog = svd.iflift(prog); prog = svd.blocking_subst(prog)
prog = svd.srl_infer(prog); prog = svd.dsp_infer(prog)
mapped = svd.gate_map(svd.pick(prog, "ocaml4142_vm_rtl"), 6, 0)
sp = svd.augment_xil_models(svd.mapped_to_prog(mapped))
ref = svd.augment_xil_models(prog)
print("VM-COSIM: " .. svd.bmc_sim(svd.pick(ref, "ocaml4142_vm_rtl"), svd.pick(sp, "ocaml4142_vm_rtl")))
LUA
(cd "$ROOT" && BMC_CYCLES=${VM_COSIM_CYCLES:-3000} "$ROOT/_build/default/sv_suite.exe" script "$OUT/cosim.lua") > "$OUT/cosim.log" 2>&1
r=$(grep -m1 "^VM-COSIM" "$OUT/cosim.log")
echo "${r:-VM-COSIM: no result (see $OUT/cosim.log)}"
case "$r" in *BOUNDED-EQUIVALENT*) exit 0;; *) exit 1;; esac
