#!/bin/bash
# Run primitive-comparison harness over the test set.
# Default backend is xilinx_rtl (matches Vivado RTL_* names);
# pass 'structural' to see the original generic-primitive output.
#
# The small tests (inverter, and2, mux2, reg1, add4) all match Vivado 1:1.
# The apb_uart test exercises a much larger design (UART 16750 with FIFO,
# baud generator, interrupt controller) and currently shows a substantial
# gap — see the printed family table for what's still missing on the
# sv_main side.
set -e
here=$(cd "$(dirname "$0")" && pwd)
cd "$here"

backend=${1:-xilinx_rtl}
tests="inverter and2 mux2 reg1 add4 sr_ff8 blocking blocking_chain blocking_observed apb_uart"

for t in $tests; do
    bash compare.sh "$t" "$backend"
done

cat <<EOF

=== Notes ===
* Vivado uses generic RTL_* primitives after \`synth_design -rtl\` (elaboration only).
* The xilinx_rtl backend renames the structural backend's primitives to match
  (bitwise_not -> RTL_INV, bitwise_and -> RTL_AND, ..., dff_en -> RTL_REG_SYNC).
* For sequential logic, sync-reset fusion (sv_gen_xilinx_rtl.ml:fuse_sync_reset)
  collapses (~rst & d) -> dff_en into a single RTL_REG_SYNC, matching how Vivado
  expresses the same source SV.
* Vivado adds per-instance numeric suffixes for its viewer (RTL_ADD0, RTL_MUX103,
  RTL_REG_ASYNC__BREG_31). compare.sh normalizes these to family names before
  counting.
* Run \`./run_all.sh structural\` to see the pre-rename gap.
EOF
