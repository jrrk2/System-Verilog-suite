#!/bin/bash
# Convenience wrapper for the bottom-up cva6 miter.
#
# Builds the driver, runs verilate_cva6.sh if the Verilator JSON is
# stale (or missing), elaborates with Vivado if cva6_elab.vhd is
# stale (or missing), then runs the per-module miter on the modules
# matched by any of the substring filters supplied as args.
#
# Examples:
#   bash bottom_up.sh                  # sweep every module
#   bash bottom_up.sh lzc popcount     # filtered to two leaves
#   FORCE=1 bash bottom_up.sh          # rebuild both elaborations

set -e
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
cva6_root=${CVA6_REPO_DIR:-$HOME/cva6}
top=${TOP:-cva6}
json=$here/${top}_verilate.json.dir/V${top}.tree.json
vhd=$here/${top}_elab.vhd
exe=$repo/_build/default/test_cva6_bottom_up.exe

# Build the driver (cheap if already up to date).
( cd "$repo" && dune build test_cva6_bottom_up.exe )

stale() {
    [ -n "$FORCE" ] && return 0
    [ ! -f "$1" ] && return 0
    # Rebuild if any cva6 source is newer than the artifact.
    find "$cva6_root/core" "$cva6_root/corev_apu" "$cva6_root/vendor" \
         -name '*.sv' -newer "$1" -print -quit 2>/dev/null | grep -q . && return 0
    return 1
}

if stale "$json"; then
    echo "[bottom_up] Verilator JSON stale → re-elaborating ($json)"
    RISCV=${RISCV:-/usr/bin} CVA6_REPO_DIR=$cva6_root \
        bash "$here/verilate_cva6.sh"
fi

if stale "$vhd"; then
    echo "[bottom_up] Vivado VHDL stale → re-elaborating ($vhd)"
    echo "  (this takes ~5 min — set TOP=<entity> to elaborate just one)"
    TOP=$top OUT_BASE=${top}_elab \
        /NFS/apps/Xilinx/Vivado/2020.1/bin/vivado \
        -mode batch -source "$here/elab_cva6.tcl" \
        > "$here/${top}_vivado_elab.log" 2>&1
fi

# Off we go.
"$exe" "$json" "$vhd" "$@"
