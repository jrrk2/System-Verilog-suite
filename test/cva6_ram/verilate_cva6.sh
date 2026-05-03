#!/bin/bash
# Run verilator --json-only on the cva6 core, mirroring the source
# list and command line that `make verilate` uses, but emitting the
# AST JSON instead of building a sim binary. The result is consumed
# by test_cva6_bottom_up to do per-module formal equivalence against
# the matching entity in cva6_elab.vhd.

set -e
here=$(cd "$(dirname "$0")" && pwd)
cva6_root=${CVA6_REPO_DIR:-$HOME/cva6}
top=${TOP:-cva6}
out_dir=$here/${top}_verilate.json.dir
mkdir -p "$out_dir"

# Mirror Makefile's `ariane_pkg` and the `--top-module ariane_testharness`
# pieces, but elaborate just `cva6` (the core, not the testharness — we
# don't have the riscv libs, and the tb references those). For module
# bottom-up comparison we don't need the testharness anyway: every
# entity in cva6_elab.vhd is a child of `cva6`.
ariane_pkg=(
    "$cva6_root/corev_apu/tb/ariane_axi_pkg.sv"
    "$cva6_root/corev_apu/tb/axi_intf.sv"
    "$cva6_root/corev_apu/register_interface/src/reg_intf.sv"
    "$cva6_root/corev_apu/tb/ariane_soc_pkg.sv"
    "$cva6_root/corev_apu/riscv-dbg/src/dm_pkg.sv"
    "$cva6_root/corev_apu/tb/ariane_axi_soc_pkg.sv"
    "$cva6_root/corev_apu/fpga/src/heavyhash/keccak_pkg.sv"
    "$cva6_root/corev_apu/fpga/src/heavyhash/heavyhash_pkg.sv"
)

# Globally-included headers cva6 sets via Vivado's
# `is_global_include 1` property (genesys2 board defines + register
# helper macros). Verilator's equivalent is +include + a forced
# read order, but a global include via the SVH wrapper works too.
hdr_genesys=$cva6_root/corev_apu/fpga/src/genesysii.svh
hdr_regs=$cva6_root/vendor/pulp-platform/common_cells/include/common_cells/registers.svh

export HPDCACHE_DIR=${HPDCACHE_DIR:-$cva6_root/core/cache_subsystem/hpdcache}
export TARGET_CFG=${TARGET_CFG:-cv64a6_imafdc_sv39}
cd "$cva6_root"
echo "[verilate] elaborating $top → $out_dir/V${top}.tree.json"

# core/Flist.cva6 already provides every cva6-core source plus the
# include dirs (with +incdir+ directives), so we just hand it to
# verilator as a -f file. Add the packages and FPGA-side bridges that
# Vivado's add_sources.tcl pulls in (skipping the testbench, the
# rv_tracer's duplicate lzc.sv, and ariane_xilinx which uses virtual
# interfaces in continuous assignments — Vivado 2020.1 doesn't
# support those and we don't need them for the core).
verilator --no-timing --json-only \
    -Wno-fatal -Wno-WIDTHTRUNC -Wno-WIDTHEXPAND -Wno-UNOPTFLAT \
    -Wno-CASEINCOMPLETE -Wno-CASEX -Wno-PINMISSING -Wno-IMPLICIT \
    -Wno-DECLFILENAME -Wno-MULTITOP -Wno-UNSIGNED -Wno-CMPCONST \
    -Wno-LATCH -Wno-style \
    --top-module "$top" \
    --unroll-count 256 \
    -f core/Flist.cva6 \
    "${ariane_pkg[@]}" \
    "$hdr_genesys" "$hdr_regs" \
    --Mdir "$out_dir" \
    > "$here/${top}_verilate.log" 2>&1
status=$?
echo "[verilate] exit=$status (log: ${top}_verilate.log)"
exit $status
