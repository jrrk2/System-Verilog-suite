#!/bin/bash
# Flatten the TALOS-V2 VC707 microgpt_eth build into a single .sv via
# verilator -E.  Mirrors scripts/run.tcl's read_verilog ordering +
# include_dirs + verilog_define list.
#
# Output: <here>/talos_vc707_flat.sv  (the single flattened file)
#         <here>/talos_vc707_flat.err (any verilator -E warnings)
#
# Override paths via TALOS_DIR=/path env knob.

set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
talos=${TALOS_DIR:-$HOME/TALOS-V2/rtl/vc707}

if [ ! -d "$talos" ]; then
    echo "[flatten] talos dir not found: $talos" >&2
    exit 2
fi

flat="$here/talos_vc707_flat.sv"

# Match run.tcl: same files, same order.
files=(
    # ethernet stack
    eth/axis_gmii_rx.sv
    eth/axis_gmii_tx.sv
    eth/dualmem_widen8.sv
    eth/dualmem_widen.sv
    eth/eth_mac_1g.sv
    eth/framing_top_sgmii.sv
    eth/rgmii_lfsr.sv
    eth/sgmii_soc.sv
    # microgpt core
    ../src/microgpt_categorical_sampler.sv
    ../src/microgpt_exact_core.sv
    ../src/rms_scale_engine.sv
    ../src/sat_div16_engine.sv
    ../src/systolic_matvec16_tile.sv
    # vc707 top + smollm blocks
    src/microgpt_eth_ctrl.sv
    src/weight_stream_axi.sv
    src/weight_tile_cache.sv
    src/ddr_write_master.sv
    src/vc707_microgpt_eth.sv
    src/smollm/cordic_sincos.sv
    src/smollm/matvec_int8_engine.sv
    src/smollm/matvec_selftest.sv
    src/smollm/rmsnorm.sv
    src/smollm/rmsnorm_selftest.sv
    src/smollm/rope.sv
    src/smollm/rope_selftest.sv
    src/smollm/swiglu.sv
    src/smollm/swiglu_selftest.sv
    src/smollm/softmax_q15.sv
    src/smollm/softmax_selftest.sv
    # int8 SmolLM2 multilayer chain retired (23040×16 hang trap):
    # smollm_layer.sv, smollm_multilayer_tm.sv, weight_streamer_brom.sv,
    # weight_streamer_mt.sv, factor_ram.sv.
    src/smollm/bfp_sdpram.sv
    src/smollm/matvec_bfp_engine.sv
    src/smollm/rmsnorm_bfp.sv
    src/smollm/rope_bfp.sv
    src/smollm/swiglu_bfp.sv
    src/smollm/softmax_bfp.sv
    src/smollm/residual_bfp.sv
    src/smollm/weight_streamer_bfp_mt.sv
    src/smollm/smollm_layer_bfp.sv
    src/smollm/smollm_layer_bfp_selftest.sv
    src/smollm/embed_lookup_bfp.sv
    src/smollm/smollm_multilayer_tm_bfp.sv
    src/smollm/smollm_decode_head_bfp.sv
    src/smollm/autoregress_bfp_top.sv
)

# Generated brom_*.sv wrappers (may not exist if `make` hasn't run yet).
shopt -s nullglob
brom_files=( "$talos/../generated"/brom_*.sv )
shopt -u nullglob

# Define set tracks scripts/run.tcl: int8 path (default) vs block-FP
# (USE_BFP=1) vs block-FP + streaming (USE_BFP=1 BFP_STREAM=1).
USE_BFP=${USE_BFP:-0}
BFP_STREAM=${BFP_STREAM:-0}
weight_dir=$(cd "$talos/../generated" 2>/dev/null && pwd || echo /tmp)
defines=(
    "+define+MICROGPT_WEIGHT_DIR=\"$weight_dir\""
    "+define+MICROGPT_NO_OP_TESTS"
    "+define+VC707"
)
if [ "$USE_BFP" = "1" ]; then
    defines+=( "+define+MICROGPT_USE_BFP" )
    if [ "$BFP_STREAM" = "1" ]; then
        defines+=( "+define+MICROGPT_BFP_STREAM" )
        echo "[flatten] mode: block-FP + DDR3 streaming (USE_BFP=1 BFP_STREAM=1)"
    else
        echo "[flatten] mode: block-FP (USE_BFP=1)"
    fi
else
    defines+=(
        "+define+MICROGPT_DDR3_WEIGHTS"
        "+define+MICROGPT_ILA"
        "+define+MICROGPT_LAYER_DEBUG"
    )
    echo "[flatten] mode: int8 multilayer (default)"
fi

incdirs=(
    "+incdir+$talos/../src/include"
    "+incdir+$talos/src"
    "+incdir+$talos/src/smollm"
    "+incdir+$talos/../generated"
)

cd "$talos"
echo "[flatten] verilator -E → $flat"
verilator -E --pp-comments --no-timing \
    -Wno-fatal -Wno-WIDTHTRUNC -Wno-WIDTHEXPAND -Wno-UNOPTFLAT \
    -Wno-CASEINCOMPLETE -Wno-CASEX -Wno-PINMISSING -Wno-IMPLICIT \
    -Wno-DECLFILENAME -Wno-MULTITOP -Wno-UNSIGNED -Wno-CMPCONST \
    -Wno-LATCH -Wno-style \
    --top-module vc707_microgpt_eth \
    "${defines[@]}" "${incdirs[@]}" \
    src/vc707.svh \
    "${files[@]}" \
    "${brom_files[@]}" \
    > "$flat" 2> "$flat.err" || true

# Strip pragma translate_off blocks + assert/property blocks (same
# treatment as test/cva6_ram/run_ff_diff.sh — Verible's parser bombs
# on the property-spec sub-language).
sed -i -E -e '/\/\/[[:space:]]*pragma[[:space:]]+translate_off/,/\/\/[[:space:]]*pragma[[:space:]]+translate_on/d' \
            -e '/^`line/d' "$flat"

awk '
  function is_assert(s) {
    return match(s, /^[[:space:]]*([A-Za-z_][A-Za-z0-9_]*[[:space:]]*:[[:space:]]*)?(assert|assume|cover|expect)[[:space:]]+property([^A-Za-z0-9_]|$)/)
  }
  function is_property_decl(s) {
    return match(s, /^[[:space:]]*(property|sequence)[[:space:]]+[A-Za-z_]/)
  }
  function is_endproperty(s) {
    return match(s, /^[[:space:]]*end(property|sequence)([^A-Za-z0-9_]|$)/)
  }
  BEGIN { in_a = 0; in_p = 0 }
  {
    if (in_p) { if (is_endproperty($0)) in_p = 0; next }
    if (in_a) { if (match($0, /;[[:space:]]*$/)) in_a = 0; next }
    if (is_assert($0))        { in_a = 1; next }
    if (is_property_decl($0)) { in_p = 1; next }
    print
  }
' "$flat" > "$flat.tmp" && mv "$flat.tmp" "$flat"

echo "[flatten] $(wc -l < "$flat") lines, $(stat -c%s "$flat") bytes"
echo "[flatten] err log: $flat.err"
