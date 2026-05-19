#!/bin/bash
# Per-file pre-flatten + floorplanner driver for TALOS-V2 VC707.
# Runs verilator -E on each source file individually (with correct
# defines + incdirs), then hands the per-file flat to test_floorplan.
# Files that don't parse get a skip note in the err log but don't
# block the rest of the sweep.
#
# Output: tsv on stdout, blowup summary on stderr.

set -eu
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
talos=${TALOS_DIR:-$HOME/TALOS-V2/rtl/vc707}
work="$here/per_file_work"
mkdir -p "$work"

files=(
    eth/axis_gmii_rx.sv
    eth/axis_gmii_tx.sv
    eth/dualmem_widen8.sv
    eth/dualmem_widen.sv
    eth/eth_mac_1g.sv
    eth/framing_top_sgmii.sv
    eth/rgmii_lfsr.sv
    eth/sgmii_soc.sv
    ../src/microgpt_categorical_sampler.sv
    ../src/microgpt_exact_core.sv
    ../src/rms_scale_engine.sv
    ../src/sat_div16_engine.sv
    ../src/systolic_matvec16_tile.sv
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
    src/smollm/weight_streamer_brom.sv
    src/smollm/weight_streamer_mt.sv
    src/smollm/factor_ram.sv
    src/smollm/smollm_layer.sv
    src/smollm/smollm_layer_selftest.sv
    src/smollm/smollm_multilayer_tm.sv
    src/smollm/smollm_multilayer_tm_selftest.sv
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

incdirs=(
    "+incdir+$talos/../src/include"
    "+incdir+$talos/src"
    "+incdir+$talos/src/smollm"
    "+incdir+$talos/../generated"
)

defines=(
    "+define+MICROGPT_WEIGHT_DIR=\"$(cd "$talos/../generated" 2>/dev/null && pwd || echo /tmp)\""
    "+define+MICROGPT_DDR3_WEIGHTS"
    "+define+MICROGPT_NO_OP_TESTS"
    "+define+VC707"
)

cd "$talos"
flats=()
for f in "${files[@]}"; do
    base=$(basename "$f" .sv)
    flat="$work/${base}.flat.sv"
    verilator -E --pp-comments --no-timing \
        -Wno-fatal -Wno-WIDTHTRUNC -Wno-WIDTHEXPAND -Wno-UNOPTFLAT \
        -Wno-CASEINCOMPLETE -Wno-CASEX -Wno-PINMISSING -Wno-IMPLICIT \
        -Wno-DECLFILENAME -Wno-MULTITOP -Wno-UNSIGNED -Wno-CMPCONST \
        -Wno-LATCH -Wno-style \
        "${defines[@]}" "${incdirs[@]}" \
        src/vc707.svh "$f" \
        > "$flat" 2>>"$work/preproc.err" || \
        echo "[preproc-fail] $f" >> "$work/preproc.err"
    # Strip pragma blocks + line directives + asserts.
    if [ -s "$flat" ]; then
      sed -i -E -e '/\/\/[[:space:]]*pragma[[:space:]]+translate_off/,/\/\/[[:space:]]*pragma[[:space:]]+translate_on/d' \
                  -e '/^`line/d' "$flat"
      flats+=("$flat")
    fi
done

echo "[floorplan] $(echo "${#flats[@]}") flattened files ready" >&2
"$repo/_build/default/test_floorplan.exe" "${flats[@]}"
