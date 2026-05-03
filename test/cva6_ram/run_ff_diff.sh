#!/bin/bash
# Per-entity FF-set diff between Vivado-elab and Verible-elab on the
# full cva6 core. Pre-flattens the SV sources via verilator -E -P
# (same trick as sv_tests integration), then hands the flattened
# single .sv to Verible via MITER_VERIBLE_FILES.

set -e
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
cva6_root=${CVA6_REPO_DIR:-$HOME/cva6}

export CVA6_REPO_DIR=$cva6_root
export HPDCACHE_DIR=${HPDCACHE_DIR:-$cva6_root/core/cache_subsystem/hpdcache}
export TARGET_CFG=${TARGET_CFG:-cv64a6_imafdc_sv39}

flat=$here/cva6_flat.sv
vhd=$here/cva6_elab.vhd
json=$here/cva6_verilate.json.dir/Vcva6.tree.json

# Skip flatten if the flat file is newer than every input we use.
need_flatten=1
if [ -f "$flat" ] && [ "$flat" -nt "$cva6_root/core/Flist.cva6" ]; then
    need_flatten=0
fi

if [ "$need_flatten" = "1" ]; then
    echo "[flatten] verilator -E -P → $flat"
    cd "$cva6_root"
    # Verible parses single-pass and needs every package defined
    # before the modules that import it. Order: global header macros
    # (genesysii, registers) → external packages → -f Flist.cva6
    # (which owns the in-tree package + module dep order itself).
    # -E + --pp-comments: keeps // pragma translate_off markers (so
    # we can sed-strip the assertion blocks below). Without
    # --pp-comments verilator silently drops every // comment.
    verilator -E --pp-comments --no-timing \
        -Wno-fatal -Wno-WIDTHTRUNC -Wno-WIDTHEXPAND -Wno-UNOPTFLAT \
        -Wno-CASEINCOMPLETE -Wno-CASEX -Wno-PINMISSING -Wno-IMPLICIT \
        -Wno-DECLFILENAME -Wno-MULTITOP -Wno-UNSIGNED -Wno-CMPCONST \
        -Wno-LATCH -Wno-style \
        --top-module cva6 \
        "$cva6_root/corev_apu/fpga/src/genesysii.svh" \
        "$cva6_root/vendor/pulp-platform/common_cells/include/common_cells/registers.svh" \
        "$cva6_root/corev_apu/tb/ariane_axi_pkg.sv" \
        "$cva6_root/corev_apu/tb/axi_intf.sv" \
        "$cva6_root/corev_apu/register_interface/src/reg_intf.sv" \
        "$cva6_root/corev_apu/tb/ariane_soc_pkg.sv" \
        "$cva6_root/corev_apu/riscv-dbg/src/dm_pkg.sv" \
        "$cva6_root/corev_apu/tb/ariane_axi_soc_pkg.sv" \
        "$cva6_root/corev_apu/fpga/src/heavyhash/keccak_pkg.sv" \
        "$cva6_root/corev_apu/fpga/src/heavyhash/heavyhash_pkg.sv" \
        -f core/Flist.cva6 \
        > "$flat" 2> "$flat.err"
    cd "$here"
    # Strip everything between `pragma translate_off` and
    # `pragma translate_on` — Verilator -E preserves these comment
    # pragmas (they're synth-tool directives), but Verible's parser
    # then sees the embedded `assert property` etc. and bombs. The
    # synthesis flow's behaviour is: those blocks don't exist.
    sed -i -E -e '/\/\/[[:space:]]*pragma[[:space:]]+translate_off/,/\/\/[[:space:]]*pragma[[:space:]]+translate_on/d' \
                -e '/^`line/d' "$flat"
    # Strip every assert/assume/cover property block too — synth tools
    # ignore them, our FF analysis doesn't care, and the grammar
    # doesn't yet handle the property-spec sub-language.
    awk '
      function is_assert(s) {
        return match(s, /^[[:space:]]*([A-Za-z_][A-Za-z0-9_]*[[:space:]]*:[[:space:]]*)?(assert|assume|cover|expect)[[:space:]]+property([^A-Za-z0-9_]|$)/)
      }
      function is_orphan_label(s) {
        return match(s, /^[[:space:]]*[A-Za-z_][A-Za-z0-9_]*[[:space:]]*:[[:space:]]*$/)
      }
      function is_property_decl(s) {
        return match(s, /^[[:space:]]*(property|sequence)[[:space:]]+[A-Za-z_]/)
      }
      function is_endproperty(s) {
        return match(s, /^[[:space:]]*end(property|sequence)([^A-Za-z0-9_]|$)/)
      }
      BEGIN { in_a = 0; in_p = 0; held = "" }
      {
        if (in_p) { if (is_endproperty($0)) in_p = 0; next }
        if (in_a) { if (match($0, /;[[:space:]]*$/)) in_a = 0; next }
        if (held != "") {
          if (is_assert($0)) { held = ""; in_a = 1; next }
          print held; held = ""
        }
        if (is_orphan_label($0))  { held = $0; next }
        if (is_assert($0))        { in_a = 1; next }
        if (is_property_decl($0)) { in_p = 1; next }
        print
      }
      END { if (held != "") print held }
    ' "$flat" > "$flat.tmp" && mv "$flat.tmp" "$flat"
    echo "[flatten] $(wc -l < "$flat") lines, $(stat -c%s "$flat") bytes"
fi

export MITER_VERIBLE_FILES=$flat
export MITER_VERIBLE_TOP=cva6

echo "[diff] running test_cva6_ff_diff.exe"
exec "$repo/_build/default/test_cva6_ff_diff.exe" "$vhd" "$json" "$@"
