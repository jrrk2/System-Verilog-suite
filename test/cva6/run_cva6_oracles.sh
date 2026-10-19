#!/bin/bash
# The three oracles over a selection of cva6 modules, from a verilator
# elaboration of the whole core:
#   1. verilator --json-only on $CVA6 (top cva6, cv64a6_imafdc_sv39) -> the
#      suite's verilator front end -> one behavioural Verilog file per
#      specialised module (svd.write_verilog), each with its children
#      (extract.py)
#   2. roundtrip: the verilator tree mitered against verible's re-read of that
#      Verilog (test/qor/roundtrip.sh's check, done in-process here since the
#      JSON parse is the slow part)
#   3. test/qor/run_qor.sh over the modules (strict netlist miter, three-way
#      vote, yosys reference; children from the BIR)
#   4. test/qor/gl_cosim.sh over the modules (unisim co-simulation)
#   test/cva6/run_cva6_oracles.sh [module-regex ...]     (default: the list below)
#   CVA6=$HOME/cva6  OUT=$HOME/svs-gatesim/cva6  SV_SUITE=<exe>  CVA6_CYCLES=5000
set -u
HERE=$(cd "$(dirname "$0")" && pwd); ROOT=$(cd "$HERE/../.." && pwd)
CVA6=${CVA6:-$HOME/cva6}; OUT=${OUT:-$HOME/svs-gatesim/cva6}; mkdir -p "$OUT/vlt" "$OUT/mods" "$OUT/rt"
SV=${SV_SUITE:-$ROOT/_build/default/sv_suite.exe}; export SV_SUITE=$SV TMPDIR=$OUT/tmp; mkdir -p "$TMPDIR"
export CVA6_REPO_DIR=$CVA6 HPDCACHE_DIR=$CVA6/core/cache_subsystem/hpdcache TARGET_CFG=${TARGET_CFG:-cv64a6_imafdc_sv39}
SEL=("$@"); [ ${#SEL[@]} -eq 0 ] && SEL=('alu__.*' 'branch_unit__.*' 'compressed_decoder__.*' 'multiplier__.*' 'serdiv__.*' 'mult__.*' 'cva6_fifo_v3__D2_DB4' 'cva6_fifo_v3__DB4_Tz45' 'amo_buffer__.*' 'lsu_bypass__.*' 'csr_buffer__.*' 'store_buffer__.*' 'scoreboard__.*' 'ariane_regfile__Cz1_D40_N3' 'ras__.*' 'btb__.*' 'bht__.*' 'instr_scan__.*' 'instr_realign__.*' 'instr_queue__.*' 'commit_stage__.*' 'decoder__.*' 'controller__.*' 'issue_read_operands__.*' 'lzc__W4' 'popcount__I2' 'rr_arb_tree__N4_D1' 'load_unit__.*' 'store_unit__.*' 'pmp__.*' 'cva6_tlb__.*')

# 1. elaborate once, dump once
if [ ! -s "$OUT/vlt/cva6.json" ] || [ "$CVA6/core/Flist.cva6" -nt "$OUT/vlt/cva6.json" ]; then
  (cd "$CVA6" && verilator --no-timing --json-only --json-only-output "$OUT/vlt/cva6.json" \
    -Wno-fatal -Wno-WIDTHTRUNC -Wno-WIDTHEXPAND -Wno-UNOPTFLAT -Wno-CASEINCOMPLETE -Wno-CASEX -Wno-PINMISSING -Wno-IMPLICIT \
    -Wno-DECLFILENAME -Wno-MULTITOP -Wno-UNSIGNED -Wno-CMPCONST -Wno-LATCH -Wno-style \
    +define+SYNTHESIS=1 --top-module cva6 --unroll-count 256 -f core/Flist.cva6 \
    corev_apu/tb/ariane_axi_pkg.sv corev_apu/tb/axi_intf.sv corev_apu/register_interface/src/reg_intf.sv corev_apu/tb/ariane_soc_pkg.sv corev_apu/riscv-dbg/src/dm_pkg.sv corev_apu/tb/ariane_axi_soc_pkg.sv \
    corev_apu/fpga/src/genesysii.svh vendor/pulp-platform/common_cells/include/common_cells/registers.svh \
    --Mdir "$OUT/vlt/obj") > "$OUT/vlt/verilate.log" 2>&1 || { echo "verilator failed: $OUT/vlt/verilate.log"; exit 2; }
fi
cat > "$OUT/vlt/dump.lua" <<LUA
prog = svd.parse("verilator", "cva6", {"$OUT/vlt/cva6.json"})
svd.write_verilog(prog, "$OUT/vlt/cva6_vlt.v")
LUA
(cd "$ROOT" && SVS_CHECK_IR=${SVS_CHECK_IR:-1} "$SV" script "$OUT/vlt/dump.lua") > "$OUT/vlt/dump.log" 2>&1 || { echo "dump failed: $OUT/vlt/dump.log"; exit 2; }
grep "^\[check_ir\]" "$OUT/vlt/dump.log" | tail -1
rm -f "$OUT"/mods/*.v; python3 -I "$HERE/extract.py" "$OUT/vlt/cva6_vlt.v" "$OUT/mods" "${SEL[@]}" > "$OUT/mods/children.txt"
{ printf '# name\ttop\tmods\tfiles\n'; while read -r top kids; do printf '%s\t%s\tauto\t%s/mods/%s.v\n' "${top%%__*}" "$top" "$OUT" "$top"; done < "$OUT/mods/children.txt"; } > "$OUT/designs.tsv"
echo "modules: $(wc -l < "$OUT/mods/children.txt")"

# 2. roundtrip (one script per module, 4 at a time: each re-parses the JSON)
: > "$OUT/roundtrip.txt"
while read -r top kids; do printf 'a = svd.parse("verilator", "cva6", {"%s/vlt/cva6.json"})\nb = svd.parse("verible", "%s", {"%s/mods/%s.v"})\nprint("ROUNDTRIP %s: " .. svd.miter_hier(a, b, "%s"))\n' "$OUT" "$top" "$OUT" "$top" "$top" "$top" > "$OUT/rt/$top.lua"; done < "$OUT/mods/children.txt"
ls "$OUT"/rt/*.lua | xargs -P "${CVA6_JOBS:-4}" -I{} sh -c 'n=$(basename {} .lua); cd '"$ROOT"'; Z3_MITER_TIMEOUT_MS=120000 timeout 3600 '"$SV"' script {} > '"$OUT"'/rt/$n.log 2>&1; r=$(grep -m1 "^HIER-SUMMARY" '"$OUT"'/rt/$n.log | sed "s/HIER-SUMMARY //"); echo "$n	${r:-FAILED $(grep -m1 "lua:" '"$OUT"'/rt/$n.log | cut -c1-120)}" >> '"$OUT"'/roundtrip.txt'
echo "roundtrip: $(grep -c "whole design EQUIVALENT" "$OUT/roundtrip.txt")/$(wc -l < "$OUT/roundtrip.txt") EQUIVALENT"

# 3. QoR harness (its own roundtrip column covers verible/yosys/verilator on the dump)
QOR_SVS_TIMEOUT=${QOR_SVS_TIMEOUT:-1500} QOR_DESIGNS="$OUT/designs.tsv" QOR_OUT="$OUT/qor" "$ROOT/test/qor/run_qor.sh" > "$OUT/qor.log" 2>&1
column -t -s $'\t' "$OUT/qor/results.tsv"

# 4. gate-level
: > "$OUT/gl_summary.txt"
while read -r top kids; do
  r=$(GL_OUT="$OUT/gl/$top" timeout "${CVA6_GL_TIMEOUT:-1500}" "$ROOT/test/qor/gl_cosim.sh" "$OUT/mods/$top.v" "$top" "${CVA6_CYCLES:-5000}" 2>&1 | grep -m1 "^GL-COSIM")
  echo "${top%%__*}	${r:-TIMEOUT/ERR}" >> "$OUT/gl_summary.txt"; rm -rf "$OUT/gl/$top/obj"
done < "$OUT/mods/children.txt"
cat "$OUT/gl_summary.txt"
