#!/bin/bash
# End-to-end side-by-side comparator: runs the floorplanner AND Vivado
# in -rtl elaboration mode over the same top-down tree, then joins
# their per-module shape estimates into one TSV.
#
# Both tools start at vc707_microgpt_eth and propagate parameter
# overrides down — OOC mode would lose those overrides and produce
# different specialisations than the real build sees.
#
# Output:
#   floorplan.tsv          our prediction (from test_floorplan)
#   vivado_rtl_cells.tsv   raw Vivado cell dump
#   vivado_per_module.tsv  aggregated per-module Vivado counts
#   compare.tsv            joined side-by-side
#
# Usage:
#   bash run_compare.sh                       # int8 default
#   USE_BFP=1 BFP_STREAM=1 bash run_compare.sh # block-FP + streaming
#
# Required: verilator on PATH, vivado on PATH, opam env active.

set -eu
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
talos=${TALOS_DIR:-$HOME/TALOS-V2/rtl/vc707}
top=${TOP:-vc707_microgpt_eth}

mkdir -p "$here/compare_work"
cd "$here/compare_work"

# ---------------------------------------------------------------- 1/3
echo "[compare] 1/3 — floorplan prediction (us)"
bash "$here/flatten_talos_vc707.sh"
"$repo/_build/default/test_floorplan.exe" "$top" \
    "$here/talos_vc707_flat.sv" \
    > "$here/compare_work/floorplan.tsv" 2> "$here/compare_work/floorplan.summary"
echo "[compare]   wrote floorplan.tsv ($(wc -l < "$here/compare_work/floorplan.tsv") rows)"

# ---------------------------------------------------------------- 2/3
echo "[compare] 2/3 — Vivado synth_design -rtl"
if ! command -v vivado >/dev/null 2>&1; then
    echo "[compare] WARN: vivado not on PATH; skipping Vivado side"
    echo "[compare]       (the floorplan.tsv is still produced)"
    exit 0
fi
cd "$here/compare_work"
VC707_FLAT="$here/talos_vc707_flat.sv" \
VC707_TOP="$top" \
VC707_OUT="$here/compare_work/vivado_rtl_cells.tsv" \
    vivado -nojournal -mode batch \
           -source "$here/vivado_dump_rtl.tcl" \
           -log "$here/compare_work/vivado.log" 2>&1 | tail -20

if [ ! -f "$here/compare_work/vivado_rtl_cells.tsv" ]; then
    echo "[compare] FATAL: Vivado did not produce vivado_rtl_cells.tsv"
    echo "         (check $here/compare_work/vivado.log)"
    exit 1
fi
echo "[compare]   Vivado dumped $(wc -l < "$here/compare_work/vivado_rtl_cells.tsv") rows"

# ---------------------------------------------------------------- 3/3
echo "[compare] 3/3 — aggregate + join"
# Per-module aggregation of Vivado's cell-level dump.
# - mems_v = count of RTL_RAM*/RTL_ROM* cells
# - ffs_v  = sum of (depth × width) over RTL_REG cells
# - muls_v = count of RTL_MULT cells
awk -F'\t' 'BEGIN {
    print "module\tvivado_mems\tvivado_ffs\tvivado_muls"
}
NR > 1 {
    mod = $1; ref = $2; depth = $3 + 0; width = $4 + 0
    if (ref ~ /^RTL_(RAM|ROM)/) mems[mod]++
    else if (ref ~ /^RTL_REG/) ffs[mod] += depth * width
    else if (ref ~ /^RTL_MULT/) muls[mod]++
    seen[mod] = 1
}
END {
    for (mod in seen)
        printf "%s\t%d\t%d\t%d\n",
            mod, (mems[mod] ? mems[mod] : 0),
                 (ffs[mod]  ? ffs[mod]  : 0),
                 (muls[mod] ? muls[mod] : 0)
}' "$here/compare_work/vivado_rtl_cells.tsv" \
   | sort > "$here/compare_work/vivado_per_module.tsv"

# Join floorplan.tsv ⨝ vivado_per_module.tsv on BASE module name.
# Both tools name specialisations differently (ours: `__P1V1_P2V2...`;
# Vivado: bare base or expression literals), so we aggregate by the
# substring before the first `__` and sum.  Lossy when one base has
# multiple specialisations, but the per-base totals still let us
# diff "did the elaborators see the same things".
awk -F'\t' '
function base(m,  i) {
    i = index(m, "__"); return (i > 0) ? substr(m, 1, i-1) : m
}
NR==FNR && FNR==1 { next }   # skip floorplan header
NR==FNR {
    # our columns: module source mems ramb18 ramb36 lutram dsps ffs blowups
    b = base($1)
    ours_mems[b] += $3; ours_ffs[b] += $8; ours_dsps[b] += $7
    seen_ours[b] = 1
    next
}
FNR==1 { next }    # skip vivado header
{
    b = base($1)
    vivado_mems[b] += $2; vivado_ffs[b] += $3; vivado_muls[b] += $4
    seen_vivado[b] = 1
}
END {
    print "module\tours_mems\tvivado_mems\tΔmems\tours_ffs\tvivado_ffs\tΔffs\tours_dsps\tvivado_muls\tΔdsps"
    for (m in seen_ours) seen[m] = 1
    for (m in seen_vivado) seen[m] = 1
    n = 0; for (m in seen) keys[n++] = m
    asort(keys)
    for (i = 0; i < n; i++) {
        m = keys[i]
        om = (m in ours_mems) ? ours_mems[m] : 0
        vm = (m in vivado_mems) ? vivado_mems[m] : 0
        of = (m in ours_ffs) ? ours_ffs[m] : 0
        vf = (m in vivado_ffs) ? vivado_ffs[m] : 0
        od = (m in ours_dsps) ? ours_dsps[m] : 0
        vd = (m in vivado_muls) ? vivado_muls[m] : 0
        printf "%s\t%d\t%d\t%+d\t%d\t%d\t%+d\t%d\t%d\t%+d\n",
            m, om, vm, (vm - om), of, vf, (vf - of), od, vd, (vd - od)
    }
}' "$here/compare_work/floorplan.tsv" "$here/compare_work/vivado_per_module.tsv" \
   > "$here/compare_work/compare.tsv"

echo "[compare] wrote $here/compare_work/compare.tsv"
echo
echo "Top 15 modules by |Δffs|:"
{ head -1 "$here/compare_work/compare.tsv"
  tail -n +2 "$here/compare_work/compare.tsv" | \
      awk -F'\t' '{print ($7<0?-$7:$7)"\t"$0}' | sort -rn | head -15 | cut -f2-
} | column -t -s$'\t'
