#!/bin/bash
# Sweep the test_placement_timing predictor over every picosoc 6_final.def
# that exists in $HOME/OpenROAD-flow-scripts/flow/results/nangate45/picosoc/
# and compare against the matching 6_finish.rpt's WNS.
#
# Output: TSV table to stdout.  Columns:
#   variant pred_arr_ps pred_wire_ps pred_total_ps meas_period_ps meas_wns_ps
#   achieved_path_ps ratio_meas_over_pred
#
# Required ORFS files per variant:
#   results/nangate45/picosoc/<variant>/6_final.def
#   reports/nangate45/picosoc/<variant>/6_finish.rpt

set -euo pipefail
LIB=${LIB:-$HOME/OpenROAD-flow-scripts/flow/platforms/nangate45/lib/NangateOpenCellLibrary_typical.lib}
PRED=${PRED:-$HOME/System-Verilog-decompiler/_build/default/test_placement_timing.exe}
ROOT=${ROOT:-$HOME/OpenROAD-flow-scripts/flow/results/nangate45/picosoc}
RPT_ROOT=${RPT_ROOT:-$HOME/OpenROAD-flow-scripts/flow/reports/nangate45/picosoc}
SDC_PERIOD_NS=${SDC_PERIOD_NS:-1.1}

printf "%-22s\t%10s\t%10s\t%10s\t%10s\t%10s\t%10s\t%6s\n" \
  variant pred_arr_ps pred_wire_ps pred_tot_ps period_ps wns_ps path_ps ratio
for d in "$ROOT"/*/; do
  v=$(basename "$d")
  def="$d/6_final.def"
  rpt="$RPT_ROOT/$v/6_finish.rpt"
  [ -f "$def" ] || continue
  [ -f "$rpt" ] || continue
  out=$("$PRED" "$def" "$LIB" 2>/dev/null || true)
  arr=$(awk '/Real Liberty/{flag=1} flag && /worst arr/{print $4; exit}' <<<"$out")
  wire=$(awk '/total wire/{print $4; exit}' <<<"$out")
  wns_ns=$(awk '/^wns max/{print $3}' "$rpt")
  if [ -z "$arr" ] || [ -z "$wns_ns" ]; then continue; fi
  period_ps=$(python3 -c "print(int($SDC_PERIOD_NS*1000))")
  wns_ps=$(python3 -c "print(int(round(float('$wns_ns')*1000)))")
  path_ps=$(python3 -c "print($period_ps - $wns_ps)")
  total_pred=$(python3 -c "print(float('$arr') + float('$wire'))")
  ratio=$(python3 -c "print(round($path_ps / $total_pred, 3))")
  printf "%-22s\t%10.3f\t%10.3f\t%10.3f\t%10d\t%10d\t%10d\t%6.3f\n" \
    "$v" "$arr" "$wire" "$total_pred" "$period_ps" "$wns_ps" "$path_ps" "$ratio"
done
