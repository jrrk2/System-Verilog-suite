#!/bin/bash
# Predictor-only sweep over /home/jonathan/TALOS-V2/rtl/vc707/src/smollm.
#
# For every standalone module in the smollm tree, run synth_orfs_shim
# (synth_pipeline + arch_swap predictor) with a per-module wall-clock
# cap and record:
#   - status (OK / TIMEOUT / FAIL)
#   - wall-clock seconds
#   - cell count emitted
#   - count + total Δ-stages of arch_swap candidates
#
# Output goes to ./smollm_sweep.tsv next to this script.  No ORFS
# layout is required — once layouts exist for any of these modules,
# pair them with the predictor row using picosoc_sweep.sh as a model.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
SHIM=${SHIM:-$HOME/System-Verilog-suite/_build/default/synth_orfs_shim.exe}
PROJ=${PROJ:-$HOME/TALOS-V2/rtl/vc707/src}
SRC=${SRC:-$PROJ/smollm}
TIMEOUT=${TIMEOUT:-180}
OUT=${OUT:-$HERE/smollm_sweep.tsv}

[ -x "$SHIM" ] || { echo "missing $SHIM — dune build first" >&2; exit 1; }
[ -d "$SRC" ]  || { echo "missing $SRC" >&2; exit 1; }

ALL=$(ls "$PROJ"/*.sv 2>/dev/null | grep -v selftest | grep -v vc707_microgpt_eth.sv)
ALL="$ALL $(ls "$SRC"/*.sv 2>/dev/null | grep -v selftest)"

# (module, file_stem) pairs.  Top is the `module <name>` declaration.
MODS=$(grep -E "^module " $ALL \
       | sed -E "s@.*/([^/]+)\.sv:module +([A-Za-z_][A-Za-z0-9_]*).*@\2|\1@" \
       | grep -v selftest)

printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\n" \
  module file status secs cells arch_swaps total_delta > "$OUT"

set +e
total=$(echo "$MODS" | wc -l)
i=0
for entry in $MODS; do
  i=$((i+1))
  top=$(echo "$entry" | cut -d'|' -f1)
  stem=$(echo "$entry" | cut -d'|' -f2)
  outv=$(mktemp -t smollm_synth.XXXXXX.v)
  printf "[%2d/%d] %-32s " "$i" "$total" "$top" >&2
  t0=$(date +%s)
  log=$(timeout "$TIMEOUT" "$SHIM" "$top" "$outv" $ALL 2>&1)
  rc=$?
  t1=$(date +%s)
  secs=$((t1 - t0))
  if [ $rc -eq 124 ]; then
    status=TIMEOUT
  elif [ $rc -ne 0 ]; then
    status=FAIL
  else
    status=OK
  fi
  cells=$(grep -oE '[0-9]+ cells, [0-9]+ child' <<<"$log" | head -1 | awk '{print $1}')
  cells=${cells:-0}
  arch_swaps=$(grep -oE '[0-9]+ swap candidate' <<<"$log" | awk '{print $1}')
  arch_swaps=${arch_swaps:-0}
  total_delta=$(grep -oE 'total stage savings: [0-9]+' <<<"$log" | awk '{print $4}')
  total_delta=${total_delta:-0}
  printf "%-8s cells=%-6s swaps=%-2s Δ=%-6s (%ds)\n" "$status" "$cells" "$arch_swaps" "$total_delta" "$secs" >&2
  printf "%s\t%s\t%s\t%d\t%s\t%s\t%s\n" \
    "$top" "$stem" "$status" "$secs" "$cells" "$arch_swaps" "$total_delta" >> "$OUT"
  rm -f "$outv" "$outv.blocks.json"
done

echo
echo "wrote $OUT" >&2
