#!/usr/bin/env bash
# Runs the stock ORFS flow on a (platform, design) pair and captures
# the resulting QoR metrics into baseline/results/<platform>/<design>/<date>.json.
#
# Usage:  ./baseline/run_baseline.sh <design> [platform=nangate45] [flow=stock]
#
# Pre-req:  ORFS installed at $HOME/OpenROAD-flow-scripts (env.sh works).
#           The target design must already exist under
#             $HOME/OpenROAD-flow-scripts/flow/designs/$platform/$design/.
#
# This script does *not* re-run ORFS if the metrics from a previous run
# are present and recent — pass --force to override.
set -euo pipefail

DESIGN="${1:-}"
PLATFORM="${2:-nangate45}"
FLOW="${3:-stock}"
ORFS="${ORFS_HOME:-$HOME/OpenROAD-flow-scripts}"
REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)"
OUT_DIR="$REPO/baseline/results/$PLATFORM/$DESIGN"
DATE_TAG="$(date -u +%Y-%m-%d_%H%M%SZ)"
OUT_JSON="$OUT_DIR/${DATE_TAG}_${FLOW}.json"

if [[ -z "$DESIGN" ]]; then
  echo "usage: $0 <design> [platform=nangate45] [flow=stock]" >&2
  exit 2
fi

CONFIG="$ORFS/flow/designs/$PLATFORM/$DESIGN/config.mk"
if [[ ! -f "$CONFIG" ]]; then
  echo "no such design: $CONFIG" >&2
  exit 2
fi

mkdir -p "$OUT_DIR"

echo "=== ORFS run: $PLATFORM / $DESIGN ($FLOW) ==="
cd "$ORFS"
source env.sh
cd flow

# Clean previous results if --force or if metrics absent
METRICS_FILE="$ORFS/flow/logs/$PLATFORM/$DESIGN/base/6_report.json"
if [[ ! -f "$METRICS_FILE" ]]; then
  echo "  (no previous metrics — running flow)"
  make DESIGN_CONFIG="$CONFIG" 2>&1 | tail -8
fi

if [[ ! -f "$METRICS_FILE" ]]; then
  echo "ERROR: ORFS did not produce $METRICS_FILE" >&2
  exit 1
fi

echo
echo "=== capturing metrics into $OUT_JSON ==="
"$REPO/_build/default/baseline/baseline_capture.exe" \
  --orfs "$ORFS" \
  --platform "$PLATFORM" \
  --design "$DESIGN" \
  --flow "$FLOW" \
  --out "$OUT_JSON"

echo
echo "=== summary ==="
python3 -c "
import json, sys
with open('$OUT_JSON') as f:
    b = json.load(f)
m = b['metrics']
print(f'  WNS setup    : {m.get(\"wns_setup_ns\", \"?\")}  ns')
print(f'  fmax         : {m.get(\"fmax_hz\", 0)/1e6:.1f} MHz')
print(f'  cells        : {m.get(\"n_cells\", \"?\")}  ({m.get(\"n_seq_cells\", \"?\")} seq)')
print(f'  stdcell area : {m.get(\"stdcell_area_um2\", \"?\")}  um^2')
print(f'  core area    : {m.get(\"core_area_um2\", \"?\")}  um^2 (placement region)')
print(f'  wirelength   : {m.get(\"wirelength_um\", \"?\")}  um')
print(f'  power        : {m.get(\"power_total_w\", 0)*1000:.3f}  mW')
print(f'  drv vio      : {m.get(\"drv_setup_violations\", 0)} setup, {m.get(\"drv_hold_violations\", 0)} hold')
"
