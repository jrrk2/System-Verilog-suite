#!/usr/bin/env bash
# Validate an fpga_synth yosys-JSON netlist through nextpnr-xilinx.
#
# Proves the suite's primitive netlist (LUTk / FDRE / FDCE / IBUF / OBUF
# / BUFG, emitted by fpga_emit.write_yosys_json) is accepted, placed and
# routed by nextpnr-xilinx on a real part, with NO yosys in the loop.
#
# Default design: test_artyz7 (28-bit counter on Arty Z7-20 / xc7z020).
#
#   ./nextpnr_validate.sh                 # build JSON + run on xc7z020
#   JSON=foo.json XDC=foo.xdc ./nextpnr_validate.sh
set -euo pipefail

NEXTPNR=${NEXTPNR:-$HOME/nextpnr-xilinx/build/nextpnr-xilinx}
CHIPDB=${CHIPDB:-$HOME/nextpnr-xilinx/xilinx/xc7z020.bin}
DEVICE=${DEVICE:-xc7z020clg400-1}
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SUITE="$(cd "$HERE/.." && pwd)"
OUT=${OUT:-/tmp/fpga_synth_pnr}
mkdir -p "$OUT"

JSON=${JSON:-/tmp/artyz7_top.json}
XDC=${XDC:-$HERE/artyz7.xdc}

# Build the chipdb on first use (needs the bundled prjxray-db + meta).
if [[ ! -f "$CHIPDB" ]]; then
  echo ">> building chipdb $CHIPDB (one-time)"
  ND="$HOME/nextpnr-xilinx"
  python3 "$ND/xilinx/python/bbaexport.py" --device "$DEVICE" \
    --bba "$ND/xilinx/$(basename "${CHIPDB%.bin}").bba"
  "$ND/build/bbasm" --l "$ND/xilinx/$(basename "${CHIPDB%.bin}").bba" "$CHIPDB"
fi

# Build the default design's JSON if it is missing.
if [[ "$JSON" == /tmp/artyz7_top.json && ! -f "$JSON" ]]; then
  echo ">> generating $JSON (test_artyz7)"
  ( cd "$SUITE" && eval "$(opam env --switch=5.3.0)" \
      && dune exec fpga_synth/test_artyz7.exe )
fi

echo ">> nextpnr-xilinx: $JSON  ($DEVICE)"
"$NEXTPNR" \
  --chipdb "$CHIPDB" \
  --xdc "$XDC" \
  --json "$JSON" \
  --write "$OUT/routed.json" \
  --fasm "$OUT/top.fasm"

echo
echo ">> PASS: placed & routed -> $OUT/top.fasm ($(grep -cvE '^\s*#|^\s*$' "$OUT/top.fasm") FASM features)"

# Optional FASM -> bitstream via prjxray (MAKE_BIT=1).  MUST use the same
# prjxray DB the chipdb was built from, else FASM features mismap.
if [[ "${MAKE_BIT:-0}" == 1 ]]; then
  PRJXRAY=${PRJXRAY:-$HOME/prjxray}
  PRJXRAY_DB=${PRJXRAY_DB:-$HOME/nextpnr-xilinx/xilinx/external/prjxray-db}
  FAMILY=${FAMILY:-artix7}
  PART=${PART:-xc7a50tcsg324-1}
  DBF="$PRJXRAY_DB/$FAMILY"
  echo ">> fasm2frames ($PART, db=$DBF)"
  "$PRJXRAY/env/bin/python3" "$PRJXRAY/utils/fasm2frames.py" \
    --part "$PART" --db-root "$DBF" "$OUT/top.fasm" "$OUT/top.frames"
  echo ">> xc7frames2bit"
  "$PRJXRAY/build/tools/xc7frames2bit" \
    --part_file "$DBF/$PART/part.yaml" --part_name "$PART" \
    --frm_file "$OUT/top.frames" --output_file "$OUT/top.bit"
  echo ">> BITSTREAM: $OUT/top.bit ($(stat -c%s "$OUT/top.bit") bytes)"
fi
