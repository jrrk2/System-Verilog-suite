#!/bin/bash
# topo_legalize.sh <netlist.json> <bels.txt> <xdc> <out.bit>
#
# Legalise a TOPOGRAPHICALLY-placed design: stamp the placer's per-primitive
# BEL assignments (bels.txt) as JSON BEL attributes, then let nextpnr-xilinx
# honour them (assign exact sub-BELs it can't override) and ROUTE, then
# prjxray fasm2frames + xc7frames2bit -> bitstream.  nextpnr does NOT place --
# our route-length-aware placement already did.
set -e
JSON=$1; BELS=$2; XDC=$3; OUTBIT=$4
STAMPED=${JSON%.json}_stamped.json
FASM=${OUTBIT%.bit}.fasm
V7=/home/jonathan/v7-johnson-demo
NEXTPNR=$V7/deps/nextpnr-xilinx/build/nextpnr-xilinx
CHIPDB=$V7/deps/nextpnr-xilinx/xilinx/xc7vx485t.bin
PXPY=$V7/deps/prjxray/env/bin/python
PXDB=/home/jonathan/prjxray/database/virtex7
PART=xc7vx485tffg1761-2

# 1. stamp BEL attributes from the placer
python3 - "$JSON" "$BELS" "$STAMPED" <<'PY'
import json, sys
j = json.load(open(sys.argv[1]))
bels = dict(l.rstrip("\n").split("\t") for l in open(sys.argv[2]) if "\t" in l)
mod = max(j["modules"].values(), key=lambda m: len(m.get("cells", {})))
n = 0
for cn, c in mod["cells"].items():
    if cn in bels:
        c.setdefault("attributes", {})["BEL"] = bels[cn]; n += 1
json.dump(j, open(sys.argv[3], "w"))
print("  stamped %d cells with BEL" % n)
PY

# 2. nextpnr: legalise our placement + route (router1 = virtex7-known-good)
flock /tmp/nextpnr.lock env NEXTPNR_ALLOW_CO_5FF_CONTENTION=1 \
  "$NEXTPNR" --router router2 --chipdb "$CHIPDB" --xdc "$XDC" \
    --json "$STAMPED" --fasm "$FASM"

# 3. FASM -> frames -> bitstream
XRAY_ALLOW_MISSING_FEATURES=1 "$PXPY" /home/jonathan/prjxray/utils/fasm2frames.py \
  --db-root "$PXDB" --part "$PART" "$FASM" "${FASM%.fasm}.frames"
/home/jonathan/prjxray/build/tools/xc7frames2bit --part_file "$PXDB/$PART/part.yaml" \
  --part_name "$PART" --frm_file "${FASM%.fasm}.frames" --output_file "$OUTBIT"
echo "  topo bitstream -> $OUTBIT"
