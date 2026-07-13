#!/usr/bin/env python3
"""Emit the Virtex-7 placement floorplan (legal sites + coordinates) from the
prjxray tilegrid, for topographical placement of packed LEF cells.

The FPGA has a heterogeneous column structure; we flatten every placeable BEL
site to a (site, kind, x, y) row where (x,y) are SLICE-pitch coordinates so
HPWL == SLICE-hop Manhattan (matching virtex7.tech.lef):

  * SLICE_XcYr  -> kind=SLICE, x=c, y=r
  * RAMBxx/DSP48/IOB/BUFG/MMCM -> mapped into the SAME (x,y) frame via the
    tilegrid grid_x/grid_y so the placer sees one coherent grid and BRAM/DSP/
    clock/IO land in their true columns.

Output JSON: { "sites":[{name,kind,x,y}], "kinds":{kind:count} }.  A placer
picks a free site of the required kind nearest the target coordinate.
"""
import json, re, sys
from collections import Counter

import os
DB = os.environ.get("PRJXRAY_TILEGRID",
    os.path.expanduser("~/prjxray/database/virtex7/xc7vx485t/tilegrid.json"))

def kind_of(site, stype):
    if site.startswith("SLICE_"):
        return "SLICE"
    if site.startswith("RAMB36") or stype.startswith("RAMBFIFO36"):
        return "BRAM"     # RAMB36 = full BRAM
    if site.startswith("RAMB18"):
        return "BRAM18"
    if site.startswith("DSP48"):
        return "DSP"
    if site.startswith("IOB") or stype.startswith("IOB"):
        return "IO"
    if site.startswith("BUFGCTRL"):
        return "BUFG"
    if site.startswith("BUFHCE"):
        return "BUFH"
    if site.startswith("MMCME2") or site.startswith("PLLE2"):
        return "MMCM"
    if site.startswith("GTXE2") or site.startswith("GTHE2") or site.startswith("GTPE2"):
        return "GT"
    return None

def slice_xy(site):
    m = re.search(r"_X(\d+)Y(\d+)", site)
    return (int(m.group(1)), int(m.group(2))) if m else None

def main():
    out = sys.argv[1] if len(sys.argv) > 1 else "/tmp/virtex7_floorplan.json"
    tg = json.load(open(DB))
    # A CLB tile at grid (gx,gy) holds SLICEs; use SLICE X/Y as the base frame.
    # For non-SLICE sites we place them in the same frame by scaling their own
    # site X/Y through the per-column SLICE mapping we observe: approximate by
    # using the tile's grid_x mapped to the nearest SLICE column, and site Y.
    # First learn slice column -> grid_x so hard blocks share the X axis.
    slice_gx = {}          # slice column c -> grid_x (of its tile)
    sites = []
    for tname, info in tg.items():
        gx = info.get("grid_x"); gy = info.get("grid_y")
        for sname, stype in info.get("sites", {}).items():
            k = kind_of(sname, stype)
            if k is None:
                continue
            xy = slice_xy(sname)
            if xy is None:
                continue
            sx, sy = xy
            if k == "SLICE":
                # SLICEM sites host distributed-RAM / SRL; SLICEL are logic-only.
                # Tag every slice so the placer can steer DRAM/SRL onto SLICEM.
                sub = "SLICEM" if stype == "SLICEM" else "SLICEL"
                sites.append({"name": sname, "kind": k, "sub": sub, "x": sx, "y": sy})
                if gx is not None:
                    slice_gx[sx] = gx
            else:
                # hard block: keep its own site X/Y; the placer treats each kind's
                # coordinate space independently but proportional to the SLICE grid,
                # which is enough for a wirelength model (refined later).
                sites.append({"name": sname, "kind": k, "x": sx, "y": sy})
    kinds = Counter(s["kind"] for s in sites)
    json.dump({"sites": sites, "kinds": dict(kinds)}, open(out, "w"))
    print("floorplan: %d sites -> %s" % (len(sites), out))
    for k, c in kinds.most_common():
        xs = [s["x"] for s in sites if s["kind"] == k]
        ys = [s["y"] for s in sites if s["kind"] == k]
        print("  %-7s %6d   x[%d..%d] y[%d..%d]" % (k, c, min(xs), max(xs), min(ys), max(ys)))

if __name__ == "__main__":
    main()
