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
    if site.startswith("MMCME2"):
        return "MMCM"
    if site.startswith("PLLE2"):
        # NOT "MMCM".  A PLLE2_ADV site has no MMCME2_ADV bel, so lumping the
        # two kinds together lets an MMCME2_ADV cell be placed on a PLL site --
        # which stayed invisible only while place_lef withheld clock stamps.
        # With stamping on it surfaces as
        #   No Bel named 'PLLE2_ADV_X1Y6/MMCME2_ADV' located for this chip
        # A design that really instantiates PLLE2_ADV will now find no site and
        # fail loudly, which is the right way round: no site beats a wrong one.
        return "PLL"
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
                # HARD BLOCK: record the TILE's grid position, not the site index.
                # Every site type numbers itself from 0 independently -- SLICE
                # x is 0..221 but GTXE2_CHANNEL is 0..1, RAMB18 0..14, BUFGCTRL
                # 0..0 -- so emitting the raw site index puts EVERY hard block
                # at the far left of the SLICE frame no matter where it
                # physically is.  GTXE2_CHANNEL_X0Y0 is tile X394, the RIGHT
                # edge of the die, and it was being reported at x=0.
                # The anchor centroid (mean of placed hard blocks) could then
                # only ever land near x=0, which is why the placer packed the
                # whole design into the bottom-LEFT corner while Vivado puts it
                # bottom-RIGHT next to the transceiver -- and why the eth nets
                # crossed the die (34-36 tile hops on the failing TX path).
                # Resolved to SLICE-frame coordinates below, once the
                # slice-column <-> grid mapping is known.
                sites.append({"name": sname, "kind": k, "x": sx, "y": sy,
                              "_gx": gx, "_gy": gy})
    # ---- normalise hard blocks into the SLICE coordinate frame --------------
    # slice_gx maps slice column -> tile grid_x; invert it (and the same for y)
    # so a hard block's TILE position can be expressed as the equivalent slice
    # column/row.  Nearest-key lookup: hard-block columns sit between slice
    # columns, and the tilegrid has gaps (clock/IO columns) where no slice lives.
    slice_gy = {}
    for st in sites:
        if st["kind"] == "SLICE" and "_gy" not in st:
            pass
    # collect gy -> slice y from the same tiles we learned gx from
    for tname, info in tg.items():
        gx = info.get("grid_x"); gy = info.get("grid_y")
        for sname, stype in info.get("sites", {}).items():
            if kind_of(sname, stype) == "SLICE":
                xy = slice_xy(sname)
                if xy and gy is not None:
                    slice_gy[gy] = xy[1]
    gx_keys = sorted(slice_gx_inv := {})
    inv_x = {}
    for sx, gx in slice_gx.items():
        inv_x.setdefault(gx, []).append(sx)
    gx_keys = sorted(inv_x)
    gy_keys = sorted(slice_gy)
    def nearest(keys, v):
        if not keys or v is None:
            return None
        lo, hi = 0, len(keys) - 1
        best = keys[0]
        for k in keys:                      # linear is fine: a few hundred keys
            if abs(k - v) < abs(best - v):
                best = k
        return best
    nfix = 0
    for st in sites:
        if "_gx" not in st:
            continue
        gx, gy = st.pop("_gx"), st.pop("_gy")
        kx = nearest(gx_keys, gx)
        ky = nearest(gy_keys, gy)
        if kx is not None:
            st["x"] = int(round(sum(inv_x[kx]) / len(inv_x[kx])))
            nfix += 1
        if ky is not None:
            st["y"] = slice_gy[ky]
    print("floorplan: normalised %d hard-block site(s) into the SLICE frame" % nfix)

    kinds = Counter(s["kind"] for s in sites)
    json.dump({"sites": sites, "kinds": dict(kinds)}, open(out, "w"))
    print("floorplan: %d sites -> %s" % (len(sites), out))
    for k, c in kinds.most_common():
        xs = [s["x"] for s in sites if s["kind"] == k]
        ys = [s["y"] for s in sites if s["kind"] == k]
        print("  %-7s %6d   x[%d..%d] y[%d..%d]" % (k, c, min(xs), max(xs), min(ys), max(ys)))

if __name__ == "__main__":
    main()
