#!/usr/bin/env python3
"""LUT logic depth of a yosys-JSON netlist: the longest chain of LUT / MUXF /
CARRY4 cells between sequential or port boundaries (a cheap, P&R-independent
timing proxy).  Usage: depth.py a.json [b.json ...]"""
import json, sys
sys.setrecursionlimit(1_000_000)

COMB = lambda t: t.startswith("LUT") or t.startswith("MUXF") or t == "CARRY4"

def depth_of(path):
    d = json.load(open(path))
    best = 0
    for m in d["modules"].values():
        cells = m["cells"]
        drv = {}
        for cn, c in cells.items():
            dirs = c.get("port_directions", {})
            for p, bits in c["connections"].items():
                if dirs.get(p) == "output":
                    for b in bits:
                        if isinstance(b, int):
                            drv[b] = cn
        memo = {}
        def depth(cn):
            if cn in memo:
                return memo[cn]
            c = cells[cn]
            if not COMB(c["type"]):
                memo[cn] = 0
                return 0
            memo[cn] = 0          # cycle guard
            dmax = 0
            dirs = c.get("port_directions", {})
            for p, bits in c["connections"].items():
                if dirs.get(p) == "input":
                    for b in bits:
                        if isinstance(b, int) and b in drv:
                            dmax = max(dmax, depth(drv[b]))
            memo[cn] = dmax + 1
            return memo[cn]
        for cn in cells:
            if COMB(cells[cn]["type"]):
                best = max(best, depth(cn))
    return best

for p in sys.argv[1:]:
    print(f"{p}: max_logic_depth={depth_of(p)}")
