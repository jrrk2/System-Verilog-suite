#!/usr/bin/env python3
"""Cell census of yosys-JSON netlists with hierarchy expansion: a cell whose
type is another module present in the (merged) JSON is expanded into that
module's census, so a MAC with 9 mul8 instances is counted as 9 copies.
Usage: count.py a.json [b.json ...]   (all files are merged first)"""
import json, sys, collections

def bucket(t):
    if t.startswith("LUT"): return "LUT"
    if t.startswith("FD"): return "FF"
    if t.startswith("MUXF"): return "MUXF"
    if t.startswith("SRL"): return "SRL"
    if t.startswith("RAMB"): return "RAMB"
    return t

mods = {}
for path in sys.argv[1:]:
    for n, m in json.load(open(path))["modules"].items():
        a = m.get("attributes", {})
        # yosys write_json carries the whole cell library as blackbox modules
        if "blackbox" in a or "whitebox" in a:
            continue
        mods[n] = m

memo = {}
def census(name, depth=0):
    if name in memo: return memo[name]
    if depth > 64: raise SystemExit(f"instance recursion at {name}")
    c = collections.Counter()
    for cell in mods[name]["cells"].values():
        t = cell["type"]
        if t == "$scopeinfo":
            continue
        if t in mods and t != name:
            c.update(census(t, depth + 1))
        else:
            c[bucket(t)] += 1
    memo[name] = c
    return c

instantiated = {cell["type"] for m in mods.values() for cell in m["cells"].values()}
tops = [n for n in mods if n not in instantiated] or list(mods)
total = collections.Counter()
for t in tops: total.update(census(t))
print(" ".join(f"{k}={v}" for k, v in sorted(total.items())))
