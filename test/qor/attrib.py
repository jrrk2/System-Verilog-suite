#!/usr/bin/env python3
"""LUT / CARRY4 attribution for a mapped netlist JSON (write_mapped_json /
yosys write_json): each combinational cell is charged to the sequential
sinks (FF D/CE/R pins, RAM/DSP ports) it reaches through combinational
logic, split evenly when it reaches several.  Per-bit register names
(x[3], x__b3_1234) are grouped under the register's base name.

Usage: attrib.py <netlist.json> [rows]   (default 30 rows)"""
import collections, json, re, sys

sys.setrecursionlimit(1000000)
d = json.load(open(sys.argv[1]))
top = next(iter(d["modules"].values())) if len(d["modules"]) == 1 else \
      max(d["modules"].values(), key=lambda m: len(m["cells"]))
cells = top["cells"]
names = {}
for nn, n in top.get("netnames", {}).items():
    for b in n["bits"]: names.setdefault(b, nn)
sinks = collections.defaultdict(list)
def isout(c, p): return c.get("port_directions", {}).get(p) == "output"
for cn, c in cells.items():
    for p, bits in c["connections"].items():
        if not isout(c, p):
            for b in bits:
                if not isinstance(b, str): sinks[b].append((cn, p))
def comb(t): return t.startswith(("LUT", "CARRY", "MUXF"))
def label(cn, p):
    c = cells[cn]; t = c["type"]
    if t.startswith("FD"):
        nm = names.get(c["connections"].get("Q", [None])[0], cn)
        return f"{t[:4]}:{nm}" + ("" if p == "D" else "." + p)
    return f"{t}:{re.sub(r'_[0-9]+$', '', cn)}.{p}"
memo = {}
def reach(cn):
    if cn in memo: return memo[cn]
    memo[cn] = frozenset(); s = set(); c = cells[cn]
    for p, bits in c["connections"].items():
        if not isout(c, p): continue
        for b in bits:
            if isinstance(b, str): continue
            for sc, sp in sinks[b]:
                if comb(cells[sc]["type"]): s |= reach(sc)
                else: s.add(label(sc, sp))
    memo[cn] = frozenset(s); return memo[cn]
def base(l): return re.sub(r"__b\d+_\d+", "", re.sub(r"(\[\d+\])+", "", l))
lut = collections.Counter(); car = collections.Counter()
for cn, c in cells.items():
    t = c["type"]
    if not comb(t): continue
    r = reach(cn) or frozenset(["<output port / unused>"])
    for l in r:
        if t.startswith("LUT"): lut[base(l)] += 1 / len(r)
        elif t.startswith("CARRY"): car[base(l)] += 1 / len(r)
print(f"LUT {sum(lut.values()):.0f}  CARRY4 {sum(car.values()):.0f}")
print(f"{'LUT':>8}  {'CARRY4':>6}  sink")
for l, v in lut.most_common(int(sys.argv[2]) if len(sys.argv) > 2 else 30):
    print(f"{v:8.1f}  {car[l]:6.1f}  {l}")
