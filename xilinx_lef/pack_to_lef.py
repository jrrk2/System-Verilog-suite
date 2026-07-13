#!/usr/bin/env python3
"""Recognition packer: map a structural Xilinx-primitive netlist (yosys JSON,
the same shape bir_to_nextpnr_json emits) onto the virtex7 LEF cell configs.

This is the step nextpnr's soup-of-cells placer skips: recognise the STRUCTURE
(a CARRY4 + its S-LUTs + sum FFs is ONE carry slice that chains vertically; a
LUT+FF is a logic cell; MMCM/BUFG/IO are dedicated sites) and emit packed cells
whose LEF pins carry exactly the nets that CROSS the pack boundary.  A route-
length-aware placer then moves the packed cells; nextpnr only legalises.

Output: a packed netlist (JSON) = { cells:[{name,lef,conns:{pin:net}}], nets:{net:[..]} }
plus a recognition report.  Consumed next by the floorplan/placement step.
"""
import json, sys
from collections import defaultdict, Counter

def load_top(path):
    j = json.load(open(path))
    mods = j["modules"]
    top = mods.get("cnt_top") or max(mods.values(), key=lambda m: len(m.get("cells", {})))
    return top

def net_of(bits):
    # single-bit connection -> the net id (int) or const string
    return bits[0] if bits else None

def main():
    path = sys.argv[1] if len(sys.argv) > 1 else "/tmp/cntwiz.json"
    out = sys.argv[2] if len(sys.argv) > 2 else "/tmp/cntwiz_packed.json"
    top = load_top(path)
    cells = top["cells"]

    # bit -> driver cell (output pin), and bit -> list of (cell,pin) sinks
    drv = {}
    sinks = defaultdict(list)
    OUT = {"O", "CO", "Q", "OUT", "CLKOUT0", "LOCKED", "CLKFBOUT"}
    for cn, c in cells.items():
        for p, bits in c["connections"].items():
            for i, b in enumerate(bits):
                if not isinstance(b, int):
                    continue
                # heuristic driver detection by well-known output pin names
                if p in OUT or p.startswith("O") or p.startswith("CLKOUT"):
                    drv[b] = (cn, p, i)
                else:
                    sinks[b].append((cn, p, i))

    packed = {}          # packed-cell name -> {lef, conns:{pin:net}}
    absorbed = set()     # primitive cell names consumed into a pack
    report = Counter()

    def bit(cn, pin, idx=0):
        b = cells[cn]["connections"].get(pin, [])
        return b[idx] if idx < len(b) else None

    # ---- 1. CARRY4 chains -> SLICE_CARRY (absorb S-LUTs + sum FFs) ----------
    for cn, c in list(cells.items()):
        if c["type"] != "CARRY4":
            continue
        conns = {}
        conns["CI"] = bit(cn, "CI")
        conns["CYINIT"] = bit(cn, "CYINIT")
        co = cells[cn]["connections"].get("CO", [])       # chain out = CO[3]
        conns["CO"] = co[3] if len(co) > 3 else (co[-1] if co else None)
        absorbed.add(cn); report["CARRY4->SLICE_CARRY"] += 1
        for i in range(4):
            s_bit = bit(cn, "S", i)
            di_bit = bit(cn, "DI", i)
            o_bit = bit(cn, "O", i)
            conns["S%d" % i] = s_bit
            conns["DI%d" % i] = di_bit
            conns["O%d" % i] = o_bit
            # absorb an S-LUT: a LUT whose O drives THIS S[i]
            if s_bit in drv:
                dn, dp, _ = drv[s_bit]
                if cells[dn]["type"].startswith("LUT") and dn not in absorbed:
                    absorbed.add(dn); report["S-LUT absorbed"] += 1
                    # the LUT's real inputs become the slice's S-lane inputs
                    for lp, lb in cells[dn]["connections"].items():
                        if lp != "O" and lb:
                            conns["S%d_%s" % (i, lp)] = lb[0]
            # absorb the sum FF: an FDRE whose D == this O[i]
            for sc, sp, _ in sinks.get(o_bit, []):
                if cells[sc]["type"].startswith("FD") and sp == "D" and sc not in absorbed:
                    absorbed.add(sc); report["sum-FF absorbed"] += 1
                    ff = cells[sc]["connections"]
                    conns["Q%d" % i] = ff["Q"][0]
                    conns["CLK"] = ff.get("C", [None])[0]
                    conns["CE"] = ff.get("CE", [None])[0]
                    conns["SR"] = ff.get("R", ff.get("S", [None]))[0]
                    break
        packed[cn.replace("_i_1", "") + "$carry"] = {"lef": "SLICE_CARRY", "conns": conns}

    # ---- 2. dedicated sites: MMCM / BUFG / IO ------------------------------
    IOMAP = {"IBUF": "IOB", "OBUF": "IOB", "IBUFDS": "IOB", "OBUFDS": "IOB",
             "IOBUF": "IOB", "MMCME2_ADV": "MMCM", "PLLE2_ADV": "MMCM",
             "BUFG": "BUFG", "BUFGCTRL": "BUFG", "BUFH": "BUFH"}
    for cn, c in cells.items():
        if cn in absorbed or c["type"] in ("GND", "VCC"):
            continue
        lef = IOMAP.get(c["type"])
        if lef:
            conns = {p: net_of(b) for p, b in c["connections"].items() if b}
            packed[cn + "$site"] = {"lef": lef, "conns": conns}
            absorbed.add(cn); report["%s->%s" % (c["type"], lef)] += 1

    # ---- 3. leftover LUT/FF -> SLICE_LOGIC / SLICE_FF ----------------------
    for cn, c in cells.items():
        if cn in absorbed or c["type"] in ("GND", "VCC"):
            continue
        t = c["type"]
        conns = {p: net_of(b) for p, b in c["connections"].items() if b}
        if t.startswith("LUT"):
            packed[cn + "$logic"] = {"lef": "SLICE_LOGIC", "conns": conns}
            report["LUT->SLICE_LOGIC"] += 1
        elif t.startswith("FD"):
            packed[cn + "$ff"] = {"lef": "SLICE_FF", "conns": conns}
            report["FF->SLICE_FF"] += 1
        else:
            packed[cn + "$?"] = {"lef": "UNKNOWN:" + t, "conns": conns}
            report["UNMAPPED " + t] += 1
        absorbed.add(cn)

    # ---- nets among packed cells (for placement HPWL) ---------------------
    nets = defaultdict(list)
    for pn, pc in packed.items():
        for pin, net in pc["conns"].items():
            if isinstance(net, int):
                nets[net].append([pn, pin])

    json.dump({"cells": packed, "nets": nets}, open(out, "w"), indent=1)

    print("=== recognition report (%s) ===" % path)
    for k, v in report.most_common():
        print("  %4d  %s" % (v, k))
    ncarry = sum(1 for c in packed.values() if c["lef"] == "SLICE_CARRY")
    print("packed %d primitives -> %d LEF cells (%d SLICE_CARRY chained via CI/CO)"
          % (len(absorbed), len(packed), ncarry))
    # show the carry chain CI/CO linkage (proves vertical-chain recognition)
    co2cell = {}
    for pn, pc in packed.items():
        if pc["lef"] == "SLICE_CARRY":
            co2cell[pc["conns"].get("CO")] = pn
    print("=== carry chain (CI<-CO links) ===")
    for pn, pc in sorted(packed.items()):
        if pc["lef"] != "SLICE_CARRY":
            continue
        ci = pc["conns"].get("CI")
        prev = co2cell.get(ci, "GND/root")
        print("  %-26s CI<-%s  CO=%s" % (pn, prev, pc["conns"].get("CO")))

if __name__ == "__main__":
    main()
