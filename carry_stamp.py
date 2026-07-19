#!/usr/bin/env python3
"""Complete the carry slices for nextpnr-xilinx.

SVS stamps only the CARRY4 anchor.  nextpnr-xilinx has NO site-level LUT
routethru, so a CARRY4 S-input driven by a non-LUT (a FF's Q via the AX
bypass, or a GND const) cannot bind -- nextpnr's auto-created $LUT routethru
fails.  Emulate what ethsoc/stamp_placement.py does: for every BEL'd CARRY4,
lay out its whole slice explicitly --
  S[k] LUT-driven   -> stamp that LUT to <site>/<slot>6LUT
  S[k] FF/const/ext -> INSERT a real LUT1 buffer at <site>/<slot>6LUT and
                        rewire CARRY4.S[k] through it (identity INIT=2)
  O[k] -> sum FF (FD* with D==O[k]) -> stamp to <site>/<slot>FF
slot: k=0->A, 1->B, 2->C, 3->D.
Usage: carry_stamp.py <ft_json> <bels> <out_json>
"""
import json, sys, os

ft_json, bels_path, out_json = sys.argv[1], sys.argv[2], sys.argv[3]
# CARRY_STAMP_AVOID_CI: don't use the DEDICATED incoming carry (CI = the
# previous CARRY4.CO) as the don't-care local input for a const-forced S/DI LUT.
# Reading it forces that CO onto general routing, where it collides with the
# previous slice's sum O[k] on the single position-k output mux (DMUX
# over-commit -> 128 unroutable nets).  Off by default so the silicon-validated
# pinned placement/golden is bit-identical; the from-source flow sets it.
AVOID_CI = os.environ.get("CARRY_STAMP_AVOID_CI") not in (None, "", "0")
j = json.load(open(ft_json))
mod = max(j["modules"].values(), key=lambda m: len(m.get("cells", {})))
cells = mod["cells"]

# 1) apply the plain BEL stamps from BELS_OUT
bels = {}
for l in open(bels_path):
    if "\t" in l:
        cn, bel = l.rstrip("\n").split("\t")
        bels[cn] = bel
for cn, bel in bels.items():
    if cn in cells:
        cells[cn].setdefault("attributes", {})["BEL"] = bel

SLOT = ["A", "B", "C", "D"]

# GND/VCC-driven net bits (outputs of GND/VCC primitives).  With
# NEXTPNR_JSON_CONST_STRINGS=1 constants ALSO appear as the string bits "0"
# and "1" directly on cell pins; helpers below treat both forms uniformly.
gnd_bits = set()
vcc_bits = set()
for cn, c in cells.items():
    if c["type"] in ("GND", "VCC"):
        tgt = gnd_bits if c["type"] == "GND" else vcc_bits
        for nets in c.get("connections", {}).values():
            for b in nets:
                if isinstance(b, int):
                    tgt.add(b)
def is_gnd_bit(b):
    return b == "0" or (isinstance(b, int) and b in gnd_bits)
def is_vcc_bit(b):
    return b == "1" or (isinstance(b, int) and b in vcc_bits)

# driver of each integer net bit -> (cellname, type, port)
drv = {}
for cn, c in cells.items():
    dirs = c.get("port_directions", {})
    for p, nets in c.get("connections", {}).items():
        if dirs.get(p) == "output":
            for b in nets:
                if isinstance(b, int):
                    drv[b] = (cn, c["type"], p)

# Global fallback net for const-LUT inputs: a const generator (INIT 00/11)
# ignores its input value, so any net routable on GENERAL interconnect works.
# Needed for const-ONLY carries (all S/DI/CI const) that have no local FD net
# -- without it those const LUTs can't be stamped and nextpnr falls back to an
# unplaceable global $PACKER_{GND,VCC}_NET$LUT feedthrough in the dense corner.
# Pick the highest-fanout NON-CLOCK net (clock nets ride dedicated routing and
# won't reach a LUT data pin).
import collections as _cx
_fan = _cx.Counter(); _clk = set()
for _cn, _c in cells.items():
    for _p, _bl in _c.get("connections", {}).items():
        for _b in _bl:
            if isinstance(_b, int):
                _fan[_b] += 1
                if _p in ("CLK", "C", "WCLK"):
                    _clk.add(_b)
GLOBAL_FALLBACK_NET = next((b for b, _ in _fan.most_common() if b not in _clk), None)

# max net id (for fresh routethru nets)
maxbit = 1
for c in cells.values():
    for nets in c.get("connections", {}).values():
        for b in nets:
            if isinstance(b, int) and b > maxbit:
                maxbit = b
for nm, nn in mod.get("netnames", {}).items():
    for b in nn.get("bits", []):
        if isinstance(b, int) and b > maxbit:
            maxbit = b

def is_int(b):
    return isinstance(b, int)

# real SLICE site names for neighbour-bel searches (floorplan + observed BELs)
import re as _re0
_known_sites = set()
try:
    _fp = json.load(open(os.environ.get("CARRY_FLOORPLAN","/tmp/virtex7_floorplan.json")))
    for _s in _fp.get("sites", []):
        if _s.get("name", "").startswith("SLICE_"):
            _known_sites.add(_s["name"])
except Exception:
    pass

def free_neighbour_lut_global(site):
    m = _re0.match(r"SLICE_X(\d+)Y(\d+)$", site)
    if not m:
        return None
    x, y = int(m.group(1)), int(m.group(2))
    for dx, dy in ((1,0),(-1,0),(0,1),(0,-1),(1,1),(-1,1),(1,-1),(-1,-1),(2,0),(-2,0)):
        ns = f"SLICE_X{x+dx}Y{y+dy}"
        if _known_sites and ns not in _known_sites:
            continue
        for sl in SLOT:
            bel = f"{ns}/{sl}6LUT"
            if bel not in occupied:
                return bel
    return None

new_cells = {}
n_buf = 0
n_slut = 0
n_ff = 0
n_di = 0
occupied = {}     # bel -> cellname, to catch slot collisions
for cn, c in cells.items():
    b = c.get("attributes", {}).get("BEL")
    if b:
        occupied[b] = cn

def claim(bel, who):
    if bel in occupied and occupied[bel] != who:
        raise SystemExit(f"SLOT COLLISION {bel}: {occupied[bel]} vs {who}")
    occupied[bel] = who

for cn, c in list(cells.items()):
    if c["type"] != "CARRY4":
        continue
    bel = c.get("attributes", {}).get("BEL", "")
    if not bel.endswith("/CARRY4"):
        continue
    site = bel.split("/")[0]
    S = c["connections"].get("S", [])
    O = c["connections"].get("O", [])
    DI = c["connections"].get("DI", [])
    # a real routable net already present at this slice, for const-0 LUT inputs
    # (a LUT1 INIT=0 fed by a LOCAL net outputs 0 with NO global GND routing)
    def slice_local_net():
        best = None
        for sb2 in S:
            if is_int(sb2) and sb2 not in gnd_bits:
                d2 = drv.get(sb2)
                if d2 and d2[1].startswith("FD"):
                    return sb2                    # FF-driven S bit: ideal
                if AVOID_CI and best is None:
                    best = sb2                    # any other real S input (sum/LUT-driven)
        if AVOID_CI:
            # Prefer a net already routed into this slice (an S input); never CI.
            if best is not None:
                return best
            v = c["connections"].get("CYINIT", [])
            if v and is_int(v[0]) and v[0] not in gnd_bits:
                d = drv.get(v[0])
                if not d or d[1] != "CARRY4":     # skip a carry-driven CYINIT too
                    return v[0]
        else:
            for pn in ("CYINIT", "CI"):
                v = c["connections"].get(pn, [])
                if v and is_int(v[0]) and v[0] not in gnd_bits:
                    return v[0]
        # const-only carry: no local routable net -> global non-clock net
        # (const-LUT output is INIT-forced, input value is a don't-care).
        return GLOBAL_FALLBACK_NET
    # per-slot: the net feeding this slot's 6LUT occupant (for 5LUT input sharing)
    slot_in = [None, None, None, None]
    # --- S inputs ---
    newS = list(S)
    for k, sb in enumerate(S):
        slot6 = f"{site}/{SLOT[k]}6LUT"
        d = drv.get(sb) if is_int(sb) else None
        is_gnd = is_gnd_bit(sb)
        is_vcc = is_vcc_bit(sb)
        if d is not None and d[1].startswith("LUT"):
            # LUT-driven: stamp that S-LUT into THIS carry's slot -- but only if
            # the driving LUT is not already committed to a DIFFERENT slot.  A
            # net feeding S of several carries (e.g. a place_lef $feedthrough
            # relay, or a shared compare term) has one driver LUT; CARRY4.S[k] is
            # a DEDICATED same-slice connection (only this slice's slot-LUT O6
            # reaches it), so a single LUT cannot serve two carries.  When the
            # LUT is already placed elsewhere, fall through to insert a per-slice
            # identity buffer here (fed by the net through general routing) --
            # otherwise the second carry's S input is sourced from a remote slice
            # and nextpnr cannot route it (X..Y1/A6LUT_O6 -> X..Y9/A6LUT_O6).
            cur_bel = cells[d[0]].get("attributes", {}).get("BEL")
            if cur_bel in (None, slot6) and occupied.get(slot6) in (None, d[0]):
                claim(slot6, d[0])
                cells[d[0]].setdefault("attributes", {})["BEL"] = slot6
                ic = cells[d[0]].get("connections", {})
                for pp in ("I0", "I1", "I2", "I3", "I4", "I5"):
                    v = ic.get(pp, [])
                    if v and is_int(v[0]):
                        slot_in[k] = v[0]
                        break
                n_slut += 1
                continue
            # else: shared S driver already placed -> buffer locally (below).
        maxbit += 1
        onet = maxbit
        bufname = f"{cn}$Srt${k}"
        if is_gnd or is_vcc:
            # S=GND/VCC: const generator -- LUT1 fed by a LOCAL net, INIT 00
            # (const-0) or 11 (const-1); needs no global GND/VCC routing.
            # Without the VCC case, S="1" was mis-stamped as const-0 (wrong
            # carry propagate) AND nextpnr still fed the VCC net through an
            # unplaceable $PACKER_VCC_NET$LUT.
            src = slice_local_net()
            if src is None:
                continue
            buf = {"type": "LUT1", "port_directions": {"I0": "input", "O": "output"},
                   "connections": {"I0": [src], "O": [onet]},
                   "parameters": {"INIT": "11" if is_vcc else "00"},
                   "attributes": {"BEL": slot6}}
        else:
            # FF / external -> identity buffer (INIT=2) passing the driver to S
            src = sb
            buf = {"type": "LUT1", "port_directions": {"I0": "input", "O": "output"},
                   "connections": {"I0": [sb], "O": [onet]},
                   "parameters": {"INIT": "10"}, "attributes": {"BEL": slot6}}
        claim(slot6, bufname)
        new_cells[bufname] = buf
        newS[k] = onet
        slot_in[k] = src
        n_buf += 1
    c["connections"]["S"] = newS
    # --- DI=GND: local const-0 at the x5LUT bel driving DI[k] via O5.
    #     Pre-empts nextpnr's global $PACKER_GND_NET$LUT fanout (243/292 DI
    #     bits are GND; those routethrus failed to place in the dense corner).
    #     Fracture legality: the 5LUT shares A-pins with the slot's 6LUT, so
    #     feed it the SAME net as the 6LUT occupant (slot_in[k]).
    newDI = list(DI)
    pending_gnd_di = []
    pending_vcc_di = []
    for k, db in enumerate(DI):
        is_gnd = is_gnd_bit(db)
        is_vcc = is_vcc_bit(db)
        # string-const "0"/"1" (CONST_STRINGS) used to be skipped here, so a
        # VCC-tied DI reached nextpnr as the global VCC net -> unplaceable
        # $PACKER_VCC_NET$LUT feedthrough.  Handle it as a local const 5LUT.
        if not is_int(db) and not is_gnd and not is_vcc:
            continue
        d = drv.get(db) if is_int(db) else None
        if (d is not None and not is_gnd and not is_vcc
                and d[1] in ("LUT1", "LUT2", "LUT3", "LUT4", "LUT5")):
            continue  # LUT1-5-driven DI: nextpnr adopts the driver into the 5LUT
        # (LUT6-driven DI is NOT adoptable -- a LUT6 has no O5 -- so buffer it)
        slot5 = f"{site}/{SLOT[k]}5LUT"
        if slot5 in occupied:
            continue
        if not is_gnd and not is_vcc:
            # fracture legality: the 5LUT shares A1-A5 with the slot's 6LUT;
            # adding a NEW input net requires occupant inputs + 1 <= 5
            occ6 = occupied.get(f"{site}/{SLOT[k]}6LUT")
            if occ6 is not None:
                oc = cells.get(occ6) or new_cells.get(occ6)
                ins = set()
                for pp, v in (oc.get("connections", {}) if oc else {}).items():
                    if pp != "O" and v and is_int(v[0]):
                        ins.add(v[0])
                if db not in ins and len(ins) + 1 > 5:
                    continue  # would be an illegal fracture; leave for nextpnr
        maxbit += 1
        onet = maxbit
        if is_gnd or is_vcc:
            # const-0/1 generator: INIT fed by the slot's 6LUT input net.
            # SAME net as the occupant's first pin -> shared A1, no conflict.
            # ILLEGAL when the occupant uses >=6 inputs (Vivado 18-608: "A6
            # cannot be used because of A5LUT usage") -- the fractured LUT's
            # O6 reads the upper INIT half with A6 tied high and the 5LUT
            # OVERWRITES the lower half, corrupting the LUT6's function in
            # hardware (this silently broke the SGMII AN comparators:
            # CONFIG_REG_MATCH/CONSISTENCY_MATCH -> link never came up).
            occ6 = occupied.get(f"{site}/{SLOT[k]}6LUT")
            occ_n = 0
            if occ6 is not None:
                oc = cells.get(occ6) or new_cells.get(occ6)
                ins6 = set()
                for pp2, v2 in (oc.get("connections", {}) if oc else {}).items():
                    if pp2 != "O" and v2 and is_int(v2[0]):
                        ins6.add(v2[0])
                occ_n = len(ins6)
            if occ_n >= 6 or slot_in[k] is None:
                (pending_vcc_di if is_vcc else pending_gnd_di).append(k)
                continue
            tag = "DIvcc" if is_vcc else "DIgnd"
            buf = {"type": "LUT1",
                   "port_directions": {"I0": "input", "O": "output"},
                   "connections": {"I0": [slot_in[k]], "O": [onet]},
                   "parameters": {"INIT": "11" if is_vcc else "00"},
                   "attributes": {"BEL": slot5}}
        else:
            # FF/LUT6-driven DI passthrough.  PIN-ALIGN with the 6LUT occupant:
            # nextpnr pin-maps each fractured LUT's I0->A1, I1->A2... per cell,
            # so a LUT1 here with a DIFFERENT net than the occupant's first pin
            # double-books sitewire A1 (seen: SLICE_X2Y65/A1 overused by 2 nets
            # -> the whole "->A1 unroutable" class).  Mirror the occupant's
            # input nets on I0..In-1 and put the DI net on the NEXT pin; INIT =
            # passthrough of the top input (upper half ones).
            tag = "DIrt"
            occ6 = occupied.get(f"{site}/{SLOT[k]}6LUT")
            occ_ins = []
            if occ6 is not None:
                oc = cells.get(occ6) or new_cells.get(occ6)
                for pp in ("I0", "I1", "I2", "I3", "I4", "I5"):
                    v2 = (oc.get("connections", {}) if oc else {}).get(pp, [])
                    if v2 and is_int(v2[0]):
                        occ_ins.append(v2[0])
            # HARD fracture rule (Vivado 18-608): an occupant using >=6
            # DISTINCT inputs forbids ANY 5LUT in the slot -- even when the
            # DI net is among its inputs (truncation does not reduce the
            # occupant's own pin usage).  nextpnr's di_via_ax handles the
            # pinned-6-input-LUT case natively; leave DI direct.
            if len(set(occ_ins)) >= 6:
                continue
            if db in occ_ins:
                occ_ins = occ_ins[:occ_ins.index(db)]  # DI net already a pin
            if len(occ_ins) + 1 > 5:
                continue  # cannot align within the 5 shared pins
            n_in = len(occ_ins) + 1
            init = "1" * (1 << (n_in - 1)) + "0" * (1 << (n_in - 1))
            conns = {f"I{i}": [b2] for i, b2 in enumerate(occ_ins)}
            conns[f"I{n_in-1}"] = [db]
            conns["O"] = [onet]
            dirs = {p: "input" for p in conns if p != "O"}
            dirs["O"] = "output"
            buf = {"type": f"LUT{n_in}", "port_directions": dirs,
                   "connections": conns,
                   "parameters": {"INIT": init}, "attributes": {"BEL": slot5}}
        bufname = f"{cn}${tag}${k}"
        claim(slot5, bufname)
        new_cells[bufname] = buf
        newDI[k] = onet
        n_di += 1
    if pending_gnd_di:
        # per-carry const-0 in a NEIGHBOUR slice; DI enters via the AX bypass
        src = slice_local_net()
        rbel = free_neighbour_lut_global(site) if src is not None else None
        if rbel is not None:
            maxbit += 1
            onet = maxbit
            gname = f"{cn}$DIgndx"
            new_cells[gname] = {"type": "LUT1",
                "port_directions": {"I0": "input", "O": "output"},
                "connections": {"I0": [src], "O": [onet]},
                "parameters": {"INIT": "00"}, "attributes": {"BEL": rbel}}
            occupied[rbel] = gname
            for k in pending_gnd_di:
                newDI[k] = onet
                n_di += 1
    if pending_vcc_di:
        # per-carry const-1 in a NEIGHBOUR slice; DI enters via the AX bypass
        src = slice_local_net()
        rbel = free_neighbour_lut_global(site) if src is not None else None
        if rbel is not None:
            maxbit += 1
            onet = maxbit
            vname = f"{cn}$DIvccx"
            new_cells[vname] = {"type": "LUT1",
                "port_directions": {"I0": "input", "O": "output"},
                "connections": {"I0": [src], "O": [onet]},
                "parameters": {"INIT": "11"}, "attributes": {"BEL": rbel}}
            occupied[rbel] = vname
            for k in pending_vcc_di:
                newDI[k] = onet
                n_di += 1
    if DI:
        c["connections"]["DI"] = newDI
    # --- O outputs: sum FF (FD* consuming O[k] on D) ---
    for k, ob in enumerate(O):
        if not is_int(ob):
            continue
        for cn2, c2 in cells.items():
            if not c2["type"].startswith("FD"):
                continue
            dcon = c2.get("connections", {}).get("D", [])
            if dcon and dcon[0] == ob:
                slotff = f"{site}/{SLOT[k]}FF"
                # only stamp if not already placed elsewhere
                if "BEL" not in c2.get("attributes", {}):
                    try:
                        claim(slotff, cn2)
                        c2.setdefault("attributes", {})["BEL"] = slotff
                        n_ff += 1
                    except SystemExit:
                        pass  # slot taken; leave FF for nextpnr
                break

# --- same-slot FF->LUT feedback relays -------------------------------------
# A counter bit-0 inverter (S[0]=~Q[0]) sits at x6LUT with its input driven by
# the SAME slot's xFF Q.  The chipdb cannot route a same-slot Q->imux bounce
# (AQ->A1 fails at any visit cap), so relay the feedback through an identity
# LUT1 in a NEIGHBOUR slice: two ordinary inter-slice arcs.
import re as _re
known_sites = set()
try:
    fp = json.load(open(os.environ.get("CARRY_FLOORPLAN","/tmp/virtex7_floorplan.json")))
    for s_ in fp.get("sites", []):
        n = s_.get("name", "")
        if n.startswith("SLICE_"):
            known_sites.add(n)
except Exception:
    pass
for cn2, c2 in cells.items():
    b = c2.get("attributes", {}).get("BEL", "")
    if b:
        known_sites.add(b.split("/")[0])
def free_neighbour_lut(site):
    m = _re.match(r"SLICE_X(\d+)Y(\d+)$", site)
    if not m:
        return None
    x, y = int(m.group(1)), int(m.group(2))
    for dx, dy in ((1,0),(-1,0),(0,1),(0,-1),(1,1),(-1,1),(1,-1),(-1,-1)):
        ns = f"SLICE_X{x+dx}Y{y+dy}"
        if ns not in known_sites:
            continue
        for sl in SLOT:
            bel = f"{ns}/{sl}6LUT"
            if bel not in occupied:
                return bel
    return None
# TARGETED only: blanket relaying of all 388 same-slot feedbacks REGRESSED
# 13->65 skips (the extra double-hop arcs congested the X2/X3 carry-column
# INT and created ~15 new marginal losers).  Relay only the nets listed in
# $CARRY_FB_NETS (one net name per line, from the previous route's skips).
import os
fb_nets = set()
fbf = os.environ.get("CARRY_FB_NETS")
if fbf and os.path.exists(fbf):
    fb_nets = {l.strip() for l in open(fbf) if l.strip()}
bit2name = {}
for nm, nn in mod.get("netnames", {}).items():
    for b in nn.get("bits", []):
        if isinstance(b, int) and b not in bit2name:
            bit2name[b] = nm
n_fb = 0
for cn2, c2 in list(cells.items()) + list(new_cells.items()):
    b = c2.get("attributes", {}).get("BEL", "")
    if "/"  not in b or not b.endswith("6LUT") or not c2["type"].startswith("LUT"):
        continue
    site, leaf = b.split("/")
    slot_letter = leaf[0]
    conns = c2.get("connections", {})
    for pp, v in list(conns.items()):
        if pp == "O" or not v or not is_int(v[0]):
            continue
        d = drv.get(v[0])
        if d is None or not d[1].startswith("FD"):
            continue
        dbel = (cells.get(d[0]) or {}).get("attributes", {}).get("BEL", "")
        if dbel != f"{site}/{slot_letter}FF":
            continue  # only the unroutable same-slot Q->imux case
        if bit2name.get(v[0]) not in fb_nets:
            continue  # targeted: only nets the previous route actually failed
        rbel = free_neighbour_lut(site)
        if rbel is None:
            continue
        maxbit += 1
        onet = maxbit
        rname = f"{cn2}$fbrelay${pp}"
        cells[rname] = {"type": "LUT1",
                        "port_directions": {"I0": "input", "O": "output"},
                        "connections": {"I0": [v[0]], "O": [onet]},
                        "parameters": {"INIT": "10"},
                        "attributes": {"BEL": rbel}}
        occupied[rbel] = rname
        conns[pp] = [onet]
        n_fb += 1
print(f"same-slot feedback relays: {n_fb}")

cells.update(new_cells)
json.dump(j, open(out_json, "w"))
print(f"carry slices completed: {n_buf} S-buffers, {n_slut} S-LUTs, {n_ff} sum-FFs, {n_di} DI-gnd 5LUTs")
print(f"total cells now: {len(cells)}")
