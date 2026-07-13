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
import json, sys

ft_json, bels_path, out_json = sys.argv[1], sys.argv[2], sys.argv[3]
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

# GND-driven net bits (outputs of GND primitives)
gnd_bits = set()
for cn, c in cells.items():
    if c["type"] == "GND":
        for nets in c.get("connections", {}).values():
            for b in nets:
                if isinstance(b, int):
                    gnd_bits.add(b)

# driver of each integer net bit -> (cellname, type, port)
drv = {}
for cn, c in cells.items():
    dirs = c.get("port_directions", {})
    for p, nets in c.get("connections", {}).items():
        if dirs.get(p) == "output":
            for b in nets:
                if isinstance(b, int):
                    drv[b] = (cn, c["type"], p)

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
        for sb2 in S:
            if is_int(sb2) and sb2 not in gnd_bits:
                d2 = drv.get(sb2)
                if d2 and d2[1].startswith("FD"):
                    return sb2
        for pn in ("CYINIT", "CI"):
            v = c["connections"].get(pn, [])
            if v and is_int(v[0]) and v[0] not in gnd_bits:
                return v[0]
        return None
    # per-slot: the net feeding this slot's 6LUT occupant (for 5LUT input sharing)
    slot_in = [None, None, None, None]
    # --- S inputs ---
    newS = list(S)
    for k, sb in enumerate(S):
        slot6 = f"{site}/{SLOT[k]}6LUT"
        d = drv.get(sb) if is_int(sb) else None
        is_gnd = (not is_int(sb)) or (sb in gnd_bits)
        if d is not None and d[1].startswith("LUT"):
            # LUT-driven: stamp that S-LUT into the slot
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
        maxbit += 1
        onet = maxbit
        bufname = f"{cn}$Srt${k}"
        if is_gnd:
            # S=GND: const-0 generator -- LUT1 INIT=0 fed by a LOCAL net
            # (needs no global GND connection at all)
            src = slice_local_net()
            if src is None:
                continue
            buf = {"type": "LUT1", "port_directions": {"I0": "input", "O": "output"},
                   "connections": {"I0": [src], "O": [onet]},
                   "parameters": {"INIT": "00"}, "attributes": {"BEL": slot6}}
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
    for k, db in enumerate(DI):
        if not is_int(db):
            continue
        d = drv.get(db)
        is_gnd = db in gnd_bits
        if (d is not None and not is_gnd
                and d[1] in ("LUT1", "LUT2", "LUT3", "LUT4", "LUT5")):
            continue  # LUT1-5-driven DI: nextpnr adopts the driver into the 5LUT
        # (LUT6-driven DI is NOT adoptable -- a LUT6 has no O5 -- so buffer it)
        slot5 = f"{site}/{SLOT[k]}5LUT"
        if slot5 in occupied:
            continue
        if not is_gnd:
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
        if is_gnd:
            # const-0 generator: INIT=0 fed by the slot's 6LUT input net
            # (fracture legality: 5LUT shares A-pins with the 6LUT occupant)
            if slot_in[k] is None:
                continue
            src, init, tag = slot_in[k], "00", "DIgnd"
        else:
            # FF/external-driven DI: identity buffer passing the net to DI via
            # O5 (pre-empts nextpnr's unconstrained di_feed, which HeAP can't
            # bind when the carry is BEL-pinned)
            src, init, tag = db, "10", "DIrt"
        maxbit += 1
        onet = maxbit
        bufname = f"{cn}${tag}${k}"
        buf = {"type": "LUT1", "port_directions": {"I0": "input", "O": "output"},
               "connections": {"I0": [src], "O": [onet]},
               "parameters": {"INIT": init}, "attributes": {"BEL": slot5}}
        claim(slot5, bufname)
        new_cells[bufname] = buf
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

cells.update(new_cells)
json.dump(j, open(out_json, "w"))
print(f"carry slices completed: {n_buf} S-buffers, {n_slut} S-LUTs, {n_ff} sum-FFs, {n_di} DI-gnd 5LUTs")
print(f"total cells now: {len(cells)}")
