#!/usr/bin/env python3
"""Structural equivalence check between SVS-emitted top.json and top.edif.

Both come from the same Netlist handle via bir_to_nextpnr_json and
bir_to_edif respectively, so they describe the same circuit graph.
We check three layers — cell-type histogram, per-cell params,
and connectivity graph equivalence."""
import json, re, sys
from collections import Counter, defaultdict

JSON_PATH = "/home/jonathan/counter125_build/top.json"
EDIF_PATH = "/home/jonathan/counter125_build/top.edif"

# ---------- read JSON ----------
def load_json():
    j = json.load(open(JSON_PATH))
    m = j["modules"]["top"]
    cells = {}
    for cn, c in m["cells"].items():
        cells[cn] = {
            "type":   c.get("type"),
            "params": dict(c.get("parameters", {})),
            "conns":  {k: list(v) for k, v in c.get("connections", {}).items()},
        }
    return cells, m["ports"]

# ---------- EDIF: paren-aware extraction ----------
def edif_balanced_extract(txt, start_pattern):
    """Yield substrings of `txt` that begin with `start_pattern` and
    extend to the matching close-paren (counting nested parens)."""
    for m in re.finditer(start_pattern, txt):
        i = m.start()
        depth = 0
        end = i
        in_string = False
        while end < len(txt):
            c = txt[end]
            if c == '"' and (end == 0 or txt[end-1] != '\\'):
                in_string = not in_string
            elif not in_string:
                if c == '(':
                    depth += 1
                elif c == ')':
                    depth -= 1
                    if depth == 0:
                        yield txt[i:end+1]
                        break
            end += 1

EDIF_INST_HEAD = re.compile(r"\(instance\s+(\S+)\s+\(viewref")
EDIF_NET_HEAD  = re.compile(r"\(net\s+(\S+)\s+\(joined")
EDIF_PROP_RE   = re.compile(r"\(property\s+(\S+)\s+\(string\s+\"([^\"]*)\"\)\)")
EDIF_REF_RE    = re.compile(r"\(portref\s+([^\s()]+)\s*(?:\(instanceref\s+([^\s()]+)\))?\)")
EDIF_REF_MEMBER_RE = re.compile(
    r"\(portref\s+\(member\s+([^\s()]+)\s+(\d+)\)\s*(?:\(instanceref\s+([^\s()]+)\))?\)")

def load_edif():
    txt = open(EDIF_PATH).read()
    cells = {}
    for block in edif_balanced_extract(txt, EDIF_INST_HEAD):
        head = EDIF_INST_HEAD.match(block)
        name = head.group(1)
        # cellref name
        m_cr = re.search(r"\(cellref\s+(\S+)\s+\(libraryref", block)
        cell_type = m_cr.group(1)
        props = {}
        for k, v in EDIF_PROP_RE.findall(block):
            mm = re.match(r"(\d+)'b([01]+)", v)
            props[k] = mm.group(2) if mm else v
        cells[name] = {"type": cell_type, "params": props,
                       "conns": defaultdict(list)}

    nets = {}
    for block in edif_balanced_extract(txt, EDIF_NET_HEAD):
        head = EDIF_NET_HEAD.match(block)
        net = head.group(1)
        refs = []
        for mm in EDIF_REF_MEMBER_RE.finditer(block):
            port = mm.group(1)
            bit  = int(mm.group(2))
            inst = mm.group(3) or "<top>"
            refs.append((inst, port, bit))
        for mm in EDIF_REF_RE.finditer(block):
            port_blob = mm.group(1)
            inst      = mm.group(2) or "<top>"
            if not port_blob.startswith("("):
                refs.append((inst, port_blob, 0))
        nets[net] = refs
        for inst, port, _ in refs:
            if inst in cells:
                cells[inst]["conns"][port].append(net)
    return cells, nets

def main():
    j_cells, j_ports = load_json()
    e_cells, e_nets  = load_edif()

    print("=== 1. cell-type histogram ===")
    jh = Counter(c["type"] for c in j_cells.values())
    eh = Counter(c["type"] for c in e_cells.values())
    print(f"JSON: {dict(jh)}")
    print(f"EDIF: {dict(eh)}")
    eh_no_tie = Counter({k: v for k, v in eh.items() if k not in ("GND","VCC")})
    print("  match (modulo GND/VCC ties):", jh == eh_no_tie)

    print()
    print("=== 2. per-cell type+params ===")
    diffs = 0
    for cn in j_cells:
        jc = j_cells[cn]
        ec = e_cells.get(cn)
        if ec is None:
            print(f"  MISSING in EDIF: {cn}")
            diffs += 1
            continue
        if jc["type"] != ec["type"]:
            print(f"  {cn}: type JSON={jc['type']} EDIF={ec['type']}")
            diffs += 1
        for pk, pv in jc["params"].items():
            ev = ec["params"].get(pk)
            if ev != pv:
                print(f"  {cn}.{pk}: JSON={pv!r} EDIF={ev!r}")
                diffs += 1
    print(f"  ({diffs} discrepancies)")

    print()
    print("=== 3. connectivity graph equivalence ===")
    # Map JSON bit-id to set of (cell, port).
    j_bit_users = defaultdict(set)
    for cn, jc in j_cells.items():
        for port, bits in jc["conns"].items():
            for b in bits:
                if isinstance(b, int):
                    j_bit_users[b].add((cn, port))
    # JSON partner sets.
    j_partners = defaultdict(set)
    for users in j_bit_users.values():
        for a in users:
            for b in users:
                if a != b:
                    j_partners[a].add(b)

    # EDIF partner sets.  Discard tie nets (n_GND/n_VCC) and inverter
    # partners that aren't expected in JSON's constant-token form.
    e_partners = defaultdict(set)
    for net, refs in e_nets.items():
        if net in ("n_GND", "n_VCC"):
            continue
        for a in refs:
            for b in refs:
                if a != b:
                    e_partners[(a[0], a[1])].add((b[0], b[1]))

    common_cells = set(j_cells) & set(e_cells)
    cell_port_pairs = set()
    for cn in common_cells:
        for port in j_cells[cn]["conns"]:
            cell_port_pairs.add((cn, port))

    # Mismatches: drop those whose JSON connection is all-constant
    # ('0'/'1'); those don't form a partner set in JSON's encoding.
    is_const_only = lambda bits: all(b in ("0", "1") for b in bits)
    diff_partners = 0
    examples = []
    for cp in cell_port_pairs:
        cn, port = cp
        bits = j_cells[cn]["conns"].get(port, [])
        if is_const_only(bits):
            continue  # JSON encodes via constant tokens, EDIF via ties
        jp = {pp for pp in j_partners.get(cp, set()) if pp[0] in common_cells}
        ep = {pp for pp in e_partners.get(cp, set()) if pp[0] in common_cells}
        if jp != ep:
            diff_partners += 1
            if len(examples) < 5:
                examples.append((cp, sorted(jp), sorted(ep)))
    for cp, jp, ep in examples:
        print(f"  {cp}:")
        print(f"    JSON-only partners: {sorted(set(jp) - set(ep))[:4]}")
        print(f"    EDIF-only partners: {sorted(set(ep) - set(jp))[:4]}")
    print(f"  ({diff_partners} connectivity mismatches over "
          f"{len(cell_port_pairs)} cell.port pairs)")

if __name__ == "__main__":
    main()
