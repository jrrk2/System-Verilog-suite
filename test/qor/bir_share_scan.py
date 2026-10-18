#!/usr/bin/env python3
"""Sharing / structuring opportunity scan over an optimised-BIR JSON dump
(GATE_MAP_BIR_JSON=<dir> makes gate_map write <dir>/<module>.json).

Reports, per module:
  1. operator inventory       -- arithmetic / compare / shift ops by op and width
  2. mutually-exclusive ops   -- same op+width in different arms of one if/case
                                 (yosys `share` candidates: one operator + input mux)
  3. mux-fed identical ops    -- `c ? x OP a : x OP b` (yosys `opt_share`:
                                 x OP (c ? a : b))
  4. duplicate subexpressions -- structurally identical non-trivial expressions
                                 occurring more than once (CSE candidates)
  5. over-wide operators      -- result width > bits actually consumed
  6. constant-compare decoders-- `x == K` families on one selector
Usage: bir_share_scan.py <dir>/<module>.json [...]"""
import json, sys, collections

ARITH = {"BAdd", "BSub", "BMul", "BDiv", "BMod"}
CMP = {"BEq", "BNe", "BLt", "BLe", "BGt", "BGe"}
SHIFT = {"BShl", "BShr", "BAshr"}
COST = {"BAdd": 1, "BSub": 1, "BMul": 8, "BDiv": 16, "BMod": 16, "BLt": 1, "BLe": 1, "BGt": 1, "BGe": 1,
        "BEq": 0.5, "BNe": 0.5, "BShl": 2, "BShr": 2, "BAshr": 2}

def tag(x):            # ppx_deriving_yojson variant: ["Ctor", payload...] or "Ctor"
    if isinstance(x, list): return x[0], (x[1] if len(x) > 1 else None)
    return x, None

def width_of_type(t):
    c, p = tag(t)
    if c == "BInt": return p["width"]
    if c == "BBool": return 1
    if c == "BArray": return p["size"] * width_of_type(p["element"])
    return 32

class Scan:
    def __init__(self, m):
        self.m = m
        self.widths = {s["name"]: width_of_type(s["stype"]) for s in m["signals"]}
        self.ops = []                 # (op, width, key, path, cond_ctx, expr)
        self.subexpr = collections.Counter()
        self.subexpr_size = {}
        self.cmp_families = collections.defaultdict(set)
        self.consumed = collections.defaultdict(int)   # signal -> max msb read + 1
        self.muxfed = []              # opt_share candidates

    # ---- expressions -----------------------------------------------------
    def key(self, e):
        c, p = tag(e)
        if c == "BVar": return p
        if c == "BConst": return f"{p['width']}'{p['value']}"
        if c == "BBinOp": return f"({self.key(p['lhs'])} {p['op'] if isinstance(p['op'], str) else p['op'][0]} {self.key(p['rhs'])})"
        if c == "BUnOp": return f"({p['op'] if isinstance(p['op'], str) else p['op'][0]} {self.key(p['operand'])})"
        if c == "BSlice": return f"{self.key(p['signal'])}[{p['msb']}:{p['lsb']}]"
        if c == "BSelect": return f"{self.key(p['array'])}[{self.key(p['index'])}]"
        if c == "BConcat": return "{" + ",".join(self.key(x) for x in p) + "}"
        if c == "BReplicate": return f"{{{p['count']}{{{self.key(p['value'])}}}}}"
        if c == "BCond": return f"({self.key(p['condition'])} ? {self.key(p['then_val'])} : {self.key(p['else_val'])})"
        if c == "BCall": return f"{p['func']}({','.join(self.key(a) for a in p['args'])})"
        return c

    def size(self, e):
        c, p = tag(e)
        if c in ("BVar", "BConst"): return 1
        if c == "BBinOp": return 1 + self.size(p["lhs"]) + self.size(p["rhs"])
        if c == "BUnOp": return 1 + self.size(p["operand"])
        if c == "BSlice": return self.size(p["signal"])
        if c == "BSelect": return 1 + self.size(p["array"]) + self.size(p["index"])
        if c == "BConcat": return sum(self.size(x) for x in p)
        if c == "BReplicate": return self.size(p["value"])
        if c == "BCond": return 1 + self.size(p["condition"]) + self.size(p["then_val"]) + self.size(p["else_val"])
        if c == "BCall": return 1 + sum(self.size(a) for a in p["args"])
        return 1

    def width(self, e):
        c, p = tag(e)
        if c == "BVar": return self.widths.get(p, 0)
        if c == "BConst": return p["width"]
        if c in ("BBinOp", "BUnOp"): return width_of_type(p["result_type"])
        if c == "BSlice": return p["msb"] - p["lsb"] + 1
        if c == "BConcat": return sum(self.width(x) for x in p)
        if c == "BReplicate": return p["count"] * self.width(p["value"])
        if c == "BCond": return self.width(p["then_val"])
        return 0

    def note_read(self, e):
        c, p = tag(e)
        if c == "BVar": self.consumed[p] = max(self.consumed[p], self.widths.get(p, 0))
        elif c == "BSlice":
            cc, pp = tag(p["signal"])
            if cc == "BVar": self.consumed[pp] = max(self.consumed[pp], p["msb"] + 1)
            else: self.note_read(p["signal"])

    def walk_expr(self, e, ctx, path):
        c, p = tag(e)
        k = self.key(e); sz = self.size(e)
        if sz >= 4:
            self.subexpr[k] += 1; self.subexpr_size[k] = sz
        if c == "BBinOp":
            op = p["op"] if isinstance(p["op"], str) else p["op"][0]
            w = width_of_type(p["result_type"])
            if op in ARITH | CMP | SHIFT:
                self.ops.append((op, w, k, path, ctx, e))
            if op in CMP:
                lc, lp = tag(p["lhs"]); rc, rp = tag(p["rhs"])
                if lc == "BVar" and rc == "BConst": self.cmp_families[lp].add(rp["value"])
                if rc == "BVar" and lc == "BConst": self.cmp_families[rp].add(lp["value"])
            self.walk_expr(p["lhs"], ctx, path); self.walk_expr(p["rhs"], ctx, path)
        elif c == "BUnOp": self.walk_expr(p["operand"], ctx, path)
        elif c == "BSlice": self.note_read(e); self.walk_expr(p["signal"], ctx, path)
        elif c == "BVar": self.note_read(e)
        elif c == "BSelect": self.walk_expr(p["array"], ctx, path); self.walk_expr(p["index"], ctx, path)
        elif c == "BConcat":
            for x in p: self.walk_expr(x, ctx, path)
        elif c == "BReplicate": self.walk_expr(p["value"], ctx, path)
        elif c == "BCond":
            # opt_share: both arms are the same op on a shared operand
            tc, tp = tag(p["then_val"]); ec, ep = tag(p["else_val"])
            if tc == "BBinOp" and ec == "BBinOp":
                top = tp["op"] if isinstance(tp["op"], str) else tp["op"][0]
                eop = ep["op"] if isinstance(ep["op"], str) else ep["op"][0]
                if top == eop and top in ARITH | SHIFT:
                    shared = self.key(tp["lhs"]) == self.key(ep["lhs"]) or self.key(tp["rhs"]) == self.key(ep["rhs"])
                    if shared:
                        self.muxfed.append((top, width_of_type(tp["result_type"]), path, self.key(e)[:90]))
            self.walk_expr(p["condition"], ctx, path)
            self.walk_expr(p["then_val"], ctx + [("cond", self.key(p["condition"]), 1)], path)
            self.walk_expr(p["else_val"], ctx + [("cond", self.key(p["condition"]), 0)], path)
        elif c == "BCall":
            for a in p["args"]: self.walk_expr(a, ctx, path)

    # ---- statements --------------------------------------------------------
    def walk_stmts(self, stmts, ctx, path):
        for i, s in enumerate(stmts): self.walk_stmt(s, ctx, path + [i])

    def walk_stmt(self, s, ctx, path):
        c, p = tag(s)
        if c == "BAssign": self.walk_expr(p["rhs"], ctx, path)
        elif c == "BIf":
            self.walk_expr(p["condition"], ctx, path)
            ck = self.key(p["condition"])
            self.walk_stmts(p["then_stmts"], ctx + [("if", ck, 1)], path + ["t"])
            self.walk_stmts(p["else_stmts"], ctx + [("if", ck, 0)], path + ["e"])
        elif c == "BCase":
            self.walk_expr(p["selector"], ctx, path)
            sk = self.key(p["selector"])
            for j, (k, body) in enumerate(p["cases"]):
                self.walk_expr(k, ctx, path)
                self.walk_stmts(body, ctx + [("case", sk, j)], path + [f"c{j}"])
            self.walk_stmts(p["default"], ctx + [("case", sk, "d")], path + ["d"])
        elif c in ("BWhile", "BFor", "BBlock"):
            body = p["body"] if isinstance(p, dict) else p
            self.walk_stmts(body, ctx, path)
        elif c == "BCallStmt":
            for a in p["args"]: self.walk_expr(a, ctx, path)

    def run(self):
        for pi, proc in enumerate(self.m["processes"]):
            pc, pp = tag(proc)
            self.walk_stmts(pp["body"], [], [pp["name"]])

def exclusive(ctx_a, ctx_b):
    """Two contexts are mutually exclusive if they diverge on the same if/case."""
    for (ka, ca, va) in ctx_a:
        for (kb, cb, vb) in ctx_b:
            if ka == kb and ca == cb and va != vb: return True
    return False

def report(path):
    m = json.load(open(path))
    sc = Scan(m); sc.run()
    print(f"\n=== {m['name']}  ({len(m['signals'])} signals, {len(m['processes'])} processes)")
    inv = collections.Counter((op, w) for op, w, *_ in sc.ops)
    cost = sum(COST.get(op, 1) * w for op, w, *_ in sc.ops)
    print(f"1. operators: {len(sc.ops)} arith/compare/shift, weighted bit-cost {cost:.0f}")
    for (op, w), n in sorted(inv.items(), key=lambda kv: -COST.get(kv[0][0], 1) * kv[0][1] * kv[1])[:10]:
        print(f"     {n:3d} x {op:6s} {w:3d}-bit")
    # 2. share: same op+width, mutually exclusive contexts, different expressions
    by_ow = collections.defaultdict(list)
    for op, w, k, path, ctx, e in sc.ops:
        if op in ARITH | SHIFT and w >= 4: by_ow[(op, w)].append((k, ctx))
    saved = 0; lines = []
    for (op, w), items in by_ow.items():
        uniq = {}
        for k, ctx in items: uniq.setdefault(k, ctx)
        keys = list(uniq)
        if len(keys) < 2: continue
        # greedy: how many could collapse onto one operator
        groups = []
        for k in keys:
            placed = False
            for g in groups:
                if all(exclusive(uniq[k], uniq[k2]) for k2 in g):
                    g.append(k); placed = True; break
            if not placed: groups.append([k])
        n_shareable = sum(len(g) - 1 for g in groups if len(g) > 1)
        if n_shareable:
            saved += n_shareable * COST.get(op, 1) * w
            lines.append(f"     {op:6s} {w:3d}-bit: {len(keys)} distinct, {n_shareable} removable by muxing inputs")
    print(f"2. mutually-exclusive operator sharing (yosys `share`): est. bit-cost saved {saved:.0f}")
    for l in lines[:8]: print(l)
    print(f"3. mux-fed identical operators (yosys `opt_share`): {len(sc.muxfed)}")
    for op, w, path, k in sc.muxfed[:5]: print(f"     {op} {w}-bit  {k}")
    dups = [(k, n, sc.subexpr_size[k]) for k, n in sc.subexpr.items() if n > 1]
    dups.sort(key=lambda t: -(t[1] - 1) * t[2])
    print(f"4. duplicate subexpressions (CSE): {len(dups)} distinct, est. nodes saved {sum((n - 1) * s for _, n, s in dups)}")
    for k, n, s in dups[:5]: print(f"     {n} x size {s}: {k[:100]}")
    wide = []
    for op, w, k, path, ctx, e in sc.ops:
        pass
    narrow = [(s, w, sc.consumed[s]) for s, w in sc.widths.items() if sc.consumed.get(s, 0) and sc.consumed[s] < w and w >= 8]
    narrow.sort(key=lambda t: -(t[1] - t[2]))
    print(f"5. signals read narrower than declared (wreduce): {len(narrow)}")
    for s, w, c in narrow[:6]: print(f"     {s}: declared {w}, read {c}")
    fam = [(s, len(ks)) for s, ks in sc.cmp_families.items() if len(ks) >= 4]
    fam.sort(key=lambda t: -t[1])
    print(f"6. constant-compare families (decoder sharing): {len(fam)}")
    for s, n in fam[:5]: print(f"     {s} == {n} different constants")

for p in sys.argv[1:]: report(p)

# ---- 7. common-operand arithmetic inside ONE next-state mux tree -------------
# `r <= s==A ? sp + 1 : s==B ? sp - 3 : ... : sp + imm` : every add/sub leaf
# shares operand sp, so the tree is one adder fed by a mux of the other
# operands (sub folded as + (~y + 1)); the mux of mostly-constant operands is
# a LUT or two per bit, the adders it replaces are a LUT + carry per bit each.
def leaf_arith(e, sc, acc):
    c, p = tag(e)
    if c == "BCond":
        leaf_arith(p["then_val"], sc, acc); leaf_arith(p["else_val"], sc, acc)
    elif c == "BBinOp":
        op = p["op"] if isinstance(p["op"], str) else p["op"][0]
        if op in ("BAdd", "BSub"):
            w = width_of_type(p["result_type"])
            lk, rk = sc.key(p["lhs"]), sc.key(p["rhs"])
            lc, _ = tag(p["lhs"]); rc, _ = tag(p["rhs"])
            # the shared operand is the non-constant side
            if lc != "BConst": acc.append((lk, w, op, rk))
            if rc != "BConst" and op == "BAdd": acc.append((rk, w, op, lk))

def report7(path):
    m = json.load(open(path)); sc = Scan(m)
    total_saved = 0; rows = []
    def visit(stmts):
        for s in stmts:
            c, p = tag(s)
            if c == "BAssign":
                acc = []; leaf_arith(p["rhs"], sc, acc)
                groups = collections.defaultdict(set)
                for (x, w, op, other) in acc: groups[(x, w)].add((op, other))
                for (x, w), others in groups.items():
                    if len(others) >= 2:
                        rows.append((p["lhs"], x, w, len(others)))
            elif c == "BIf": visit(p["then_stmts"]); visit(p["else_stmts"])
            elif c == "BCase":
                for _, b in p["cases"]: visit(b)
                visit(p["default"])
            elif c in ("BBlock",): visit(p)
    for proc in m["processes"]:
        _, pp = tag(proc); visit(pp["body"])
    rows.sort(key=lambda r: -(r[3] - 1) * r[2])
    saved_bits = sum((n - 1) * w for _, _, w, n in rows)
    print(f"7. common-operand add/sub leaves in one next-state tree: {len(rows)} trees, "
          f"{sum(n for *_, n in rows)} adders -> {len(rows)} shared adders; est. {saved_bits} adder-bits removed")
    for lhs, x, w, n in rows[:10]:
        print(f"     {lhs:24s} <= {x:20s} +/- (mux of {n:2d} operands)  {w}-bit")

if __name__ == "__main__" and len(sys.argv) > 1:
    for p in sys.argv[1:]: report7(p)
