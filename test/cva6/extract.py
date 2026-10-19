# extract.py <flat.v> <outdir> <module-regex>...: for every module whose
# name matches a regex, write <outdir>/<name>.v holding it and every module
# it (transitively) instantiates.  Prints "name children..." per module.
import re, sys
flat, out, pats = sys.argv[1], sys.argv[2], sys.argv[3:]
t = open(flat).read()
mods = {}
for m in re.finditer(r'^module\s+(\w+)\b.*?^endmodule', t, re.M | re.S):
    mods[m.group(1)] = m.group(0)
names = set(mods)
def children(body):
    # instance: <modname> [#(...)] <inst> (  -- find identifiers followed by an instance name and '('
    cs = set()
    for mm in re.finditer(r'^\s*(\w+)\s*(?:#\s*\(.*?\)\s*)?(\w+)\s*\(', body, re.M | re.S):
        if mm.group(1) in names and mm.group(1) != mm.group(2): cs.add(mm.group(1))
    return cs
sel = [n for n in sorted(mods) if any(re.fullmatch(p, n) for p in pats)]
for n in sel:
    todo, keep = [n], []
    while todo:
        x = todo.pop()
        if x in keep: continue
        keep.append(x); todo += sorted(children(mods[x]))
    open(f"{out}/{n}.v", "w").write("\n\n".join(mods[k] for k in keep) + "\n")
    print(n, *(k for k in keep if k != n))
