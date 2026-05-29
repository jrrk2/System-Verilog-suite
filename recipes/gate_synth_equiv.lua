-- recipes/gate_synth_equiv.lua
--
-- Replaces test_synth_equiv.ml: formal equivalence between source SV
-- and the cell-mapped Verilog produced by a synthesiser.
--
-- For each module name present in BOTH the source program and the
-- cell-mapped program, runs Z3 equivalence and prints a per-module
-- verdict.  Returns 0 iff every common module proves equivalent.
--
-- Caller globals:
--   SRC_FILES   table  -- source .sv / .v files
--   CELL_FILE   string -- cell-mapped Verilog (single file)
--   LIB         string -- Liberty file path
--   TOP         string -- top-module name (used as parse anchor only;
--                       all common modules are mitered, not just top)
--   FRONTEND    string (optional, default "verible")

fe = FRONTEND or "verible"
print("recipe: gate_synth_equiv  top=" .. TOP .. "  lib=" .. LIB)

src_prog  = svd.parse(fe, TOP, SRC_FILES)
print("  src  modules: " .. svd.module_names(src_prog))

-- Cell-mapped: parse with externals via the Liberty stub trick
-- (gate_miter handles a single top this way; here we go a level lower
-- and expand the whole program so every common module is comparable).
lib_h     = svd.liberty(LIB)
cell_prog = svd.parse(fe, TOP, {CELL_FILE})
cell_prog = svd.expand(cell_prog, lib_h)
print("  cell modules: " .. svd.module_names(cell_prog))

-- Common module names: intersect by name. lua-ml is Lua 2.5 so we
-- split the comma-separated module_names output with strfind/strsub
-- rather than gfind/gmatch.
function name_list(s)
    t = {}
    i = 1
    pos = 1
    while pos <= strlen(s) do
        c = strfind(s, ",", pos)
        if c == nil then
            t[i] = strsub(s, pos, strlen(s)); return t
        end
        t[i] = strsub(s, pos, c - 1)
        i = i + 1; pos = c + 1
    end
    return t
end

src_names  = name_list(svd.module_names(src_prog))
cell_names = name_list(svd.module_names(cell_prog))

ok = 0; fail = 0; skip = 0
i = 1
while src_names[i] do
    name = src_names[i]
    found = 0
    j = 1
    while cell_names[j] and found == 0 do
        if cell_names[j] == name then found = 1 end
        j = j + 1
    end
    if found == 1 then
        ma = svd.pick(src_prog,  name)
        mb = svd.pick(cell_prog, name)
        r  = svd.miter(svd.prep_for_z3(ma), svd.prep_for_z3(mb))
        if r == "EQUIVALENT" then ok = ok + 1 else fail = fail + 1 end
        print(format("  %-30s %s", name, r))
    else
        skip = skip + 1
    end
    i = i + 1
end
print("  summary: " .. ok .. " equivalent, " .. fail .. " differ, "
                   .. skip .. " src-only (no cell counterpart)")
