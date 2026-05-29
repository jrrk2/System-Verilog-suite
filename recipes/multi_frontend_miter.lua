-- recipes/multi_frontend_miter.lua
--
-- Replaces test_two_frontend_miter / test_three_frontend_miter /
-- test_four_frontend_miter / test_five_frontend_miter / test_3way_*
-- and the pairwise variants in test_*_vs_*. Parses one design with
-- N front ends and checks Z3 equivalence of every pair.
--
-- Caller globals:
--   TOP       string -- top-module name
--   FILES     table  -- source files (Verilog/SystemVerilog/etc.)
--   FRONTENDS table  -- list of frontend names (verible/slang/yosys/
--                    --   verilator/sv-parser/vhdl/surelog)
--   STOP_ON_FIRST_DIFF bool (optional, default true)

stop = STOP_ON_FIRST_DIFF
if stop == nil then stop = 1 end

print("recipe: multi_frontend_miter  top=" .. TOP)

-- Phase 1: parse with every frontend, prep_for_z3 each module
mods = {}
labels = {}
i = 1
fe_idx = 1
while FRONTENDS[fe_idx] do
    fe = FRONTENDS[fe_idx]
    print("  parsing with " .. fe)
    prog = svd.parse(fe, TOP, FILES)
    m = svd.pick(prog, TOP)
    m = svd.prep_for_z3(m)
    mods[i] = m
    labels[i] = fe
    i = i + 1
    fe_idx = fe_idx + 1
end

-- Phase 2: pairwise miter
n = i - 1
ok = 0
fail = 0
print("  miter " .. n .. " front ends pairwise:")
a = 1
while a < n do
    b = a + 1
    while b <= n do
        r = svd.miter(mods[a], mods[b])
        print(format("    %-12s <-> %-12s  %s",
              labels[a], labels[b], r))
        if r == "EQUIVALENT" then ok = ok + 1 else fail = fail + 1 end
        if r ~= "EQUIVALENT" and stop ~= 0 then
            print("STOP_ON_FIRST_DIFF set — bailing out.")
            return
        end
        b = b + 1
    end
    a = a + 1
end
print("  summary: " .. ok .. " equivalent, " .. fail .. " differ")
