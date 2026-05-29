-- recipes/asym_pair_miter.lua
--
-- Replaces test_yosys_rtlil_miter.ml and test_xilinx_rtl_miter.ml: a
-- Z3 miter where the two sides come from *different* file lists
-- and/or *different* frontends. multi_frontend_miter.lua assumes one
-- file list for all frontends, which doesn't fit a Vivado-elaborated
-- side that needs its own primitive stubs.
--
-- Caller globals:
--   TOP        string -- top-module name
--   FILES_A    table  -- side-A source files
--   FRONTEND_A string -- side-A parser ("verible", "yosys",
--                       "verilator", "slang", "vhdl", "surelog")
--   FILES_B    table  -- side-B source files
--   FRONTEND_B string -- side-B parser
--   LABEL_A    string (optional) -- pretty label for side A
--   LABEL_B    string (optional) -- pretty label for side B

la = LABEL_A or FRONTEND_A
lb = LABEL_B or FRONTEND_B
print("recipe: asym_pair_miter  top=" .. TOP)
print("  A: " .. la .. " (frontend=" .. FRONTEND_A .. ")")
print("  B: " .. lb .. " (frontend=" .. FRONTEND_B .. ")")

pa = svd.parse(FRONTEND_A, TOP, FILES_A)
pb = svd.parse(FRONTEND_B, TOP, FILES_B)
ma = svd.pick(pa, TOP);  ma = svd.prep_for_z3(ma)
mb = svd.pick(pb, TOP);  mb = svd.prep_for_z3(mb)

result = svd.miter(ma, mb)
print("verdict: " .. result)
