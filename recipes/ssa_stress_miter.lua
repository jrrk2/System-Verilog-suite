-- recipes/ssa_stress_miter.lua
--
-- Replaces test_ssa_stress_miter.ml: round-trip a design through
-- yosys's `write_verilog` and prove that the result is equivalent to
-- the directly-parsed source under the SSA pipeline. Confirms that
-- our SSA pass plus yosys's output are not introducing semantic drift.
--
-- Caller globals:
--   TOP    string -- top-module name
--   FILES  table  -- source files (parsed twice — once via verible
--                  -- direct, once via yosys roundtrip)

print("recipe: ssa_stress_miter  top=" .. TOP)

-- Direct parse (the oracle): straight through verible + SSA.
direct = svd.parse("verible", TOP, FILES)
direct = svd.ssa(direct)
md = svd.pick(direct, TOP)
md = svd.prep_for_z3(md)

-- Yosys round-trip parse. svd.parse("yosys", …) runs yosys with
-- read_verilog + write_verilog internally and re-parses the result.
yos = svd.parse("yosys", TOP, FILES)
yos = svd.ssa(yos)
my = svd.pick(yos, TOP)
my = svd.prep_for_z3(my)

result = svd.miter(md, my)
print("  verdict: " .. result)
