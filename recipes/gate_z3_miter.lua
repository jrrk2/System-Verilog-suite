-- recipes/gate_z3_miter.lua
--
-- Replaces test_gate_z3_miter.ml: Z3 miter for a behavioral source vs
-- a gate-level netlist, given a Liberty library for the cell bodies.
-- The whole job is one svd.gate_miter call now that the binding exists.
--
-- Caller globals:
--   TOP    string -- top-module name
--   BEH    string -- behavioral source (.sv / .v)
--   GATE   string -- cell-mapped gate netlist (.sv / .v)
--   LIB    string -- Liberty library path  (optional, default
--                   $HOME/hardcaml-lua.0.0.1/liberty/simcells.lib)

if LIB == nil then
    -- lua-ml doesn't expose os.getenv; let caller pass HOME if non-default.
    home = HOME or "/home/jonathan"
    LIB = home .. "/hardcaml-lua.0.0.1/liberty/simcells.lib"
end

print("recipe: gate_z3_miter  top=" .. TOP)
print("  beh:     " .. BEH)
print("  gate:    " .. GATE)
print("  liberty: " .. LIB)

result = svd.gate_miter(TOP, BEH, GATE, LIB)
print("verdict: " .. result)
