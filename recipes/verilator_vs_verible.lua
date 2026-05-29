-- recipes/verilator_vs_verible.lua
--
-- Replaces test_verilator_vs_verible.exe: Z3 miter on the same
-- SystemVerilog source parsed by Verilator and by Verible. Used by
-- the sv-tests Decompiler_Miter runner via decompiler_flatten.sh,
-- which passes top + flattened .sv as positional args.
--
-- Usage: sv_suite script verilator_vs_verible.lua <top> <file.sv>

if ARGN < 2 then
    print("usage: sv_suite script verilator_vs_verible.lua <top> <file.sv>")
    error("missing args")
end

top  = ARGV[1]
file = ARGV[2]
print("recipe: verilator_vs_verible  top=" .. top .. "  file=" .. file)

va = svd.parse("verilator", top, {file})
vb = svd.parse("verible",   top, {file})

ma = svd.prep_for_z3(svd.pick(va, top))
mb = svd.prep_for_z3(svd.pick(vb, top))

verdict = svd.miter(ma, mb)
print("verdict: " .. verdict)
if verdict ~= "EQUIVALENT" then
    error("not equivalent")
end
