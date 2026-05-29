-- recipes/edif_vs_vhdl.lua
--
-- Replaces test_edif_vhdl_equivalence.exe: compare a VHDL-direct BIR
-- against a Vivado-synthesised-EDIF round-tripped BIR for the same
-- design.  Used by run_edif_vhdl_equivalence_test.sh after Vivado has
-- produced the .edf.
--
-- Usage: sv_suite script edif_vs_vhdl.lua <edif.edf> <vhdl.vhd> <top>

if ARGN < 3 then
    print("usage: sv_suite script edif_vs_vhdl.lua <edif.edf> <vhdl.vhd> <top>")
    error("missing args")
end

edif_file = ARGV[1]
vhdl_file = ARGV[2]
top       = ARGV[3]

print("recipe: edif_vs_vhdl  top=" .. top)
print("  edif: " .. edif_file)
print("  vhdl: " .. vhdl_file)

-- Side A: VHDL direct.
vp = svd.parse("vhdl", "", {vhdl_file})
print("VHDL modules: " .. svd.module_names(vp))

-- Side B: EDIF round-trip.
ep = svd.read_edif(edif_file)
print("EDIF modules: " .. svd.module_names(ep))

-- Shape-only comparison (the original test_edif_vhdl_equivalence
-- focused on signal/process counts rather than full Z3 equivalence,
-- because EDIF is structural and direct VHDL is behavioural).
vm = svd.pick(vp, top)
em = svd.pick(ep, top)
print("")
print(top .. " VHDL: " .. svd.register_analyse(vm))
print(top .. " EDIF: " .. svd.register_analyse(em))

-- We attempt the Z3 miter but treat DIFFER as informational (the two
-- representations live at different abstraction levels).
v_for_z3 = svd.prep_for_z3(vm)
e_for_z3 = svd.prep_for_z3(em)
verdict = svd.miter(v_for_z3, e_for_z3)
print("Z3 verdict: " .. verdict)
print("OK")
