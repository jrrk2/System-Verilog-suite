-- recipes/vhdl_sv_equiv.lua
--
-- Unified VHDL ≡ SystemVerilog equivalence verifier. Replaces
-- test_behavioral_z3.exe, test_behavioral_equivalence.exe,
-- test_miter_equivalence.exe, test_vhdl_uart.exe (single-file VHDL
-- parse check).
--
-- Invocation modes:
--
--   sv_suite script vhdl_sv_equiv.lua <vhdl.vhd> <sv.sv>
--     -> parses both, picks the (only) module from each, runs Z3
--        miter, prints VERIFICATION SUCCESS / FAILURE marker, exits
--        0 on equivalent and 1 otherwise (via error() on failure).
--
--   sv_suite script vhdl_sv_equiv.lua <vhdl.vhd>
--     -> single-file VHDL parse smoke test. Prints module names and
--        signal counts; exits 0 if parse + register-inference are OK.

if ARGN < 1 then
    print("usage: sv_suite script vhdl_sv_equiv.lua <vhdl.vhd> [<sv.sv>]")
    error("missing args")
end

vhdl_file = ARGV[1]
sv_file   = ARGV[2]

print("═══════════════════════════════════════════════════════════════")
print("  VHDL ≡ SystemVerilog equivalence")
print("═══════════════════════════════════════════════════════════════")

function first_name(s)
    p = strfind(s, ",")
    if p == nil then return s end
    return strsub(s, 1, p - 1)
end

-- VHDL doesn't take a top hint; just parse the file.
vp = svd.parse("vhdl", "", {vhdl_file})
print("VHDL: " .. vhdl_file)
print("  modules: " .. svd.module_names(vp))
vname = first_name(svd.module_names(vp))

if sv_file == nil then
    print("(single-file VHDL parse smoke test — no SV peer)")
    return
end

-- For Verible we need the top name as a hint — use the VHDL name.
sp = svd.parse("verible", vname, {sv_file})
print("SV:   " .. sv_file)
print("  modules: " .. svd.module_names(sp))
sname = vname

-- Optimise both before miter — the original test_behavioral_z3 used
-- Behavioral_optimize.optimize_custom to normalise widths and DCE.
vp = svd.optimize(vp)
sp = svd.optimize(sp)

vm = svd.prep_for_z3(svd.pick(vp, vname))
sm = svd.prep_for_z3(svd.pick(sp, sname))

-- Register-counts comparison, like test_behavioral_equivalence used to
-- print (some callers grep for this).
print("")
print("Register Inference Results:")
print("  VHDL: " .. svd.register_analyse(vm))
print("  SV:   " .. svd.register_analyse(sm))
print("")

verdict = svd.miter(vm, sm)
if verdict == "EQUIVALENT" then
    print("VERIFICATION SUCCESS")
    print("The two designs are formally equivalent.")
else
    print("VERIFICATION FAILED")
    print("Z3 result: " .. verdict)
    error("not equivalent")
end
