-- recipes/vhdl_vs_verible.lua
--
-- THE VIVADO -rtl ORACLE.  Miter a module as read from Vivado's
-- `synth_design -rtl` + `write_vhdl` against the same module as read from the
-- ORIGINAL SystemVerilog by another front end.
--
-- Why -rtl and write_vhdl specifically (both halves matter):
--   * -rtl is elaborated RTL, no optimisation and no technology mapping, so
--     names and structure track the SOURCE.  Against a fully synthesised
--     netlist the miter paired 319 of 22835 variables.
--   * write_vhdl, not write_verilog: write_verilog of an -rtl elaboration
--     emits bodyless RTL_ADD / RTL_MUX8 / RTL_REG cells and every leaf comes
--     back INCONCLUSIVE.
--
--   sv_suite script vhdl_vs_verible.lua <module> <rtl.vhd> <src> [src ...]
--
-- SV_FRONTEND selects the source-side reader (default "verible"; "slang" and
-- "verilator" are the other independent elaborators).  Running the same module
-- against the Vivado read from TWO different source front ends is the 3-way:
-- two agreeing and one differing identifies the outlier, which a 2-way cannot.
--
-- Run recipes/null_test_miter.lua on the SAME file first and believe this only
-- for modules that are self-equivalent there.  A harness that cannot prove a
-- module equal to itself cannot tell you anything about two designs.

if ARGN < 3 then
    print("usage: sv_suite script vhdl_vs_verible.lua <module> <rtl.vhd> <src>...")
    error("missing args")
end

target = ARGV[1]
vhdl   = ARGV[2]

srcs = {}
i = 3
n = 1
while i <= ARGN do
    srcs[n] = ARGV[i]
    n = n + 1
    i = i + 1
end

vp = svd.parse("vhdl", "", {vhdl})
svfe = os.getenv("SV_FRONTEND")
if svfe == nil then svfe = "verible" end
sp = svd.parse(svfe, target, srcs)

-- GND/VCC have no body in either read; without the models every verdict is
-- INCONCLUSIVE on GND:GND (augment_xil_models covers GND, expand_fpga doesn't).
vp = svd.augment_xil_models(vp)
sp = svd.augment_xil_models(sp)

vm = svd.pick(vp, target)
sm = svd.pick(sp, target)

print("REGS vhdl    " .. svd.register_analyse(vm))
print("REGS " .. svfe .. " " .. svd.register_analyse(sm))

-- Rename the SV-side registers onto Vivado's names (see reg_canon_names):
-- Vivado drops the `_reg` of a register that drives a like-named port and
-- spells an inverted copy `_reg_n`.  Without this the state does not pair and
-- the miter reports DIFFER whatever the logic does.
sm = svd.reg_canon_names(sm, vm)

vz = svd.prep_for_z3(vm)
sz = svd.prep_for_z3(sm)

verdict = svd.miter(vz, sz)
print("VERDICT " .. target .. " vhdl-vs-" .. svfe .. " " .. verdict)
