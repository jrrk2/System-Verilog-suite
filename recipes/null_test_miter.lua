-- recipes/null_test_miter.lua
--
-- NULL TEST: miter a design against ITSELF, per module.
--
-- A design is equivalent to itself.  Any verdict other than EQUIVALENT is a
-- fault in the HARNESS (reader, prep_for_z3, flatten, register correspondence,
-- solver budget), not in the design -- so until this comes back clean, every
-- cross-flow verdict from the same harness is noise.  This exists because the
-- Vivado -> write_vhdl -> SVS oracle was believed for a while on the strength
-- of DIFFER verdicts the null test later showed were the harness's own.
--
-- Run it FIRST, on the same file and with the same passes as the real
-- comparison, and only trust the real comparison on modules that pass here.
--
--   sv_suite script null_test_miter.lua <file> [frontend]           -- list modules
--   sv_suite script null_test_miter.lua <file> [frontend] <module>  -- test one
--
-- ONE MODULE PER INVOCATION is deliberate: a module that makes the harness
-- raise would otherwise abort the sweep, and the modules that ERROR are as
-- interesting as the ones that DIFFER.  Drive it from a shell loop over the
-- listing form.  (This binding exposes no protected call, so there is no
-- in-process way to survive one.)
--
-- Set Z3_MITER_TIMEOUT_MS generously: the default 30 s once turned a 33.4 s
-- proof into a false DIFFER, and a timeout that masquerades as a difference is
-- exactly the failure this recipe exists to catch.

if ARGN < 1 then
    print("usage: sv_suite script null_test_miter.lua <file> [frontend] [module]")
    error("missing args")
end

srcfile = ARGV[1]
fe      = ARGV[2]
if fe == nil then fe = "vhdl" end
target  = ARGV[3]

p1 = svd.parse(fe, "", {srcfile})

if target == nil then
    -- listing form: one module name per line, for the shell loop to consume
    names = svd.module_names(p1)
    rest = names
    while rest ~= "" do
        c = strfind(rest, ",")
        if c == nil then
            nm = rest
            rest = ""
        else
            nm = strsub(rest, 1, c - 1)
            rest = strsub(rest, c + 1)
        end
        while strsub(nm, 1, 1) == " " do nm = strsub(nm, 2) end
        if nm ~= "" then print("MODULE " .. nm) end
    end
    return
end

p2 = svd.parse(fe, "", {srcfile})

-- GND/VCC and other Xilinx primitives have no body in the source; without the
-- models every verdict is INCONCLUSIVE on GND:GND.  augment_xil_models covers
-- GND, which expand_fpga does not.
p1 = svd.augment_xil_models(p1)
p2 = svd.augment_xil_models(p2)

a = svd.prep_for_z3(svd.pick(p1, target))
b = svd.prep_for_z3(svd.pick(p2, target))
print("NULLVERDICT " .. target .. " " .. svd.miter(a, b))
