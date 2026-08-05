-- recipes/reg_name_compare.lua
--
-- Dump the REGISTER NAMES of one module, as read by one front end, so two
-- front ends' reads of the SAME design can be diffed by name.
--
-- WHY.  A Vivado `synth_design -rtl` elaboration is worth using as an oracle
-- precisely because it preserves register names from the source (against a
-- fully SYNTHESISED netlist the miter paired only 319 of 22835 variables).
-- But "preserves names" is a claim to MEASURE, not assume: if the names do not
-- coincide with the other flow's, the miter has to pair state by simulation or
-- not at all, and every verdict before that is noise.  So count the overlap
-- first.
--
--   sv_suite script reg_name_compare.lua <frontend> <module> <file> [file ...]
--
-- Prints one "<name>:<width>" per line, prefixed REG, plus a REGCOUNT summary.
-- The caller diffs two runs.  Widths are included because a name that matches
-- at the wrong width is not a correspondence, it is a coincidence.

if ARGN < 3 then
    print("usage: sv_suite script reg_name_compare.lua <frontend> <module> <file>...")
    error("missing args")
end

fe     = ARGV[1]
target = ARGV[2]

files = {}
i = 3
n = 1
while i <= ARGN do
    files[n] = ARGV[i]
    n = n + 1
    i = i + 1
end

-- verible and friends want a top hint; the VHDL reader ignores it.
p = svd.parse(fe, target, files)

m = svd.pick(p, target)
print("REGCOUNT " .. fe .. " " .. target .. " " .. svd.register_analyse(m))
print(svd.register_names(m))
