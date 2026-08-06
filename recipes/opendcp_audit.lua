-- recipes/opendcp_audit.lua
--
-- Structural audit of a reconstructed physical database, and a cross-format
-- cell census.
--
--   sv_suite script opendcp_audit.lua check   <ours.xml>
--   sv_suite script opendcp_audit.lua compare <golden.xml> <ours.xml>
--   sv_suite script opendcp_audit.lua census  <file>            (.xml/.json/.edf)
--   sv_suite script opendcp_audit.lua census2 <fileA> <fileB>
--
-- Why this is a recipe and not another throwaway script: the DCP that
-- RapidWright's json2dcp rebuilds from a nextpnr routed JSON used to segfault
-- Vivado, and each diagnosis was a one-off that counted tokens.  Two of those
-- counts were wrong -- '->' in the nextpnr ROUTING string counted SITEWIRE
-- (intra-site) entries as PIPs and claimed a quarter of the routing was lost
-- when the real figure was ~2%; another attributed pips to the wrong tile and
-- reported matching routes as missing.  Both sent the search after the wrong
-- subsystem.  The checks behind `check` are structural and name the offending
-- cell or net, so a finding is actionable and a clean run means something.

if ARGN < 2 then
    print("usage: sv_suite script opendcp_audit.lua <check|compare|census|census2> <file> [file2]")
    error("missing args")
end

mode = ARGV[1]

if mode == "check" then
    print(svd.opendcp_check(ARGV[2]))
elseif mode == "compare" then
    if ARGN < 3 then error("compare needs two files") end
    print(svd.opendcp_compare(ARGV[2], ARGV[3]))
elseif mode == "census" then
    print(svd.cell_census(ARGV[2]))
elseif mode == "census2" then
    if ARGN < 3 then error("census2 needs two files") end
    print(svd.cell_census_compare(ARGV[2], ARGV[3]))
else
    error("unknown mode " .. mode)
end
