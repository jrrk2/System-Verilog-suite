-- recipes/frontend_probe.lua
--
-- One front end, one design: can it READ it, and what state does it see?
--
-- The unit of a front-end reliability census.  Coverage comes first: a reader
-- that cannot parse a file tells you nothing about it, so "which front end is
-- the reliable Z3 baseline" is only meaningful over the subset a candidate can
-- actually read.  Register count is the second axis -- two readers that agree
-- on the module but disagree on how much STATE it has cannot be mitered
-- against each other at all (unpaired state is free for the solver, so the
-- verdict is DIFFER whatever the logic does).
--
--   sv_suite script frontend_probe.lua <frontend> <top> <file> [file ...]
--
-- Prints exactly one machine-readable line:
--   PROBE <frontend> <top> ok    <n_modules> <n_registers>
-- and on failure nothing (the process may also die), so the DRIVER must treat
-- "no PROBE line" as failure rather than expecting a verdict.  That is
-- deliberate: several front ends abort the process on a read they dislike.

if ARGN < 3 then
    print("usage: sv_suite script frontend_probe.lua <frontend> <top> <file>...")
    error("missing args")
end

fe  = ARGV[1]
top = ARGV[2]

files = {}
i = 3
n = 1
while i <= ARGN do
    files[n] = ARGV[i]
    n = n + 1
    i = i + 1
end

p = svd.parse(fe, top, files)

-- count modules in the comma-separated listing
names = svd.module_names(p)
nm = 0
rest = names
while rest ~= "" do
    c = strfind(rest, ",")
    if c == nil then rest = "" else rest = strsub(rest, c + 1) end
    nm = nm + 1
end

m = svd.pick(p, top)
-- register_analyse returns "module <name>: <n> registers"; keep just the count
ra = svd.register_analyse(m)
c1 = strfind(ra, ": ")
nr = strsub(ra, c1 + 2)
c2 = strfind(nr, " ")
if c2 ~= nil then nr = strsub(nr, 1, c2 - 1) end

print("PROBE " .. fe .. " " .. top .. " ok " .. nm .. " " .. nr)
