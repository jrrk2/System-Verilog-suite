-- recipes/verilator_parse.lua
--
-- Replaces test_verilator_behavioral.exe: parse a verilator-emitted
-- JSON tree, succeed (exit 0) iff at least one module is produced.
-- Used by the sv-tests Decompiler_Verilator_Parse runner and by
-- verify_interactive.lua's Verilator parse-check menu item.

if ARGN < 1 then
    print("usage: sv_suite script verilator_parse.lua <V*.tree.json>")
    error("missing args")
end

json_file = ARGV[1]
print("recipe: verilator_parse  file=" .. json_file)

-- top="" because verilator-emitted JSON is self-contained.
prog = svd.parse("verilator", "", {json_file})
names = svd.module_names(prog)
print("  modules: " .. names)

if names == "" or names == nil then
    error("no modules parsed")
end
print("OK")
