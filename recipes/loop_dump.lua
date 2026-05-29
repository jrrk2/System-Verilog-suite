-- recipes/loop_dump.lua
--
-- Replaces test_loop_dump.ml: dump the BIR after each pass to localize
-- where a combinational loop is introduced.  Like dump_bir.lua but
-- emits a separator-and-dump after every pass instead of only at the
-- end.
--
-- Caller globals:
--   TOP    string -- top-module name
--   FILES  table  -- source files
--   FRONTEND string (optional, default "verible")

fe = FRONTEND or "verible"
print("recipe: loop_dump  top=" .. TOP)

prog = svd.parse(fe, TOP, FILES)
m = svd.flatten_z3(prog, TOP)
prog = svd.owner(m)

passes = {"flatten_z3", "unroll", "inline", "iflift",
          "blocking_subst", "meminfer", "memlower"}

function dump(tag)
    print("")
    print("========== after " .. tag .. " ==========")
    mod = svd.pick(prog, TOP)
    print(svd.bir(mod))
end

dump("flatten_z3")
prog = svd.unroll(prog);          dump("unroll")
prog = svd.inline(prog);          dump("inline")
prog = svd.iflift(prog);          dump("iflift")
prog = svd.blocking_subst(prog);  dump("blocking_subst")
prog = svd.meminfer(prog);        dump("meminfer")
prog = svd.memlower(prog);        dump("memlower")
