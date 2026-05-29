-- recipes/dump_bir.lua
--
-- Replaces dump_picosoc_fsm.ml / dump_regs_bir.ml / dump_svparser.ml
-- / dump_verible_alu.ml / dump_synlig_bir.ml: parse, run the chosen
-- pipeline, dump the BIR for inspection.
--
-- Caller globals:
--   TOP     string  -- top-module name
--   FILES   table   -- source files
--   FRONTEND string -- parser frontend (default "verible")
--   PIPELINE table  -- ordered list of pass names; pass-through if nil
--                   --   {"unroll","inline","iflift","blocking_subst",
--                   --    "meminfer","memlower","ssa"}
--   OUT     string  -- output file (default "/tmp/bir.txt")

fe       = FRONTEND or "verible"
outpath  = OUT or "/tmp/bir.txt"
print("recipe: dump_bir  top=" .. TOP .. "  frontend=" .. fe)

prog = svd.parse(fe, TOP, FILES)
if PIPELINE then
    i = 1
    while PIPELINE[i] do
        pass = PIPELINE[i]
        if     pass == "unroll"         then prog = svd.unroll(prog)
        elseif pass == "inline"         then prog = svd.inline(prog)
        elseif pass == "iflift"         then prog = svd.iflift(prog)
        elseif pass == "blocking_subst" then prog = svd.blocking_subst(prog)
        elseif pass == "meminfer"       then prog = svd.meminfer(prog)
        elseif pass == "memlower"       then prog = svd.memlower(prog)
        elseif pass == "ssa"            then prog = svd.ssa(prog)
        elseif pass == "flatten"        then prog = svd.flatten(prog)
        elseif pass == "optimize"       then prog = svd.optimize(prog)
        else print("  unknown pass: " .. pass) end
        i = i + 1
    end
    print("  pipeline done")
end

m = svd.pick(prog, TOP)
text = svd.bir(m)
-- lua-ml doesn't expose openfile; recipe prints to stdout, redirect to
-- a file at the caller (e.g.  sv_suite script foo.lua > out.txt).
print(text)
