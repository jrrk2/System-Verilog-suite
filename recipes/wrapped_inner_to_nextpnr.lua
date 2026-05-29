-- recipes/wrapped_inner_to_nextpnr.lua
--
-- Lua recipe replacing the test_telegraph_fpga round-trip pattern with
-- composable svd.* calls. The design under test is a `top` wrapper
-- that has primitive-cell instances (IBUFDS, BUFG, IBUF/OBUF, …) and a
-- child user module with behavioural body.  Goal: reach nextpnr-xilinx
-- JSON without any design-specific OCaml binary.
--
-- Caller defines four globals before `dofile`-ing this file:
--   TOP      string  -- wrapper module name (e.g. "top")
--   CHILD    string  -- inner module name (e.g. "telegraph_core")
--   FILES    table   -- list of source .v files (Verible front end)
--   OUTDIR   string  -- output dir (must already exist)
--
-- See recipes/example_uart_vc707.lua for a concrete invocation.
--
-- The same recipe with gate_map/write_*_json swapped for an ASIC
-- cell-mapper + EDIF/Liberty emitter drives the ASIC flow.

print("recipe: wrapped_inner_to_nextpnr")
print("  TOP="    .. TOP    .. "  CHILD=" .. CHILD)
print("  OUTDIR=" .. OUTDIR)

-- 1. Parse the whole design with Verible-ext so user-instantiated
--    primitive cells (IBUFDS_GTE2, BUFG, IBUF, OBUF, …) in the wrapper
--    are retained as binstances rather than being silently dropped
--    as "unresolved module references" (the historic miter default).
prog = svd.parse("verible-ext", TOP, FILES)
print("  parsed: " .. svd.module_names(prog))

-- 2. Run the generic behavioural pipeline on a child-only program.
--    The wrapper's primitive instances are not touched.
child_prog = svd.parse("verible", CHILD, FILES)
child_prog = svd.unroll(child_prog)
child_prog = svd.inline(child_prog)
child_prog = svd.iflift(child_prog)
child_prog = svd.blocking_subst(child_prog)
child_prog = svd.meminfer(child_prog)
child_prog = svd.memlower(child_prog)
print("  child pipeline done")

-- 3. Gate-map the child (BIR -> AIG -> LUT cover -> Hardcaml Circuit)
--    then lift the Mapped Circuit.t directly back into BIR via
--    hardcaml_to_behavioral.  The old route went through
--    write_cellmapped_v + ver_front re-parse, but Hardcaml's Verilog
--    emitter flattens bus ports into `<port>__<i>` scalars and ver_front
--    re-parsed them as 1-bit signals — collapsing CARRY4.DI/S to 1 bit
--    and crashing nextpnr-xilinx's carry packer (tasks #38/#39).  The
--    direct Circuit.t -> bmodule path keeps bus widths vector all the
--    way through.
child_pick  = svd.pick(child_prog, CHILD)
mapped      = svd.gate_map(child_pick, 6, 0)  -- k_lut=6, io=false
struct_prog = svd.mapped_to_prog(mapped)
print("  Mapped -> BIR: " .. svd.module_names(struct_prog))

-- 4. Splice the structural child back under the wrapper, flatten
--    structurally (primitive instances in the wrapper survive),
--    write nextpnr JSON.
merged = svd.splice(prog, CHILD, struct_prog)
flat   = svd.flatten_struct(merged, TOP)
out    = svd.write_nextpnr_json(flat, OUTDIR .. "/" .. TOP .. ".json")
print("  wrote " .. out)
