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

-- 1. Parse the whole design with Verible.
prog = svd.parse("verible", TOP, FILES)
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

-- 3. Gate-map the child (BIR -> AIG -> LUT cover -> Hardcaml Circuit),
--    dump as cell-mapped Verilog, re-parse via ver_front. This is the
--    behavioural -> structural transition.
child_pick = svd.pick(child_prog, CHILD)
mapped     = svd.gate_map(child_pick, 6, 0)  -- k_lut=6, io=false
cells_v    = svd.write_cellmapped_v(mapped, OUTDIR .. "/" .. CHILD .. "_cells.v")
print("  wrote " .. cells_v)
struct_prog = svd.parse_v_cells(cells_v)
print("  re-parsed cell-mapped: " .. svd.module_names(struct_prog))

-- 4. Splice the structural child back under the wrapper, flatten
--    structurally (primitive instances in the wrapper survive),
--    write nextpnr JSON.
merged = svd.splice(prog, CHILD, struct_prog)
flat   = svd.flatten_struct(merged, TOP)
out    = svd.write_nextpnr_json(flat, OUTDIR .. "/" .. TOP .. ".json")
print("  wrote " .. out)
