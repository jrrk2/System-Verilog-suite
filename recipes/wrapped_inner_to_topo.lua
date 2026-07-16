-- recipes/wrapped_inner_to_topo.lua
--
-- TOPOGRAPHICAL variant of wrapped_inner_to_nextpnr.lua.  Steps 1-5 are
-- identical (Verible -> internal opt -> Xilinx gate-map -> nextpnr JSON), but
-- instead of handing the JSON to nextpnr for placement + routing, we run the
-- SVS route-length-aware topographical placer (pack_to_lef recognition +
-- place_lef) and hand nextpnr ONLY the legalisation + routing job via
-- per-primitive BEL stamps.  This gives placement the semantic recognition
-- (LUT+FF slice packing, carry-chain columns) that nextpnr's soup placer lacks.
--
-- Globals (as wrapped_inner_to_nextpnr.lua):
--   TOP, CHILD, FILES, OUTDIR   (required)
--   CHIPDB, XDC                 (required for the pack->place->bit tail)
--   FLOORPLAN                   (optional; default /tmp/virtex7_floorplan.json)

print("recipe: wrapped_inner_to_topo")
print("  TOP=" .. TOP .. "  CHILD=" .. CHILD .. "  OUTDIR=" .. OUTDIR)

local SVS = "/home/jonathan/System-Verilog-suite"

-- 1-5: Verible parse (wrapper keeps primitive instances) + child behavioural
--      pipeline + Xilinx gate-map + splice + flatten + nextpnr JSON.
prog = svd.parse("verible-ext", TOP, FILES)
print("  parsed: " .. svd.module_names(prog))
child_prog = svd.parse("verible", CHILD, FILES)
child_prog = svd.unroll(child_prog)
child_prog = svd.inline(child_prog)
child_prog = svd.iflift(child_prog)
child_prog = svd.blocking_subst(child_prog)
child_prog = svd.meminfer(child_prog)
child_prog = svd.memlower(child_prog)
-- Recognise shift-register chains and map them onto SRL16E/SRLC32E instead of
-- bit-blasting into FF chains (matches Vivado's SRL inference; big FF savings in
-- PCS/PMA wait-counters and any distributed-RAM-free delay lines).
child_prog = svd.srl_infer(child_prog)
-- FPGA arch choice: lift attributed adder/mul subcells to abstract BAdd/BMul so
-- gate_map lowers them onto CARRY4/DSP (the one FPGA choice).  Set
-- ARCH_SUBST_FPGA=1 in the environment to make the lift cert-free.
child_prog = svd.arch_subst(child_prog)
child_pick = svd.pick(child_prog, CHILD)
mapped     = svd.gate_map(child_pick, 6, 0)
struct_prog = svd.mapped_to_prog(mapped)
merged = svd.splice(prog, CHILD, struct_prog)
flat   = svd.flatten_struct(merged, TOP)
local json = OUTDIR .. "/" .. TOP .. ".json"
svd.write_nextpnr_json(flat, json)
print("  wrote " .. json)

-- 6: topographical pack + place (recognition -> route-length placement) ->
--    per-primitive BEL stamps, then legalise + route + bitstream via nextpnr.
if CHIPDB ~= nil and XDC ~= nil then
    local fp     = FLOORPLAN or "/tmp/virtex7_floorplan.json"
    local placed = OUTDIR .. "/" .. TOP .. "_placed.txt"
    local bels   = OUTDIR .. "/" .. TOP .. "_bels.txt"
    local bit    = OUTDIR .. "/" .. TOP .. "_topo.bit"
    print("  topographical pack + place")
    execute("PLACED_OUT=" .. placed .. " BELS_OUT=" .. bels .. " " ..
            SVS .. "/_build/default/place_lef.exe " .. fp .. " " .. json)
    print("  legalise (nextpnr route) + bitstream")
    execute(SVS .. "/xilinx_lef/topo_legalize.sh " ..
            json .. " " .. bels .. " " .. XDC .. " " .. bit)
    print("  emitted " .. bit)
end
