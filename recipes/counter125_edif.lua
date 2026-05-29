-- recipes/counter125_edif.lua
--
-- Drive the counter125 wrapper through SVS gate-mapping and out via the
-- svd.write_edif Fpga_emit path.  The EDIF becomes a Vivado-readable
-- artefact for read_edif/link_design DRC validation — catches emitter
-- bugs (port widths, vector-port flattening, direction mismatches) that
-- nextpnr-xilinx accepts silently or crashes on.

TOP    = "counter125_core"
FILES  = { "/home/jonathan/counter125_build/counter125.v" }
OUTDIR = "/home/jonathan/counter125_build"

execute("mkdir -p " .. OUTDIR)

-- Parse + pick + gate-map only the inner module — same shape as
-- wrapped_inner_to_nextpnr.lua's inner pipeline.  Mirrors that recipe's
-- pick/gate_map sequence so the EDIF reflects the same Hardcaml
-- Circuit.t that the JSON path would emit.
local p   = svd.parse("verible", TOP, FILES)
local mp  = svd.pick(p, TOP)
local mc  = svd.gate_map(mp, 6, 0)     -- k=6 LUTs, io=false (inner only)

local edif_path = OUTDIR .. "/" .. TOP .. ".edif"
svd.write_edif(mc, edif_path)
print("wrote " .. edif_path)
