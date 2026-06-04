-- recipes/counter28_vc707/recipe.lua
--
-- Slowed-LFSR sibling of the counter25 VC707 example, used to confirm
-- the SVS-flow #65 (FDSE INIT) and #106 (LUT5 OBUF buffer -> LUT6
-- workaround) fixes on hardware at a Johnson-advance rate slow enough
-- for the human eye to resolve (~1 Hz vs the 25-bit's ~6 Hz, both
-- before any residual silicon-rate inflation).
--
-- Run from this directory:
--   sv_suite script recipe.lua
--
-- Then feed top.json through nextpnr-xilinx -> prjxray -> openFPGALoader
-- as the regular counter25 example does.  The visible Johnson sequence
-- on the eight VC707 LEDs is the hardware-level regression for the SVS
-- frontend's open-flow correctness.

local here = "/home/jonathan/System-Verilog-suite/recipes/counter28_vc707"

TOP    = "top"
CHILD  = "counter25_core"
FILES  = {
    here .. "/top.v",
    here .. "/counter25_core.v",
}
OUTDIR = here

execute("mkdir -p " .. OUTDIR)

dofile("/home/jonathan/System-Verilog-suite/recipes/wrapped_inner_to_nextpnr.lua")
