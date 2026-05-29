-- recipes/example_uart_vc707.lua
--
-- Concrete invocation of the wrapped_inner_to_nextpnr recipe for the
-- VC707 UART smoke test. The wrapper instantiates IBUFDS_GTE2 + BUFG
-- + IBUF/OBUF + a single child (the UART core).

TOP    = "top"
CHILD  = "sonata_top"      -- legacy module name in the existing sources
FILES  = {
    "/home/jonathan/demo-projects/vc707_telegraph/vc707_telegraph.v",
    "/home/jonathan/demo-projects/vc707_telegraph/telegraph.v"
}
OUTDIR = "/home/jonathan/telegraph_build/lua"

-- Make sure the output dir exists.
execute("mkdir -p " .. OUTDIR)

dofile("/home/jonathan/System-Verilog-suite/recipes/wrapped_inner_to_nextpnr.lua")
