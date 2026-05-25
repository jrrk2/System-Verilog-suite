## Sonata board (xc7a50tcsg324-1) pin constraints for the fpga_synth
## validation design (test_sonata). Pins from picosoc_demo/sonata.pcf;
## port names are 1-bit (clk, led__N) to match the bit-blasted netlist.

set_property PACKAGE_PIN P15 [get_ports clk]
set_property IOSTANDARD LVCMOS33 [get_ports clk]
create_clock -period 40.0 [get_ports clk]

set_property PACKAGE_PIN K6 [get_ports led__0]
set_property PACKAGE_PIN L1 [get_ports led__1]
set_property PACKAGE_PIN M1 [get_ports led__2]
set_property PACKAGE_PIN K3 [get_ports led__3]

set_property IOSTANDARD LVCMOS33 [get_ports led__0]
set_property IOSTANDARD LVCMOS33 [get_ports led__1]
set_property IOSTANDARD LVCMOS33 [get_ports led__2]
set_property IOSTANDARD LVCMOS33 [get_ports led__3]
