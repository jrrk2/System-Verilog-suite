## Arty Z7-20 (xc7z020clg400-1) pin constraints for the fpga_synth
## validation design (test_artyz7). Port names are 1-bit (clk, btn__N,
## led__N) to match the bit-blasted netlist the suite emits.

set_property PACKAGE_PIN H16 [get_ports clk]
set_property IOSTANDARD LVCMOS33 [get_ports clk]
create_clock -period 8.0 [get_ports clk]

set_property PACKAGE_PIN D19 [get_ports btn__0]
set_property PACKAGE_PIN D20 [get_ports btn__1]
set_property PACKAGE_PIN L20 [get_ports btn__2]
set_property PACKAGE_PIN L19 [get_ports btn__3]

set_property PACKAGE_PIN R14 [get_ports led__0]
set_property PACKAGE_PIN P14 [get_ports led__1]
set_property PACKAGE_PIN N16 [get_ports led__2]
set_property PACKAGE_PIN M14 [get_ports led__3]

set_property IOSTANDARD LVCMOS33 [get_ports btn__0]
set_property IOSTANDARD LVCMOS33 [get_ports btn__1]
set_property IOSTANDARD LVCMOS33 [get_ports btn__2]
set_property IOSTANDARD LVCMOS33 [get_ports btn__3]
set_property IOSTANDARD LVCMOS33 [get_ports led__0]
set_property IOSTANDARD LVCMOS33 [get_ports led__1]
set_property IOSTANDARD LVCMOS33 [get_ports led__2]
set_property IOSTANDARD LVCMOS33 [get_ports led__3]
