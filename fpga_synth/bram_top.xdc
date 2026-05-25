## Minimal pin constraints for the BRAM-exerciser validation
## (test_mem_fpga bram_top): clk + led[3:0], sonata xc7a50t pins.
set_property PACKAGE_PIN P15 [get_ports clk]
set_property IOSTANDARD LVCMOS33 [get_ports clk]
create_clock -period 10.0 [get_ports clk]

set_property PACKAGE_PIN K6 [get_ports led__0]
set_property PACKAGE_PIN L1 [get_ports led__1]
set_property PACKAGE_PIN M1 [get_ports led__2]
set_property PACKAGE_PIN K3 [get_ports led__3]
set_property IOSTANDARD LVCMOS33 [get_ports led__0]
set_property IOSTANDARD LVCMOS33 [get_ports led__1]
set_property IOSTANDARD LVCMOS33 [get_ports led__2]
set_property IOSTANDARD LVCMOS33 [get_ports led__3]
