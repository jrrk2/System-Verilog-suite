current_design cascade_mac
create_clock -name clk -period 4.0 [get_ports clk]
set_input_delay 0.5 -clock clk [get_ports {ena dclr}]
set_input_delay 0.5 -clock clk [get_ports {din[*] coef[*]}]
set_output_delay 0.5 -clock clk [get_ports {result[*]}]
