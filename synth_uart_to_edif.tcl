# Vivado TCL script to synthesize UART VHDL design to EDIF netlist
# Usage: vivado -mode tcl < synth_uart_to_edif.tcl

create_project -force project_uart_synth . -part xc7a35tcpg236-3

# Add VHDL source files
add_files -norecurse sysver_tests/slib_clock_div.vhd
add_files -norecurse sysver_tests/slib_counter.vhd
add_files -norecurse sysver_tests/slib_edge_detect.vhd
add_files -norecurse sysver_tests/slib_fifo.vhd
add_files -norecurse sysver_tests/slib_input_filter.vhd
add_files -norecurse sysver_tests/slib_input_sync.vhd
add_files -norecurse sysver_tests/slib_mv_filter.vhd
add_files -norecurse sysver_tests/uart_baudgen.vhd
add_files -norecurse sysver_tests/uart_interrupt.vhd
add_files -norecurse sysver_tests/uart_receiver.vhd
add_files -norecurse sysver_tests/uart_transmitter.vhd
add_files -norecurse sysver_tests/apb_uart.vhd

# Synthesize design
synth_design -top apb_uart -part xc7a35tcpg236-3

# Write EDIF netlist
write_edif -force uart_synthesized.edf

# Also write checkpoint for debugging
write_checkpoint -force uart_post_synth.dcp

puts "Synthesis complete. EDIF netlist written to uart_synthesized.edf"
exit
