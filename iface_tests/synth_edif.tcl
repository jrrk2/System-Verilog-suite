set sv  [lindex $argv 0]
set top [lindex $argv 1]
set out [lindex $argv 2]
read_verilog -sv $sv
synth_design -top $top -part xc7vx485tffg1761-2 -mode out_of_context
write_edif -force $out
