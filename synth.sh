yosys -p "read_verilog -sv $*; synth; techmap; write_verilog"
