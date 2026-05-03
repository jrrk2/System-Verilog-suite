# Generic Vivado RTL elaboration to EDIF.
# synth_design -rtl produces a generic netlist of RTL_* primitives
# (RTL_INV, RTL_AND, RTL_REG, RTL_ADD, RTL_MUX, ...) without technology mapping.
#
# Usage: vivado -mode batch -source elab.tcl -tclargs <sources> <top> <out.edf>
#   <sources>: colon-separated list of .sv files (the top must be in this list)

set sources_arg [lindex $argv 0]
set top         [lindex $argv 1]
set out_edf     [lindex $argv 2]

set sources [split $sources_arg ":"]
set proj_dir [file rootname $out_edf]_proj

create_project -force ${top}_elab ${proj_dir} -part xc7a35tcpg236-3
foreach src $sources {
    add_files -norecurse $src
}
set_property top ${top} [current_fileset]

synth_design -rtl -name rtl_1 -top ${top} -part xc7a35tcpg236-3
write_edif -force ${out_edf}

# Also write a Verilog netlist of the elaborated design (consumed by the
# ver_front path).
set out_v [file rootname ${out_edf}].v
write_verilog -force ${out_v}

# And a VHDL netlist. VHDL forces every component to be declared, so this
# is more self-contained than the Verilog form (no implicit black boxes).
set out_vhd [file rootname ${out_edf}].vhd
write_vhdl -force ${out_vhd}
exit
