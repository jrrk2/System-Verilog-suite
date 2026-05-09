# Vivado RTL elaboration of a smollm module — oracle for memlower decisions.
#
# Usage:
#   vivado -mode batch -source elab_smollm.tcl -tclargs <top> <out_dir>
#
# Reads all non-selftest .sv files from ~/TALOS-V2/rtl/vc707/src/smollm and
# the .svh includes from ~/TALOS-V2/rtl/generated, then runs synth_design -rtl
# (RTL elaboration) and writes the EDIF + Verilog + VHDL netlists.  The
# Verilog form makes Vivado's RAM inference visible — particularly the
# distinction between RTL_RAM (single-port BRAM), replicated banks, or
# bit-blasted RTL_REG.

set top     [lindex $argv 0]
set out_dir [lindex $argv 1]

file mkdir $out_dir

set src_dir   $::env(HOME)/TALOS-V2/rtl/vc707/src/smollm
set inc_dir   $::env(HOME)/TALOS-V2/rtl/generated
set proj_dir  ${out_dir}/${top}_elab

# Collect every .sv except the selftests (testbenches use unsynthesizable
# constructs that crash elaboration).
set sources [list]
foreach f [glob -nocomplain ${src_dir}/*.sv] {
    if {[string match "*_selftest.sv" $f]} { continue }
    lappend sources $f
}

create_project -force ${top}_elab ${proj_dir} -part xc7vx485tffg1761-2
foreach src $sources {
    add_files -norecurse $src
}
# .svh files are includes, not separate compilation units; expose via include path.
set_property include_dirs [list $inc_dir $src_dir] [current_fileset]
set_property top ${top} [current_fileset]

# Generic RTL_* primitives (no LUT mapping yet) — easier to read for
# inference questions.
synth_design -rtl -name rtl_1 -top ${top} -part xc7vx485tffg1761-2

write_edif    -force ${out_dir}/${top}.edf
write_verilog -force ${out_dir}/${top}.v
exit
