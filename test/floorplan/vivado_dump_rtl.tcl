# vivado_dump_rtl.tcl — minimal top-down `synth_design -rtl` over the
# pre-flattened TALOS-V2 sources, then dump every RTL_* cell grouped
# by its containing module specialisation.
#
# Self-contained — doesn't depend on the TALOS project, .xci IP files,
# constraints, or generated/. We feed Vivado the same flat .sv that
# the OCaml floorplanner already consumed, so both tools see the
# identical source state.
#
# Inputs (env / args):
#   VC707_FLAT      absolute path to talos_vc707_flat.sv
#   VC707_TOP       top module name (default: vc707_microgpt_eth)
#   VC707_PART      Xilinx part   (default: xc7vx485tffg1761-2)
#   VC707_OUT       output TSV    (default: ./vivado_rtl_cells.tsv)

set flat [expr {[info exists ::env(VC707_FLAT)] ? $::env(VC707_FLAT)
                                                : "talos_vc707_flat.sv"}]
set top  [expr {[info exists ::env(VC707_TOP)]  ? $::env(VC707_TOP)
                                                : "vc707_microgpt_eth"}]
set part [expr {[info exists ::env(VC707_PART)] ? $::env(VC707_PART)
                                                : "xc7vx485tffg1761-2"}]
set out  [expr {[info exists ::env(VC707_OUT)]  ? $::env(VC707_OUT)
                                                : "vivado_rtl_cells.tsv"}]

puts "INFO: flat=$flat  top=$top  part=$part  out=$out"

# In-memory project — no .xpr file on disk, no IPs, no constraints.
create_project -in_memory -part $part

read_verilog -sv $flat
set_property top $top [current_fileset]
update_compile_order -fileset sources_1

# Top-down RTL elaboration.  Stops before BRAM/DSP inference, so does
# not hit the hang the full synth has on this design.  In-memory
# projects don't carry named runs, so we skip the -name flag and use
# the resulting current_design directly without open_run.
synth_design -rtl

set fp [open $out w]
puts $fp "parent_module\tref_name\tdepth\twidth"

set top_name [get_property NAME [current_design]]

foreach c [get_cells -hierarchical] {
    set ref [get_property REF_NAME $c]
    if {![regexp {^RTL_(RAM|ROM|REG|ADD|SUB|MULT|MUX|AND|OR|XOR|NOT)} $ref]} {
        continue
    }
    set name [get_property NAME $c]
    set slash [string last "/" $name]
    set parent_ref $top_name
    if {$slash >= 0} {
        set parent_path [string range $name 0 [expr {$slash - 1}]]
        set parent_cells [get_cells $parent_path -quiet]
        if {[llength $parent_cells] > 0} {
            set parent_ref [get_property REF_NAME [lindex $parent_cells 0]]
        }
    }
    set depth 0
    set width 0
    catch {set depth [get_property DEPTH $c]}
    catch {set width [get_property WIDTH $c]}
    if {$depth == 0 && $width != 0} { set depth 1 }
    puts $fp "$parent_ref\t$ref\t$depth\t$width"
}
close $fp

puts "INFO: wrote $out"
exit 0
