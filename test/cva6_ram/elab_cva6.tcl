# Modified copy of cva6's corev_apu/fpga/scripts/run.tcl that stops after
# RTL elaboration and writes the netlist for our equivalence pipeline.
#
# Differences from upstream run.tcl:
#   - Always BOARD=genesys2 (constraints don't matter for elaboration).
#   - No `read_ip` (the Xilinx IPs aren't on this dev box and
#     synth_design -rtl doesn't need them; ariane_xilinx instantiates
#     them as black boxes for elaboration purposes).
#   - Drops the rv_tracer files (their lzc.sv breaks compile order
#     because it references cf_math_pkg before that package loads).
#   - Skips `launch_runs synth_1` etc. — the fpga build's synth_design
#     -rtl is the step we equivalence-check.
#   - Allows `top` to be overridden via the TOP env var (defaults to
#     ariane_xilinx, the actual fpga top). For sub-module studies (e.g.,
#     cva6_icache) you want a wrapper providing concrete parameters.
#
# Usage:
#   TOP=ariane_xilinx OUT_BASE=cva6_full \
#     vivado -mode batch -source elab_cva6.tcl

set top      [expr {[info exists ::env(TOP)]      ? $::env(TOP)      : "ariane_xilinx"}]
set out_base [expr {[info exists ::env(OUT_BASE)] ? $::env(OUT_BASE) : "${top}_elab"}]
set project  ariane

set cva6_root [file normalize "$::env(HOME)/cva6"]
set fpga_dir  "${cva6_root}/corev_apu/fpga"
set proj_dir  "[pwd]/cva6_${top}_proj"

create_project -force cva6_${top} ${proj_dir} -part xc7k325tffg900-2

# Make path resolution work — cva6's add_sources.tcl uses paths anchored
# at corev_apu/fpga.
cd ${fpga_dir}

# Same constraints/include_dirs as run.tcl (genesys2 default).
add_files -fileset constrs_1 -norecurse constraints/genesys-2.xdc

set_property include_dirs { \
    "src/axi_sd_bridge/include" \
    "../../vendor/pulp-platform/common_cells/include" \
    "../../vendor/pulp-platform/axi/include" \
    "../../core/cache_subsystem/hpdcache/rtl/include" \
    "../register_interface/include" \
    "../instr_tracing/ITI/include" \
    "../../core/include" \
} [current_fileset]

source scripts/add_sources.tcl

# rv_tracer ships its own lzc.sv that's a duplicate of common_cells/lzc.sv.
# Vivado picks the duplicate that loads first; the rv_tracer copy fails
# because it references cf_math_pkg before that package has compiled.
# Remove just the rv_tracer's lzc.sv (keep the rest of rv_tracer — the
# fpga top uses iti_pkg/te_pkg/encap_pkg from it).
foreach drop [get_files -of_objects [current_fileset] {*rv_tracer-main/rtl/lzc.sv}] {
    remove_files $drop
}

# Board-specific defines (genesys2). Pulled in as global include so they
# affect compilation of all SV sources.
read_verilog -sv {src/genesysii.svh ../../vendor/pulp-platform/common_cells/include/common_cells/registers.svh}
set file "src/genesysii.svh"
set registers "../../vendor/pulp-platform/common_cells/include/common_cells/registers.svh"
set file_obj [get_files -of_objects [get_filesets sources_1] \
                  [list "*$file" "$registers"]]
set_property -dict { file_type {Verilog Header} is_global_include 1} -objects $file_obj

update_compile_order -fileset sources_1

set_property top ${top} [current_fileset]
update_compile_order -fileset sources_1

# RTL elaboration — same step the FPGA build does as `synth_design -rtl
# -name rtl_1` before launching synth_1. We stop here.
synth_design -rtl -name rtl_${top} -top ${top}

# Restore CWD so output paths are relative to the test dir.
cd [file dirname [info script]]
write_edif    -force ${out_base}.edf
write_verilog -force ${out_base}.v
write_vhdl    -force ${out_base}.vhd

puts "============================================================"
puts "  RTL elaboration complete. Outputs:"
puts "    ${out_base}.edf"
puts "    ${out_base}.v"
puts "    ${out_base}.vhd"
puts "============================================================"
exit
