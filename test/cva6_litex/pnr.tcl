# Vivado place and route of the SVS-synthesised LiteX CVA6 SoC.
# Filled in by run.sh (@EDIF@ @XDC@ @TOP@ @PART@ @JOBS@).  Prints one
# `PNR <step> ...` line per step so the log can be grepped; a failing step
# prints `PNR FAIL <step>: <message>` and exits 1.

set_param general.maxThreads @JOBS@

proc step {name body} {
  puts "PNR start $name"
  if {[catch {uplevel 1 $body} msg]} {
    puts "PNR FAIL $name: $msg"
    exit 1
  }
}

proc crit {} { return [get_msg_config -count -severity {CRITICAL WARNING}] }

step read {
  read_edif @EDIF@
  read_xdc @XDC@
}
step link {
  link_design -top @TOP@ -part @PART@
}
# constraints that did not apply (a net or cell the SVS netlist names
# differently from Vivado's own synthesis) surface as critical warnings here
puts "PNR link: [crit] critical warnings so far"
report_utilization -file util_link.rpt
report_methodology -file methodology_link.rpt

step opt   { opt_design }
step place { place_design }
step physopt_place { phys_opt_design }
report_utilization -file util_place.rpt
report_utilization -hierarchical -file util_place_hier.rpt
write_checkpoint -force @TOP@_place.dcp

step route { route_design }
step physopt_route { phys_opt_design }
write_checkpoint -force @TOP@_route.dcp

report_route_status -file route_status.rpt
report_timing_summary -max_paths 20 -file timing_summary.rpt
report_clock_utilization -file clock_util.rpt
report_drc -file drc.rpt

set wns [get_property SLACK [get_timing_paths -max_paths 1 -nworst 1 -setup]]
set whs [get_property SLACK [get_timing_paths -max_paths 1 -nworst 1 -hold]]
set unrouted [llength [get_nets -hier -filter {ROUTE_STATUS == UNROUTED}]]
puts "PNR timing: WNS $wns ns, WHS $whs ns"
puts "PNR route: $unrouted unrouted net(s)"
foreach t {LUT FF RAMB36 RAMB18 DSP48E1} {
  set n [llength [get_cells -hier -filter "PRIMITIVE_GROUP == $t || PRIMITIVE_SUBGROUP == $t || REF_NAME =~ ${t}*"]]
  puts "PNR util: $t $n"
}
puts "PNR critical warnings: [crit]"

step bitstream { write_bitstream -force @TOP@.bit }
puts "PNR done: [file normalize @TOP@.bit]"
exit 0
