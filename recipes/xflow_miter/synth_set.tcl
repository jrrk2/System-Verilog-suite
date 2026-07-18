set part xc7vx485tffg1761-2
set setfile [lindex $argv 0]
set fh [open $setfile r]; set lines [split [read $fh] "\n"]; close $fh
foreach ln $lines {
  if {$ln eq ""} continue
  set p [split $ln "|"]; set name [lindex $p 0]; set top [lindex $p 1]; set file [lindex $p 2]
  puts "=== SYNTH $name (top=$top) ==="
  if {[catch {
    create_project -force -in_memory -part $part
    read_verilog -sv $file
    synth_design -top $top -part $part -mode out_of_context -flatten_hierarchy rebuilt
    write_verilog -force -mode design /tmp/eb/xf/${name}_viv.v
    close_project
    puts "OK $name"
  } err]} { puts "SYNTH_FAIL $name : $err"; catch {close_project} }
}
puts "SYNTH_BATCH_DONE"
