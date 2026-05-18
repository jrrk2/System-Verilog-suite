# Batch-elaborate sysver_tests/*.sv through Vivado synth_design -rtl.
# Reads a manifest (test \t top \t sv-path), writes <out>/<top>.v per
# success and a CSV row per test.
#
#   vivado -mode batch -nojournal -nolog \
#     -source sysver_vivado_batch.tcl \
#     -tclargs <manifest.tsv> <out_dir> <results.csv>

set manifest [lindex $argv 0]
set out_dir  [lindex $argv 1]
set out_csv  [lindex $argv 2]

set fp  [open $manifest r]
file mkdir $out_dir

set already_done [dict create]
if {[file exists $out_csv]} {
    set rfp [open $out_csv r]
    while {[gets $rfp line] >= 0} {
        if {$line eq "" || [string match "test,*" $line]} { continue }
        set name [lindex [split $line ","] 0]
        dict set already_done $name 1
    }
    close $rfp
    set out [open $out_csv a]
} else {
    set out [open $out_csv  w]
    puts $out "test,top,result,ms,error"
}

set part "xc7a35tcpg236-1"
set total 0
set passed 0

while {[gets $fp line] >= 0} {
    if {$line eq "" || [string index $line 0] eq "#"} { continue }
    set parts [split $line "\t"]
    if {[llength $parts] < 3} { continue }
    set name [lindex $parts 0]
    set top  [lindex $parts 1]
    set sv   [lindex $parts 2]
    if {[dict exists $already_done $name]} { continue }
    incr total
    set t0 [clock milliseconds]
    set elab "$out_dir/${name}_elab.vhd"
    set proj_dir "$out_dir/${name}_proj"
    set ok 0
    set err ""
    if {[catch {
        # In-progress marker so a crash resumes after this entry.
        set marker_fp [open "$out_csv.in_progress" w]
        puts $marker_fp $name
        close $marker_fp
        # Fresh project per design — avoids `read_verilog` file-pool
        # pollution where a syntax error in design N propagates into
        # design N+1's compile.  Modelled on test/edif_compare/elab.tcl.
        file delete -force $proj_dir
        create_project -force ${name}_elab $proj_dir -part $part -quiet
        add_files -norecurse $sv
        set_property top $top [current_fileset]
        synth_design -top $top -rtl -name rtl_${name} -part $part -quiet
        write_vhdl -force $elab
        close_project -quiet
        file delete -force $proj_dir
    } emsg]} {
        catch {close_project -quiet}
        catch {file delete -force $proj_dir}
        set err $emsg
    } else {
        set ok 1
    }
    file delete "$out_csv.in_progress"
    set t1 [clock milliseconds]
    set ms [expr {$t1 - $t0}]
    if {$ok} {
        incr passed
        puts $out "$name,$top,PASS,$ms,"
        puts "  PASS $name ($ms ms)"
    } else {
        # Trim error to first line for CSV friendliness.
        set first [lindex [split $err "\n"] 0]
        regsub -all "," $first ";" first
        puts $out "$name,$top,FAIL,$ms,\"$first\""
        puts "  FAIL $name ($ms ms): $first"
    }
    flush $out
}
close $fp
close $out
puts "==== $passed / $total passed ===="
