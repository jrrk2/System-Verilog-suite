# Batch-synthesise sv-tests through Vivado in a single session.
#
# Invoked from vivado_baseline.sh as:
#   vivado -mode batch -nojournal -nolog \
#     -source vivado_batch_synth.tcl \
#     -tclargs <manifest.tsv> <results.csv>
#
# manifest.tsv has one TEST per line, tab-separated:
#   <test_name> \t <top_module> \t <flat_sv_path>
#
# results.csv gets one line per test:
#   <test_name>,<top>,<PASS|FAIL>,<elapsed_ms>,<error_excerpt>

set manifest [lindex $argv 0]
set out_csv  [lindex $argv 1]

set fp  [open $manifest r]
# Append, not truncate — lets the wrapper restart after a Vivado
# crash and pick up where we left off.
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
    set result "PASS"
    set err ""

    # Pre-record this test as CRASH so a Vivado segfault here gets
    # logged. We overwrite with the real verdict on success/clean fail.
    set crash_marker [open ${out_csv}.in_progress w]
    puts $crash_marker "$name"
    close $crash_marker

    # Reset state first — every test starts clean.
    catch { close_design  -quiet }
    catch { close_project -quiet }

    # synth_design needs a place to put things; use a fresh in-memory
    # project per test so the netlist database doesn't accumulate.
    catch { create_project -in_memory -part $part } cp_err
    if {[catch {
        read_verilog -sv $sv
        synth_design -rtl -top $top -part $part
    } errmsg]} {
        set result "FAIL"
        # Trim newlines, commas, quotes for CSV safety
        set err [string map {"\n" " | " "," ";" "\"" "'"} $errmsg]
        if {[string length $err] > 200} {
            set err [string range $err 0 199]
        }
    } else {
        incr passed
    }

    set ms [expr {[clock milliseconds] - $t0}]
    puts $out "$name,$top,$result,$ms,\"$err\""
    flush $out

    # Test completed cleanly — clear the crash marker.
    catch { file delete ${out_csv}.in_progress }

    # Belt-and-braces cleanup before the next test.
    catch { close_design  -quiet }
    catch { close_project -quiet }

    if {$total % 50 == 0} {
        puts "  …processed $total tests, $passed passed"
        flush stdout
    }
}

puts "DONE: $passed / $total Vivado-synthesisable"
close $fp
close $out
exit 0
