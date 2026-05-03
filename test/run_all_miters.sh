#!/bin/bash
# Sweep every known standalone .sv testcase through every applicable
# miter and tally pass/fail per miter. Software-only miters always
# run; the Vivado EDIF↔SV miter is included only if --vivado is given.
#
# Suites swept:
#   test/regressions/   — minimal-shape regressions for each fix
#   test/edif_compare/  — small SV ground-truth examples
#   sysver_tests/       — UART hierarchy modules
#   test/unroll_inline/ — function/task/loop/RAM/ROM examples
#
# Miters applied:
#   vlt↔vrb  Verilator JSON ↔ Verible parse-tree (software-only)
#   yos↔vrb  Yosys RTLIL ↔ Verible parse-tree (software-only)
#   xrtl     Vivado EDIF/VHDL ↔ original SV (--vivado only)

here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/.." && pwd)

include_vivado=0
[ "$1" = "--vivado" ] && include_vivado=1

vlt_vrb=$repo/_build/default/test_verilator_vs_verible.exe
yos_vrb=$repo/_build/default/test_yosys_vs_verible.exe
xrtl=$repo/_build/default/test_xilinx_rtl_miter.exe
vivado=/NFS/apps/Xilinx/Vivado/2020.1/bin/vivado

( cd "$repo" && dune build test_verilator_vs_verible.exe test_yosys_vs_verible.exe )
[ $include_vivado = 1 ] && ( cd "$repo" && dune build test_xilinx_rtl_miter.exe )

tally_dir=$(mktemp -d); trap "rm -rf $tally_dir" EXIT
for label in vlt_vrb yos_vrb xrtl; do
    : > "$tally_dir/${label}_pass"; : > "$tally_dir/${label}_fail"; : > "$tally_dir/${label}_skip"
done
inc()   { echo x >> "$tally_dir/${1}_${2}"; }
count() { wc -l < "$tally_dir/${1}_${2}" | tr -d ' '; }

# Top name from filename (strip dir + .sv).
top_of() { basename "$1" .sv; }

run_pair() {
    local label=$1 exe=$2 top=$3 sv=$4 deps=$5 logdir=$6
    if [ ! -x "$exe" ]; then inc "$label" skip; return; fi
    local log=$logdir/${top}_${label}.log
    if "$exe" "$top" "$sv" $deps > "$log" 2>&1; then
        inc "$label" pass
    else
        inc "$label" fail
    fi
}

run_xrtl() {
    local top=$1 sv=$2 logdir=$3
    if [ ! -x "$xrtl" ] || [ ! -x "$vivado" ]; then inc xrtl skip; return; fi
    local outbase=$logdir/${top}_elab
    local edf=${outbase}.edf vhd=${outbase}.vhd
    if [ ! -f "$vhd" ] || [ "$sv" -nt "$vhd" ]; then
        ( cd "$logdir" && "$vivado" -mode batch \
            -source "$repo/test/edif_compare/elab.tcl" \
            -tclargs "$sv" "$top" "$edf" ) > "$logdir/${top}_vivado.log" 2>&1 \
            || { inc xrtl fail; return; }
    fi
    [ ! -f "$vhd" ] && { inc xrtl fail; return; }
    "$xrtl" "$top" "$vhd" -- "$sv" > "$logdir/${top}_xrtl.log" 2>&1 \
        && inc xrtl pass || inc xrtl fail
}

# Per-suite deps map for the few SVs that instantiate sub-modules.
deps_for() {
    case "$1" in
        uart_receiver)
            echo "sysver_tests/slib_counter.sv sysver_tests/slib_mv_filter.sv \
                  sysver_tests/slib_input_filter.sv" ;;
        *) echo "" ;;
    esac
}

logdir=$here/_logs_sweep
mkdir -p "$logdir"

if [ $include_vivado = 1 ]; then
    printf '%-12s  %-26s  %-8s  %-8s  %-8s\n' \
        suite test "vlt↔vrb" "yos↔vrb" "edif↔sv"
    printf '%s\n' "$(printf '%.0s-' {1..72})"
else
    printf '%-12s  %-26s  %-8s  %-8s\n' suite test "vlt↔vrb" "yos↔vrb"
    printf '%s\n' "$(printf '%.0s-' {1..62})"
fi

run_suite() {
    local suite=$1; shift
    for sv in "$@"; do
        local top=$(top_of "$sv")
        case "$top" in rtl_primitives|stubs|apb_uart) continue;; esac
        local deps=$(deps_for "$top")
        run_pair vlt_vrb "$vlt_vrb" "$top" "$sv" "$deps" "$logdir"
        run_pair yos_vrb "$yos_vrb" "$top" "$sv" "$deps" "$logdir"
        local res_v=$(grep -q ok "$tally_dir/markers" 2>/dev/null; echo)
        # Print last per-test result by reading the log.
        local rv=$(tail -3 "$logdir/${top}_vlt_vrb.log" 2>/dev/null | grep -oE "FORMALLY EQUIVALENT|NOT EQUIVALENT|Input interfaces|Output interfaces|Sorts|int_of_string|Failure|Fatal" | head -1)
        local ry=$(tail -3 "$logdir/${top}_yos_vrb.log" 2>/dev/null | grep -oE "FORMALLY EQUIVALENT|NOT EQUIVALENT|Input interfaces|Output interfaces|Sorts|int_of_string|Failure|Fatal" | head -1)
        local sv_mark="❌"; [ "$rv" = "FORMALLY EQUIVALENT" ] && sv_mark="✅"
        local sy_mark="❌"; [ "$ry" = "FORMALLY EQUIVALENT" ] && sy_mark="✅"
        if [ $include_vivado = 1 ]; then
            run_xrtl "$top" "$sv" "$logdir"
            local rx=$(tail -3 "$logdir/${top}_xrtl.log" 2>/dev/null | grep -oE "FORMALLY EQUIVALENT|NOT EQUIVALENT" | head -1)
            local sx_mark="❌"; [ "$rx" = "FORMALLY EQUIVALENT" ] && sx_mark="✅"
            printf '%-12s  %-26s  %-8s  %-8s  %-8s\n' "$suite" "$top" "$sv_mark" "$sy_mark" "$sx_mark"
        else
            printf '%-12s  %-26s  %-8s  %-8s\n' "$suite" "$top" "$sv_mark" "$sy_mark"
        fi
    done
}

run_suite regressions    "$here"/regressions/*.sv
run_suite edif_compare   "$here"/edif_compare/*.sv
run_suite sysver_uart    "$repo"/sysver_tests/slib_*.sv "$repo"/sysver_tests/uart_*.sv
run_suite unroll_inline  "$here"/unroll_inline/*.sv

echo
echo "=== Summary ==="
echo "Verilator↔Verible: $(count vlt_vrb pass) ✅  $(count vlt_vrb fail) ❌  $(count vlt_vrb skip) ⊘"
echo "Yosys↔Verible:     $(count yos_vrb pass) ✅  $(count yos_vrb fail) ❌  $(count yos_vrb skip) ⊘"
[ $include_vivado = 1 ] && \
    echo "Vivado EDIF↔SV:    $(count xrtl pass) ✅  $(count xrtl fail) ❌  $(count xrtl skip) ⊘"
