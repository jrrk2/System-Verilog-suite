#!/bin/bash
# Run every regression .sv through every available miter.
# By default: software-only (Verilator↔Verible + Yosys↔Verible).
# Pass --vivado to also include the Vivado EDIF↔SV miter (slow, needs Xilinx).
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
cd "$here"

include_vivado=0
[ "$1" = "--vivado" ] && include_vivado=1

vlt_vrb=$repo/_build/default/test_verilator_vs_verible.exe
yos_vrb=$repo/_build/default/test_yosys_vs_verible.exe
xrtl=$repo/_build/default/test_xilinx_rtl_miter.exe
vivado=/NFS/apps/Xilinx/Vivado/2020.1/bin/vivado

# Build whatever's needed.
( cd "$repo" && dune build test_verilator_vs_verible.exe test_yosys_vs_verible.exe )
[ $include_vivado = 1 ] && ( cd "$repo" && dune build test_xilinx_rtl_miter.exe )

# Tally state. Use plain files since bash subshells in $(...) can't
# update parent-shell variables.
tally_dir=$(mktemp -d)
trap "rm -rf $tally_dir" EXIT
for label in vlt_vrb yos_vrb xrtl; do
    : > "$tally_dir/${label}_pass"
    : > "$tally_dir/${label}_fail"
done
inc()  { echo x >> "$tally_dir/${1}_${2}"; }
count() { wc -l < "$tally_dir/${1}_${2}" | tr -d ' '; }

run_miter() {
    local label=$1 exe=$2 top=$3 sv=$4 logdir=$5
    local log=$logdir/${top}_${label}.log
    if "$exe" "$top" "$sv" > "$log" 2>&1; then
        inc "$label" pass
        echo "✅"
    else
        inc "$label" fail
        local why
        why=$(grep -m1 -E "Input interfaces differ|Output interfaces differ|Sorts.*incompatible|int_of_string|❌ NOT EQUIVALENT|Fatal" "$log" | head -c 60)
        echo "❌ $why"
    fi
}

run_vivado_miter() {
    local top=$1 sv=$2 logdir=$3
    local outbase=$logdir/${top}_elab
    local edf=${outbase}.edf vhd=${outbase}.vhd
    local vlog=$logdir/${top}_vivado.log
    if [ ! -f "$vhd" ] || [ "$sv" -nt "$vhd" ]; then
        "$vivado" -mode batch -source "$repo/test/edif_compare/elab.tcl" \
            -tclargs "$sv" "$top" "$edf" > "$vlog" 2>&1 || { echo "vivado-fail"; return; }
    fi
    [ ! -f "$vhd" ] && { echo "no-vhd"; return; }
    local mlog=$logdir/${top}_xrtl.log
    if "$xrtl" "$top" "$vhd" -- "$sv" > "$mlog" 2>&1; then
        inc xrtl pass; echo "✅"
    else
        inc xrtl fail; echo "❌"
    fi
}

logdir=$here/_logs
mkdir -p "$logdir"

# Header
if [ $include_vivado = 1 ]; then
    printf '%-30s  %-12s  %-12s  %-12s\n' \
        "test" "vlt↔vrb" "yos↔vrb" "edif↔sv"
    printf '%s\n' "$(printf '%.0s-' {1..72})"
else
    printf '%-30s  %-12s  %-12s\n' "test" "vlt↔vrb" "yos↔vrb"
    printf '%s\n' "$(printf '%.0s-' {1..58})"
fi

for sv in *.sv; do
    top=$(basename "$sv" .sv)
    printf '%-30s  ' "$top"
    res_vv=$(run_miter vlt_vrb "$vlt_vrb" "$top" "$sv" "$logdir")
    printf '%-12s  ' "$res_vv"
    res_yv=$(run_miter yos_vrb "$yos_vrb" "$top" "$sv" "$logdir")
    printf '%-12s' "$res_yv"
    if [ $include_vivado = 1 ]; then
        printf '  '
        res_xrtl=$(run_vivado_miter "$top" "$sv" "$logdir")
        printf '%-12s' "$res_xrtl"
    fi
    echo
done

echo
echo "=== Summary ==="
echo "Verilator↔Verible: $(count vlt_vrb pass) ✅  $(count vlt_vrb fail) ❌"
echo "Yosys↔Verible:     $(count yos_vrb pass) ✅  $(count yos_vrb fail) ❌"
[ $include_vivado = 1 ] && \
    echo "Vivado EDIF↔SV:    $(count xrtl pass) ✅  $(count xrtl fail) ❌"
