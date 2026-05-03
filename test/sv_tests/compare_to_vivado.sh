#!/bin/bash
# Quick comparison: Vivado-accepted set vs our three runners.
# Run AFTER both vivado_baseline.sh and run.sh have produced logs.

set -e
sv_tests=${SV_TESTS_DIR:-$HOME/sv-tests}
vivado_csv="$sv_tests/out/vivado_baseline.csv"
[ -f "$vivado_csv" ] || { echo "no $vivado_csv — run vivado_baseline.sh first"; exit 1; }

vivado_pass=$(awk -F, 'NR>1 && $3=="PASS" {print $1}' "$vivado_csv" | sort -u)
vivado_n=$(echo "$vivado_pass" | wc -l)

printf "==== Vivado-synthesisable subset (%d tests) ====\n\n" "$vivado_n"

for r in Decompiler_Verible_Parse Decompiler_Verilator_Parse Decompiler_Miter; do
    rdir="$sv_tests/out/logs/$r"
    [ -d "$rdir" ] || continue
    rpass=$(find "$rdir" -name '*.log' ! -size 0 \
            | xargs grep -l '^rc: 0' 2>/dev/null \
            | sed "s|$rdir/||;s|\\.sv\\.log$||;s|\\.log$||" | sort -u)
    both=$(comm -12 <(echo "$rpass") <(echo "$vivado_pass") | wc -l)
    only_us=$(comm -23 <(echo "$rpass") <(echo "$vivado_pass") | wc -l)
    only_v=$(comm -13 <(echo "$rpass") <(echo "$vivado_pass") | wc -l)
    pct=$(( both * 100 / (vivado_n > 0 ? vivado_n : 1) ))
    printf "  %-32s  %4d / %4d  (%2d%% of Vivado-synth)  ours-only=%d  Vivado-only=%d\n" \
        "$r" "$both" "$vivado_n" "$pct" "$only_us" "$only_v"
done

echo
echo "Tests Vivado accepts that our miter rejects (= room to grow):"
miter="$sv_tests/out/logs/Decompiler_Miter"
if [ -d "$miter" ]; then
    miter_pass=$(find "$miter" -name '*.log' ! -size 0 \
                 | xargs grep -l '^rc: 0' 2>/dev/null \
                 | sed "s|$miter/||;s|\\.sv\\.log$||;s|\\.log$||" | sort -u)
    comm -23 <(echo "$vivado_pass") <(echo "$miter_pass") | head -20
    n=$(comm -23 <(echo "$vivado_pass") <(echo "$miter_pass") | wc -l)
    echo
    echo "  ($n tests in this category — pick the smallest for converter triage)"
fi
