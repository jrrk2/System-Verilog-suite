#!/bin/bash
# Run the Z3 miter between two independent SV frontends across the
# sysver_tests/ corpus.  No Vivado involved.
#
# Frontends are passed as env vars (defaults: verible vs slang).
# Output: /tmp/sysver_two_fe/<A>_vs_<B>.csv plus per-test logs.

set -e
HERE=/home/jonathan/System-Verilog-suite
A=${A:-verible}
B=${B:-slang}
MITER=$HERE/_build/default/test_two_frontend_miter.exe
OUT=${OUT:-/tmp/sysver_two_fe}
mkdir -p $OUT/logs_${A}_vs_${B}
CSV=$OUT/${A}_vs_${B}.csv

[ -x "$MITER" ] || { echo "Build first: dune build test_two_frontend_miter.exe"; exit 1; }

echo "test,top,verdict,seconds" > $CSV
n=0
eq=0
diff=0
err=0
for sv in $HERE/sysver_tests/*.sv; do
    name=$(basename "$sv" .sv)
    top=$(grep -hE "^[[:space:]]*module[[:space:]]+[A-Za-z_][A-Za-z0-9_]*" "$sv" 2>/dev/null \
          | head -1 \
          | sed -E 's/^[[:space:]]*module[[:space:]]+([A-Za-z_][A-Za-z0-9_]*).*/\1/')
    [ -z "$top" ] && continue
    n=$((n+1))
    log=$OUT/logs_${A}_vs_${B}/${name}.log
    t0=$(date +%s)
    if timeout 60 "$MITER" "$top" "$sv" "$A" "$B" > "$log" 2>&1; then
        verdict=EQUIVALENT
        eq=$((eq+1))
    else
        rc=$?
        if [ $rc -eq 1 ]; then
            verdict=DIFFER
            diff=$((diff+1))
        elif [ $rc -eq 124 ]; then
            verdict=TIMEOUT
            err=$((err+1))
        else
            verdict=ERROR_rc$rc
            err=$((err+1))
        fi
    fi
    t1=$(date +%s)
    echo "$name,$top,$verdict,$((t1-t0))" >> $CSV
    printf "  %-40s %s\n" "$name" "$verdict"
done

echo
echo "==== $A vs $B over sysver_tests ($n tests) ===="
echo "  EQUIVALENT: $eq"
echo "  DIFFER:     $diff"
echo "  ERROR/TO:   $err"
echo
echo "Results: $CSV"
echo "Logs:    $OUT/logs_${A}_vs_${B}/"
