#!/bin/bash
# Run Vivado-vs-Verible miter across sysver_tests/*.sv.
#
# Phase 1: build a manifest, batch-elaborate each through Vivado
#          synth_design -rtl (one Vivado session, restart on crash).
# Phase 2: for each Vivado-PASS test, run test_xilinx_rtl_miter
#          against the original .sv + RTL primitives.
#
# Output: /tmp/sysver_miter/results.csv

set -e
HERE=/home/jonathan/System-Verilog-decompiler
VIVADO=${VIVADO:-/NFS/apps/Xilinx/Vivado/2020.1/bin/vivado}
TCL=$HERE/test/regressions/sysver_vivado_batch.tcl
MITER=$HERE/_build/default/test_verible_vhdl_miter.exe

FRONTEND=${FRONTEND:-verible}
OUT=${OUT:-/tmp/sysver_miter}
mkdir -p $OUT/elab "$OUT/logs_$FRONTEND"

MANIFEST=$OUT/manifest.tsv
VIVADO_CSV=$OUT/vivado_baseline.csv
MITER_CSV=$OUT/miter_results_${FRONTEND}.csv
VIVADO_LOG=$OUT/vivado_batch.log

[ -x "$VIVADO" ] || { echo "Vivado not at $VIVADO (set VIVADO=...)"; exit 1; }
[ -x "$MITER"  ] || { echo "miter binary not built — dune build test_xilinx_rtl_miter.exe"; exit 1; }

# ── Phase 1: manifest ───────────────────────────────────────────────
echo "==> Phase 1: manifest"
> $MANIFEST
for sv in $HERE/sysver_tests/*.sv; do
    name=$(basename "$sv" .sv)
    top=$(grep -hE "^[[:space:]]*module[[:space:]]+[A-Za-z_][A-Za-z0-9_]*" "$sv" 2>/dev/null \
          | head -1 \
          | sed -E 's/^[[:space:]]*module[[:space:]]+([A-Za-z_][A-Za-z0-9_]*).*/\1/')
    [ -z "$top" ] && continue
    printf "%s\t%s\t%s\n" "$name" "$top" "$sv" >> $MANIFEST
done
echo "    $(wc -l < $MANIFEST) tests"

# ── Phase 2: Vivado batch ───────────────────────────────────────────
echo "==> Phase 2: Vivado synth_design -rtl"
cd $OUT
attempt=0
while [ $attempt -lt 5 ]; do
    attempt=$((attempt + 1))
    echo "  attempt $attempt"
    if [ -f "$VIVADO_CSV.in_progress" ]; then
        crashed=$(cat "$VIVADO_CSV.in_progress")
        echo "  recording crash for: $crashed"
        echo "$crashed,?,CRASH,0,\"vivado segfault\"" >> "$VIVADO_CSV"
        rm -f "$VIVADO_CSV.in_progress"
    fi
    if "$VIVADO" -mode batch -nojournal -nolog \
            -source "$TCL" \
            -tclargs "$MANIFEST" "$OUT/elab" "$VIVADO_CSV" \
            >> "$VIVADO_LOG" 2>&1
    then
        rm -f "$VIVADO_CSV.in_progress"
        break
    fi
    if [ ! -f "$VIVADO_CSV.in_progress" ]; then
        echo "  vivado non-zero, no marker — stopping"
        break
    fi
done

# ── Phase 3: per-test miter for Vivado-PASS designs ─────────────────
echo "==> Phase 3: miter (Vivado-PASS only)"
echo "test,top,vivado,miter,error" > $MITER_CSV
while IFS=, read -r test top result ms error; do
    [ "$test" = "test" ] && continue          # header
    [ "$result" != "PASS" ] && {
        echo "$test,$top,$result,SKIP," >> $MITER_CSV
        continue
    }
    elab="$OUT/elab/${test}_elab.vhd"
    sv="$HERE/sysver_tests/${test}.sv"
    log="$OUT/logs_$FRONTEND/${test}_miter.log"
    [ -f "$elab" ] || {
        echo "$test,$top,PASS,NO_ELAB," >> $MITER_CSV
        continue
    }
    # ${FRONTEND} (sv) vs vhdl (vivado-elaborated)
    if timeout 120 "$MITER" "$top" "$sv" "$elab" "$FRONTEND" \
            > "$log" 2>&1
    then
        verdict=$(grep -oE "EQUIVALENT|DIFFER|TIMEOUT" "$log" | head -1)
        [ -z "$verdict" ] && verdict="UNKNOWN"
        echo "$test,$top,PASS,$verdict," >> $MITER_CSV
        printf "  %-40s %s\n" "$test" "$verdict"
    else
        rc=$?
        verdict=$([ $rc -eq 124 ] && echo "TIMEOUT" || echo "ERROR_rc$rc")
        first_err=$(grep -m1 "error:" "$log" 2>/dev/null | head -c 80 || echo "")
        echo "$test,$top,PASS,$verdict,\"$first_err\"" >> $MITER_CSV
        printf "  %-40s %s\n" "$test" "$verdict"
    fi
done < "$VIVADO_CSV"

# ── Summary ─────────────────────────────────────────────────────────
echo
echo "==== Summary (frontend=$FRONTEND) ===="
total=$(($(wc -l < $MITER_CSV) - 1))
viv_pass=$(awk -F, 'NR>1 && $3=="PASS" {n++} END{print n+0}' $MITER_CSV)
equiv=$(awk -F, 'NR>1 && $4=="EQUIVALENT" {n++} END{print n+0}' $MITER_CSV)
differ=$(awk -F, 'NR>1 && $4=="DIFFER"     {n++} END{print n+0}' $MITER_CSV)
echo "Total tests:       $total"
echo "Vivado-elab PASS:  $viv_pass"
echo "Miter EQUIVALENT:  $equiv"
echo "Miter DIFFER:      $differ"
echo
echo "Results: $MITER_CSV"
echo "Logs:    $OUT/logs_$FRONTEND/"
