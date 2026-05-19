#!/bin/bash
# Run pairwise Z3 miters across three independent SystemVerilog
# frontends (verible, slang, verilator) over the sysver_tests/ corpus.
#
# Output: /tmp/sysver_three_fe/three_way.csv with one column per pair:
#         test, top, ver_vs_slang, ver_vs_lat, slang_vs_lat, consensus
# `consensus` is EQ when all three pairs say EQUIVALENT, NEQ otherwise.

set -e
HERE=/home/jonathan/System-Verilog-suite
MITER=$HERE/_build/default/test_three_frontend_miter.exe
OUT=${OUT:-/tmp/sysver_three_fe}
mkdir -p "$OUT/logs" "$OUT/json"
CSV=$OUT/three_way.csv

[ -x "$MITER" ] || { echo "build first: dune build test_three_frontend_miter.exe"; exit 1; }

VERILATOR_FLAGS="-Wno-fatal -Wno-WIDTHTRUNC -Wno-WIDTHEXPAND \
  -Wno-UNOPTFLAT -Wno-ASCRANGE -Wno-MULTIDRIVEN -Wno-UNUSEDPARAM \
  -Wno-UNUSEDSIGNAL -Wno-IMPORTSTAR -Wno-IMPLICIT -Wno-PINMISSING \
  -Wno-CASEINCOMPLETE -Wno-LATCH -Wno-VARHIDDEN -Wno-ZEROREPL \
  -Wno-SYMRSVDWORD -Wno-CASTCONST -Wno-SELRANGE -Wno-PROFOUTOFDATE \
  -Wno-COMBDLY -Wno-INITIALDLY -Wno-STMTDLY -Wno-REALCVT -Wno-NULLPORT \
  -Wno-ENUMVALUE -Wno-CMPCONST -Wno-UNDRIVEN -Wno-IFDEPTH"

echo "test,top,ver_vs_slang,ver_vs_lat,slang_vs_lat,consensus,seconds" > $CSV

# Counters
n=0; consensus_eq=0; one_off=0; multi_off=0; verilator_fail=0; err=0

for sv in $HERE/sysver_tests/*.sv; do
    name=$(basename "$sv" .sv)
    top=$(grep -hE "^[[:space:]]*module[[:space:]]+[A-Za-z_][A-Za-z0-9_]*" "$sv" 2>/dev/null \
          | head -1 \
          | sed -E 's/^[[:space:]]*module[[:space:]]+([A-Za-z_][A-Za-z0-9_]*).*/\1/')
    [ -z "$top" ] && continue
    n=$((n+1))
    json=$OUT/json/${name}.json
    log=$OUT/logs/${name}.log
    t0=$(date +%s)

    # Verilator → JSON
    if ! verilator --json-only --json-only-output "$json" \
            $VERILATOR_FLAGS --top-module "$top" "$sv" > "$log.verilator" 2>&1; then
        echo "$name,$top,,,,VERILATOR_FAIL,$(( $(date +%s) - t0 ))" >> $CSV
        verilator_fail=$((verilator_fail+1))
        printf "  %-40s VERILATOR_FAIL\n" "$name"
        continue
    fi
    # 3-way miter
    if ! timeout 90 "$MITER" "$top" "$sv" "$json" > "$log" 2>&1; then
        rc=$?
        # Parse the verdict triple from the log even on rc != 0.
        :
    fi
    # Normalise each pair to one of: EQUIVALENT, DIFFER, ERROR.
    classify() {
        local line="$1"
        if echo "$line" | grep -q "EQUIVALENT"; then echo "EQUIVALENT"
        elif echo "$line" | grep -q "DIFFER";   then echo "DIFFER"
        else echo "ERROR"
        fi
    }
    vs=$(classify "$(grep 'verible vs slang'     "$log" 2>/dev/null)")
    vl=$(classify "$(grep 'verible vs verilator' "$log" 2>/dev/null)")
    sl=$(classify "$(grep 'slang vs verilator'   "$log" 2>/dev/null)")

    # Transitivity: equivalence is, well, equivalent.  With three pairs,
    # mathematically only 0, 1, or 3 of them can be EQUIVALENT — never
    # exactly 2.  So the actionable bucket is "exactly one EQUIVALENT":
    # the lone disagreeing frontend is the one NOT in that pair.
    eq_count=0
    [ "$vs" = "EQUIVALENT" ] && eq_count=$((eq_count+1))
    [ "$vl" = "EQUIVALENT" ] && eq_count=$((eq_count+1))
    [ "$sl" = "EQUIVALENT" ] && eq_count=$((eq_count+1))
    case $eq_count in
        3) consensus="EQ_ALL";    consensus_eq=$((consensus_eq+1)) ;;
        2) consensus="IMPOSSIBLE"; multi_off=$((multi_off+1)) ;;
        1)
            # Identify the odd one out: the frontend NOT in the
            # EQUIVALENT pair.
            if   [ "$vs" = "EQUIVALENT" ]; then consensus="ODD=verilator"
            elif [ "$vl" = "EQUIVALENT" ]; then consensus="ODD=slang"
            else                                consensus="ODD=verible"
            fi
            one_off=$((one_off+1))
            ;;
        0) consensus="NONE_AGREE"; multi_off=$((multi_off+1)) ;;
    esac

    echo "$name,$top,$vs,$vl,$sl,$consensus,$(( $(date +%s) - t0 ))" >> $CSV
    printf "  %-40s %-10s %-10s %-10s  %s\n" "$name" "$vs" "$vl" "$sl" "$consensus"
done

echo
echo "==== three-way miter over sysver_tests ($n tests) ===="
echo "  ALL EQUIVALENT:          $consensus_eq"
echo "  one frontend disagrees:  $one_off    (the actionable bucket)"
echo "  multiple disagree:       $multi_off"
echo "  verilator failed:        $verilator_fail"
echo
echo "Results: $CSV"
echo "Logs:    $OUT/logs/"
