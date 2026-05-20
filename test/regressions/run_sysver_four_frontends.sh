#!/bin/bash
# Run pairwise Z3 miters across four independent SystemVerilog
# frontends (verible, slang, verilator, sv-parser) over the
# sysver_tests/ corpus.
#
# Output: $OUT/four_way.csv with one column per pair (6 columns:
# 4 choose 2) plus a consensus column.

set -e
HERE=/home/jonathan/System-Verilog-suite
MITER=$HERE/_build/default/test_four_frontend_miter.exe
OUT=${OUT:-/tmp/sysver_four_fe}
mkdir -p "$OUT/logs" "$OUT/json"
CSV=$OUT/four_way.csv

[ -x "$MITER" ] || { echo "build first: dune build test_four_frontend_miter.exe"; exit 1; }

VERILATOR_FLAGS="-Wno-fatal -Wno-WIDTHTRUNC -Wno-WIDTHEXPAND \
  -Wno-UNOPTFLAT -Wno-ASCRANGE -Wno-MULTIDRIVEN -Wno-UNUSEDPARAM \
  -Wno-UNUSEDSIGNAL -Wno-IMPORTSTAR -Wno-IMPLICIT -Wno-PINMISSING \
  -Wno-CASEINCOMPLETE -Wno-LATCH -Wno-VARHIDDEN -Wno-ZEROREPL \
  -Wno-SYMRSVDWORD -Wno-CASTCONST -Wno-SELRANGE -Wno-PROFOUTOFDATE \
  -Wno-COMBDLY -Wno-INITIALDLY -Wno-STMTDLY -Wno-REALCVT -Wno-NULLPORT \
  -Wno-ENUMVALUE -Wno-CMPCONST -Wno-UNDRIVEN -Wno-IFDEPTH"

echo "test,top,ver_vs_slang,ver_vs_lat,ver_vs_p,slang_vs_lat,slang_vs_p,lat_vs_p,consensus,seconds" > $CSV

n=0; all_eq=0; one_off=0; multi_off=0; verilator_fail=0

classify() {
    local line="$1"
    if echo "$line" | grep -q "EQUIVALENT"; then echo "EQUIVALENT"
    elif echo "$line" | grep -q "DIFFER";   then echo "DIFFER"
    else echo "ERROR"
    fi
}

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

    if ! verilator --json-only --json-only-output "$json" \
            $VERILATOR_FLAGS --top-module "$top" "$sv" > "$log.verilator" 2>&1; then
        echo "$name,$top,,,,,,VERILATOR_FAIL,$(( $(date +%s) - t0 ))" >> $CSV
        verilator_fail=$((verilator_fail+1))
        printf "  %-40s VERILATOR_FAIL\n" "$name"
        continue
    fi
    timeout 90 "$MITER" "$top" "$sv" "$json" > "$log" 2>&1 || true

    vs=$(classify "$(grep 'verible vs slang'     "$log" 2>/dev/null)")
    vl=$(classify "$(grep 'verible vs verilator' "$log" 2>/dev/null)")
    vp=$(classify "$(grep 'verible vs sv-parser' "$log" 2>/dev/null)")
    sl=$(classify "$(grep 'slang vs verilator'   "$log" 2>/dev/null)")
    sp=$(classify "$(grep 'slang vs sv-parser'   "$log" 2>/dev/null)")
    lp=$(classify "$(grep 'verilator vs sv-parser' "$log" 2>/dev/null)")

    # With 4 frontends and 6 pairs, ALL-EQ requires all 6 to be EQUIVALENT.
    # The actionable bucket is "exactly one frontend disagrees" — every pair
    # involving that frontend is NEQ (3 pairs), every other pair is EQ (3).
    eq_count=0
    for v in "$vs" "$vl" "$vp" "$sl" "$sp" "$lp"; do
        [ "$v" = "EQUIVALENT" ] && eq_count=$((eq_count+1))
    done
    # Per-frontend NEQ count: each frontend appears in 3 pairs.
    bad_v=0; bad_s=0; bad_l=0; bad_p=0
    [ "$vs" != "EQUIVALENT" ] && { bad_v=$((bad_v+1)); bad_s=$((bad_s+1)); }
    [ "$vl" != "EQUIVALENT" ] && { bad_v=$((bad_v+1)); bad_l=$((bad_l+1)); }
    [ "$vp" != "EQUIVALENT" ] && { bad_v=$((bad_v+1)); bad_p=$((bad_p+1)); }
    [ "$sl" != "EQUIVALENT" ] && { bad_s=$((bad_s+1)); bad_l=$((bad_l+1)); }
    [ "$sp" != "EQUIVALENT" ] && { bad_s=$((bad_s+1)); bad_p=$((bad_p+1)); }
    [ "$lp" != "EQUIVALENT" ] && { bad_l=$((bad_l+1)); bad_p=$((bad_p+1)); }
    case $eq_count in
        6) consensus="EQ_ALL"; all_eq=$((all_eq+1)) ;;
        3)
            # Find the frontend with bad_count == 3 (the lone dissenter).
            if   [ "$bad_v" = 3 ]; then consensus="ODD=verible"
            elif [ "$bad_s" = 3 ]; then consensus="ODD=slang"
            elif [ "$bad_l" = 3 ]; then consensus="ODD=verilator"
            elif [ "$bad_p" = 3 ]; then consensus="ODD=sv-parser"
            else consensus="ODD=?"
            fi
            one_off=$((one_off+1))
            ;;
        *) consensus="MULTI"; multi_off=$((multi_off+1)) ;;
    esac

    echo "$name,$top,$vs,$vl,$vp,$sl,$sp,$lp,$consensus,$(( $(date +%s) - t0 ))" >> $CSV
    printf "  %-40s %-3s %-3s %-3s %-3s %-3s %-3s  %s\n" "$name" \
        "${vs:0:1}" "${vl:0:1}" "${vp:0:1}" "${sl:0:1}" "${sp:0:1}" "${lp:0:1}" "$consensus"
done

echo
echo "==== four-way miter over sysver_tests ($n tests) ===="
echo "  ALL EQUIVALENT:          $all_eq"
echo "  exactly one disagrees:   $one_off"
echo "  multiple disagree/err:   $multi_off"
echo "  verilator failed:        $verilator_fail"
echo
echo "Results: $CSV"
echo "Logs:    $OUT/logs/"
