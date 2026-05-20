#!/bin/bash
# Run pairwise Z3 miters across four frontends (verible, slang,
# verilator, sv-parser) over every .sv under $HOME/sv-tests/tests.
# Synlig is intentionally excluded (Surelog OOMs on large designs —
# see memory/cva6-synlig-oom).
#
# Output: $OUT/svtests_four.csv with one row per file:
#   test, top, ver_vs_slang, ver_vs_lat, ver_vs_p,
#   slang_vs_lat, slang_vs_p, lat_vs_p, consensus, seconds
# E/D/X = EQUIVALENT/DIFFER/ERROR; VERILATOR_FAIL in consensus when
# verilator's --json-only pre-render refuses the file.

set -u
HERE=/home/jonathan/System-Verilog-suite
MITER=$HERE/_build/default/test_four_frontend_miter.exe
ROOT=${SVTESTS_ROOT:-$HOME/sv-tests/tests}
OUT=${OUT:-/tmp/svtests_four}
TIMEOUT=${TIMEOUT:-90}
mkdir -p "$OUT/logs" "$OUT/json"
CSV=$OUT/svtests_four.csv

[ -x "$MITER" ] || { echo "build first: dune build test_four_frontend_miter.exe"; exit 1; }
[ -d "$ROOT" ] || { echo "missing corpus: $ROOT"; exit 1; }

VERILATOR_FLAGS="-Wno-fatal -Wno-WIDTHTRUNC -Wno-WIDTHEXPAND \
  -Wno-UNOPTFLAT -Wno-ASCRANGE -Wno-MULTIDRIVEN -Wno-UNUSEDPARAM \
  -Wno-UNUSEDSIGNAL -Wno-IMPORTSTAR -Wno-IMPLICIT -Wno-PINMISSING \
  -Wno-CASEINCOMPLETE -Wno-LATCH -Wno-VARHIDDEN -Wno-ZEROREPL \
  -Wno-SYMRSVDWORD -Wno-CASTCONST -Wno-SELRANGE -Wno-PROFOUTOFDATE \
  -Wno-COMBDLY -Wno-INITIALDLY -Wno-STMTDLY -Wno-REALCVT -Wno-NULLPORT \
  -Wno-ENUMVALUE -Wno-CMPCONST -Wno-UNDRIVEN -Wno-IFDEPTH"

echo "test,top,ver_vs_slang,ver_vs_lat,ver_vs_p,slang_vs_lat,slang_vs_p,lat_vs_p,consensus,seconds" > $CSV

n=0; skipped=0; all_eq=0; one_off=0; multi_off=0; verilator_fail=0

classify() {
    local line="$1"
    if echo "$line" | grep -q "EQUIVALENT"; then echo "EQUIVALENT"
    elif echo "$line" | grep -q "DIFFER";   then echo "DIFFER"
    else echo "ERROR"
    fi
}

short() { case "$1" in EQUIVALENT) echo E ;; DIFFER) echo D ;; *) echo X ;; esac; }

while IFS= read -r -d '' sv; do
    rel=${sv#$ROOT/}
    name=${rel%.sv}
    name=${name%.v}
    name=${name//\//__}

    top=$(grep -hE "^[[:space:]]*module[[:space:]]+[A-Za-z_][A-Za-z0-9_]*" "$sv" 2>/dev/null \
          | head -1 \
          | sed -E 's/^[[:space:]]*module[[:space:]]+([A-Za-z_][A-Za-z0-9_]*).*/\1/')
    if [ -z "$top" ]; then
        skipped=$((skipped+1))
        continue
    fi

    n=$((n+1))
    json=$OUT/json/${name}.json
    log=$OUT/logs/${name}.log
    t0=$(date +%s)

    if ! verilator --json-only --json-only-output "$json" \
            $VERILATOR_FLAGS --top-module "$top" "$sv" > "$log.verilator" 2>&1; then
        echo "$name,$top,,,,,,VERILATOR_FAIL,$(( $(date +%s) - t0 ))" >> $CSV
        verilator_fail=$((verilator_fail+1))
        continue
    fi

    timeout "$TIMEOUT" "$MITER" "$top" "$sv" "$json" > "$log" 2>&1 || true

    vs=$(classify "$(grep 'verible vs slang'     "$log" 2>/dev/null)")
    vl=$(classify "$(grep 'verible vs verilator' "$log" 2>/dev/null)")
    vp=$(classify "$(grep 'verible vs sv-parser' "$log" 2>/dev/null)")
    sl=$(classify "$(grep 'slang vs verilator'   "$log" 2>/dev/null)")
    sp=$(classify "$(grep 'slang vs sv-parser'   "$log" 2>/dev/null)")
    lp=$(classify "$(grep 'verilator vs sv-parser' "$log" 2>/dev/null)")

    eq_count=0
    for v in "$vs" "$vl" "$vp" "$sl" "$sp" "$lp"; do
        [ "$v" = "EQUIVALENT" ] && eq_count=$((eq_count+1))
    done
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

    echo "$name,$top,$(short "$vs"),$(short "$vl"),$(short "$vp"),$(short "$sl"),$(short "$sp"),$(short "$lp"),$consensus,$(( $(date +%s) - t0 ))" >> $CSV
done < <(find "$ROOT" \( -name '*.sv' -o -name '*.v' \) -type f -print0)

echo
echo "==== four-way (verible/slang/verilator/sv-parser) over sv-tests ($n tested, $skipped skipped) ===="
echo "  ALL EQUIVALENT:          $all_eq"
echo "  exactly one disagrees:   $one_off"
echo "  multiple disagree/err:   $multi_off"
echo "  verilator pre-render fail: $verilator_fail"
echo
echo "Results: $CSV"
echo "Logs:    $OUT/logs/"
