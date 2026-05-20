#!/bin/bash
# Run pairwise Z3 miters across five SystemVerilog frontends over a
# selection of leaf modules from cva6.  Source is the pre-flattened
# test/cva6_ram/cva6_flat.sv (slang's monomorphised flatten output);
# each leaf module is extracted into its own .sv file so verilator
# can parse it standalone.
#
# Output: $OUT/cva6_five.csv

set -e
HERE=/home/jonathan/System-Verilog-suite
MITER=$HERE/_build/default/test_five_frontend_miter.exe
FLAT=$HERE/test/cva6_ram/cva6_flat.sv
OUT=${OUT:-/tmp/cva6_five}
mkdir -p "$OUT/src" "$OUT/json" "$OUT/logs"
CSV=$OUT/cva6_five.csv

[ -x "$MITER" ] || { echo "build first: dune build test_five_frontend_miter.exe"; exit 1; }
[ -f "$FLAT" ] || { echo "missing $FLAT"; exit 1; }

VERILATOR_FLAGS="-Wno-fatal -Wno-WIDTHTRUNC -Wno-WIDTHEXPAND \
  -Wno-UNOPTFLAT -Wno-ASCRANGE -Wno-MULTIDRIVEN -Wno-UNUSEDPARAM \
  -Wno-UNUSEDSIGNAL -Wno-IMPORTSTAR -Wno-IMPLICIT -Wno-PINMISSING \
  -Wno-CASEINCOMPLETE -Wno-LATCH -Wno-VARHIDDEN -Wno-ZEROREPL \
  -Wno-SYMRSVDWORD -Wno-CASTCONST -Wno-SELRANGE -Wno-PROFOUTOFDATE \
  -Wno-COMBDLY -Wno-INITIALDLY -Wno-STMTDLY -Wno-REALCVT -Wno-NULLPORT \
  -Wno-ENUMVALUE -Wno-CMPCONST -Wno-UNDRIVEN -Wno-IFDEPTH"

PAIRS=("verible vs slang" "verible vs verilator" "verible vs sv-parser" "verible vs synlig" \
       "slang vs verilator" "slang vs sv-parser" "slang vs synlig" \
       "verilator vs sv-parser" "verilator vs synlig" \
       "sv-parser vs synlig")

echo "test,top,vs,vl,vp,vy,sl,sp,sy,lp,ly,py,consensus,seconds" > $CSV

# Use awk to extract each `module foo` ... `endmodule` block and save as its
# own file under $OUT/src/.  Match modules whose name is a plain identifier
# (no parameterised-monomorphised forms like `foo__N4_D8`).
awk -v out="$OUT/src" '
BEGIN { in_mod = 0; depth = 0; }
/^[[:space:]]*module[[:space:]]+[A-Za-z_][A-Za-z0-9_]*[[:space:]]*[#(]/ {
  match($0, /module[[:space:]]+([A-Za-z_][A-Za-z0-9_]*)/, m);
  modname = m[1];
  if (modname !~ /^pa_|^ct_|^fpnew_/ && modname !~ /__/) {
    in_mod = 1; depth = 1;
    file = out "/" modname ".sv";
    cur_file = file;
    print > file;
    next;
  }
}
in_mod {
  print >> cur_file;
  if (/^[[:space:]]*module[[:space:]]/) depth++;
  if (/^[[:space:]]*endmodule/) {
    depth--;
    if (depth == 0) { in_mod = 0; close(cur_file); }
  }
}
' "$FLAT"

n=0; all_eq=0; one_off=0; multi_off=0; verilator_fail=0

classify() {
    local line="$1"
    if echo "$line" | grep -q "EQUIVALENT"; then echo "EQUIVALENT"
    elif echo "$line" | grep -q "DIFFER";   then echo "DIFFER"
    else echo "ERROR"
    fi
}

short() { case "$1" in EQUIVALENT) echo E ;; DIFFER) echo D ;; *) echo X ;; esac; }

for sv in $OUT/src/*.sv; do
    name=$(basename "$sv" .sv)
    top="$name"
    n=$((n+1))
    json=$OUT/json/${name}.json
    log=$OUT/logs/${name}.log
    t0=$(date +%s)

    if ! verilator --json-only --json-only-output "$json" \
            $VERILATOR_FLAGS --top-module "$top" "$sv" > "$log.verilator" 2>&1; then
        printf '%s,%s,,,,,,,,,,,VERILATOR_FAIL,%d\n' "$name" "$top" \
               $(( $(date +%s) - t0 )) >> $CSV
        verilator_fail=$((verilator_fail+1))
        printf "  %-40s VERILATOR_FAIL\n" "$name"
        continue
    fi
    timeout 120 "$MITER" "$top" "$sv" "$json" > "$log" 2>&1 || true

    declare -a V=()
    for label in "${PAIRS[@]}"; do
        V+=("$(classify "$(grep -E "^  $label " "$log" 2>/dev/null)")")
    done

    bad_v=0; bad_s=0; bad_l=0; bad_p=0; bad_y=0
    for i in "${!PAIRS[@]}"; do
        if [ "${V[$i]}" != "EQUIVALENT" ]; then
            case "${PAIRS[$i]}" in
                "verible vs slang")        bad_v=$((bad_v+1)); bad_s=$((bad_s+1)) ;;
                "verible vs verilator")    bad_v=$((bad_v+1)); bad_l=$((bad_l+1)) ;;
                "verible vs sv-parser")    bad_v=$((bad_v+1)); bad_p=$((bad_p+1)) ;;
                "verible vs synlig")       bad_v=$((bad_v+1)); bad_y=$((bad_y+1)) ;;
                "slang vs verilator")      bad_s=$((bad_s+1)); bad_l=$((bad_l+1)) ;;
                "slang vs sv-parser")      bad_s=$((bad_s+1)); bad_p=$((bad_p+1)) ;;
                "slang vs synlig")         bad_s=$((bad_s+1)); bad_y=$((bad_y+1)) ;;
                "verilator vs sv-parser")  bad_l=$((bad_l+1)); bad_p=$((bad_p+1)) ;;
                "verilator vs synlig")     bad_l=$((bad_l+1)); bad_y=$((bad_y+1)) ;;
                "sv-parser vs synlig")     bad_p=$((bad_p+1)); bad_y=$((bad_y+1)) ;;
            esac
        fi
    done

    eq_count=0
    for v in "${V[@]}"; do [ "$v" = "EQUIVALENT" ] && eq_count=$((eq_count+1)); done

    if [ "$eq_count" -eq 10 ]; then
        consensus="EQ_ALL"; all_eq=$((all_eq+1))
    elif [ "$eq_count" -eq 6 ]; then
        if   [ "$bad_v" = 4 ]; then consensus="ODD=verible"
        elif [ "$bad_s" = 4 ]; then consensus="ODD=slang"
        elif [ "$bad_l" = 4 ]; then consensus="ODD=verilator"
        elif [ "$bad_p" = 4 ]; then consensus="ODD=sv-parser"
        elif [ "$bad_y" = 4 ]; then consensus="ODD=synlig"
        else consensus="ODD=?"
        fi
        one_off=$((one_off+1))
    else
        consensus="MULTI"; multi_off=$((multi_off+1))
    fi

    row=""
    for v in "${V[@]}"; do row+="$(short "$v"),"; done
    row="${row%,}"
    echo "$name,$top,$row,$consensus,$(( $(date +%s) - t0 ))" >> $CSV
    printf "  %-40s %s  %s\n" "$name" \
        "$(echo "${V[@]}" | sed 's/EQUIVALENT/E/g; s/DIFFER/D/g; s/ERROR/X/g')" \
        "$consensus"
done

echo
echo "==== five-way miter over cva6 leaf modules ($n tests) ===="
echo "  ALL EQUIVALENT:          $all_eq"
echo "  exactly one disagrees:   $one_off"
echo "  multiple disagree/err:   $multi_off"
echo "  verilator failed:        $verilator_fail"
echo
echo "Results: $CSV"
echo "Logs:    $OUT/logs/"
