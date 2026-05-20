#!/bin/bash
# Run pairwise Z3 miters across four frontends (verible, slang,
# sv-parser, synlig) over per-module cva6 sources extracted from
# test/cva6_ram/cva6_flat.sv.  Bypasses the verilator dependency,
# which crashes with an "internal fault" on cva6's package-qualified
# packed-struct types.
#
# Output: $OUT/cva6_four.csv

set -e
HERE=/home/jonathan/System-Verilog-suite
MITER=$HERE/_build/default/test_no_verilator_miter.exe
FLAT=$HERE/test/cva6_ram/cva6_flat.sv
OUT=${OUT:-/tmp/cva6_four}
mkdir -p "$OUT/src" "$OUT/logs"
CSV=$OUT/cva6_four.csv

[ -x "$MITER" ] || { echo "build first: dune build test_no_verilator_miter.exe"; exit 1; }
[ -f "$FLAT" ] || { echo "missing $FLAT"; exit 1; }

PAIRS=("verible vs slang" "verible vs sv-parser" "verible vs synlig" \
       "slang vs sv-parser" "slang vs synlig" \
       "sv-parser vs synlig")

echo "test,top,vs,vp,vy,sp,sy,py,consensus,seconds" > $CSV

# Extract individual modules into per-module .sv files, same convention
# as the 5-way driver.
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

n=0; all_eq=0; one_off=0; multi_off=0

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
    log=$OUT/logs/${name}.log
    t0=$(date +%s)

    timeout 60 "$MITER" "$top" "$sv" > "$log" 2>&1 || true

    declare -a V=()
    for label in "${PAIRS[@]}"; do
        V+=("$(classify "$(grep -E "^  $label " "$log" 2>/dev/null)")")
    done

    # With 4 frontends and 6 pairs: ALL-EQ requires all 6 to be EQ.
    # Exactly one dissenter: its 3 pairs are NEQ, others' 0.
    bad_v=0; bad_s=0; bad_p=0; bad_y=0
    [ "${V[0]}" != "EQUIVALENT" ] && { bad_v=$((bad_v+1)); bad_s=$((bad_s+1)); }
    [ "${V[1]}" != "EQUIVALENT" ] && { bad_v=$((bad_v+1)); bad_p=$((bad_p+1)); }
    [ "${V[2]}" != "EQUIVALENT" ] && { bad_v=$((bad_v+1)); bad_y=$((bad_y+1)); }
    [ "${V[3]}" != "EQUIVALENT" ] && { bad_s=$((bad_s+1)); bad_p=$((bad_p+1)); }
    [ "${V[4]}" != "EQUIVALENT" ] && { bad_s=$((bad_s+1)); bad_y=$((bad_y+1)); }
    [ "${V[5]}" != "EQUIVALENT" ] && { bad_p=$((bad_p+1)); bad_y=$((bad_y+1)); }

    eq_count=0
    for v in "${V[@]}"; do [ "$v" = "EQUIVALENT" ] && eq_count=$((eq_count+1)); done

    if [ "$eq_count" -eq 6 ]; then
        consensus="EQ_ALL"; all_eq=$((all_eq+1))
    elif [ "$eq_count" -eq 3 ]; then
        if   [ "$bad_v" = 3 ]; then consensus="ODD=verible"
        elif [ "$bad_s" = 3 ]; then consensus="ODD=slang"
        elif [ "$bad_p" = 3 ]; then consensus="ODD=sv-parser"
        elif [ "$bad_y" = 3 ]; then consensus="ODD=synlig"
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
echo "==== four-way (no-verilator) miter over cva6 leaf modules ($n tests) ===="
echo "  ALL EQUIVALENT:          $all_eq"
echo "  exactly one disagrees:   $one_off"
echo "  multiple disagree/err:   $multi_off"
echo
echo "Results: $CSV"
echo "Logs:    $OUT/logs/"
