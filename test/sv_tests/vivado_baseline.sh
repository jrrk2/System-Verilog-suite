#!/bin/bash
# Build a Vivado-2020.1 synthesis baseline over the in-scope sv-tests.
#
# For each in-scope test we:
#   1. Pre-flatten with verilator -E (resolves `include / `define so
#      Vivado sees a single self-contained file).
#   2. Extract the first `module X` name as the top.
#   3. Append to a manifest (one TSV line per test).
# Then run Vivado in a single batch session that loops the manifest
# (read_verilog → synth_design -rtl → close_design between tests).
#
# Result: ~/sv-tests/out/vivado_baseline.csv with PASS/FAIL per test.
#
# Compares to our miter pass-set at the end.

set -e
here=$(cd "$(dirname "$0")" && pwd)
sv_tests=${SV_TESTS_DIR:-$HOME/sv-tests}
vivado=${VIVADO:-/NFS/apps/Xilinx/Vivado/2020.1/bin/vivado}
out_dir=${OUT_DIR:-$sv_tests/out}
manifest=$out_dir/vivado_manifest.tsv
results=$out_dir/vivado_baseline.csv
flat_dir=$out_dir/vivado_flat
log=$out_dir/vivado_batch.log

[ -x "$vivado" ] || { echo "Vivado not at $vivado (set VIVADO=...)"; exit 1; }

mkdir -p "$flat_dir"
> "$manifest"

# Walk in-scope tests (synthesisable subset = same filter as our runners).
echo "==> building manifest"
n=0
skipped=0
while IFS= read -r src; do
    # Same filter as our runner get_mode override.
    if grep -q "^:unsynthesizable: 1" "$src" 2>/dev/null; then
        skipped=$((skipped+1)); continue
    fi
    if grep -qE "^:tags:.*\\b(uvm[a-z-]*|testbench)\\b" "$src" 2>/dev/null; then
        skipped=$((skipped+1)); continue
    fi
    # Chapter-18 is constraint randomization — not synthesisable AND
    # crashes Vivado 2020.1's RTL elaboration. Skip preemptively.
    case "$src" in
        */chapter-18/*) skipped=$((skipped+1)); continue ;;
    esac
    name=${src#$sv_tests/tests/}
    name=${name%.sv}
    flat="$flat_dir/${name//\//_}.flat.sv"
    mkdir -p "$(dirname "$flat")"
    incdir="-I$(dirname "$src")"
    if ! verilator -E -Wno-fatal -Wno-WIDTHTRUNC -Wno-WIDTHEXPAND -Wno-UNOPTFLAT \
            -Wno-ASCRANGE -Wno-MULTIDRIVEN -Wno-UNUSEDPARAM -Wno-UNUSEDSIGNAL \
            -Wno-IMPORTSTAR $incdir "$src" > "$flat" 2>/dev/null
    then
        # Couldn't even flatten — skip silently (verilator parser issue).
        rm -f "$flat"
        continue
    fi
    # Extract first module name.
    top=$(grep -hE "^[[:space:]]*module[[:space:]]+[A-Za-z_][A-Za-z0-9_]*" "$flat" 2>/dev/null \
          | head -1 | sed -E 's/^[[:space:]]*module[[:space:]]+([A-Za-z_][A-Za-z0-9_]*).*/\1/')
    [ -z "$top" ] && top="top"
    printf "%s\t%s\t%s\n" "$name" "$top" "$flat" >> "$manifest"
    n=$((n+1))
done < <(find "$sv_tests/tests" -name '*.sv' -print | sort)
echo "    manifest: $n entries (skipped $skipped non-synth)"

# Run Vivado in batch, retry on crash. The Tcl script appends to
# `$results`, so each restart picks up after the last completed
# entry. If the previous run crashed mid-test, the test name is left
# in `$results.in_progress` — we record it as CRASH then resume.
echo "==> running Vivado synth_design -rtl (resume-on-crash mode)"
cd "$out_dir"
attempt=0
max_attempts=10
while [ $attempt -lt $max_attempts ]; do
    attempt=$((attempt + 1))
    echo "  attempt $attempt"
    # If a previous attempt crashed, log that test as CRASH.
    if [ -f "$results.in_progress" ]; then
        crashed=$(cat "$results.in_progress")
        echo "  recording crash for: $crashed"
        # Append a CRASH row (so the resumed Tcl skips it via its
        # already_done check on next start).
        echo "$crashed,?,CRASH,0,\"vivado segfault\"" >> "$results"
        rm -f "$results.in_progress"
    fi
    if "$vivado" -mode batch -nojournal -nolog \
            -source "$here/vivado_batch_synth.tcl" \
            -tclargs "$manifest" "$results" \
            >> "$log" 2>&1
    then
        rm -f "$results.in_progress"
        break
    fi
    # Non-zero exit. Confirm by checking if we have a marker.
    if [ ! -f "$results.in_progress" ]; then
        echo "  vivado returned non-zero but no marker — stopping"
        break
    fi
done

echo
echo "==== Vivado baseline ===="
if [ -f "$results" ]; then
    total=$(($(wc -l < "$results") - 1))
    passed=$(awk -F, 'NR>1 && $3=="PASS" {n++} END{print n+0}' "$results")
    pct=$(( passed * 100 / (total > 0 ? total : 1) ))
    printf "  Vivado synth_design -rtl: %d / %d (%d%%)\n" "$passed" "$total" "$pct"
fi

# Compare to our miter pass-set, if available.
miter_dir="$sv_tests/out/logs/Decompiler_Miter"
if [ -d "$miter_dir" ]; then
    echo
    echo "==== overlap with our Decompiler_Miter ===="
    miter_pass=$(find "$miter_dir" -name '*.log' ! -size 0 \
                 | xargs grep -l '^rc: 0' 2>/dev/null \
                 | sed "s|$miter_dir/||;s|\\.sv\\.log$||;s|\\.log$||" | sort -u)
    vivado_pass=$(awk -F, 'NR>1 && $3=="PASS" {print $1}' "$results" | sort -u)

    both=$(comm -12 <(echo "$miter_pass") <(echo "$vivado_pass") | wc -l)
    only_miter=$(comm -23 <(echo "$miter_pass") <(echo "$vivado_pass") | wc -l)
    only_vivado=$(comm -13 <(echo "$miter_pass") <(echo "$vivado_pass") | wc -l)
    echo "  pass on both:           $both"
    echo "  pass on miter only:     $only_miter   (suspicious: we accept what Vivado rejects)"
    echo "  pass on Vivado only:    $only_vivado   (room: synthesisable but our miter misses)"
fi

echo
echo "Manifest: $manifest"
echo "Results:  $results"
echo "Log:      $log"

# Convert the CSV into per-test sv-report logs so the next
# `make report` shows a Vivado_Synth_RTL column in the dashboard.
echo
echo "==> emitting per-test logs for the dashboard"
python3 "$here/vivado_csv_to_logs.py" --sv-tests "$sv_tests" --csv "$results"
