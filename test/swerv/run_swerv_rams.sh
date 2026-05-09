#!/bin/bash
# Sweep the SweRV-EH1 design through cva6_ram_scan to identify every
# RAM/ROM variant the meminfer pass recognises.
#
# Pass `-v` to keep verbose per-file output; default is summary-only.

set -e
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
verbose=0
[ "$1" = "-v" ] && verbose=1

# Parse the flist for SWERV_ROOT, -I dirs, and -f prerequisite files.
flist="$here/flist"
swerv_root=$(grep -E '^SWERV_ROOT=' "$flist" | head -1 | cut -d= -f2)
[ -z "$swerv_root" ] && { echo "flist: SWERV_ROOT not set" >&2; exit 1; }

inc_args=()
prereq_args=()
while read -r line; do
    case "$line" in
        '#'*|'') ;;
        '-I '*) d=${line#-I }; d=${d//\$\{SWERV_ROOT\}/$swerv_root}
                inc_args+=( -I "$d" ) ;;
        '-y '*) d=${line#-y }; d=${d//\$\{SWERV_ROOT\}/$swerv_root}
                inc_args+=( -y "$d" ) ;;
        '-f '*) f=${line#-f }; f=${f//\$\{SWERV_ROOT\}/$swerv_root}
                prereq_args+=( -f "$f" ) ;;
    esac
done < "$flist"

if [ ! -d "$swerv_root" ]; then
    echo "SweRV corpus not found at: $swerv_root"
    echo "(Edit test/swerv/flist if the synlig tree has moved.)"
    exit 1
fi

scan="$repo/_build/default/cva6_ram_scan.exe"
[ -x "$scan" ] || ( cd "$repo" && dune build cva6_ram_scan.exe )

# Scan every SweRV design directory for memory-bearing cells.
# `design/lib` holds mem_lib.sv (41 SRAM macros) plus the bus bridges;
# `design/ifu` and `design/lsu` hold caches/CCM wrappers; `design`
# itself has mem.sv and the top-level glue.  The `dec/dbg/dmi/exu`
# subdirs were previously listed without `design/` prefix and silently
# skipped — now scanned correctly so the summary is exhaustive even
# though those four typically declare no inferred memories.
out=$(mktemp)
trap "rm -f $out" EXIT

for dir in design/lib design/ifu design/lsu design \
           design/dec design/dbg design/dmi design/exu; do
    target="$swerv_root/$dir"
    [ -e "$target" ] || continue
    if [ -d "$target" ]; then
        echo "=== $dir ==="
        "$scan" "${inc_args[@]}" "${prereq_args[@]}" "$target" 2>&1 | tee -a "$out"
    fi
done

echo
echo "================================================================"
echo "  SweRV RAM-variant summary (unique memory cells across the design)"
echo "================================================================"
# Tally one row per (memory_name, category) pair — collapses the case
# where the same RAM macro is elaborated under several parent
# top-modules (e.g. pic_ctrl appears under pic_ctrl, swerv, and
# swerv_wrapper).
awk '
  /single_port_bram|true_dual_port_bram|simple_dual_port_bram|distributed_async|ram_[0-9]w[0-9]r|ROM|^[A-Z_a-z]+ +ROM/ {
    # rows look like:  <top>  <memname>  <DxW>  <category>
    if (NF >= 4) {
      key = $2 "\t" $NF
      seen[key] = 1
    }
  }
  END {
    for (k in seen) {
      n = split(k, parts, "\t"); cat = parts[2]
      tally[cat]++
    }
    for (c in tally) printf "  %4d  %s\n", tally[c], c
  }
' "$out" | sort -rn

# ──────────────────────────────────────────────────────────────────
# RAM-macro parallel-correctness sanity check (verilator ↔ verible).
# Each ram_<D>x<W> macro in mem_lib.sv is tiny (~10 lines) and a
# perfect Z3 oracle test.  Pre-flatten the prereq files + mem_lib
# via verilator -E so verible (which doesn't take -I/-y/-f) can
# parse them; then run the unified harness with --oracle verilator
# --peer verible against the resulting flat .sv on each ram macro.
# ──────────────────────────────────────────────────────────────────
oracle="$repo/_build/default/test_yosys_oracle_sweep.exe"
if [ -x "$oracle" ]; then
    echo
    echo "================================================================"
    echo "  Z3 parallel-correctness on RAM macros (verilator ↔ verible)"
    echo "================================================================"
    flat_dir=$(mktemp -d)
    trap "rm -rf $flat_dir; rm -f $out" EXIT
    # Pre-flatten the prereq context + mem_lib into a single .sv via
    # verilator -E so verible (which doesn't take -I/-y/-f) parses it.
    # The prereq files are passed positionally because verilator's -f
    # flag means "read CLI args from this file", not "compile this
    # file first" — the existing flist-based prereq_args in the
    # cva6_ram_scan path was abusing that.
    mem_lib="$swerv_root/design/lib/mem_lib.sv"
    flat_sv="$flat_dir/mem_lib_flat.sv"
    verilator -E \
        -I"$swerv_root/design/include" \
        -I"$swerv_root/design/lib" \
        -I"$swerv_root/snapshots/default" \
        "$swerv_root/snapshots/default/common_defines.vh" \
        "$swerv_root/design/include/swerv_types.sv" \
        "$swerv_root/design/lib/beh_lib.sv" \
        "$mem_lib" \
        > "$flat_sv" 2>/dev/null
    if [ -s "$flat_sv" ]; then
        # The harness's exit code is 1 when a NOTEQUIV is found and 0
        # for all-EQUIV; under set -e a single NOTEQUIV would kill the
        # loop after the first macro.  Drop into a `set +e` block so
        # we record verdicts for every macro.
        set +e
        for top in $(grep -oE "^module ram_[0-9]+x[0-9]+\\b" "$mem_lib" \
                     | awk '{print $2}'); do
            "$oracle" --oracle verilator --peer verible --top "$top" \
                "$flat_sv" 2>/dev/null \
                | grep -E "^  \[" >> "$flat_dir/oracle.log"
        done
        set -e
        if [ -s "$flat_dir/oracle.log" ]; then
            awk '{print $1}' "$flat_dir/oracle.log" \
                | sort | uniq -c | sort -rn
            echo
            echo "  $(wc -l < $flat_dir/oracle.log) RAM macros mitered"
        else
            echo "(no per-macro verdicts produced — see $flat_dir for diagnostics)"
        fi
    else
        echo "(verilator -E pre-flatten failed)"
    fi
fi

if [ $verbose = 0 ]; then
    echo
    echo "(re-run with -v for per-module detail)"
fi
