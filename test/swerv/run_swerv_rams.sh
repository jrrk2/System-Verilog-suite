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

# Scan the three SweRV directories that contain memory-bearing cells.
# `design/lib` holds mem_lib.sv (41 SRAM macros) plus the bus bridges;
# `design/ifu` and `design/lsu` hold caches/CCM wrappers; `design`
# itself has mem.sv and the top-level glue.
out=$(mktemp)
trap "rm -f $out" EXIT

for dir in design/lib design/ifu design/lsu design dec dbg dmi exu pic_ctrl.sv dma_ctrl.sv; do
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

if [ $verbose = 0 ]; then
    echo
    echo "(re-run with -v for per-module detail)"
fi
