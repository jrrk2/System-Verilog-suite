#!/bin/bash
# Verilator-frontend parse-only wrapper.
#   1. verilator --json-only ...         → produces V<top>.tree.json
#   2. test_verilator_behavioral.exe …   → loads the JSON, returns rc=0 if
#                                          ≥ 1 module survived BIR conversion.
#
# Args (same shape as decompiler_flatten.sh so the runner code stays
# uniform):
#   $1  exe path  (test_verilator_behavioral.exe)
#   $2  top-module name
#   $3  output dir
#   $4  incdirs string
#   $5  defines string
#   $6+ source files

set -e
exe=$1
top=$2
outdir=$3
incdirs=$4
defines=$5
shift 5

mkdir -p "$outdir"
mdir="$outdir/Vtree"

# If the caller's `top` doesn't actually appear in any source, fall
# back to the first `module X` we find.
if [ -n "$top" ]; then
    if ! grep -qE "^[[:space:]]*module[[:space:]]+$top\\b" "$@" 2>/dev/null; then
        first=$(grep -hE "^[[:space:]]*module[[:space:]]+[A-Za-z_][A-Za-z0-9_]*" "$@" 2>/dev/null \
                | head -1 | sed -E 's/^[[:space:]]*module[[:space:]]+([A-Za-z_][A-Za-z0-9_]*).*/\1/')
        if [ -n "$first" ]; then
            top="$first"
        fi
    fi
fi

if ! verilator --json-only -Wno-fatal -Wno-WIDTHTRUNC -Wno-WIDTHEXPAND \
        -Wno-UNOPTFLAT -Wno-ASCRANGE -Wno-MULTIDRIVEN \
        -Wno-UNUSEDPARAM -Wno-UNUSEDSIGNAL -Wno-IMPORTSTAR \
        --top-module "$top" $incdirs $defines "$@" \
        --Mdir "$mdir" > "$outdir/verilator.log" 2>&1
then
    echo "(decompiler_verilator_parse) verilator --json-only failed:" >&2
    tail -n 20 "$outdir/verilator.log" >&2
    exit 1
fi

json="$mdir/V$top.tree.json"
if [ ! -f "$json" ]; then
    echo "(decompiler_verilator_parse) no JSON produced at $json" >&2
    exit 1
fi

# test_verilator_behavioral.exe accepts a JSON path positionally and
# prints "✗" markers / non-zero exit when conversion fails.
exec "$exe" script recipes/verilator_parse.lua "$json"
