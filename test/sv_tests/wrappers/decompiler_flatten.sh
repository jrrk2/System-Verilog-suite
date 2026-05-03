#!/bin/bash
# Flatten an SV testcase (expand `include and `define) with verilator -E,
# then invoke a target binary on the flattened result.
#
# Args:
#   $1  exe path  (e.g. /repo/_build/default/test_verilator_vs_verible.exe)
#   $2  top-module name
#   $3  output dir for the flattened .sv (created if needed)
#   $4  incdirs string  ("" or "-I dir1 -I dir2 ...")
#   $5  defines string  ("" or "+define+X=1 +define+Y=foo")
#   $6+ source files
#
# Returns the target exe's exit code (0 = test passes by that exe's
# rules). On flatten failure, returns 71 (sv-tests' timeout/error code).

set -e
exe=$1
top=$2
outdir=$3
incdirs=$4
defines=$5
shift 5

mkdir -p "$outdir"
flat="$outdir/flat.sv"

# If the caller's `top` doesn't actually appear in any source, fall
# back to the first `module X` we find. sv-tests defaults the runner
# config to top='top', but most chapter tests use other names.
if [ -n "$top" ]; then
    if ! grep -qE "^[[:space:]]*module[[:space:]]+$top\\b" "$@" 2>/dev/null; then
        first=$(grep -hE "^[[:space:]]*module[[:space:]]+[A-Za-z_][A-Za-z0-9_]*" "$@" 2>/dev/null \
                | head -1 | sed -E 's/^[[:space:]]*module[[:space:]]+([A-Za-z_][A-Za-z0-9_]*).*/\1/')
        if [ -n "$first" ]; then
            top="$first"
        fi
    fi
fi

# verilator -E expands `include and `define, producing a single SV
# stream on stdout. We strip out verilator's own annotation lines
# (`# 1 "..."`) since the downstream parsers don't expect them.
if ! verilator -E -Wno-fatal -Wno-WIDTHTRUNC -Wno-WIDTHEXPAND \
        -Wno-UNOPTFLAT -Wno-ASCRANGE -Wno-MULTIDRIVEN \
        -Wno-UNUSEDPARAM -Wno-UNUSEDSIGNAL -Wno-IMPORTSTAR \
        $incdirs $defines "$@" > "$flat" 2> "$outdir/flatten.err"
then
    echo "(decompiler_flatten) verilator -E failed:" >&2
    cat "$outdir/flatten.err" >&2
    exit 71
fi

# Run the target exe on the flattened source.
exec "$exe" "$top" "$flat"
