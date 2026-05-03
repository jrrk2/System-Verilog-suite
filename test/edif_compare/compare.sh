#!/bin/bash
# Compare a Vivado RTL-elaborated EDIF against the original SV using both
#
#   (1) Z3 formal miter equivalence (test_xilinx_rtl_miter): converts both
#       sides to Behavioral IR and checks SAT/UNSAT on a miter circuit.
#       This is the ground-truth equivalence test — no name heuristics.
#
#   (2) Cell-family count table: fast diagnostic showing which RTL_* cell
#       families appear in each side. Useful when the formal check fails
#       (or the converter doesn't yet handle the design) to see *where*
#       structural mismatches sit.
#
# Usage: compare.sh <top> [backend]
#   top      - design name (expects <top>.sv or <top>.srcs)
#   backend  - sv_main_unified backend (default: xilinx_rtl). Use 'structural'
#              to see the original generic-primitive output.
#
#   reads:   <top>.sv [or <top>.srcs listing additional sources]
#   writes:  <top>_elab.edf, <top>_<backend>.v, <top>_miter.log
#   prints:  formal miter result + cell-family count table

set -e
top=$1
backend=${2:-xilinx_rtl}
if [ -z "$top" ]; then
    echo "usage: $0 <top> [backend]" >&2
    exit 1
fi

here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
cd "$here"

# Source list resolution: prefer <top>.srcs (one path per line) if it exists,
# else fall back to <top>.sv. Paths in .srcs are resolved relative to $here.
if [ -f "${top}.srcs" ]; then
    mapfile -t src_list < "${top}.srcs"
else
    if [ ! -f "${top}.sv" ]; then
        echo "missing ${top}.sv (and no ${top}.srcs)" >&2
        exit 1
    fi
    src_list=("${top}.sv")
fi

# Tcl-side sources: colon-separated absolute paths
tcl_sources=""
for s in "${src_list[@]}"; do
    abs=$([[ "$s" = /* ]] && echo "$s" || echo "$here/$s")
    tcl_sources="${tcl_sources}${tcl_sources:+:}${abs}"
done

vivado=/NFS/apps/Xilinx/Vivado/2020.1/bin/vivado
sv_main=$repo/_build/default/sv_main_unified.exe

# 1. Vivado elaboration -> EDIF (rebuild if EDIF older than any source)
need_elab=0
if [ ! -f "${top}_elab.edf" ]; then
    need_elab=1
else
    for s in "${src_list[@]}"; do
        abs=$([[ "$s" = /* ]] && echo "$s" || echo "$here/$s")
        if [ "$abs" -nt "${top}_elab.edf" ]; then need_elab=1; break; fi
    done
fi
if [ "$need_elab" = 1 ]; then
    echo "[vivado] elaborating ${top}..."
    "$vivado" -mode batch -source elab.tcl \
        -tclargs "${tcl_sources}" "${top}" "${top}_elab.edf" \
        > "${top}_vivado.log" 2>&1
fi

# 2. Verilator JSON + sv_main_unified
echo "[sv_main] processing ${top} via ${backend}..."
mkdir -p obj_dir
verilator_args=()
for s in "${src_list[@]}"; do
    abs=$([[ "$s" = /* ]] && echo "$s" || echo "$here/$s")
    verilator_args+=("$abs")
done
# Pull in stubs.sv (blackbox declarations of Xilinx primitives that source
# files directly instantiate, e.g. RAMB16_S9_S9). Vivado has these natively;
# Verilator just needs a port-compatible declaration to elaborate.
unisim_extra=()
if [ -f "$here/stubs.sv" ]; then
    unisim_extra+=("$here/stubs.sv")
fi
verilator --json-only -Wno-fatal -Wno-WIDTHTRUNC -Wno-WIDTHEXPAND -Wno-UNOPTFLAT \
    -Wno-CASEINCOMPLETE -Wno-CASEX -Wno-PINMISSING -Wno-IMPLICIT \
    --top-module "${top}" "${verilator_args[@]}" "${unisim_extra[@]}" \
    --Mdir obj_dir > "${top}_verilator.log" 2>&1
"$sv_main" file "${backend}" "obj_dir/V${top}.tree.json" "${top}_${backend}.v" \
    > "${top}_svmain.log" 2>&1

# 3. Z3 formal miter equivalence (primary check).
#
# Both sides go through Verilator → Behavioral IR. The Vivado side is its
# `write_verilog` output (<top>_elab.v) plus rtl_primitives.sv (which
# defines RTL_INV/AND/OR/.../RTL_REG_*). We pre-normalise the Vivado .v to
# strip per-instance suffixes (RTL_REG_SYNC__BREG_4 → RTL_REG_SYNC,
# RTL_OR4 → RTL_OR) so the cell instantiations resolve to the models.
miter_bin=$repo/_build/default/test_xilinx_rtl_miter.exe
miter_status=skip
miter_summary=""
if [ -x "$miter_bin" ]; then
    # Vivado side: prefer the .vhd output (cleaner — vector ports are
    # preserved as STD_LOGIC_VECTOR; no .NAME shorthand or bit-blasted
    # PRDATA). Fall back to the .v if no .vhd is present.
    viv_input=""
    if [ -f "${top}_elab.vhd" ]; then
        viv_input="${top}_elab.vhd"
    elif [ -f "${top}_elab.v" ]; then
        viv_input="${top}_elab.v"
    fi
fi
if [ -x "$miter_bin" ] && [ -n "$viv_input" ]; then
    miter_args=("$top" "${viv_input}" "--")
    for s in "${src_list[@]}"; do
        abs=$([[ "$s" = /* ]] && echo "$s" || echo "$here/$s")
        miter_args+=("$abs")
    done
    [ -f "$here/stubs.sv" ] && miter_args+=("$here/stubs.sv")
    if "$miter_bin" "${miter_args[@]}" > "${top}_miter.log" 2>&1; then
        miter_status=pass
    else
        rc=$?
        # rc==1 is the script's "not equivalent" exit (counterexample or
        # interface mismatch). Other non-zero (e.g. uncaught Z3 exception)
        # means a converter couldn't encode the design.
        if [ "$rc" = 1 ]; then
            miter_status=fail
            # Pull a one-line input/output summary from the counterexample.
            miter_summary=$(awk '
                /^Input values:/  { p=1; next }
                /^Output values:/ { p=2; next }
                /^$/              { p=0 }
                p==1 || p==2      { gsub(/^[ \t]+/, ""); print }
            ' "${top}_miter.log" | tr '\n' '; ')
        else
            miter_status=converter_limit
            miter_summary=$(grep -m1 -E "Fatal|exception" "${top}_miter.log")
        fi
    fi
fi

# 4. Extract primitive cells.
#
# Vivado disambiguates per-instance for its viewer with trailing digits
# (RTL_ADD0, RTL_MUX103) and per-instance width suffixes (RTL_REG_ASYNC__BREG_31).
# We collapse those to family names (RTL_ADD, RTL_MUX, RTL_REG_ASYNC) so the
# comparison shows the real gap rather than Vivado's bookkeeping.
normalize_family='
  s/__BREG_[0-9]+$//
  s/[0-9]+$//
'

viv_prims=$(grep -oE 'cellref +RTL_[A-Z0-9_]+' "${top}_elab.edf" \
    | awk '{print $2}' | sed -E "$normalize_family" \
    | sort | uniq -c | awk '{printf "%-25s %d\n", $2, $1}')

case "$backend" in
  xilinx_rtl|xrtl)  known='RTL_[A-Z0-9_]+' ;;
  structural|struct) known='bitwise_not|bitwise_and|bitwise_or|bitwise_xor|mux2|mux4|adder|adder_carry|subtractor|dff|dff_en|latch_en|comparator' ;;
  *) known='[A-Za-z_][A-Za-z0-9_]*' ;;
esac

if [ "$backend" = "xilinx_rtl" ] || [ "$backend" = "xrtl" ]; then
    sv_prims=$(grep -oE "^[[:space:]]+(${known})" "${top}_${backend}.v" \
        | awk '{print $1}' | sed -E "$normalize_family" \
        | sort | uniq -c | awk '{printf "%-25s %d\n", $2, $1}')
else
    sv_prims=$(grep -oE "^[[:space:]]+(${known})" "${top}_${backend}.v" \
        | awk '{print $1}' | sort | uniq -c | awk '{printf "%-25s %d\n", $2, $1}')
fi

# 4. Print formal-miter result first (the headline equivalence check).
printf '\n=== %s (sv_main backend: %s) ===\n' "$top" "$backend"
case "$miter_status" in
  pass)
      printf 'Z3 miter:   ✅ FORMALLY EQUIVALENT (Vivado EDIF ≡ original SV)\n' ;;
  fail)
      printf 'Z3 miter:   ❌ NOT EQUIVALENT — see %s_miter.log\n' "$top"
      [ -n "$miter_summary" ] && printf '            %s\n' "$miter_summary" ;;
  converter_limit)
      printf 'Z3 miter:   ⚠ converter limitation (see %s_miter.log) — falling back to cell-count table\n' "$top" ;;
  skip)
      printf 'Z3 miter:   skipped (test_xilinx_rtl_miter not built; run `dune build`)\n' ;;
esac

# Cell-family count table (secondary diagnostic).
printf '\n%-25s | %-10s | %-10s | %s\n' "cell family" "Vivado" "sv_main" "delta"
printf '%-25s-+-%-10s-+-%-10s-+-%s\n' \
    "$(printf '%.0s-' {1..25})" "$(printf '%.0s-' {1..10})" \
    "$(printf '%.0s-' {1..10})" "$(printf '%.0s-' {1..10})"

# Build a union of family names, then print Vivado count vs sv_main count.
{ printf '%s\n' "$viv_prims" | awk '{print $1}'; \
  printf '%s\n' "$sv_prims"  | awk '{print $1}'; } \
    | sort -u | grep -v '^$' | while read -r fam; do
    vc=$(printf '%s\n' "$viv_prims" | awk -v f="$fam" '$1==f{print $2}')
    sc=$(printf '%s\n' "$sv_prims"  | awk -v f="$fam" '$1==f{print $2}')
    vc=${vc:-0}; sc=${sc:-0}
    if [ "$vc" = "$sc" ]; then mark='='; else mark='!='; fi
    printf '%-25s | %-10s | %-10s | %s\n' "$fam" "$vc" "$sc" "$mark"
done

# 5. Match summary
if [ "$backend" = "xilinx_rtl" ] || [ "$backend" = "xrtl" ]; then
    viv_set=$(printf '%s\n' "$viv_prims" | awk '{print $1}' | sort -u)
    sv_set=$(printf '%s\n' "$sv_prims"  | awk '{print $1}' | sort -u)
    only_viv=$(comm -23 <(echo "$viv_set") <(echo "$sv_set") | tr '\n' ' ')
    only_sv=$(comm -13 <(echo "$viv_set") <(echo "$sv_set") | tr '\n' ' ')
    common=$(comm -12 <(echo "$viv_set") <(echo "$sv_set") | tr '\n' ' ')
    printf '\nshared families:    %s\n' "${common:-<none>}"
    printf 'only in Vivado:     %s\n' "${only_viv:-<none>}"
    printf 'only in sv_main:    %s\n' "${only_sv:-<none>}"
fi
echo
