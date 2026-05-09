#!/bin/bash
# microgpt + smollm parallel-correctness sweep.
#
# Top-level board target is `vc707_microgpt_eth` (VC707 + SGMII +
# DDR3 MIG + microgpt inference engine).  Most of vc707_microgpt_eth's
# children are Xilinx vendor IP (MIG, PCS/PMA, BUFG, MMCME2) so the
# board top itself LOADFAILs by design.  This sweep walks the
# project's reachable .sv tree (both src/ glue and src/smollm/ leaves)
# and Z3-mitre's verilator vs verible per module.
#
# The harness handles preprocessing in-OCaml via Sv_preproc with
# --incdir threaded through, so module/include resolution does NOT
# depend on verilator -E quirks.  We just pass every reachable .sv
# file to the harness and let it pick the named --top.
#
# Per the project memory, smollm has known elaboration challenges
# (multi-write-port memlower, comb-loop in smollm_layer); this sweep
# enumerates which modules currently pass and which classes of
# disagreement remain.

set -u
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
proj_root=/home/jonathan/TALOS-V2/rtl/vc707/src
src=/home/jonathan/TALOS-V2/rtl/vc707/src/smollm
oracle="$repo/_build/default/test_yosys_oracle_sweep.exe"

[ -d "$src" ] || { echo "smollm source not at $src" >&2; exit 1; }
[ -x "$oracle" ] || ( cd "$repo" && eval "$(opam env)" && \
  dune build _build/default/test_yosys_oracle_sweep.exe )

work=$(mktemp -d)
trap "rm -rf $work" EXIT
log="$work/sweep.log"

# Every non-selftest .sv that's reachable from the board top.
# `vc707_microgpt_eth.sv` is excluded from the project-set list
# because it instantiates `*_selftest` wrappers (which include
# generated data .svh files we don't have); when it's in the file
# list, verilator's elaborator follows the instantiations and fails
# on missing layer_hidden_in_packed.svh.  The board top is
# vendor-IP-bound regardless, so we report it as expected-LOADFAIL
# in a separate row.
all_files=$(ls "$proj_root"/*.sv 2>/dev/null \
            | grep -v selftest \
            | grep -v vc707_microgpt_eth.sv)
all_files+=" $(ls "$src"/*.sv 2>/dev/null | grep -v selftest)"

# (module_name, file_stem) pairs from the headers — same regex as the
# old script; the file stem is informational only.
modules=$(grep "^module" $all_files \
          | sed -E 's@.*/([^/]+)\.sv:module ([A-Za-z_][A-Za-z0-9_]*).*@\2|\1@')

echo "═══════════════════════════════════════════════════════════════"
echo "  microgpt + smollm sweep: verilator ↔ verible (Z3 parallel-correctness)"
echo "  top: vc707_microgpt_eth   modules: $(echo "$modules" | wc -l)"
echo "  preprocessor: Sv_preproc (in-OCaml; --incdir-driven)"
echo "═══════════════════════════════════════════════════════════════"

set +e
for entry in $modules; do
    top=$(echo "$entry" | cut -d'|' -f1)
    file=$(echo "$entry" | cut -d'|' -f2)
    # Pass all files + both incdirs to the harness; it handles preprocess
    # + module resolution per frontend.
    verdict=$("$oracle" \
                --oracle verilator --peer verible \
                --top "$top" \
                --incdir "$proj_root" --incdir "$src" \
                $all_files 2>/dev/null \
                | grep -oE "EQUIV|NOTEQUIV|LOADFAIL|Z3ERR|NOTOP" \
                | head -1)
    verdict=${verdict:-NORESULT}
    printf "  [%-8s] %-30s  (top=%s)\n" "$verdict" "$file" "$top"
    echo "$verdict $file" >> "$log"
done
set -e

echo
echo "──────────── tally ────────────"
awk '{print $1}' "$log" | sort | uniq -c | sort -rn \
    | awk '{printf "  %-10s %d\n", $2, $1}'
echo "  TOTAL      $(wc -l < $log)"
