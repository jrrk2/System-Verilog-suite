#!/bin/bash
# FPGA QoR + correctness regression: this suite's LUT flow vs yosys synth_xilinx,
# with a three-way source vote (verible / yosys-slang / verilator through the Z3
# miter) and a STRICT miter of the mapped netlist against the source.
#
#   test/qor/run_qor.sh [design ...]        (default: every row of designs.tsv)
#
# Env:
#   SVS_PROFILE=legacy        run the old everything-off defaults (A/B)
#   YOSYS=<path>              yosys binary (default: ORFS install, then PATH)
#   QOR_OUT=<dir>             output dir (default test/qor/out)
#   QOR_NO_VOTE=1             skip the Z3 checks (numbers only)
#   any GATE_MAP_* / MEMLOWER_FPGA / LUTCOVER_BACKEND override (see fpga_opts.ml)
#
# Output: one TSV row per design in $QOR_OUT/results.tsv plus a summary table.
set -u
HERE=$(cd "$(dirname "$0")" && pwd)
ROOT=$(cd "$HERE/../.." && pwd)
OUT=${QOR_OUT:-$HERE/out}; mkdir -p "$OUT"
YOSYS=${YOSYS:-$HOME/OpenROAD-flow-scripts/tools/install/yosys/bin/yosys}
[ -x "$YOSYS" ] || YOSYS=$(command -v yosys || true)
export YOSYS_BIN="$YOSYS"   # the Lua "yosys" frontend (slang plugin -> RTLIL) finds it here
export Z3_MITER_TIMEOUT_MS=${QOR_Z3_MS:-120000}   # per-miter solver budget
SV_SUITE=$ROOT/_build/default/sv_suite.exe
[ -x "$SV_SUITE" ] || { echo "build first: dune build sv_suite.exe"; exit 2; }

abspath() { local f=$1; f=${f//\$HOME/$HOME}; case $f in /*) echo "$f";; *) echo "$ROOT/$f";; esac; }

RES=$OUT/results.tsv
TAB=$'\t'
echo -e "design\tflow\tLUT\tFF\tCARRY4\tDSP\tRAM\tMUXF\tSRL\tdepth\twall_s\tvote\tnetlist_check" > "$RES"

count_field() { # count_field <counts-line> <key>
  echo "$1" | tr ' ' '\n' | awk -F= -v k="$2" '$1==k {print $2; f=1} END{if(!f) print 0}'
}
summarise() { # summarise <json...> -> LUT FF CARRY4 DSP RAM MUXF SRL depth
  local c; c=$(python3 -I "$HERE/count.py" "$@")
  local lut=$(count_field "$c" LUT) ff=$(count_field "$c" FF) carry=$(count_field "$c" CARRY4)
  local dsp=$(count_field "$c" DSP48E1) muxf=$(count_field "$c" MUXF) srl=$(count_field "$c" SRL)
  local ram=$(( $(count_field "$c" RAMB) + $(count_field "$c" RAM32M) + $(count_field "$c" RAM64M) + $(count_field "$c" RAM32X1D) + $(count_field "$c" RAM64X1D) ))
  local depth=$(python3 -I "$HERE/depth.py" "$@" | awk -F= '{d=$NF+0; if(d>m)m=d} END{print m+0}')
  echo "$lut $ff $carry $dsp $ram $muxf $srl $depth"
}

run_design() {
  local name=$1 top=$2 mods=$3 files=$4
  local d=$OUT/$name; mkdir -p "$d"
  local absfiles=(); local f
  for f in $files; do absfiles+=("$(abspath "$f")"); done
  for f in "${absfiles[@]}"; do [ -f "$f" ] || { echo "== $name: SKIP (missing $f)"; return; }; done
  local lua_files; lua_files=$(printf '"%s",' "${absfiles[@]}"); lua_files=${lua_files%,}
  [ "$mods" = "-" ] && mods=""

  # ---- SVS flow: BIR pipeline -> gate_map per module -> splice -> flat JSON ----
  {
    echo "prog = svd.parse(\"verible\", \"$top\", {$lua_files})"
    for p in unroll inline iflift blocking_subst meminfer memlower srl_infer dsp_infer; do echo "prog = svd.$p(prog)"; done
    echo "print(\"opts: \" .. svd.fpga_opts())"
    for m in $mods $top; do
      echo "mapped = svd.gate_map(svd.pick(prog, \"$m\"), 6, 0)"
      echo "svd.write_mapped_json(mapped, \"$d/$m.svs.json\")"
      echo "prog = svd.splice(prog, \"$m\", svd.mapped_to_prog(mapped))"
    done
    # netlist check: source RTL (fresh parse) vs the mapped program, module by
    # module, children abstracted as uninterpreted functions on both sides
    # (same machinery as recipes/xflow_miter).  HIER-SUMMARY line is the verdict.
    if [ "${QOR_NO_VOTE:-0}" != 1 ]; then
      echo "src = svd.parse(\"verible\", \"$top\", {$lua_files})"
      echo "print(svd.miter_hier(src, prog, \"$top\"))"
    fi
    echo 'print("done")'
  } > "$d/svs.lua"
  local t0=$(date +%s.%N)
  (cd "$ROOT" && "$SV_SUITE" script "$d/svs.lua") > "$d/svs.log" 2>&1
  local svs_wall; svs_wall=$(echo "$(date +%s.%N) - $t0" | bc)
  local svs_stats="- - - - - - - -"
  # one JSON holding every mapped module (children stay as instances; count.py
  # expands them, read_nextpnr_json + the miter's flatten inline them)
  if [ -f "$d/$top.svs.json" ]; then
    python3 -I - "$d/$top.flat.json" "$d"/*.svs.json <<'PY'
import json, sys
out, srcs = sys.argv[1], sys.argv[2:]
mods = {}
for p in srcs: mods.update(json.load(open(p))["modules"])
json.dump({"creator": "svs qor merge", "modules": mods}, open(out, "w"))
PY
  fi
  if [ -f "$d/$top.flat.json" ]; then svs_stats=$(summarise "$d/$top.flat.json")
  else echo "   SVS flow FAILED for $name: $(grep -m1 -i 'error\|exception\|fail' "$d/svs.log")"; fi

  # ---- yosys reference: slang plugin reads the SV (sv2v if YOSYS_SV2V=1) ----
  local read_cmd="plugin -i slang; read_slang --top $top ${absfiles[*]}"
  if [ "${YOSYS_SV2V:-0}" = 1 ]; then
    local sv2v; sv2v=$(command -v sv2v || echo "$HOME/sv2v/bin/sv2v")
    "$sv2v" "${absfiles[@]}" > "$d/sv2v.v" 2> "$d/sv2v.log" && read_cmd="read_verilog -sv $d/sv2v.v"
  fi
  t0=$(date +%s.%N)
  "$YOSYS" -q -p "$read_cmd; hierarchy -top $top; synth_xilinx -top $top -flatten -noiopad -noclkbuf; write_json $d/$top.yosys.json" > "$d/yosys.log" 2>&1
  local y_wall; y_wall=$(echo "$(date +%s.%N) - $t0" | bc)
  local y_stats="- - - - - - - -"
  [ -f "$d/$top.yosys.json" ] && y_stats=$(summarise "$d/$top.yosys.json") || echo "   yosys FAILED for $name: $(grep -m1 -i error "$d/yosys.log")"

  # ---- correctness: 3-way source vote + strict netlist miter ----
  local vote="-" check="-"
  if [ "${QOR_NO_VOTE:-0}" != 1 ]; then
    cat > "$d/vote.lua" <<LUA
a = svd.parse("verible", "$top", {$lua_files})
b = svd.parse("yosys", "$top", {$lua_files})
c = svd.parse("verilator", "$top", {$lua_files})
-- hierarchical miter: module by module with children abstracted, so a design
-- whose flattened datapath is beyond Z3 (24-bit MAC) still gets a verdict
print("VOTE verible/yosys:    " .. svd.miter_hier(a, b, "$top"))
print("VOTE verible/verilator: " .. svd.miter_hier(a, c, "$top"))
print("VOTE yosys/verilator:   " .. svd.miter_hier(b, c, "$top"))
LUA
    (cd "$ROOT" && timeout "${QOR_TIMEOUT:-600}" "$SV_SUITE" script "$d/vote.lua") > "$d/vote.log" 2>&1
    local eq; eq=$(grep -c "whole design EQUIVALENT" "$d/vote.log")
    vote="$eq/3"
    check=$(grep -o "HIER-SUMMARY.*" "$d/svs.log" | tail -1 | sed 's/HIER-SUMMARY //; s/ modules//' | cut -c1-48)
    [ -z "$check" ] && check="no-result"
  fi

  echo -e "$name\tsvs\t${svs_stats// /$TAB}\t$svs_wall\t$vote\t$check" >> "$RES"
  echo -e "$name\tyosys\t${y_stats// /$TAB}\t$y_wall\t-\t-" >> "$RES"
  echo "== $name  svs: LUT/FF/CARRY/DSP/RAM/MUXF/SRL/depth = $svs_stats (${svs_wall}s)  vote=$vote  check=$check"
  echo "   yosys: $y_stats (${y_wall}s)"
}

want=("$@")
while IFS=$'\t' read -r name top mods files; do
  case "$name" in ''|\#*) continue;; esac
  if [ ${#want[@]} -gt 0 ]; then printf '%s\n' "${want[@]}" | grep -qx "$name" || continue; fi
  run_design "$name" "$top" "$mods" "$files"
done < "$HERE/designs.tsv"

echo; echo "results: $RES"; column -t -s $'\t' "$RES"
