#!/bin/bash
# Gate-level co-simulation: the source RTL against this suite's LUT netlist
# simulated with Xilinx's own unisim models (Vivado 2020.1, three small patches
# in test/qor/gl/unisims_v for Verilator), same random inputs, every output
# compared every cycle.  The oracle for memory and DSP mappings, which the
# BIR-level models cut as black boxes.
#   test/qor/gl_cosim.sh <file.sv> <top> [cycles=5000] [extra verilator args]
#   GL_PIPELINE="unroll inline flatten iflift blocking_subst meminfer memlower srl_infer dsp_infer"
#   GL_OUT=<dir>   GL_DEFINES="-DSYNTHESIS"
#   GL_NET_V=<file.v>  simulate THIS behavioural Verilog of <top> against the
#                      source instead of mapping: the importer oracle (a front
#                      end's dump against the RTL Verilator reads itself)
set -u
HERE=$(cd "$(dirname "$0")" && pwd); ROOT=$(cd "$HERE/../.." && pwd)
SRC=$(cd "$(dirname "$1")" && pwd)/$(basename "$1"); TOP=$2; CYC=${3:-5000}; shift 3 2>/dev/null || shift $#
U=${XIL_UNISIM_SRC:-/home/Xilinx/Vivado/2020.1/data/verilog/src}
# Verilator rejects three idioms in the unisims: `pin === 1'bz` / `!== 1'bz`
# (never true on a driven pin: folded to a constant), and one DSP48E1
# `always @(x) ... <=` block (made @* with blocking assigns).  Patched copies
# of the whole library are cached once.
UV=${GL_UNISIMS:-$HOME/.cache/svs-unisims-$(basename "$(dirname "$(dirname "$(dirname "$U")")")")}
if [ ! -f "$UV/.patched" ]; then
  mkdir -p "$UV"
  for f in "$U"/unisims/*.v; do
    sed -E -e "s/\(\s*[A-Za-z_][A-Za-z0-9_]*\s*!==\s*1'bz\s*\)/1'b1/g" -e "s/\(\s*[A-Za-z_][A-Za-z0-9_]*\s*===\s*1'bz\s*\)/1'b0/g" "$f" > "$UV/$(basename "$f")"
  done
  python3 -I - "$UV/DSP48E1.v" <<'PY'
import sys, re
p = sys.argv[1]; t = open(p).read()
t = t.replace("    always @(qc_o_mux) begin\n      if (USE_PATTERN_DETECT == \"NO_PATDET\") \n        the_mask <= {48{1'b1}};",
              "    always @* begin\n      if (USE_PATTERN_DETECT == \"NO_PATDET\") \n        the_mask = {48{1'b1}};")
t = re.sub(r'("MASK"\s*:\s*the_mask\s*)<=', r'\1=', t)
t = re.sub(r'("C"\s*:\s*the_mask\s*)<=', r'\1=', t)
t = re.sub(r'("ROUNDING_MODE[12]"\s*:\s*the_mask\s*)<=', r'\1=', t)
open(p, 'w').write(t)
PY
  touch "$UV/.patched"
fi
OUT=${GL_OUT:-$HERE/out_gl/$TOP}; mkdir -p "$OUT"; export TMPDIR=$OUT
PIPE=${GL_PIPELINE:-"unroll inline flatten iflift blocking_subst meminfer memlower srl_infer dsp_infer"}
[ -n "${GL_NET_V:-}" ] && PIPE=""   # the source is only parsed for its ports and clocks
{
  echo "prog = svd.parse(\"verible\", \"$TOP\", {\"$SRC\"})"
  for p in $PIPE; do echo "prog = svd.$p(prog)"; done
  # the module's ports as the SOURCE declares them: a netlist that has lost
  # an input (an unread register's source) has no port for it, and a
  # testbench built from the netlist's ports would tie that input to zero on
  # the RTL side too and compare nothing
  [ -z "${GL_NET_V:-}" ] && echo "svd.write_verilog(prog, \"$OUT/src_bir.v\")"
  # ...and asked of the BIR directly (ports in declaration order, clocks and
  # resets with their edges), not scraped from that text with a regex
  echo "svd.write_text(\"$OUT/ports.txt\", svd.ports(prog, \"$TOP\"))"
  echo "svd.write_text(\"$OUT/clocks.txt\", svd.clocks(prog, \"$TOP\"))"
  if [ -n "${GL_NET_V:-}" ]; then
    echo "-- GL_NET_V: no mapping, $GL_NET_V is the netlist side"
  elif [ "${GL_STAGE:-net}" = bir ]; then
    # the BIR after the pipeline, as behavioural Verilog with the primitive
    # instances memlower/dsp_infer made: isolates the front end and the BIR
    # passes from gate_map
    echo "svd.write_verilog(prog, \"$OUT/net.v\")"
  else
    echo "mapped = svd.gate_map(svd.pick(prog, \"$TOP\"), 6, 0)"
    echo "svd.write_cellmapped_v(mapped, \"$OUT/net.v\")"
  fi
} > "$OUT/map.lua"
(cd "$ROOT" && "${SV_SUITE:-./_build/default/sv_suite.exe}" script "$OUT/map.lua") > "$OUT/map.log" 2>&1 || { echo "GL-COSIM $TOP: mapping failed ($(grep -m1 -iE 'error|exception' "$OUT/map.log"))"; exit 2; }
[ -s "$OUT/ports.txt" ] || { echo "GL-COSIM $TOP: no ports (svd.ports)"; exit 2; }
[ -n "${GL_NET_V:-}" ] && cp "$GL_NET_V" "$OUT/net.v"
if [ -n "${GL_NET_V:-}" ] || [ "${GL_STAGE:-net}" = bir ]; then
  sed -e "s/^module $TOP\b/module ${TOP}_gl/" "$OUT/net.v" > "$OUT/net_r.v"
  printf '`timescale 1ps/1ps\nmodule %s_gl_glbl; glbl glbl(); endmodule\n' "$TOP" > "$OUT/wrap.sv"
else
  python3 -I "$HERE/gl/wrap.py" "$OUT/net.v" "$OUT/net_r.v" "$OUT/wrap.sv" "$TOP" "${TOP}_gl" --ports "$OUT/ports.txt" > "$OUT/wrap.log"
fi
python3 -I "$HERE/gl/gen_tb.py" "$OUT/ports.txt" "$TOP" "$CYC" "$OUT/clocks.txt" > "$OUT/tb.sv"
rm -rf "$OUT/obj"
[ "${GL_VCD:-0}" = 1 ] && TRACE="--trace" || TRACE=""
verilator --binary --timing $TRACE -Wno-fatal -Wno-lint -Wno-style -Wno-BLKANDNBLK -Wno-MULTIDRIVEN -j "${GL_JOBS:-4}" -CFLAGS "${GL_CFLAGS:--O0}" \
  --top-module tb -Mdir "$OUT/obj" ${GL_DEFINES:--DSYNTHESIS} -I"$(dirname "$SRC")" \
  -y "$UV" "$U/glbl.v" "$OUT/tb.sv" "$OUT/wrap.sv" "$OUT/net_r.v" "$SRC" "$@" > "$OUT/build.log" 2>&1 \
  || { echo "GL-COSIM $TOP: verilator build failed ($(grep -m1 '%Error' "$OUT/build.log"))"; exit 2; }
# a fixed seed: the same stimulus every run, so two runs' VCDs line up
(cd "$OUT" && ./obj/Vtb +verilator+seed+${GL_SEED:-1} $([ "${GL_VCD:-0}" = 1 ] && echo +vcd)) > "$OUT/sim.log" 2>&1
r=$(grep -m1 "^GL-COSIM" "$OUT/sim.log"); grep -m4 MISMATCH "$OUT/sim.log"
echo "GL-COSIM $TOP: ${r#GL-COSIM: }"
case "$r" in *MATCH*) exit 0;; *) exit 1;; esac
