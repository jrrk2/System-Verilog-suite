#!/bin/bash
# Vivado-mapped vs SVS-gate-map Z3 miter over a set file (name|top|origfile).
# Requires /tmp/eb/xf/<name>_viv.v (Vivado OOC netlist) to exist.
cd /home/jonathan/System-Verilog-suite; eval $(opam env)
EXE=./_build/default/sv_suite.exe
SET=$1
pass=0; diff=0; err=0; noviv=0
while IFS='|' read -r name top orig; do
  [ -z "$name" ] && continue
  viv=/tmp/eb/xf/${name}_viv.v
  if [ ! -s "$viv" ]; then echo "NOVIV   $name"; noviv=$((noviv+1)); continue; fi
  cat > /tmp/eb/xf/one.lua <<LUA
viv = svd.augment_xil_models(svd.parse("verible", "$top", {"$viv"}))
p = svd.parse("verible", "$top", {"$orig"})
p = svd.unroll(p); p=svd.inline(p); p=svd.iflift(p); p=svd.blocking_subst(p); p=svd.meminfer(p); p=svd.memlower(p)
im = svd.augment_xil_models(svd.mapped_to_prog(svd.gate_map(svd.pick(p, "$top"), 6, 0)))
print("MITER_RESULT " .. svd.miter(svd.pick(viv, "$top"), svd.pick(im, "$top")))
LUA
  r=$(MEMLOWER_FPGA=1 FPGA_LEC_NAMES=1 timeout 300 $EXE script /tmp/eb/xf/one.lua 2>/dev/null | grep -a MITER_RESULT | head -1)
  v=$(echo "$r" | sed 's/.*MITER_RESULT //')
  case "$v" in
    EQUIVALENT) echo "EQUIV   $name ($top)"; pass=$((pass+1));;
    *DIFFER*)   echo "DIFFER  $name ($top)"; diff=$((diff+1));;
    *)          echo "ERROR   $name ($top)  [$v]"; err=$((err+1));;
  esac
done < "$SET"
echo "== SUMMARY: EQUIV=$pass DIFFER=$diff ERROR=$err NOVIV=$noviv =="
