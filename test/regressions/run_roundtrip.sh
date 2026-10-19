#!/bin/bash
# Writer-roundtrip regression: every sysver_tests design through every front
# end, written back out and re-read (test/qor/roundtrip.sh).  One row per
# design, one column per front end; summary line last.
#   test/regressions/run_roundtrip.sh [design ...]     RT_FRONTENDS="verible yosys verilator"
HERE=$(cd "$(dirname "$0")" && pwd); ROOT=$(cd "$HERE/../.." && pwd)
FES=${RT_FRONTENDS:-"verible yosys verilator"}
want=("$@")
pass=0; total=0
printf '%-28s' design; for fe in $FES; do printf ' %-9s' "$fe"; done; echo
for sv in "$ROOT"/sysver_tests/*.sv; do
  top=$(basename "$sv" .sv)
  if [ ${#want[@]} -gt 0 ]; then printf '%s\n' "${want[@]}" | grep -qx "$top" || continue; fi
  grep -q "^\s*module\s\+$top\b" "$sv" || continue
  printf '%-28s' "$top"
  # sibling files the design instantiates (apb_uart's slib_* children), by name
  deps=""; for d in "$ROOT"/sysver_tests/*.sv; do m=$(basename "$d" .sv); [ "$m" != "$top" ] && grep -qw "$m" "$sv" && deps="$deps $d"; done
  for fe in $FES; do
    total=$((total+1))
    if "$ROOT/test/qor/roundtrip.sh" "$fe" "$top" "$sv" $deps > /dev/null 2>&1; then pass=$((pass+1)); printf ' %-9s' EQ
    else r=$(cat "$ROOT/test/qor/out_rt/$fe/$top/rt.log" 2>/dev/null | grep -m1 "^ROUNDTRIP"); case "$r" in *DIFFER*) printf ' %-9s' DIFFER;; *) printf ' %-9s' FAIL;; esac; fi
  done; echo
done
echo "ROUNDTRIP-SUMMARY: $pass/$total EQUIVALENT"
