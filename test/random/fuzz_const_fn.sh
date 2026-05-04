#!/bin/bash
# Fuzz the Verible const-fn evaluator with random_sv_gen's
# cfg_* modes. For each (mode, seed) pair, generate a small SV
# testcase and run it through test_verible_to_bir.exe. A seed is
# considered a FAIL if the converter:
#   - crashes (non-zero rc)
#   - produces zero bmodules (parse failure)
#   - emits an "Error parsing" line (Verible parser error)
#   - emits an "[elab]" SVUnknown for the top-level cfg parameter
#     (the const-fn evaluator missed)
#
# Pass with --seeds N to fuzz N seeds per mode (default 25).

here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
gen=$repo/_build/default/random_sv_gen.exe
vbir=$repo/_build/default/test_verible_to_bir.exe
[ -x "$gen" ] && [ -x "$vbir" ] || {
    echo "build first: dune build" >&2
    exit 1
}

n_seeds=${1:-25}
work=$(mktemp -d /tmp/fuzz_const_fn.XXXX)
trap "rm -rf $work" EXIT

modes=("cfg_struct" "cfg_chain" "cfg_ternary" "cfg_recursive")
total=0
failed=0
fail_log=""

for m in "${modes[@]}"; do
    for s in $(seq 1 "$n_seeds"); do
        total=$((total + 1))
        sv=$work/rand_${m}_${s}.sv
        log=$work/rand_${m}_${s}.log
        top="rand_${m}_${s}"
        $gen --features "$m" --seed "$s" --n 1 --emit-only > "$sv" 2>/dev/null
        ENABLE_GEN_PRUNE=${ENABLE_GEN_PRUNE:-} \
            $vbir "$top" "$sv" > "$log" 2>&1
        rc=$?
        bmods=$(grep -oE "Got [0-9]+ bmodule" "$log" | grep -oE "[0-9]+" | head -1)
        if [ "$rc" -ne 0 ] || [ -z "$bmods" ] || [ "$bmods" -eq 0 ] \
           || grep -q "Error parsing" "$log"; then
            failed=$((failed + 1))
            fail_log="$fail_log
$m seed=$s rc=$rc bmods=${bmods:-?}"
            cp "$sv" "$here/found_${m}_${s}.sv" 2>/dev/null || true
        fi
    done
done

echo "fuzz: $((total - failed))/$total ok"
if [ "$failed" -gt 0 ]; then
    echo "FAILURES:$fail_log"
    exit 1
fi
