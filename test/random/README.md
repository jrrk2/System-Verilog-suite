# Constrained-random SV testcase generator

`random_sv_gen.exe` emits small synthesizable SystemVerilog modules
that exercise the patterns the Verilator↔Verible miter cares about,
then runs each through the miter and stashes any failing seed +
generated `.sv` for repro.

## What it covers

For each module:

- Mixed input/output ports with random widths (1, 2, 4, 5, 7, 8, 16, 32 bits)
- Comma-grouped port declarations (`input [3:0] a, b, c`)
- Mix of continuous assigns and `always_ff` with sync/async reset
- Random expressions: arithmetic, logical, ternary, slice, concat,
  replicate, bit-select, unary

Reproducible: same `--seed` always emits the same module.

## Usage

```
# 100 random cases, save passes too:
_build/default/random_sv_gen.exe --seed 1 --n 100 --keep-pass

# Just print one module to stdout (debug the generator):
_build/default/random_sv_gen.exe --seed 42 --emit-only

# Re-run a specific failing seed end-to-end:
_build/default/random_sv_gen.exe --seed 1 --n 1 --keep-pass

# Custom output dir:
_build/default/random_sv_gen.exe --seed 1 --n 200 --out /tmp/sv_fuzz
```

Each failing case lands in `<out>/found/rand_<seed>.sv` plus an
`INDEX` line with the first-line diagnostic.

## Triage workflow

1. `random_sv_gen.exe --seed $START --n 200` → batch of failures
2. `cat /tmp/random_sv/found/INDEX` → group by error class
3. Pick the smallest failing case, reduce by hand if needed, add to
   `test/regressions/` as a proper named regression
4. Fix the underlying bug, re-run the random sweep to confirm no new
   regressions emerged

The generator finds three broad classes of bugs:
- **"outputs differ"** — semantic mismatch between Verilator and
  Verible BIR. Often: width inference for sign-extended or negated
  expressions, or ordering issues in nested ternaries.
- **"Sorts incompatible"** / **"Argument has sort"** — Z3 width
  inference disagreement during miter encoding. Usually narrows down
  to one mishandled operator (shift, slice, concat).
- **"verilator failed"** (rare with current generator constraints) —
  the generator emitted invalid SV. Each new failure of this kind
  belongs in the generator as a constraint, not the converter.
