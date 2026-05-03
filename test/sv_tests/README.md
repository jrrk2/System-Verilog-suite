# sv-tests integration

Wires three System-Verilog-decompiler frontends into the
[chipsalliance/sv-tests](https://github.com/chipsalliance/sv-tests)
corpus (~616 tests organised by IEEE 1800 chapter) so we can:

- compare our pass rate against established tools (Verilator, Slang,
  Surelog, Yosys, Icarus, sv-parser, …) on identical input,
- get a per-feature breakdown via sv-tests' HTML dashboard, and
- track historical drift each time we change a converter.

## Three runners

| Runner name | What passes | When to read it |
|---|---|---|
| `decompiler_verible_parse` | Verible→BIR converted ≥ 1 module | "did Verible accept this construct?" |
| `decompiler_verilator_parse` | Verilator→JSON→BIR converted ≥ 1 module | "did Verilator accept this construct?" |
| `decompiler_miter` | both converters AND Z3-equivalence | "do the two converters agree on semantics?" |

The miter is strictest — it surfaces disagreement between the two
frontends that the per-frontend pair would miss. The pair runners
isolate which frontend rejected a given test.

## One-time install

```sh
bash test/sv_tests/install.sh
```

This:
1. Clones sv-tests into `~/sv-tests` (override with `SV_TESTS_DIR=…`).
2. Installs Python deps from sv-tests' `conf/requirements.txt`.
3. Symlinks our three runners into `~/sv-tests/tools/runners/` and the
   wrappers into `~/sv-tests/tools/wrappers/`.

## Running

```sh
bash test/sv_tests/run.sh                # default: just decompiler_miter
bash test/sv_tests/run.sh all            # all three runners
bash test/sv_tests/run.sh chapter-5      # single chapter for triage
```

Reports per-runner pass count to stdout. Per-test logs land under
`~/sv-tests/out/logs/<runner>/`.

For the dashboard:
```sh
cd ~/sv-tests
make report
xdg-open out/report.html
```

## How tests with `\`include` / `\`define` are handled

Many sv-tests cases use `:incdirs:` and `:defines:` metadata that our
exes don't natively understand. The runners route through
`tools/wrappers/decompiler_flatten.sh`, which:

1. Runs `verilator -E -I... +define+...` to expand the includes and
   macros into a single self-contained `.sv`.
2. Invokes the target exe on that flattened result.

Tests that fail under verilator's preprocessor (e.g. SystemVerilog
constructs verilator itself doesn't yet handle) get reported as
failures — accurately reflecting that we can't currently reach them
end-to-end.

## Files

- `install.sh` — bootstrap.
- `run.sh` — convenience driver + pass-rate summary.
- `runners/Decompiler_*.py` — three sv-tests `BaseRunner` subclasses.
- `wrappers/decompiler_flatten.sh` — verilator-E flatten + exe call.
- `wrappers/decompiler_verilator_parse.sh` — verilator JSON + BIR
  conversion (used by the Verilator-side parse runner).

## Where we stand (Vivado-oracle anchored)

The `unsynthesizable: 1` metadata is conservative — many tests it
labels as synthesisable actually aren't.  The real oracle is what
Vivado 2020.1's `synth_design -rtl` accepts. Of 649 in-scope tests
(after our metadata-based pre-filter; chapter-18 randomization
skipped because it segfaults Vivado), Vivado synthesises **497**.

Recall against that 497-test ground-truth, with brain-dead semantic
checks enabled:

| Runner | Pass on Vivado-synth | False positives (we accept, Vivado rejects) |
|---|---|---|
| `Decompiler_Verible_Parse` | **443 / 497 (89%)** | 91 |
| `Decompiler_Verilator_Parse` | 440 / 497 (88%) | 72 |
| `Decompiler_Miter` | **419 / 497 (84%)** | 72 |

The 78 Vivado-synth tests we miss cluster on:

| Chapter | # | What's there |
|---|---|---|
| chapter-22 | 21 | compiler directives (`timescale`, `default_nettype`, …) |
| generic | 15 | preproc, number formats, typedefs |
| chapter-9 | 9 | `always_*` block variants |
| chapter-11 | 9 | tagged unions, indexed part-selects |
| chapter-6 | 7 | vector/packed/unpacked types |
| chapter-5 | 6 | lexical edge cases |
| chapter-16 | 4 | properties / assertions |
| chapter-7 | 3 | arrays |
| chapter-10 | 3 | procedural assigns with delays |
| chapter-8 | 1 | class |

The 72 false positives are mostly the should_fail set (semantic
errors our parse-only check can't see). The brain-dead semantic
pass (`behavioral_sanity.ml`) catches multi-driver and mixed
proc/cont; deeper checks (typedef refs, void-fn-returns-value,
enum value validation) need elaboration-time tracking to fix.



All three runners skip tests where `unsynthesizable: 1` is set in
the metadata or whose tags include `uvm` / `testbench` — the
decompiler targets synthesis, not simulation testbenches. That filter
removes 410/1027 tests, leaving **617 in-scope**:

| Runner | Pass | % |
|---|---|---|
| `Decompiler_Verible_Parse` | 617 / 617 | **100%** |
| `Decompiler_Verilator_Parse` | 324 / 617 | 52% |
| `Decompiler_Miter` | 312 / 617 | **50%** |

Verible accepts **every** synthesisable test. Verilator-side is the
binding constraint — but **292/293 remaining Verilator-side fails
are Verilator itself rejecting the source**, not our converter (e.g.
SV constructs verilator's own parser doesn't accept). Per-user
constraint, we don't modify verilator itself.

Of the converter-fixable space, this run added:

- `===` / `==?` / `!==` / `!=?` (case-equality / wildcard) treated
  as plain `==` / `!=` for the X/Z-free synthesis subset
- `SLICESEL` (unpacked array part-select `arr[hi:lo]`) routed
  through the existing `Sel` path
- Graceful stubs for non-synthesisable tags (string methods,
  dynamic / associative arrays, file I/O, `$dist_*`, classes,
  `force x = …`) so other modules in the same file still convert

## Vivado oracle

The "synthesisable" subset isn't really `:unsynthesizable: 0` in the
metadata — the metadata is conservative. The true oracle is whether
Vivado's `synth_design -rtl` accepts the file.

```sh
bash test/sv_tests/vivado_baseline.sh        # ~1 hour
bash test/sv_tests/compare_to_vivado.sh      # diff against our runners
```

`vivado_baseline.sh`:
1. Walks all in-scope tests, pre-flattens each with `verilator -E`.
2. Extracts the first `module X` name as the top.
3. Builds a TSV manifest, then runs Vivado in **a single batch session**
   (`vivado_batch_synth.tcl`) that loops `read_verilog → synth_design
   -rtl -top → close_design` per test.

Output: `$SV_TESTS_DIR/out/vivado_baseline.csv` with PASS/FAIL +
elapsed ms + first error excerpt per test.

`vivado_baseline.sh` also converts the CSV into per-test logs under
`out/logs/Vivado_Synth_RTL/`, so the next `make report` adds a
**Vivado_Synth_RTL** column to the HTML grid alongside our three
Decompiler columns. The column shows green for tests Vivado
synthesised, red for those it rejected — directly comparable to
the others on a per-test basis.

`compare_to_vivado.sh` then diffs the Vivado-accepted set against our
three runners' pass-sets:

- **pass on both** = our runner agrees with the oracle ✅
- **pass on miter only** = our runner accepts what Vivado rejects ⚠️
  (might be a false positive — the test isn't really synthesisable)
- **pass on Vivado only** = synthesisable but our converter rejected
  it → triage candidate

The "Vivado only" set is the most actionable list of "what to fix
next in the converters".

## Out of scope

- A `Decompiler_Yosys` 4th runner (Yosys↔Verible miter) — easy to
  add later, same pattern.
- CI integration.
- Auto-bisecting failures back to converter source lines.
