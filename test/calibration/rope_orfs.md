# rope ORFS layout attempt (2026-05-14)

## Result

ORFS flow completed all layout stages on `rope` (synth → floorplan →
place → CTS → route → fill → SPEF extract), terminating only on a
crash in OpenROAD's RC-extraction cleanup (`rcx::NameTable::~NameTable`)
during the optional 6_report step.  The routed design is intact at
`results/nangate45/rope_macro/base/6_final.def` / `6_final.odb`.

**But the routed result is degenerate:** post-route `report_wns` gives
0.00 ns, design area 104 µm², 0 % utilization.

## Why

The top-level `rope` module is mostly arch-block bindings — its
arithmetic is hidden in `gen_add` / `gen_mul` blocks that
`lib_map.ml` emits as single-cell stubs.  At the gate-Verilog emit
step, only ~3 wrapper cells reach the top netlist, even though the
synth log reports 10 390 cells internally.

OpenROAD's `RSZ-0104` warnings ("Net pos[*] only has one pin")
confirm it: the top-level input buses are tied to placeholder stubs
and never reach a real consumer.

## What landed in tree

- `flow/designs/nangate45/rope_macro/config.mk` updated: explicit
  `DIE_AREA` / `CORE_AREA` (200×200 / 180×180 µm) because
  `CORE_UTILIZATION`-based sizing produces a 9 µm core that's
  narrower than PDN's 28.5 µm metal4 strap.
- `1_2_yosys.v` post-processing recipe (described below) for the
  OpenRAM SRAM wrapper blackboxing.

## Synth-pipeline fixes needed for a meaningful rope layout

1. **Emit arch-block contents as real gates at the top.**
   `behavioral_to_hardcaml.ml` lowers arch blocks via the per-arch
   strategy (Brent-Kung, Wallace, etc.), but only when the block
   reaches the `gen_*` dispatch — wrappers that route through child
   modules end up as opaque stubs.  Fix: inline the arch-block bodies
   inside the parent's `lib_map` output, or wire OpenROAD's `flatten`
   step to descend through them.
2. **OpenRAM SRAM wrapper post-process.**  `mem_macro_resolve.ml`
   inlines the OpenRAM behavioural model wrapper, but OpenSTA can't
   parse `parameter RAM_DEPTH = 1 << ADDR_WIDTH;`.  Workaround
   applied to `1_2_yosys.v` after emit:
   ```python
   # For every `module sram_*(...) ... endmodule` block:
   #   - parse data width / depth from the module name
   #   - replace the body with ports-only declarations
   #   - substitute literal widths for ADDR_WIDTH / DATA_WIDTH
   ```
   The proper fix is in `synth_pipeline.ml`: emit a blackbox stub
   instead of the behavioural wrapper for any macro whose Liberty
   file is also passed via `ADDITIONAL_LIBS` (STA reads timing from
   the Liberty; the body is redundant and STA-incompatible).

## Reproduction once those land

```sh
cd $HOME/OpenROAD-flow-scripts/flow
USE_DECOMP_SYNTH=1 make DESIGN_CONFIG=designs/nangate45/rope_macro/config.mk
# Expected outputs once arch-block inlining is in:
#   results/nangate45/rope_macro/base/6_final.def       — routed layout
#   reports/nangate45/rope_macro/base/6_finish.rpt      — WNS / TNS / fmax
#   logs/nangate45/rope_macro/base/6_report.log         — full STA report
```

Pair the resulting `6_finish.rpt` against the predictor row added by
`run_smollm_sweep.sh` (rope: predicted 40 swap candidates, total
Δ-stages = 6933).  This is the same shape as `picosoc_sweep.tsv` and
slots into the calibration table directly.

## Predictor-side row (already captured)

```
module=rope  cells=10390  swaps=40  total_delta=6933
```

(`test/calibration/smollm_sweep.tsv`, status=OK row from the smollm
sweep.)
