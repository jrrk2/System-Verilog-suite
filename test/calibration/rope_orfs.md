# rope ORFS layout attempt (2026-05-14)

## Result

ORFS flow completed all layout stages on `rope` (synth → floorplan →
place → CTS → route → fill → SPEF extract), terminating only on a
crash in OpenROAD's RC-extraction cleanup (`rcx::NameTable::~NameTable`)
during the optional 6_report step.  The routed design is intact at
`results/nangate45/rope_macro/base/6_final.def` / `6_final.odb`.

**But the routed result is degenerate:** post-route `report_wns` gives
0.00 ns, design area 104 µm², 0 % utilization.

## Why (revised after inspecting `1_2_yosys.v`)

The 10 390 cells *do* land in the rope module body: 3762 `XOR2_X1`,
3691 `AND2_X1`, 1836 `OR2_X1`, 903 `AND3_X1`, 124 `LOGIC0_X1`, and the
expected scattering of `INV` / `MUX2` / `DFF`.  Arch-block library
search ran correctly — Wallace multipliers and Brent-Kung adders are
bit-blasted into XOR/AND trees in the emitted Verilog.

What's broken is the **operand wiring around the arch blocks**.  Inside
the rope module body:

  - `in_x[*]` references: **0**
  - `pos[*]` references:  **0**
  - `out_y[*]` references: **0**
  - only the 1-bit control signals `in_valid` / `start` / `out_valid`
    appear (4-3 references each, the FF enables)

Every assign feeding the gate tree is a giant concat of the form

    {_T__b72896__AUX__B__, _T__b72896__AUX__B__, …,
     _tie_lo_, _tie123__148278_, _tie122__148276_, …}

i.e. an auxiliary signal that resolves to LOGIC0 plus a string of tie
cells.  So the 10 387 gates of arithmetic are driving constants —
electrically valid, but trivially equivalent to all-zero on the
data path.  RSZ-0104 / RSZ-0020 ("18 floating nets") flag the
top-level input buses as having no fanout, and CTS / route happily
pruned everything not reaching an observable, leaving 104 µm² of
control-path debris.

The user's intuition that "libraries were not searched adequately"
was the right shape one level out — the library was searched, the
arch blocks materialised, but the input-bus wiring into those arch
blocks got lost.

### Final root cause (cross-checked Verilator vs Verible frontends)

Running both frontends on the same `rope.sv` and diffing the resulting
BIR pins the bug to Verible's elaborator.  Line 115 of rope.sv:

```systemverilog
ang_t43 = 43'(reg_pos) * {11'b0, FREQ_TURNS_Q31[cord_pair[4:0]]};
```

`FREQ_TURNS_Q31` is an array-typed localparam whose initialiser lives
in the included `rope_freq_turns.svh`.

| | Verible BIR | Verilator BIR |
|---|---|---|
| `FREQ_TURNS_Q31[cord_pair[4:0]]` | `32'0` (silent zero) | `FREQ_TURNS_Q31[cord_pair[4:0]]` preserved |
| `PI_OVER_8_Q29` (scalar localparam) | `32'210828714` ✓ | `PI_OVER_8_Q29` ✓ |
| `mac_q15` function body | 5 params, 1 body stmt | 9 params, 5 body stmts |
| Other localparams (H, H2, ICW, …) | all dropped | preserved |

So Verible's elaborator silently substitutes 0 for array-typed
localparams initialised in include files (and also drops the scalar
localparam *declarations* — though it does inline their values where
referenced).  The multiplier sees `reg_pos * 0`, downstream BIR DCE
collapses the entire chain to constants, and `lib_map` faithfully
materialises a 10 387-gate "compute zero from inputs and tie cells"
network.

Tracked as task #139.  The fix is to either:
  (a) actually look up the .svh-defined initialiser inside the
      elaborator (what Verilator does), or
  (b) raise an explicit elaboration error rather than silently
      substituting zero.

Either fix unblocks calibration layouts for rope, cordic_sincos,
matvec_int8_engine, rmsnorm, softmax_q15 — all of which use the
same shape of .svh-defined lookup tables.

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
