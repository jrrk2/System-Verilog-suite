# Predictor calibration sweep (picosoc, 5 layout variants)

Tracks how the `test_placement_timing.exe` analytical-arrival predictor
agrees with measured ORFS post-CTS WNS across multiple layout
configurations of the same design (picosoc on nangate45, 1.1 ns SDC
clock).

## Running

```sh
bash run_picosoc_sweep.sh > picosoc_sweep.tsv
```

The script expects ORFS layout artifacts under

```
$HOME/OpenROAD-flow-scripts/flow/results/nangate45/picosoc/<variant>/6_final.def
$HOME/OpenROAD-flow-scripts/flow/reports/nangate45/picosoc/<variant>/6_finish.rpt
```

— populate it by running ORFS with the desired knobs (kary_merge on/off,
slack ref on/off, baseline yosys, etc.) and either renaming the
timestamped directory or letting the sweep enumerate them as-is.

## Current snapshot (paper-v2)

| Variant | pred_arr (ps) | pred_wire (ps) | pred_tot (ps) | meas WNS (ps) | meas path (ps) | ratio |
|---|---:|---:|---:|---:|---:|---:|
| 20260511_112016 (stock yosys, no kary) | 8057.0 | 99.5 | 8156.5 | -6470 | 7570 | 0.93 |
| 20260513_073313 (stock yosys + kary) | 4226.8 | 89.3 | 4316.2 | -2810 | 3910 | 0.91 |
| baseline_no_kary (decomp synth, no kary) | 1439.8 | 117.6 | 1557.4 | -2260 | 3360 | 2.16 |
| dup_no_ref (kary on, no slack ref) | 1342.6 | 40.0 | 1382.6 | -580 | 1680 | 1.22 |
| dup_with_ref (kary on, slack ref on) | 1342.6 | 40.0 | 1382.6 | -580 | 1680 | 1.22 |

Ratio = (measured achieved path = SDC + |WNS|) ÷ (predictor total arrival).

## Reading the table

The predictor agrees within ~10 % on the two stock-yosys runs (ratios
0.91, 0.93) because their critical paths are dominated by intrinsic
cell delay — long chains, comparatively little routing slop — where
the predictor's Liberty NLDM lookup is already accurate.

It is **2.16× optimistic on `baseline_no_kary`** because the predictor
underestimates wire delay on a still-long-chain netlist where the wire
contribution is large relative to a small arrival.  Once `kary_merge`
collapses the chain into wider gates (`dup_*`), the predicted wire
shrinks to 40 ps (from 117 ps) and the ratio drops to 1.22 — much
closer to the +500 ps of clock-to-Q + setup that the predictor doesn't
model.

## Calibration knobs

The predictor exposes `wire_cap_ff` (default 0.5 fF/μm) and uses Liberty
NLDM cell delay tables.  Wire delay is currently a simple
`wire_cap × R × length` model and explains the 2.16× outlier — the
predictor is structurally optimistic on long chains where wire is the
dominant term.  A proper RC model (per-net resistance from net length
and metal stack) would move the baseline_no_kary ratio toward 1.0;
the existing kary-merged variants are well-served by the current
intrinsic-delay-only path.

The predictor also does not include:
- clock-to-Q (FF launch delay) — ~150 ps for DFF_X1 at typical load
- setup time at the capture FF — ~80 ps
- clock skew (small for the picosoc design — `-0.01 ps` from 6_finish)
- OCV / setup-derating pessimism (off in this ORFS config)

Adding ~250 ps of constant FF launch+capture overhead to every total
would move the four well-matched ratios closer to 1.0 (0.93 → 1.00,
0.91 → 0.97, 1.22 → 1.04) and lift the baseline_no_kary outlier to
2.31 — clearly out of band, confirming the wire-delay model as the
primary calibration gap.

## What's stable, what's not

The two `dup_*` variants produce IDENTICAL predictor outputs — the
slack-feedback knob doesn't change the gate-level netlist (only which
cells we'd promote on a future run), so the predictor sees the same
DEF.  The slack-feedback is consequently a no-op on picosoc per the
README's note that its critical path is single-fanout end-to-end.

The two stock-yosys runs have very different ratios from each other
(0.93 vs 0.91) but trail the decomp-synth runs by 2-5× in raw delay
— the predictor IS picking up the gate count difference, just not the
absolute scale.

## Open work

- Add an RC-aware wire model (per-segment R from metal-stack +
  per-net C from fanout).  Expected to lift `baseline_no_kary` from
  2.16 to ~1.3.
- Add a constant FF launch/capture offset (~250 ps for nangate45
  typical) as a `--with-ff-overhead` flag.
- Sweep against gcd, AES, and the 8-bit MAC reference layouts (each
  needs its own ORFS run to capture measured WNS).  The MAC reference
  point already in the paper (predicted 592.7 ps vs measured
  592.67 ps, ratio 1.00005) is the one calibration data point where
  the predictor sees a single-cell swap delta — that's the cleanest
  comparison because clock-to-Q / setup cancel out of the delta.

## Predictor-only side: smollm sweep

A second sweep — `run_smollm_sweep.sh` → `smollm_sweep.tsv` — runs the
synth pipeline + arch_swap predictor over every standalone module in
`$HOME/TALOS-V2/rtl/vc707/src/smollm/`, capturing predicted-side
metrics only (no ORFS layouts have been run for smollm yet).  Useful
as a "what would the predictor recommend?" inventory until the layout
side catches up.

Snapshot (per-module 120 s cap):

| | count |
|---|---:|
| OK (synth + predictor passed) | 19 |
| FAIL (frontend gap, mostly `_bfp` variants) | 6 |
| TIMEOUT@120s (deep hierarchy: `smollm_layer` / `_multilayer` / `_full`) | 10 |

Predicted arch-swap savings on the OK subset (sorted by total Δ-stages):

| Module | Cells | Swaps | Δ-stages |
|---|---:|---:|---:|
| `softmax_q15` | 5 | 8 | **10 367** |
| `smollm_decode_head` | 37 | 31 | 8 976 |
| `rope` | 10 390 | 40 | 6 933 |
| `rmsnorm` | 5 | 9 | 6 593 |
| `matvec_int8_engine` | 3 | 3 | 1 783 |
| `microgpt_eth_ctrl` | 1 093 | 18 | 1 035 |
| `cordic_sincos` | 3 | 9 | 690 |
| `weight_stream_axi` | 665 | 6 | 504 |
| `weight_streamer` | 51 | 3 | 242 |

(Modules with 0 candidates omitted: 10 of the 19 OK rows are
arithmetic-light and have no swap candidate above the width
threshold of 8.  `cordic_sincos`, `rmsnorm`, `softmax_q15`, and
`matvec_int8_engine` look small in the cells column because most of
their logic is hidden in arch blocks — the predictor counts the
arch block as a single cell, not its bit-blasted contents.)

What the predicted Δ-stages mean: each block has a baseline (e.g.
32-bit ripple add = 96 stages of 2-XOR-cell delay) and an alternative
(e.g. brent-kung = 11 stages).  Δ-stages is the stage-count drop the
predictor would gate on if `SV_DECOMP_ARCH_SWAP=1` were set AND the
slack-feedback gate said it was on the critical path.  No layout has
yet been run to confirm any of these.

`rope` is the standout: 10 390 cells after lib_map (consistent with
its complex BFP CORDIC chain) and 40 swap candidates.  Pushing rope
through ORFS would give the dataset a third large-design data point
alongside picosoc and (once captured) AES.

Failures and timeouts are inputs to other tracks:

- `_bfp` variants (block-floating-point) fail BIR conversion — most
  likely a frontend gap around the BFP-specific struct types.  Worth
  tracking but not a predictor concern.
- `smollm_layer` / `_multilayer` / `_full` time out at 120 s — these
  instantiate deep hierarchies with the multi-write-port memlower
  blocker noted in `memory/project_smollm_multiwrite_blocker.md`.
  Increasing the timeout to 600 s would probably pull a few more in
  but not the multilayer family until that blocker is fixed.
