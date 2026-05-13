# Paper changelog

Versions of the paper are tracked as git tags `paper-vN`.  Each tag
points at the commit whose tree produces the numbers reported in
that revision of the paper.  This file lists what changed at each
tag and, if any subsequent commit on `main` materially affects
the published numbers, calls that out so reproducers know what to
adjust.

## paper-v1 — initial draft

- 9-page draft (later 12 with the reproducibility appendix).
- Three contributions:
  1. Cell-mapped equivalence by Liberty function expansion.
  2. Compositional verification by SMT boundary substitution.
  3. Cert-gated post-layout architecture swap with measured
     prediction error 0.005 % on an 8-bit MAC.
- Headline numbers reported at this tag:
  - gcd: 10 / 10 modules formally equivalent.
  - AES: 4 / 4 modules formally equivalent
    (`Z3_MITER_TIMEOUT_MS=120000`).
  - cascade_mac: 3 / 3 modules formally equivalent in ~5 min
    (add48 0.16 s, mul8 137 s, cascade_mac 178 s).
  - 8-bit MAC ORFS layout: predicted 592.7 ps · measured
    592.67 ps · error -0.005 %.
- Reproducibility appendix added with copy-paste commands for
  every numbered result.

Subsequent material changes (none yet) will be listed below as
new bullets under the next `paper-vN` heading.

## paper-v2 — DFT track and v1 follow-up

Material changes since `paper-v1`:

- New Evaluation paragraph "DFT and structural ATPG" describing the
  three optional passes (`scan_insert`, `mem_boundary_scan`,
  `jtag_tap_insert`) and the three-tier fault simulator (random,
  directed, PODEM).  Headline coverage numbers reported:
  picosoc top 85.23 % with full DFT stack, picorv32 CPU 94.04 %,
  AES S-box 93.99 % (PODEM 352 / 1102 patterns).
- Three of the four v1 open-work items removed because they
  landed in tree: cascaded MAC ORFS layout, Verible LHS-context
  width propagation, yosys-as-oracle parallel-correctness sweep.
- The remaining open-work list now lists the auto-cascade
  `gen_mul`, fault-sim pseudo-IO for hard macros (no-area
  alternative to BSC wrap), and Naja interchange certificate
  caching.

Numbers added to the README that are not yet in the paper
(referenced as future tightening if a v3 needs them):

- per-block coverage table for the smaller designs (gcd 100 %,
  distributed\_dual\_port\_ram 99.96 %, fifo\_v3\_small 88.64 %,
  aes\_rcon 72.99 %).
- The picosoc DFT-layer breakdown
  (no-DFT 59.5 % → +SCAN 59.5 % → +HIER\_BSR 83.2 % → +JTAG 85.2 %).
