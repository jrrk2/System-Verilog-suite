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
