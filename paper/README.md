# Paper draft

`paper.tex` — arxiv-style draft describing the verification-first
synthesis path. Build with any LaTeX distribution that has `amsmath`, `booktabs`,
`listings`, `xcolor`, `hyperref`, `microtype`, and `lmodern`. The
`lmodern` package provides scalable Computer Modern; without it,
microtype's font-expansion pass aborts with
"auto expansion is only possible with scalable fonts" on TeX Live
distributions whose default CM fonts are bitmap-only. No bibtex run
needed (`thebibliography` inline).

```sh
cd paper
pdflatex paper.tex && pdflatex paper.tex   # twice for cross-refs
```

On Debian / Ubuntu the prerequisite packages are
`texlive-latex-recommended` (article, amsmath, hyperref, lmodern,
booktabs, caption, xcolor) and `texlive-latex-extra` (microtype,
listings).

The three contributions claimed:

1. **Cell-mapped equivalence** — prove our gate-level Verilog
   equivalent to source SV by re-parsing the cell-mapped output
   and expanding each Liberty cell via its `function` /
   `next_state` strings into a BIR fragment, then SMT-mitering
   against the source. Implemented in `test_synth_equiv.exe`.

2. **Compositional verification by boundary substitution** —
   parent-module miters replace each child instance with a Z3
   uninterpreted-function `BCall` whose name encodes the
   `(child_module, output_port)` pair. Identical names + identical
   args on both sides give the leaf for free. Sequential children
   get their outputs promoted to primary inputs (depth-1
   sequential check). Implemented in
   `behavioral_boundary.ml` + the `BCall` clause of
   `z3_miter.ml`.

3. **Cert-gated post-layout resynthesis** — analytical depth ratios
   for arithmetic alternates (Wallace, Dadda, Brent–Kung,
   Sklansky, Kogge–Stone) project a critical-path delay; the
   recommendation is only emitted when a Z3 leaf certificate
   exists for the target arch+width. Predict-vs-measured error
   on an 8-bit MAC laid out through ORFS: 0.005 %.

Implementation details and reproduction commands are in the main
`README.md` and in each evaluation point's test directory.
