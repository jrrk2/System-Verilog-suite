# test/qor — FPGA quality-of-results + correctness regression

Compares this suite's xc7 LUT flow against `yosys synth_xilinx` on the same
sources, and gates every number on formal checks:

* **three-way source vote**: the Verible, yosys-slang (RTLIL) and Verilator
  front-ends are mitered pairwise through Z3 (`svd.miter_hier`, module by
  module with children abstracted).  3/3 means the elaboration is agreed; a
  2/3 names the odd front-end out.
* **netlist check**: the mapped netlist (LUT/FDxE/CARRY4/RAM32M/DSP48E1
  instances with their behavioural models) against a fresh parse of the
  source, hierarchically (`HIER-SUMMARY n/m modules EQUIVALENT`).

```
test/qor/run_qor.sh                 # every design in designs.tsv
test/qor/run_qor.sh uart_receiver   # one design
test/qor/dsp_selftest.sh            # DSP48E1 inference: 6 proofs on 4-bit multipliers
```

Per design, `out/<name>/` holds the per-module SVS JSON (`<mod>.svs.json`),
the merged `<top>.flat.json`, the yosys reference `<top>.yosys.json`, and the
logs (`svs.log` has the `[fpga_opts]` profile line and the HIER verdicts,
`vote.log` the source vote).  `out/results.tsv` is the table.

Columns: LUT FF CARRY4 DSP RAM MUXF SRL depth (depth = longest LUT/MUXF/CARRY4
chain between registers, `depth.py`, a P&R-free timing proxy).  `count.py`
expands hierarchy, so a MAC with nine `mul8` instances counts nine copies.

Knobs: `SVS_PROFILE=legacy` (old everything-off defaults), `QOR_Z3_MS` (solver
budget per miter, default 120000), `QOR_NO_VOTE=1` (numbers only),
`YOSYS_SV2V=1` (feed yosys through sv2v instead of the slang plugin), plus
any `GATE_MAP_*` / `MEMLOWER_FPGA` / `LUTCOVER_BACKEND` override listed in
`fpga_synth/fpga_opts.ml`.

`designs.tsv` columns: name, top, extra modules to map (children, `-` if
none), source files (`$HOME` expanded; a missing file skips the row).
