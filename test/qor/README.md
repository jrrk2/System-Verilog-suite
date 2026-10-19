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
test/qor/dsp_selftest.sh            # DSP48E1 inference: 10 proofs (4-bit, split wide)
test/qor/run_vm.sh                  # large benchmark: the OCaml bytecode VM, numbers only
test/qor/attrib.py <netlist.json>   # which registers / RAM ports the LUTs feed
test/qor/vm_cosim.sh                # VM: gate_map input vs output, 3000 random cycles
```

The proofs use CaDiCaL when `sat_backend.ml` finds it (`SVS_CADICAL`, then
`~/bytecode-work/cadical-src/build/cadical`, /usr/local/bin, PATH).  Without
it they fall back to Z3, and the flat MAC proof times out.  A timeout is
reported as `INCONCLUSIVE` in the HIER-SUMMARY line, never as `DIFFER`.

Per design, `out/<name>/` holds the per-module SVS JSON (`<mod>.svs.json`),
the merged `<top>.flat.json`, the yosys reference `<top>.yosys.json`, and the
logs (`svs.log` has the `[fpga_opts]` profile line and the HIER verdicts,
`vote.log` the source vote).  `out/results.tsv` is the table.

Columns: LUT FF CARRY4 DSP RAM MUXF SRL depth (depth = longest LUT/MUXF/CARRY4
chain between registers, `depth.py`, a P&R-free timing proxy).  `count.py`
expands hierarchy, so a MAC with nine `mul8` instances counts nine copies.

`run_vm.sh` maps `~/bytecode/ocaml4142_vm_rtl.sv` (`VM_SRC`, `VM_FRONTEND`
= verible | verilator, `VM_OUT`).  No yosys reader takes that source and a
whole-module vote does not finish, so it reports counts, depth and the
`attrib.py` table of the 15 biggest LUT sinks.

Knobs: `SVS_PROFILE=legacy` (old everything-off defaults), `QOR_Z3_MS` (solver
budget per miter, default 120000), `QOR_NO_VOTE=1` (numbers only),
`YOSYS_SV2V=1` (feed yosys through sv2v instead of the slang plugin), plus
any `GATE_MAP_*` / `MEMLOWER_FPGA` / `LUTCOVER_BACKEND` override listed in
`fpga_synth/fpga_opts.ml`.

`designs.tsv` columns: name, top, extra modules to map (children, `-` if
none, `auto` to take them from the BIR via `svd.children`), source files
(`$HOME` expanded; a missing file skips the row).  `QOR_DESIGNS=<tsv>` runs
another table; `QOR_SVS_TIMEOUT=<s>` bounds the SVS flow per design.

The `roundtrip` column is `roundtrip.sh` over verible, yosys and verilator
(`QOR_NO_ROUNDTRIP=1` skips it).

### `roundtrip.sh` — the writer as an oracle

    test/qor/roundtrip.sh <frontend> <top> <file.sv>...

Parses with `<frontend>`, writes the tree back out with `svd.write_verilog`,
re-reads that with verible and miters the two (`svd.miter_hier`).  Proves the
behavioural Verilog says what the tree says, which is what makes a dump
usable as the source of any other check (the cva6 flow in `test/cva6/` runs
every oracle on dumps).  `test/regressions/run_roundtrip.sh` runs it over
`sysver_tests/` for all three front ends and prints a table.  A FAIL (as
opposed to DIFFER) is a front end that cannot read the source at all
(yosys-slang on `always @(unpacked_array)`).  `RT_TIMEOUT` (default 1800 s)
bounds the whole parse-write-reread-miter run (the yosys read of picorv32
once took longer than that; see the RTLIL importer note in the top-level
README).  The miter needs CaDiCaL on PATH or in
`SVS_CADICAL`: without it the check falls back to Z3 and reports
INCONCLUSIVE on anything with a multiplier.

### `test/cva6/run_cva6_oracles.sh` — the oracles on a real core

Elaborates `$CVA6` (`cv64a6_imafdc_sv39`) with `verilator --json-only`,
converts it with the verilator front end, writes one behavioural Verilog file
per selected specialised module (with its children, `extract.py`), and runs
the roundtrip, `run_qor.sh` (children `auto`) and `gl_cosim.sh` over them.
Outputs under `$OUT` (default `~/svs-gatesim/cva6`): `roundtrip.txt`,
`qor/results.tsv`, `gl_summary.txt`.

### `gl_cosim.sh` — gate-level co-simulation against the unisims

    test/qor/gl_cosim.sh <file.sv> <top> [cycles=5000] [extra verilator args]

Maps `<top>` through the standard pipeline, writes the cell netlist, wraps
its bit-blasted ports back into vectors (`gl/wrap.py`), generates a
testbench (`gl/gen_tb.py`) that drives the source module and the netlist
with the same random inputs (`+verilator+seed+1`, `GL_SEED`) and compares
every output at every negedge, and builds it all with Verilator against
Xilinx's unisim library (Vivado 2020.1 at `XIL_UNISIM_SRC`; three idioms
Verilator rejects are patched into a cached copy, `GL_UNISIMS`).  The clock
starts after glbl's 100 ns GSR so both sides leave reset together.  Prints
`GL-COSIM <top>: MATCH|DIFFER (...)`, the first mismatches, and a CONTEXT
block with every output two cycles before the first one.

- `GL_NET_V=<file.v>` simulates that behavioural Verilog of `<top>` against
  the source instead of mapping: a front end's dump (`svd.write_verilog`)
  against the RTL Verilator reads itself, i.e. the importer oracle.
- `GL_STAGE=bir` simulates the post-memlower BIR as behavioural Verilog
  instead of the LUT netlist: a DIFFER there is a front-end or BIR-pass bug,
  a DIFFER only at the netlist stage is gate_map's.
- `GL_VCD=1` dumps `dump.vcd`; `gl/vcdsig.py` prints signals per cycle,
  `gl/vcdregs.py` reassembles bit-blasted registers from the netlist's FDRE
  instance names.
- `SV_SUITE=<exe>` picks the binary (also honoured by `run_qor.sh`,
  `run_vm.sh`, `dsp_selftest.sh`); `GL_PIPELINE`, `GL_OUT`, `GL_DEFINES`
  (default `-DSYNTHESIS`), `GL_JOBS`, `GL_CFLAGS`.

The testbench's port list, clock and reset come from the source module's
BIR (`svd.ports`, `svd.clocks`, written to `ports.txt` / `clocks.txt`), never
from the netlist or from a regex over emitted text: a netlist that has lost an input (the register it fed was
dropped) has no port for it, and a testbench built from the netlist's ports
tied that input to zero on the RTL side too and compared nothing -- the
`strget_reg` netlist with its `str_byte` register missing passed 5000 cycles
that way.  Ports the netlist lacks are left open on the wrapper.

This is the only check that sees RAMB36E1/DSP48E1 behaviour: the BIR-level
models (`augment_xil_models`, `bmc_sim`, `vm_cosim.sh`) cut those as black
boxes.  `vm_cosim.sh` is kept for the LUT/FF-only comparison it does.
