# SweRV regression — RAM-variant identification + Z3 oracle sweep

This regression sweeps the SweRV-EH1 design (from `synlig`'s test corpus
at `/home/jonathan/synlig/third_party/surelog/third_party/tests/CoresSweRV`)
through two passes:

**Pass 1 — RAM identification** via `cva6_ram_scan.exe` enumerates every
`bmem` the meminfer pass recognises across `design/lib`, `design/ifu`,
`design/lsu`, and the top `design/` directory.  The `dec/dbg/dmi/exu`
subdirs are also scanned (correctness only — those four declare no
inferred memories).  The single richest file is `design/lib/mem_lib.sv`
— it declares 41 SRAM macro modules (`ram_<depth>x<width>`) which all
classify as `single_port_bram`.

**Pass 2 — Z3 oracle parallel-correctness sweep** on the RAM macros.
Each `ram_<D>x<W>` is fed independently through verilator and verible,
and `Z3_miter` checks the resulting BIRs.  This is a tight feedback
loop on the bit-level FF-shape encoding because the macros are tiny
(~20 lines) but exercise registered output + bit-select read + sync
write — the FF-rip / blocking-subst pipeline's load-bearing case.

To run:

```
./test/swerv/run_swerv_rams.sh
```

The `flist` file lists the include paths and prerequisite source files
in the order verilator needs them. Edit if the synlig corpus moves.

Pass 2's pre-flatten step runs `verilator -E` over the prereq context
plus `mem_lib.sv` once, then iterates the per-macro Z3 miter via
`test_yosys_oracle_sweep.exe --oracle verilator --peer verible
--top ram_<D>x<W>` against the flat output.  Verdicts are tallied as
EQUIV / NOTEQUIV / LOADFAIL / Z3ERR.
