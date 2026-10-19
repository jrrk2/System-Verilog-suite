# test/cva6_litex — LiteX CVA6 SoC: SVS synthesis, Vivado place and route

The VC707 LiteX SoC from `~/xc7-bitstream-tools/examples/vc707-litex`
(`vc707_litex.py`), with CVA6 as the CPU, in its 1 GB DDR3 + ethmin
configuration (`--with-ddr --with-ddr64 --with-ethmin-phy --with-led-chaser`),
synthesised by this suite and handed to Vivado 2020.1 as EDIF.

```
test/cva6_litex/run.sh            # gen vlt svs vivado
test/cva6_litex/run.sh svs vivado # re-run from the synthesis
```

| stage | does | output (under `$OUT`, default `~/svs-gatesim/cva6-litex`) |
| --- | --- | --- |
| `gen` | `gen_soc.py` → `vc707_litex.py --cpu-type cva6 ... --build --no-compile-gateware` | `soc/gateware/xilinx_vc707.{v,tcl,xdc}`, `*_rom.init` (BIOS) |
| `vlt` | `verilator --json-only` over LiteX's own source list | `vlt/soc.json` |
| `svs` | verilator front end → unroll/inline/iflift/blocking_subst/meminfer/memlower/srl_infer/dsp_infer → `gate_map` per module (top with I/O buffers) → splice → `write_hier_edif` | `svs/xilinx_vc707.edif`, `svs/synth.log` |
| `vivado` | `pnr.tcl`: `read_edif` + LiteX's xdc → link → opt → place → phys_opt → route → reports → bitstream | `vivado/*.rpt`, `vivado/xilinx_vc707.bit`, `vivado/vivado.log` (`PNR ...` lines) |

## Status (2026-10-19): paused at the svs stage, on memory

`gen` and `vlt` work.  `svs` mapped 16 modules, then gate_map of
`rr_arb_tree__N5_Tz47_Az83_Lz83` (CVA6's AXI crossbar arbiter) passed
`SVS_MAX_MEM_MB=20000` (23.8 GB RSS, ~17 min in).  The `vivado` stage has not
run.  The stack at the abort is deep recursion in
`Behavioral_to_hardcaml.expr_to_signal`, ending in `uresize`/`concat_msb`.
That function has no memoisation, so the suspicion -- not yet confirmed -- is
that the IR shares subexpressions (successive versions of the arbiter's
`data_nodes`, each built from slices of the last and threaded by
blocking_subst) and the conversion rebuilds each shared reference as a tree,
exponentially.

Next step: `test/cva6_litex/repro_rr_arb_tree.sh` maps the same arbiter
standalone at data widths 8/32/128 and prints time and peak memory.  Confirm
the growth there, memoise `expr_to_signal` (keyed on the expression AND the
context's current bindings -- they change between statements of a process),
and re-run the repro before the whole SoC.

## Why the CVA6 port comes from upstream LiteX

`litex-deps/litex` (the jrrk2 fork) carries an older `cpu/cva6`: its
`core.py` asks for `core/Flist.cv64a6_imafdc_sv39`, removed from CVA6 in #1012,
and its wrapper manifest for `corev_apu/src/axi_riscv_atomics`, vendored in
#3183 and removed again since -- no `pythondata-cpu-cva6` commit has both.
Upstream's port (LiteX `b78494107`, `TARGET_CFGS` + `core/Flist.cva6`, the
`cv64a6_imac_sv39_litex` config) matches `pythondata-cpu-cva6` HEAD.
`gen_soc.py` registers that class as `cva6` -- the hook `vc707_litex.py` uses
for BOOM -- and runs the board script unchanged.  `LITEX_CVA6_CORE` points at
it (default `~/trixium-litex/litex/litex/soc/cores/cpu/cva6`).
`pythondata-cpu-cva6` is cloned into `litex-deps/` and found via `PYTHONPATH`;
nothing is installed into the venv.

## Front end

The SoC goes through the **verilator** front end: Verilator elaborates all 275
sources in ~4 s, and the import takes ~6 min / 0.7 GB.  Verilator needs a
declaration for every Xilinx primitive, so Vivado's `unisim_comp.v` is put in
front; `drop_xil_prims` removes those definitions again and carries the
specialised parameters back onto the instances, so the EDIF names the real
primitives (MMCME2_ADV, OSERDESE2/ISERDESE2/IDELAYE2 for the DDR3 PHY,
GTXE2_CHANNEL for ethmin).  `MEM_INIT_DIR` points at `soc/gateware` for the
BIOS ROM / SRAM / ident images.

Found on the way: the verilator importer attached `$readmemh` images by array
NAME across all modules, so the LiteX top's `mem` image landed in every CVA6
module with a `mem` array (`ariane_regfile_fpga`, `AsyncDpRam`,
`AsyncThreePortRam`).  `readmem_pairs` now records the enclosing MODULE.

Fixed for this design:

- `abc_cover`: a module with no logic of its own (`axi_xbar_intf`, which only
  re-packs an interface into structs) gave ABC an empty AIG, and ABC aborts on
  that (`Abc_NtkCheckNames` assertion, rc 134).  Empty graphs skip ABC now.
- `behavioral_unroll`: three loop shapes CVA6 uses were left rolled, which
  gate_map rejects ("un-unrolled BWhile/BFor reached create_circuit"):
  a `BFor` whose update verilator left EMPTY with the step at the end of the
  body (common_cells `addr_decode`); an inner `for (k = level; ...)` whose start
  is only constant after the outer loop is unrolled (`cva6_shared_tlb`, 12
  loops); and `i = 0; begin while ... end`, the remains of a JUMPBLOCK
  (`store_buffer`).  `svd.check_ir(prog, "unrolled")` after `svd.unroll` lists
  any left (only the unused `is_inside_*_regions` config functions remain).

Known, not fixed: verilator lowers `break`/`continue`/`return` to a JUMPGO;
the importer drops those inside always blocks, so the loop runs every
iteration.  That is right for `store_buffer`'s two `matches = 1; break;`
loops and wrong in general, so the importer now says so per module
(`... break/continue/return in always blocks DROPPED`).

`ariane_regfile_fpga`'s `initial` block (`mem = $random()`, "to suppress
assert warnings on Questa") is reported by check_ir and dropped; Vivado also
ignores it, so both leave the register file uninitialised.

## Knobs

`SOC_ARGS`, `CVA6_VARIANT` (`standard`; `full` = rv64gc), `XC7`, `SV_SUITE`,
`SVS_MAX_MEM_MB` (24000), `VIVADO`, `VIVADO_JOBS`, `UNISIM_COMP`.
