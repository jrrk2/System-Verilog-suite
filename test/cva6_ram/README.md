# cva6 RAM Equivalence Tests

Equivalence-check cva6's RAM modules through the same harness as
`test/edif_compare/`, with a focus on the `make fpga` flow's RAM
inference behaviour.

## Why a separate directory

cva6 RAM modules use parameters (depth, width, byte-enable presence,
output-register count) that change Vivado's inference outcome. A
single fixed test won't exercise the full surface — we want one
concrete-parameter wrapper per inference shape we care about. Keeping
those alongside the `cva6` checkout makes the source paths sane via
`<top>.srcs`.

## Tests so far

| Top | Source | Inference outcome |
|---|---|---|
| `syncspram_small` | `SyncSpRam` 16×8 (no byte enable, no output reg) | 1 `RTL_RAM` (distributed) + 1 `RTL_AND` + 2 `RTL_MUX` + 8 `RTL_REG` w/ CE |
| `fifo_v3_small` | `fifo_v3` 8 entries × 16 bits (pulp-platform) | 138 `RTL_REG_ASYNC` (bit-blasted register-array inference) + 27 `RTL_MUX` + 8 `RTL_ROM` + comparators/adders |

`fifo_v3` is widely used inside cva6 (frontend instr_queue, store_buffer,
load_unit, scoreboard, etc.). It exercises *register-array* inference
rather than RAM inference because of how the read path is structured —
combinational reads from `mem_q[read_pointer]` block BRAM/LUTRAM
inference, so Vivado emits one register per bit. Comparing the two
tests shows the spectrum of memory-flavour outcomes Vivado can pick.

`fifo_v3_small` cell-count diagnostic (xilinx_rtl backend):

| family | Vivado | sv_main | delta |
|---|---|---|---|
| RTL_ADD | 3 | 3 | = |
| RTL_AND | 5 | 5 | = |
| RTL_EQ | 2 | 2 | = |
| RTL_SUB | 1 | 1 | = |
| RTL_INV | 2 | 4 | partial |
| RTL_MUX | 27 | 7 | partial |
| RTL_REG_ASYNC | 138 | 0 | only in Vivado (memory + control regs) |
| RTL_ROM | 8 | 0 | only in Vivado (constant lookups) |
| RTL_LSHIFT | 0 | 3 | only in sv_main (probably barrel-shifter pattern collapses differently) |

## Planned tests (vendor/pulp-platform/fpga-support/rtl/)

Each of these will exercise a different RAM-inference outcome. Vivado's
choice of LUTRAM / BRAM / register-file depends on geometry, port
count, byte-enable presence, and read latency.

| Source module | Notes |
|---|---|
| `SyncSpRam` | Single-port, no byte enable. (small instance covered above) |
| `SyncSpRamBeNx64` / `SyncSpRamBeNx32` | Byte-enabled single-port — exposes byte-write inference (likely RTL_RAM with vector WE) |
| `SyncDpRam`, `SyncDpRam_ind_r_w` | Dual-port — should infer dual-port BRAM at scale |
| `SyncTpRam` / `SyncThreePortRam` | Triple-port — typically falls back to multi-bank LUTRAM |
| `AsyncDpRam`, `AsyncThreePortRam` | Asynchronous read — pure LUTRAM |
| `BramLogger`, `AxiBramLogger`, `BramDwc`, `TdpBramArray` | Higher-level wrappers — exercise structural composition |

`syncspram_small` is the first concrete test — picked deliberately
small (128 bits total) so it elaborates fast and exercises the
distributed-RAM (LUTRAM-style) inference path. Larger word counts will
trigger BRAM inference (`RAMB36E1` / `RAMB18E1`) under full synthesis.

## What works today

* Vivado RTL elaboration (`synth_design -rtl`) runs cleanly on cva6
  RAM modules.
* `vhdl_to_ver_front` parses the resulting VHDL — RAMs come out with
  proper STD_LOGIC_VECTOR widths in the entity, and component
  declarations expose the cell port shapes (RTL_RAM, RTL_REG__BREG_1,
  etc).
* The miter framework runs to completion; cell-count diagnostic shows
  exactly which families need work.
* cva6's own FPGA build (`corev_apu/fpga/scripts/run.tcl`) runs
  `synth_design -rtl -name rtl_1` itself, so our infrastructure is
  directly applicable to the same step it uses.

## What needs work (in order of impact)

1. **`RTL_RAM` cell mapping** (next concrete step). New primitive shape:
   ```
   RTL_RAM
     RA1[N-1:0] : in   (read address)
     RO1[W-1:0] : out  (read data, async — combinational read of mem[RA1])
     WA2[N-1:0] : in   (write address)
     WD2[W-1:0] : in   (write data)
     WE2        : in   (write enable)
     WCLK       : in   (write clock)
   ```
   Semantics: `RO1 = mem[RA1]` (async); `if (WE2) mem[WA2] <= WD2` on
   posedge WCLK. Encoding requires either:
   - An array-typed signal in BIR (whose BAssign has an indexed lhs), or
   - A bit-blasted form: a 2^N-element array of registers and a mux
     for the read.
   The first form is more concise and Z3-friendly via `Array(BV, BV)`.
2. **`RTL_REG` with CE** (small extension): when CE = 0 the register
   holds, else it loads D. cell_to_bprocess's `mk_register` ignores
   the CE pin today.
3. **Byte-enable RAMs** (`SyncSpRamBeNx64`, `SyncSpRamBeNx32`): same
   shape as `RTL_RAM` but with per-byte WE strobes — likely a
   different cell variant (`RTL_RAM_BWE` or just `RTL_RAM` with a
   wider WE2).
4. **Post-synthesis (full `synth_design`) path**: the FPGA flow runs
   full synthesis after `-rtl` and that's where actual BRAM/LUTRAM
   inference happens. The post-synth `write_vhdl` would emit
   technology-mapped cells (`RAMB36E1`, `LUT*`, `FDRE` etc.) with
   their behavioural models in `/NFS/apps/Xilinx/Vivado/2020.1/data/verilog/src/unisims/`.
   This is a separate and bigger pipeline to add.

## Whole cva6 core elaborates ✅

`elab_cva6.tcl` is a modified copy of cva6's
`corev_apu/fpga/scripts/run.tcl` that:

* Reuses cva6's `add_sources.tcl` (so all packages, types, and
  `localparam type` definitions flow through naturally).
* Sets the same `include_dirs` cva6's run.tcl uses (this was the
  missing piece — without them, `\`include` directives don't resolve).
* Reads the genesys2 board SVH as a global include.
* Drops only the rv_tracer's duplicate `lzc.sv` (its compile-order bug
  references `cf_math_pkg` before that package loads; the rest of
  rv_tracer is needed because `ariane_xilinx` references its packages).
* Stops after `synth_design -rtl` and writes `.edf`/`.v`/`.vhd` —
  before `launch_runs synth_1` (which needs Xilinx IPs we don't have).

Running it with `TOP=cva6` elaborates the **whole cva6 core** in
~5 minutes:

```bash
TOP=cva6 OUT_BASE=cva6_elab \
  vivado -mode batch -source elab_cva6.tcl
```

Output sizes for the genesys2 default config:

| File | Size | Note |
|---|---|---|
| `cva6_elab.edf` | 113 MB | Vivado RTL elaboration EDIF |
| `cva6_elab.v` | 33 MB | write_verilog netlist |
| `cva6_elab.vhd` | 52 MB | write_vhdl (preferred for our pipeline) |

The elaborated design has **144 entities** and exercises **31 unique
RTL_* primitive families** (top by count): `RTL_REG_ASYNC` 29 791,
`RTL_MUX` 14 795, `RTL_AND` 2 406, `RTL_XOR` 2 105, `RTL_OR` 960,
`RTL_ROM` 694, `RTL_EQ` 467, `RTL_INV` 374, `RTL_REDUCTION_OR` 277,
`RTL_LSHIFT` 212, `RTL_ADD` 167, `RTL_REDUCTION_AND` 150, ...

The **instruction cache** is a sub-hierarchy of this — `cva6_icache` is
referenced 374 times within the elaborated design (once for the
`i_cva6_icache` instance plus per-bit register-array entries). The
miter pipeline can target this sub-hierarchy by extracting the
`cva6_icache` entity from `cva6_elab.vhd`.

## Whole-cva6 sweep through the converter (this round)

`test_cva6_sweep.exe cva6_elab.vhd` runs `Vhdl_to_ver_front.convert_vhd_file`
across all 144 entities and reports per-module signal/process counts.
Used to drive iteration: `MITER_STRICT=1` bombs on the first
unrecognised pattern, we add a converter case, repeat.

Gaps surfaced and closed in this round:

* **Escaped-name parity** in `collect_entity` — VHDL escaped identifiers
  `\foo__parameterizedN\` weren't unescaped on the entity-decl side, so
  55 of 144 entities failed the entity↔architecture join. One
  `unescape` call brings the converter to **144/144** modules.
* **`Double (Vhdwaveform_element, e)`** in `actual_to_vparser` — Vivado
  emits these for default-driven concurrent assignments. Recurse.
* **`RTL_BSEL`** (dynamic bit-select): exact handler `O = (I >> S) & 1`.
* **`RTL_RAM`** (distributed-RAM): approximate handler
  `RO1 = WD2 XOR RA1` — strict-mode passes; tests exercising the RAM
  honestly counter-example.

After all four fixes, `MITER_STRICT=1` runs to completion across every
entity in `cva6_elab.vhd`.

## cva6 leaf modules through the formal miter

These cva6 sub-modules now pass `compare.sh <top>` end-to-end (formal
equivalence):

| Module | Source | Notes |
|---|---|---|
| `gated_clk_cell` | `core/cvfpu/vendor/openc910/.../gated_clk_cell.v` | trivial pass-through |
| `exp_backoff` | `vendor/pulp-platform/common_cells/src/exp_backoff.sv` | LFSR backoff timer |
| `ct_vfdsu_ff1` | `core/cvfpu/vendor/openc910/.../ct_vfdsu_ff1.v` | priority encoder |
| `ct_vfdsu_pack` | `core/cvfpu/vendor/openc910/.../ct_vfdsu_pack.v` | sign/exp packer |
| `rstgen_bypass` | `vendor/pulp-platform/common_cells/src/rstgen_bypass.sv` | reset synchroniser bypass |
| `clk_div` | `vendor/pulp-platform/common_cells/src/clk_div.sv` | clock divider |
| `mv_filter` | `vendor/pulp-platform/common_cells/src/mv_filter.sv` | majority-vote filter |
| `lfsr_8bit`, `lfsr_16bit` | `vendor/pulp-platform/common_cells/src/lfsr_*.sv` | LFSRs |
| `binary_to_gray` | `vendor/pulp-platform/common_cells/src/binary_to_gray.sv` | unblocked by VhdBlockSignalDeclaration extraction + per-bit pin grouping |
| `delta_counter`, `shift_reg` | `vendor/pulp-platform/common_cells/src/*.sv` | unblocked by generate-block flattening |
| `stream_filter`, `stream_mux` | `vendor/pulp-platform/common_cells/src/stream_*.sv` | flat stream-handshake glue |
| `sync` | `vendor/pulp-platform/common_cells/src/sync.sv` | parameterized synchroniser chain |
| `onehot_to_bin` | `vendor/pulp-platform/common_cells/src/onehot_to_bin.sv` | unblocked by Sel-LHS recovery in `assignw_processes` |

Round-3 converter additions (this session) that unlocked the new
passes:

* **`Double (VhdBlockSignalDeclaration, ...)`** in `extract_arch_signals`
  — Vivado emits this wrapper (rather than `VhdSignalDeclaration`) for
  any architecture with attribute specifications. Without this, internal
  signals (e.g. `Z0` in binary_to_gray) silently lost their declared
  width and got inferred as 64-bit ints, breaking shift/xor encodings.
* **Per-bit port-map grouping** in `extract_arch_instances` — when a
  port appears with subscripted formals (`I0(2) => A(0); I0(1) => A(1);
  I0(0) => A(2)`), gather the bits, detect whole-vector mappings to a
  single base signal, and emit one pin with the bare ID. Falls back to
  Concat-of-bit-selects for non-canonical mappings.
* **Flatten `Begin` generate blocks at module level** in
  `verilator_to_behavioral.module_to_bmodule` — Verilator places
  genvar-for / conditional-generate children inside nested `Begin`
  nodes, which the always/assign extractors weren't descending into.
  Recursive flatten unblocks `delta_counter`, `shift_reg`, `binary_to_gray`,
  and partial improvement on lzc.
* **Sel/ArraySel LHS recovery in `assignw_processes`** — bit-indexed
  LHSs like `tmp_mask[i] = ...` were being silently dropped because the
  filter only looked for bare `VarRef`. Walk through Sel/ArraySel to
  recover the base name (lossy: assigns the whole vector). Unblocks
  `onehot_to_bin` and other doubly-nested-genvar modules whose generate
  bodies write to indexed locals.

Modules that fail the formal miter (real gaps, not converter bombs):

| Module | Reason |
|---|---|
| `popcount` | recursive self-instantiation — miter only encodes the top |
| `lzc`, `cc_onehot` | tree-style generate triggers `Z3.Error("invalid extract application")` — internal `sel_nodes`/`index_nodes` arrays land as 1-bit, breaking slice encoding. Needs array-typed signal lifting. |
| `gray_to_binary`, `stream_demux` | Vivado emits empty / partial netlist (parameter `N=-1` default) |

## Cross-suite progress (this session)

Tracked alongside `test/picorv32/` — adding picorv32 as a regression
target uncovered general width-handling bugs in `z3_miter` whose fixes
benefit *every* suite. Notable side-effects:

* `apb_uart` (in `test/edif_compare/`) went from ⚠ converter-limited to
  ✅ FORMALLY EQUIVALENT — the `bvand` 32-vs-64 width sort error is now
  handled by zero-extending operands in `BBinOp`.
* `ashr4` (arithmetic right shift) likewise now passes.
* `picorv32` (the whole RISC-V core) passes formal equivalence end-to-
  end. Verdict is *partial* — Z3 has 609 constraints from the Vivado
  side vs 320 from the orig SV (verilator_to_behavioral coverage gap).

The two remaining cva6_ram tests (`syncspram_small`, `fifo_v3_small`)
still fail at formal level because of `RTL_RAM` (cva6's actual SRAM
modules) and bit-blasted register-array reads (the `mem_q` array in
`fifo_v3`). These are the same array-aware-BIR gap that affects
`concat2`/`bitsel4`/`picorv32_regs`.

## Primitive families seen in cva6 vs what our converter handles

Currently in `ver_front_to_behavioral.cell_to_bprocess`:

| family | handled? |
|---|---|
| RTL_INV, AND, OR, XOR | ✅ |
| RTL_ADD, SUB, MUL | ✅ |
| RTL_LSHIFT, RSHIFT | ✅ |
| RTL_EQ, NEQ, LT, LEQ, GT, GEQ | ✅ |
| RTL_MUX | ✅ |
| RTL_REG, RTL_REG_SYNC, RTL_REG_ASYNC | ✅ (no CE pin) |

Missing — **need work for cva6 equivalence**:

| family | what it does |
|---|---|
| `RTL_RAM` | distributed-RAM inference — async read, sync write. The single most impactful gap. |
| `RTL_REG` w/ CE | clock-enable variant of the register cell |
| `RTL_ROM` | constant-lookup table |
| `RTL_REDUCTION_AND/OR/NAND/NOR` | reduction operators (`&a`, `|a`, `~&a`, `~|a`) |
| `RTL_BMERGE`, `RTL_BSEL` | bit merge (concat) / bit select |
| `RTL_ALSHIFT`, `RTL_ARSHIFT` | arithmetic shifts (sign-aware) |
| `RTL_XNOR` | xnor |
| `RTL_LATCH` | latch (rare in cva6 but present) |
| `RTL_MINUS` | unary negation (different from `RTL_SUB`) |
| `RTL_MULT` | multiplier (we have `RTL_MUL` mapping; check Vivado's actual name) |

## Things that don't work yet

* **`ariane_xilinx` (the actual fpga top)** — `ariane_peripherals_xilinx.sv`
  uses **virtual interfaces in continuous assignments**, not supported by
  Vivado 2020.1 (`virtual interface in continuous assignment not
  supported`). The cva6 trunk targets Vivado 2022+; this is the local
  tool version. The `cva6` core elaborates fine — that's where the
  icache lives anyway.

* **`cva6_icache` standalone** — uses `parameter type` for its request
  structures, with no working defaults. Either elaborate the whole core
  (works, see above) and extract the icache sub-hierarchy, or write a
  type-binding wrapper.

## Running

```bash
# RAM modules (these elaborate cleanly today)
bash compare.sh syncspram_small xilinx_rtl
bash compare.sh fifo_v3_small  xilinx_rtl

# cva6 sub-module via cva6's own project (when usable)
vivado -mode batch -source elab_cva6.tcl -tclargs <top> <out_basename>
```

Same options as `test/edif_compare/compare.sh`. Logs land in this
directory.
