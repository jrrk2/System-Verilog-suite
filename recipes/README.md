# SVS Lua recipes

Lua scripts driving the `svd.*` bindings exposed in `sv_lua.ml`.
Each recipe replaces one (or many) of the legacy `test_*.exe`
standalone executables now sitting in `old/`.

## Running a recipe

```
./_build/default/sv_suite.exe script recipes/<recipe>.lua
```

Most recipes take their inputs as Lua globals. See the first 15 lines
of each `.lua` for the expected variable names; the simplest pattern is
a thin caller script (`recipes/example_*.lua`) that sets the globals
then `dofile`s the parameterised recipe.

## Entry points exposed in `svd.*`

### Loading

| Lua call                          | OCaml underlying            | Notes |
|---|---|---|
| `svd.parse(fe, top, {files…})`    | `Verible_to_behavioral` etc. | fe ∈ {verible, slang, yosys, verilator, sv-parser, vhdl, surelog} |
| `svd.parse_all(fe, {files…})`     | `Verible_to_behavioral.convert_files_all` | no-top read |
| `svd.parse_v_cells(path)`         | `Ver_front_to_behavioral.convert_v_file` | cell-mapped Verilog |
| `svd.liberty(path)`               | `Sv_liberty.parse_liberty_file` | Liberty library |

### Generic behavioural passes (ASIC and FPGA)

| Call                                  | Underlying                                |
|---|---|
| `svd.unroll(prog)`                    | `Behavioral_unroll.unroll_program`        |
| `svd.inline(prog)`                    | `Behavioral_inline.inline_program`        |
| `svd.iflift(prog)`                    | `Behavioral_iflift.lift_program`          |
| `svd.blocking_subst(prog)`            | `Behavioral_blocking_subst.blocking_subst_program` |
| `svd.meminfer(prog)`                  | `Behavioral_meminfer.infer_program`       |
| `svd.memlower(prog)`                  | `Behavioral_memlower.lower_program`       |
| `svd.ssa(prog)`                       | per-module `Behavioral_ssa.module_to_ssa` |
| `svd.flatten(prog)`                   | `Behavioral_flatten.flatten_program`      |
| `svd.optimize(prog)`                  | `Behavioral_optimize.optimize_full`       |
| `svd.flatten_z3(prog, top)` → *mod*   | `Behavioral_hier.flatten_for_z3`  (drops primitive binstances) |
| `svd.flatten_struct(prog, top)` → *mod* | `Behavioral_hier_struct.flatten_structural` (keeps primitives) |
| `svd.splice(prog, child, src_prog)`   | swap a child bmodule by name              |
| `svd.module_names(prog)`              | comma-separated names                     |

`MEMLOWER_FPGA` is **not** set by `svd.meminfer` — recipes target either
ASIC or FPGA memory mapping by setting/unsetting the env var themselves.

### Analysis (module-level)

| Call                       | Underlying                            |
|---|---|
| `svd.pick(prog, name)` → *mod* | look up by module name |
| `svd.bir(handle)`              | dump textual BIR |
| `svd.insts(mod)`               | list instance names |
| `svd.timing(mod, lim)`         | rough longest-path estimate |
| `svd.register_analyse(mod)`    | `Behavioral_registers.analyze_module` |
| `svd.cdc_analyse(mod)`         | `Cdc_analysis.analyse` + `format_report` |
| `svd.prep_for_z3(mod)` → *mod* | `Behavioral_arch_subst` + flatten if hierarchical |
| `svd.ffrip(mod)` → *mod*       | `Behavioral_ffrip.rip_module` (FF flatten) |

### Equivalence checking

| Call                                          | Underlying        | Result |
|---|---|---|
| `svd.miter(mod_a, mod_b)`                     | `Z3_miter.check_miter_equivalence` | `"EQUIVALENT"` / `"DIFFER"` |
| `svd.gate_miter(top, beh, gate, lib)`         | as above + Liberty expand | same |
| `svd.expand(prog, lib)`                       | `Gate_netlist_to_behavioral.expand_program` | new prog |

### FPGA-specific (Fpga_synth + nextpnr-xilinx)

| Call                                       | Underlying                                | Notes |
|---|---|---|
| `svd.gate_map(mod, k_lut, io)` → *mapped*  | `Behavioral_to_hardcaml.create_circuit` → `Bir_to_aig.lower_circuit` → `Fpga_map.map_lowered` | k_lut typically 6, io=0/1 |
| `svd.write_cellmapped_v(mapped, path)`     | `Hardcaml.Rtl.output Verilog`             | round-trip-able via `svd.parse_v_cells` |
| `svd.write_mapped_json(mapped, path)`      | `Fpga_synth.Fpga_emit.write_yosys_json`   | direct yosys JSON |
| `svd.write_nextpnr_json(mod, path)`        | `Bir_to_nextpnr_json.write_yosys_json`    | for already-structural BIR |

### Emit / round-trip

| Call                                       | Underlying                          |
|---|---|
| `svd.emit_verilog(handle)`                 | `Behavioral_to_verilog.verilog_of_program` (returns string) |
| `svd.emit_vhdl(handle)`                    | `Behavioral_to_vhdl.vhdl_of_program`       |
| `svd.write_verilog(handle, path)`          | write to file                              |
| `svd.write_vhdl(handle, path)`             | write to file                              |
| `svd.convert_hdl(input.{sv,v,vhd}, output.{sv,v,vhd})` | parse + emit in different language |

### Bookkeeping

| Call                  | Notes |
|---|---|
| `svd.name(handle)`    | label of a handle |
| `svd.items()`         | tab-separated list of all live handles |
| `svd.owner(mod)`      | recover the bprogram owning a module handle (lets a recipe go `prog → flatten_z3 → mod → owner → prog` for the next pass-chain) |

## Recipes in this directory

| Recipe                                 | Replaces                                  |
|---|---|
| `wrapped_inner_to_nextpnr.lua`         | `test_telegraph_fpga.ml` and any wrapper+child design |
| `multi_frontend_miter.lua`             | `test_two_frontend_miter`, `test_three_frontend_miter`, `test_four_frontend_miter`, `test_five_frontend_miter`, `test_3way_verify`, `test_3way_suite`, `test_*_vs_*`, plus the fixed-pair miters `test_apb_uart_z3`, `test_simple_z3`, `test_uart_modules_z3`, `test_yosys_rtlil_vs_verilog`, `test_vhdl_via_sv` |
| `cdc_report.lua`                       | `test_cdc.ml` |
| `dump_bir.lua`                         | `dump_picosoc_fsm`, `dump_regs_bir`, `dump_svparser`, `dump_synlig_bir`, `dump_verible_alu`, `test_dump_counter`, `test_dump_three`, `test_dump_four`, `test_unroll_inline`, `test_behavioral_optimization` |
| `picosoc_sonata.lua`                   | `picosoc_build_sonata.sh` (full 4-phase build to UF2) |
| `gate_synth_equiv.lua`                 | `test_synth_equiv.ml` (formal equivalence between source and cell-mapped output, per-module verdict) |
| `ssa_stress_miter.lua`                 | `test_ssa_stress_miter.ml` (yosys round-trip vs direct parse under SSA) |
| `example_uart_vc707.lua`               | concrete driver invoking `wrapped_inner_to_nextpnr.lua` |

## Adding new recipes

A new recipe is a Lua script that calls existing `svd.*` operations.
Only when the operation itself doesn't exist yet (i.e. a passing
through a library function the bindings don't cover) do you edit
`sv_lua.ml`: add a small `l<thing>` wrapper, register it in the
`svd` init block. Both halves are ≈10 lines.

## Remaining standalone executables (active callers)

The following 12 `.ml` files are deliberately kept at the repository root
because at least one external caller — a shell-script regression runner,
`verify_interactive.lua`, or a Python test-suite runner — still calls
the standalone binary by name.  Porting them is now mostly a matter of
updating those callers rather than writing new bindings.

| Standalone | Active callers |
|---|---|
| `test_behavioral_equivalence.ml` | `run_uart_structural_equivalence.sh`, `verify_interactive.lua` |
| `test_behavioral_optimization.ml` | `compare_frontends.sh` |
| `test_behavioral_z3.ml` | `run_uart_z3_equivalence.sh` |
| `test_edif_vhdl_equivalence.ml` | `run_edif_vhdl_equivalence_test.sh` |
| `test_hardcaml_equivalence.ml`, `test_hardcaml_sat.ml` | `verify_interactive.lua` |
| `test_miter_equivalence.ml` | `run_uart_miter_sat.sh`, `verify_interactive.lua` |
| `test_sv_behavioral.ml` | `compare_frontends.sh` |
| `test_verilator_behavioral.ml` | `verify_interactive.lua`, `test/sv_tests/runners/Decompiler_Verilator_Parse.py` |
| `test_verilator_vs_verible.ml` | `test/sv_tests/runners/Decompiler_Miter.py` |
| `test_vhdl_uart.ml` | `run_vhdl_uart_regression.sh`, `verify_interactive.lua` |
| `debug_ports.ml` | (no callers, left at root by oversight) |

Each is logically equivalent to a recipe call (`svd.parse + svd.pick +
svd.miter` for the miter family, `svd.parse + svd.optimize` for the
optimisation family, etc.), so the port for each is short.  The blocker
is converting the caller scripts.

## Future recipes blocked on new bindings

These standalones in the root tree exercise libraries that are **not yet
exposed in `svd.*`**.  Each is a small bindings addition rather than a
recipe-only port:

| Standalone | Needs binding for |
|---|---|
| `test_cell_delay_probe`, `test_fanout_cone`, `test_placement_timing` | `Cell_delay`, `Fanout_cone`, `Hpwl`, `Lef_pins` |
| `test_synth_mac`, `test_gate_verilog_liberty` | `Synth_mac`, `Gate_verilog` |
| `test_atpg` | `Fault_sim`, `Synth_pipeline.run` |
| `test_eco_emit`, `test_predict_swap`, `test_post_swap_check` | `Eco_emit`, `Bir_def_bind` |
| `test_floorplan` | placement-result helpers in `Behavioral_floorplan` (if present) |
| `test_picosoc_hcsim` | `Hardcaml.Cyclesim` lifted to a `svd.sim*` family |
| `test_gui_sim` | `Gui_sim` (already wired to `gui.*`, just not from headless `script`) |

Their existing OCaml entry points are stable; once the corresponding
`svd.*` shim is added to `sv_lua.ml`, the standalone joins `old/` and
its job is taken over by an equivalent recipe in this directory.

## Why this organisation

The old pattern of one `test_*.ml` standalone per design or per
pipeline variant duplicated the same first ~50 lines (parse, prep,
optimise) and put the interesting bit at the bottom of yet another
compiled executable. Recipes invert that: the pipeline composition
is in plain text, in one place, and the OCaml binary is shared.
Discoverability of "promising entry points" is now this README plus
`recipes/`. See `old/` for the historical executables.
