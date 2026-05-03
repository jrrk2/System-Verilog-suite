# Converter regression suite

Each `.sv` here is a minimal stress-test for one specific converter
fix made during the UART-hierarchy bring-up. A successful run via
`run_all.sh` proves all the listed fixes are still in place.

| File                          | What it stresses                                                                 | Miter side         |
|-------------------------------|----------------------------------------------------------------------------------|--------------------|
| `param_default.sv`            | Header `#(parameter W=4)` substitution into port widths                          | Verible            |
| `param_body.sv`               | Body `parameter W = 4;` substitution into expressions                            | Verible            |
| `clog2_width.sv`              | `[$clog2(N)-1:0]` width inference (system-tf-call as plain identifier)           | Verible            |
| `unsigned_pass.sv`            | `$unsigned({…})` passthrough in width and value paths                            | Verible            |
| `enum_width.sv`               | `typedef enum logic [N:0] {…} t;` resolves to N+1-bit width                      | Verilator + Verible |
| `multi_reg_decl.sv`           | `reg [W-1:0] a, b;` — second var (`gate_instance_…1`) keeps the decl's width     | Verible            |
| `bit_select.sv`               | `a[1]` bit-select reads on a packed reg → BSlice                                 | Verible            |
| `concat.sv`                   | `{a, b}` concatenation → BConcat                                                 | Verible            |
| `unary_ops.sv`                | `~`, `-`, `&`, `\|`, `^` unary operators dispatched correctly                    | Verible            |
| `ternary.sv`                  | `s ? a : b` cond_expr2 (TUPLE6, not TUPLE5)                                      | Verible            |
| `logand_logor.sv`             | `&&` / `\|\|` (logand_expr / logor_expr)                                         | Verible            |
| `level_always_comb.sv`        | `always @(level signals)` is combinational, not BSequential                      | Verilator + Verible |
| `default_with_refinement.sv`  | Combinational always with default + conditional override                         | Verible            |
| `case_full.sv`                | Case with default — all branches assign → combinational                          | Verible            |
| `seq_default_refine.sv`       | Sequential always: TLIST reversal so default precedes refinement                 | Verible            |
| `output_port_ff.sv`           | Output-port FF gets pass-thru rip (Q__Q input + Q output + Q__D output)          | FF-rip             |
| `dffe_basic.sv`               | Yosys `$dffe` (clock-enable FF)                                                  | RTLIL              |
| `adffe_basic.sv`              | Yosys `$adffe` (async reset + clock enable)                                      | RTLIL              |
| `sdff_basic.sv`               | Yosys `$sdff` (sync reset)                                                       | RTLIL              |
| `pre_clr_async.sv`            | Async preset/clear (ver_front mk_register PRE/CLR pins)                          | ver_front          |
| `port_dim_param.sv`           | `[WIDTH-1:0] q` — WIDTH is a param, not a port                                   | Verible            |
| `bit_select_lhs.sv`           | LHS bit-select `a[i] <= …` in always block                                       | Verible            |
| `logical_not_signed_eq.sv`    | Yosys `$logic_and`/`$logic_or` reductions                                         | RTLIL              |

## Running

```
bash run_all.sh                # software-only (Verilator↔Verible + Yosys↔Verible)
bash run_all.sh --vivado       # also include Vivado EDIF↔SV miter (needs Xilinx)
```
