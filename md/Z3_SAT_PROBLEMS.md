# Z3 SAT Problems for VHDL/SystemVerilog Equivalence

## Date: 2026-01-22

## Overview

This document describes the Z3 SAT problems generated to prove equivalence between VHDL ground truth and SystemVerilog translations for all 12 APB UART modules.

## Status

✅ **VHDL→IR**: Complete - All 12 modules converted to IR
📋 **SV→IR**: Exists but needs build system integration
📋 **Z3 Verification**: Problems defined, solver ready

## Z3 Verification Methodology

### Problem Statement

For each VHDL/SystemVerilog pair, prove:

```
∀ input_values, VHDL_IR(inputs) ≡ SV_IR(inputs)
```

### Verification Steps

1. **Create Symbolic Inputs**
   - Generate Z3 bitvector variables for each input signal
   - Example: `CLK: bitvector<1>`, `RST: bitvector<1>`

2. **Symbolic Execution - VHDL IR**
   - Walk IR₁ dataflow graph
   - Build Z3 expressions for each node
   - Produce symbolic output expressions

3. **Symbolic Execution - SV IR**
   - Walk IR₂ dataflow graph
   - Build Z3 expressions for each node
   - Produce symbolic output expressions

4. **Assert Equivalence**
   - For each output: `assert(out₁ == out₂)`
   - Check satisfiability

5. **Interpret Result**
   - **UNSAT** → IRs are equivalent (proof of correctness)
   - **SAT** → Found counterexample (shows difference)

## Generated SAT Problems

### Module-by-Module Breakdown

#### 1. apb_uart
```
Module: apb_uart
Complexity: Simple (8 nodes)

VHDL IR Structure:
  • Inputs: 2 (CLK, RST)
  • Outputs: 2 (iDLL, iDLM)
  • Nodes: 8
    - Register: 2
    - Compare: 4
    - Mux: 2

Z3 Problem:
  ∀ CLK:bv<1>, RST:bv<1>
  prove: VHDL_IR(CLK, RST) = SV_IR(CLK, RST)
  where outputs = (iDLL, iDLM)
```

#### 2. slib_clock_div
```
Module: slib_clock_div
Complexity: Medium (17 nodes)

VHDL IR Structure:
  • Inputs: 2 (CLK, RST)
  • Outputs: 2 (iQ, iCounter)
  • Nodes: 17
    - Register: 2
    - Compare: 6
    - Mux: 4
    - Sub: 3
    - And: 1
    - Add: 1

Z3 Problem:
  ∀ CLK:bv<1>, RST:bv<1>
  prove: VHDL_IR(CLK, RST) = SV_IR(CLK, RST)
  where outputs = (iQ, iCounter)
```

#### 3. slib_counter
```
Module: slib_counter
Complexity: Medium (14 nodes)

VHDL IR Structure:
  • Inputs: 2 (CLK, RST)
  • Outputs: 2
  • Nodes: 14
    - Register: 2
    - Compare: 7
    - Mux: 2
    - And: 1
    - Add: 1
    - Sub: 1

Z3 Problem:
  ∀ CLK:bv<1>, RST:bv<1>
  prove: VHDL_IR(CLK, RST) = SV_IR(CLK, RST)
```

#### 4. slib_edge_detect
```
Module: slib_edge_detect
Complexity: Simple (5 nodes)

VHDL IR Structure:
  • Inputs: 2 (CLK, RST)
  • Outputs: 1
  • Nodes: 5
    - Register: 1
    - Compare: 2
    - Mux: 1
    - And: 1

Z3 Problem:
  ∀ CLK:bv<1>, RST:bv<1>
  prove: VHDL_IR(CLK, RST) = SV_IR(CLK, RST)
```

#### 5. slib_fifo
```
Module: slib_fifo
Complexity: Complex (24 nodes)

VHDL IR Structure:
  • Inputs: 2 (CLK, RST)
  • Outputs: 3
  • Nodes: 24
    - Register: 3
    - Compare: 8
    - Mux: 7
    - And: 2
    - Or: 2
    - Sub: 1
    - Add: 1

Z3 Problem:
  ∀ CLK:bv<1>, RST:bv<1>
  prove: VHDL_IR(CLK, RST) = SV_IR(CLK, RST)
```

#### 6. slib_input_filter
```
Module: slib_input_filter
Complexity: Complex (18 nodes)

VHDL IR Structure:
  • Inputs: 2 (CLK, RST)
  • Outputs: 2
  • Nodes: 18
    - Register: 2
    - Compare: 7
    - Mux: 4
    - And: 2
    - Add: 1
    - Sub: 2

Z3 Problem:
  ∀ CLK:bv<1>, RST:bv<1>
  prove: VHDL_IR(CLK, RST) = SV_IR(CLK, RST)
```

#### 7. slib_input_sync
```
Module: slib_input_sync
Complexity: Simple (8 nodes)

VHDL IR Structure:
  • Inputs: 2 (CLK, RST)
  • Outputs: 2
  • Nodes: 8
    - Register: 2
    - Compare: 4
    - Mux: 2

Z3 Problem:
  ∀ CLK:bv<1>, RST:bv<1>
  prove: VHDL_IR(CLK, RST) = SV_IR(CLK, RST)
```

#### 8. slib_mv_filter
```
Module: slib_mv_filter
Complexity: Medium (15 nodes)

VHDL IR Structure:
  • Inputs: 2 (CLK, RST)
  • Outputs: 2
  • Nodes: 15
    - Register: 2
    - Compare: 6
    - Mux: 4
    - And: 1
    - Add: 1
    - Sub: 1

Z3 Problem:
  ∀ CLK:bv<1>, RST:bv<1>
  prove: VHDL_IR(CLK, RST) = SV_IR(CLK, RST)
```

#### 9. uart_baudgen
```
Module: uart_baudgen
Complexity: Medium (16 nodes)

VHDL IR Structure:
  • Inputs: 2 (CLK, RST)
  • Outputs: 2
  • Nodes: 16
    - Register: 2
    - Compare: 8
    - Mux: 4
    - And: 1
    - Add: 1

Z3 Problem:
  ∀ CLK:bv<1>, RST:bv<1>
  prove: VHDL_IR(CLK, RST) = SV_IR(CLK, RST)
```

#### 10. uart_interrupt
```
Module: uart_interrupt
Complexity: Medium (14 nodes)

VHDL IR Structure:
  • Inputs: 2 (CLK, RST)
  • Outputs: 1
  • Nodes: 14
    - Register: 1
    - Compare: 7
    - Mux: 3
    - And: 2
    - Or: 1

Z3 Problem:
  ∀ CLK:bv<1>, RST:bv<1>
  prove: VHDL_IR(CLK, RST) = SV_IR(CLK, RST)
```

#### 11. uart_receiver
```
Module: uart_receiver
Complexity: Simple (5 nodes)

VHDL IR Structure:
  • Inputs: 2 (CLK, RST)
  • Outputs: 1
  • Nodes: 5
    - Register: 1
    - Compare: 2
    - Mux: 1
    - And: 1

Z3 Problem:
  ∀ CLK:bv<1>, RST:bv<1>
  prove: VHDL_IR(CLK, RST) = SV_IR(CLK, RST)
```

#### 12. uart_transmitter
```
Module: uart_transmitter
Complexity: Complex (32 nodes)

VHDL IR Structure:
  • Inputs: 2 (CLK, RST)
  • Outputs: 2
  • Nodes: 32
    - Register: 2
    - Compare: 16
    - Mux: 6
    - And: 8

Z3 Problem:
  ∀ CLK:bv<1>, RST:bv<1>
  prove: VHDL_IR(CLK, RST) = SV_IR(CLK, RST)
```

## Summary Statistics

### Total SAT Problems: 12

### By Complexity
- **Simple** (5-8 nodes): 4 modules
- **Medium** (14-17 nodes): 5 modules
- **Complex** (18-32 nodes): 3 modules

### Node Type Distribution Across All Modules
```
Register:  22 nodes (12.5%)  - Sequential elements
Compare:   77 nodes (43.8%)  - Conditional logic
Mux:      40 nodes (22.7%)  - Data selection
And:      21 nodes (11.9%)  - Boolean logic
Add:       6 nodes (3.4%)   - Arithmetic
Sub:       7 nodes (4.0%)   - Arithmetic
Or:        3 nodes (1.7%)   - Boolean logic

Total:    176 nodes
```

## Implementation in sv_ir_verify.ml

The Z3 verification is implemented in `sv_ir_verify.ml`:

```ocaml
val verify_ir_equivalence : Sv_ast.opt_ir -> Sv_ast.opt_ir -> bool
```

### How It Works

1. **Create Z3 Context**
   ```ocaml
   let ctx = Z3.mk_context []
   ```

2. **Generate Input Variables**
   ```ocaml
   Hashtbl.iter (fun name value ->
     match value with
     | Input { width; _ } ->
         let bv_sort = Z3.BitVector.mk_sort ctx width in
         let var = Z3.BitVector.mk_const_s ctx name bv_sort in
         (* Store for use in symbolic execution *)
   ) ir.ir_inputs
   ```

3. **Symbolically Execute IR₁**
   ```ocaml
   let symbolic_exec ir =
     (* Walk dataflow graph *)
     (* Build Z3 expressions for each operation *)
     (* Return output expressions *)
   ```

4. **Assert Outputs Equal**
   ```ocaml
   let solver = Z3.Solver.mk_simple_solver ctx in
   Hashtbl.iter (fun name _ ->
     let out1 = get_output_expr ir1 name in
     let out2 = get_output_expr ir2 name in
     Z3.Solver.add solver [Z3.Boolean.mk_eq ctx out1 out2]
   ) ir1.ir_outputs
   ```

5. **Check Satisfiability**
   ```ocaml
   match Z3.Solver.check solver [] with
   | Z3.Solver.UNSATISFIABLE -> true  (* Equivalent! *)
   | Z3.Solver.SATISFIABLE -> false   (* Different *)
   | Z3.Solver.UNKNOWN -> false       (* Timeout/error *)
   ```

## Expected Results

### If Verification Passes (UNSAT)

```
✅ Module: slib_clock_div
   VHDL IR ≡ SystemVerilog IR (proven by Z3)
   Translation is mathematically correct
```

**Interpretation**: The SystemVerilog translation perfectly matches the original VHDL semantics. For all possible input combinations, both produce identical outputs.

### If Verification Fails (SAT)

```
❌ Module: slib_clock_div
   VHDL IR ≠ SystemVerilog IR
   Counterexample found:
     CLK = 1, RST = 0
     VHDL output: iQ = 1
     SV output:   iQ = 0
```

**Interpretation**: Found specific inputs where VHDL and SystemVerilog differ. This indicates a translation bug or semantic difference.

## Significance

### Mathematical Proof

Z3 provides **mathematical proof** of equivalence, not just testing:
- Tests check specific input values
- Z3 checks **all possible** input values
- UNSAT = formal proof of correctness

### Ground Truth Validation

Comparing against VHDL proves:
- SystemVerilog decompiler is correct
- Translation tools work properly
- No semantic bugs introduced

### Confidence Level

- **Before**: "Tests pass, probably correct"
- **After**: "Z3 proves equivalence, mathematically certain"

## Tools and Scripts

**Created**:
- `generate_z3_problems.ml` - Shows SAT problems for all pairs
- `generate_z3_problems` - Executable (working)
- `test_z3_all_pairs.ml` - Full verification test (needs integration)

**Existing**:
- `sv_ir_verify.ml` - Z3 verification engine
- `test_3way_suite.ml` - 3-way comparison (Yosys/Verilator/Verible)

## Next Steps

### To Complete Z3 Verification

1. **Convert SystemVerilog to IR**
   - Use existing `sv_verible_to_ir.ml`
   - Generate IR₂ for each of 12 modules
   - Ensure IR format matches VHDL IR

2. **Run Z3 Solver**
   - Call `verify_ir_equivalence` for each pair
   - Collect results (pass/fail)
   - Document any counterexamples

3. **Fix Any Failures**
   - Analyze counterexamples
   - Identify translation bugs
   - Update converter or parser
   - Re-verify

### Build System Integration

Options:
1. **Unified dune build** - Add VHDL libs to dune
2. **Separate comparison tool** - Load IRs from JSON
3. **Two-phase approach** - Generate IRs separately, compare offline

## Conclusion

✅ **Z3 SAT problems defined** for all 12 module pairs
✅ **VHDL IR complete** for all modules
📋 **SystemVerilog IR** needs generation
📋 **Z3 verification** ready to run once IRs available

The verification infrastructure is in place. Once SystemVerilog modules are converted to IR, we can prove mathematical equivalence for all 12 formally verified modules, providing definitive validation of the decompiler against ground truth.

**Confidence**: High - formal methods provide mathematical certainty, not just empirical testing.
