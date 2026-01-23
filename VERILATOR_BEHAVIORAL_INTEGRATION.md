# Verilator → Behavioral IR Integration

## Summary

Successfully integrated Verilator's JSON AST output with the language-neutral behavioral IR infrastructure. This completes the unification of all three frontend paths (VHDL, SystemVerilog/Verible, and Verilator) to use the same optimization pipeline.

## Test Results

```bash
$ dune exec ./test_verilator_behavioral.exe test/obj_dir/Vcounter.tree.json
```

**Module:** counter (8-bit counter with reset)
- **Signals:** 4 (clk, reset, count, count_reg)
- **Processes:** 1 (always_ff block)
- **Registers after optimization:** 1 (count_reg)
- **Wires:** 2 (CSE temps)

## Architecture

### Pipeline Flow

```
Verilator JSON → Sv_parse.parse → sv_ast
                      ↓
              verilator_to_behavioral
                      ↓
              Behavioral IR (bprogram)
                      ↓
         Optimization Pipeline (SSA, const prop, DCE, CSE)
                      ↓
              Register Inference
                      ↓
           1 register, 2 wires ✅
```

### Key Components

**verilator_to_behavioral.ml (394 lines)**
- `expr_to_bexpr` - Converts Verilator expressions to behavioral IR
- `stmt_to_bstmt` - Converts Verilator statements to behavioral IR
- `always_to_bprocess` - Converts always blocks to BSequential/BCombinational
- `module_to_bmodule` - Extracts signals and processes
- `convert_verilator_json_to_behavioral` - Main entry point

**test_verilator_behavioral.ml (136 lines)**
- Test harness demonstrating full pipeline
- Compares old opt_ir approach vs new behavioral IR approach
- Shows register inference results

## Old vs New Approach

### OLD: behavioural_to_opt_ir.ml
- Verilator JSON → opt_ir (dataflow graph)
- No high-level optimization passes
- No SSA, no DCE, no CSE
- Register inference at opt_ir level
- **Separate code path from VHDL/SV**

### NEW: verilator_to_behavioral.ml
- Verilator JSON → Behavioral IR
- ✅ SSA construction
- ✅ Constant propagation
- ✅ Dead code elimination
- ✅ Common subexpression elimination
- ✅ Register inference with MUX tree building
- **Shared code path with VHDL/SV**

## Benefits

1. **Language-Neutral IR** - Verilator now uses same IR as VHDL and SystemVerilog
2. **Shared Optimization** - All optimizations work on all three frontends
3. **Module-Level DCE** - Cross-process analysis eliminates truly unused signals
4. **Z3 Miter Verification** - Can formally verify Verilator vs Verible vs VHDL
5. **Unified Register Inference** - No more language-specific register bugs

## Integration Points

### Parsing
Uses existing `Sv_parse.parse` to handle Verilator's JSON format with "p" suffix fields (modulesp, stmtsp, sensesp).

### Type Mapping
- Verilator JSON → sv_ast (existing)
- sv_ast → Behavioral IR (new)
  - Always blocks → BSequential/BCombinational
  - Vars → bsignal
  - Expressions → bexpr
  - Statements → bstmt

### Optimization
Uses `behavioral_optimize.ml` pipeline:
1. SSA construction
2. Constant propagation (iterative to fixpoint)
3. Dead code elimination (module-level liveness)
4. Common subexpression elimination
5. Register inference (MUX tree building)

## Files Modified

- **verilator_to_behavioral.ml** (394 lines) - New converter
- **test_verilator_behavioral.ml** (136 lines) - New test harness
- **dune** - Added verilator_to_behavioral and test_verilator_behavioral

## Usage

### Generate Verilator JSON
```bash
verilator --json-only --dump-tree-json \
  --json-only-output output.json \
  --top-module <module_name> <sv_file>
```

### Run Conversion
```bash
dune exec ./test_verilator_behavioral.exe output.json
```

## Example Output

```
[1/3] Converting Verilator JSON to Behavioral IR...
✓ Conversion successful (1 modules)

Module: counter
  Signals: 4
  Processes: 1

[2/3] Running Optimization Pipeline...
✓ Optimization complete

[3/3] Register Inference...
  Registers: 1
  Wires: 2

Registers:
  - count_reg: 8 bits, clock=clk

═══════════════════════════════════════════════════════════════
  ✅ SUCCESS
═══════════════════════════════════════════════════════════════

Verilator JSON successfully processed through behavioral IR!
Module counter: 1 registers, 2 wires
```

## Status

✅ **COMPLETE AND WORKING**

All three frontends (VHDL, SystemVerilog/Verible, Verilator) now share the same behavioral IR infrastructure and optimization pipeline.
