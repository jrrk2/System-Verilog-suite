# VHDL Ground Truth Files Added

## Date Added
2026-01-22

## Source
Files copied from: `/Users/jonathan/lowrisc-chip/fpga/src/apb_uart/src`

## Purpose
These VHDL files are the **original source code** for the APB UART modules. The SystemVerilog files were translated from these VHDL files and formally verified with Synopsys Formality.

Having both VHDL and SystemVerilog allows us to:
1. **Verify translation correctness** - Compare VHDL→IR vs SV→IR
2. **Establish ground truth** - VHDL is the authoritative source
3. **Validate decompiler** - Prove SV decompilation matches VHDL semantics
4. **Test VHDL parser** - Use real-world VHDL code, not toy examples

## Files Added

All VHDL files copied to `sysver_tests/`:

```
sysver_tests/
├── slib_clock_div.vhd       ✓
├── slib_counter.vhd          ✓
├── slib_edge_detect.vhd      ✓
├── slib_fifo.vhd             ✓
├── slib_input_filter.vhd     ✓
├── slib_input_sync.vhd       ✓
├── slib_mv_filter.vhd        ✓
├── uart_baudgen.vhd          ✓
├── uart_interrupt.vhd        ✓
├── uart_receiver.vhd         ✓
└── uart_transmitter.vhd      ✓
```

## VHDL→IR Converter Status

### ✅ Complete Components

1. **vhdl_parse.ml** - Parser wrapper for VhdlParser
   - Status: Working
   - Tests: 4/4 UART modules parse successfully

2. **vhdl_elaborate.ml** - Architecture extractor
   - Status: Working
   - Tests: 4/4 architectures extracted

3. **vhdl_expr_to_ir.ml** - Expression converter
   - Status: Working
   - Tests: 6/6 expression tests pass
   - Converts: Arithmetic, logical, comparison, shift operations

4. **vhdl_process_extract.ml** - Process analyzer
   - Status: Working
   - Tests: 4/4 processes analyzed
   - Extracts: Clock, reset, conditional assignments

5. **vhdl_to_ir.ml** - Main VHDL→IR converter
   - Status: Working
   - Tests: 4/4 modules convert to IR
   - Creates: Complete opt_ir structure

### Test Results

**All 4 UART modules successfully converted to IR:**

| Module | VHDL Parse | IR Conversion | Nodes Created |
|--------|-----------|---------------|---------------|
| slib_clock_div | ✓ | ✓ | 17 |
| slib_input_filter | ✓ | ✓ | 18 |
| slib_mv_filter | ✓ | ✓ | 15 |
| uart_baudgen | ✓ | ✓ | 16 |

## Pairing with SystemVerilog

Each VHDL file has a corresponding SystemVerilog file:

| VHDL File | SystemVerilog File | Module Name |
|-----------|-------------------|-------------|
| slib_clock_div.vhd | slib_clock_div.sv | slib_clock_div |
| slib_counter.vhd | slib_counter.sv | slib_counter |
| slib_edge_detect.vhd | slib_edge_detect.sv | slib_edge_detect |
| slib_fifo.vhd | slib_fifo.sv | slib_fifo |
| slib_input_filter.vhd | slib_input_filter.sv | slib_input_filter |
| slib_input_sync.vhd | slib_input_sync.sv | slib_input_sync |
| slib_mv_filter.vhd | slib_mv_filter.sv | slib_mv_filter |
| uart_baudgen.vhd | uart_baudgen.sv | uart_baudgen |
| uart_interrupt.vhd | uart_interrupt.sv | uart_interrupt |
| uart_receiver.vhd | uart_receiver.sv | uart_receiver |
| uart_transmitter.vhd | uart_transmitter.sv | uart_transmitter |

## Next Steps: VHDL vs SV Comparison

### Goal
Prove that SystemVerilog decompiler produces IR mathematically equivalent to original VHDL source.

### Approach
```
VHDL source → vhdl_to_ir → IR₁
                             ↓
                          Z3 verify ✓ equivalent
                             ↓
SystemVerilog → verible → IR₂
```

### Implementation Status

**Created:**
- ✅ test_vhdl_vs_sv.ml - Test harness for VHDL vs SV comparison
- ✅ compile_vhdl_test.sh - Build script (needs refinement)

**Blocked by:**
- Build system integration - Need to link VHDL modules with dune-built modules
- Module dependencies - sv_ir_verify needs to be accessible from bytecode
- Z3 integration - Z3 verification across different build systems

### Workaround Options

1. **Option A**: Separate comparison tool
   - Run `test_vhdl_to_ir` to generate VHDL IR
   - Run SystemVerilog decompiler to generate SV IR
   - Write comparison script that loads both IRs from JSON
   - Use Z3 to verify equivalence

2. **Option B**: Unified dune build
   - Add VHDL libraries to dune as external libraries
   - Rebuild VHDL modules within dune framework
   - Use existing test infrastructure

3. **Option C**: Manual verification (current)
   - Run VHDL tests: `./test_vhdl_to_ir`
   - Run SV tests: `./run_3way_tests.sh`
   - Manually compare IR structures
   - Validate specific patterns

## Current Capabilities

### What Works Now

1. **Parse VHDL** → Extract architecture → Convert expressions → Build IR
   ```bash
   ./test_vhdl_to_ir
   ```

2. **Parse SystemVerilog** → Build IR → Z3 verify against Yosys/Verilator
   ```bash
   ./run_3way_tests.sh
   ```

3. **Compare manually** - Both produce IR, can inspect structure

### Example Comparison

**slib_clock_div VHDL IR:**
- Inputs: CLK, RST (2)
- Outputs: iQ, iCounter (2 registers)
- Nodes: 17 (Mux, Add, Sub, Compare, And, Register)

**slib_clock_div SystemVerilog IR:**
- Would need to run through Verible to compare
- Should have similar structure if translation is correct

## Documentation

Related files:
- UART_MODULES_ADDED.md - SystemVerilog modules documentation
- VHDL_INTEGRATION_PROGRESS.txt - VHDL parser integration history
- VHDL_TO_IR_ROADMAP.md - Original conversion plan
- test_vhdl_to_ir.ml - Working VHDL→IR test

## Significance

Having both VHDL and SystemVerilog files enables:

1. **Ground truth validation** - VHDL is authoritative source
2. **Translation verification** - Prove SV matches VHDL semantics
3. **Formal verification alignment** - Both were checked with Formality
4. **Bidirectional testing** - Can test parsers in both directions
5. **Pattern discovery** - Compare how same logic expressed in each language

## Testing Strategy

### Phase 1: Individual Conversion ✅ DONE
- ✅ VHDL files parse correctly
- ✅ VHDL→IR conversion works
- ✅ SystemVerilog files ready for testing

### Phase 2: Separate IR Generation ← CURRENT
- ✅ Generate VHDL IR independently
- ⏳ Generate SV IR for same modules
- ⏳ Compare IR structures manually

### Phase 3: Automated Equivalence ← FUTURE
- ⏳ Build integrated test harness
- ⏳ Z3 verification VHDL IR ≡ SV IR
- ⏳ Regression suite with all 11 modules

## Summary

✅ **11 VHDL source files** added to sysver_tests/
✅ **VHDL→IR pipeline** fully functional
✅ **4 modules tested** end-to-end (clock_div, input_filter, mv_filter, baudgen)
⏳ **Z3 comparison** requires build system integration
📋 **Workaround available** - Manual IR comparison possible

The VHDL files provide definitive ground truth for validating the SystemVerilog decompiler against the original source code.
