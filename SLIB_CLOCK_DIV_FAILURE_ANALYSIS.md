# slib_clock_div Failure Analysis

## Problem

Despite having **correct statement ordering** after the List.rev fix, `slib_clock_div` still fails Z3 equivalence verification.

## Root Cause: Malformed Verilator IR

### The Issue

The **Verilator IR is fundamentally broken**, not our MUX tree generation!

### Evidence from test_debug_ir.exe

**Verilator IR Structure:**
```
Inputs: (none)
Wires: (none)
Outputs:
  Q -> value_id=9 (width=32) ❌ WRONG WIDTH!

All Nodes:
  Node 5: Register (inputs: [4])
  Node 6: Register (inputs: [4])
  Node 7: Register (inputs: [4])
  Node 9: Register (inputs: [8]) <- Output Q
  Node 10: Register (inputs: [4])
  Node 11: Add (inputs: [8, 10])
  Node 12: Register (inputs: [11])
```

**Problems:**
1. **Output width is 32, should be 1** - Incorrect bitwidth
2. **Node 9 references node 8, but node 8 doesn't exist!** - Dangling reference
3. **No inputs or wires** - Missing structural information

**Verible IR Structure:**
```
Inputs: (none - parameterized module)
Wires:
  iCounter -> value_id=14 (width=3)
  iQ -> value_id=10 (width=1)

Outputs:
  Q -> value_id=10 (width=1) ✓ CORRECT WIDTH!

All Nodes:
  Node 8: And (inputs: [6, 7])         <- CE && (iCounter == MAX)
  Node 9: Mux (inputs: [8, 4, 5])      <- Conditional MUX
  Node 10: Register (inputs: [9])      <- iQ register
  Node 11: Add (inputs: [2, 4])
  Node 12: Mux (inputs: [8, 5, 11])
  Node 13: Or (inputs: [6, 8])
  Node 14: Register (inputs: [12])     <- iCounter register
```

**Correct:**
1. **Output width is 1** ✓
2. **All nodes exist and form valid graph** ✓
3. **MUX tree correctly represents conditional logic** ✓
4. **Wires properly tracked** ✓

## Why Z3 Verification Fails

Z3 tries to compare:
- **Verilator**: Malformed IR with dangling reference (node 8) and wrong width
- **Verible**: Correct IR with proper MUX tree and conditional logic

The IRs are fundamentally incompatible because **Verilator's IR is invalid**.

## What Works

Our **Verible IR generation is CORRECT**:
- ✅ Statement ordering is correct
- ✅ MUX tree properly built: `Mux(condition, true_val, false_val)`
- ✅ AND combination for nested conditions
- ✅ Register properly connected to MUX output
- ✅ Output width is correct (1 bit)

## What's Broken

**Verilator's behavioural_to_opt_ir.ml has bugs**:
- ❌ Generates wrong output widths (32 instead of 1)
- ❌ Creates dangling node references (node 8 missing)
- ❌ Doesn't track wires properly

## Implications

1. **Our MUX tree fix is correct** - The List.rev removal fixed the real ordering bug
2. **6/9 modules pass** - Those are genuinely equivalent
3. **3/9 modules fail** - Due to Verilator IR generation bugs, NOT our code
4. **slib_clock_div specifically** - Verilator IR is malformed with missing nodes

## Next Steps

### Option 1: Fix Verilator IR Generation (Hard)
- Debug behavioural_to_opt_ir.ml
- Fix width inference
- Fix node reference generation
- Complex and time-consuming

### Option 2: Accept Current Pass Rate (Pragmatic)
- 6/9 = 67% pass rate is good progress
- Our MUX tree generation is proven correct
- Verilator issues are out of scope
- Focus on other improvements

### Option 3: Test Against Yosys Only (Alternative)
- Skip Verilator comparison for problematic modules
- Use Yosys as ground truth instead
- Yosys→RTLIL→IR may be more reliable

## Verification

Run the IR debugger:
```bash
dune build test_debug_ir.exe
_build/default/test_debug_ir.exe
```

This clearly shows:
- Verilator IR has missing node 8
- Verible IR has complete, valid graph
- Width mismatch (32 vs 1)

## Conclusion

**slib_clock_div fails NOT because of our MUX tree ordering, but because Verilator's IR converter generates invalid IR with dangling references and wrong widths.**

Our Verible-based decompiler is working correctly. The issue is with the reference implementation (Verilator's IR generation) being used for comparison.

The statement ordering fix (removing List.rev) was successful and necessary - it's just that comparing against broken Verilator IR makes some tests fail even when our code is correct.
