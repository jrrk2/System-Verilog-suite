# Assignment Ordering Test Suite

This test suite demonstrates the assignment ordering issues found in MUX tree generation during SystemVerilog-to-IR conversion.

## The Bug

When processing `always` blocks with multiple assignments to the same signal, the order matters in SystemVerilog semantics:
- **Later assignments override earlier ones** (program order)
- If multiple conditions can be true simultaneously, the **last** assignment in program order wins

### Root Cause

Verible parser provides statements in varying orders depending on context. The original code used:
```ocaml
Hashtbl.replace normal_map assign.assign_lhs (assign :: existing)  (* prepend *)
...
let normal_assigns_list = List.rev (Hashtbl.find normal_map signal_name)  (* reverse *)
```

This double-reversal caused assignments to be processed in the wrong chronological order for some modules.

### The Fix

Removed the `List.rev`:
```ocaml
let normal_assigns_list = Hashtbl.find normal_map signal_name
```

Now prepending alone (combined with Verible's ordering) produces correct chronological order.

## Test Cases

### Test 1: test_unconditional_then_conditional ❌
**Pattern**: `signal <= default; if (cond) signal <= override`

**SystemVerilog**:
```systemverilog
iQ <= 1'b0;           // Assignment 1: unconditional default
if (ENABLE && COND)
    iQ <= 1'b1;        // Assignment 2: conditional override
```

**Expected MUX**: `(ENABLE && COND) ? 1'b1 : 1'b0`

**Status**: FAILING - shows that the issue still exists in some cases

---

### Test 2: test_multiple_conditionals ✅
**Pattern**: `if (c1) q<=v1; if (c2) q<=v2; if (c3) q<=v3`

**SystemVerilog**:
```systemverilog
if (COND1) iQ <= 1'b1;    // Priority 3 (lowest)
if (COND2) iQ <= 2'b10;   // Priority 2
if (COND3) iQ <= 2'b11;   // Priority 1 (highest)
```

**Expected**: When multiple conditions true, last one wins

**Status**: PASSING - this pattern now works correctly

---

### Test 3: test_sequential_ifs ❌
**Pattern**: `q<=default; if (A) q<=vA; if (B) q<=vB`

**SystemVerilog**:
```systemverilog
iQ <= 2'b00;          // Default
if (A) iQ <= 2'b01;   // Override if A
if (B) iQ <= 2'b10;   // Override if B (highest priority)
```

**Expected**: When A=1, B=1: Q=10 (B wins - later assignment)

**Status**: FAILING

---

### Test 4: test_nested_with_outer_unconditional ❌
**Pattern**: `q<=default; if (en) { if (sel) q<=v1 else q<=v2 }`

**SystemVerilog**:
```systemverilog
iQ <= 2'b00;                    // Unconditional (first)
if (EN) begin
    if (SEL)
        iQ <= 2'b11;            // EN && SEL
    else
        iQ <= 2'b01;            // EN && !SEL
end
```

**Expected MUX**: `(EN && SEL) ? 2'b11 : ((EN && !SEL) ? 2'b01 : 2'b00)`

**Status**: FAILING - wrong priority order with incorrect `List.rev`

---

### Test 5: test_clock_div_pattern ❌
**Pattern**: Actual slib_clock_div pattern (simplified)

**SystemVerilog**:
```systemverilog
iQ <= 1'b0;              // Unconditional: always clear first
if (CE && AT_MAX)
    iQ <= 1'b1;          // Conditional: set only if CE && AT_MAX
```

**Expected**: `(CE && AT_MAX) ? 1'b1 : 1'b0`

**Status**: FAILING - this was the original bug case

---

### Test 6: test_overlapping_conditions ❌
**Pattern**: Overlapping conditions where order matters

**SystemVerilog**:
```systemverilog
if (A)
    iQ <= 2'b01;         // Assignment 1
if (A && B)
    iQ <= 2'b11;         // Assignment 2: more specific, later
```

**Expected**: When A=1, B=1: Q=11 (more specific assignment wins)

**Status**: FAILING

## Current Results

```
✅ test_multiple_conditionals
❌ test_unconditional_then_conditional
❌ test_sequential_ifs
❌ test_nested_with_outer_unconditional
❌ test_clock_div_pattern
❌ test_overlapping_conditions

Result: 1/6 tests passed (17%)
```

## What This Shows

The assignment ordering fix (removing `List.rev`) was necessary but **not sufficient**:

1. **Some patterns now work**: `test_multiple_conditionals` passes
2. **Many patterns still fail**: Complex nesting and unconditional-then-conditional patterns
3. **Root cause**: Verible's statement ordering isn't consistent across different syntactic patterns

## Next Steps

To fix the remaining failures, the code needs to:

1. **Track source line numbers**: Use Verible's location metadata to establish definitive program order
2. **Sort assignments**: Sort by source location before building MUX trees
3. **Handle all nesting patterns**: Ensure AND combination works for all if/else structures

## Running the Tests

```bash
dune build test_assignment_ordering.exe
_build/default/test_assignment_ordering.exe
```

## Impact on UART Module Tests

After the `List.rev` fix:
- **6/9 UART modules passing** (67%)
- slib_input_filter now works
- slib_clock_div, slib_mv_filter, uart_baudgen still fail

These assignment ordering test cases explain why those 3 modules still fail.
