# Statement Ordering Analysis Tool

This document explains the statement ordering dumper tool and the assignment ordering bug it revealed.

## The Tool: test_token_dumper.exe

**Purpose**: Show the exact order in which Verible's parser extracts statements from SystemVerilog source code.

**Usage**:
```bash
_build/default/test_token_dumper.exe <systemverilog_file>
```

**What It Does**:
1. Shows the original SystemVerilog source with line numbers
2. Parses the file using Verible's parser
3. Extracts all assignments from `always` blocks
4. Shows the order assignments were extracted from the parse tree
5. Highlights ordering issues

## Example Output

```
╔════════════════════════════════════════════════════════════════╗
║  SystemVerilog Statement Ordering Analyzer                    ║
║  Shows how Verible orders statements in parse tree            ║
╚════════════════════════════════════════════════════════════════╝

File: /tmp/slib_clock_div.sv

════════════════════════════════════════════════════════════
ORIGINAL SOURCE (program order)
════════════════════════════════════════════════════════════

 20:   iQ <=  1'b0;              // Unconditional default
 21:     if ((CE ==  1'b1))
 22:           begin
 23:       if ((iCounter == (RATIO - 1)))
 24:                   begin
 25:           iQ <=  1'b1;     // Conditional override
 26:             iCounter <= 0;
 27:                       end
 28:            else
 29:           begin
 30:           iCounter <= iCounter + 1;
 31:                       end
 32:                 end

════════════════════════════════════════════════════════════
PARSED STATEMENT ORDER (from Verible parse tree)
════════════════════════════════════════════════════════════

Module: slib_clock_div

always @(posedge CLK or posedge RST)
  Statements extracted in this order:
    [0] iQ <= <expr>  (condition: NONE - unconditional)
    [1] iQ <= <expr>  (condition: SOME - conditional)
```

## What This Reveals

### Before the Fix (with List.rev)

**Bug**: Some modules had assignments in wrong order because:

1. Verible parser stores statements in varying orders depending on context
2. Our code did: `assign :: existing` (prepending) THEN `List.rev`
3. This double-reversal caused wrong ordering for some statement patterns

**Symptom**: Unconditional assignments appeared AFTER conditional ones, causing MUX trees to have wrong priority.

### After the Fix (removing List.rev)

**Solution**: Remove the `List.rev` when retrieving grouped assignments.

```ocaml
(* BEFORE - Bug *)
let normal_assigns_list = List.rev (Hashtbl.find normal_map signal_name)

(* AFTER - Fixed *)
let normal_assigns_list = Hashtbl.find normal_map signal_name
```

**Result**: Prepending alone (with Verible's ordering) now produces correct chronological order for most patterns.

### Current Status

**6/9 UART modules passing (67%)**

**Correct ordering for**:
- slib_clock_div: unconditional (line 20) → conditional (line 25) ✓
- test_unconditional_then_conditional: unconditional → conditional ✓
- test_multiple_conditionals: sequential conditionals in order ✓

**Still has issues**:
- 3 modules still fail Z3 verification
- May indicate remaining edge cases in conditional combination logic
- Or issues with MUX tree building beyond just ordering

## The Assignment Ordering Bug Explained

### Why Order Matters

In SystemVerilog:
```systemverilog
always @(posedge CLK) begin
    signal <= default;      // Assignment 1
    if (condition)
        signal <= override; // Assignment 2
end
```

**Semantics**: Later assignments override earlier ones if their condition is true.

**MUX Tree**: `condition ? override : default`

**Wrong Order Would Give**: `false ? default : override` ❌

### The Double-Reversal Problem

**Verible Parse Tree**: May store statements in reverse order (implementation detail)

**Our Grouping Code**:
```ocaml
(* As we iterate, prepend to list *)
Hashtbl.replace map signal (assign :: existing)  (* Reverses *)

(* Then retrieve and reverse again *)
List.rev (Hashtbl.find map signal)              (* Double reversal! *)
```

**Effect**:
- If Verible gives [A, B, C], we prepend to get [C, B, A], then reverse to [A, B, C] ✓
- But if Verible gives [C, B, A], we prepend to get [A, B, C], then reverse to [C, B, A] ❌

**Fix**: Trust that prepending gives correct order, remove the `List.rev`

### Testing the Fix

Use the dumper to verify ordering:

```bash
# Test slib_clock_div
_build/default/test_token_dumper.exe /tmp/slib_clock_div.sv

# Test other patterns
_build/default/test_token_dumper.exe /tmp/test_unconditional_then_conditional.sv
_build/default/test_token_dumper.exe /tmp/test_sequential_ifs.sv
```

Look for:
- `[0] signal <= <expr>  (condition: NONE)`  should come first if it's the default
- `[1] signal <= <expr>  (condition: SOME)` should come second if it's the override
- Compare [N] indices with source code line numbers

## Files

- **sv_token_dumper.ml**: Core dumper logic (reconstructs source from parse tree)
- **test_token_dumper.ml**: Test program that uses elaboration to show statement order
- **test_assignment_order.sv**: Test cases demonstrating ordering issues
- **test_assignment_ordering.ml**: Automated test suite

## Key Takeaways

1. **Verible's parser order is not guaranteed** - depends on grammar rules
2. **Double-reversals are dangerous** - one reversal should be enough
3. **Always verify with dumper** - visual inspection reveals ordering bugs
4. **Z3 verification catches these** - formal verification is essential
5. **Fix was simple** - removing `List.rev` fixed the core issue

## Next Steps

To achieve 100% pass rate:

1. **Debug remaining 3 failures** using the dumper
2. **Check MUX tree priority logic** - may have other issues
3. **Verify enable signal generation** - timing may be off
4. **Test more edge cases** - nested patterns, multiple overrides

The dumper tool makes debugging these issues straightforward by showing exactly what order Verible extracts statements.
