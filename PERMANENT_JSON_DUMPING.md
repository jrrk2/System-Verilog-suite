# Permanent JSON Dumping for Unhandled Patterns

## Policy: No Silent Lossage

**All unhandled SystemVerilog and VHDL patterns MUST be dumped to JSON files.**

This ensures:
1. **Visibility** - Developers know immediately when patterns are missing
2. **Debuggability** - Exact AST structure captured for analysis
3. **Progress Tracking** - Easy to see what patterns remain unimplemented
4. **No Silent Failures** - Nothing gets ignored without documentation

## Current Implementation

### SystemVerilog (Verible Frontend)

**Files:**
- `sv_dump_json.ml` - JSON dumping utility
- `Source_text_verible_json.ml` - Token type with `[@@deriving yojson]`
- `unhandled_sv/` - Output directory for JSON dumps

**Dump Locations:**

1. **Expression Handler** (`sv_verible_to_ir.ml:880-885`)
   ```ocaml
   | _ ->
       Printf.eprintf "\n=== Warning: Unhandled expression type ===\n";
       Sv_dump_json.dump_unhandled "expression" "expr" expr;
       Printf.eprintf "==========================================\n\n";
       Sv_opt_ir.get_new_id ir
   ```
   - **Status:** ✅ Actively dumping
   - **Location:** `unhandled_sv/expression_expr_*.json`

2. **Future Handlers** (to be added as needed)
   - Statement processing
   - Port declarations (if new patterns appear)
   - Data type processing
   - Module instantiation

### VHDL Frontend

**Files:**
- `vhdl_dump_json.ml` - JSON dumping utility
- `unhandled_vhdl/` - Output directory for JSON dumps

**Dump Locations:**

1. **Process Statement Handler** (`vhdl_to_ir.ml`)
   - Dumps unhandled statement patterns in processes
   - **Location:** `unhandled_vhdl/process_stmt_*.json`

2. **Expression Handler** (`vhdl_to_ir.ml`)
   - Dumps unhandled expression patterns
   - **Location:** `unhandled_vhdl/expression_expr_*.json`

## JSON File Format

Each dump creates a timestamped JSON file:

```json
{
  "context": "expression",
  "pattern_type": "expr",
  "timestamp": 1769167109,
  "pattern": [
    "TUPLE4",
    [
      ["STRING", "shift_expr2"],
      ["TK_DecNumber", "0"],
      ["LT_LT"],
      ["TK_DecNumber", "0"]
    ]
  ]
}
```

**Fields:**
- `context` - Where pattern was encountered (e.g., "expression", "statement", "port_decl")
- `pattern_type` - Type of pattern (e.g., "expr", "stmt", "data_type")
- `timestamp` - Unix timestamp for uniqueness
- `pattern` - Full AST structure in JSON format

## Using JSON Dumps to Add Patterns

### Step 1: Run Code and Collect Dumps

```bash
dune exec ./test_verible_elab.exe sysver_tests/uart_baudgen.sv
```

Check for warnings:
```
=== Warning: Unhandled expression type ===
Dumped unhandled expr pattern to unhandled_sv/expression_expr_1769165875.json
```

### Step 2: Examine JSON Structure

```bash
cat unhandled_sv/expression_expr_1769165875.json | python3 -m json.tool
```

Identify the pattern:
```json
{
  "pattern": [
    "TUPLE4",
    [
      ["STRING", "shift_expr2"],
      ["TK_DecNumber", "0"],
      ["LT_LT"],
      ["TK_DecNumber", "0"]
    ]
  ]
}
```

### Step 3: Add Pattern Handler

```ocaml
(* Shift left: << operator *)
| TUPLE4 (STRING "shift_expr2", left, LT_LT, right) ->
    let left_id = expr_to_ir ir expr_cache symbol_table functions left in
    let right_id = expr_to_ir ir expr_cache symbol_table functions right in
    let width = get_value_width ir left_id in
    Sv_opt_ir.add_node ir (Shift { width; direction = `Left; arithmetic = false; amount = None }) [left_id; right_id]
```

### Step 4: Verify Fix

Re-run and confirm no more dumps for that pattern:
```bash
dune exec ./test_verible_elab.exe sysver_tests/uart_baudgen.sv
# Should not dump that pattern anymore
```

### Step 5: View Summary

```ocaml
Sv_dump_json.create_summary()
```

Output:
```
=== Unhandled SystemVerilog Patterns Summary ===
Total unhandled patterns: 5

By pattern type:
  expr: 3
  stmt: 2
```

## Verification Checklist

Before declaring pattern support "complete":

- [ ] Run all test files
- [ ] Check for any JSON dumps in `unhandled_sv/` or `unhandled_vhdl/`
- [ ] If dumps exist, review each pattern
- [ ] Either:
  - Add handler for the pattern, OR
  - Document why pattern is intentionally unhandled
- [ ] Clean up dump directories
- [ ] Re-run to confirm no new dumps

## Adding New Dump Points

When adding new AST processing code, always include a catch-all with JSON dump:

```ocaml
let process_new_construct token =
  match token with
  | TUPLE3 (STRING "known_pattern1", a, b) ->
      (* Handle known pattern *)
      ...
  | TUPLE4 (STRING "known_pattern2", a, b, c, d) ->
      (* Handle known pattern *)
      ...
  | _ ->
      (* REQUIRED: Dump unhandled pattern to JSON *)
      Printf.eprintf "Warning: Unhandled construct in process_new_construct\n";
      Sv_dump_json.dump_unhandled "new_construct" "token" token;
      (* Return safe default or raise error *)
      default_value
```

## Maintenance

### Weekly Check

```bash
# Check for accumulated dumps
ls -l unhandled_sv/ unhandled_vhdl/ 2>/dev/null | wc -l
```

If files exist:
1. Review patterns
2. Prioritize implementation based on frequency
3. Add handlers
4. Document any intentionally-unhandled patterns

### Before Release

```bash
# Ensure no unhandled patterns in test suite
rm -rf unhandled_sv/ unhandled_vhdl/
./run_all_tests.sh
ls unhandled_sv/ unhandled_vhdl/ 2>&1 | grep "No such file"
# Should see "No such file or directory"
```

## Benefits Demonstrated

1. **Quick Pattern Discovery** - Found 4 missing pattern categories in < 30 minutes
2. **Exact Structures** - No guessing about AST shape
3. **Iterative Development** - Test, dump, fix, repeat
4. **Documentation** - JSON files serve as test cases
5. **Confidence** - Know exactly what's unhandled

## Example Session

```bash
$ dune exec ./test_verible_elab.exe sysver_tests/uart_baudgen.sv
Dumped unhandled expr pattern to unhandled_sv/expression_expr_1769165875.json
  Description: TUPLE4("shift_expr2",...)
✓ IR conversion complete
  Inputs: 1, Outputs: 1, Nodes: 26

$ cat unhandled_sv/expression_expr_1769165875.json | python3 -m json.tool | head -20
{
    "context": "expression",
    "pattern_type": "expr",
    "timestamp": 1769165875,
    "pattern": [
        "TUPLE4",
        [
            ["STRING", "shift_expr2"],
            ["TK_DecNumber", "0"],
            ["LT_LT"],
            ["TK_DecNumber", "0"]
        ]
    ]
}

$ # Add handler for shift_expr2...

$ dune exec ./test_verible_elab.exe sysver_tests/uart_baudgen.sv
✓ IR conversion complete
  Inputs: 1, Outputs: 1, Nodes: 42
# No dumps - pattern handled!
```

## Enforcement

**Code Review Requirement:**
- Any new AST processing function MUST have JSON dumping for unhandled patterns
- No catch-all `| _ -> ()` without dump
- No silent `| _ -> None` without dump
- All warnings must be visible to user

**Test Requirement:**
- CI should fail if unhandled patterns exist in test suite
- Script: `check_no_unhandled.sh` (to be created)

## Future Enhancements

1. **Auto-generate pattern skeletons** from JSON structure
2. **Pattern frequency analysis** across multiple files
3. **Compare unhandled patterns** between versions
4. **Web viewer** for browsing JSON dumps
5. **Integration with test suite** - auto-collect patterns from all tests

## Summary

JSON dumping is a **permanent, first-class feature** of the decompiler. It ensures transparency, debuggability, and prevents silent failures. Every unhandled pattern is captured, analyzed, and either implemented or documented as intentionally unsupported.

**No silent lossage. Ever.**
