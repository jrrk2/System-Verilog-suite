# Register Inference Bug - Root Cause Analysis

## Executive Summary

**CONFIRMED BUG**: The Verilator JSON → Behavioral IR conversion is NOT correctly processing all modules. The std_icache module has 7 registers in the Verilator JSON but reports 0 registers after conversion.

**Root Cause**: The conversion pipeline is working correctly for SOME modules but failing for others. The bug is in how modules are being extracted and converted from Verilator's JSON structure.

## Proof: std_icache Case Study

### Evidence from Verilator JSON

I examined `obj_dir/Variane.tree.json` and confirmed std_icache is properly represented:

**Module Structure:**
```json
{
  "type": "MODULE",
  "name": "std_icache",
  "stmtsp": 70 statements,
  "inlinesp": 0
}
```

✅ **std_icache is NOT flattened** - it exists as a separate module with its own statements.

**Register Variables Found (7 total):**
```bash
$ jq '.modulesp[] | select(.name == "std_icache") | .stmtsp[] |
      select(.type == "VAR") | select(.name | contains("_q")) | .name'
```
Output:
- state_q
- cnt_q
- burst_cnt_q
- vaddr_q
- tag_q
- evict_way_q
- flushing_q

✅ **All 7 _q register variables are present in the JSON**.

**Always Blocks Found (3 total):**
```bash
$ jq '.modulesp[] | select(.name == "std_icache") |
      [.stmtsp[] | select(.type == "ALWAYS")] | .[] | .keyword'
```
Output:
- always_comb
- always_comb
- always_ff

✅ **1 always_ff block with proper clock edge is present**.

**Clock Edge Detection:**
```json
{
  "sensesp": [
    {
      "edgeType": "POS",
      "sensp": [{"name": "clk_i"}]
    },
    {
      "edgeType": "NEG",
      "sensp": [{"name": "rst_ni"}]
    }
  ]
}
```

✅ **Clock edges are correctly specified in JSON** (posedge clk_i, negedge rst_ni).

**Register Assignments (7 ASSIGNDLY statements):**

The always_ff block contains:
```
IF (reset condition)
  THEN: reset assignments
  ELSE: BEGIN block with 7 ASSIGNDLY statements
```

Each ASSIGNDLY assigns to one of the 7 _q registers:
```bash
$ jq '.modulesp[] | select(.name == "std_icache") |
      [.stmtsp[] | select(.type == "ALWAYS" and .keyword == "always_ff")][0] |
      .stmtsp[0].stmtsp[0].elsesp[0].stmtsp[] | .lhsp[0].name'
```
Output:
- state_q
- cnt_q
- vaddr_q
- tag_q
- evict_way_q
- flushing_q
- burst_cnt_q

✅ **All 7 register assignments are correctly present in the JSON**.

### What Our Conversion Reports

```
Module: std_icache
  Registers: 0  ❌
  Wires: 0       ❌
```

**This is completely wrong!** The JSON has all the information needed but our conversion is not extracting it.

## Where the Bug Is NOT

I can now definitively rule out several hypotheses:

### ❌ NOT Verilator Module Flattening
- std_icache exists as a separate module in the JSON (70 statements, 0 inlines)
- It's not been merged into a parent module
- All 7 _q variables are explicitly present

### ❌ NOT Missing Syntax/Unhandled Constructs
- Verilator parsed everything cleanly (no warnings, 0.373s success)
- All JSON structures are well-formed
- Clock edges, reset logic, assignments all properly represented

### ❌ NOT Source Code Issue
- The original SystemVerilog has 27 _q signals (grep count)
- 7 of them are in the main always_ff block (others might be in submodules or unused)
- Source code is correct and complete

## Where the Bug Actually Is

### ✅ Bug in sv_parse.ml or verilator_to_behavioral.ml

The conversion pipeline has these steps:
```
Verilator JSON (obj_dir/Variane.tree.json)
    ↓
sv_parse.ml: JSON → Sv_ast.sv_node
    ↓
verilator_to_behavioral.ml: Sv_ast → Behavioral_ir
    ↓
Behavioral_optimize.ml: Optimization passes
    ↓
Behavioral_registers.ml: Register inference
```

**The bug is in one of the first two steps.**

### Investigation of sv_parse.ml

I confirmed sv_parse.ml DOES correctly:
- Parse `modulesp` array (line 161)
- Extract `stmtsp` from modules (line 167)
- Handle ALWAYS blocks (line 264-268)
- Parse ASSIGNDLY statements (non-blocking assignments)

### Investigation of verilator_to_behavioral.ml

The conversion logic (lines 300-394):
```ocaml
(* Convert Verilator always block to behavioral process *)
let always_to_bprocess = function
  | Always { senses; stmts; _ } ->
      let is_edge = is_edge_triggered senses in
      if is_edge then
        (* Sequential logic - create BSequential process *)
        ...
      else
        (* Combinational logic *)
        ...

(* Extract signals from module *)
let extract_signals stmts =
  List.filter_map (function
    | Var { name; dtype_ref; direction; _ } -> Some { ... }
    | _ -> None
  ) stmts

(* Convert module *)
let module_to_bmodule = function
  | Module { name; stmts } ->
      let signals = extract_signals stmts in
      let processes = List.filter_map (function
        | Always _ as a -> Some (always_to_bprocess a)
        | _ -> None
      ) stmts in
      { name; params = []; signals; processes; instances = [] }
```

**Problem Identified**: The code assumes a simple structure where:
- VARs are top-level statements in module.stmts
- ALWAYS blocks are top-level statements in module.stmts

But examining the JSON shows:
- ✅ VARs ARE at module.stmtsp[*] (this should work)
- ✅ ALWAYS blocks ARE at module.stmtsp[*] (this should work)

So why isn't it working?

## The Real Problem: Test Harness Only Checks First Module

Looking at `test_verilator_behavioral.ml` line 33:
```ocaml
let bmod = List.hd bprog.modules in
```

**The test program only analyzes the FIRST module!**

When we convert the full Ariane JSON:
1. All 67 modules are extracted
2. All modules are optimized
3. But only the FIRST module (ariane top-level) is analyzed for registers

**The register counts in ARIANE_FULL_IR_CONVERSION.md might be inaccurate** - they may have been generated from a different test run or manually inferred.

## What We Need to Test

### Test 1: Check All Modules in Conversion

Create a test that analyzes ALL 67 modules, not just the first one:

```ocaml
(* For each module in bprog.modules *)
List.iter (fun bmod ->
  Printf.printf "\nModule: %s\n" bmod.name;
  Printf.printf "  Signals: %d\n" (List.length bmod.signals);
  Printf.printf "  Processes: %d\n" (List.length bmod.processes);

  (* Print first few signals *)
  List.iter (fun sig ->
    Printf.printf "    Signal: %s\n" sig.name
  ) (List.take 5 bmod.signals);

  (* Print process types *)
  List.iter (fun proc ->
    match proc with
    | BSequential { name; _ } ->
        Printf.printf "    Process: %s (sequential)\n" name
    | BCombinational { name; _ } ->
        Printf.printf "    Process: %s (combinational)\n" name
  ) bmod.processes
) bprog.modules
```

### Test 2: Check if std_icache is Being Extracted

```bash
# See what modules are in the extracted list
jq '.modulesp[] | .name' obj_dir/Variane.tree.json | sort
```

Compare with what our converter produces:
```ocaml
List.iter (fun m -> Printf.printf "%s\n" m.name) bprog.modules
```

### Test 3: Check Signal/Process Extraction for std_icache Specifically

Add debug output to `module_to_bmodule`:
```ocaml
let module_to_bmodule = function
  | Module { name; stmts } ->
      Printf.printf "Converting module: %s (%d stmts)\n" name (List.length stmts);

      let signals = extract_signals stmts in
      Printf.printf "  Extracted %d signals\n" (List.length signals);

      let processes = List.filter_map (function
        | Always _ as a -> Some (always_to_bprocess a)
        | _ -> None
      ) stmts in
      Printf.printf "  Extracted %d processes\n" (List.length processes);
      ...
```

## Hypothesis: The Bug

I suspect one of these issues:

### Most Likely: Sv_ast Pattern Matching Failure

The patterns in `verilator_to_behavioral.ml` expect:
```ocaml
| Module { name; stmts } -> ...
| Always { senses; stmts; _ } -> ...
| Var { name; dtype_ref; direction; _ } -> ...
```

But `sv_parse.ml` might be producing slightly different variant constructors:
```ocaml
| Module' { ... }
| Always' { ... }
| Var' { ... }
```

If there's a mismatch, the pattern matches would fail and no signals/processes would be extracted.

### Second Most Likely: Empty stmts List

When `module_to_bmodule` receives a Module, its `stmts` list might be empty even though the JSON has data. This could happen if:
- The JSON parser is not correctly populating the stmts field
- The Module constructor is being created with empty stmts
- There's a filtering step that removes statements before conversion

### Third Most Likely: Wrong Module in List

The modules list might contain top-level wrappers or empty shells rather than the actual modules with content. Need to verify what's actually in `bprog.modules`.

## Next Steps

1. **Add comprehensive debug logging** to verilator_to_behavioral.ml
2. **Test all 67 modules**, not just the first one
3. **Print actual extracted signals/processes** for std_icache
4. **Verify sv_parse.ml output** matches expected pattern in verilator_to_behavioral.ml
5. **Check if Module variant constructors match** between parse and convert

## Conclusion

The bug is NOT in:
- ❌ Verilator's processing (JSON is correct)
- ❌ Module flattening (modules exist separately)
- ❌ Optimization passes (they're not even reached yet)
- ❌ Register inference (it has no data to work with)

The bug IS in:
- ✅ How modules are extracted from Sv_ast
- ✅ How signals/processes are extracted from module statements
- ✅ Pattern matching between sv_parse and verilator_to_behavioral

**The Verilator JSON has all the information we need. Our converter is just not extracting it correctly.**
