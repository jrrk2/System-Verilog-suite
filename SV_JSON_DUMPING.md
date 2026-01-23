# SystemVerilog JSON Pattern Dumping

## Overview

Added JSON annotation support to the Verible SystemVerilog front-end to enable automatic dumping of unhandled patterns during IR conversion. This mirrors the VHDL JSON dumping functionality and helps identify patterns that need to be implemented.

## Implementation

### 1. Source_text_verible_json.ml

Created a wrapper module that re-exports the menhir-generated `token` type with `[@@deriving yojson]` annotation. This avoids conflicts with menhir's auto-generation while adding JSON serialization support.

```ocaml
type token = Source_text_verible.token =
  | SymbolIdentifier of (string)
  | TUPLE2 of (token*token)
  | TUPLE3 of (token*token*token)
  (* ... all 500+ token constructors ... *)
[@@deriving yojson]
```

**Why this approach:**
- Menhir auto-generates Source_text_verible.mli, so we can't directly modify it
- Creating a manual .mli would conflict with dune's menhir rules
- The wrapper module imports and re-exports with deriving annotation
- ppx_deriving_yojson generates `token_to_yojson` and `token_of_yojson` functions

### 2. sv_dump_json.ml

Utility module for dumping unhandled SystemVerilog patterns:

```ocaml
(* Compact description for logging *)
val get_description : ?depth:int -> token -> string

(* Dump unhandled pattern to JSON file *)
val dump_unhandled : string -> string -> token -> unit

(* Create summary of all dumped patterns *)
val create_summary : unit -> unit
```

**Features:**
- Uses auto-generated `token_to_yojson` for serialization
- Creates timestamped JSON files in `unhandled_sv/` directory
- Provides compact descriptions for console output
- Generates summary statistics by pattern type

### 3. Integration with sv_verible_to_ir.ml

Updated the expression handler to dump unhandled patterns:

```ocaml
(* Before *)
| _ ->
    Printf.eprintf "\n=== Warning: Unhandled expression type ===\n";
    let oc = open_out_gen [...] "unhandled_tokens.txt" in
    Printf.fprintf oc "%s\n" (token_to_json_string ~max_depth:5 0 expr);
    close_out oc;
    Sv_opt_ir.get_new_id ir

(* After *)
| _ ->
    Printf.eprintf "\n=== Warning: Unhandled expression type ===\n";
    Sv_dump_json.dump_unhandled "expression" "expr" expr;
    Sv_opt_ir.get_new_id ir
```

## Usage

### Test Example

```ocaml
open Source_text_verible_json
open Sv_dump_json

let test_token = TUPLE3 (
  STRING "binary_add",
  SymbolIdentifier "a",
  SymbolIdentifier "b"
)

let () =
  dump_unhandled "test" "tuple3" test_token;
  create_summary ()
```

**Output:**
```
Dumped unhandled tuple3 pattern to unhandled_sv/test_tuple3_1769163535.json
  Description: TUPLE3("binary_add",...)

=== Unhandled SystemVerilog Patterns Summary ===
Total unhandled patterns: 1

By pattern type:
  tuple3: 1
```

### Generated JSON Structure

```json
{
  "context": "test",
  "pattern_type": "tuple3",
  "timestamp": 1769163535,
  "pattern": [
    "TUPLE3",
    [
      [ "STRING", "binary_add" ],
      [ "SymbolIdentifier", "a" ],
      [ "SymbolIdentifier", "b" ]
    ]
  ]
}
```

## Benefits

1. **Systematic Pattern Discovery**
   - JSON files capture exact structure of unhandled patterns
   - Easy to inspect and analyze with JSON tools
   - Can be shared and reviewed without running the code

2. **Parallel to VHDL Approach**
   - Same workflow for both front-ends
   - Consistent debugging experience
   - Shared understanding of pattern completion

3. **No Parser Modification**
   - Wrapper module approach avoids menhir conflicts
   - No need to maintain custom .mli files
   - Clean separation from generated code

4. **Automated Summary**
   - Quick overview of unhandled pattern types
   - Identifies which patterns are most common
   - Helps prioritize implementation work

## File Organization

```
unhandled_sv/
├── expression_expr_1769163535.json       # From sv_verible_to_ir.ml
├── test_tuple3_1769163535.json           # From test programs
└── ...
```

Each file is named: `{context}_{pattern_type}_{timestamp}.json`

## Comparison to VHDL JSON Dumping

| Feature | VHDL (vhdl_dump_json.ml) | SystemVerilog (sv_dump_json.ml) |
|---------|--------------------------|----------------------------------|
| **Type Source** | VhdlTree.ml manual type | Menhir-generated token type |
| **Annotation** | Direct on vhdintf type | Via wrapper module |
| **JSON Library** | ppx_deriving_yojson | ppx_deriving_yojson |
| **Directory** | unhandled_vhdl/ | unhandled_sv/ |
| **Max Depth** | 5 levels | Auto (full tree) |

Both use the same overall approach but adapted to their respective parser technologies.

## Future Enhancements

1. **Pattern Matching Hints**
   - Suggest OCaml pattern based on JSON structure
   - Generate skeleton match cases

2. **Diff Tool**
   - Compare unhandled patterns across versions
   - Track progress on pattern implementation

3. **Interactive Viewer**
   - Web-based JSON pattern browser
   - Search and filter capabilities

## Testing

Run the test suite:
```bash
dune exec ./test_sv_json_dump.exe
```

This creates sample patterns and verifies JSON generation works correctly.

## Summary

Successfully added JSON annotation to Verible front-end using:
- ✅ Wrapper module approach (Source_text_verible_json.ml)
- ✅ Auto-generated yojson serialization (ppx_deriving_yojson)
- ✅ Integration with IR converter (sv_verible_to_ir.ml)
- ✅ Test program demonstrating functionality
- ✅ No conflicts with menhir auto-generation

The solution elegantly handles the menhir constraint and provides the same debugging capabilities as the VHDL front-end.
