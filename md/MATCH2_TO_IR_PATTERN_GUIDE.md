# match2' to IR Conversion Pattern Guide

## Overview

Instead of generating SystemVerilog strings, we generate IR nodes directly.

**Key Insight**: Copy the proven pattern matching from `rewrite.ml match2'` but replace:
- `Buffer.add_string args.buf " == "` → `add_node ctx (Compare { cmp_op = \`Eq; ... })`

## Pattern Mapping Reference

### From rewrite.ml (lines 169-676)

Here are the key patterns to copy:

### 1. Relations (lines 175-184)

**Original (generates SV string):**
```ocaml
| Triple (VhdEqualRelation, lft, rght) ->
   (match2 args) lft; Buffer.add_string args.buf " == "; (match2 args) rght
```

**Direct IR version:**
```ocaml
| Triple (VhdEqualRelation, lft, rght) ->
    let l_id = expr_to_ir ctx lft in
    let r_id = expr_to_ir ctx rght in
    add_node ctx (Compare { cmp_op = `Eq; width = 32; signed = false }) [l_id; r_id]
```

### 2. Arithmetic (lines 185-188)

**Original:**
```ocaml
| Triple (VhdAddSimpleExpression, lft, rght) ->
    (match2 args) lft; Buffer.add_string args.buf " + "; (match2 args) rght
```

**Direct IR:**
```ocaml
| Triple (VhdAddSimpleExpression, lft, rght) ->
    let l_id = expr_to_ir ctx lft in
    let r_id = expr_to_ir ctx rght in
    add_node ctx (Add { width = 32; signed = true }) [l_id; r_id]
```

### 3. Logical (lines 189-202)

**Original:**
```ocaml
| Triple (VhdAndLogicalExpression, lft, rght) ->
    (match2 args) lft; Buffer.add_string args.buf " && "; (match2 args) rght
```

**Direct IR:**
```ocaml
| Triple (VhdAndLogicalExpression, lft, rght) ->
    let l_id = expr_to_ir ctx lft in
    let r_id = expr_to_ir ctx rght in
    add_node ctx (And { width = 1 }) [l_id; r_id]
```

### 4. Synchronous Process (lines 553-604)

**Original (generates SV always block):**
```ocaml
| Double (VhdConcurrentProcessStatement,
    Sextuple (Vhdprocess_statement, Str process, _,
      Double (VhdSensitivityExpressionList, List [Str clk; Str reset]),
      decls, main_clause))
  when has_clk_and_reset ->
  Buffer.add_string args.buf "always @(posedge clk or posedge reset)\n";
  (* ... generate reset and main logic *)
```

**Direct IR (generates Register nodes):**
```ocaml
| Sextuple (Vhdprocess_statement, Str _process, _,
    Double (VhdSensitivityExpressionList, List dep_lst),
    _decls,
    Double (VhdSequentialIf,
      Quintuple (Vhdif_statement, _,
        (* reset condition *)
        reset_test,
        reset_clause,
        Double (VhdElsif,
          Quintuple (Vhdif_statement, _,
            (* clock edge condition *)
            clk_edge_test,
            main_clause,
            VhdElseNone)))))
  when has_clk_and_reset ->
  (* Extract assignments from main_clause *)
  let assigns = stmt_to_ir ctx main_clause in
  let clk_id = get_signal ctx clk 1 in
  let rst_id = get_signal ctx reset 1 in
  (* Create Register node for each assignment *)
  List.iter (fun (dst_id, data_id) ->
    let _reg_id = add_node ctx
      (Register { width = 32; clock = clk_id; reset = Some rst_id;
                 enable = None; reset_value = 0 })
      [data_id]
  ) assigns
```

### 5. Signal Assignments (lines 264-277)

**Original:**
```ocaml
| Double (VhdSequentialSignalAssignment,
    Double (VhdSimpleSignalAssignment,
      Quintuple (Vhdsimple_signal_assignment_statement,
        Str "", Str nam, VhdDelayNone, expr))) ->
  Buffer.add_string args.buf (nam ^ " <= ");
  match2 args expr;
  Buffer.add_string args.buf ";\n"
```

**Direct IR:**
```ocaml
| Double (VhdSequentialSignalAssignment,
    Double (VhdSimpleSignalAssignment,
      Quintuple (Vhdsimple_signal_assignment_statement,
        Str "", Str nam, VhdDelayNone,
        Double (Vhdwaveform_element, rhs)))) ->
  let rhs_id = expr_to_ir ctx rhs in
  let lhs_id = get_signal ctx nam 32 in
  [(lhs_id, rhs_id)]  (* Return assignment pair *)
```

### 6. Literals (lines 225-227, 244)

**Original:**
```ocaml
| Double (VhdCharPrimary, Char ch) ->
    Buffer.add_string args.buf (" 1'b" ^ String.make 1 ch)

| Double (VhdIntPrimary, Num n) ->
    Buffer.add_string args.buf n
```

**Direct IR:**
```ocaml
| Double (VhdCharPrimary, Char ch) ->
    let value = if ch = '1' then 1 else 0 in
    add_constant ctx value 1

| Double (VhdIntPrimary, Num n) ->
    add_constant ctx (int_of_string n) 32
```

## Complete Pattern Coverage

### Essential Patterns from match2' to Implement

| rewrite.ml Line | Pattern | SV Output | IR Operation |
|-----------------|---------|-----------|--------------|
| 175-184 | Relations | `==`, `!=`, `<`, `>`, `>=` | `Compare { cmp_op; ... }` |
| 185-188 | Arithmetic | `+`, `-` | `Add`, `Sub` |
| 189-196 | Logical/Shifts | `&&`, `\|`, `^`, `<<`, `>>` | `And`, `Or`, `Xor`, `Shift` |
| 197-200 | Parentheses | `(` expr `)` | Pass through |
| 203-204 | Power of 2 | `1 << n` | `Shift` |
| 211-223 | Indexing | `sig[idx]`, `sig[hi:lo]` | `Extract` |
| 225-227 | Char/String literals | `1'b0`, `8'b10101010` | `Constant` |
| 244 | Integer literals | `42` | `Constant` |
| 255-262 | Variable assign | `var := expr` | Assignment pair |
| 264-277 | Signal assign | `sig <= expr` | Assignment pair |
| 553-604 | Sync process | `always @(...)` | `Register` nodes |

## Implementation Strategy

### Step 1: Start with Expressions

Copy all expression patterns from `match2'` (lines 175-244):
- Relations → `Compare`
- Arithmetic → `Add`, `Sub`, `Mul`, `Div`
- Logical → `And`, `Or`, `Xor`, `Not`
- Shifts → `Shift`
- Literals → `Constant`
- Indexing → `Extract`

### Step 2: Add Sequential Statements

Copy statement patterns (lines 255-277):
- Signal assignments → Return `(dst_id, src_id)` pairs
- Variable assignments → Same
- If/Case → Convert to `Mux` or `Pmux`

### Step 3: Add Process Patterns

Copy process patterns (lines 553-676):
- Synchronous with reset → Create `Register` nodes
- Combinational → Wire assignments
- State machines → `Register` + `Mux` trees

### Step 4: Add Architecture Traversal

Iterate through the vhdintf tree to find:
- Entity declarations → Extract ports
- Architecture bodies → Process declarations
- Concurrent statements → Processes, assignments

## Code Structure

```ocaml
(* Context replaces rewrite.ml's match2_args *)
type ir_context = {
  mutable next_id: int;
  signals: (string, value_id * int) Hashtbl.t;
  mutable nodes: (value_id * operation * value_id list) list;
  (* ... *)
}

(* Recursive converter replaces match2' *)
let rec expr_to_ir ctx = function
  | Triple (VhdEqualRelation, lft, rght) ->
      let l_id = expr_to_ir ctx lft in
      let r_id = expr_to_ir ctx rght in
      add_node ctx (Compare { cmp_op = `Eq; ... }) [l_id; r_id]
  (* ... copy all other patterns from match2' ... *)

(* Statement converter *)
let rec stmt_to_ir ctx = function
  | Double (VhdSequentialSignalAssignment, ...) ->
      (* Return assignment pairs *)
  (* ... *)

(* Process converter *)
let process_to_ir ctx = function
  | Sextuple (Vhdprocess_statement, ...) ->
      (* Create Register nodes *)
  (* ... *)
```

## Key Differences from rewrite.ml

| rewrite.ml | vhdl_to_ir_direct.ml |
|------------|---------------------|
| `Buffer.add_string` | `add_node ctx` |
| `match2_args` context | `ir_context` |
| Recursive `match2` | Recursive `expr_to_ir` |
| Returns `unit` | Returns `value_id` |
| Builds strings | Builds IR graph |

## Testing Strategy

1. **Test expressions first**: Simple arithmetic, comparisons
2. **Test assignments**: Single signal assignments
3. **Test processes**: Simple combinational, then registered
4. **Test full modules**: Complete VHDL designs

## Next Steps

1. ✅ Copy proven patterns from `match2'`
2. ✅ Replace string generation with IR creation
3. ⏳ Complete all pattern matches (currently ~30% done)
4. ⏳ Add tree traversal to find all processes
5. ⏳ Test on simple VHDL modules
6. ⏳ Expand to full UART suite

## Advantages of This Approach

- ✅ Reuses 100% proven patterns
- ✅ Direct VHDL → IR (no intermediate parsing)
- ✅ Type-safe IR generation
- ✅ Easier to optimize (IR graph vs strings)
- ✅ Same pattern coverage as rewrite.ml
