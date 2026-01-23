# HardCaml Backend - Implementation Status

## Current Status: Placeholder

The HardCaml backend is currently implemented as a **placeholder** that uses the standard SystemVerilog generator. This ensures the unified CLI compiles and works while demonstrating the intended architecture.

## Why Placeholder?

The full HardCaml implementation requires deep integration with the SystemVerilog AST types defined in `sv_ast.mli`, which uses complex recursive node types:

```ocaml
type sv_node = 
  | Module of { name: string; stmts: sv_node list }
  | Var of { name: string; dtype_ref: sv_type option; direction: string; ... }
  | AlwaysFF of { ... }
  (* Many more variants *)
```

Converting this to HardCaml's `Signal.t`, `Variable.t`, and `Always.t` types requires:
1. Full AST traversal with pattern matching on all node types
2. Type resolution for width/signedness
3. Expression evaluation
4. Always block transformation
5. Module hierarchy handling

## Current Implementation

```ocaml
(* sv_gen_hardcaml.ml - Placeholder *)

let generate_hardcaml_simple ast indent =
  let warnings = ["HardCaml backend: Using standard generator..."] in
  let sv_output = Sv_gen.generate_sv ast in
  let header = "(* Generated via HardCaml backend (placeholder) *)\n\n" in
  (header ^ sv_output, warnings)
```

## Planned Full Implementation

### Phase 1: Port Extraction
```ocaml
let extract_ports stmts =
  List.filter_map (function
    | Var { name; dtype_ref; direction = "input"; ... } ->
        let width = extract_width dtype_ref in
        Some (name, width, Input)
    | Var { name; dtype_ref; direction = "output"; ... } ->
        let width = extract_width dtype_ref in
        Some (name, width, Output)
    | _ -> None
  ) stmts
```

### Phase 2: Signal Declaration
```ocaml
open Hardcaml
open Signal

let create_declarations ports =
  let decls = Hashtbl.create 64 in
  
  (* Create inputs *)
  List.iter (fun (name, width, Input) ->
    let s = Signal.input name width in
    Hashtbl.add decls name (Sig s)
  ) (filter_inputs ports);
  
  (* Create wires/regs *)
  List.iter (fun (name, width) ->
    let v = Variable.wire ~default:(zero width) in
    Hashtbl.add decls name (Var v)
  ) internal_signals;
  
  decls
```

### Phase 3: Expression Translation
```ocaml
let rec translate_expr decls = function
  | Identifier name -> Hashtbl.find decls name
  | BinOp (Add, a, b) ->
      let lhs = sig_of (translate_expr decls a) in
      let rhs = sig_of (translate_expr decls b) in
      Sig (lhs +: rhs)
  | Select (expr, hi, lo) ->
      let e = sig_of (translate_expr decls expr) in
      Sig (select e lo (hi - lo + 1))
  (* ... *)
```

### Phase 4: Always Block Compilation
```ocaml
let translate_always decls = function
  | AlwaysComb stmts ->
      let alws = List.map (translate_stmt decls) stmts in
      Always.compile alws
      
  | AlwaysFF { clock; reset; stmts } ->
      let spec = Reg_spec.create 
        ~clock:(find_sig decls clock)
        ~reset:(find_sig decls reset) () in
      let alws = List.map (translate_seq_stmt decls spec) stmts in
      Always.compile alws
```

### Phase 5: Circuit Construction
```ocaml
let build_circuit module_name ports stmts =
  let decls = create_declarations ports in
  
  (* Process always blocks *)
  List.iter (function
    | AlwaysComb _ | AlwaysFF _ as alw ->
        translate_always decls alw
    | _ -> ()
  ) stmts;
  
  (* Build outputs *)
  let outputs = List.filter_map (fun (name, _, Output) ->
    match Hashtbl.find decls name with
    | Sig s -> Some (output name s)
    | Var v -> Some (output name v.value)
    | _ -> None
  ) ports in
  
  Circuit.create_exn ~name:module_name outputs
```

### Phase 6: Verilog Output
```ocaml
let generate_hardcaml ast indent =
  let circuits = List.filter_map (function
    | Module { name; stmts } ->
        let ports = extract_ports stmts in
        Some (build_circuit name ports stmts)
    | _ -> None
  ) ast in
  
  let verilog = List.map (fun c ->
    Rtl.output Verilog c
  ) circuits in
  
  String.concat "\n\n" verilog
```

## Integration Requirements

### Build Dependencies
```lisp
; dune file
(executables
 (libraries 
   str 
   yojson 
   unix 
   hardcaml)  ; Add this
)
```

### Installation
```bash
opam install hardcaml
```

## Challenges

1. **Type System Complexity**
   - `sv_type` with RefType/ArrayType/StructType/etc
   - Width inference across operations
   - Signed vs unsigned handling

2. **AST Variants**
   - 50+ `sv_node` variants to handle
   - Nested structures (modules, functions, generate blocks)
   - Interface/modport resolution

3. **Always Block Semantics**
   - Blocking vs non-blocking assignments
   - Sensitivity lists
   - Edge detection (posedge/negedge)
   - Multiple clocks/resets

4. **Expression Complexity**
   - Concatenations, replications
   - Part selects, bit selects
   - System functions ($signed, $unsigned, etc)
   - Ternary operators, case expressions

## Current Usage

The placeholder backend works identically to the standard backend:

```bash
./sv_main_unified scan hardcaml output/
# Outputs: output/decompile_*.sv (using standard generator)
# Warning: "HardCaml backend: Using standard generator as placeholder"
```

## Path to Full Implementation

### Step 1: Extract Type Information
Create helper functions to extract widths and signedness from `sv_type`:

```ocaml
val extract_width : sv_type option -> int
val extract_signedness : sv_type option -> bool
val resolve_type : sv_type option -> string
```

### Step 2: Port Processing
Extract ports from module statements and categorize as input/output/inout:

```ocaml
val extract_ports : sv_node list -> (string * int * direction) list
```

### Step 3: Expression Translator
Build the core expression to `Signal.t` translator:

```ocaml
val translate_expr : (string, remap) Hashtbl.t -> sv_node -> remap
```

### Step 4: Statement Translator
Translate assignments and control flow to `Always.t`:

```ocaml
val translate_stmt : (string, remap) Hashtbl.t -> sv_node -> Always.t
```

### Step 5: Circuit Builder
Combine all pieces to build actual `Circuit.t`:

```ocaml
val build_module : string -> sv_node list -> Circuit.t
```

### Step 6: Replace Placeholder
Replace `Sv_gen.generate_sv ast` with `build_and_output_circuits ast`

## Comparison with Input_hardcaml.ml

Your reference implementation has similar structure but works with a different AST:

```ocaml
(* Input_hardcaml.ml works with Input.rw types *)
type rw =
  | VRF of string * typetable_t * rw list
  | ASGN of bool * string * rw list
  | ARITH of arithop * rw list
  (* ... *)

(* Our AST uses sv_node *)
type sv_node =
  | Var of { name; dtype_ref; direction; ... }
  | AssignStmt of { lhs; rhs; ... }
  | BinOp of { op; left; right; ... }
  (* ... *)
```

Both need to:
1. Track signals in hashtable
2. Convert expressions to Signal.t
3. Build Always.t for procedural blocks
4. Compile with Always.compile
5. Create Circuit.t
6. Output with Rtl.output Verilog

## Alternative: Simplified AST

An alternative approach would be to add a transformation pass:

```ocaml
(* sv_transform_hardcaml.ml *)
val simplify_for_hardcaml : sv_node -> simplified_ast
```

This would convert the complex `sv_node` types to simpler types more amenable to HardCaml translation, similar to how Input_hardcaml.ml works with the `rw` type.

## Conclusion

The placeholder demonstrates the intended architecture while allowing the unified tool to compile and work. Full implementation requires significant effort to:

1. Navigate the complex `sv_node` AST
2. Extract type/width information
3. Translate expressions and statements
4. Build actual HardCaml circuits
5. Handle edge cases

This work is valuable but requires dedicated time to implement properly. The placeholder ensures the tool is usable while this work progresses.

## Contributing

If you want to contribute to the full HardCaml backend implementation:

1. Start with port extraction (`extract_ports`)
2. Add simple expression translation (identifiers, constants)
3. Gradually add operators
4. Add always block support
5. Test incrementally with simple modules

Each phase can be tested independently before moving to the next.
