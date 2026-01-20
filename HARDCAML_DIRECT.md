# HardCaml Backend - Direct Circuit Construction

## Overview

The HardCaml backend **constructs HardCaml circuits directly** within `sv_gen_hardcaml.ml`, following the `Input_hardcaml.ml` approach. It does NOT generate OCaml source code - instead, it:

1. **Links against HardCaml** - The backend itself depends on the hardcaml library
2. **Creates circuits in memory** - Builds actual `Circuit.t` values
3. **Outputs Verilog** - Converts circuits to Verilog using `Rtl.output`

## Key Difference from Code Generation

### ❌ NOT This (Code Generation):
```ocaml
(* Backend generates text that looks like: *)
let output = 
  "let circuit = Signal.input \"a\" 8 in\n" ^
  "Signal.output \"out\" circuit"
```

### ✅ This (Direct Construction):
```ocaml
(* Backend directly calls HardCaml API: *)
open Hardcaml
open Signal

let circuit = 
  let a = Signal.input "a" 8 in
  [Signal.output "out" a]
```

## Architecture

### Backend Structure

```ocaml
(* sv_gen_hardcaml.ml *)
open Sv_ast
open Hardcaml      (* Backend imports HardCaml *)
open Signal
open Always

(* Remap types track HardCaml values *)
type remap =
  | Sig of Signal.t           (* Actual Signal.t value *)
  | Var of Variable.t         (* Actual Variable.t value *)
  | Sigs of Signal.Signed.v   (* Actual Signed.v value *)
  | Con of Constant.t         (* Actual Constant.t value *)
  | Alw of Always.t           (* Actual Always.t value *)
  | Invalid

(* Convert expressions to actual Signal values *)
let rec expr_to_remap decls = function
  | SCALAR (n, w) -> Con (Constant.of_int ~width:w n)
  | ID name -> Hashtbl.find decls name  (* Returns Sig of actual Signal *)
  | BITSEL (e, idx) ->
      let expr = expr_to_remap decls e in
      Sig (bit (sig' expr) idx)  (* Actual Signal.bit call *)
  | FN ("ADD", [a; b]) ->
      let lhs = sig' (expr_to_remap decls a) in
      let rhs = sig' (expr_to_remap decls b) in
      Sig (lhs +: rhs)  (* Actual Signal addition *)
  (* ... *)

(* Build actual HardCaml circuit *)
let generate_hardcaml_circuit module_name ports items =
  let decls = Hashtbl.create 64 in
  
  (* Create real Signal.input values *)
  List.iter (fun (name, width, _) ->
    let s = Signal.input name width in  (* Real HardCaml call *)
    Hashtbl.add decls name (Sig s)
  ) inputs;
  
  (* Create real Variables *)
  List.iter (fun item ->
    match item with
    | IVAR (_, name, width, _, _) ->
        let v = Variable.wire ~default:(Signal.zero width) in
        Hashtbl.add decls name (Var v)
    | _ -> ()
  ) items;
  
  (* Process always blocks *)
  let alws = process_always_blocks decls items in
  Always.compile alws;  (* Real compile call *)
  
  (* Create real circuit *)
  let outputs = build_outputs decls outputs in
  Circuit.create_exn ~name:module_name outputs  (* Returns Circuit.t *)
```

## Data Flow

```
SystemVerilog JSON
       ↓
   sv_parse.ml (parse to Sv_ast)
       ↓
   sv_gen_hardcaml.ml
       ↓
   [Builds actual HardCaml circuits in memory]
   - Signal.input creates real input signals
   - Variable.wire creates real wires
   - Always.compile creates real always blocks
   - Circuit.create_exn creates real circuits
       ↓
   Rtl.output Verilog circuit
       ↓
   Verilog output string
```

## Why This Approach?

### Benefits

1. **Type Safety** - OCaml type system ensures correctness
2. **No Parsing** - No need to parse generated code
3. **Efficiency** - Direct construction, no text manipulation
4. **Composability** - Can use circuits in other backends
5. **Testing** - Can unit test circuit construction directly

### Example: Direct vs. Generated

**Generated Approach:**
```ocaml
(* Generate text *)
let code = Printf.sprintf 
  "let a = Signal.input \"a\" %d in\n\
   let b = Signal.input \"b\" %d in\n\
   Signal.output \"sum\" (a +: b)"
  width_a width_b

(* User must compile this text separately *)
```

**Direct Approach (Our Method):**
```ocaml
(* Build circuit directly *)
let a = Signal.input "a" width_a in
let b = Signal.input "b" width_b in
let circuit = Circuit.create_exn ~name:"adder"
  [Signal.output "sum" (a +: b)]

(* Circuit is ready to use immediately *)
let verilog = Rtl.output Verilog circuit
```

## Implementation Details

### Signal Tracking

```ocaml
(* Hashtable maps names to actual HardCaml values *)
let decls : (string, remap) Hashtbl.t = Hashtbl.create 64

(* Store actual Signal *)
let s = Signal.input "data" 8 in
Hashtbl.add decls "data" (Sig s)

(* Retrieve and use *)
let data_sig = match Hashtbl.find decls "data" with
  | Sig s -> s
  | _ -> failwith "not a signal"

let result = data_sig +: Signal.of_int ~width:8 1
```

### Always Block Compilation

```ocaml
(* Process statements into real Always.t values *)
let rec process_stmt decls = function
  | ASSIGN (ID lhs, rhs) ->
      let lhs_var = var' (Hashtbl.find decls lhs) in
      let rhs_sig = sig' (expr_to_remap decls rhs) in
      Alw (lhs_var <-- rhs_sig)  (* Real Always assignment *)
  
  | IF (cond, then_stmts, else_stmts) ->
      let cond_sig = sig' (expr_to_remap decls cond) in
      let then_alws = List.map (process_stmt decls) then_stmts in
      let else_alws = List.map (process_stmt decls) else_stmts in
      Alw (if_ cond_sig then_alws else_alws)  (* Real if_ call *)

(* Compile all always blocks *)
let alws = List.map (process_stmt decls) stmts in
Always.compile alws  (* Real compile *)
```

### Circuit Output

```ocaml
(* Build output list from actual signals *)
let build_outputs decls outputs =
  List.filter_map (fun (name, _, _) ->
    match Hashtbl.find decls name with
    | Var v -> Some (Signal.output name v.value)  (* Real output *)
    | Sig s -> Some (Signal.output name s)
    | _ -> None
  ) outputs

(* Create circuit *)
let circuit = Circuit.create_exn ~name:"top" output_list

(* Convert to Verilog *)
let verilog = Rtl.output Verilog circuit
```

## Build Requirements

### dune Configuration

```lisp
(executables
 (names sv_main_unified)
 (libraries 
   str 
   yojson 
   unix 
   hardcaml)  ; Required for HardCaml backend
)
```

### Installation

```bash
# Install HardCaml
opam install hardcaml

# Build decompiler (HardCaml backend included)
dune build sv_main_unified.exe
```

## Output Format

The backend outputs **Verilog**, not OCaml source code:

```bash
# Input: SystemVerilog JSON from Verilator
# Output: Verilog (via HardCaml circuit construction)
./sv_main_unified scan hardcaml output/

# Result: output/decompile_*.sv (Verilog files)
```

The Verilog is generated by:
1. Parsing SV JSON → AST
2. Building HardCaml circuit from AST
3. Converting HardCaml circuit → Verilog with `Rtl.output`

## Advantages Over Code Generation

| Aspect | Code Generation | Direct Construction |
|--------|----------------|---------------------|
| Type Safety | ❌ Text, no checking | ✅ Full OCaml types |
| Speed | ❌ Text manipulation | ✅ Direct construction |
| Debugging | ❌ Generated code | ✅ Backend code |
| Composability | ❌ Must parse | ✅ Use Circuit.t directly |
| Dependencies | ❌ User needs HardCaml | ✅ Backend includes it |

## Usage Example

```bash
# SystemVerilog source
cat counter.sv
module counter(
  input wire clock,
  output reg [7:0] count
);
  always @(posedge clock)
    count <= count + 1;
endmodule

# Generate JSON with Verilator
verilator --xml-only counter.sv

# Backend constructs HardCaml circuit internally:
# 1. Creates Signal.input "clock" 1
# 2. Creates Variable.reg for count
# 3. Creates Always.compile block
# 4. Builds Circuit.create_exn
# 5. Outputs Rtl.output Verilog circuit

./sv_main_unified scan hardcaml output/

# Result: output/decompile_counter.sv
cat output/decompile_counter.sv
module counter (
  clock,
  count
);
  input clock;
  output [7:0] count;
  
  wire [7:0] _count;
  
  always @(posedge clock) begin
    _count <= _count + 8'd1;
  end
  
  assign count = _count;
endmodule
```

## Comparison with Input_hardcaml.ml

Your reference implementation `Input_hardcaml.ml` works similarly:

```ocaml
(* Input_hardcaml.ml approach *)
open Hardcaml
open Signal
open Always

(* Direct construction *)
let cnv (modnam, modul) =
  let declare_lst = ref [] in
  
  (* Create actual inputs *)
  let declare_input port = function
    | Width(hi,lo,signed) ->
      let s = Signal.input port (hi-lo+1) in
      add_decl port (if signed then Sigs (Signed.of_signal s) else Sig s)
  
  (* Build actual circuit *)
  let remapp = List.map (tranitm attr) !(modul.alwys) in
  let remap' = List.map remap remapp in
  let remap'' = List.map alw' remap' in
  Always.compile remap'';
  
  (* Output Verilog *)
  let rtl = Buffer.create 65535 in
  Hardcaml.Rtl.output ~output_mode:(To_buffer rtl) Verilog 
    (Circuit.create_exn ~name:modnam !oplst);
  Buffer.contents rtl
```

Our backend follows the same pattern: **direct HardCaml API usage**.

## Summary

The HardCaml backend:

1. ✅ **Links against HardCaml** - `open Hardcaml` in the backend
2. ✅ **Constructs circuits directly** - Real `Signal.t`, `Variable.t` values
3. ✅ **Outputs Verilog** - Via `Rtl.output Verilog circuit`
4. ❌ **Does NOT generate OCaml code** - No text generation

This matches the `Input_hardcaml.ml` approach of direct API usage.
