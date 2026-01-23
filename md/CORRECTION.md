# HardCaml Backend - Architecture Correction

## What Changed

The HardCaml backend has been corrected to follow the `Input_hardcaml.ml` approach properly.

### ❌ Previous (Incorrect) Approach

The initial implementation **generated OCaml source code** that would need to be compiled separately:

```ocaml
(* Generated .ml file that user would compile *)
module I = struct
  type 'a t = { clock : 'a [@bits 1] } 
  [@@deriving sexp_of, hardcaml]
end

let create scope (i : _ I.t) = ...
```

**Problems:**
- User needs to compile generated OCaml code
- Extra build step required
- Not actually using HardCaml within the backend

### ✅ Current (Correct) Approach  

The backend now **directly uses HardCaml API** to construct circuits:

```ocaml
(* sv_gen_hardcaml.ml - THE BACKEND itself *)
open Hardcaml
open Signal
open Always

let generate_hardcaml_circuit module_name ports items =
  (* Build actual HardCaml circuit *)
  let s = Signal.input "clock" 1 in          (* Real Signal *)
  let v = Variable.wire ~default:(zero 8) in (* Real Variable *)
  let alws = process_always_blocks ... in
  Always.compile alws;                        (* Real compile *)
  
  (* Create actual circuit *)
  Circuit.create_exn ~name:module_name outputs

(* Convert circuit to Verilog *)
let verilog = Rtl.output Verilog circuit
```

**Benefits:**
- Backend constructs circuits directly
- No separate compilation needed
- Outputs Verilog immediately
- Matches `Input_hardcaml.ml` pattern

## Output Format

### Before
- Extension: `.ml`
- Content: OCaml source code
- User action: Compile with HardCaml

### After  
- Extension: `.sv`
- Content: Verilog
- User action: Use directly or re-synthesize

## Usage (Unchanged)

```bash
./sv_main_unified scan hardcaml output/

# Creates: output/decompile_*.sv (Verilog files)
```

## Why This Is Better

| Aspect | Code Generation | Direct Construction |
|--------|----------------|---------------------|
| Compilation | User compiles .ml files | Backend outputs final result |
| Dependencies | User needs HardCaml + ppx | Just run the tool |
| Type Safety | At user compile time | At backend runtime |
| Speed | Generate + compile | Direct conversion |
| Integration | Two-step process | One command |

## Architecture Diagram

### Before (Code Generation)
```
SV JSON → Backend → OCaml Code (.ml) → User Compiles → HardCaml Circuit → Verilog
         (Text Gen)                    (dune build)
```

### After (Direct Construction)
```
SV JSON → Backend → HardCaml Circuit → Verilog (.sv)
         (open Hardcaml)    (in memory)    (Rtl.output)
```

## Build Changes

### dune file
```lisp
(executables
 (libraries 
   hardcaml)  ; Backend depends on HardCaml
)
```

The backend itself links against HardCaml.

## Code Structure

```ocaml
(* sv_gen_hardcaml.ml *)

(* 1. Import HardCaml *)
open Hardcaml
open Signal
open Always

(* 2. Define remap types to track actual HardCaml values *)
type remap =
  | Sig of Signal.t      (* Actual Signal.t *)
  | Var of Variable.t    (* Actual Variable.t *)
  | Sigs of Signed.v     (* Actual Signed.v *)
  | ...

(* 3. Convert AST to HardCaml constructs *)
let expr_to_remap decls expr =
  match expr with
  | ID name -> Hashtbl.find decls name  (* Returns Sig of real Signal *)
  | FN ("ADD", [a; b]) ->
      let lhs = sig' (expr_to_remap decls a) in
      let rhs = sig' (expr_to_remap decls b) in
      Sig (lhs +: rhs)  (* Real HardCaml addition *)

(* 4. Build actual circuit *)
let generate_hardcaml_circuit name ports items =
  let inputs = Signal.input "x" 8 in  (* Real input *)
  let vars = Variable.wire ~default:(zero 8) in
  let alws = process_always_blocks ... in
  Always.compile alws;  (* Real compile *)
  Circuit.create_exn ~name [output "y" result]

(* 5. Convert to Verilog *)
let generate_hardcaml ast indent =
  let circuits = List.map build_circuit modules in
  let verilog = List.map (Rtl.output Verilog) circuits in
  String.concat "\n\n" verilog
```

## Comparison with Input_hardcaml.ml

Both use the same pattern:

### Input_hardcaml.ml
```ocaml
open Hardcaml
open Signal

let cnv (modnam, modul) =
  (* Create actual signals *)
  let s = Signal.input port (hi-lo+1) in
  
  (* Build circuit *)
  let remap' = List.map remap remapp in
  Always.compile remap';
  
  (* Output Verilog *)
  Rtl.output Verilog (Circuit.create_exn ~name:modnam !oplst)
```

### sv_gen_hardcaml.ml (Now)
```ocaml
open Hardcaml
open Signal

let generate_hardcaml_circuit name ports items =
  (* Create actual signals *)
  let s = Signal.input name width in
  
  (* Build circuit *)
  let alws = process_always_blocks items in
  Always.compile alws;
  
  (* Output Verilog *)
  Rtl.output Verilog (Circuit.create_exn ~name outputs)
```

Same approach: **direct HardCaml API usage**.

## Files Changed

1. **`sv_gen_hardcaml.ml`** - Complete rewrite for direct construction
2. **`dune`** - Added `hardcaml` library dependency
3. **`sv_main_unified.ml`** - Changed extension to `.sv` for HardCaml
4. **`HARDCAML_DIRECT.md`** - New guide explaining architecture
5. **`README_UNIFIED.md`** - Updated to clarify output format
6. **`REFACTORING_SUMMARY.md`** - Updated backend table

## Installation

```bash
# Install HardCaml (required for building the backend)
opam install hardcaml

# Build
dune build sv_main_unified.exe

# Use
./sv_main_unified scan hardcaml output/
```

## Summary

The HardCaml backend now:

✅ **Constructs circuits directly** using HardCaml API  
✅ **Outputs Verilog** (`.sv` files)  
✅ **Requires HardCaml** at backend build time  
✅ **Matches `Input_hardcaml.ml`** pattern  
✅ **One-step process** for users  

This is the correct approach as demonstrated in your reference implementation.
