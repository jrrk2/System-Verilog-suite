(* Slang AST JSON → Behavioral IR.
 *
 * Slang (https://github.com/MikePopoloski/slang) is an independent
 * SystemVerilog elaborator. Its `slang --ast-json` driver dumps the
 * fully-elaborated AST as JSON: parameters resolved, generates
 * unrolled, types canonicalised, expressions kept in source-shape.
 * This module parses that JSON into the same Behavioral_ir.bprogram
 * the Verible/Verilator/Yosys frontends emit.
 *
 * Why a third independent SV reader: the goal is a canonical
 * elaborated-but-unoptimised reference netlist for the miter, with
 * no Xilinx-specific tricks (`$aldff`, RTL_REG_anything) and no Yosys
 * transformations (`proc;flatten` introducing `$buf` collectors,
 * anonymous `$Ny` intermediate wires). If Verible-BIR and Slang-BIR
 * agree, we have proof we understood the source the same way. *)

open Behavioral_ir

(* ─── JSON helpers ───────────────────────────────────────────────── *)

type json = Yojson.Safe.t

(* Slang's JSON output prints each unique InstanceBody only once. Repeat
 * uses (e.g. left_child / right_child of a recursive popcount) appear
 * as `"body": "<addr> <name>"` strings — back-references to the first
 * full occurrence keyed by `"addr"`. We pre-scan the whole tree once
 * to build the address table so `extract_instances` can resolve those
 * references back to a full InstanceBody node. *)
let body_by_addr : (string, Yojson.Safe.t) Hashtbl.t = Hashtbl.create 64

(* Enum-typedef name → base-type string (e.g. "state_type" →
 * "logic[3:0]").  Slang prints an enum-typed signal's `type` as
 * "<id> [scope.]typename" rather than the resolved width, so parse_type
 * resolves the typename through this table.  Populated by
 * build_enum_table from every EnumType node before conversion. *)
let enum_base_tbl : (string, string) Hashtbl.t = Hashtbl.create 32

(* Parameter symbol address → (resolved value string, type string).
 * Slang prints `Parameter` members with `addr` + `value` for any
 * specialised instance. NamedValue references to those symbols don't
 * always carry a `constant` annotation (only when slang's evaluator
 * is sure of the context); folding via this table lets us resolve
 * them anyway. *)
let param_value_by_addr : (string, string * string) Hashtbl.t =
  Hashtbl.create 64

let assoc k (j : json) =
  match j with
  | `Assoc xs -> List.assoc_opt k xs
  | _ -> None

let str_field k j =
  match assoc k j with
  | Some (`String s) -> Some s
  | _ -> None

let list_field k j =
  match assoc k j with
  | Some (`List xs) -> xs
  | _ -> []

let kind_of j = match str_field "kind" j with Some s -> s | None -> ""

let name_of j = match str_field "name" j with Some s -> s | None -> ""

(* ─── Symbol address ↔ name table ────────────────────────────────── *)

(* Slang's NamedValue references a symbol via a string of the form
 * "<addr> <name>". We don't actually need the addr — the name is
 * sufficient because Slang elaborates duplicate-named variables
 * out of scope. But the format also distinguishes
 * `<scope>.<name>`, which matters for hierarchical references. *)
let strip_addr s =
  match String.index_opt s ' ' with
  | Some i -> String.sub s (i + 1) (String.length s - i - 1)
  | None -> s

(* ─── Type → width ────────────────────────────────────────────────── *)

(* Slang's "type" strings look like "logic", "logic[3:0]", "int",
 * "logic[63:0][7:0]"          (2-D packed),
 * "reg[7:0]$[0:63]"           (unpacked array of packed elements),
 * "shortint", …               Returns (total_bits, elem_bits, ndims).
 *
 * Slang uses '$' as a packed/unpacked separator: anything BEFORE '$'
 * is the (packed) element type's dimensions, anything AFTER is the
 * unpacked array dimensions.  Without the separator we used to read
 * `reg[7:0]$[0:63]` as `8 × 64` (8 elements of 64 bits), exactly
 * inverted — slib_fifo.sv's iFIFOMem turned into arr[8]bv64 instead
 * of arr[64]bv8. *)
let rec parse_type s =
  let s = String.trim s in
  (* Resolve an enum-typedef name to its base type.  Slang emits an
     enum-typed signal's type as "<id> [scope.]typename"; map the bare
     typename to its EnumType baseType (logic[3:0] etc.) and re-parse,
     so `state_type` sizes to 4 bits instead of defaulting to 1. *)
  (match
     let tok = match List.rev (String.split_on_char ' ' s) with
       | t :: _ -> t | [] -> s in
     let name = match List.rev (String.split_on_char '.' tok) with
       | n :: _ -> n | [] -> tok in
     (* Qualified name (module.typedef) first — disambiguates same-named
        typedefs across modules — then the bare name as fallback. *)
     (match Hashtbl.find_opt enum_base_tbl tok with
      | Some _ as r -> r
      | None -> Hashtbl.find_opt enum_base_tbl name)
   with
   | Some base when base <> s -> parse_type base
   | _ ->
  if s = "logic" || s = "bit" || s = "reg" then (1, 1, 0)
  else if s = "int" || s = "integer" || s = "shortint"
       || s = "longint" then (32, 32, 0)
  else if s = "byte" then (8, 8, 0)
  else
    let parse_dims str =
      let n = String.length str in
      let dims = ref [] in
      let i = ref 0 in
      while !i < n do
        if str.[!i] = '[' then begin
          let j = ref (!i + 1) in
          while !j < n && str.[!j] <> ']' do incr j done;
          if !j < n then begin
            let body = String.sub str (!i + 1) (!j - !i - 1) in
            (match String.split_on_char ':' body with
             | [a; b] ->
                 (try
                    let hi = int_of_string (String.trim a) in
                    let lo = int_of_string (String.trim b) in
                    dims := (abs (hi - lo) + 1) :: !dims
                  with _ -> ())
             | _ -> ());
            i := !j + 1
          end else incr i
        end else incr i
      done;
      List.rev !dims in
    let packed_str, unpacked_str =
      match String.split_on_char '$' s with
      | [p; u] -> p, u
      | _      -> s, "" in
    let pds = parse_dims packed_str in
    let uds = parse_dims unpacked_str in
    let prod = List.fold_left ( * ) 1 in
    if uds <> [] then
      (* Unpacked array — element type given by packed dims, array
         size given by unpacked dims (product when nested). *)
      let elem = if pds = [] then 1 else prod pds in
      let size = prod uds in
      (elem * size, elem, List.length pds + List.length uds)
    else
      match pds with
      | []           -> (1, 1, 0)
      | [w]          -> (w, w, 1)
      | outer :: inner :: _ ->
          (* 2-D packed: outer × inner total, inner is the element. *)
          (outer * inner, inner, List.length pds))

(* ─── Operators ──────────────────────────────────────────────────── *)

let bop_of_string = function
  | "Add" -> Some BAdd
  | "Subtract" -> Some BSub
  | "Multiply" -> Some BMul
  | "Divide" -> Some BDiv
  | "Mod" -> Some BMod
  | "BinaryAnd" | "LogicalAnd" -> Some BAnd
  | "BinaryOr"  | "LogicalOr"  -> Some BOr
  | "BinaryXor" -> Some BXor
  | "Equality" | "CaseEquality" -> Some BEq
  | "Inequality" | "CaseInequality" -> Some BNe
  | "LessThan" -> Some BLt
  | "LessThanEqual" -> Some BLe
  | "GreaterThan" -> Some BGt
  | "GreaterThanEqual" -> Some BGe
  | "LogicalShiftLeft" | "ArithmeticShiftLeft" -> Some BShl
  | "LogicalShiftRight" -> Some BShr
  | "ArithmeticShiftRight" -> Some BAshr
  | _ -> None

(* `LogicalNot` is NOT in this table — it's a 1-bit OR-reduction
   followed by a NOT, which can't be expressed as a single unary op.
   The UnaryOp case below special-cases it.  Treating LogicalNot as
   plain BitwiseNot is wrong: `!8'b00000001` is 1'b0, not 8'b11111110. *)
let uop_of_string = function
  | "BitwiseNot" -> Some BNot
  | "Minus" -> Some BNeg
  | "BitwiseAnd" -> Some BRedAnd
  | "BitwiseOr"  -> Some BRedOr
  | "BitwiseXor" -> Some BRedXor
  | _ -> None

(* ─── Constant-literal parsing ───────────────────────────────────── *)

(* Slang's IntegerLiteral has fields `value` (decimal string) and
 * sometimes a typed shape like `4'd0`, `4'b1010`. We accept either. *)
let parse_const value_str type_str =
  let s = String.trim value_str in
  let total_w =
    let w, _, _ = parse_type type_str in w
  in
  if String.contains s '\'' then begin
    (* SV-style literal: <width>'<base><digits>. *)
    match String.split_on_char '\'' s with
    | [w_s; rest] when String.length rest >= 1 ->
        let w =
          try int_of_string w_s with _ -> total_w
        in
        let v = match rest.[0] with
          | 'b' | 'B' ->
              (try int_of_string ("0b" ^ String.sub rest 1
                                   (String.length rest - 1))
               with _ -> 0)
          | 'h' | 'H' ->
              (try int_of_string ("0x" ^ String.sub rest 1
                                   (String.length rest - 1))
               with _ -> 0)
          | 'd' | 'D' ->
              (try int_of_string (String.sub rest 1
                                    (String.length rest - 1))
               with _ -> 0)
          | _ -> (try int_of_string rest with _ -> 0)
        in
        BConst { value = Z.of_int v; width = w }
    | _ -> BConst { value = (try Z.of_string s with _ -> Z.zero);
                    width = total_w }
  end else
    BConst { value = (try Z.of_string s with _ -> Z.zero);
             width = total_w }

(* ─── Expressions ────────────────────────────────────────────────── *)

(* Slang expands compound assignment `c op= e` into
     Assignment { left = c; op = Op; right = BinaryOp { left = LValueReference; op = Op; right = e } }
   The `LValueReference` in the BinaryOp's left is a placeholder for
   "the current value of the LHS".  We resolve it via this mutable
   ref, set by the Assignment-statement handler before converting the
   RHS and restored afterwards.                                       *)
let lvalue_ref_ctx : Behavioral_ir.bexpr option ref = ref None

(* Function-body lowering context.  Slang emits `return expr;` as a
   `Return { expr }` node, but the BIR convention (matching verilator
   and downstream passes) is for a function's return value to be
   assigned to a variable named the same as the function.  This ref
   tells the Return-statement handler which name to assign to. *)
let current_function_name : string option ref = ref None

let rec expr_to_bexpr j =
  match kind_of j with
  | "LValueReference" ->
      (match !lvalue_ref_ctx with
       | Some e -> e
       | None -> BConst { value = Z.zero; width = 1 })
  | "NamedValue" ->
      (* Resolve in priority order:
       *   1. Inline `constant` annotation (slang's evaluator certain).
       *   2. Symbol address → Parameter value table (any specialised
       *      parameter has a known value, even if slang didn't fold
       *      the inline NamedValue — covers cva6 lfsr's RstVal etc.).
       *   3. Fall back to BVar for genuine signal references. *)
      (match str_field "constant" j with
       | Some s ->
           let t = match str_field "type" j with
             | Some t -> t | None -> "int" in
           parse_const s t
       | None ->
           let raw_sym = match str_field "symbol" j with
             | Some s -> s | None -> "?" in
           let addr = match String.index_opt raw_sym ' ' with
             | Some i -> String.sub raw_sym 0 i
             | None -> "" in
           match Hashtbl.find_opt param_value_by_addr addr with
           | Some (v, t) -> parse_const v t
           | None -> BVar (strip_addr raw_sym))
  | "IntegerLiteral" ->
      let v = match str_field "value" j with Some s -> s | None -> "0" in
      let t = match str_field "type" j with Some s -> s | None -> "int" in
      parse_const v t
  | "UnbasedUnsizedIntegerLiteral" ->
      (* `'0`, `'1`, `'x`, `'z` — the value is the bit pattern,
       * the LHS context determines the width. We size to the
       * declared type. *)
      let v = match str_field "value" j with Some s -> s | None -> "0" in
      let t = match str_field "type" j with Some s -> s | None -> "logic" in
      parse_const v t
  | "BinaryOp" ->
      let op_s = match str_field "op" j with Some s -> s | None -> "" in
      let lhs = match assoc "left" j with
        | Some j' -> expr_to_bexpr j'
        | None -> BConst { value = Z.zero; width = 1 } in
      let rhs = match assoc "right" j with
        | Some j' -> expr_to_bexpr j'
        | None -> BConst { value = Z.zero; width = 1 } in
      let t = match str_field "type" j with Some s -> s | None -> "logic" in
      let w, _, _ = parse_type t in
      (match bop_of_string op_s with
       | Some op -> BBinOp { op; lhs; rhs;
                             result_type = BInt { width = w; signed = Unsigned } }
       | None -> lhs)
  | "UnaryOp" ->
      let op_s = match str_field "op" j with Some s -> s | None -> "" in
      let operand = match assoc "operand" j with
        | Some j' -> expr_to_bexpr j'
        | None -> BConst { value = Z.zero; width = 1 } in
      let t = match str_field "type" j with Some s -> s | None -> "logic" in
      let w, _, _ = parse_type t in
      let result_type =
        Behavioral_ir.BInt { width = w; signed = Unsigned } in
      (match op_s with
       | "LogicalNot" ->
           (* SV `!v` is a 1-bit logical NOT — equivalent to
              `~(|v)`: NOT-of-OR-reduction.  The result is 1 bit
              wide; if the use-site needs N bits the surrounding
              context widens it.  Matches verilator's emission. *)
           let bit1 = Behavioral_ir.BInt { width = 1; signed = Unsigned } in
           BUnOp { op = BNot;
                   operand = BUnOp { op = BRedOr; operand; result_type = bit1 };
                   result_type }
       | _ ->
           match uop_of_string op_s with
           | Some op -> BUnOp { op; operand; result_type }
           | None -> operand)
  | "ConditionalOp" ->
      (* Slang uses `conditions: [{expr: …}]` for the predicate (a
       * list to support `c1 &&& c2 ? a : b` SV-2017 patterns), and
       * `left`/`right` for then/else. *)
      let cond = match list_field "conditions" j with
        | first :: _ ->
            (match assoc "expr" first with
             | Some j' -> expr_to_bexpr j'
             | None -> BConst { value = Z.zero; width = 1 })
        | [] -> BConst { value = Z.zero; width = 1 }
      in
      let then_val = match assoc "left" j with
        | Some j' -> expr_to_bexpr j'
        | None -> BConst { value = Z.zero; width = 1 } in
      let else_val = match assoc "right" j with
        | Some j' -> expr_to_bexpr j'
        | None -> BConst { value = Z.zero; width = 1 } in
      BCond { condition = cond; then_val; else_val }
  | "Concatenation" ->
      (* `operands` is a list, MSB-first per SV convention — same
       * order BConcat expects. *)
      let ops = list_field "operands" j in
      BConcat (List.map expr_to_bexpr ops)
  | "Replication" ->
      let count = match assoc "count" j with
        | Some j' ->
            (match expr_to_bexpr j' with
             | BConst { value; _ } -> Z.to_int value
             | _ -> 1)
        | None -> 1
      in
      let value = match assoc "concat" j with
        | Some j' -> expr_to_bexpr j'
        | None -> BConst { value = Z.zero; width = 1 }
      in
      BReplicate { count; value }
  | "ElementSelect" ->
      let arr = match assoc "value" j with
        | Some j' -> expr_to_bexpr j'
        | None -> BConst { value = Z.zero; width = 1 } in
      let idx = match assoc "selector" j with
        | Some j' -> expr_to_bexpr j'
        | None -> BConst { value = Z.zero; width = 1 } in
      BSelect { array = arr; index = idx }
  | "RangeSelect" ->
      let arr = match assoc "value" j with
        | Some j' -> expr_to_bexpr j'
        | None -> BConst { value = Z.zero; width = 1 } in
      (* Slang annotates the resolved value of arithmetic msb/lsb
       * expressions in the `constant` field as `"<width>'d<num>"` or
       * `"<width>'h<hex>"`. Trust that when present — otherwise
       * recurse via expr_to_bexpr and only accept BConst.  Without
       * this, RangeSelect with parameter-derived bounds (e.g.
       * `padded_input[PaddedWidth-1 : PaddedWidth/2]` in cva6's
       * popcount tree) collapsed to [0:0], silently producing the
       * wrong bit-slice. *)
      let const_of_constant_field j' =
        match str_field "constant" j' with
        | Some s ->
            let s =
              match String.index_opt s '\'' with
              | Some i when i + 1 < String.length s ->
                  String.sub s (i + 2) (String.length s - i - 2)
              | _ -> s in
            (try Some (int_of_string s)
             with _ ->
               try Some (int_of_string ("0x" ^ s))
               with _ -> None)
        | None -> None in
      let resolve k =
        match assoc k j with
        | Some j' ->
            (match const_of_constant_field j' with
             | Some n -> n
             | None ->
                 match expr_to_bexpr j' with
                 | BConst { value; _ } -> Z.to_int value
                 | _ -> 0)
        | None -> 0 in
      let msb = resolve "left" in
      let lsb = resolve "right" in
      BSlice { signal = arr; msb; lsb }
  | "Conversion" ->
      (* Type cast — value-preserving for our integer arithmetic. *)
      (match assoc "operand" j with
       | Some j' -> expr_to_bexpr j'
       | None -> BConst { value = Z.zero; width = 1 })
  | "Assignment" ->
      (* Embedded assignment as an expression — usually return the RHS.
         But for task-call output arguments, slang wraps the argument as
         `Assignment { left: <lvalue>, right: EmptyArgument }` to record
         the back-binding direction.  In that case the *lvalue* is the
         interesting value (the variable the task writes to), so return
         it instead — matches verilator's emission. *)
      let right = assoc "right" j in
      (match right with
       | Some j' when kind_of j' = "EmptyArgument" ->
           (match assoc "left" j with
            | Some l -> expr_to_bexpr l
            | None -> BConst { value = Z.zero; width = 1 })
       | Some j' -> expr_to_bexpr j'
       | None -> BConst { value = Z.zero; width = 1 })
  | "Call" ->
      (* Function call: `{ "subroutine": "<addr> <name>", "arguments": [...] }`.
         z3_miter treats unresolved BCalls as uninterpreted functions —
         the two designs both producing the same BCall { func; args }
         then matches by uninterpreted-function equality even before
         any inliner runs.

         Special case: `$unsigned`/`$signed` are value-preserving sign
         casts.  Inline them to the first argument so the BIR matches
         verible's lowering (which also inlines these). *)
      let func = match str_field "subroutine" j with
        | Some s -> strip_addr s
        | None -> "?" in
      let args = match assoc "arguments" j with
        | Some (`List xs) -> List.map expr_to_bexpr xs
        | _ -> [] in
      (match func, args with
       | ("$unsigned" | "$signed"), x :: _ -> x
       | _ -> BCall { func; args })
  | _ ->
      (* Unrecognised — emit a placeholder zero. Matching strict mode
       * here would catch shapes the converter doesn't model. *)
      BConst { value = Z.zero; width = 1 }

(* ─── Statements ─────────────────────────────────────────────────── *)

(* Lower the LHS of an Assignment to the appropriate BIR statement.

   - bare `name`            →  BAssign { lhs = name; rhs }
   - `name[idx]`            →  @mem_write(name, idx, rhs)
                                 (also covers bit-select into a vector,
                                  which the BIR convention treats the
                                  same as a 1-element array write — the
                                  Hardcaml lowering bit-merges it with
                                  any concurrent full-bus assignment).
   - `name[msb:lsb]`        →  @slice_write(name, msb, lsb, rhs)
   - `name[base +: width]`  →  @part_sel_write_up(name, base, width, rhs)
   - `name[base -: width]`  →  @part_sel_write_down(name, base, width, rhs)
   - `Conversion` wrapping  →  recurse on the inner expression

   Mirrors verible_to_behavioral's [assign_to] dispatch.  Without this,
   `iCounter[WIDTH] <= 0` was widening into `iCounter := 0`, clobbering
   the rest of the bus on every clock edge — a real semantic bug
   surfaced by the verible-vs-slang cross-frontend miter on
   sysver_tests/slib_counter.sv.                                     *)
let rec lhs_to_bstmt l rhs =
  let rec base_name j =
    match str_field "symbol" j with
    | Some s -> Some (strip_addr s)
    | None ->
        (match assoc "value" j with
         | Some j' -> base_name j'
         | None ->
             match assoc "operand" j with
             | Some j' -> base_name j'
             | None ->
                 match assoc "target" j with
                 | Some j' -> base_name j'
                 | None -> None)
  in
  match kind_of l with
  | "ElementSelect" ->
      let name_opt = match assoc "value" l with
        | Some v -> base_name v
        | None -> None in
      let idx = match assoc "selector" l with
        | Some j' -> expr_to_bexpr j'
        | None -> BConst { value = Z.zero; width = 1 } in
      (match name_opt with
       | Some n -> BCallStmt {
           func = "@mem_write";
           args = [BVar n; idx; rhs];
         }
       | None -> BBlock [])
  | "RangeSelect" ->
      let name_opt = match assoc "value" l with
        | Some v -> base_name v
        | None -> None in
      let sel_kind = match str_field "selectionKind" l with
        | Some s -> s
        | None -> "Simple" in
      let left_e = match assoc "left" l with
        | Some j' -> expr_to_bexpr j'
        | None -> BConst { value = Z.zero; width = 1 } in
      let right_e = match assoc "right" l with
        | Some j' -> expr_to_bexpr j'
        | None -> BConst { value = Z.zero; width = 1 } in
      (match name_opt with
       | None -> BBlock []
       | Some n ->
           let func =
             match sel_kind with
             | "IndexedUp"   -> "@part_sel_write_up"
             | "IndexedDown" -> "@part_sel_write_down"
             | _             -> "@slice_write" in
           BCallStmt { func; args = [BVar n; left_e; right_e; rhs] })
  | "Conversion" ->
      (match assoc "operand" l with
       | Some j' -> lhs_to_bstmt j' rhs
       | None -> BBlock [])
  | _ ->
      (match base_name l with
       | Some n -> BAssign { lhs = n; rhs }
       | None -> BAssign { lhs = "?"; rhs })

let rec stmt_to_bstmt j =
  match kind_of j with
  | "Timed" ->
      (* `always_comb` / `always @(...)` wrap their body in a Timed
         node carrying the sensitivity in `timing` and the actual
         body in `stmt`.  The sequential ProceduralBlock path strips
         Timed before calling here; the combinational path didn't —
         which dropped every always_comb body to BBlock [], silently
         losing all combinational logic.  Recurse into stmt.        *)
      (match assoc "stmt" j with
       | Some j' -> stmt_to_bstmt j'
       | None -> BBlock [])
  | "Block" ->
      (* `body` is either a single statement or a List node. *)
      (match assoc "body" j with
       | Some inner -> stmt_to_bstmt inner
       | None -> BBlock [])
  | "List" ->
      (* Slang sometimes wraps a sequence of statements as a `List`
       * with a `list` field. *)
      let items = list_field "list" j in
      BBlock (List.map stmt_to_bstmt items)
  | "Conditional" ->
      let cond = match list_field "conditions" j with
        | first :: _ ->
            (match assoc "expr" first with
             | Some j' -> expr_to_bexpr j'
             | None -> BConst { value = Z.zero; width = 1 })
        | [] -> BConst { value = Z.zero; width = 1 }
      in
      let then_stmts = match assoc "ifTrue" j with
        | Some j' -> [stmt_to_bstmt j']
        | None -> [] in
      let else_stmts = match assoc "ifFalse" j with
        | Some j' -> [stmt_to_bstmt j']
        | None -> [] in
      BIf { condition = cond; then_stmts; else_stmts }
  | "ExpressionStatement" ->
      (* The wrapped expression is usually an Assignment, but task
         calls and void function calls also surface here as `Call`. *)
      (match assoc "expr" j with
       | Some inner ->
           (match kind_of inner with
            | "Assignment" ->
                let l = match assoc "left" inner with
                  | Some j' -> j' | None -> `Null in
                (* For compound `lhs op= rhs`, slang plants an
                   LValueReference inside the rhs that stands for the
                   pre-update value of the lhs.  Resolve it by
                   computing the lhs expression and stashing it in the
                   shared ref while we convert the rhs.              *)
                let saved = !lvalue_ref_ctx in
                (match str_field "op" inner with
                 | Some _ -> lvalue_ref_ctx := Some (expr_to_bexpr l)
                 | None -> ());
                let rhs = match assoc "right" inner with
                  | Some r -> expr_to_bexpr r
                  | None -> BConst { value = Z.zero; width = 1 } in
                lvalue_ref_ctx := saved;
                lhs_to_bstmt l rhs
            | "Call" ->
                (* Task call or void-function call as a statement.
                   Mirrors verilator_to_behavioral's TaskRef/FuncRef
                   StmtExpr path: emit BCallStmt so the call survives
                   into z3_miter as an uninterpreted statement.
                   Testbench primitives ($display/$finish/$stop/etc.)
                   are dropped — they have no synth semantics.       *)
                let func = match str_field "subroutine" inner with
                  | Some s -> strip_addr s
                  | None -> "?" in
                let args = match assoc "arguments" inner with
                  | Some (`List xs) -> List.map expr_to_bexpr xs
                  | _ -> [] in
                (match func with
                 | "$display" | "$write" | "$strobe" | "$monitor"
                 | "$finish" | "$stop" | "$exit"
                 | "$fdisplay" | "$fwrite" | "$fopen" | "$fclose"
                 | "$readmemh" | "$readmemb"
                 | "$dumpfile" | "$dumpvars" | "$dumpon" | "$dumpoff" ->
                     BBlock []
                 | _ -> BCallStmt { func; args })
            | _ -> BBlock [])
       | None -> BBlock [])
  | "Return" ->
      (* `return expr;` inside a function body.  Lower to an assignment
         to the implicit return-value variable (named the same as the
         function).  Matches verilator's `add := (a + b)` shape so
         z3_miter can equate the two frontends' function bodies.    *)
      let rhs = match assoc "expr" j with
        | Some j' -> expr_to_bexpr j'
        | None -> BConst { value = Z.zero; width = 1 } in
      (match !current_function_name with
       | Some fname -> BAssign { lhs = fname; rhs }
       | None -> BReturn (Some rhs))
  | "Case" ->
      let sel = match assoc "expr" j with
        | Some j' -> expr_to_bexpr j'
        | None -> BConst { value = Z.zero; width = 1 } in
      let items = list_field "items" j in
      let cases, default =
        List.fold_left (fun (cs, def) ci ->
          match assoc "expressions" ci with
          | Some (`List exprs) ->
              let body = match assoc "stmt" ci with
                | Some j' -> [stmt_to_bstmt j'] | None -> [] in
              let arm_keys = List.map expr_to_bexpr exprs in
              (cs @ List.map (fun k -> (k, body)) arm_keys, def)
          | _ ->
              let body = match assoc "stmt" ci with
                | Some j' -> [stmt_to_bstmt j'] | None -> [] in
              (cs, body)
        ) ([], []) items
      in
      BCase { selector = sel; cases; default }
  | _ -> BBlock []

(* ─── Module body extraction ─────────────────────────────────────── *)

(* Slang's elaborated AST keeps elaborated `GenerateBlock`s as nested
 * members of the InstanceBody. The branch that was actually taken
 * for the current parameter set has its `ContinuousAssign` /
 * `ProceduralBlock` / `Variable` children inside that block —
 * extract_signals / extract_processes need to descend recursively to
 * find them. Branches that weren't taken carry
 * `isUninstantiated: true` and we skip them. *)
let bool_field f j =
  match assoc f j with Some (`Bool b) -> b | _ -> false

let rec flatten_members members =
  List.concat_map (fun m ->
    match kind_of m with
    | "GenerateBlock" when not (bool_field "isUninstantiated" m) ->
        let sub = list_field "members" m in
        flatten_members sub
    | "GenerateBlock" -> []     (* uninstantiated branch *)
    | "GenerateBlockArray" ->
        (* `for (genvar i …) generate ... endgenerate` — each
         * iteration is a GenerateBlock entry. Recurse into all of
         * them; only instantiated ones produce members. *)
        let sub = list_field "members" m in
        flatten_members sub
    | _ -> [m]
  ) members

(* Slang emits two members per signal — a `Port` (carries direction)
 * and a `Variable` (carries storage type). Match by name and merge
 * into a single bsignal. *)
let extract_signals members_raw =
  let members = flatten_members members_raw in
  let ports : (string, [`Input|`Output|`Internal]) Hashtbl.t =
    Hashtbl.create 16 in
  List.iter (fun m ->
    match kind_of m with
    | "Port" ->
        let name = name_of m in
        let dir = match str_field "direction" m with
          | Some "In" -> `Input
          | Some "Out" -> `Output
          | Some "InOut" -> `Internal
          | _ -> `Internal
        in
        Hashtbl.replace ports name dir
    | _ -> ()
  ) members;
  List.filter_map (fun m ->
    match kind_of m with
    | "Variable" | "Net" ->
        let name = name_of m in
        if name = "" then None
        else
          let t = match str_field "type" m with Some s -> s | None -> "logic" in
          let w, elem_w, ndims = parse_type t in
          let stype =
            if ndims >= 2 && w > 0 && elem_w > 0 then
              BArray {
                element = BInt { width = elem_w; signed = Unsigned };
                size = w / elem_w;
              }
            else
              BInt { width = w; signed = Unsigned }
          in
          let dir = try Hashtbl.find ports name with Not_found -> `Internal in
          Some {
            name;
            stype;
            direction = dir;
            initial_value = None; attrs = []; 
          }
    | _ -> None
  ) members

(* Pull the clock + edge from the FIRST SignalEvent in an EventList.
 * Slang emits the events in source order; the first edge identifier
 * is the clock for `always_ff @(posedge clk or negedge rst_n)` —
 * synthesis treats `negedge rst_n` as the async-reset trigger,
 * not as a second clock. *)
let extract_clock_event timing =
  (* Slang's timing comes in two shapes: a `SignalEvent` directly
   * for `always @(posedge clk)` (single event), or an `EventList`
   * with `events: [...]` for `always @(posedge clk or negedge
   * rst_n)`. Handle both — pick the first event's expr+edge. *)
  let pick_from_event e =
    let clk = match assoc "expr" e with
      | Some j' ->
          (match str_field "symbol" j' with
           | Some s -> strip_addr s | None -> "clk")
      | None -> "clk"
    in
    let edge = match str_field "edge" e with
      | Some "PosEdge" -> `Pos
      | Some "NegEdge" -> `Neg
      | _ -> `Pos
    in
    (clk, edge)
  in
  match kind_of timing with
  | "SignalEvent" -> pick_from_event timing
  | "EventList" ->
      (match assoc "events" timing with
       | Some (`List (e :: _)) -> pick_from_event e
       | _ -> ("clk", `Pos))
  | _ -> ("clk", `Pos)

(* Extract Subroutine (function / task) definitions into bfunc records
   so downstream inlining or any other pass that walks m.funcs can see
   them.  Without this slang's BIR had `funcs = []` even though every
   call site emitted a BCall — the bodies were lost. *)
let extract_funcs members_raw =
  let members = flatten_members members_raw in
  List.filter_map (fun m ->
    match kind_of m with
    | "Subroutine" ->
        let fname = name_of m in
        if fname = "" then None
        else
          let sub_kind = str_field "subroutineKind" m in
          let is_task = (sub_kind = Some "Task") in
          let sub_members = list_field "members" m in
          let parse_btype t =
            let w, _, _ = parse_type t in
            Behavioral_ir.BInt { width = w; signed = Unsigned } in
          let params = List.filter_map (fun mm ->
            match kind_of mm with
            | "FormalArgument" ->
                let n = name_of mm in
                let t = match str_field "type" mm with
                  | Some s -> s | None -> "logic" in
                let dir = match str_field "direction" mm with
                  | Some "In"    -> `Input
                  | Some "Out"   -> `Output
                  | Some "InOut" -> `Inout
                  | _            -> `Input in
                Some (n, parse_btype t, dir)
            | _ -> None
          ) sub_members in
          let locals = List.filter_map (fun mm ->
            match kind_of mm with
            | "Variable" ->
                let n = name_of mm in
                (* Skip the implicit return-value variable that slang
                   names the same as the function. *)
                if n = fname then None
                else
                  let t = match str_field "type" mm with
                    | Some s -> s | None -> "logic" in
                  Some (n, parse_btype t)
            | _ -> None
          ) sub_members in
          let ftype =
            let t = match str_field "returnType" m with
              | Some s -> s | None -> "logic" in
            parse_btype t in
          let saved = !current_function_name in
          current_function_name := Some fname;
          let body = match assoc "body" m with
            | Some j' -> [stmt_to_bstmt j']
            | None -> [] in
          current_function_name := saved;
          Some { Behavioral_ir.fname; is_task; ftype;
                 params; locals; body }
    | _ -> None
  ) members

let extract_processes members_raw =
  let members = flatten_members members_raw in
  List.filter_map (fun m ->
    match kind_of m with
    | "ContinuousAssign" ->
        let assignment = match assoc "assignment" m with
          | Some j' -> j'
          | None -> `Null in
        (* LHS may be a plain NamedValue OR a RangeSelect /
         * ElementSelect / Conversion wrapping the destination. Walk
         * down the wrappers (looking at `value`/`operand` fields)
         * until we hit a NamedValue, then take its symbol. Without
         * this, `assign x[7:0] = ...` lost the LHS to "?" and the
         * downstream miter saw `x` as undriven. *)
        let rec lhs_name_of l =
          match str_field "symbol" l with
          | Some s -> strip_addr s
          | None ->
              (* Common wrappers: try each. *)
              let try_field f =
                match assoc f l with
                | Some j' -> Some (lhs_name_of j')
                | None -> None in
              (match try_field "value" with
               | Some n -> n
               | None ->
                   match try_field "operand" with
                   | Some n -> n
                   | None ->
                       match try_field "target" with
                       | Some n -> n
                       | None -> "?")
        in
        let lhs = match assoc "left" assignment with
          | Some l -> lhs_name_of l
          | None -> "?" in
        let rhs = match assoc "right" assignment with
          | Some r -> expr_to_bexpr r
          | None -> BConst { value = Z.zero; width = 1 } in
        Some (BCombinational {
          name = "assign_" ^ lhs;
          sensitivity = [BAny];
          body = [BAssign { lhs; rhs }];
        })
    | "ProceduralBlock" ->
        let kind = match str_field "procedureKind" m with
          | Some s -> s | None -> "Always" in
        let inner = match assoc "body" m with Some j' -> j' | None -> `Null in
        (* For procedureKind "Always" Slang doesn't tell us whether
         * the block is sequential or combinational — we have to
         * inspect the sensitivity list. PosEdge/NegEdge anywhere
         * inside the Timed timing means sequential. Without this
         * every cva6 `always @(posedge clk)` block (used heavily in
         * the ct_vfdsu_* family) lowered to BCombinational, losing
         * every FF and producing input-interface mismatches with
         * Verible's BSequential-and-FF-rip path. *)
        let has_edge_event timing =
          let found = ref false in
          let rec walk = function
            | `Assoc xs ->
                (match List.assoc_opt "edge" xs with
                 | Some (`String ("PosEdge" | "NegEdge")) -> found := true
                 | _ -> List.iter (fun (_, v) -> walk v) xs)
            | `List xs -> List.iter walk xs
            | _ -> ()
          in
          walk timing;
          !found
        in
        let is_sequential =
          match kind with
          | "AlwaysFF" | "Always_FF" -> true
          | "Always" ->
              (match kind_of inner with
               | "Timed" ->
                   (match assoc "timing" inner with
                    | Some t -> has_edge_event t
                    | None -> false)
               | _ -> false)
          | _ -> false
        in
        if is_sequential then begin
          let timing, body_stmt =
            match kind_of inner with
            | "Timed" ->
                let t = match assoc "timing" inner with
                  | Some j' -> j' | None -> `Null in
                let s = match assoc "stmt" inner with
                  | Some j' -> j' | None -> `Null in
                (t, s)
            | _ -> (`Null, inner)
          in
          let (clk, edge) = extract_clock_event timing in
          let body = [stmt_to_bstmt body_stmt] in
          Some (BSequential {
            name = "always_ff";
            clock = clk;
            clock_edge = edge;
            reset = None;
            reset_edge = None;
            reset_async = false;
            body;
            blocking_vars = [];
          })
        end else
          let body = [stmt_to_bstmt inner] in
          Some (BCombinational {
            name = "always_comb";
            sensitivity = [BAny];
            body;
          })
    | _ -> None
  ) members

(* ─── Top-level ──────────────────────────────────────────────────── *)

(* Build a Verible-compatible specialised name from a base name and
 * the InstanceBody's port-Parameter members. Mirrors
 * verible_elaborate.suffix_of_params so paired modules across the
 * two frontends use the same key — Verible specialises every unique
 * (module, param-set) tuple into its own `<base>__<param-suffix>`
 * bmodule, and Slang creates a separate InstanceBody per
 * specialisation but always names them after the base. Without this
 * suffix Slang's 90+ InstanceBodies named `cva6_fifo_v3` (one per
 * specialisation) would collapse into a single bmodule, losing
 * almost the entire design. *)
let abbrev s =
  let buf = Buffer.create 4 in
  let next_upper = ref true in
  String.iter (fun c ->
    if c = '_' then next_upper := true
    else if !next_upper then begin
      Buffer.add_char buf (Char.uppercase_ascii c);
      next_upper := false
    end
  ) s;
  Buffer.contents buf

let strip_value_prefix v =
  (* Verible's suffix_of_params strips the SV `<width>'<base>` prefix
   * from numeric values so a parameter `INPUT_WIDTH=32'd64` ends up
   * as `IW64` not `IW32'd64`. *)
  try
    let i = String.index v '\'' in
    if i + 1 < String.length v
    then String.sub v (i + 2) (String.length v - i - 2)
    else v
  with Not_found -> v

let suffix_of_slang_params members =
  (* Include only port-parameters (`isPort: true`) whose value
   * parses as a small-ish integer; struct-typed CVA6Cfg-style
   * parameters carry hundreds of hex digits and would explode
   * the suffix to thousands of characters. Verible's
   * specialise_design also skips non-int params naturally because
   * its scope-builder only stores names that resolve via Eval, so
   * matching this rule keeps the two suffixes aligned. *)
  let pairs = List.filter_map (fun m ->
    match kind_of m with
    | "Parameter" ->
        let is_port = match assoc "isPort" m with
          | Some (`Bool b) -> b
          | _ -> false
        in
        if not is_port then None
        else
          let n = name_of m in
          let v_raw = match str_field "value" m with
            | Some s -> s | None -> ""
          in
          let v_clean = strip_value_prefix v_raw in
          (* Only keep if v_clean is a reasonable integer in
           * decimal or hex (≤ 32 chars). The 32-char limit lets us
           * accept normal int parameters but reject the giant
           * struct hex blobs Slang prints for CVA6Cfg. *)
          if String.length v_clean = 0 || String.length v_clean > 32
          then None
          else
            let is_dec_or_hex =
              String.for_all (fun c ->
                (c >= '0' && c <= '9') ||
                (c >= 'a' && c <= 'f') ||
                (c >= 'A' && c <= 'F')) v_clean
            in
            if not is_dec_or_hex then None
            else Some (n, v_clean)
    | _ -> None
  ) members in
  if pairs = [] then ""
  else "__" ^ String.concat "_"
    (List.map (fun (k, v) -> abbrev k ^ v) pairs)

(* Extract child Instance members. Each Instance carries:
 *   - name        (the instance label, e.g. "left_child")
 *   - body        (the InstanceBody being instantiated; contains
 *                  the Parameter list for suffix computation)
 *   - connections (list of port-connection records; each has
 *                  `port: { name; direction }` and `expr` which is
 *                  the actual for inputs, or an Assignment whose
 *                  `left` is the actual for outputs)
 *
 * Returns binstance records keyed by the specialised module_name
 * (matching what `collect_instances` deposits into the bprogram). *)
let extract_instances members_raw =
  let members = flatten_members members_raw in
  List.filter_map (fun m ->
    match kind_of m with
    | "Instance" ->
        let inst_name = name_of m in
        if inst_name = "" then None
        else
          (* Resolve body: it's either a full InstanceBody Assoc, or
           * a `<addr> <name>` string back-reference to the first
           * occurrence in `body_by_addr`. *)
          let body = match assoc "body" m with
            | Some (`Assoc _ as j) -> j
            | Some (`String s) ->
                let addr = match String.index_opt s ' ' with
                  | Some i -> String.sub s 0 i
                  | None -> s in
                (try Hashtbl.find body_by_addr addr
                 with Not_found -> `Null)
            | _ -> `Null in
          let body_members = list_field "members" body in
          let base = match str_field "name" body with
            | Some s -> s | None -> "" in
          let suffix = suffix_of_slang_params body_members in
          let module_name = base ^ suffix in
          if Sys.getenv_opt "SLANG_INST_DEBUG" <> None then
            Printf.eprintf "[slang] inst %s → %s\n" inst_name module_name;
          let conns = list_field "connections" m in
          let port_connections =
            List.filter_map (fun c ->
              let port = match assoc "port" c with
                | Some j -> j | None -> `Null in
              let pname = name_of port in
              let dir = str_field "direction" port in
              let expr_node = match assoc "expr" c with
                | Some j -> j | None -> `Null in
              (* For Output ports the expr is wrapped in an
               * Assignment whose `left` is the actual we want. *)
              let actual =
                match dir, kind_of expr_node with
                | Some "Out", "Assignment" ->
                    (match assoc "left" expr_node with
                     | Some j -> expr_to_bexpr j
                     | None -> BConst { value = Z.zero; width = 1 })
                | _ -> expr_to_bexpr expr_node
              in
              if pname = "" then None
              else Some (pname, actual)
            ) conns
          in
          Some {
            inst_name;
            module_name;
            param_values = []; param_strs = [];
            port_connections;
          }
    | _ -> None
  ) members

(* Collect every Package node anywhere in the design, then pull
   subroutines out of their members.  Mirrors verilator's pre-collect
   in convert_ast: functions/tasks defined inside `package func_pkg`
   need to be visible to modules that do `import func_pkg::*`,
   otherwise the call sites stay as uninterpreted BCalls and z3_miter
   can't equate them with verilator's inlined bodies.              *)
let collect_package_funcs j =
  let acc = ref [] in
  let rec walk = function
    | `Assoc fields as node ->
        (match List.assoc_opt "kind" fields with
         | Some (`String "Package") ->
             let members = match List.assoc_opt "members" fields with
               | Some (`List xs) -> xs | _ -> [] in
             acc := extract_funcs members @ !acc
         | _ -> ());
        List.iter (fun (_, v) -> walk v) fields;
        ignore node
    | `List xs -> List.iter walk xs
    | _ -> ()
  in
  walk j;
  !acc

(* Walk every Instance / InstanceBody under `design.members` and
 * produce one bmodule each. Recurse into the InstanceBody's
 * members too — nested Instance nodes (sub-instances of the parent)
 * point at their own InstanceBody, which is the next level of the
 * hierarchy and needs its own bmodule. Each (base, param-suffix)
 * combination becomes one bmodule; duplicate instances of the same
 * specialisation collapse via the seen set. *)
let rec collect_instances ~seen ~pkg_funcs acc j =
  match kind_of j with
  | "InstanceBody" ->
      let base = name_of j in
      let members = list_field "members" j in
      let suffix = suffix_of_slang_params members in
      let name = base ^ suffix in
      let acc =
        if name <> "" && not (Hashtbl.mem seen name) then begin
          Hashtbl.add seen name ();
          let signals = extract_signals members in
          let processes = extract_processes members in
          let instances = extract_instances members in
          let local_funcs = extract_funcs members in
          (* Module-local definitions shadow package-level ones with
             the same name. *)
          let local_names =
            List.fold_left (fun s (f : Behavioral_ir.bfunc) ->
              s @ [f.fname]) [] local_funcs in
          let funcs =
            local_funcs @
            List.filter (fun (f : Behavioral_ir.bfunc) ->
              not (List.mem f.fname local_names)) pkg_funcs
          in
          let m = {
            name;
            params = [];
            signals;
            processes;
            instances;
            funcs;
            mems = []; attrs = [];
          } in
          m :: acc
        end else acc
      in
      (* Sub-instances live inside members. Recurse to find their
       * InstanceBody too. *)
      List.fold_left (collect_instances ~seen ~pkg_funcs) acc members
  | _ ->
      let acc =
        match assoc "body" j with
        | Some j' -> collect_instances ~seen ~pkg_funcs acc j'
        | None -> acc
      in
      List.fold_left (collect_instances ~seen ~pkg_funcs) acc
        (list_field "members" j)

(* Pre-scan: stuff every InstanceBody's `addr` field into
 * `body_by_addr` so later `Instance.body` string back-references
 * resolve. *)
let rec build_addr_table j =
  let addr_string addr_j = match addr_j with
    | `String s -> s
    | `Int i -> string_of_int i
    | `Intlit s -> s
    | _ -> "" in
  match j with
  | `Assoc fields ->
      (match List.assoc_opt "kind" fields,
             List.assoc_opt "addr" fields with
       | Some (`String "InstanceBody"), Some addr_j ->
           let addr = addr_string addr_j in
           if addr <> "" && not (Hashtbl.mem body_by_addr addr) then
             Hashtbl.replace body_by_addr addr j
       | Some (`String "Parameter"), Some addr_j ->
           let addr = addr_string addr_j in
           let value = match List.assoc_opt "value" fields with
             | Some (`String s) -> s | _ -> "" in
           let typ = match List.assoc_opt "type" fields with
             | Some (`String s) -> s | _ -> "" in
           if addr <> "" && value <> ""
              && not (Hashtbl.mem param_value_by_addr addr) then
             Hashtbl.replace param_value_by_addr addr (value, typ)
       | _ -> ());
      List.iter (fun (_, v) -> build_addr_table v) fields
  | `List xs -> List.iter build_addr_table xs
  | _ -> ()

(* Populate enum_base_tbl from every EnumType node: baseType keyed by
 * both the bare typedef name and the module-qualified name (e.g.
 * "state_type" and "uart_transmitter.state_type" → "logic[3:0]").
 * The qualified key is essential because distinct modules reuse the
 * same typedef name with different widths (uart_receiver's state_type
 * is enum logic [2:0], uart_transmitter's is enum logic [3:0]); a
 * bare-name table would collide.  parse_type tries the qualified name
 * (which slang emits in the signal's type string) first. *)
let build_enum_table (j : json) =
  Hashtbl.clear enum_base_tbl;
  let rec walk scope = function
    | `Assoc fields ->
        let scope' =
          match List.assoc_opt "kind" fields, List.assoc_opt "name" fields with
          | Some (`String ("Instance" | "InstanceBody")), Some (`String nm)
            when nm <> "" -> nm
          | _ -> scope
        in
        (match List.assoc_opt "kind" fields with
         | Some (`String "EnumType") ->
             (match List.assoc_opt "name" fields,
                    List.assoc_opt "baseType" fields with
              | Some (`String name), Some (`String base) when name <> "" ->
                  Hashtbl.replace enum_base_tbl name base;
                  if scope' <> "" then
                    Hashtbl.replace enum_base_tbl (scope' ^ "." ^ name) base
              | _ -> ())
         | _ -> ());
        List.iter (fun (_, v) -> walk scope' v) fields
    | `List xs -> List.iter (walk scope) xs
    | _ -> ()
  in
  walk "" j

let convert_json (j : json) : bprogram =
  let design = match assoc "design" j with Some d -> d | None -> j in
  Hashtbl.clear body_by_addr;
  build_addr_table j;
  build_enum_table j;
  let pkg_funcs = collect_package_funcs design in
  let seen = Hashtbl.create 256 in
  let mods = List.rev (collect_instances ~seen ~pkg_funcs [] design) in
  { modules = mods; library_cells = [] }

(* ─── Driver invocation ──────────────────────────────────────────── *)

let find_slang () =
  let home = try Sys.getenv "HOME" with Not_found -> "/root" in
  let candidates = [
    home ^ "/sv-tests/third_party/tools/slang/build/bin/slang";
    "/usr/local/bin/slang";
  ] in
  List.find_opt Sys.file_exists candidates

let convert_files ~top files : bprogram option =
  match find_slang () with
  | None ->
      Printf.eprintf "[slang] driver not found\n";
      None
  | Some slang ->
      let json_path = Filename.temp_file "slang_" ".json" in
      (* `--timescale` supplies a default for modules that lack a
         `\`timescale` directive, so a fileset where only some files
         carry one (e.g. picosoc: picorv32.v has one, the rest don't)
         doesn't trip slang's "design element does not have a time
         scale defined but others do" error.  Timescale is irrelevant
         to the synthesisable BIR. *)
      let cmd = Printf.sprintf
        "%s --ast-json %s --timescale 1ns/1ps --top %s %s 2>/dev/null"
        (Filename.quote slang)
        (Filename.quote json_path)
        (Filename.quote top)
        (String.concat " " (List.map Filename.quote files))
      in
      let rc = Sys.command cmd in
      if rc <> 0 then begin
        Printf.eprintf "[slang] driver returned rc=%d\n" rc;
        (try Sys.remove json_path with _ -> ());
        None
      end else begin
        let j = try Yojson.Safe.from_file json_path
                with e ->
                  Printf.eprintf "[slang] json parse failed: %s\n"
                    (Printexc.to_string e);
                  `Null
        in
        (try Sys.remove json_path with _ -> ());
        Some (convert_json j)
      end
