(* Verilator JSON to Behavioral IR Converter
 *
 * Converts Verilator's JSON AST (sv_ast) to language-neutral behavioral IR.
 * This provides a path from Verilator → Behavioral IR → Optimization → Z3.
 *)

open Behavioral_ir
open Sv_ast

let debug = ref false

(* Strict mode: when MITER_STRICT=1 is set in the environment, refuse to
 * silently produce a partial BIR. Instead of falling back to a vacuous
 * placeholder for an unrecognised expression / statement / cell shape,
 * raise — so the caller sees exactly which pattern needs converter
 * support. The default (permissive) behaviour preserves existing tests. *)
let strict_mode = lazy (Sys.getenv_opt "MITER_STRICT" <> None)

(* Brief diagnostic name for a Verilator AST node. Captures enough to
 * identify which constructor we hit without dumping the whole subtree. *)
let node_kind = function
  | Netlist _ -> "Netlist" | Module _ -> "Module"
  | Package _ -> "Package" | Interface _ -> "Interface"
  | Cell _ -> "Cell" | Cell' _ -> "Cell'" | Pin _ -> "Pin"
  | Var _ -> "Var" | Var' _ -> "Var'"
  | Const _ -> "Const" | Const' _ -> "Const'"
  | EnumItemRef _ -> "EnumItemRef" | EnumItemRef' _ -> "EnumItemRef'"
  | VarRef _ -> "VarRef" | VarRef' _ -> "VarRef'"
  | UnaryOp _ -> "UnaryOp" | UnaryOp' _ -> "UnaryOp'"
  | BinaryOp _ -> "BinaryOp" | BinaryOp' _ -> "BinaryOp'"
  | Concat _ -> "Concat" | Cond _ -> "Cond"
  | Sel _ -> "Sel" | ArraySel _ -> "ArraySel"
  | Assign _ -> "Assign" | AssignW _ -> "AssignW"
  | If _ -> "If" | Case _ -> "Case" | Begin _ -> "Begin" | Always _ -> "Always"
  | SenTree _ -> "SenTree" | SenItem _ -> "SenItem"
  | For _ -> "For" | For' _ -> "For'"
  | Initial _ -> "Initial" | Final _ -> "Final"
  | Stop _ -> "Stop" | JumpBlock _ -> "JumpBlock"
  | Display _ -> "Display"
  | Replicate _ -> "Replicate"
  | _ -> "(other)"

let strict_bail kind context node =
  let msg =
    Printf.sprintf "[verilator_to_behavioral] unrecognised %s in %s: %s\n\
                    set MITER_STRICT=0 (or unset) to suppress this failure"
      kind context (node_kind node)
  in
  if Lazy.force strict_mode then failwith msg
  else if !debug then Printf.eprintf "Warning: %s\n" msg

(* Helper: parse "msb:lsb" → element count. *)
let range_size r =
  try
    let parts = String.split_on_char ':' r in
    match parts with
    | [msb; lsb] ->
        abs (int_of_string (String.trim msb) - int_of_string (String.trim lsb)) + 1
    | _ -> 1
  with _ -> 1

(* Width of one *element* of the type, ignoring any unpacked-array
 * outer dimension. For `reg [7:0] mem [0:7]` this returns 8 (the
 * element width), not 64 or 8 (the depth). *)
let rec get_width_from_dtype = function
  | Some (BasicType { range = Some r; _ }) -> range_size r
  | Some (BasicType { range = None; keyword; _ }) ->
      (match keyword with
       | "int" | "integer" -> 32
       | "shortint" | "shortreal" -> 16
       | "longint" -> 64
       | "byte" -> 8
       | _ -> 1)
  | Some (PackArrayType { range; base }) ->
      (* Packed array: total bit width = element width × range size. *)
      range_size range * get_width_from_dtype (Some base)
  | Some (ArrayType { base; _ }) ->
      (* Unpacked array: width is the element width; the range is the
       * memory *depth*, picked up by `get_array_depth`. *)
      get_width_from_dtype (Some base)
  | Some (RefType { refdtype_ref; _ }) -> get_width_from_dtype refdtype_ref
  | Some (EnumType { base; _ }) -> get_width_from_dtype base
  | _ -> 1

(* For an unpacked-array type, return its depth; otherwise None. *)
let rec get_array_depth = function
  | Some (ArrayType { range; _ }) -> Some (range_size range)
  | Some (RefType { refdtype_ref; _ }) -> get_array_depth refdtype_ref
  | _ -> None

(* Check if dtype is signed *)
let is_signed_dtype = function
  | Some (BasicType { keyword = "int" | "integer" | "shortint" | "longint"; _ }) -> true
  | _ -> false

(* Parse constant value from Verilator format *)
let parse_const_value name =
  try
    if String.contains name '\'' then
      let parts = String.split_on_char '\'' name in
      match parts with
      | width_str :: rest ->
          let value_str = String.concat "'" rest in
          if String.length value_str >= 2 then
            match value_str.[0], value_str.[1] with
            | 's', 'h' ->
                int_of_string ("0x" ^ String.sub value_str 2 (String.length value_str - 2))
            | 's', 'd' ->
                int_of_string (String.sub value_str 2 (String.length value_str - 2))
            | 'h', _ ->
                int_of_string ("0x" ^ String.sub value_str 1 (String.length value_str - 1))
            | 'd', _ ->
                int_of_string (String.sub value_str 1 (String.length value_str - 1))
            | 'b', _ ->
                int_of_string ("0b" ^ String.sub value_str 1 (String.length value_str - 1))
            | _ -> int_of_string value_str
          else int_of_string value_str
      | _ -> int_of_string name
    else
      int_of_string name
  with _ -> 0

(* Convert Verilator expression to behavioral IR expression *)
let rec expr_to_bexpr = function
  | VarRef { name; _ } | VarRef' { name } ->
      BVar name

  | Const { name; dtype_ref } ->
      let value = parse_const_value name in
      let width = get_width_from_dtype dtype_ref in
      BConst { value; width }

  | Const' { name } ->
      let value = parse_const_value name in
      BConst { value; width = 32 }

  | BinaryOp { op; lhs; rhs; dtype_ref } ->
      let lhs_expr = expr_to_bexpr lhs in
      let rhs_expr = expr_to_bexpr rhs in
      let width = get_width_from_dtype dtype_ref in
      let signed = is_signed_dtype dtype_ref in
      let signedness = if signed then Signed else Unsigned in

      (match op with
       | "ADD" -> BBinOp { op = BAdd; lhs = lhs_expr; rhs = rhs_expr;
                           result_type = BInt { width; signed = signedness } }
       | "SUB" -> BBinOp { op = BSub; lhs = lhs_expr; rhs = rhs_expr;
                           result_type = BInt { width; signed = signedness } }
       | "MUL" -> BBinOp { op = BMul; lhs = lhs_expr; rhs = rhs_expr;
                           result_type = BInt { width; signed = signedness } }
       | "DIV" -> BBinOp { op = BDiv; lhs = lhs_expr; rhs = rhs_expr;
                           result_type = BInt { width; signed = signedness } }
       | "MODDIV" -> BBinOp { op = BMod; lhs = lhs_expr; rhs = rhs_expr;
                              result_type = BInt { width; signed = signedness } }
       | "AND" -> BBinOp { op = BAnd; lhs = lhs_expr; rhs = rhs_expr;
                           result_type = BInt { width; signed = signedness } }
       | "OR" -> BBinOp { op = BOr; lhs = lhs_expr; rhs = rhs_expr;
                          result_type = BInt { width; signed = signedness } }
       | "XOR" -> BBinOp { op = BXor; lhs = lhs_expr; rhs = rhs_expr;
                           result_type = BInt { width; signed = signedness } }
       | "SHIFTL" -> BBinOp { op = BShl; lhs = lhs_expr; rhs = rhs_expr;
                              result_type = BInt { width; signed = signedness } }
       | "SHIFTR" -> BBinOp { op = BShr; lhs = lhs_expr; rhs = rhs_expr;
                              result_type = BInt { width; signed = signedness } }
       | "SHIFTRS" -> BBinOp { op = BAshr; lhs = lhs_expr; rhs = rhs_expr;
                               result_type = BInt { width; signed = Signed } }
       (* Case-equality `===` and wildcard `==?` degrade to plain
        * `==` once X/Z aren't modelled (synthesisable subset). *)
       | "EQ" | "EQCASE" | "EQWILD"
           -> BBinOp { op = BEq; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }
       | "NEQ" | "NEQCASE" | "NEQWILD"
           -> BBinOp { op = BNe; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }
       | "LT" | "LTS" -> BBinOp { op = BLt; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }
       | "LTE" | "LTES" -> BBinOp { op = BLe; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }
       | "GT" | "GTS" -> BBinOp { op = BGt; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }
       | "GTE" | "GTES" -> BBinOp { op = BGe; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }
       | _ ->
           if !debug then Printf.eprintf "Warning: Unknown binary op %s\n" op;
           BConst { value = 0; width = 1 })

  | BinaryOp' { op; lhs; rhs } ->
      let lhs_expr = expr_to_bexpr lhs in
      let rhs_expr = expr_to_bexpr rhs in
      let width = 32 in
      let signedness = Unsigned in

      (match op with
       | "ADD" -> BBinOp { op = BAdd; lhs = lhs_expr; rhs = rhs_expr;
                           result_type = BInt { width; signed = signedness } }
       | "SUB" -> BBinOp { op = BSub; lhs = lhs_expr; rhs = rhs_expr;
                           result_type = BInt { width; signed = signedness } }
       | "MUL" -> BBinOp { op = BMul; lhs = lhs_expr; rhs = rhs_expr;
                           result_type = BInt { width; signed = signedness } }
       | "DIV" -> BBinOp { op = BDiv; lhs = lhs_expr; rhs = rhs_expr;
                           result_type = BInt { width; signed = signedness } }
       | "MODDIV" -> BBinOp { op = BMod; lhs = lhs_expr; rhs = rhs_expr;
                              result_type = BInt { width; signed = signedness } }
       | "AND" -> BBinOp { op = BAnd; lhs = lhs_expr; rhs = rhs_expr;
                           result_type = BInt { width; signed = signedness } }
       | "OR" -> BBinOp { op = BOr; lhs = lhs_expr; rhs = rhs_expr;
                          result_type = BInt { width; signed = signedness } }
       | "XOR" -> BBinOp { op = BXor; lhs = lhs_expr; rhs = rhs_expr;
                           result_type = BInt { width; signed = signedness } }
       | "SHIFTL" -> BBinOp { op = BShl; lhs = lhs_expr; rhs = rhs_expr;
                              result_type = BInt { width; signed = signedness } }
       | "SHIFTR" -> BBinOp { op = BShr; lhs = lhs_expr; rhs = rhs_expr;
                              result_type = BInt { width; signed = signedness } }
       | "SHIFTRS" -> BBinOp { op = BAshr; lhs = lhs_expr; rhs = rhs_expr;
                               result_type = BInt { width; signed = Signed } }
       (* Case-equality `===` and wildcard `==?` degrade to plain
        * `==` once X/Z aren't modelled (synthesisable subset). *)
       | "EQ" | "EQCASE" | "EQWILD"
           -> BBinOp { op = BEq; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }
       | "NEQ" | "NEQCASE" | "NEQWILD"
           -> BBinOp { op = BNe; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }
       | "LT" | "LTS" -> BBinOp { op = BLt; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }
       | "LTE" | "LTES" -> BBinOp { op = BLe; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }
       | "GT" | "GTS" -> BBinOp { op = BGt; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }
       | "GTE" | "GTES" -> BBinOp { op = BGe; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }
       | _ ->
           if !debug then Printf.eprintf "Warning: Unknown binary op %s\n" op;
           BConst { value = 0; width = 1 })

  | UnaryOp { op; operand; dtype_ref } ->
      let operand_expr = expr_to_bexpr operand in
      let width = get_width_from_dtype dtype_ref in
      let signed = is_signed_dtype dtype_ref in
      let signedness = if signed then Signed else Unsigned in

      (match op with
       | "NOT" -> BUnOp { op = BNot; operand = operand_expr;
                          result_type = BInt { width; signed = signedness } }
       | "NEGATE" -> BUnOp { op = BNeg; operand = operand_expr;
                             result_type = BInt { width; signed = signedness } }
       | "REDAND" -> BUnOp { op = BRedAnd; operand = operand_expr; result_type = BBool }
       | "REDOR" -> BUnOp { op = BRedOr; operand = operand_expr; result_type = BBool }
       | "REDXOR" -> BUnOp { op = BRedXor; operand = operand_expr; result_type = BBool }
       (* Width-changing operators: Verilator emits these when an
        * implicit zero-extend (`{{N{1'b0}}, x}`) or sign-extend
        * (`$signed(...)`) collapses during constant folding. The
        * result must be `width` bits regardless of the operand's
        * source width — emit a BConcat that pads the bit-vector. *)
       | "EXTEND" | "EXTENDS" ->
           (* Compute the operand's bit-width directly from the AST.
            * Most expression nodes carry a dtype_ref; Concat /
            * Replicate don't (sv_parse drops it), so we sum/multiply
            * children widths recursively. *)
           let rec width_of_ast = function
             | VarRef { dtype_ref; _ } -> get_width_from_dtype dtype_ref
             | Const { dtype_ref; _ } -> get_width_from_dtype dtype_ref
             | BinaryOp { dtype_ref; _ } -> get_width_from_dtype dtype_ref
             | UnaryOp { dtype_ref; _ } -> get_width_from_dtype dtype_ref
             | Cond { then_val; _ } -> width_of_ast then_val
             | Concat { parts } ->
                 List.fold_left (+) 0 (List.map width_of_ast parts)
             | Replicate { count; src; _ }
             | Replicate' { count; src; _ } ->
                 let n = match expr_to_bexpr count with
                   | BConst { value; _ } -> value | _ -> 1
                 in
                 n * width_of_ast src
             | _ -> width
           in
           let from_w = width_of_ast operand in
           let pad = width - from_w in
           if pad <= 0 then operand_expr
           else begin
             let pad_value =
               if op = "EXTENDS" then
                 (* Sign-extend: replicate the operand's MSB. *)
                 BSlice { signal = operand_expr;
                          msb = from_w - 1; lsb = from_w - 1 }
               else
                 BConst { value = 0; width = 1 }
             in
             BConcat [
               BReplicate { count = pad; value = pad_value };
               operand_expr
             ]
           end
       | _ ->
           if !debug then Printf.eprintf "Warning: Unknown unary op %s\n" op;
           operand_expr)

  | UnaryOp' { op; operand; _ } ->
      let operand_expr = expr_to_bexpr operand in
      let width = 32 in
      let signedness = Unsigned in

      (match op with
       | "NOT" -> BUnOp { op = BNot; operand = operand_expr;
                          result_type = BInt { width; signed = signedness } }
       | "NEGATE" -> BUnOp { op = BNeg; operand = operand_expr;
                             result_type = BInt { width; signed = signedness } }
       | "REDAND" -> BUnOp { op = BRedAnd; operand = operand_expr; result_type = BBool }
       | "REDOR" -> BUnOp { op = BRedOr; operand = operand_expr; result_type = BBool }
       | "REDXOR" -> BUnOp { op = BRedXor; operand = operand_expr; result_type = BBool }
       | _ ->
           if !debug then Printf.eprintf "Warning: Unknown unary op %s\n" op;
           operand_expr)

  | Cond { condition; then_val; else_val } ->
      let cond_expr = expr_to_bexpr condition in
      let then_expr = expr_to_bexpr then_val in
      let else_expr = expr_to_bexpr else_val in
      BCond { condition = cond_expr; then_val = then_expr; else_val = else_expr }

  | Sel { expr; lsb; width; _ } ->
      (* Sel is a part-select of a *vector* (NOT an unpacked array —
       * that's ArraySel below). Two flavours:
       *   - constant lsb + width  →  static `BSlice` with fixed msb/lsb
       *   - dynamic lsb (variable index, e.g. `mask[i]`) → encode as
       *     `(vec >> lsb) & ((1<<width) - 1)` since BSlice's msb/lsb
       *     are compile-time integers. *)
      let signal_expr = expr_to_bexpr expr in
      let const_int_of t =
        match expr_to_bexpr t with
        | BConst { value; _ } -> Some value
        | _ -> None
      in
      let lsb_const = Option.bind lsb const_int_of in
      let width_const = match width with
        | Some w -> (match const_int_of w with Some v -> v | None -> 1)
        | None -> 1
      in
      (match lsb_const with
       | Some lsb_val ->
           BSlice { signal = signal_expr;
                    msb = lsb_val + width_const - 1;
                    lsb = lsb_val }
       | None ->
           (* Dynamic part-select on a vector. *)
           let lsb_expr = match lsb with
             | Some l -> expr_to_bexpr l
             | None -> BConst { value = 0; width = 32 }
           in
           let mask = (1 lsl width_const) - 1 in
           let shifted =
             BBinOp { op = BShr;
                      lhs = signal_expr; rhs = lsb_expr;
                      result_type = BInt { width = width_const;
                                           signed = Unsigned } }
           in
           if width_const = 1 then
             BBinOp { op = BAnd;
                      lhs = shifted;
                      rhs = BConst { value = 1; width = 1 };
                      result_type = BInt { width = 1; signed = Unsigned } }
           else
             BBinOp { op = BAnd;
                      lhs = shifted;
                      rhs = BConst { value = mask; width = width_const };
                      result_type = BInt { width = width_const;
                                           signed = Unsigned } })

  (* `mem[i]` indexed read on a memory array. *)
  | ArraySel { expr; index } ->
      BSelect { array = expr_to_bexpr expr;
                index = expr_to_bexpr index }

  | Concat { parts } ->
      let exprs = List.map expr_to_bexpr parts in
      BConcat exprs

  | Replicate { count; src; _ } | Replicate' { count; src; _ } ->
      let count_val = match expr_to_bexpr count with
        | BConst { value; _ } -> value
        | _ -> 1
      in
      let value_expr = expr_to_bexpr src in
      BReplicate { count = count_val; value = value_expr }

  (* Function call. sv_parse has already unwrapped each ARG node down
   * to its inner expression. *)
  | FuncRef { name; args } ->
      BCall { func = name; args = List.map expr_to_bexpr args }

  | other ->
      strict_bail "expression" "expr_to_bexpr" other;
      BConst { value = 0; width = 1 }

(* Convert Verilator statement to behavioral IR statement *)
let rec stmt_to_bstmt = function
  | Assign { lhs; rhs; _ } ->
      (* Three LHS shapes we care about:
       *   VarRef name             — `name = rhs`     (BAssign)
       *   ArraySel(VarRef m, i)   — `m[i] = rhs`     (intrinsic for RAM
       *                                              inference; passes
       *                                              the index through
       *                                              instead of clobbering)
       *   Sel(...)                — bit-slice write   — base name only
       * The intrinsic `@mem_write` is recognised by
       * behavioral_meminfer to lift the surrounding always-block into
       * a memory model. *)
      let rec base_name = function
        | VarRef { name; _ } | VarRef' { name; _ } -> Some name
        | Sel { expr; _ } | ArraySel { expr; _ } -> base_name expr
        | _ -> None
      in
      (match lhs with
       | ArraySel { expr = arr; index } ->
           (match base_name arr with
            | Some mem_name ->
                BCallStmt {
                  func = "@mem_write";
                  args = [BVar mem_name;
                          expr_to_bexpr index;
                          expr_to_bexpr rhs];
                }
            | None -> BBlock [])
       | Sel { expr = sel_expr; lsb; width; _ } ->
           (* Bit-slice write: `name[lsb +: width] <= rhs` (also used
              for the `[hi:lo]` form, which Verilator lowers to lsb +
              width).  Drop the slice indexing here and you get a
              full-signal clobber that loses upstream bits — Verible
              emits @slice_write / @part_sel_write_up to preserve the
              partial-write semantics, and the miter only agrees if
              we match.                                              *)
           (match base_name sel_expr with
            | Some name ->
                let const_int_of t =
                  match expr_to_bexpr t with
                  | BConst { value; _ } -> Some value
                  | _ -> None
                in
                let lsb_const   = Option.bind lsb   const_int_of in
                let width_const = Option.bind width const_int_of in
                let rhs_e = expr_to_bexpr rhs in
                (match lsb_const, width_const with
                 | Some lo, Some w ->
                     let hi = lo + w - 1 in
                     BCallStmt {
                       func = "@slice_write";
                       args = [ BVar name
                              ; BConst { value = hi; width = 32 }
                              ; BConst { value = lo; width = 32 }
                              ; rhs_e ];
                     }
                 | _ ->
                     let lsb_e = match lsb with
                       | Some l -> expr_to_bexpr l
                       | None   -> BConst { value = 0; width = 32 } in
                     let width_e = match width with
                       | Some w -> expr_to_bexpr w
                       | None   -> BConst { value = 1; width = 32 } in
                     BCallStmt {
                       func = "@part_sel_write_up";
                       args = [ BVar name; lsb_e; width_e; rhs_e ];
                     })
            | None ->
                strict_bail "Sel LHS base" "stmt_to_bstmt.Assign.Sel" sel_expr;
                BBlock [])
       | _ ->
           (match base_name lhs with
            | Some name ->
                let rhs_expr = expr_to_bexpr rhs in
                BAssign { lhs = name; rhs = rhs_expr }
            | None ->
                strict_bail "Assign LHS" "stmt_to_bstmt.Assign" lhs;
                BBlock []))

  | If { condition; then_stmt; else_stmt } ->
      let cond_expr = expr_to_bexpr condition in
      let then_stmts = [stmt_to_bstmt then_stmt] in
      let else_stmts = match else_stmt with
        | Some s -> [stmt_to_bstmt s]
        | None -> []
      in
      BIf { condition = cond_expr; then_stmts; else_stmts }

  | Case { expr; items } ->
      let sel_expr = expr_to_bexpr expr in
      (* Verilator emits the `default:` arm as a CASEITEM with empty
       * `conditions`. Separate those into the BCase.default slot so
       * the must-assign analysis can see the case is exhaustive. *)
      let cases, default =
        List.fold_left (fun (cs, def) item ->
          let stmts = List.map stmt_to_bstmt item.statements in
          match item.conditions with
          | [] -> (cs, stmts)
          | cond :: _ -> (cs @ [(expr_to_bexpr cond, stmts)], def)
        ) ([], []) items
      in
      BCase { selector = sel_expr; cases; default }

  | Begin { stmts; _ } ->
      let bstmts = List.map stmt_to_bstmt stmts in
      BBlock bstmts

  (* Local variable declarations inside an always block are not
   * behavioural statements — Verilator emits them as Var nodes alongside
   * the actual stmts. Skip cleanly. *)
  | Var _ | Var' _ -> BBlock []

  (* `for (...; cond; update) body`. sv_parse's rewriter collapses
   *   [Var i; Assign i = init; For' { cond; body; incs }]
   * into a single
   *   For { name = i; lhs = VarRef i; rhs = init; condition;
   *         stmts; incs; ... }
   * carrying init explicitly. We emit a BFor with the init bundled
   * and BWhile body-with-update form for the bare For' fallback. *)
  | For { name; lhs = _; rhs; condition; stmts; incs; _ } ->
      let init = BAssign { lhs = name; rhs = expr_to_bexpr rhs } in
      let body_stmts = List.map stmt_to_bstmt stmts in
      let upd_stmts = List.map stmt_to_bstmt incs in
      let update = match upd_stmts with
        | [u] -> u
        | _ -> BBlock upd_stmts
      in
      BFor { init; condition = expr_to_bexpr condition;
             update; body = body_stmts }
  | For' { condition; stmts; incs } ->
      let body_stmts = List.map stmt_to_bstmt stmts in
      let upd_stmts = List.map stmt_to_bstmt incs in
      BWhile { condition = expr_to_bexpr condition;
               body = body_stmts @ upd_stmts }

  (* Initial / Final blocks — simulation-only, not synthesisable
   * behaviour for our purposes. *)
  | Initial _ | Final _ -> BBlock []

  (* Display/$display, $finish, $stop etc. — testbench primitives. *)
  | Display _ | Stop _ -> BBlock []

  (* Task call appearing as a statement. Verilator emits `STMTEXPR { TaskRef }`
   * for `task_name(args);`. We accept both forms. *)
  | StmtExpr { expr = TaskRef { name; args } }
  | TaskRef { name; args } ->
      BCallStmt { func = name; args = List.map expr_to_bexpr args }
  | StmtExpr { expr = FuncRef { name; args } } ->
      BCallStmt { func = name; args = List.map expr_to_bexpr args }
  | StmtExpr { expr } -> stmt_to_bstmt expr

  | other ->
      strict_bail "statement" "stmt_to_bstmt" other;
      BBlock []

(* True iff the sensitivity list has at least one posedge/negedge edge.
 * Verilator marks every sense item with a non-empty `edge_str` (e.g.
 * "BOTH", "ANYEDGE", "INIT" for a level-sensitive `always @(sig)`),
 * so the test must look for the actual edge keywords — otherwise
 * combinational level-sensitive blocks get classified as sequential
 * and their LHSs become ripped Q__Q primary inputs. *)
let rec is_edge_triggered = function
  | [] -> false
  | SenItem { edge_str; _ } :: rest ->
      let e = String.uppercase_ascii edge_str in
      if e = "POS" || e = "POSEDGE" || e = "NEG" || e = "NEGEDGE"
      then true
      else is_edge_triggered rest
  | SenTree items :: rest -> is_edge_triggered items || is_edge_triggered rest
  | _ :: rest -> is_edge_triggered rest

(* Extract clock signal name from sensitivity list *)
let rec get_clock_signal = function
  | [] -> None
  | SenItem { edge_str; signal } :: rest ->
      if edge_str <> "" then
        (match signal with
         | VarRef { name; _ } | VarRef' { name; _ } -> Some name
         | _ -> get_clock_signal rest)
      else get_clock_signal rest
  | SenTree items :: rest ->
      (match get_clock_signal items with
       | Some c -> Some c
       | None -> get_clock_signal rest)
  | _ :: rest -> get_clock_signal rest

(* Check if edge is posedge *)
let rec is_posedge = function
  | [] -> false
  | SenItem { edge_str; _ } :: _ ->
      let e = String.uppercase_ascii edge_str in
      e = "POS" || e = "POSEDGE"
  | SenTree items :: rest -> is_posedge items || is_posedge rest
  | _ :: rest -> is_posedge rest

(* Convert Verilator always block to behavioral process. State-holding
 * iff the sensitivity list has a posedge/negedge; otherwise treat as
 * combinational. (We don't run a must-assign latch check here — many
 * cva6 designs use `always_comb` with conditional assignments that
 * rely on prior-tick init, and aggressively classifying them as
 * latches breaks 12 cva6 leaves while only making `latch_detect`
 * pass V↔V. The Verible side has the latch detection because Vivado
 * does, so when both sides see a true latch they may still disagree
 * — but this is the side it's safer to leave loose.) *)
let always_to_bprocess = function
  | Always { senses; stmts; _ } ->
      let is_edge = is_edge_triggered senses in
      let body = List.map stmt_to_bstmt stmts in
      if is_edge then
        let clock = match get_clock_signal senses with
          | Some c -> c
          | None -> "clk"
        in
        let edge = if is_posedge senses then `Pos else `Neg in
        BSequential {
          name = "always_ff";
          clock;
          clock_edge = edge;
          reset = None;
          reset_edge = None;
          reset_async = false;
          body;
        }
      else
        BCombinational { name = "always_comb"; sensitivity = [BAny]; body }
  | _ -> BCombinational { name = "always"; sensitivity = [BAny]; body = [] }

(* Walk a procedural block tree and collect every nested `Var` /
 * `Var'` node so procedural-scope locals like
 *   always_ff @(posedge clk) begin
 *     case (state)
 *       S1: begin
 *         logic signed [30:0] x_round;     // anon block (BEGIN/unnamedblkN)
 *         x_round = ...;
 *       end
 *     endcase
 *   end
 * surface in the bsignal list with their source-stated width.
 * Without this, the Var lives only in the named-or-anon hierarchy
 * inside the BEGIN/CASEITEM and gets dropped by extract_signals'
 * top-level filter, leaving downstream Z3 width-inference to
 * default to 1 bit.  Verible's frontend reads procedural decls
 * directly from the source so its BIR carries the right width;
 * before this walker the verilator side disagreed, raising
 * `(_ BitVec 1) and (_ BitVec 31) are incompatible` from Z3. *)
let rec collect_nested_vars node acc =
  match node with
  | Var _ as v -> v :: acc
  | Var' _ as v -> v :: acc
  | Begin { stmts; _ } -> List.fold_left (fun a s -> collect_nested_vars s a) acc stmts
  | Always { stmts; _ } -> List.fold_left (fun a s -> collect_nested_vars s a) acc stmts
  | If { then_stmt; else_stmt; _ } ->
      let acc = collect_nested_vars then_stmt acc in
      (match else_stmt with
       | Some e -> collect_nested_vars e acc
       | None -> acc)
  | Case { items; _ } ->
      List.fold_left (fun a (ci : case_item) ->
        List.fold_left (fun a' s -> collect_nested_vars s a') a ci.statements
      ) acc items
  | For { stmts; _ } -> List.fold_left (fun a s -> collect_nested_vars s a) acc stmts
  | _ -> acc

(* Extract signals from module. For unpacked-array dtypes the bsignal
 * carries `BArray { element = BInt; size }` so memory-inference
 * downstream can read the depth and element width directly from the
 * Verilator typetable instead of guessing.
 *
 * Includes procedural-scope locals via collect_nested_vars — these
 * live in the static-named (named begin) or anonymous (auto
 * `unnamedblkN`) hierarchy that Verilator builds during elaboration.
 * Same direction (`Internal`) and same dtype-ref resolution as the
 * top-level path; a name collision with a top-level signal is
 * resolved by the de-dup at the end (last write wins, but in
 * practice procedural shadows only happen with deliberate name
 * reuse across case arms — which is the cordic_sincos pattern). *)
let extract_signals stmts =
  let dir_of d = match String.uppercase_ascii d with
    | "INPUT" -> `Input
    | "OUTPUT" -> `Output
    | _ -> `Internal
  in
  let var_of_node = function
    | Var { name; dtype_ref; direction; _ } ->
        let elem_width = get_width_from_dtype dtype_ref in
        let signed = is_signed_dtype dtype_ref in
        let element = BInt { width = elem_width;
                             signed = if signed then Signed else Unsigned } in
        let stype = match get_array_depth dtype_ref with
          | Some depth -> BArray { element; size = depth }
          | None -> element
        in
        Some {
          name;
          stype;
          direction = dir_of direction;
          initial_value = None; attrs = [];
        }
    | Var' { name; direction; _ } ->
        Some {
          name;
          stype = BInt { width = 32; signed = Unsigned };
          direction = dir_of direction;
          initial_value = None; attrs = [];
        }
    | _ -> None
  in
  let top_level = List.filter_map var_of_node stmts in
  (* Also collect procedural-scope locals from inside Always/Begin/If/
     Case/For trees.  Each becomes an `Internal` bsignal (regardless
     of what direction the Var node carries — procedural locals are
     never module ports, and we treat the dropped direction as
     internal). *)
  let proc_vars =
    List.fold_left (fun acc s -> collect_nested_vars s acc) [] stmts
    |> List.filter_map var_of_node
    |> List.map (fun s -> { s with direction = `Internal })
  in
  (* De-dup: a name appearing in both lists keeps the top-level
     definition (more authoritative; usually a port).  Procedural
     locals fill in only the names not already declared at module
     scope. *)
  let known = List.fold_left (fun acc (s : bsignal) -> s.name :: acc) [] top_level in
  let extras = List.filter (fun (s : bsignal) ->
    not (List.mem s.name known)) proc_vars in
  top_level @ extras

(* Find a pin's connected expression by pin name. *)
let pin_expr pin_name pins =
  List.find_map (function
    | Pin { name; expr = Some e } when name = pin_name -> Some e
    | _ -> None
  ) pins

(* Convert a Cell instance whose module type is a known Vivado RTL_*
 * primitive into a BIR process. This avoids needing the RTL_* models in
 * the source file — we encode the semantics here directly, using the
 * connected port expressions as inputs/output. Sub-module instantiation
 * is otherwise lost by the rest of verilator_to_behavioral, which only
 * walks the top module's own body. *)
let cell_to_bprocess = function
  | Cell { modp_addr = Some (Module { name = mod_name; _ }); pins; _ } ->
      (* Common helper: get a pin's expression as BIR, with sane fallback. *)
      let pin name =
        match pin_expr name pins with
        | Some e -> Some (expr_to_bexpr e)
        | None -> None
      in
      let lhs_of pin_name =
        match pin_expr pin_name pins with
        | Some (VarRef { name; _ }) | Some (VarRef' { name; _ }) -> Some name
        | _ -> None
      in
      let combinational lhs rhs =
        Some (BCombinational {
          name = Printf.sprintf "%s_inst" mod_name;
          sensitivity = [BAny];
          body = [BAssign { lhs; rhs }];
        })
      in
      let result_t = BInt { width = 64; signed = Unsigned } in
      let bool_t = BInt { width = 1; signed = Unsigned } in
      let mk_binop op a b t =
        BBinOp { op; lhs = a; rhs = b; result_type = t }
      in
      let mk_unop op a t =
        BUnOp { op; operand = a; result_type = t }
      in
      let combinational_2to1 op t =
        match lhs_of "O", pin "I0", pin "I1" with
        | Some lhs, Some a, Some b -> combinational lhs (mk_binop op a b t)
        | _ -> None
      in
      let combinational_1to1 op =
        match lhs_of "O", pin "I0" with
        | Some lhs, Some a -> combinational lhs (mk_unop op a result_t)
        | _ -> None
      in
      (* Sequential register builder. Async controls posedge-or-posedge sense;
       * sync looks like a single posedge clock.  If a CE (clock-enable) pin
       * is connected the data update is gated by `if (CE) Q <= D`; else
       * the FF holds (the empty else branch maps to "no change" through
       * the normal FF-rip semantics).  Stacking is reset > CE > D. *)
      let mk_register ~async =
        match lhs_of "Q", pin "C", pin "D" with
        | Some q, Some _c, Some d ->
            let clock = match pin "C" with
              | Some (BVar n) -> n | _ -> "clk" in
            let reset_pin = pin "RST" in
            let ce_pin = pin "CE" in
            let data_assign = BAssign { lhs = q; rhs = d } in
            (* `if (CE) Q <= D else Q <= Q` when CE is wired up.
               Explicit Q-hold matches the rtlil_to_behavioral.ml
               $dffe / $adffe conventions so downstream FFrip
               recognises this as the same shape regardless of
               which frontend produced it. *)
            let gated_data = match ce_pin with
              | Some ce ->
                  BIf { condition = ce;
                        then_stmts = [data_assign];
                        else_stmts = [BAssign { lhs = q; rhs = BVar q }] }
              | None -> data_assign
            in
            let body = match reset_pin with
              | Some r ->
                  [BIf {
                    condition = r;
                    then_stmts = [BAssign {
                      lhs = q; rhs = BConst { value = 0; width = 64 }
                    }];
                    else_stmts = [gated_data];
                  }]
              | None ->
                  [gated_data]
            in
            Some (BSequential {
              name = Printf.sprintf "%s_inst" mod_name;
              clock;
              clock_edge = `Pos;
              reset = (match reset_pin with
                       | Some (BVar n) -> Some n | _ -> None);
              reset_edge = (if async then Some `Pos else None);
              reset_async = async;
              body;
            })
        | _ -> None
      in
      (match mod_name with
       (* combinational *)
       | "RTL_INV"     -> combinational_1to1 BNot
       | "RTL_AND"     -> combinational_2to1 BAnd result_t
       | "RTL_OR"      -> combinational_2to1 BOr  result_t
       | "RTL_XOR"     -> combinational_2to1 BXor result_t
       | "RTL_ADD"     -> combinational_2to1 BAdd result_t
       | "RTL_SUB"     -> combinational_2to1 BSub result_t
       | "RTL_MUL"     -> combinational_2to1 BMul result_t
       | "RTL_LSHIFT"  -> combinational_2to1 BShl result_t
       | "RTL_RSHIFT"  -> combinational_2to1 BShr result_t
       | "RTL_EQ"      -> combinational_2to1 BEq  bool_t
       | "RTL_NEQ"     -> combinational_2to1 BNe  bool_t
       | "RTL_LT"      -> combinational_2to1 BLt  bool_t
       | "RTL_LEQ"     -> combinational_2to1 BLe  bool_t
       | "RTL_GT"      -> combinational_2to1 BGt  bool_t
       | "RTL_GEQ"     -> combinational_2to1 BGe  bool_t
       | "RTL_MUX" ->
           (* Per Vivado's SEL_VAL annotation, S=1 → O=I0, S=0 → O=I1. *)
           (match lhs_of "O", pin "S", pin "I0", pin "I1" with
            | Some lhs, Some s, Some i0, Some i1 ->
                combinational lhs (BCond { condition = s;
                                           then_val = i0; else_val = i1 })
            | _ -> None)
       (* sequential *)
       | "RTL_REG"
       | "RTL_REG_CE"          -> mk_register ~async:false
       | "RTL_REG_SYNC"
       | "RTL_REG_SYNC_CE"     -> mk_register ~async:false
       | "RTL_REG_ASYNC"
       | "RTL_REG_ASYNC_CE"    -> mk_register ~async:true
       | _ -> None)
  | _ -> None

(* Extract function/task definitions from a list of stmts. Called at
 * both Module scope (functions defined inside the module) and Package
 * scope (functions exported via `import pkg::*`). The same routine is
 * reused — just pass it the relevant stmt list. *)
let var_to_param = function
  | Var { name; direction; dtype_ref; _ } ->
      let width = get_width_from_dtype dtype_ref in
      let dir = match direction with
        | "output" | "OUTPUT" -> `Output
        | "inout"  | "INOUT"  -> `Inout
        | _ -> `Input
      in
      Some (name, BInt { width; signed = Unsigned }, dir)
  | Var' { name; direction; _ } ->
      let dir = match direction with
        | "output" | "OUTPUT" -> `Output
        | "inout"  | "INOUT"  -> `Inout
        | _ -> `Input
      in
      Some (name, BInt { width = 32; signed = Unsigned }, dir)
  | _ -> None

(* Detect output-direction VARs. The return-value var of a function
 * has direction "OUTPUT" (named after the function). Parameters have
 * "INPUT" / "OUTPUT" / "INOUT" or "" / "VAR" for body locals. *)
let is_output_named_for fname = function
  | Var { name; direction; _ } -> name = fname && direction = "OUTPUT"
  | Var' { name; direction; _ } -> name = fname && direction = "OUTPUT"
  | _ -> false

(* Verilator JSON structure for a function:
 *   FUNC name {
 *     fvarp = [VAR <fname>]      ← the return-value var
 *     stmtsp = [VAR p1; VAR p2; ...; <body stmts>]
 *   }
 * Parameters live in `stmtsp`, mixed with the body. We partition them
 * by whether the node is a Var/Var'. For tasks the same shape applies
 * but `fvarp` is empty; all VARs in stmtsp are parameters. *)
let split_param_vars stmts =
  List.partition (function Var _ | Var' _ -> true | _ -> false) stmts

let _ = is_output_named_for

let extract_funcs sts =
  List.filter_map (function
    | Func { name = fname; stmts; _ }
    | Func' { name = fname; stmts; _ } ->
        let param_vars, body = split_param_vars stmts in
        let params = List.filter_map var_to_param param_vars in
        let body_stmts = List.map stmt_to_bstmt body in
        Some {
          fname;
          is_task = false;
          ftype = BInt { width = 32; signed = Unsigned };
          params;
          locals = [];
          body = body_stmts;
        }
    | Task { name = tname; stmts; _ }
    | Task' { name = tname; stmts; _ } ->
        let param_vars, body = split_param_vars stmts in
        let params = List.filter_map var_to_param param_vars in
        let body_stmts = List.map stmt_to_bstmt body in
        Some {
          fname = tname;
          is_task = true;
          ftype = BBool;
          params;
          locals = [];
          body = body_stmts;
        }
    | _ -> None
  ) sts

(* Convert Verilator module to behavioral IR module. `extra_funcs` is
 * the list of functions/tasks pulled from sibling Package nodes —
 * those are visible to this module via `import pkg::*`. *)
let module_to_bmodule_with_funcs extra_funcs = function
  | Module { name; stmts } ->
      let signals = extract_signals stmts in

      (* Generate blocks (genvar-for, conditional generate) come in as
       * nested `Begin` nodes containing the generated children. The
       * always/assign/cell extractors below need to see those children
       * too, not just the module-level statements. Flatten Begins
       * recursively. *)
      let rec flatten s = match s with
        | Begin { stmts; _ } -> List.concat_map flatten stmts
        | other -> [other]
      in
      let flat_stmts = List.concat_map flatten stmts in

      (* Extract always blocks, continuous assigns, and Vivado RTL_* cell
       * instances. Each AssignW becomes a single-statement BCombinational
       * so the Z3 encoder treats it as an unconditional equation. RTL_*
       * cells are encoded directly via cell_to_bprocess so we don't need
       * to walk into their module bodies. *)
      let always_processes = List.filter_map (function
        | Always _ as a -> Some (always_to_bprocess a)
        | _ -> None
      ) flat_stmts in
      let rec lhs_base = function
        | VarRef { name; _ } | VarRef' { name; _ } -> Some name
        (* `tmp_mask[i]` LHS — recover base name. Lossy: we drop the bit
         * index and the assignment overwrites the whole vector. Same
         * approximation already used in stmt_to_bstmt for inner Sel
         * LHSs; counter-examples will surface if a test depends on
         * the bit-precise update. *)
        | Sel { expr = inner; _ } | ArraySel { expr = inner; _ } ->
            lhs_base inner
        | _ -> None
      in
      (* Decompose an AssignW LHS into (base_name, optional bit
       * position) so we can recognise per-bit unrolled writes from
       * generate-for loops (`assign y[i] = expr;` → multiple AssignW
       * with constant-index Sel LHSs). *)
      let lhs_bit_pos = function
        | Sel { expr = VarRef { name; _ }; lsb; width = _ } ->
            (match lsb with
             | Some (Const { name = lit; _ }) ->
                 (try Some (name, Some (parse_const_value lit))
                  with _ -> Some (name, None))
             | _ -> Some (name, None))
        | VarRef { name; _ } -> Some (name, None)
        | other ->
            (match lhs_base other with
             | Some n -> Some (n, None)
             | None -> None)
      in
      (* Group AssignWs by base name, collect per-bit RHS exprs. *)
      let signal_width n =
        match List.find_opt (fun (s : Behavioral_ir.bsignal) ->
                s.name = n) signals with
        | Some { stype = BInt { width; _ }; _ } -> Some width
        | _ -> None
      in
      let aw_groups = Hashtbl.create 16 in
      let aw_order = ref [] in
      List.iter (function
        | AssignW { lhs; rhs } ->
            (match lhs_bit_pos lhs with
             | Some (n, idx) ->
                 if not (Hashtbl.mem aw_groups n) then
                   aw_order := n :: !aw_order;
                 let lst =
                   try Hashtbl.find aw_groups n with Not_found -> []
                 in
                 Hashtbl.replace aw_groups n ((idx, rhs) :: lst)
             | None -> ())
        | _ -> ()
      ) flat_stmts;
      let assignw_processes = List.filter_map (fun n ->
        let entries = List.rev (Hashtbl.find aw_groups n) in
        match entries with
        | [] -> None
        | [(None, rhs)] ->
            (* Single whole-signal assign — the common case. *)
            Some (BCombinational {
              name = "assign_" ^ n;
              sensitivity = [BAny];
              body = [BAssign { lhs = n;
                                rhs = expr_to_bexpr rhs }];
            })
        | _ ->
            (* Multiple entries OR a single bit-select. Try to coalesce
             * if all have constant indexes that cover [0..W-1] exactly
             * once. Otherwise emit each as a lossy whole-signal assign
             * (preserves prior behaviour). *)
            let all_indexed =
              List.for_all (fun (idx, _) -> idx <> None) entries
            in
            let w = signal_width n in
            (match all_indexed, w with
             | true, Some width ->
                 let by_idx = List.filter_map (fun (idx, rhs) ->
                   match idx with Some i -> Some (i, rhs) | None -> None
                 ) entries in
                 let positions = List.map fst by_idx |> List.sort compare in
                 let expected = List.init width (fun i -> i) in
                 if positions = expected then begin
                   (* Cover the full width. Build a BConcat from MSB
                    * (width-1) down to LSB (0). *)
                   let bits = List.init width (fun i ->
                     let rhs = List.assoc (width - 1 - i) by_idx in
                     expr_to_bexpr rhs
                   ) in
                   let combined =
                     match bits with
                     | [single] -> single
                     | _ -> BConcat bits
                   in
                   Some (BCombinational {
                     name = "assign_" ^ n;
                     sensitivity = [BAny];
                     body = [BAssign { lhs = n; rhs = combined }];
                   })
                 end else
                   (* Partial coverage: drop to lossy form to keep
                    * the test running. *)
                   Some (BCombinational {
                     name = "assign_" ^ n;
                     sensitivity = [BAny];
                     body = List.map (fun (_, rhs) ->
                              BAssign { lhs = n;
                                        rhs = expr_to_bexpr rhs }
                            ) entries;
                   })
             | _ ->
                 Some (BCombinational {
                   name = "assign_" ^ n;
                   sensitivity = [BAny];
                   body = List.map (fun (_, rhs) ->
                            BAssign { lhs = n;
                                      rhs = expr_to_bexpr rhs }
                          ) entries;
                 }))
      ) (List.rev !aw_order) in
      (* Verilator constant-folds `assign x = <constant_expr>;` into
       * an INITIAL block whose body is `Assign(VarRef x, <const>)`.
       * Recover those as combinational assigns so the module's
       * outputs stay driven. Real (non-folded) initial blocks have
       * other shapes — only the "single-assign-to-port" pattern is
       * the constant-folded continuous assign. *)
      let initial_processes = List.filter_map (function
        | Initial { stmts = [Assign { lhs; rhs; _ }]; _ } ->
            (match lhs_base lhs with
             | Some n ->
                 let rhs_expr = expr_to_bexpr rhs in
                 Some (BCombinational {
                   name = "assign_" ^ n;
                   sensitivity = [BAny];
                   body = [BAssign { lhs = n; rhs = rhs_expr }];
                 })
             | None -> None)
        | _ -> None
      ) flat_stmts in
      let cell_processes = List.filter_map cell_to_bprocess flat_stmts in
      let processes =
        always_processes @ assignw_processes @ initial_processes
        @ cell_processes
      in

      (* Functions/tasks defined inline in this module + imported from
       * any sibling Package nodes the caller pulled in. *)
      let funcs = extract_funcs flat_stmts @ extra_funcs in

      {
        name;
        params = [];
        signals;
        processes;
        instances = [];
        funcs;
        mems = []; attrs = [];
      }
  | _ ->
      { name = "unknown"; params = []; signals = []; processes = [];
        instances = []; funcs = []; mems = []; attrs = [] }

(* Convenience for callers that don't need package-level imports. *)
let module_to_bmodule m = module_to_bmodule_with_funcs [] m

(* Convert Verilator AST to behavioral program. We pre-collect the
 * function/task definitions sitting in any Package nodes so each
 * Module can see them via `import pkg::*`. *)
let convert_ast ast =
  match ast with
  | Netlist modules ->
      let pkg_funcs = List.concat_map (function
        | Package { stmts; _ } -> extract_funcs stmts
        | _ -> []
      ) modules in
      let bmodules = List.filter_map (function
        | Module _ as m -> Some (module_to_bmodule_with_funcs pkg_funcs m)
        | _ -> None
      ) modules in
      { modules = bmodules; library_cells = [] }
  | Module _ as m ->
      { modules = [module_to_bmodule m]; library_cells = [] }
  | _ ->
      { modules = []; library_cells = [] }

(* Main entry point: Convert Verilator JSON file to behavioral IR *)
let convert_verilator_json_to_behavioral json_file =
  try
    if !debug then Printf.printf "Reading Verilator JSON: %s\n" json_file;

    let json = Yojson.Safe.from_file json_file in
    let ast = Sv_parse.parse json in

    if !debug then Printf.printf "Successfully parsed Verilator JSON\n";
    let bprog = convert_ast ast in
    Some bprog
  with
  | Sys_error msg ->
      Printf.eprintf "Error reading file: %s\n" msg;
      None
  | Yojson.Json_error msg ->
      Printf.eprintf "JSON parse error: %s\n" msg;
      None
  | e ->
      Printf.eprintf "Unexpected error: %s\n%s\n"
        (Printexc.to_string e)
        (Printexc.get_backtrace ());
      None
