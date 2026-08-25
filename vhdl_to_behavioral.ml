(* VHDL to Behavioral IR Converter
 *
 * Converts VHDL AST (VhdlTree) to language-neutral behavioral IR.
 * This eliminates VHDL-isms and provides a clean intermediate representation.
 *)

open Vhd_front.VhdlTree
open Behavioral_ir

(* Conversion context *)
type context = {
  mutable signal_types: (string * btype) list;
  mutable next_temp_id: int;
  (* Generic-parameter values folded at the entity boundary so range
     expressions like `WIDTH-1 downto 0` reduce to literal widths. *)
  mutable generics: (string * int) list;
  (* Record types declared in the architecture: type_name → list of
     (field_name, field_btype).  Populated by extract_decls before
     signal-decl processing so signals of record type can be
     expanded. *)
  mutable record_types: (string * (string * btype) list) list;
  (* User-declared SUBTYPES: `subtype t is std_logic_vector(3 downto 0);`
     name -> btype.  Without these a signal declared through a subtype fell to
     infer_btype's 1-bit default, so `q <= wrap_q` became
     `q = {3'b000, wrap_q[0]}` -- the vector silently narrowed to its LSB.
     GHDL's synthesis output declares its wrapper signals exactly this way, so
     every GHDL-derived reference read as near-constant. *)
  mutable subtypes: (string * btype) list;
  (* Signals in the current module that are of a record type:
     signal_name → record_type_name.  Used to split record-level
     assigns like `r <= rin` into per-field assigns. *)
  mutable record_signals: (string * string) list;
  (* Enum types: type_name → ordered list of value names.  Each
     value is encoded as its position; enum width = clog2(n). *)
  mutable enum_types: (string * string list) list;
  (* Inverse map: enum value name → (type_name, integer encoding). *)
  mutable enum_values: (string * (string * int)) list;
}

let create_context () = {
  signal_types = [];
  next_temp_id = 0;
  generics = [];
  record_types = [];
  subtypes = [];
  record_signals = [];
  enum_types = [];
  enum_values = [];
}

let bits_needed n =
  let rec loop p k = if p >= n then k else loop (p * 2) (k + 1) in
  if n <= 1 then 1 else loop 1 0

(* Fold a VHDL range bound (msb or lsb of a discrete range) into an
   integer when possible.  Handles literal constants plus simple
   arithmetic against bound generic-parameter names. *)
let rec fold_range_int ctx = function
  | Double (VhdIntPrimary, Num s) ->
      (try Some (int_of_string s) with _ -> None)
  | Str name ->
      (try Some (List.assoc name ctx.generics) with Not_found -> None)
  | Triple (VhdAddSimpleExpression, l, r) ->
      (match fold_range_int ctx l, fold_range_int ctx r with
       | Some a, Some b -> Some (a + b) | _ -> None)
  | Triple (VhdSubSimpleExpression, l, r) ->
      (match fold_range_int ctx l, fold_range_int ctx r with
       | Some a, Some b -> Some (a - b) | _ -> None)
  | Triple (VhdMultTerm, l, r) ->
      (match fold_range_int ctx l, fold_range_int ctx r with
       | Some a, Some b -> Some (a * b) | _ -> None)
  | Double (VhdParenthesedPrimary, e) -> fold_range_int ctx e
  | _ -> None

(* Extract bit width from a VHDL subtype_indication.
   Returns (width, signedness) when recognisable, else default 1-bit
   unsigned.  Mirrors vhdl_simple_to_ir.ml's infer_width but folds
   range bounds against ctx.generics so parameterised widths resolve. *)
let infer_btype ctx subty =
  let signed_of name = match name with
    | "signed" | "integer" -> Signed
    | _ -> Unsigned
  in
  let mk_range type_name hi_e lo_e =
    match fold_range_int ctx hi_e, fold_range_int ctx lo_e with
    | Some h, Some l ->
        let w = max 1 (abs (h - l) + 1) in
        BInt { width = w; signed = signed_of type_name }
    | _ -> BInt { width = 1; signed = Unsigned }
  in
  match subty with
  | Quadruple (Vhdsubtype_indication, _, Str "std_logic", VhdNoConstraint) ->
      BInt { width = 1; signed = Unsigned }
  | Quadruple (Vhdsubtype_indication, _, Str "std_ulogic", VhdNoConstraint) ->
      BInt { width = 1; signed = Unsigned }
  | Quadruple (Vhdsubtype_indication, _, Str "boolean", VhdNoConstraint) ->
      BInt { width = 1; signed = Unsigned }
  | Quadruple (Vhdsubtype_indication, _, Str "natural", _) ->
      BInt { width = 32; signed = Unsigned }
  | Quadruple (Vhdsubtype_indication, _, Str "integer", _) ->
      BInt { width = 32; signed = Signed }
  | Quadruple (Vhdsubtype_indication, _, Str type_name,
               Double (VhdArrayConstraint,
                 Triple (Vhdassociation_element, VhdFormalIndexed,
                   Double (VhdActualDiscreteRange,
                     Double (VhdRange,
                       Triple (VhdDecreasingRange, hi, lo)))))) ->
      mk_range type_name hi lo
  | Quadruple (Vhdsubtype_indication, _, Str type_name,
               Double (VhdArrayConstraint,
                 Triple (Vhdassociation_element, VhdFormalIndexed,
                   Double (VhdActualDiscreteRange,
                     Double (VhdRange,
                       Triple (VhdIncreasingRange, lo, hi)))))) ->
      mk_range type_name hi lo
  (* a user-declared subtype name, resolved from the architecture's decls *)
  | Quadruple (Vhdsubtype_indication, _, Str type_name, _)
    when List.mem_assoc type_name ctx.subtypes ->
      List.assoc type_name ctx.subtypes
  | _ -> BInt { width = 1; signed = Unsigned }

let fresh_temp ctx =
  let id = ctx.next_temp_id in
  ctx.next_temp_id <- id + 1;
  Printf.sprintf "_temp%d" id

let add_signal_type ctx name ty =
  ctx.signal_types <- (name, ty) :: ctx.signal_types

let get_signal_type ctx name =
  try List.assoc name ctx.signal_types
  with Not_found -> BInt { width = 32; signed = Unsigned }  (* Default *)

(* Convert VHDL expressions to behavioral IR expressions *)
(* An unhandled VHDL construct used to WARN and then substitute something
   plausible -- an expression became BConst 0, a statement became [], an
   architecture statement was skipped entirely.  Silent substitution is how a
   whole module comes out as a constant with its inputs unused, and how a
   cross-flow miter then reports confident nonsense: a vector built bit by bit
   read as 0 and every verdict involving it was noise for hours.  This is FATAL
   and deliberately has NO opt-out: an "allow unhandled" switch just recreates
   the silent-corruption path under a different name, and the first thing it
   gets used for is to push a broken design through one more stage. *)
let vhdl_unhandled kind shape =
  failwith
    (Printf.sprintf
       "[vhdl2bir] UNHANDLED %s: %s -- the reader has no rule for this \
        construct.  Continuing would silently substitute a constant or drop the \
        statement, giving a WRONG design that still looks well-formed.  Add a \
        rule for it." kind shape)

let rec expr_to_bexpr ctx = function
  (* Simple name — but enum literals look like simple names too.
     If `name` is a known enum value, fold it to a constant of the
     enum's encoding width (clog2 of value count). *)
  | Str name ->
      (match List.assoc_opt name ctx.enum_values with
       | Some (type_name, ord) ->
           let n =
             try List.length (List.assoc type_name ctx.enum_types)
             with Not_found -> 1 in
           BConst { value = Z.of_int ord; width = bits_needed n }
       | None -> BVar name)

  (* Integer literal *)
  | Double (VhdIntPrimary, Num num_str) ->
      (try
         let value = int_of_string num_str in
         BConst { value = Z.of_int value; width = 32 }
       with _ -> BConst { value = Z.zero; width = 32 })

  (* Bit string literal: "0001", "1100", etc. *)
  | Double (VhdOperatorString, Str bit_str) ->
      (try
         let value = int_of_string ("0b" ^ bit_str) in
         BConst { value = Z.of_int value; width = String.length bit_str }
       with _ -> BConst { value = Z.zero; width = 1 })

  (* Character literal: '0', '1' *)
  | Double (VhdCharPrimary, Char c) ->
      let value = if c = '1' then 1 else 0 in
      BConst { value = Z.of_int value; width = 1 }

  (* Relational operators *)
  | Triple (VhdEqualRelation, lhs, rhs) ->
      let lhs_expr = expr_to_bexpr ctx lhs in
      let rhs_expr = expr_to_bexpr ctx rhs in
      BBinOp { op = BEq; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }

  | Triple (VhdNotEqualRelation, lhs, rhs) ->
      let lhs_expr = expr_to_bexpr ctx lhs in
      let rhs_expr = expr_to_bexpr ctx rhs in
      BBinOp { op = BNe; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }

  | Triple (VhdLessRelation, lhs, rhs) ->
      let lhs_expr = expr_to_bexpr ctx lhs in
      let rhs_expr = expr_to_bexpr ctx rhs in
      BBinOp { op = BLt; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }

  | Triple (VhdGreaterRelation, lhs, rhs) ->
      let lhs_expr = expr_to_bexpr ctx lhs in
      let rhs_expr = expr_to_bexpr ctx rhs in
      BBinOp { op = BGt; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }

  | Triple (VhdLessOrEqualRelation, lhs, rhs) ->
      let lhs_expr = expr_to_bexpr ctx lhs in
      let rhs_expr = expr_to_bexpr ctx rhs in
      BBinOp { op = BLe; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }

  | Triple (VhdGreaterOrEqualRelation, lhs, rhs) ->
      let lhs_expr = expr_to_bexpr ctx lhs in
      let rhs_expr = expr_to_bexpr ctx rhs in
      BBinOp { op = BGe; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }

  (* Arithmetic operators *)
  | Triple (VhdAddSimpleExpression, lhs, rhs) ->
      let lhs_expr = expr_to_bexpr ctx lhs in
      let rhs_expr = expr_to_bexpr ctx rhs in
      BBinOp { op = BAdd; lhs = lhs_expr; rhs = rhs_expr;
               result_type = BInt { width = 32; signed = Unsigned } }

  | Triple (VhdSubSimpleExpression, lhs, rhs) ->
      let lhs_expr = expr_to_bexpr ctx lhs in
      let rhs_expr = expr_to_bexpr ctx rhs in
      BBinOp { op = BSub; lhs = lhs_expr; rhs = rhs_expr;
               result_type = BInt { width = 32; signed = Unsigned } }

  | Triple (VhdMultTerm, lhs, rhs) ->
      let lhs_expr = expr_to_bexpr ctx lhs in
      let rhs_expr = expr_to_bexpr ctx rhs in
      BBinOp { op = BMul; lhs = lhs_expr; rhs = rhs_expr;
               result_type = BInt { width = 32; signed = Unsigned } }

  | Triple (VhdDivTerm, lhs, rhs) ->
      BBinOp { op = BDiv;
               lhs = expr_to_bexpr ctx lhs; rhs = expr_to_bexpr ctx rhs;
               result_type = BInt { width = 32; signed = Unsigned } }

  | Triple (VhdModTerm, lhs, rhs) ->
      BBinOp { op = BMod;
               lhs = expr_to_bexpr ctx lhs; rhs = expr_to_bexpr ctx rhs;
               result_type = BInt { width = 32; signed = Unsigned } }

  | Triple (VhdRemTerm, lhs, rhs) ->
      BBinOp { op = BMod;
               lhs = expr_to_bexpr ctx lhs; rhs = expr_to_bexpr ctx rhs;
               result_type = BInt { width = 32; signed = Unsigned } }

  | Triple (VhdShiftLeftLogicalExpression, lhs, rhs)
  | Triple (VhdShiftLeftArithmeticExpression, lhs, rhs) ->
      BBinOp { op = BShl;
               lhs = expr_to_bexpr ctx lhs; rhs = expr_to_bexpr ctx rhs;
               result_type = BInt { width = 32; signed = Unsigned } }

  | Triple (VhdShiftRightLogicalExpression, lhs, rhs) ->
      BBinOp { op = BShr;
               lhs = expr_to_bexpr ctx lhs; rhs = expr_to_bexpr ctx rhs;
               result_type = BInt { width = 32; signed = Unsigned } }

  | Triple (VhdShiftRightArithmeticExpression, lhs, rhs) ->
      BBinOp { op = BAshr;
               lhs = expr_to_bexpr ctx lhs; rhs = expr_to_bexpr ctx rhs;
               result_type = BInt { width = 32; signed = Signed } }

  | Triple (VhdRotateLeftExpression, lhs, rhs)
  | Triple (VhdRotateRightExpression, lhs, rhs) ->
      (* No native rotate in BIR; approximate as shift — caller usually
         operates on width-bounded signals so the loss is silent. *)
      let _ = rhs in
      expr_to_bexpr ctx lhs

  | Triple (VhdExpFactor, lhs, _rhs) ->
      (* `a ** b` rarely synthesises; pass a through unchanged. *)
      expr_to_bexpr ctx lhs

  | Triple (VhdXorLogicalExpression, lhs, rhs)
  | Triple (VhdXorFactor, lhs, rhs) ->
      BBinOp { op = BXor;
               lhs = expr_to_bexpr ctx lhs; rhs = expr_to_bexpr ctx rhs;
               result_type = BBool }

  | Triple (VhdXnorLogicalExpression, lhs, rhs)
  | Triple (VhdXnorFactor, lhs, rhs) ->
      BUnOp { op = BNot;
              operand = BBinOp { op = BXor;
                                 lhs = expr_to_bexpr ctx lhs;
                                 rhs = expr_to_bexpr ctx rhs;
                                 result_type = BBool };
              result_type = BBool }

  | Triple (VhdNandLogicalExpression, lhs, rhs)
  | Triple (VhdNandFactor, lhs, rhs) ->
      BUnOp { op = BNot;
              operand = BBinOp { op = BAnd;
                                 lhs = expr_to_bexpr ctx lhs;
                                 rhs = expr_to_bexpr ctx rhs;
                                 result_type = BBool };
              result_type = BBool }

  | Triple (VhdNorLogicalExpression, lhs, rhs)
  | Triple (VhdNorFactor, lhs, rhs) ->
      BUnOp { op = BNot;
              operand = BBinOp { op = BOr;
                                 lhs = expr_to_bexpr ctx lhs;
                                 rhs = expr_to_bexpr ctx rhs;
                                 result_type = BBool };
              result_type = BBool }

  (* Unary negation: -x *)
  | Double (VhdNegSimpleExpression, expr) ->
      BUnOp { op = BNeg; operand = expr_to_bexpr ctx expr;
              result_type = BInt { width = 32; signed = Signed } }


  (* Unwrap discrete-range and qualified-expression containers when
     they appear directly in an expr context. *)
  | Double (VhdActualDiscreteRange, inner) -> expr_to_bexpr ctx inner
  | Double (VhdActualExpression, inner) -> expr_to_bexpr ctx inner
  | Double (VhdQualifiedExpressionPrimary, inner) -> expr_to_bexpr ctx inner
  | Double (VhdRange, inner) -> expr_to_bexpr ctx inner
  | Triple (VhdRange, _, _) ->
      (* Bare range with hi/lo — pick the hi side as a stand-in
         scalar.  Range-as-value in expr context is rare, mostly
         appears inside aggregate/subtype constraints we already
         destructured elsewhere; keep going. *)
      BConst { value = Z.zero; width = 1 }
  | Triple (VhdDecreasingRange, hi, _) -> expr_to_bexpr ctx hi
  | Triple (VhdIncreasingRange, _, hi) -> expr_to_bexpr ctx hi

  (* Qualified expression `T'(expr)` — drop the type qualifier and
     evaluate the inner expr.  Triple form: (qualified, type, expr). *)
  | Triple (VhdQualifiedExpression, _, inner) -> expr_to_bexpr ctx inner
  | Triple (VhdQualifiedAggregate, _, inner) -> expr_to_bexpr ctx inner

  (* Physical literals like `5 ns` — non-synthesisable, treat as 0. *)
  | Double (VhdPhysicalPrimary, _) -> BConst { value = Z.zero; width = 32 }

  (* Float / abs / new — rare, non-synthesisable in our scope. *)
  | Double (VhdFloatPrimary, _) -> BConst { value = Z.zero; width = 32 }
  | Double (VhdAbsFactor, e) -> expr_to_bexpr ctx e
  | Double (VhdNewFactor, _) -> BConst { value = Z.zero; width = 1 }

  (* Bare leaf primary literals that escape the named wrappers.
     Some VHDL parses leave `Num` directly as an atomic literal. *)
  | Num s ->
      (try BConst { value = Z.of_string s; width = 32 }
       with _ -> BConst { value = Z.zero; width = 32 })
  | Char '0' -> BConst { value = Z.zero; width = 1 }
  | Char '1' -> BConst { value = Z.one; width = 1 }

  (* Two-operand form (no leading op) seen in some shapes. *)
  | Double (VhdConcatSimpleExpression, List parts) ->
      BConcat (List.map (expr_to_bexpr ctx) parts)
  | Double (VhdConcatSimpleExpression, single) ->
      expr_to_bexpr ctx single

  (* Concatenation: a & b → BConcat [a; b].  Chained `a & b & c`
     in VHDL parses left-associatively, so we splat any nested
     concat into a flat list MSB-first. *)
  | Triple (VhdConcatSimpleExpression, lhs, rhs) ->
      let flatten e =
        let rec aux acc = function
          | Triple (VhdConcatSimpleExpression, l, r) -> aux (aux acc r) l
          | other -> expr_to_bexpr ctx other :: acc
        in
        aux [] e
      in
      let parts = flatten (Triple (VhdConcatSimpleExpression, lhs, rhs)) in
      BConcat parts

  (* `(others => '0')` and `(others => '1')` aggregate.  Width is
     unknown at expression level; assignment-time resize will
     extend the 1-bit constant to the LHS width. *)
  | Double (VhdAggregatePrimary,
           Triple (Vhdelement_association, VhdChoiceOthers,
                   Double (VhdCharPrimary, Char c))) ->
      let v = if c = '1' then 1 else 0 in
      BConst { value = Z.of_int v; width = 1 }

  (* `(others => expr)` with non-trivial expr — fall back to the
     inner expr; assignment-time resize / replicate behaviour
     downstream. *)
  | Double (VhdAggregatePrimary,
           Triple (Vhdelement_association, VhdChoiceOthers, e)) ->
      expr_to_bexpr ctx e

  (* Aggregate with explicit elements `(0 => a, 1 => b, others => c)`.
     Build a BConcat MSB-first when all keys fold to integers; else
     fall back to the last element so we don't silently drop. *)
  | Double (VhdAggregatePrimary, List elems) ->
      let pair_of = function
        | Triple (Vhdelement_association, VhdChoiceOthers, e) ->
            (`Others, expr_to_bexpr ctx e)
        | Triple (Vhdelement_association, key, e) ->
            (match fold_range_int ctx key with
             | Some i -> (`Idx i, expr_to_bexpr ctx e)
             | None -> (`Others, expr_to_bexpr ctx e))
        | other -> (`Others, expr_to_bexpr ctx other)
      in
      let pairs = List.map pair_of elems in
      let indexed = List.filter_map
        (function (`Idx i, e) -> Some (i, e) | _ -> None) pairs in
      let _others = List.find_map
        (function (`Others, e) -> Some e | _ -> None) pairs in
      (match List.sort (fun (a,_) (b,_) -> compare b a) indexed with
       | [] ->
           (* No indexed parts; just the others-branch (or
              fallback to first element). *)
           (match pairs with [] -> BConst { value = Z.zero; width = 1 }
                            | (_, e) :: _ -> e)
       | sorted -> BConcat (List.map snd sorted))

  (* 3+-level dotted name `pkg.rec.field` parses as VhdLdotted.
     Flatten the same way as VhdSelectedName. *)
  | Triple (VhdLdotted, lhs, rhs) ->
      let head = expr_to_bexpr ctx lhs in
      let tail = expr_to_bexpr ctx rhs in
      let s_of = function BVar n -> n | _ -> "_unknown" in
      BVar (s_of head ^ "__" ^ s_of tail)

  (* Logical operators *)
  | Triple (VhdAndLogicalExpression, lhs, rhs) ->
      let lhs_expr = expr_to_bexpr ctx lhs in
      let rhs_expr = expr_to_bexpr ctx rhs in
      BBinOp { op = BAnd; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }

  | Triple (VhdOrLogicalExpression, lhs, rhs) ->
      let lhs_expr = expr_to_bexpr ctx lhs in
      let rhs_expr = expr_to_bexpr ctx rhs in
      BBinOp { op = BOr; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }

  (* Unary operators *)
  | Double (VhdNotFactor, expr) ->
      let operand = expr_to_bexpr ctx expr in
      BUnOp { op = BNot; operand; result_type = BBool }

  (* Parenthesized expression *)
  | Double (VhdParenthesedPrimary, expr) ->
      expr_to_bexpr ctx expr

  (* Case-arm choice wrappers: `when X =>` parses as
     Double(VhdChoiceSimpleExpression, X).  Strip and recurse. *)
  | Double (VhdChoiceSimpleExpression, inner) -> expr_to_bexpr ctx inner
  | Double (VhdChoiceDiscreteRange, inner) -> expr_to_bexpr ctx inner

  (* `Vhdwaveform_element` wraps the rhs of a signal assignment.
     `value after delay` produces the 3-arg Triple form; drop the
     delay (sim-only). *)
  | Double (Vhdwaveform_element, inner) -> expr_to_bexpr ctx inner
  | Triple (Vhdwaveform_element, value, _delay) -> expr_to_bexpr ctx value

  (* Condition wrapper *)
  | Double (VhdCondition, expr) ->
      expr_to_bexpr ctx expr

  (* Attribute name (like clk'event) - ignore for now *)
  | Double (VhdAttributeName, _) ->
      BConst { value = Z.one; width = 1 }

  (* Record-field access `r.field` (and chained `r.s.field`) flattens
     to a synthetic name `r__s__field`.  VHDL records aren't a
     first-class BIR concept, so we treat them as named bundles of
     signals (the common synthesis pattern).  dump_selected_name
     emits suffixes leaf-first ([state; r] for `r.state`); reverse
     so the BIR name reads parent-first. *)
  | Double (VhdSelectedName, List suffixes) ->
      let rec name_of = function
        | Double (VhdSuffixSimpleName, Double (VhdSimpleName, Str n)) -> Some n
        | Double (VhdSuffixSimpleName, Str n) -> Some n
        | Str n -> Some n
        | Double (VhdSuffixSimpleName, inner) -> name_of inner
        | _ -> None
      in
      let parts = List.rev (List.filter_map name_of suffixes) in
      (match parts with
       | [] -> BVar "_unknown_record"
       | _ -> BVar (String.concat "__" parts))

  (* `signal(...)`: in VHDL this overloads bit-select, slice,
     type-cast, and function call.  Disambiguate by the inner
     parameter shape, falling back to BCall for anything else.

       sig(N)                    → BSlice{sig, N, N}
       sig(M downto L)           → BSlice{sig, M, L}
       sig(L to M)               → BSlice{sig, M, L}
       unsigned/signed/...(expr) → expr (cast erased)
       fn(args...)               → BCall{fn, args} *)
  (* Composite head `pkg.fn(args)` flattens the head to an opaque
     name and processes args under the same logic as the simple
     `Str name` case below. *)
  | Triple (VhdNameParametersPrimary,
            (Double (VhdSelectedName, List _) as head_node), params) ->
      let head_e = expr_to_bexpr ctx head_node in
      let name = match head_e with BVar n -> n | _ -> "_unknown_call" in
      (* A selected name followed by `(…)` is usually a slice/bit-select of a
         record field (`ctrl_i.ir_funct12(11 downto 5)` → the scalarised field
         `ctrl_i__ir_funct12[11:5]`), not a function call — the record collapse
         used to drop the range and mis-read it as a call. *)
      (match params with
       | Triple (Vhdassociation_element, VhdFormalIndexed,
                Double (VhdActualDiscreteRange,
                  Double (VhdRange, Triple (VhdDecreasingRange, hi_e, lo_e)))) ->
           (match fold_range_int ctx hi_e, fold_range_int ctx lo_e with
            | Some h, Some l -> BSlice { signal = BVar name; msb = h; lsb = l }
            | _ -> BVar name)
       | Triple (Vhdassociation_element, VhdFormalIndexed,
                Double (VhdActualDiscreteRange,
                  Double (VhdRange, Triple (VhdIncreasingRange, lo_e, hi_e)))) ->
           (match fold_range_int ctx hi_e, fold_range_int ctx lo_e with
            | Some h, Some l -> BSlice { signal = BVar name; msb = h; lsb = l }
            | _ -> BVar name)
       | _ ->
           let actual_e = match params with
             | Triple (Vhdassociation_element, _, Double (VhdActualExpression, e)) -> e
             | Triple (Vhdassociation_element, _, e) -> e
             | other -> other in
           (match fold_range_int ctx actual_e with
            | Some i -> BSlice { signal = BVar name; msb = i; lsb = i }
            | None -> BCall { func = name; args = [expr_to_bexpr ctx actual_e] }))

  (* Edge-detection builtins surface in conds as `rising_edge(clk)`.
     Convert to a BCall so the clock-guard elider can recognise it. *)
  | Triple (VhdNameParametersPrimary, Str ("rising_edge"|"falling_edge" as fn),
            Triple (Vhdassociation_element, _,
                    Double (VhdActualExpression, arg))) ->
      BCall { func = fn; args = [expr_to_bexpr ctx arg] }
  | Triple (VhdNameParametersPrimary, Str ("rising_edge"|"falling_edge" as fn),
            Triple (Vhdassociation_element, _, arg)) ->
      BCall { func = fn; args = [expr_to_bexpr ctx arg] }

  | Triple (VhdNameParametersPrimary, Str name, params) ->
      let cast_names = ["unsigned"; "signed"; "std_logic_vector";
                        "std_ulogic_vector"; "to_unsigned"; "to_signed";
                        "to_integer"; "conv_integer"; "conv_std_logic_vector";
                        "resize"] in
      let rec extract_actual = function
        | Triple (Vhdassociation_element, VhdFormalIndexed, actual) ->
            extract_actual actual
        | Double (VhdActualExpression, inner) -> Some inner
        | other -> Some other
      in
      let is_cast = List.mem name cast_names in
      let reduce_ops = [ "or_reduce_f", BRedOr; "and_reduce_f", BRedAnd;
                         "xor_reduce_f", BRedXor ] in
      if List.mem_assoc name reduce_ops then
        (match extract_actual params with
         | Some actual ->
             BUnOp { op = List.assoc name reduce_ops;
                     operand = expr_to_bexpr ctx actual;
                     result_type = BInt { width = 1; signed = Unsigned } }
         | None -> BVar name)
      else
      (match params with
       (* range argument: sig(M downto L) → slice *)
       | Triple (Vhdassociation_element, VhdFormalIndexed,
                Double (VhdActualDiscreteRange,
                  Double (VhdRange, Triple (VhdDecreasingRange, hi_e, lo_e)))) ->
           (match fold_range_int ctx hi_e, fold_range_int ctx lo_e with
            | Some h, Some l -> BSlice { signal = BVar name; msb = h; lsb = l }
            | _ -> BVar name)
       | Triple (Vhdassociation_element, VhdFormalIndexed,
                Double (VhdActualDiscreteRange,
                  Double (VhdRange, Triple (VhdIncreasingRange, lo_e, hi_e)))) ->
           (match fold_range_int ctx hi_e, fold_range_int ctx lo_e with
            | Some h, Some l -> BSlice { signal = BVar name; msb = h; lsb = l }
            | _ -> BVar name)
       (* single index or single actual expression *)
       | _ ->
           (match extract_actual params with
            | None -> BVar name
            | Some actual ->
                if is_cast then
                  (* type-cast: drop the cast wrapper, keep the inner
                     expression.  std_logic_vector(unsigned(x)) → x. *)
                  expr_to_bexpr ctx actual
                else match fold_range_int ctx actual with
                  | Some i -> BSlice { signal = BVar name; msb = i; lsb = i }
                  | None ->
                      (* dynamic single-index: array-style read *)
                      BSelect { array = BVar name;
                                index = expr_to_bexpr ctx actual }))

  (* Target dotted (for assignments) *)
  | Double (VhdTargetDotted, inner) ->
      expr_to_bexpr ctx inner

  | VhdNone -> BConst { value = Z.zero; width = 1 }

  (* Bare List in expr context: typically an aggregate body or
     element-association list whose outer wrapper was already
     consumed.  Empty list → 0; single → recurse; multiple → concat
     in MSB-first order. *)
  | List [] -> BConst { value = Z.zero; width = 1 }
  | List [single] -> expr_to_bexpr ctx single
  | List items -> BConcat (List.map (expr_to_bexpr ctx) items)

  (* `Vhdassociation_element` and `Vhdelement_association` wrap an
     inner actual expression in port/aggregate contexts.  When they
     surface as a bare expr we want to drop the formal/index part
     and evaluate the actual. *)
  | Triple (Vhdassociation_element, _formal, actual) ->
      let inner = match actual with
        | Double (VhdActualExpression, e) -> e
        | Double (VhdActualDiscreteRange, e) -> e
        | other -> other in
      expr_to_bexpr ctx inner
  | Triple (Vhdelement_association, _choice, value) ->
      expr_to_bexpr ctx value

  (* Catch-all metavalue characters ('X', 'Z', 'U', 'W', '-' etc.)
     — SystemVerilog metavalues for don't-care/unknown.  Synthesis
     treats them as 0. *)
  | Char _ -> BConst { value = Z.zero; width = 1 }

  | other ->
      let head_name h =
        try Vhd_front.Asctoken.asctoken h with _ -> "?"
      in
      let shape = match other with
        | Double (h, _) -> Printf.sprintf "Double(%s)" (head_name h)
        | Triple (h, _, _) -> Printf.sprintf "Triple(%s)" (head_name h)
        | Quadruple (h, _, _, _) -> Printf.sprintf "Quad(%s)" (head_name h)
        | Quintuple (h, _, _, _, _) -> Printf.sprintf "Quint(%s)" (head_name h)
        | Sextuple (h, _, _, _, _, _) -> Printf.sprintf "Sext(%s)" (head_name h)
        | List _ -> "List"
        | Str s -> Printf.sprintf "Str(%s)" s
        | Num s -> Printf.sprintf "Num(%s)" s
        | Real f -> Printf.sprintf "Real(%g)" f
        | Char c -> Printf.sprintf "Char(%c)" c
        | VhdNone -> "VhdNone"
        | _ ->
            (* zero-arg constructor (constant tag) — name it via asctoken *)
            (try Printf.sprintf "const(%s)" (Vhd_front.Asctoken.asctoken other)
             with _ -> "leaf?")
      in
      vhdl_unhandled "expression" shape;
      BConst { value = Z.zero; width = 1 }

(* Whole-record copy-out: if `lhs` is a known record signal/variable
   and `rhs` is another record name (or evaluates to one), expand the
   single-line assign into per-field assigns.  Returns Some [..] on
   match, None to fall through to the scalar default. *)
let rec expand_record_assign ctx lhs rhs_expr =
  match List.assoc_opt lhs ctx.record_signals with
  | None -> None
  | Some type_name ->
      let fields = try List.assoc type_name ctx.record_types with Not_found -> [] in
      if fields = [] then None
      else
        let rhs_name = match rhs_expr with BVar n -> Some n | _ -> None in
        let per_field (fname, _fty) =
          let lhs_field = lhs ^ "__" ^ fname in
          let rhs_field = match rhs_name with
            | Some n -> BVar (n ^ "__" ^ fname)
            | None -> rhs_expr  (* scalar broadcast — unusual but safe *)
          in
          BAssign { lhs = lhs_field; rhs = rhs_field }
        in
        Some (List.map per_field fields)

(* A waveform element is `Double (Vhdwaveform_element, rhs)` for a plain
   assignment, but `Triple (Vhdwaveform_element, rhs, delay)` when the source
   writes `x <= e after 100 ps;`.  The `after` time is simulation-only timing
   (VHDL projected-waveform semantics); BIR is delta-cycle, so drop it and
   keep the value.  Xilinx's own unisim primitives use it -- SRL16E.vhd's
   `Q <= Q_Index after 100 ps;` was silently unhandled, so the SRL16E body
   came out EMPTY and srl_infer's SRL cells degenerated into a combinational
   feed-through. *)
and waveform_rhs = function
  | Double (Vhdwaveform_element, rhs) -> Some rhs
  | Triple (Vhdwaveform_element, rhs, _after) -> Some rhs
  | _ -> None

(* Convert VHDL statements to behavioral IR statements *)
and stmt_to_bstmt ctx = function
  (* Signal assignment: signal <= value *)
  | Double (VhdSequentialSignalAssignment,
           Double (VhdSimpleSignalAssignment,
                  Quintuple (Vhdsimple_signal_assignment_statement,
                            Str "", Str name, _delay, wave)))
    when waveform_rhs wave <> None ->
      let rhs = Option.get (waveform_rhs wave) in
      let rhs_expr = expr_to_bexpr ctx rhs in
      (match expand_record_assign ctx name rhs_expr with
       | Some xs -> xs
       | None -> [BAssign { lhs = name; rhs = rhs_expr }])

  (* Variable assignment: variable := value *)
  | Double (VhdSequentialVariableAssignment,
           Double (VhdSimpleVariableAssignment,
                  Quadruple (Vhdsimple_variable_assignment, Str "", Str name, rhs))) ->
      let rhs_expr = expr_to_bexpr ctx rhs in
      (match expand_record_assign ctx name rhs_expr with
       | Some xs -> xs
       | None -> [BAssign { lhs = name; rhs = rhs_expr }])

  (* Signal assignment with indexed target *)
  | Double (VhdSequentialSignalAssignment,
           Double (VhdSimpleSignalAssignment,
                  Quintuple (Vhdsimple_signal_assignment_statement,
                            Str "", target, _delay, wave)))
    when waveform_rhs wave <> None ->
      let rhs = Option.get (waveform_rhs wave) in
      (* Extract target name *)
      let rec get_target_name = function
        | Str n -> n
        | Double (VhdTargetDotted, Triple (VhdNameParametersPrimary, Str n, _)) -> n
        | Double (VhdTargetDotted, inner) -> get_target_name inner
        | _ -> "_unknown"
      in
      let name = get_target_name target in
      let rhs_expr = expr_to_bexpr ctx rhs in
      [BAssign { lhs = name; rhs = rhs_expr }]

  (* If statement *)
  | Double (VhdSequentialIf,
           Quintuple (Vhdif_statement, Str "",
                     cond, then_clause, elsif_part)) ->
      let cond_expr = expr_to_bexpr ctx cond in
      let then_stmts = List.concat (List.map (stmt_to_bstmt ctx)
                                    (match then_clause with List l -> l | x -> [x])) in
      let else_stmts = convert_elsif_to_else ctx elsif_part in
      [BIf { condition = cond_expr; then_stmts; else_stmts }]

  (* List of statements *)
  | List stmts ->
      List.concat (List.map (stmt_to_bstmt ctx) stmts)

  (* Null statement *)
  | Double (VhdSequentialNull, _) -> []

  (* For loop: `for i in lo to hi loop ... end loop`.
     Translates to a BFor with init/cond/update on the loop var
     plus body — caller's behavioral_unroll pass folds it. *)
  | Double (VhdSequentialLoop,
           Quadruple (Vhdloop_statement, _,
                      Double (VhdForLoop,
                              Triple (Vhdparameter_specification, Str i,
                                      Double (VhdRange, range_inner))),
                      body)) ->
      let body_stmts = match body with
        | List l -> List.concat (List.map (stmt_to_bstmt ctx) l)
        | x -> stmt_to_bstmt ctx x in
      let lo, hi = match range_inner with
        | Triple (VhdIncreasingRange, lo, hi) -> (lo, hi)
        | Triple (VhdDecreasingRange, hi, lo) -> (lo, hi)
        | _ -> (VhdNone, VhdNone) in
      let lo_e = expr_to_bexpr ctx lo in
      let hi_e = expr_to_bexpr ctx hi in
      let init = BAssign { lhs = i; rhs = lo_e } in
      let cond = BBinOp { op = BLe; lhs = BVar i; rhs = hi_e;
                          result_type = BBool } in
      let update = BAssign { lhs = i;
                             rhs = BBinOp { op = BAdd;
                                            lhs = BVar i;
                                            rhs = BConst { value = Z.one; width = 32 };
                                            result_type = BInt { width = 32; signed = Unsigned } } } in
      [BFor { init; condition = cond; update; body = body_stmts }]

  (* While loop. *)
  | Double (VhdSequentialLoop,
           Quadruple (Vhdloop_statement, _,
                      Double (VhdWhileLoop, cond_e), body)) ->
      let body_stmts = match body with
        | List l -> List.concat (List.map (stmt_to_bstmt ctx) l)
        | x -> stmt_to_bstmt ctx x in
      [BWhile { condition = expr_to_bexpr ctx cond_e;
                body = body_stmts }]

  (* Bare `loop ... end loop` (infinite) — best-effort: convert
     body once and skip the loop wrapping. *)
  | Double (VhdSequentialLoop, Quadruple (Vhdloop_statement, _, _, body)) ->
      (match body with
       | List l -> List.concat (List.map (stmt_to_bstmt ctx) l)
       | x -> stmt_to_bstmt ctx x)

  (* Non-synthesisable testbench constructs — silent drop so the
     synth-relevant body still converts cleanly. *)
  | Double (VhdSequentialWait, _) -> []
  | Double (VhdSequentialAssertion, _) -> []
  | Double (VhdSequentialReport, _) -> []
  | Double (VhdSequentialProcedureCall, _) -> []
  | Double (VhdSequentialReturn, _) -> [BReturn None]
  | Double (VhdSequentialExit, _) -> []
  | Double (VhdSequentialNext, _) -> []

  (* Variable assignment — alternate shape: target may be a complex
     name (record field, indexed slot) so the lhs lookup is more
     forgiving. *)
  | Double (VhdSequentialVariableAssignment, inner) ->
      let rec target_name = function
        | Str n -> Some n
        | Double (VhdTargetDotted, Triple (VhdNameParametersPrimary, Str n, _)) ->
            Some n
        | Double (VhdTargetDotted, inner) -> target_name inner
        | Triple (VhdNameParametersPrimary, Str n, _) -> Some n
        (* Record-field target `v.state` parses as VhdSelectedName with
           suffixes leaf-first; reverse for parent-first naming. *)
        | Double (VhdSelectedName, List suffixes) ->
            let rec suf_name = function
              | Double (VhdSuffixSimpleName, Double (VhdSimpleName, Str n)) -> Some n
              | Double (VhdSuffixSimpleName, Str n) -> Some n
              | Str n -> Some n
              | Double (VhdSuffixSimpleName, inner) -> suf_name inner
              | _ -> None
            in
            let parts = List.rev (List.filter_map suf_name suffixes) in
            (match parts with
             | [] -> None
             | _ -> Some (String.concat "__" parts))
        | _ -> None
      in
      let extract_assign = function
        | Double (VhdSimpleVariableAssignment,
                 Quadruple (Vhdsimple_variable_assignment, Str "",
                            target, rhs)) -> Some (target, rhs)
        | _ -> None
      in
      (match extract_assign inner with
       | Some (target, rhs) ->
           (match target_name target with
            | Some n ->
                let rhs_e = expr_to_bexpr ctx rhs in
                (match expand_record_assign ctx n rhs_e with
                 | Some xs -> xs
                 | None -> [BAssign { lhs = n; rhs = rhs_e }])
            | None -> [])
       | None -> [])

  (* Case statement: 5 fields.  Selector is wrapped in
     Double(VhdSelector, sel_expr).  alts is a list of triples
     `Triple(Vhdcase_statement_alternative, choice, body)`. *)
  | Double (VhdSequentialCase,
           Quintuple (Vhdcase_statement, _,
                      Double (VhdSelector, sel_expr),
                      _ord_selection,
                      List alts)) ->
      let selector = expr_to_bexpr ctx sel_expr in
      let body_of stmts =
        List.concat (List.map (stmt_to_bstmt ctx)
                     (match stmts with List l -> l | x -> [x]))
      in
      let arm = function
        | Triple (Vhdcase_statement_alternative,
                  VhdChoiceOthers, body) ->
            `Default (body_of body)
        | Triple (Vhdcase_statement_alternative,
                  Double (VhdChoiceOthers, _), body) ->
            `Default (body_of body)
        | Triple (Vhdcase_statement_alternative, choice, body) ->
            `Case (expr_to_bexpr ctx choice, body_of body)
        | _ -> `Default []
      in
      let cases = List.filter_map (fun a ->
        match arm a with `Case (v, b) -> Some (v, b) | _ -> None) alts in
      let default = List.fold_left (fun acc a ->
        match arm a with `Default b when b <> [] -> b | _ -> acc) [] alts in
      [BCase { selector; cases; default }]

  | VhdNone -> []

  | other ->
      let head_name h =
        try Vhd_front.Asctoken.asctoken h with _ -> "?"
      in
      (* Print the shape SEVERAL levels deep: the outer constructor alone is
         never enough to write a pattern against (every signal assignment is
         `Double(VhdSequentialSignalAssignment)`; what differs is the delay
         and waveform nested three levels in). *)
      let rec shape_of d n =
        if d <= 0 then "..." else
        match n with
        | Double (h, a) ->
            Printf.sprintf "Double(%s,%s)" (head_name h) (shape_of (d-1) a)
        | Triple (h, a, b) ->
            Printf.sprintf "Triple(%s,%s,%s)" (head_name h)
              (shape_of (d-1) a) (shape_of (d-1) b)
        | Quadruple (h, a, b, c) ->
            Printf.sprintf "Quad(%s,%s,%s,%s)" (head_name h)
              (shape_of (d-1) a) (shape_of (d-1) b) (shape_of (d-1) c)
        | Quintuple (h, a, b, c, e) ->
            Printf.sprintf "Quint(%s,%s,%s,%s,%s)" (head_name h)
              (shape_of (d-1) a) (shape_of (d-1) b) (shape_of (d-1) c)
              (shape_of (d-1) e)
        | Str x -> Printf.sprintf "Str %S" x
        | List l -> Printf.sprintf "List[%d]" (List.length l)
        | x -> (try Vhd_front.Asctoken.asctoken x with _ -> "leaf")
      in
      let shape = shape_of 5 other in
      vhdl_unhandled "statement" shape;
      []

(* Convert elsif chain to nested if-else *)
and convert_elsif_to_else ctx = function
  | Double (VhdElsif,
           Quintuple (Vhdif_statement, Str "",
                     cond, then_clause, else_part)) ->
      let cond_expr = expr_to_bexpr ctx cond in
      let then_stmts = List.concat (List.map (stmt_to_bstmt ctx)
                                    (match then_clause with List l -> l | x -> [x])) in
      let else_stmts = convert_elsif_to_else ctx else_part in
      [BIf { condition = cond_expr; then_stmts; else_stmts }]

  | Double (VhdElse, stmts) ->
      List.concat (List.map (stmt_to_bstmt ctx)
                   (match stmts with List l -> l | x -> [x]))

  | VhdElseNone -> []

  | other -> []

(* Extract clock signal from process body *)
let rec find_clock_in_expr = function
  (* Pattern: signal'event and signal = '1' *)
  | Triple (VhdAndLogicalExpression,
           Double (VhdAttributeName,
                  Triple (Vhdattribute_name,
                         Double (VhdSuffixSimpleName, Str sig_name),
                         Str "event")),
           _comparison) ->
      Some (sig_name, `Pos)

  (* Pattern: rising_edge(signal) *)
  | Triple (VhdNameParametersPrimary, Str "rising_edge",
           Triple (Vhdassociation_element, _,
                  Double (VhdActualExpression, Str sig_name))) ->
      Some (sig_name, `Pos)

  (* Pattern: falling_edge(signal) *)
  | Triple (VhdNameParametersPrimary, Str "falling_edge",
           Triple (Vhdassociation_element, _,
                  Double (VhdActualExpression, Str sig_name))) ->
      Some (sig_name, `Neg)

  | Double (VhdParenthesedPrimary, expr) -> find_clock_in_expr expr
  | Double (VhdCondition, expr) -> find_clock_in_expr expr

  | _ -> None

(* Find reset signal in condition *)
let rec find_reset_in_expr = function
  | Str s -> Some s
  | Triple (VhdEqualRelation, Str s, _) -> Some s
  | Triple (VhdEqualRelation, _, Str s) -> Some s
  | Double (VhdCondition, expr) -> find_reset_in_expr expr
  | Double (VhdParenthesedPrimary, expr) -> find_reset_in_expr expr
  | _ -> None

(* Reset polarity from the reset condition: `rstn = '0'` is active-LOW (`Neg),
   `rst = '1'` / bare `rst` is active-HIGH (`Pos).  Downstream, ffrip inverts the
   reset cone for `Neg so a synthesised active-high reset (a tool emits `~rstn`)
   and this active-low source agree in the miter. *)
let rec reset_edge_of_cond = function
  | Double (VhdCondition, e) | Double (VhdParenthesedPrimary, e) -> reset_edge_of_cond e
  | Triple (VhdEqualRelation, _, Double (VhdCharPrimary, Char '0'))
  | Triple (VhdEqualRelation, Double (VhdCharPrimary, Char '0'), _) -> `Neg
  | Triple (VhdEqualRelation, _, Double (VhdCharPrimary, Char '1'))
  | Triple (VhdEqualRelation, Double (VhdCharPrimary, Char '1'), _) -> `Pos
  | _ -> `Pos

(* Analyze process to determine if it's sequential or combinational *)
let analyze_process_structure body =
  let clock_info = ref None in
  let reset_info = ref None in

  let rec scan = function
    | Double (VhdSequentialIf,
             Quintuple (Vhdif_statement, _, cond, _then_clause, elsif_part)) ->
        (* Try clock detection on the outer condition first.  Pattern:
              if rising_edge(clk) then BODY end if
              if rising_edge(clk) then BODY else …
              if reset then RESET_BODY elsif rising_edge(clk) then BODY end if
           Whichever sub-position has the rising_edge wins. *)
        (match find_clock_in_expr cond with
         | Some (clk, edge) -> clock_info := Some (clk, edge)
         | None ->
             (* Outer cond is the reset; clock should be in elsif. *)
             (match find_reset_in_expr cond with
              | Some rst -> reset_info := Some (rst, reset_edge_of_cond cond)
              | None -> ());
             (match elsif_part with
              | Double (VhdElsif,
                       Quintuple (Vhdif_statement, _, clk_cond, _, _)) ->
                  (match find_clock_in_expr clk_cond with
                   | Some (clk, edge) -> clock_info := Some (clk, edge)
                   | None -> ())
              | _ -> ()))

    | List items -> List.iter scan items
    | _ -> ()
  in
  scan body;
  (!clock_info, !reset_info)

(* Drop a redundant inner `if (clk'event and clk = '1')` once the
   outer process has been classified as sequential.  The VHDL idiom
       if (rst = '1') then ... elsif rising_edge(clk) then BODY end if
   parses into BIR as a BIf whose else-branch contains a nested
   BIf gated on a clock-edge predicate.  Once we've absorbed the
   clock edge into the BSequential metadata, the inner gate is
   pure noise — replace `[BIf {clk_cond, X, []}]` in the else
   branch with X. *)
(* `if rising_edge(clk) then BODY end if;` (no else) is a pure clock
   guard — once we know the process is sequential on `clk`, the BIf
   itself is redundant and we want to expose BODY directly. *)
let is_clock_guard clk = function
  | BCall { func = ("rising_edge"|"falling_edge"); args = [BVar c] } -> c = clk
  | BSelect { array = BVar ("rising_edge"|"falling_edge"); index = BVar c } -> c = clk
  | BBinOp { op = BAnd; rhs = BBinOp { op = BEq;
               lhs = BVar c; rhs = BConst { value = zv; _ } }; _ } when Z.equal zv Z.one -> c = clk
  | BBinOp { op = BAnd; lhs = BBinOp { op = BEq;
               lhs = BVar c; rhs = BConst { value = zv; _ } }; _ } when Z.equal zv Z.one -> c = clk
  | _ -> false

(* Drop a redundant inner clock guard from a reset else-branch: the idiom
       if (rst = '1') then ... elsif rising_edge(clk) then BODY end if
   parses as a BIf whose else-branch is `[BIf {clk_guard, BODY, []}]`.  Once the
   clock edge is in the BSequential metadata the inner gate is noise → BODY.
   (Handles BOTH `rising_edge(clk)` and `clk'event and clk='1'` guard forms.) *)
let rec elide_clock_guard clk = function
  | [] -> []
  | [BIf { condition; then_stmts; else_stmts = [] }]
    when is_clock_guard clk condition -> then_stmts
  | x :: tl -> x :: elide_clock_guard clk tl

let rec elide_clock_guard_outer clk = function
  | [] -> []
  | BIf { condition; then_stmts; else_stmts = [] } :: tl
    when is_clock_guard clk condition ->
      then_stmts @ elide_clock_guard_outer clk tl
  | BIf { condition; then_stmts; else_stmts } :: tl ->
      BIf { condition;
            then_stmts;
            else_stmts = elide_clock_guard clk else_stmts } ::
      elide_clock_guard_outer clk tl
  | x :: tl -> x :: elide_clock_guard_outer clk tl

(* Walk a process declarative-part tree and register any
   record-typed variables in ctx.record_signals so that
   stmt_to_bstmt can split whole-record assigns. *)
let register_proc_record_vars ctx proc_decls =
  let user_type_of subty =
    match subty with
    | Quadruple (Vhdsubtype_indication, _, Str type_name, _) -> Some type_name
    | _ -> None
  in
  let rec walk = function
    | List xs -> List.iter walk xs
    | Double (VhdProcessVariableDeclaration,
             Quintuple (Vhdvariable_declaration, _shared, names_node, subty, _init)) ->
        (match user_type_of subty with
         | Some type_name when List.mem_assoc type_name ctx.record_types ->
             let names = match names_node with
               | List xs -> List.filter_map (function Str n -> Some n | _ -> None) xs
               | Str n -> [n]
               | _ -> []
             in
             List.iter (fun n ->
               ctx.record_signals <- (n, type_name) :: ctx.record_signals
             ) names
         | _ -> ())
    | _ -> ()
  in
  walk proc_decls

(* Convert VHDL process to behavioral IR process *)
let process_to_bprocess ctx name sens_list ?(proc_decls=VhdNone) body =
  register_proc_record_vars ctx proc_decls;
  let (clock_info, reset_info) = analyze_process_structure body in

  (* Convert body statements *)
  let body_stmts = match body with
    | List stmts -> List.concat (List.map (stmt_to_bstmt ctx) stmts)
    | stmt -> stmt_to_bstmt ctx stmt
  in

  let body_stmts = match clock_info with
    | Some (clock, _) -> elide_clock_guard_outer clock body_stmts
    | None -> body_stmts
  in

  match clock_info with
  | Some (clock, edge) ->
      (* Sequential process *)
      let (reset_name, reset_edge) = match reset_info with
        | Some (rst, edge) -> (Some rst, Some edge)
        | None -> (None, None)
      in
      BSequential {
        name;
        clock;
        clock_edge = edge;
        reset = reset_name;
        reset_edge;
        reset_async = true;  (* VHDL async reset if in sensitivity list *)
        body = body_stmts;
        blocking_vars = [];
      }

  | None ->
      (* Combinational process *)
      BCombinational {
        name;
        sensitivity = [BAny];  (* Simplify for now *)
        body = body_stmts;
      }

(* ── Record / enum type resolution (shared by the type pre-pass and both the
 *    port and internal-signal extractors) ─────────────────────────────────── *)
let resolve_user_type_g ctx subty =
  match subty with
  | Quadruple (Vhdsubtype_indication, _, Str type_name, VhdNoConstraint)
  | Quadruple (Vhdsubtype_indication, _, Str type_name, _) ->
      (match List.assoc_opt type_name ctx.record_types with
       | Some fields -> `Record (type_name, fields)
       | None ->
           (match List.assoc_opt type_name ctx.enum_types with
            | Some values -> `Enum (BInt { width = bits_needed (List.length values); signed = Unsigned })
            | None -> `Scalar (infer_btype ctx subty)))
  | _ -> `Scalar (infer_btype ctx subty)

let extract_record_fields_g ctx elems =
  let acc = ref [] in
  List.iter (function
    | Triple (Vhdelement_declaration, names_node, subty) ->
        let names = match names_node with
          | List xs -> List.filter_map (function Str n -> Some n | _ -> None) xs
          | Str n -> [n] | _ -> [] in
        let fty = match resolve_user_type_g ctx subty with
          | `Record _ -> infer_btype ctx subty
          | `Enum ty | `Scalar ty -> ty in
        List.iter (fun n -> acc := (n, fty) :: !acc) names
    | _ -> ()) elems;
  List.rev !acc

(* Pre-pass: scan whole design units (entities, architectures AND packages) for
 * record/enum type declarations so record-typed PORTS resolve.  Ports are
 * extracted before the architecture body, and package types (ctrl_bus_t) live in
 * a separate design unit — without this pre-pass a record port collapsed to the
 * 1-bit infer_btype default and its fields dangled. *)
let scan_types_into ctx trees =
  let register_enum type_name lits =
    let value_names = List.filter_map (function
      | Double (VhdIdentifierEnumeration, Str n) -> Some n
      | Double (VhdIdentifierEnumeration, Double (VhdSimpleName, Str n)) -> Some n
      | Str n -> Some n | _ -> None) lits in
    if not (List.mem_assoc type_name ctx.enum_types) then begin
      ctx.enum_types <- (type_name, value_names) :: ctx.enum_types;
      List.iteri (fun i n -> ctx.enum_values <- (n, (type_name, i)) :: ctx.enum_values) value_names
    end in
  let register_record type_name elems =
    if not (List.mem_assoc type_name ctx.record_types) then
      ctx.record_types <- (type_name, extract_record_fields_g ctx elems) :: ctx.record_types in
  let rec go = function
    | List xs -> List.iter go xs
    | Triple (Vhddesign_unit, _, x) -> go x
    | Double (VhdPrimaryUnit, x) | Double (VhdSecondaryUnit, x) -> go x
    | Double (VhdPackageDeclaration, x) -> go x
    | Quintuple (Vhdpackage_declaration, _, _, _, decls) -> go decls
    | Double (VhdPackageTypeDeclaration, body) -> go body
    | Double (VhdArchitectureBody, x) -> go x
    | Quintuple (Vhdarchitecture_body, _, _, decls, _) -> go decls
    | Double (VhdBlockTypeDeclaration, body) | Double (VhdFullType, body) -> go body
    | Triple (VhdEnumerationTypeDefinition, Str type_name, List lits) ->
        register_enum type_name lits
    | Triple (Vhdfull_type_declaration, Str type_name, def) ->
        (match def with
         | Double (VhdRecordTypeDefinition, Triple (Vhdrecord_type_definition, List elems, _)) ->
             register_record type_name elems
         | Double (VhdEnumerationTypeDefinition, List lits) -> register_enum type_name lits
         | _ -> ())
    | Double (VhdBlockSubTypeDeclaration,
              Triple (Vhdsubtype_declaration, Str type_name, subty)) ->
        if not (List.mem_assoc type_name ctx.subtypes) then
          ctx.subtypes <- (type_name, infer_btype ctx subty) :: ctx.subtypes
    | _ -> ()
  in
  List.iter go trees

(* Extract entity ports *)
let extract_entity_ports ctx = function
  | Triple (Vhddesign_unit, _,
           Double (VhdPrimaryUnit,
                  Double (VhdEntityDeclaration,
                         Quintuple (Vhdentity_declaration, Str entity_name,
                                   Triple (Vhdentity_header, generics, port_list),
                                   _decls, _stmts)))) ->

      (* Generic-parameter defaults so range expressions like
         `WIDTH-1 downto 0` fold to literal widths in infer_btype. *)
      let rec extract_generics = function
        | List gs -> List.iter extract_generics gs
        | Double (VhdInterfaceObjectDeclaration,
                 Double (VhdInterfaceDefaultDeclaration,
                        Sextuple (Vhdinterface_default_declaration, Str gname,
                                 _mode, _subtype, _kind, default_e))) ->
            (match fold_range_int ctx default_e with
             | Some v -> ctx.generics <- (gname, v) :: ctx.generics
             | None -> ())
        | _ -> ()
      in
      extract_generics generics;

      let rec extract_ports signals = function
        | List ports ->
            List.concat (List.map (extract_ports signals) ports)

        | Double (VhdInterfaceObjectDeclaration,
                 Double (VhdInterfaceDefaultDeclaration,
                        Sextuple (Vhdinterface_default_declaration, name_node,
                                 mode, subtype, _kind, _default))) ->
            let dir = match mode with
              | VhdInterfaceModeOut -> `Output
              | VhdInterfaceModeInOut -> `Output   (* approximate inout as out *)
              | _ -> `Input in
            (* Multi-name port declarations (`clk_i, rstn_i : in std_ulogic`) put a
               List of names in the name slot; single-name a bare Str. *)
            let names = match name_node with
              | Str n -> [n]
              | List xs -> List.filter_map (function Str n -> Some n | _ -> None) xs
              | _ -> [] in
            (* A record-typed port (`ctrl_i : ctrl_bus_t`) scalarizes to one port
             * per field (`ctrl_i__ir_funct3`, …), matching how record field
             * accesses are lowered — without this the whole record collapses to
             * the 1-bit infer_btype default and every field read dangled. *)
            List.concat_map (fun name ->
              match resolve_user_type_g ctx subtype with
              | `Record (type_name, fields) ->
                  ctx.record_signals <- (name, type_name) :: ctx.record_signals;
                  List.map (fun (fname, fty) ->
                    let sn = name ^ "__" ^ fname in
                    add_signal_type ctx sn fty;
                    { name = sn; stype = fty; direction = dir;
                      initial_value = None; attrs = [] }) fields
              | `Enum ty | `Scalar ty ->
                  add_signal_type ctx name ty;
                  [{ name; stype = ty; direction = dir; initial_value = None; attrs = [] }])
              names

        | other ->
            if Sys.getenv_opt "VHDL_DEBUG" <> None then begin
              let tag h = try Vhd_front.Asctoken.asctoken h with _ -> "?" in
              let shp = function
                | Double (h, _) -> "Double " ^ tag h
                | Triple (h, _, _) -> "Triple " ^ tag h
                | Sextuple (h, _, _, _, _, _) -> "Sext " ^ tag h
                | List xs -> Printf.sprintf "List[%d]" (List.length xs)
                | Str s -> "Str " ^ s | _ -> "?" in
              Printf.eprintf "[port-miss] %s\n%!" (shp other);
              (match other with
               | Double (_, Double (_, Sextuple (_, nm, _, _, _, _))) ->
                   Printf.eprintf "  name-slot: %s\n%!" (shp nm)
               | _ -> ())
            end;
            []
      in
      (entity_name, extract_ports [] port_list)

  | _ -> ("", [])

(* Extract architecture body and convert processes *)
let extract_architecture ctx entity_name = function
  | Triple (Vhddesign_unit, _,
           Double (VhdSecondaryUnit,
                  Double (VhdArchitectureBody,
                         Quintuple (Vhdarchitecture_body, Str arch_name,
                                   Str entity_ref, decls, stmts))))
    when String.lowercase_ascii entity_ref
         = String.lowercase_ascii entity_name ->

      (* Extract internal signals from declarations *)
      let internal_signals = ref [] in

      let mk_signal name stype =
        let init_w = match stype with
          | BInt { width; _ } -> width | _ -> 1 in
        let signal = {
          name; stype; direction = `Internal;
          initial_value = Some (BConst { value = Z.zero; width = init_w });
          attrs = [];
        } in
        add_signal_type ctx name stype;
        internal_signals := signal :: !internal_signals
      in
      (* Resolve a subtype against ctx.record_types / ctx.enum_types so
         that signals declared with a user-defined type get expanded
         (records) or width-sized (enums) instead of falling through
         to the 1-bit default. *)
      let resolve_user_type subty =
        match subty with
        | Quadruple (Vhdsubtype_indication, _, Str type_name, VhdNoConstraint)
        | Quadruple (Vhdsubtype_indication, _, Str type_name, _) ->
            (match List.assoc_opt type_name ctx.record_types with
             | Some fields -> `Record (type_name, fields)
             | None ->
                 (match List.assoc_opt type_name ctx.enum_types with
                  | Some values ->
                      let w = bits_needed (List.length values) in
                      `Enum (BInt { width = w; signed = Unsigned })
                  | None -> `Scalar (infer_btype ctx subty)))
        | _ -> `Scalar (infer_btype ctx subty)
      in
      (* Extract a subtype's user type-mark name (`type_name`) when
         present, ignoring constraints.  Used for record-field
         subtypes that themselves reference another record. *)
      let mk_signal_for ~name subty =
        match resolve_user_type subty with
        | `Record (type_name, fields) ->
            ctx.record_signals <- (name, type_name) :: ctx.record_signals;
            List.iter (fun (fname, fty) ->
              mk_signal (name ^ "__" ^ fname) fty
            ) fields
        | `Enum ty | `Scalar ty -> mk_signal name ty
      in
      (* Walk an element_declaration list inside a record_type_definition,
         producing a (field_name, btype) pair for each leaf
         declaration.  Nested records aren't fully expanded here — they
         get registered with their type-mark name and resolved when
         the *signal* using that record is expanded. *)
      let extract_record_fields elems =
        let acc = ref [] in
        List.iter (function
          | Triple (Vhdelement_declaration, names_node, subty) ->
              let names = match names_node with
                | List xs -> List.filter_map (function Str n -> Some n | _ -> None) xs
                | Str n -> [n]
                | _ -> []
              in
              let fty = match resolve_user_type subty with
                | `Record _ -> infer_btype ctx subty  (* will be flattened separately *)
                | `Enum ty | `Scalar ty -> ty
              in
              List.iter (fun n -> acc := (n, fty) :: !acc) names
          | _ -> ()
        ) elems;
        List.rev !acc
      in
      (* First pass: collect type declarations so signal-decl
         processing can resolve user-defined types.  Records become
         entries in ctx.record_types; enums populate ctx.enum_types
         and ctx.enum_values (encoding by ordinal position). *)
      let register_enum type_name lits =
        let value_names = List.filter_map (function
          | Double (VhdIdentifierEnumeration, Str n) -> Some n
          | Double (VhdIdentifierEnumeration,
                    Double (VhdSimpleName, Str n)) -> Some n
          | Str n -> Some n
          | _ -> None
        ) lits in
        ctx.enum_types <- (type_name, value_names) :: ctx.enum_types;
        List.iteri (fun i n ->
          ctx.enum_values <- (n, (type_name, i)) :: ctx.enum_values
        ) value_names
      in
      let register_record type_name elems =
        let fields = extract_record_fields elems in
        ctx.record_types <- (type_name, fields) :: ctx.record_types
      in
      let rec extract_types = function
        | List xs -> List.iter extract_types xs
        | Double (VhdBlockTypeDeclaration, body) -> extract_types body
        | Double (VhdFullType, body) -> extract_types body
        (* rewrite.ml flattens enum decls to Triple(...,name,lits) *)
        | Triple (VhdEnumerationTypeDefinition, Str type_name, List lits) ->
            register_enum type_name lits
        | Triple (Vhdfull_type_declaration, Str type_name, def) ->
            (match def with
             | Double (VhdRecordTypeDefinition,
                       Triple (Vhdrecord_type_definition, List elems, _name)) ->
                 register_record type_name elems
             | Double (VhdEnumerationTypeDefinition, List lits) ->
                 register_enum type_name lits
             | _ -> ())
        | _ -> ()
      in
      extract_types decls;
      let rec extract_decls = function
        | List decl_list -> List.iter extract_decls decl_list
        | Double (VhdBlockSignalDeclaration,
                 Quintuple (Vhdsignal_declaration, names_node,
                            subtype, _kind, _init)) ->
            (match names_node with
             | List names ->
                 List.iter (function
                   | Str n -> mk_signal_for ~name:n subtype
                   | _ -> ()) names
             | Str n -> mk_signal_for ~name:n subtype
             | _ -> ())
        | _ -> ()
      in
      extract_decls decls;

      (* Extract processes from statements *)
      let processes = ref [] in

      let conc_stmt_id = ref 0 in
      let next_conc_name () =
        let i = !conc_stmt_id in
        incr conc_stmt_id;
        Printf.sprintf "conc_%d" i
      in
      let mk_combinational lhs rhs =
        let body = [BAssign { lhs; rhs }] in
        BCombinational {
          name = next_conc_name ();
          sensitivity = [BAny];
          body;
        }
      in
      let instances = ref [] in
      (* Concurrent assignments to an INDEXED target (`q(3) <= a;`) are collected
         per target and emitted as ONE combinational process of @slice_write
         calls, so the existing slice-write merge builds a single driver.  They
         used to match no arm at all and were SILENTLY DROPPED -- a vector built
         bit-by-bit came out as its default constant with the driving inputs
         unused, so any such module read as a constant and every cross-flow
         miter verdict involving it was noise. *)
      let idx_writes : (string, (int * bexpr) list ref) Hashtbl.t =
        Hashtbl.create 8 in
      let rec first_actual = function
        | Triple (Vhdassociation_element, VhdFormalIndexed, actual) ->
            first_actual actual
        | Double (VhdActualExpression, inner) -> first_actual inner
        | List (x :: _) -> first_actual x
        | other -> other in
      let const_index ctx params =
        match expr_to_bexpr ctx (first_actual params) with
        | BConst { value; _ } -> (try Some (Z.to_int value) with _ -> None)
        | _ -> None in
      (* Walk a port-association list and build BIR
         port_connections.  Each association is
         `Triple(Vhdassociation_element, formal, actual)`.
         Positional associations have VhdFormalNone. *)
      let connect_ports assoc_node =
        let elems = match assoc_node with
          | List xs -> xs
          | other -> [other]
        in
        List.mapi (fun i a ->
          let unwrap = function
            | Double (VhdActualExpression, e) -> e
            | Double (VhdActualOpen, _) -> VhdNone
            | VhdActualOpen -> VhdNone
            | other -> other
          in
          match a with
          | Triple (Vhdassociation_element,
                    Double (VhdFormalExpression, Str fname), actual) ->
              (fname, expr_to_bexpr ctx (unwrap actual))
          | Triple (Vhdassociation_element,
                    Double (VhdFormalExpression,
                            Triple (VhdNameParametersPrimary, Str fname, _)),
                    actual) ->
              (fname, expr_to_bexpr ctx (unwrap actual))
          | Triple (Vhdassociation_element, VhdFormalIndexed, actual) ->
              (Printf.sprintf "_pos%d" i, expr_to_bexpr ctx (unwrap actual))
          | Triple (Vhdassociation_element, _, actual) ->
              (Printf.sprintf "_pos%d" i, expr_to_bexpr ctx (unwrap actual))
          | other ->
              (Printf.sprintf "_pos%d" i, expr_to_bexpr ctx other)
        ) elems
      in
      (* Resolve an instantiation's reference to a plain name.  Vivado writes it
         as a SELECTED name -- `work.sgmii_soc` (2 parts) for a user entity,
         `unisim.vcomponents.GTXE2_CHANNEL` (3 parts) for a primitive -- so the
         entity/component name is the LAST identifier, not the first, and never
         a bare Str.  Taking the last non-empty identifier handles the bare,
         2-part and 3-part forms alike. *)
      let ref_last_ident ref_node =
        (* FIRST identifier in traversal order, not the last: the selected-name
           node stores its parts in reverse source order, so `work.sgmii_soc`
           traverses as [sgmii_soc; work] and
           `unisim.vcomponents.GTXE2_CHANNEL` as
           [GTXE2_CHANNEL; vcomponents; unisim].  Taking the last yielded the
           LIBRARY name ("work" / "unisim") as the module, which resolves to
           nothing -- the miter then reported
             INCONCLUSIVE - 2 unresolved primitive bodies: cpu_bufg:unisim,
             i_sgmii:work
           even though the instances themselves had been created. *)
        let last = ref None in
        let rec go = function
          | Str x when x <> "" -> if !last = None then last := Some x
          | Double (a, b) -> go a; go b
          | Triple (a, b, c) -> go a; go b; go c
          | Quadruple (a, b, c, d) -> List.iter go [a; b; c; d]
          | Quintuple (a, b, c, d, e) -> List.iter go [a; b; c; d; e]
          | Sextuple (a, b, c, d, e, f) -> List.iter go [a; b; c; d; e; f]
          | List l -> List.iter go l
          | _ -> () in
        go ref_node;
        match !last with Some n -> n | None -> "_unknown_entity" in
      let rec extract_stmts stmt0 =
        (* VHDL_STMT_TRACE=<substring>: print the deep shape of any architecture
           statement mentioning the substring.  Needed because an instantiation
           that fails to match its pattern is not necessarily REPORTED -- it can
           be absorbed by an earlier case -- so the unhandled reporter alone
           cannot find it. *)
        (match Sys.getenv_opt "VHDL_STMT_TRACE" with
         | Some pat ->
             let head_name h = try Vhd_front.Asctoken.asctoken h with _ -> "?" in
             let rec shp d n =
               if d <= 0 then "..." else match n with
               | Double (h,a) -> Printf.sprintf "Double(%s,%s)" (head_name h) (shp (d-1) a)
               | Triple (h,a,b) -> Printf.sprintf "Triple(%s,%s,%s)" (head_name h) (shp (d-1) a) (shp (d-1) b)
               | Quadruple (h,a,b,c) -> Printf.sprintf "Quad(%s,%s,%s,%s)" (head_name h) (shp (d-1) a) (shp (d-1) b) (shp (d-1) c)
               | Quintuple (h,a,b,c,e) -> Printf.sprintf "Quint(%s,%s,%s,%s,%s)" (head_name h) (shp (d-1) a) (shp (d-1) b) (shp (d-1) c) (shp (d-1) e)
               | Str x -> Printf.sprintf "Str %S" x
               | List l -> Printf.sprintf "List[%d]" (List.length l)
               | x -> (try Vhd_front.Asctoken.asctoken x with _ -> "leaf") in
             let sh = shp 7 stmt0 in
             (try ignore (Str.search_forward (Str.regexp_string pat) sh 0);
                  Printf.eprintf "[vhdl-stmt] %s\n%!" sh
              with Not_found -> ())
         | None -> ());
        match stmt0 with
        | List stmt_list -> List.iter extract_stmts stmt_list

        | Double (VhdConcurrentProcessStatement,
                 Sextuple (Vhdprocess_statement, Str proc_name, _postponed,
                          sens_node, proc_decls, proc_body)) ->
            let sens_list = match sens_node with
              | Double (VhdSensitivityExpressionList, List xs) -> xs
              | Double (VhdSensitivityExpressionList, x) -> [x]
              | VhdSensitivityAll -> []
              | _ -> []
            in
            let proc = process_to_bprocess ctx proc_name sens_list ~proc_decls proc_body in
            processes := proc :: !processes

        (* Concurrent signal assignment with an INDEXED target: `Q(i) <= rhs;` *)
        | Double (VhdConcurrentSignalAssignmentStatement,
                 Quadruple (Vhdconcurrent_signal_assignment_statement,
                            Str "", _,
                            Double (VhdConcurrentSimpleSignalAssignment,
                                    Quintuple (Vhdconcurrent_simple_signal_assignment,
                                               Double (VhdTargetDotted,
                                                       Triple (VhdNameParametersPrimary,
                                                               Str dst, params)),
                                               _, VhdDelayNone,
                                               Double (Vhdwaveform_element, rhs)))))
          when const_index ctx params <> None ->
            let i = Option.get (const_index ctx params) in
            let rhs_expr = expr_to_bexpr ctx rhs in
            let l = match Hashtbl.find_opt idx_writes dst with
              | Some r -> r
              | None -> let r = ref [] in Hashtbl.replace idx_writes dst r; r in
            l := (i, rhs_expr) :: !l

        (* Concurrent signal assignment: `Q <= rhs;` *)
        | Double (VhdConcurrentSignalAssignmentStatement,
                 Quadruple (Vhdconcurrent_signal_assignment_statement,
                            Str "", _,
                            Double (VhdConcurrentSimpleSignalAssignment,
                                    Quintuple (Vhdconcurrent_simple_signal_assignment,
                                               Str dst, _, VhdDelayNone,
                                               Double (Vhdwaveform_element, rhs))))) ->
            let rhs_expr = expr_to_bexpr ctx rhs in
            processes := mk_combinational dst rhs_expr :: !processes

        (* Component instantiation: `inst : entity_or_component PORT MAP (…)`. *)
        | Double (VhdConcurrentComponentInstantiationStatement,
                 Quintuple (Vhdcomponent_instantiation_statement,
                            Str inst_name,
                            Double (VhdInstantiatedComponent, ref_node),
                            _generic_assoc,
                            port_assoc_list)) ->
            let ref_name = ref_last_ident ref_node in
            let port_connections = connect_ports port_assoc_list in
            instances := {
              inst_name; module_name = ref_name;
              param_values = []; param_strs = [];
              port_connections;
            } :: !instances

        (* Direct entity instantiation:
              `inst : entity work.foo(rtl) PORT MAP (…)`
           wraps the ref name in VhdInstantiatedEntityArchitecture.

           Vivado's `synth_design -rtl` + `write_vhdl` emits this as a TRIPLE
           (entity reference AND architecture name, the latter often "") with
           the reference itself a `Double (VhdSelectedName, List [work; foo])`.
           Only the Double form was matched and only a bare `Str`/one-level ref
           was understood, so every hierarchical instantiation in such a netlist
           fell through SILENTLY -- not even reaching the unhandled reporter,
           because the statement did match the outer constructor.  The result
           was 32 entities imported with ZERO instances between them: the whole
           hierarchy disconnected, and `top` flattening to a 145-signal stub.

           Accept both arities, and resolve the reference by taking the LAST
           non-empty identifier in it, which is the entity/component name for a
           bare name, `work.foo`, and `unisim.vcomponents.BUFG` alike. *)
        | Double (VhdConcurrentComponentInstantiationStatement,
                 Quintuple (Vhdcomponent_instantiation_statement,
                            Str inst_name,
                            ( Double (VhdInstantiatedEntityArchitecture, ref_node)
                            | Triple (VhdInstantiatedEntityArchitecture, ref_node, _) ),
                            _generic_assoc,
                            port_assoc_list)) ->
            let ref_name = ref_last_ident ref_node in
            let port_connections = connect_ports port_assoc_list in
            instances := {
              inst_name; module_name = ref_name;
              param_values = []; param_strs = [];
              port_connections;
            } :: !instances

        (* Concurrent conditional signal assignment:
           `dst <= a when cond1 else b when cond2 else c;`
           becomes a BCombinational process with a chained BIf body.
           Each alt is `Triple(Vhdconditional_waveform, value, cond)`;
           the final alt has cond = VhdNone (the unconditional else). *)
        | Double (VhdConcurrentSignalAssignmentStatement,
                 Quadruple (Vhdconcurrent_signal_assignment_statement,
                            Str "", _,
                            Double (VhdConcurrentConditionalSignalAssignment,
                                    Quintuple (Vhdconcurrent_conditional_signal_assignment,
                                               target_node, _guarded,
                                               _delay,
                                               List alts)))) ->
            let dst = match target_node with
              | Str n -> n
              | Double (VhdTargetDotted, Triple (VhdNameParametersPrimary, Str n, _)) -> n
              | Double (VhdTargetDotted, Str n) -> n
              | _ -> "_unknown_target"
            in
            let unwrap_value = function
              | Double (Vhdwaveform_element, e) -> e
              | other -> other
            in
            let rec build_chain = function
              | [] -> BAssign { lhs = dst; rhs = BConst { value = Z.zero; width = 1 } }
              | [Triple (Vhdconditional_waveform, value, _)] ->
                  (* Final unconditional value (the `else` arm). *)
                  BAssign { lhs = dst; rhs = expr_to_bexpr ctx (unwrap_value value) }
              | Triple (Vhdconditional_waveform, value, cond) :: rest ->
                  BIf {
                    condition = expr_to_bexpr ctx cond;
                    then_stmts = [BAssign { lhs = dst;
                                            rhs = expr_to_bexpr ctx (unwrap_value value) }];
                    else_stmts = [build_chain rest];
                  }
              | _ -> BAssign { lhs = dst; rhs = BConst { value = Z.zero; width = 1 } }
            in
            let body = [build_chain alts] in
            processes := BCombinational {
              name = next_conc_name ();
              sensitivity = [BAny];
              body;
            } :: !processes

        | other ->
            begin
              let tag = match other with
                | Double (Vhdarchitecture_body, _) -> "arch_body"
                | Double (VhdConcurrentSignalAssignmentStatement, _) ->
                    "conc_sig_assign(unmatched-shape)"
                | Double (VhdConcurrentConditionalSignalAssignment, _) ->
                    "conc_cond_assign"
                | Double (_, _) -> "Double(?)"
                | Triple (_, _, _) -> "Triple(?)"
                | _ -> "other"
              in
              (* Print the SHAPE several levels deep, not just the outer tag:
                 every component instantiation is the same outer constructor and
                 what differs (a selected name `work.foo` vs a bare one) is
                 nested three levels in. *)
              let head_name h = try Vhd_front.Asctoken.asctoken h with _ -> "?" in
              let rec shape d n =
                if d <= 0 then "..." else
                match n with
                | Double (h, a) -> Printf.sprintf "Double(%s,%s)" (head_name h) (shape (d-1) a)
                | Triple (h, a, b) ->
                    Printf.sprintf "Triple(%s,%s,%s)" (head_name h) (shape (d-1) a) (shape (d-1) b)
                | Quadruple (h,a,b,c) ->
                    Printf.sprintf "Quad(%s,%s,%s,%s)" (head_name h)
                      (shape (d-1) a) (shape (d-1) b) (shape (d-1) c)
                | Quintuple (h,a,b,c,e) ->
                    Printf.sprintf "Quint(%s,%s,%s,%s,%s)" (head_name h)
                      (shape (d-1) a) (shape (d-1) b) (shape (d-1) c) (shape (d-1) e)
                | Str x -> Printf.sprintf "Str %S" x
                | List l -> Printf.sprintf "List[%d]" (List.length l)
                | x -> (try Vhd_front.Asctoken.asctoken x with _ -> "leaf") in
              vhdl_unhandled ("architecture statement " ^ tag) (shape 6 other)
            end
      in
      extract_stmts stmts;

      (* one driver per indexed target, MSB-first so the merge sees a clean
         set of constant-range slice writes *)
      Hashtbl.iter (fun dst l ->
        let body =
          List.map (fun (i, e) ->
            let ic = BConst { value = Z.of_int i; width = 32 } in
            BCallStmt { func = "@slice_write"; args = [BVar dst; ic; ic; e] })
            (List.sort (fun (a, _) (b, _) -> compare b a) !l) in
        if body <> [] then
          processes := BCombinational { name = next_conc_name ();
                                        sensitivity = [BAny]; body }
                       :: !processes) idx_writes;

      (!internal_signals, !processes, !instances)

  | _ -> ([], [], [])

(* Main conversion function *)
let rec dbg_tree depth t =
  if depth > 6 then () else
  let pad = String.make (2*depth) ' ' in
  let tag h = try Vhd_front.Asctoken.asctoken h with _ -> "?" in
  match t with
  | Str s -> Printf.eprintf "%sStr %S\n%!" pad s
  | Num s -> Printf.eprintf "%sNum %s\n%!" pad s
  | List xs -> Printf.eprintf "%sList[%d]\n%!" pad (List.length xs);
               List.iter (dbg_tree (depth+1)) xs
  | Double (h, a) -> Printf.eprintf "%sDouble %s\n%!" pad (tag h); dbg_tree (depth+1) a
  | Triple (h, a, b) -> Printf.eprintf "%sTriple %s\n%!" pad (tag h);
                        dbg_tree (depth+1) a; dbg_tree (depth+1) b
  | Quadruple (h,a,b,c) -> Printf.eprintf "%sQuad %s\n%!" pad (tag h);
                        List.iter (dbg_tree (depth+1)) [a;b;c]
  | Quintuple (h,a,b,c,d) -> Printf.eprintf "%sQuint %s\n%!" pad (tag h);
                        List.iter (dbg_tree (depth+1)) [a;b;c;d]
  | Sextuple (h,a,b,c,d,e) -> Printf.eprintf "%sSext %s\n%!" pad (tag h);
                        List.iter (dbg_tree (depth+1)) [a;b;c;d;e]
  | h -> Printf.eprintf "%s<%s>\n%!" pad (tag h)

let convert_vhdl_to_behavioral vhdl_ast =
  if Sys.getenv_opt "VHDL_DEBUG" <> None then
    List.iter (fun du ->
      match du with
      | Triple (Vhddesign_unit, _, Double (VhdPrimaryUnit, Double (h, _)))
        when (try Vhd_front.Asctoken.asctoken h with _ -> "") = "VhdPackageDeclaration" ->
          Printf.eprintf "==== PACKAGE DU ====\n%!"; dbg_tree 0 du
      | _ -> ()) vhdl_ast;
  (* The input may carry several designs (entity+architecture pairs)
     when multiple .vhd files were parsed together.  Build one bmodule
     per entity rather than folding everything into a single module
     (the old behaviour mashed all units together — for a multi-file
     parse that produced one giant self-referential module that hung
     the flattener). *)
  (* Pre-scan every design unit (incl. packages) for record/enum types so
     record-typed PORTS resolve and scalarize. *)
  let type_ctx = create_context () in
  scan_types_into type_ctx vhdl_ast;
  let seed_types ctx =
    ctx.record_types <- type_ctx.record_types;
    ctx.subtypes <- type_ctx.subtypes;
    ctx.enum_types <- type_ctx.enum_types;
    ctx.enum_values <- type_ctx.enum_values in
  (* One context per entity, shared between port extraction and the architecture
     body, so record-typed ports registered in ctx.record_signals are visible
     when the body's field accesses / whole-record copies are lowered. *)
  let modules =
    List.filter_map (fun du ->
      let ctx = create_context () in
      seed_types ctx;
      let (entity_name, entity_ports) = extract_entity_ports ctx du in
      if entity_name = "" then None
      else begin
        let (internal_signals, processes, instances) =
          List.fold_left (fun (sigs, procs, insts) design_unit ->
            let (s, p, i) = extract_architecture ctx entity_name design_unit in
            (s @ sigs, p @ procs, i @ insts)
          ) ([], [], []) vhdl_ast
        in
        Some {
          name = entity_name;
          params = [];
          signals = entity_ports @ internal_signals;
          processes;
          instances;
          funcs = [];
          mems = []; attrs = [];
        }
      end) vhdl_ast
  in
  { modules; library_cells = [] }

(* Convert multiple VHDL ASTs to behavioral IR *)
let convert_multiple vhdl_asts =
  let all_modules = List.concat_map (fun ast ->
    let prog = convert_vhdl_to_behavioral ast in
    prog.Behavioral_ir.modules
  ) vhdl_asts in
  { Behavioral_ir.modules = all_modules; library_cells = [] }

(* Convert one or more VHDL files to a single behavioral IR program.
 * Parsing all files together puts every entity/architecture in one
 * bprogram, so a top whose architecture instantiates sub-entities
 * (component port maps) can be flattened by prep_for_z3 — otherwise
 * the sub-entities are missing and the top comes out hollow. *)
let convert_vhdl_files_to_behavioral filenames =
  (* Create a fresh hashtable for this parse to ensure isolation *)
  let fresh_hash = Hashtbl.create 256 in
  let old_hash = !Vhd_front.Vabstraction.vhdlhash in
  Vhd_front.Vabstraction.vhdlhash := fresh_hash;

  (* Also clear the settings filelists to prevent accumulation *)
  let old_settings = !Vhd_front.VhdlSettings.settings in
  Vhd_front.VhdlSettings.settings := {!Vhd_front.VhdlSettings.settings with
    fileparsedlist = [];
    filefailedlist = [];
  };

  try
    (* Parse the files *)
    let succ = ref true in
    Vhd_front.VhdlMain.main succ filenames;

    if not !succ then begin
      Vhd_front.Vabstraction.vhdlhash := old_hash;
      Vhd_front.VhdlSettings.settings := old_settings;
      None
    end else begin
      (* Extract vhdintf trees from our fresh hashtable *)
      let trees = ref [] in
      Hashtbl.iter (fun (k, _) _ ->
        let simplified = Vhd_front.Rewrite.abstraction (Vhd_front.Rewrite.abstraction k) in
        trees := simplified :: !trees
      ) fresh_hash;

      (* Convert to behavioral IR *)
      let result = convert_vhdl_to_behavioral !trees in

      (* Resolve Vivado RTL_* primitives (from `synth_design -rtl; write_vhdl`)
         into behavioural processes.  The mux family needs the per-instance
         SEL_VAL attribute (select->input map); text-scan it from the source
         since the VHDL AST drops attribute specifications. *)
      let result =
        let norm s =
          let s = String.trim s in
          let s = if String.length s > 0 && s.[0] = '\\'
                  then String.sub s 1 (String.length s - 1) else s in
          if String.length s > 0 && s.[String.length s - 1] = '\\'
          then String.sub s 0 (String.length s - 1) else s in
        (* (inst, attr) -> value, for SEL_VAL (mux) and INIT_VAL (ROM). *)
        let attr_tbl : (string * string, string) Hashtbl.t = Hashtbl.create 128 in
        let re = Str.regexp
          "attribute \\(SEL_VAL\\|INIT_VAL\\) of \\([^ ]+\\) : label is \"\\([^\"]*\\)\"" in
        List.iter (fun fn ->
          try
            let ic = open_in fn in
            (try while true do
               let line = input_line ic in
               (try
                  ignore (Str.search_forward re line 0);
                  Hashtbl.replace attr_tbl
                    (norm (Str.matched_group 2 line), Str.matched_group 1 line)
                    (Str.matched_group 3 line)
                with Not_found -> ())
             done with End_of_file -> ());
            close_in ic
          with _ -> ()) filenames;
        let has_rtl = List.exists (fun (m : Behavioral_ir.bmodule) ->
          List.exists (fun (i : Behavioral_ir.binstance) ->
            let mn = if String.length i.module_name > 0 && i.module_name.[0] = '\\'
                     then String.sub i.module_name 1 (String.length i.module_name - 1)
                     else i.module_name in
            String.length mn >= 4 && String.sub mn 0 4 = "RTL_") m.instances) result.modules in
        if not has_rtl then result
        else
          let attr_of inst name = Hashtbl.find_opt attr_tbl (norm inst, name) in
          { result with modules =
              List.map (Rtl_prim.resolve_rtl_instances attr_of) result.modules } in

      (* Restore old hashtable and settings before returning *)
      Vhd_front.Vabstraction.vhdlhash := old_hash;
      Vhd_front.VhdlSettings.settings := old_settings;

      Some result
    end
  with _ ->
    Vhd_front.Vabstraction.vhdlhash := old_hash;
    Vhd_front.VhdlSettings.settings := old_settings;
    None

(* ====================================================================== *)
(* Xilinx unisim primitive-port lookup                                    *)
(*                                                                        *)
(* Task #36: when SV parsing leaves cell-types unresolved (IBUFDS,        *)
(* BUFG, CARRY4, …), reuse the existing VHDL frontend on Vivado's        *)
(* per-primitive `.vhd` files to produce port-direction / width meta     *)
(* in the bprogram.library_cells field.  No new parser — just            *)
(* orchestration over convert_vhdl_files_to_behavioral above.            *)
(* ====================================================================== *)

let xil_unisims_dir =
  try Sys.getenv "XIL_UNISIMS_VHD_DIR"
  with Not_found ->
    "/opt/Xilinx/Vivado/2020.1/data/vhdl/src/unisims/primitive"

(* Search directories for per-primitive `.vhd` entity interfaces.  The simple
   primitives (SRL16E, RAM64M, MMCME2_ADV, BUFG, …) live in `primitive/`, but
   the hard analog macros (GTXE2_CHANNEL/COMMON, PCIE, …) live in the sibling
   `secureip/` directory.  Search BOTH so a GT's VHDL entity — the authoritative
   source of its port directions and bus widths — actually resolves; otherwise
   the net-usage heuristic guesses every GT pin as input, the genuine outputs
   (CPLLLOCK, RXOUTCLK, …) orphan driverless, and nextpnr reports false
   combinatorial loops.  Honours XIL_UNISIMS_VHD_DIR (its sibling secureip too). *)
let xil_unisims_dirs =
  let sib sub = Filename.concat (Filename.dirname xil_unisims_dir) sub in
  (* retarget/ holds the variants Vivado maps onto a base primitive rather than
     modelling separately -- the negative-edge flops FDRE_1/FDSE_1/FDCE_1/FDPE_1
     among them.  yosys emits those for any negedge flop, so leaving the
     directory out means the interface lookup finds NOTHING, every pin defaults
     to `input`, and the cell reaches the EDIF with no D port at all: Vivado
     stops with "Cannot find port 'D' on instance ... of cell 'FDRE_1'".
     primitive/ alone is not the whole unisim library. *)
  [ xil_unisims_dir; sib "secureip"; sib "retarget" ]

let xil_primitive_cache :
    (string, Behavioral_ir.library_port list) Hashtbl.t =
  Hashtbl.create 256
let xil_primitive_missing : (string, unit) Hashtbl.t = Hashtbl.create 64

(* Persistent CACHE of primitive interfaces so the tool runs on machines that
 * cannot run Vivado (the unisim .vhd files are absent there).  On a Vivado
 * machine the VHDL entity interface is authoritative and this is written back;
 * elsewhere it is the fallback.  Same file bir_to_edif reads for EDIF
 * interfaces (env XIL_PRIM_PORTS_JSON). *)
let xil_ports_cache_path () =
  try Sys.getenv "XIL_PRIM_PORTS_JSON"
  with Not_found ->
    Filename.concat (Filename.dirname xil_unisims_dir |> fun _ ->
      "/home/jonathan/System-Verilog-suite/xilinx_lef") "xil_primitive_ports.json"

let dir_of_string = function
  | "output" | "out" -> `Output
  | _ -> `Input

let json_cache : (string, Behavioral_ir.library_port list) Hashtbl.t Lazy.t =
  lazy (
    let tbl = Hashtbl.create 256 in
    let path = xil_ports_cache_path () in
    (if Sys.file_exists path then
       try match Yojson.Safe.from_file path with
         | `Assoc entries ->
             List.iter (fun (cell, ports) ->
               match ports with
               | `List ps ->
                   let plist = List.filter_map (function
                     | `Assoc kv ->
                         let get k = List.assoc_opt k kv in
                         (match get "name", get "dir" with
                          | Some (`String pn), Some (`String d) ->
                              let w = match get "width" with
                                | Some (`Int w) -> w | _ -> 1 in
                              Some { Behavioral_ir.port_name = pn;
                                     port_direction = dir_of_string d;
                                     port_width = w }
                          | _ -> None)
                     | _ -> None) ps in
                   Hashtbl.replace tbl cell plist
               | _ -> ()) entries
         | _ -> ()
       with _ -> ());
    tbl)

let bmodule_to_library_ports (m : Behavioral_ir.bmodule) :
    Behavioral_ir.library_port list =
  List.filter_map (fun (s : Behavioral_ir.bsignal) ->
    match s.direction with
    | `Internal -> None
    (* library_port carries no bidirectional case, and the unisim port table
       already classes IOBUF.IO as an output; follow that convention. *)
    | `Inout -> None
    | (`Input | `Output) as d ->
        let width = match s.stype with
          | Behavioral_ir.BInt { width; _ } -> width
          | _ -> 1 in
        Some { Behavioral_ir.port_name = s.name;
               port_direction = d;
               port_width = width })
    m.signals

(* Look up port metadata for a list of cell-type names by parsing
 * Vivado's per-primitive `<NAME>.vhd` files through this same VHDL
 * frontend.  Returns one (name, port-list) pair per name that
 * resolved; silently skips names with no matching VHD file (the
 * caller decides whether an unresolved name is fatal — for the
 * Verible hook it isn't, since the user may have a non-Xilinx
 * primitive name).  Memoised across calls. *)
(* Builtin port interfaces for the non-clock-enable flip-flop variants
   (FD/FDC/FDP/FDS): Vivado ships FDCE/FDPE/FDRE/FDSE .vhd but NOT these, yet the
   Xilinx PCS/PMA transceiver source instantiates FDP (async preset, no CE) in
   its reset synchronisers.  All ports are 1-bit; yosys knows these cells, so
   SVS only needs the directions to wire them. *)
let builtin_prim_ports : (string * (string * [`Input|`Output]) list) list = [
  "FD",  [ "Q",`Output; "D",`Input; "C",`Input ];
  "FDC", [ "Q",`Output; "D",`Input; "C",`Input; "CLR",`Input ];
  "FDP", [ "Q",`Output; "D",`Input; "C",`Input; "PRE",`Input ];
  "FDS", [ "Q",`Output; "D",`Input; "C",`Input; "S",`Input ];
  (* Negative-edge variants.  Their .vhd is in unisims/retarget/ rather than
     primitive/, and although the bulk XIL_PRIM_PORTS_WRITE=all sweep reads it,
     the per-name lookup does not resolve it -- so seed them here, which also
     keeps them working on a machine with no Vivado at all.  Getting this wrong
     is SILENT: with no interface every pin defaults to `input`, the cell is
     emitted with no D port, and Vivado stops at link_design with
     "Cannot find port 'D' on instance ... of cell 'FDRE_1'".  yosys emits these
     for any negedge flop, so they turn up in ordinary designs. *)
  "FDRE_1", [ "Q",`Output; "C",`Input; "CE",`Input; "D",`Input; "R",`Input ];
  "FDSE_1", [ "Q",`Output; "C",`Input; "CE",`Input; "D",`Input; "S",`Input ];
  "FDCE_1", [ "Q",`Output; "C",`Input; "CE",`Input; "D",`Input; "CLR",`Input ];
  "FDPE_1", [ "Q",`Output; "C",`Input; "CE",`Input; "D",`Input; "PRE",`Input ];
]

(* Write the resolved primitive-interface cache back to the JSON fallback so a
 * Vivado machine can produce the file that a Vivado-free machine (no unisim
 * .vhd) then reads.  Opt-in via XIL_PRIM_PORTS_WRITE to avoid surprise writes;
 * format matches json_cache's reader above.
 *   XIL_PRIM_PORTS_WRITE=1    dump whatever the run resolved (per-flow subset)
 *   XIL_PRIM_PORTS_WRITE=all  first parse EVERY unisim <NAME>.vhd interface so
 *                             the cache is complete regardless of this flow. *)
let full_dump_done = ref false
let maybe_write_ports_cache () =
  match Sys.getenv_opt "XIL_PRIM_PORTS_WRITE" with
  | None -> ()
  | Some mode ->
    if mode = "all" && not !full_dump_done then begin
      full_dump_done := true;
      let vhd_paths =
        List.concat_map (fun d ->
          if (try Sys.is_directory d with _ -> false) then
            Array.to_list (Sys.readdir d)
            |> List.filter (fun f -> Filename.check_suffix f ".vhd")
            |> List.map (fun f -> Filename.concat d f)
          else []) xil_unisims_dirs in
      (* Parse each interface on its own so one unparseable secureip body does
         not abort the batch; skip cells already cached. *)
      List.iter (fun p ->
        match (try convert_vhdl_files_to_behavioral [p] with _ -> None) with
        | Some prog ->
            List.iter (fun (m : Behavioral_ir.bmodule) ->
              if not (Hashtbl.mem xil_primitive_cache m.name) then
                Hashtbl.replace xil_primitive_cache m.name
                  (bmodule_to_library_ports m)) prog.modules
        | None -> ()) vhd_paths
    end;
    let path = xil_ports_cache_path () in
    let dir_str = function `Input -> "input" | `Output -> "output" in
    let entries =
      Hashtbl.fold (fun cell ports acc ->
        (cell, `List (List.map (fun (p : Behavioral_ir.library_port) ->
           `Assoc [ "name",  `String p.port_name;
                    "dir",   `String (dir_str p.port_direction);
                    "width", `Int p.port_width ]) ports)) :: acc)
        xil_primitive_cache [] in
    let entries = List.sort (fun (a,_) (b,_) -> compare a b) entries in
    (try Yojson.Safe.to_file path (`Assoc entries)
     with e -> Printf.eprintf "[xil-ports] cache write to %s failed: %s\n"
                 path (Printexc.to_string e))

(* Verilator SPECIALISES modules by parameter and renames them, so a primitive
   arrives as MMCME2_ADV__CJz1_CHz2 / RAMB18E1__pi6 rather than MMCME2_ADV /
   RAMB18E1.  Every lookup here is keyed on the PLAIN primitive name (the
   unisim .vhd filename, the builtin table, the JSON cache), so the mangled name
   misses and gate_map aborts with "unresolved primitive port directions".  The
   verible frontend never does this, which is why the same design emits there.
   Strip a trailing __<suffix> and retry with the base name. *)
let strip_param_suffix n =
  match Str.bounded_split_delim (Str.regexp_string "__") n 2 with
  | base :: _ :: [] when base <> "" -> base
  | _ -> n

let lookup_xil_primitive_ports names :
    (string * Behavioral_ir.library_port list) list =
  let dedup = List.sort_uniq compare names in
  (* seed the cache with the builtin FF variants that have no Vivado .vhd *)
  List.iter (fun n ->
    if not (Hashtbl.mem xil_primitive_cache n) then
      match List.assoc_opt n builtin_prim_ports with
      | Some ps ->
          Hashtbl.replace xil_primitive_cache n
            (List.map (fun (pn, d) ->
               { Behavioral_ir.port_name = pn; port_direction = d;
                 port_width = 1 }) ps)
      | None -> ()) dedup;
  let need = List.filter (fun n ->
    not (Hashtbl.mem xil_primitive_cache n
         || Hashtbl.mem xil_primitive_missing n)) dedup in
  let to_parse = List.filter_map (fun n ->
    match List.find_map (fun d ->
            let p = Filename.concat d (n ^ ".vhd") in
            if Sys.file_exists p then Some p else None) xil_unisims_dirs with
    | Some p -> Some (n, p)
    | None ->
      (* No Vivado unisim .vhd on this machine — serve from the persistent JSON
         cache so the tool still resolves the interface. *)
      (match Hashtbl.find_opt (Lazy.force json_cache) n with
       | Some ports -> Hashtbl.replace xil_primitive_cache n ports; None
       | None ->
         (* retry under the un-specialised name (see strip_param_suffix) *)
         let base = strip_param_suffix n in
         (match (if base = n then None
                 else match Hashtbl.find_opt (Lazy.force json_cache) base with
                      | Some ports -> Some ports
                      | None ->
                        (match List.assoc_opt base builtin_prim_ports with
                         | Some ps -> Some (List.map (fun (pn, d) ->
                             { Behavioral_ir.port_name = pn; port_direction = d;
                               port_width = 1 }) ps)
                         | None -> None)) with
          | Some ports -> Hashtbl.replace xil_primitive_cache n ports; None
          | None -> Hashtbl.add xil_primitive_missing n (); None))) need in
  if to_parse <> [] then begin
    let paths = List.map snd to_parse in
    (match convert_vhdl_files_to_behavioral paths with
     | Some p ->
         List.iter (fun (m : Behavioral_ir.bmodule) ->
           Hashtbl.replace xil_primitive_cache m.name
             (bmodule_to_library_ports m)) p.modules;
         List.iter (fun (n, _) ->
           if not (Hashtbl.mem xil_primitive_cache n)
           then Hashtbl.add xil_primitive_missing n ()) to_parse
     | None ->
         List.iter (fun (n, _) ->
           Hashtbl.add xil_primitive_missing n ()) to_parse)
  end;
  let result = List.filter_map (fun n ->
    match Hashtbl.find_opt xil_primitive_cache n with
    | Some ports -> Some (n, ports)
    | None -> None) dedup in
  maybe_write_ports_cache ();
  result

(* ====================================================================== *)
(* Xilinx unisim primitive IMPLEMENTATION bodies                          *)
(*                                                                        *)
(* Variant of lookup_xil_primitive_ports that keeps the full bmodule —    *)
(* processes + signals + everything the VHDL parser produced — so the     *)
(* mapped-Netlist → Z3 verification loop (task #44) can substitute        *)
(* each binstance with its primitive body and miter against the source   *)
(* RTL.  Cached separately from the ports-only path.                     *)
(* ====================================================================== *)

let xil_primitive_impl_cache : (string, Behavioral_ir.bmodule) Hashtbl.t =
  Hashtbl.create 64

let lookup_xil_primitive_impl names :
    (string * Behavioral_ir.bmodule) list =
  let dedup = List.sort_uniq compare names in
  let need = List.filter (fun n ->
    not (Hashtbl.mem xil_primitive_impl_cache n
         || Hashtbl.mem xil_primitive_missing n)) dedup in
  let to_parse = List.filter_map (fun n ->
    match List.find_map (fun d ->
            let p = Filename.concat d (n ^ ".vhd") in
            if Sys.file_exists p then Some p else None) xil_unisims_dirs with
    | Some p -> Some (n, p)
    | None ->
      (* No Vivado unisim .vhd on this machine — serve from the persistent JSON
         cache so the tool still resolves the interface. *)
      (match Hashtbl.find_opt (Lazy.force json_cache) n with
       | Some ports -> Hashtbl.replace xil_primitive_cache n ports; None
       | None -> Hashtbl.add xil_primitive_missing n (); None)) need in
  if to_parse <> [] then begin
    let paths = List.map snd to_parse in
    (match convert_vhdl_files_to_behavioral paths with
     | Some p ->
         List.iter (fun (m : Behavioral_ir.bmodule) ->
           Hashtbl.replace xil_primitive_impl_cache m.name m) p.modules;
         List.iter (fun (n, _) ->
           if not (Hashtbl.mem xil_primitive_impl_cache n)
           then Hashtbl.add xil_primitive_missing n ()) to_parse
     | None ->
         List.iter (fun (n, _) ->
           Hashtbl.add xil_primitive_missing n ()) to_parse)
  end;
  List.filter_map (fun n ->
    match Hashtbl.find_opt xil_primitive_impl_cache n with
    | Some m -> Some (n, m)
    | None -> None) dedup

(* Helper: Convert a single VHDL file to behavioral IR *)
let convert_vhdl_file_to_behavioral filename =
  convert_vhdl_files_to_behavioral [filename]
