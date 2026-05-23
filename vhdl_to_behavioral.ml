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
           BConst { value = ord; width = bits_needed n }
       | None -> BVar name)

  (* Integer literal *)
  | Double (VhdIntPrimary, Num num_str) ->
      (try
         let value = int_of_string num_str in
         BConst { value; width = 32 }
       with _ -> BConst { value = 0; width = 32 })

  (* Bit string literal: "0001", "1100", etc. *)
  | Double (VhdOperatorString, Str bit_str) ->
      (try
         let value = int_of_string ("0b" ^ bit_str) in
         BConst { value; width = String.length bit_str }
       with _ -> BConst { value = 0; width = 1 })

  (* Character literal: '0', '1' *)
  | Double (VhdCharPrimary, Char c) ->
      let value = if c = '1' then 1 else 0 in
      BConst { value; width = 1 }

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
      BConst { value = 0; width = 1 }
  | Triple (VhdDecreasingRange, hi, _) -> expr_to_bexpr ctx hi
  | Triple (VhdIncreasingRange, _, hi) -> expr_to_bexpr ctx hi

  (* Qualified expression `T'(expr)` — drop the type qualifier and
     evaluate the inner expr.  Triple form: (qualified, type, expr). *)
  | Triple (VhdQualifiedExpression, _, inner) -> expr_to_bexpr ctx inner
  | Triple (VhdQualifiedAggregate, _, inner) -> expr_to_bexpr ctx inner

  (* Physical literals like `5 ns` — non-synthesisable, treat as 0. *)
  | Double (VhdPhysicalPrimary, _) -> BConst { value = 0; width = 32 }

  (* Float / abs / new — rare, non-synthesisable in our scope. *)
  | Double (VhdFloatPrimary, _) -> BConst { value = 0; width = 32 }
  | Double (VhdAbsFactor, e) -> expr_to_bexpr ctx e
  | Double (VhdNewFactor, _) -> BConst { value = 0; width = 1 }

  (* Bare leaf primary literals that escape the named wrappers.
     Some VHDL parses leave `Num` directly as an atomic literal. *)
  | Num s ->
      (try BConst { value = int_of_string s; width = 32 }
       with _ -> BConst { value = 0; width = 32 })
  | Char '0' -> BConst { value = 0; width = 1 }
  | Char '1' -> BConst { value = 1; width = 1 }

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
      BConst { value = v; width = 1 }

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
           (match pairs with [] -> BConst { value = 0; width = 1 }
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
      BConst { value = 1; width = 1 }

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
      let actual_e = match params with
        | Triple (Vhdassociation_element, _, Double (VhdActualExpression, e)) -> e
        | Triple (Vhdassociation_element, _, e) -> e
        | other -> other
      in
      BCall { func = name; args = [expr_to_bexpr ctx actual_e] }

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

  | VhdNone -> BConst { value = 0; width = 1 }

  (* Bare List in expr context: typically an aggregate body or
     element-association list whose outer wrapper was already
     consumed.  Empty list → 0; single → recurse; multiple → concat
     in MSB-first order. *)
  | List [] -> BConst { value = 0; width = 1 }
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
  | Char _ -> BConst { value = 0; width = 1 }

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
      Printf.eprintf "[vhdl2bir] expr unhandled %s\n%!" shape;
      BConst { value = 0; width = 1 }

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

(* Convert VHDL statements to behavioral IR statements *)
and stmt_to_bstmt ctx = function
  (* Signal assignment: signal <= value *)
  | Double (VhdSequentialSignalAssignment,
           Double (VhdSimpleSignalAssignment,
                  Quintuple (Vhdsimple_signal_assignment_statement,
                            Str "", Str name, VhdDelayNone,
                            Double (Vhdwaveform_element, rhs)))) ->
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
                            Str "", target, VhdDelayNone,
                            Double (Vhdwaveform_element, rhs)))) ->
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
                                            rhs = BConst { value = 1; width = 32 };
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
      let shape = match other with
        | Double (h, _) -> Printf.sprintf "Double(%s)" (head_name h)
        | Triple (h, _, _) -> Printf.sprintf "Triple(%s)" (head_name h)
        | Quadruple (h, _, _, _) -> Printf.sprintf "Quad(%s)" (head_name h)
        | Quintuple (h, _, _, _, _) -> Printf.sprintf "Quint(%s)" (head_name h)
        | _ -> "leaf"
      in
      Printf.eprintf "[vhdl2bir] stmt unhandled %s\n%!" shape;
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
              | Some rst -> reset_info := Some (rst, `Pos)
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
let rec elide_clock_guard clk = function
  | [] -> []
  | [BIf { condition; then_stmts; else_stmts = [] }]
    when (match condition with
          | BBinOp { op = BAnd; rhs = BBinOp { op = BEq;
                       lhs = BVar c; rhs = BConst { value = 1; _ } }; _ }
            when c = clk -> true
          | BBinOp { op = BAnd; lhs = BBinOp { op = BEq;
                       lhs = BVar c; rhs = BConst { value = 1; _ } }; _ }
            when c = clk -> true
          | _ -> false) -> then_stmts
  | x :: tl -> x :: elide_clock_guard clk tl

(* `if rising_edge(clk) then BODY end if;` (no else) is a pure clock
   guard — once we know the process is sequential on `clk`, the BIf
   itself is redundant and we want to expose BODY directly. *)
let is_clock_guard clk = function
  | BCall { func = ("rising_edge"|"falling_edge"); args = [BVar c] } -> c = clk
  | BSelect { array = BVar ("rising_edge"|"falling_edge"); index = BVar c } -> c = clk
  | BBinOp { op = BAnd; rhs = BBinOp { op = BEq;
               lhs = BVar c; rhs = BConst { value = 1; _ } }; _ } -> c = clk
  | BBinOp { op = BAnd; lhs = BBinOp { op = BEq;
               lhs = BVar c; rhs = BConst { value = 1; _ } }; _ } -> c = clk
  | _ -> false

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
      }

  | None ->
      (* Combinational process *)
      BCombinational {
        name;
        sensitivity = [BAny];  (* Simplify for now *)
        body = body_stmts;
      }

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
                        Sextuple (Vhdinterface_default_declaration, Str name,
                                 VhdInterfaceModeIn, subtype, _kind, _default))) ->
            let stype = infer_btype ctx subtype in
            let signal = {
              name; stype; direction = `Input;
              initial_value = None; attrs = [];
            } in
            add_signal_type ctx name stype;
            [signal]

        | Double (VhdInterfaceObjectDeclaration,
                 Double (VhdInterfaceDefaultDeclaration,
                        Sextuple (Vhdinterface_default_declaration, Str name,
                                 VhdInterfaceModeOut, subtype, _kind, _default))) ->
            let stype = infer_btype ctx subtype in
            let signal = {
              name; stype; direction = `Output;
              initial_value = None; attrs = [];
            } in
            add_signal_type ctx name stype;
            [signal]

        | Double (VhdInterfaceObjectDeclaration,
                 Double (VhdInterfaceDefaultDeclaration,
                        Sextuple (Vhdinterface_default_declaration, Str name,
                                 VhdInterfaceModeInOut, subtype, _kind, _default))) ->
            let stype = infer_btype ctx subtype in
            let signal = {
              name; stype; direction = `Output;  (* approximate inout as out *)
              initial_value = None; attrs = [];
            } in
            add_signal_type ctx name stype;
            [signal]

        | _ -> []
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
          initial_value = Some (BConst { value = 0; width = init_w });
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
      let rec extract_stmts = function
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
                            Double (VhdInstantiatedComponent, Str ref_name),
                            _generic_assoc,
                            port_assoc_list)) ->
            let port_connections = connect_ports port_assoc_list in
            instances := {
              inst_name; module_name = ref_name;
              param_values = [];
              port_connections;
            } :: !instances

        (* Direct entity instantiation:
              `inst : entity work.foo(rtl) PORT MAP (…)`
           wraps the ref name in VhdInstantiatedEntityArchitecture.
           The architecture name is optional; pull just the entity. *)
        | Double (VhdConcurrentComponentInstantiationStatement,
                 Quintuple (Vhdcomponent_instantiation_statement,
                            Str inst_name,
                            Double (VhdInstantiatedEntityArchitecture, ref_node),
                            _generic_assoc,
                            port_assoc_list)) ->
            let ref_name = match ref_node with
              | Str n -> n
              | Double (_, Str n) -> n
              | Triple (_, Str n, _) -> n
              | _ -> "_unknown_entity"
            in
            let port_connections = connect_ports port_assoc_list in
            instances := {
              inst_name; module_name = ref_name;
              param_values = [];
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
              | [] -> BAssign { lhs = dst; rhs = BConst { value = 0; width = 1 } }
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
              | _ -> BAssign { lhs = dst; rhs = BConst { value = 0; width = 1 } }
            in
            let body = [build_chain alts] in
            processes := BCombinational {
              name = next_conc_name ();
              sensitivity = [BAny];
              body;
            } :: !processes

        | other ->
            if Sys.getenv_opt "VHDL2BIR_DEBUG" <> None then begin
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
              Printf.eprintf "[vhdl2bir] arch_stmt unhandled: %s\n%!" tag
            end
      in
      extract_stmts stmts;

      (!internal_signals, !processes, !instances)

  | _ -> ([], [], [])

(* Main conversion function *)
let convert_vhdl_to_behavioral vhdl_ast =
  (* The input may carry several designs (entity+architecture pairs)
     when multiple .vhd files were parsed together.  Build one bmodule
     per entity rather than folding everything into a single module
     (the old behaviour mashed all units together — for a multi-file
     parse that produced one giant self-referential module that hung
     the flattener). *)
  let entities =
    List.filter_map (fun du ->
      let (n, p) = extract_entity_ports (create_context ()) du in
      if n <> "" then Some (n, p) else None) vhdl_ast
  in
  let modules =
    List.map (fun (entity_name, entity_ports) ->
      (* Fresh context per module so signal-name state doesn't bleed
         across designs.  extract_architecture filters by entity_name,
         so folding over all units picks this entity's architecture. *)
      let ctx = create_context () in
      let (internal_signals, processes, instances) =
        List.fold_left (fun (sigs, procs, insts) design_unit ->
          let (s, p, i) = extract_architecture ctx entity_name design_unit in
          (s @ sigs, p @ procs, i @ insts)
        ) ([], [], []) vhdl_ast
      in
      {
        name = entity_name;
        params = [];
        signals = entity_ports @ internal_signals;
        processes;
        instances;
        funcs = [];
        mems = []; attrs = [];
      }
    ) entities
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

      (* Restore old hashtable and settings before returning *)
      Vhd_front.Vabstraction.vhdlhash := old_hash;
      Vhd_front.VhdlSettings.settings := old_settings;

      Some result
    end
  with _ ->
    Vhd_front.Vabstraction.vhdlhash := old_hash;
    Vhd_front.VhdlSettings.settings := old_settings;
    None

(* Helper: Convert a single VHDL file to behavioral IR *)
let convert_vhdl_file_to_behavioral filename =
  convert_vhdl_files_to_behavioral [filename]
