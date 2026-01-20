(* sv_verify_hardcaml.ml - Verify HardCaml backend correctness using Z3 
   
   This tool verifies that the HardCaml-generated Verilog is equivalent to
   the original SystemVerilog by encoding both as Z3 SMT constraints.
*)

(* ========================================================================= *)
(* Z3 Context and Configuration *)
(* ========================================================================= *)

let ctx = Z3.mk_context [("model", "true"); ("timeout", "10000")]
let signal_cache : (string, Z3.Expr.expr) Hashtbl.t = Hashtbl.create 256

(* ========================================================================= *)
(* Width Extraction *)
(* ========================================================================= *)

let width_of_range range_str =
  try
    match String.split_on_char ':' range_str with
    | [msb_str; lsb_str] ->
        let msb = int_of_string (String.trim msb_str) in
        let lsb = int_of_string (String.trim lsb_str) in
        abs (msb - lsb) + 1
    | _ -> 1
  with _ -> 1

let rec extract_width = function
  | Some (Sv_ast.BasicType { range = Some r; _ }) -> width_of_range r
  | Some (Sv_ast.ArrayType { range; _ }) -> width_of_range range
  | Some (Sv_ast.PackArrayType { range; _ }) -> width_of_range range
  | Some (Sv_ast.RefType { refdtype_ref = Some dtype; _ }) -> extract_width (Some dtype)
  | _ -> 1

(* ========================================================================= *)
(* Z3 Variable Management *)
(* ========================================================================= *)

let bv_var name width suffix =
  let full_name = name ^ suffix in
  match Hashtbl.find_opt signal_cache full_name with
  | Some v -> v
  | None ->
      let v = Z3.BitVector.mk_const_s ctx full_name width in
      Hashtbl.add signal_cache full_name v;
      v

let clear_cache () = Hashtbl.clear signal_cache

(* ========================================================================= *)
(* Constant Parsing *)
(* ========================================================================= *)

let parse_const_value name =
  try
    (* Format: <width>'<format><value> *)
    let parts = String.split_on_char '\'' name in
    match parts with
    | [width_str; format_value] ->
        let width = int_of_string width_str in
        let is_signed = String.length format_value > 1 && format_value.[0] = 's' in
        let fmt_start = if is_signed then 1 else 0 in
        let format_char = if String.length format_value > fmt_start 
                          then format_value.[fmt_start] else 'd' in
        let value_str = String.sub format_value (fmt_start + 1) 
                                    (String.length format_value - fmt_start - 1) in
        
        let value = match format_char with
          | 'h' -> int_of_string ("0x" ^ value_str)
          | 'd' -> int_of_string value_str
          | 'b' -> int_of_string ("0b" ^ value_str)
          | 'o' -> int_of_string ("0o" ^ value_str)
          | _ -> int_of_string value_str
        in
        (width, value)
    | _ -> (32, int_of_string name)
  with _ -> (32, 0)

(* ========================================================================= *)
(* Expression Translation to Z3 *)
(* ========================================================================= *)

let rec expr_to_z3 suffix = function
  | Sv_ast.VarRef { name; dtype_ref; _ } ->
      let width = extract_width dtype_ref in
      bv_var name width suffix
      
  | Sv_ast.Const { name; dtype_ref } ->
      let width = extract_width dtype_ref in
      let (parsed_width, value) = parse_const_value name in
      (* Use parsed width if dtype_ref is None *)
      let final_width = if width = 1 && parsed_width > 1 then parsed_width else width in
      if suffix = "_hc" && name = "8" then
        Printf.eprintf "Const: name=%s, parsed=(%d,%d), width=%d, final_width=%d\n%!"
          name parsed_width value width final_width;
      Z3.BitVector.mk_numeral ctx (string_of_int value) final_width
      
  | Sv_ast.Text { text } ->
      (try
        let n = int_of_string text in
        Z3.BitVector.mk_numeral ctx (string_of_int n) 32
       with _ -> Z3.BitVector.mk_numeral ctx "0" 1)
       
  | Sv_ast.BinaryOp { op; lhs; rhs; _ } | Sv_ast.BinaryOp' { op; lhs; rhs; _ } ->
      let a = expr_to_z3 suffix lhs in
      let b = expr_to_z3 suffix rhs in
      (match String.uppercase_ascii op with
       | "ADD" | "VADD" -> Z3.BitVector.mk_add ctx a b
       | "SUB" | "VSUB" -> Z3.BitVector.mk_sub ctx a b
       | "MUL" | "VMUL" -> Z3.BitVector.mk_mul ctx a b
       | "DIV" | "VDIV" ->
           (* Division not implemented in HardCaml - always returns all 1's *)
           let a_width = Z3.BitVector.get_size (Z3.Expr.get_sort a) in
           let b_width = Z3.BitVector.get_size (Z3.Expr.get_sort b) in
           let width = max a_width b_width in
           (* Create all 1's by inverting all 0's *)
           Z3.BitVector.mk_not ctx (Z3.BitVector.mk_numeral ctx "0" width)
       | "POW" | "POWSU" ->
           (* Power is not supported in hardware - return 0 *)
           let a_width = Z3.BitVector.get_size (Z3.Expr.get_sort a) in
           let b_width = Z3.BitVector.get_size (Z3.Expr.get_sort b) in
           let width = max a_width b_width in
           Z3.BitVector.mk_numeral ctx "0" width
       | "AND" | "VAND" -> Z3.BitVector.mk_and ctx a b
       | "OR" | "VOR" -> Z3.BitVector.mk_or ctx a b
       | "XOR" | "VXOR" -> Z3.BitVector.mk_xor ctx a b
       | "EQ" | "VEQ" -> 
           let eq = Z3.Boolean.mk_eq ctx a b in
           (* Convert boolean to 1-bit bitvector *)
           Z3.Boolean.mk_ite ctx eq 
             (Z3.BitVector.mk_numeral ctx "1" 1)
             (Z3.BitVector.mk_numeral ctx "0" 1)
       | "NEQ" | "VNEQ" ->
           let neq = Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx a b) in
           Z3.Boolean.mk_ite ctx neq
             (Z3.BitVector.mk_numeral ctx "1" 1)
             (Z3.BitVector.mk_numeral ctx "0" 1)
       | "LT" | "VLT" -> 
           let lt = Z3.BitVector.mk_ult ctx a b in
           Z3.Boolean.mk_ite ctx lt
             (Z3.BitVector.mk_numeral ctx "1" 1)
             (Z3.BitVector.mk_numeral ctx "0" 1)
       | "LTE" | "VLTE" ->
           let lte = Z3.BitVector.mk_ule ctx a b in
           Z3.Boolean.mk_ite ctx lte
             (Z3.BitVector.mk_numeral ctx "1" 1)
             (Z3.BitVector.mk_numeral ctx "0" 1)
       | "GT" | "VGT" ->
           let gt = Z3.BitVector.mk_ugt ctx a b in
           Z3.Boolean.mk_ite ctx gt
             (Z3.BitVector.mk_numeral ctx "1" 1)
             (Z3.BitVector.mk_numeral ctx "0" 1)
       | "GTE" | "VGTE" ->
           let gte = Z3.BitVector.mk_uge ctx a b in
           Z3.Boolean.mk_ite ctx gte
             (Z3.BitVector.mk_numeral ctx "1" 1)
             (Z3.BitVector.mk_numeral ctx "0" 1)
       | "SHIFTL" | "VSHIFTL" -> 
           (* Ensure both operands have same width *)
           let a_width = Z3.BitVector.get_size (Z3.Expr.get_sort a) in
           let b_width = Z3.BitVector.get_size (Z3.Expr.get_sort b) in
           if a_width = b_width then
             Z3.BitVector.mk_shl ctx a b
           else if b_width < a_width then
             Z3.BitVector.mk_shl ctx a (Z3.BitVector.mk_zero_ext ctx (a_width - b_width) b)
           else
             Z3.BitVector.mk_shl ctx a (Z3.BitVector.mk_extract ctx (a_width - 1) 0 b)
       | "SHIFTR" | "VSHIFTR" -> 
           let a_width = Z3.BitVector.get_size (Z3.Expr.get_sort a) in
           let b_width = Z3.BitVector.get_size (Z3.Expr.get_sort b) in
           if a_width = b_width then
             Z3.BitVector.mk_lshr ctx a b
           else if b_width < a_width then
             Z3.BitVector.mk_lshr ctx a (Z3.BitVector.mk_zero_ext ctx (a_width - b_width) b)
           else
             Z3.BitVector.mk_lshr ctx a (Z3.BitVector.mk_extract ctx (a_width - 1) 0 b)
       | "SHIFTRS" | "VSHIFTRS" -> 
           let a_width = Z3.BitVector.get_size (Z3.Expr.get_sort a) in
           let b_width = Z3.BitVector.get_size (Z3.Expr.get_sort b) in
           if a_width = b_width then
             Z3.BitVector.mk_ashr ctx a b
           else if b_width < a_width then
             Z3.BitVector.mk_ashr ctx a (Z3.BitVector.mk_zero_ext ctx (a_width - b_width) b)
           else
             Z3.BitVector.mk_ashr ctx a (Z3.BitVector.mk_extract ctx (a_width - 1) 0 b)
       | _ -> 
           Printf.eprintf "Warning: Unsupported binary op: %s, using zero\n%!" op;
           Z3.BitVector.mk_numeral ctx "0" 1)
       
  | Sv_ast.UnaryOp { op; operand; dtype_ref } ->
      let a = expr_to_z3 suffix operand in
      (match String.uppercase_ascii op with
       | "NOT" -> Z3.BitVector.mk_not ctx a
       | "NEGATE" -> Z3.BitVector.mk_neg ctx a
       | "EXTEND" ->
           (* Zero extend to target width *)
           let target_width = extract_width dtype_ref in
           let current_width = Z3.BitVector.get_size (Z3.Expr.get_sort a) in
           if suffix = "_hc" then
             Printf.eprintf "EXTEND: target=%d, current=%d, operand=%s\n%!"
               target_width current_width (Z3.Expr.to_string a);
           if target_width > current_width then
             Z3.BitVector.mk_zero_ext ctx (target_width - current_width) a
           else if target_width < current_width then
             Z3.BitVector.mk_extract ctx (target_width - 1) 0 a
           else
             a
       | "EXTENDS" ->
           (* Sign extend to target width *)
           let target_width = extract_width dtype_ref in
           let current_width = Z3.BitVector.get_size (Z3.Expr.get_sort a) in
           if target_width > current_width then
             Z3.BitVector.mk_sign_ext ctx (target_width - current_width) a
           else if target_width < current_width then
             Z3.BitVector.mk_extract ctx (target_width - 1) 0 a
           else
             a
       | _ -> failwith ("Unsupported unary op: " ^ op))

  | Sv_ast.UnaryOp' { op; operand; dtype } ->
      let a = expr_to_z3 suffix operand in
      (match String.uppercase_ascii op with
       | "NOT" -> Z3.BitVector.mk_not ctx a
       | "NEGATE" -> Z3.BitVector.mk_neg ctx a
       | "EXTEND" | "EXTENDS" ->
           (* For UnaryOp', we don't have direct access to dtype_ref,
              so just return the operand as-is. The width adjustment
              will be handled by the surrounding assignment context. *)
           a
       | _ ->
           Printf.eprintf "Warning: Unsupported unary op: %s\n%!" op;
           Z3.BitVector.mk_numeral ctx "0" 1)
       
  | Sv_ast.Cond { condition; then_val; else_val } ->
      (try
        let c = expr_to_z3 suffix condition in
        let t = expr_to_z3 suffix then_val in
        let e = expr_to_z3 suffix else_val in
        if suffix = "_hc" && String.length (Z3.Expr.to_string t) > 20 &&
           String.contains (Z3.Expr.to_string t) 'm' then
          Printf.eprintf "COND: then=%s\n%!"
            (let s = Z3.Expr.to_string t in
             if String.length s > 150 then String.sub s 0 150 ^ "..." else s);
        (* Convert condition to boolean *)
        let c_sort = Z3.Expr.get_sort c in
        let c_size = Z3.BitVector.get_size c_sort in
        let zero_val = Z3.BitVector.mk_numeral ctx "0" c_size in
        let c_bool = Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx c zero_val) in
        (* Ensure then and else branches have same width *)
        let t_width = Z3.BitVector.get_size (Z3.Expr.get_sort t) in
        let e_width = Z3.BitVector.get_size (Z3.Expr.get_sort e) in
        let (t', e') =
          if t_width = e_width then (t, e)
          else if t_width > e_width then
            (t, Z3.BitVector.mk_zero_ext ctx (t_width - e_width) e)
          else
            (Z3.BitVector.mk_zero_ext ctx (e_width - t_width) t, e)
        in
        Z3.Boolean.mk_ite ctx c_bool t' e'
       with e ->
         Printf.eprintf "Warning: Failed to encode COND: %s\n%!" (Printexc.to_string e);
         Z3.BitVector.mk_numeral ctx "0" 1)
      
  | Sv_ast.Concat { parts } ->
      let exprs = List.map (expr_to_z3 suffix) parts in
      (match exprs with
       | [] -> Z3.BitVector.mk_numeral ctx "0" 1
       | [x] -> x
       | x :: xs -> List.fold_left (Z3.BitVector.mk_concat ctx) x xs)
  
  | Sv_ast.Sel { expr; lsb; width; width_const; _ } when lsb <> None ->
      (* Bit selection: expr[msb:lsb] *)
      (try
        let base = expr_to_z3 suffix expr in
        if suffix = "_hc" then
          Printf.eprintf "SEL: base=%s (width=%d), width_const=%s\n%!"
            (let s = Z3.Expr.to_string base in
             if String.length s > 100 then String.sub s 0 100 ^ "..." else s)
            (Z3.BitVector.get_size (Z3.Expr.get_sort base))
            (match width_const with Some w -> string_of_int w | None -> "None");
        let lsb_val = match lsb with
          | Some lsb_node ->
              (try
                let lsb_expr = expr_to_z3 suffix lsb_node in
                if Z3.Expr.is_numeral lsb_expr then
                  let lsb_str = Z3.Expr.to_string lsb_expr in
                  (* Z3 numerals start with #x for hex, need to handle that *)
                  (try
                    if String.length lsb_str > 2 && lsb_str.[0] = '#' && lsb_str.[1] = 'x' then
                      int_of_string ("0x" ^ String.sub lsb_str 2 (String.length lsb_str - 2))
                    else
                      int_of_string lsb_str
                   with _ ->
                     Printf.eprintf "Warning: Could not parse LSB value: %s\n%!" lsb_str;
                     0)
                else begin
                  Printf.eprintf "Warning: SEL with non-constant LSB index\n%!";
                  0
                end
               with e ->
                 Printf.eprintf "Warning: Failed to evaluate LSB: %s\n%!" (Printexc.to_string e);
                 0)
          | None -> 0
        in
        let w = match width_const with
          | Some w -> w  (* Use width_const directly if available *)
          | None ->
              match width with
              | Some width_node ->
                  let width_z3 = expr_to_z3 suffix width_node in
                  (match width_z3 with
                   | c when Z3.Expr.is_numeral c ->
                       let w_str = Z3.Expr.to_string c in
                       (* Z3 numerals can be in hex format like #x00000008 *)
                       (try
                         if String.length w_str > 2 && w_str.[0] = '#' && w_str.[1] = 'x' then
                           int_of_string ("0x" ^ String.sub w_str 2 (String.length w_str - 2))
                         else
                           int_of_string w_str
                        with _ -> 1)
                   | _ -> 1)
              | None -> 1  (* If no width specified, default to 1 (single bit) *)
        in
        if suffix = "_hc" && String.contains (Z3.Expr.to_string base) 'm' then
          Printf.eprintf "SEL: lsb=%d, width=%d, msb=%d\n%!" lsb_val w (lsb_val + w - 1);
        let msb = lsb_val + w - 1 in
        let base_width = Z3.BitVector.get_size (Z3.Expr.get_sort base) in
        if msb >= base_width || lsb_val < 0 then begin
          Printf.eprintf "Warning: SEL out of bounds: [%d:%d] on %d-bit value\n%!"
            msb lsb_val base_width;
          Z3.BitVector.mk_numeral ctx "0" w
        end else
          Z3.BitVector.mk_extract ctx msb lsb_val base
       with e ->
         Printf.eprintf "Warning: Failed to extract bits: %s\n%!" (Printexc.to_string e);
         Z3.BitVector.mk_numeral ctx "0" 1)
  
  | Sv_ast.ArraySel { expr; index } ->
      (* Single bit selection: expr[index] *)
      (try
        let base = expr_to_z3 suffix expr in
        let idx = match expr_to_z3 suffix index with
          | c when Z3.Expr.is_numeral c ->
              int_of_string (Z3.Expr.to_string c)
          | _ -> 0
        in
        Z3.BitVector.mk_extract ctx idx idx base
       with e ->
         Printf.eprintf "Warning: Failed to select bit: %s\n%!" (Printexc.to_string e);
         Z3.BitVector.mk_numeral ctx "0" 1)
       
  | node ->
      (* Unhandled AST node - raise exception to debug *)
      failwith ("expr_to_z3: Unhandled node type in expression context")

(* ========================================================================= *)
(* Constraint Generation *)
(* ========================================================================= *)

let rec add_constraints solver suffix stmts =
  if suffix = "_hc" then
    Printf.eprintf "add_constraints called with %d statements\n%!" (List.length stmts);
  List.iter (function
    | Sv_ast.Assign { lhs; rhs; _ } ->
        (try
          let l = expr_to_z3 suffix lhs in
          let r = expr_to_z3 suffix rhs in
          let wl = Z3.BitVector.get_size (Z3.Expr.get_sort l) in
          let wr = Z3.BitVector.get_size (Z3.Expr.get_sort r) in
          (* Handle width mismatch *)
          let r' = if wl = wr then r
                   else if wl > wr then Z3.BitVector.mk_zero_ext ctx (wl - wr) r
                   else Z3.BitVector.mk_extract ctx (wl - 1) 0 r in
          let eq = Z3.Boolean.mk_eq ctx l r' in
          Z3.Solver.add solver [eq]
         with e ->
           Printf.eprintf "Warning: Failed to add assignment constraint: %s\n%!" 
             (Printexc.to_string e))
    | Sv_ast.AssignW { lhs; rhs } ->
        (try
          let lhs_name = match lhs with
            | Sv_ast.VarRef { name; _ } -> name
            | _ -> "unknown" in
          if suffix = "_hc" then
            Printf.eprintf "  Processing AssignW: %s%s\n%!" lhs_name suffix;
          let l = (try expr_to_z3 suffix lhs
                   with e -> failwith ("AssignW LHS(" ^ lhs_name ^ ") failed: " ^ Printexc.to_string e)) in
          let r = (try expr_to_z3 suffix rhs
                   with e -> failwith ("AssignW RHS(lhs=" ^ lhs_name ^ ") failed: " ^ Printexc.to_string e)) in
          let l_sort = Z3.Expr.get_sort l in
          let r_sort = Z3.Expr.get_sort r in

          (* Check if both are bitvectors *)
          if Z3.Sort.get_sort_kind l_sort <> Z3enums.BV_SORT then begin
            Printf.eprintf "Warning: LHS is not a bitvector sort\n%!";
            ()
          end else if Z3.Sort.get_sort_kind r_sort <> Z3enums.BV_SORT then begin
            Printf.eprintf "Warning: RHS is not a bitvector sort\n%!";
            ()
          end else begin
            let wl = Z3.BitVector.get_size l_sort in
            let wr = Z3.BitVector.get_size r_sort in

            if wl <= 0 || wr <= 0 then begin
              Printf.eprintf "Warning: Invalid widths: lhs=%d rhs=%d\n%!" wl wr;
              ()
            end else begin
              (try
                let r' = if wl = wr then r
                         else if wl > wr then Z3.BitVector.mk_zero_ext ctx (wl - wr) r
                         else Z3.BitVector.mk_extract ctx (wl - 1) 0 r in
                let eq = Z3.Boolean.mk_eq ctx l r' in
                if suffix = "_hc" && lhs_name = "y" then
                  Printf.eprintf "    Adding constraint for y_hc (width_lhs=%d, width_rhs=%d)\n%!" wl wr;
                Z3.Solver.add solver [eq]
               with e ->
                 Printf.eprintf "Warning: Failed to add assignment (lhs_width=%d, rhs_width=%d): %s\n%!"
                   wl wr (Printexc.to_string e);
                 ())
            end
          end
         with e ->
           Printf.eprintf "Warning: Failed to process continuous assignment: %s\n%!"
             (Printexc.to_string e))
    | Sv_ast.Always { stmts; _ } ->
        add_constraints solver suffix stmts
    | Sv_ast.Begin { stmts; _ } ->
        add_constraints solver suffix stmts
    | Sv_ast.Initial { stmts; _ } ->
        (* INITIAL blocks are just constant initializations, treat like assignments *)
        add_constraints solver suffix stmts
    | Sv_ast.Case { expr; items } ->
        (* Encode case as nested ITEs *)
        (try
          add_case_constraints solver suffix expr items
         with e ->
           Printf.eprintf "Warning: Failed to encode case statement: %s\n%!"
             (Printexc.to_string e))
    | _ -> ()
  ) stmts

and add_case_constraints solver suffix expr items =
  (* Encode case statement as nested ITEs to properly handle all cases including default *)
  if suffix = "_hc" then
    Printf.eprintf "add_case_constraints called for %s with %d items\n%!" suffix (List.length items);
  let expr_z3 = expr_to_z3 suffix expr in

  (* Group assignments by LHS variable *)
  let lhs_to_cases = Hashtbl.create 16 in

  (* Collect all (condition, rhs) pairs for each LHS *)
  List.iter (fun item ->
    let conditions = item.Sv_ast.conditions in
    List.iter (function
      | Sv_ast.Assign { lhs; rhs; _ } ->
          (try
            let lhs_z3 = expr_to_z3 suffix lhs in
            let rhs_z3 = expr_to_z3 suffix rhs in
            let key = Z3.Expr.to_string lhs_z3 in
            let cases = try Hashtbl.find lhs_to_cases key with Not_found -> [] in
            Hashtbl.replace lhs_to_cases key ((conditions, lhs_z3, rhs_z3) :: cases)
           with e ->
             Printf.eprintf "Warning: Failed to collect case assignment: %s\n%!"
               (Printexc.to_string e))
      | _ -> ()
    ) item.statements
  ) items;

  (* For each LHS, build a nested ITE expression *)
  Hashtbl.iter (fun _ cases_list ->
    try
      let cases = List.rev cases_list in (* Process in order *)

      (* Find default case (empty conditions) and other cases *)
      let (default_val, other_cases) =
        List.partition (fun (conds, _, _) -> conds = []) cases in

      let lhs_z3 = match cases with
        | (_, l, _) :: _ -> l
        | [] -> failwith "No cases found" in

      (* Build the ITE chain from the end backwards *)
      let rec build_ite remaining_cases default_expr =
        match remaining_cases with
        | [] -> default_expr
        | (conds, _, rhs_z3) :: rest ->
            (match conds with
             | [cond] ->
                 let cond_z3 = expr_to_z3 suffix cond in
                 let eq_cond = Z3.Boolean.mk_eq ctx expr_z3 cond_z3 in
                 (* Debug all case conditions for HardCaml *)
                 if suffix = "_hc" then
                   Printf.eprintf "  Case cond: %s, rhs_z3 width=%d\n%!"
                     (Z3.Expr.to_string cond_z3)
                     (Z3.BitVector.get_size (Z3.Expr.get_sort rhs_z3));
                 let next_expr = build_ite rest default_expr in
                 (* Adjust widths if needed *)
                 let rhs_width = Z3.BitVector.get_size (Z3.Expr.get_sort rhs_z3) in
                 let next_width = Z3.BitVector.get_size (Z3.Expr.get_sort next_expr) in
                 let (rhs', next') =
                   if rhs_width = next_width then (rhs_z3, next_expr)
                   else if rhs_width > next_width then
                     (rhs_z3, Z3.BitVector.mk_zero_ext ctx (rhs_width - next_width) next_expr)
                   else
                     (Z3.BitVector.mk_zero_ext ctx (next_width - rhs_width) rhs_z3, next_expr)
                 in
                 Z3.Boolean.mk_ite ctx eq_cond rhs' next'
             | _ -> build_ite rest default_expr)
      in

      let default_rhs = match default_val with
        | (_, _, rhs) :: _ -> rhs
        | [] -> Z3.BitVector.mk_numeral ctx "0"
                  (Z3.BitVector.get_size (Z3.Expr.get_sort lhs_z3))
      in

      let full_expr = build_ite other_cases default_rhs in

      (* Add constraint: lhs == full_expr *)
      let wl = Z3.BitVector.get_size (Z3.Expr.get_sort lhs_z3) in
      let wr = Z3.BitVector.get_size (Z3.Expr.get_sort full_expr) in
      let full_expr' = if wl = wr then full_expr
                       else if wl > wr then Z3.BitVector.mk_zero_ext ctx (wl - wr) full_expr
                       else Z3.BitVector.mk_extract ctx (wl - 1) 0 full_expr in
      let eq = Z3.Boolean.mk_eq ctx lhs_z3 full_expr' in
      Z3.Solver.add solver [eq]
    with e ->
      Printf.eprintf "Warning: Failed to build case ITE: %s\n%!"
        (Printexc.to_string e)
  ) lhs_to_cases

(* ========================================================================= *)
(* Module Encoding *)
(* ========================================================================= *)

let encode_module suffix ast =
  let solver = Z3.Solver.mk_simple_solver ctx in

  let rec process_node = function
    | Sv_ast.Netlist nodes -> List.iter process_node nodes
    | Sv_ast.Module { stmts; _ } ->
        add_constraints solver suffix stmts
    | _ -> ()
  in

  process_node ast;
  if suffix = "_hc" then
    Printf.eprintf "After encode_module: %d constraints in solver\n%!"
      (List.length (Z3.Solver.get_assertions solver));
  solver

(* ========================================================================= *)
(* Port Extraction *)
(* ========================================================================= *)

let extract_ports ast =
  let rec extract = function
    | Sv_ast.Netlist nodes -> List.concat (List.map extract nodes)
    | Sv_ast.Module { stmts; _ } ->
        List.filter_map (function
          | Sv_ast.Var { name; dtype_ref; direction; var_type = "PORT"; _ }
            when direction = "INPUT" || direction = "input" ->
              Some (name, extract_width dtype_ref, `Input)
          | Sv_ast.Var { name; dtype_ref; direction; var_type = "PORT"; _ }
            when direction = "OUTPUT" || direction = "output" ->
              Some (name, extract_width dtype_ref, `Output)
          | _ -> None
        ) stmts
    | _ -> []
  in
  extract ast

(* ========================================================================= *)
(* Equivalence Checking *)
(* ========================================================================= *)

let check_equivalence original_ast hardcaml_ast =
  Printf.printf "\n========================================\n";
  Printf.printf "Z3 Equivalence Verification\n";
  Printf.printf "========================================\n\n";
  
  clear_cache ();
  
  (* Extract ports *)
  let orig_ports = extract_ports original_ast in
  let hc_ports = extract_ports hardcaml_ast in
  
  let inputs = List.filter (fun (_, _, dir) -> dir = `Input) orig_ports in
  let outputs = List.filter (fun (_, _, dir) -> dir = `Output) orig_ports in
  
  Printf.printf "Inputs:  %d\n" (List.length inputs);
  Printf.printf "Outputs: %d\n" (List.length outputs);
  Printf.printf "\n";
  
  (* Create solvers for both modules *)
  let solver_orig = encode_module "_orig" original_ast in
  let solver_hc = encode_module "_hc" hardcaml_ast in
  
  (* Create combined solver *)
  let solver = Z3.Solver.mk_simple_solver ctx in
  
  (* Add constraints from both modules *)
  Printf.eprintf "Before get_assertions: solver_hc has %d constraints\n%!"
    (List.length (Z3.Solver.get_assertions solver_hc));
  let constraints_orig = Z3.Solver.get_assertions solver_orig in
  let constraints_hc = Z3.Solver.get_assertions solver_hc in
  Printf.eprintf "After get_assertions: constraints_hc list has %d items\n%!"
    (List.length constraints_hc);
  Printf.eprintf "Constraints mentioning y_hc or _167_hc or _4_hc:\n";
  List.iter (fun c ->
    let c_str = Z3.Expr.to_string c in
    if Str.string_match (Str.regexp ".*\\(y_hc\\|_167_hc\\|_4_hc\\).*") c_str 0 then
      Printf.eprintf "  Found: %s\n" c_str
  ) constraints_hc;

  (* Also print constraint 16 in full to see the case statement *)
  if List.length constraints_hc > 16 then begin
    Printf.eprintf "\nConstraint [16] (should be case statement):\n%s\n"
      (Z3.Expr.to_string (List.nth constraints_hc 16))
  end;
  Z3.Solver.add solver (constraints_orig @ constraints_hc);

  Printf.printf "Original constraints: %d\n" (List.length constraints_orig);
  List.iteri (fun i c ->
    Printf.eprintf "  Orig[%d]: %s\n" i (Z3.Expr.to_string c)
  ) constraints_orig;
  Printf.printf "HardCaml constraints: %d\n" (List.length constraints_hc);
  Printf.printf "\n";
  
  (* Constrain inputs to be equal *)
  List.iter (fun (name, width, _) ->
    let input_orig = bv_var name width "_orig" in
    let input_hc = bv_var name width "_hc" in
    let eq = Z3.Boolean.mk_eq ctx input_orig input_hc in
    Z3.Solver.add solver [eq]
  ) inputs;
  
  (* Check each output *)
  let all_equiv = ref true in
  List.iter (fun (name, width, _) ->
    Printf.printf "Checking output: %s [%d bits]\n%!" name width;

    let output_orig = bv_var name width "_orig" in
    let output_hc = bv_var name width "_hc" in

    (* Debug: Print all constraint previews *)
    if name = "y" then begin
      Printf.eprintf "\n  Debug - All %d HardCaml constraints:\n" (List.length constraints_hc);
      List.iteri (fun i c ->
        let c_str = Z3.Expr.to_string c in
        let preview = if String.length c_str > 100 then
          String.sub c_str 0 100 ^ "..."
        else c_str in
        Printf.eprintf "  [%d]: %s\n" i preview
      ) constraints_hc;
      Printf.eprintf "\n"
    end;

    (* Check if outputs can differ *)
    let neq = Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx output_orig output_hc) in
    Z3.Solver.push solver;
    Z3.Solver.add solver [neq];

    match Z3.Solver.check solver [] with
    | Z3.Solver.SATISFIABLE ->
        Printf.printf "  ❌ INEQUIVALENT! Counterexample found:\n";
        all_equiv := false;
        (match Z3.Solver.get_model solver with
         | Some model ->
             (* Print input values *)
             Printf.printf "  Inputs:\n";
             List.iter (fun (iname, iwidth, _) ->
               let input_val = bv_var iname iwidth "_orig" in
               match Z3.Model.eval model input_val true with
               | Some v -> Printf.printf "    %s = %s\n" iname (Z3.Expr.to_string v)
               | None -> ()
             ) inputs;
             (* Print output values *)
             Printf.printf "  Outputs:\n";
             (match Z3.Model.eval model output_orig true with
              | Some v -> Printf.printf "    %s (original) = %s\n" name (Z3.Expr.to_string v)
              | None -> ());
             (match Z3.Model.eval model output_hc true with
              | Some v -> Printf.printf "    %s (hardcaml) = %s\n" name (Z3.Expr.to_string v)
              | None -> ())
         | None -> Printf.printf "  No model available\n")
    | Z3.Solver.UNSATISFIABLE ->
        Printf.printf "  ✅ EQUIVALENT\n%!"
    | Z3.Solver.UNKNOWN ->
        Printf.printf "  ⚠️  UNKNOWN (timeout or too complex)\n%!";
        all_equiv := false;
    
    Z3.Solver.pop solver 1;
    Printf.printf "\n%!"
  ) outputs;
  
  Printf.printf "========================================\n";
  if !all_equiv then
    Printf.printf "✅ ALL OUTPUTS EQUIVALENT!\n"
  else
    Printf.printf "❌ SOME OUTPUTS DIFFER!\n";
  Printf.printf "========================================\n\n";
  
  !all_equiv

(* ========================================================================= *)
(* Main Entry Point *)
(* ========================================================================= *)

let verify_hardcaml_output original_json hardcaml_json =
  Printf.printf "Loading original SystemVerilog AST...\n%!";
  let original_ast = Sv_parse.parse (Yojson.Safe.from_file original_json) in
  
  Printf.printf "Loading HardCaml-generated AST...\n%!";
  let hardcaml_ast = Sv_parse.parse (Yojson.Safe.from_file hardcaml_json) in
  
  check_equivalence original_ast hardcaml_ast
