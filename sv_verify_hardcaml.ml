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
      let (_, value) = parse_const_value name in
      Z3.BitVector.mk_numeral ctx (string_of_int value) width
      
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
       | "DIV" | "VDIV" -> Z3.BitVector.mk_udiv ctx a b  (* Unsigned division *)
       | "POW" -> 
           (* Power is complex - for now, approximate with multiplication *)
           (* TODO: Proper power implementation *)
           Z3.BitVector.mk_mul ctx a b
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
       
  | Sv_ast.UnaryOp { op; operand; _ } | Sv_ast.UnaryOp' { op; operand; _ } ->
      let a = expr_to_z3 suffix operand in
      (match String.uppercase_ascii op with
       | "NOT" -> Z3.BitVector.mk_not ctx a
       | "NEGATE" -> Z3.BitVector.mk_neg ctx a
       | _ -> failwith ("Unsupported unary op: " ^ op))
       
  | Sv_ast.Cond { condition; then_val; else_val } ->
      let c = expr_to_z3 suffix condition in
      let t = expr_to_z3 suffix then_val in
      let e = expr_to_z3 suffix else_val in
      (* Convert condition to boolean *)
      let c_sort = Z3.Expr.get_sort c in
      let c_size = Z3.BitVector.get_size c_sort in
      let zero_val = Z3.BitVector.mk_numeral ctx "0" c_size in
      let c_bool = Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx c zero_val) in
      Z3.Boolean.mk_ite ctx c_bool t e
      
  | Sv_ast.Concat { parts } ->
      let exprs = List.map (expr_to_z3 suffix) parts in
      (match exprs with
       | [] -> Z3.BitVector.mk_numeral ctx "0" 1
       | [x] -> x
       | x :: xs -> List.fold_left (Z3.BitVector.mk_concat ctx) x xs)
  
  | Sv_ast.Sel { expr; lsb = Some lsb_node; width = Some width_node; _ } ->
      (* Bit selection: expr[msb:lsb] *)
      (try
        let base = expr_to_z3 suffix expr in
        let lsb = match expr_to_z3 suffix lsb_node with
          | c when Z3.Expr.is_numeral c -> 
              int_of_string (Z3.Expr.to_string c)
          | _ -> 0
        in
        let w = match expr_to_z3 suffix width_node with
          | c when Z3.Expr.is_numeral c ->
              int_of_string (Z3.Expr.to_string c)
          | _ -> 1
        in
        let msb = lsb + w - 1 in
        Z3.BitVector.mk_extract ctx msb lsb base
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
      let node_type = match node with
        | Sv_ast.Module _ -> "Module"
        | Sv_ast.Netlist _ -> "Netlist"
        | Sv_ast.Var _ -> "Var"
        | Sv_ast.Always _ -> "Always"
        | Sv_ast.Assign _ -> "Assign"
        | Sv_ast.AssignW _ -> "AssignW"
        | Sv_ast.Begin _ -> "Begin"
        | Sv_ast.Case _ -> "Case"
        | Sv_ast.If _ -> "If"
        | _ -> "Unknown"
      in
      Printf.eprintf "Warning: Unsupported expression node type: %s, returning zero\n%!" node_type;
      Z3.BitVector.mk_numeral ctx "0" 1

(* ========================================================================= *)
(* Constraint Generation *)
(* ========================================================================= *)

let rec add_constraints solver suffix stmts =
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
          let l = expr_to_z3 suffix lhs in
          let r = expr_to_z3 suffix rhs in
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
              let r' = if wl = wr then r
                       else if wl > wr then Z3.BitVector.mk_zero_ext ctx (wl - wr) r
                       else Z3.BitVector.mk_extract ctx (wl - 1) 0 r in
              let eq = Z3.Boolean.mk_eq ctx l r' in
              Z3.Solver.add solver [eq]
            end
          end
         with e ->
           Printf.eprintf "Warning: Failed to add continuous assignment: %s\n%!"
             (Printexc.to_string e))
    | Sv_ast.Always { stmts; _ } ->
        add_constraints solver suffix stmts
    | Sv_ast.Begin { stmts; _ } ->
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
  (* Encode case statement as: for each output variable assigned in ANY case item,
     create a constraint that captures all possible assignments *)
  
  (* Collect all assignments across all case items *)
  let expr_z3 = expr_to_z3 suffix expr in
  
  (* For each case item, create (condition ==> assignments) *)
  List.iter (fun item ->
    match item.Sv_ast.conditions with
    | [cond] ->
        (try
          let cond_z3 = expr_to_z3 suffix cond in
          let eq_cond = Z3.Boolean.mk_eq ctx expr_z3 cond_z3 in
          
          (* Process each assignment in this case *)
          List.iter (function
            | Sv_ast.Assign { lhs; rhs; _ } ->
                (try
                  let l = expr_to_z3 suffix lhs in
                  let r = expr_to_z3 suffix rhs in
                  let wl = Z3.BitVector.get_size (Z3.Expr.get_sort l) in
                  let wr = Z3.BitVector.get_size (Z3.Expr.get_sort r) in
                  let r' = if wl = wr then r
                           else if wl > wr then Z3.BitVector.mk_zero_ext ctx (wl - wr) r
                           else Z3.BitVector.mk_extract ctx (wl - 1) 0 r in
                  
                  (* Add: (expr == cond) => (lhs == rhs) *)
                  let assignment = Z3.Boolean.mk_eq ctx l r' in
                  let implication = Z3.Boolean.mk_implies ctx eq_cond assignment in
                  Z3.Solver.add solver [implication]
                 with e ->
                   Printf.eprintf "Warning: Failed to add case assignment: %s\n%!" 
                     (Printexc.to_string e))
            | _ -> ()
          ) item.statements
         with e ->
           Printf.eprintf "Warning: Failed to process case item: %s\n%!" 
             (Printexc.to_string e))
    | _ -> ()
  ) items

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
  let constraints_orig = Z3.Solver.get_assertions solver_orig in
  let constraints_hc = Z3.Solver.get_assertions solver_hc in
  Z3.Solver.add solver (constraints_orig @ constraints_hc);
  
  Printf.printf "Original constraints: %d\n" (List.length constraints_orig);
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
