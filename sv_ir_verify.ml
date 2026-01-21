(* sv_ir_verify.ml - Z3-based verification for hardware IR *)

open Sv_ast

(* Z3 context *)
let ctx = Z3.mk_context [("model", "true"); ("timeout", "30000")]
let solver = Z3.Solver.mk_solver ctx None

(* Cache for Z3 variables *)
let z3_cache : (int, Z3.Expr.expr) Hashtbl.t = Hashtbl.create 256

let clear_cache () =
  Hashtbl.clear z3_cache;
  Z3.Solver.reset solver

(* Create or retrieve Z3 bitvector for a node ID *)
let get_z3_var ir node_id width =
  match Hashtbl.find_opt z3_cache node_id with
  | Some v -> v
  | None ->
      let name = Printf.sprintf "n%d_%s" node_id ir.ir_name in
      let v = Z3.BitVector.mk_const_s ctx name width in
      Hashtbl.add z3_cache node_id v;
      v

(* Helper to extend Z3 bitvectors to matching widths *)
let extend_to_match_width ctx signed a b =
  let width_a = Z3.BitVector.get_size (Z3.Expr.get_sort a) in
  let width_b = Z3.BitVector.get_size (Z3.Expr.get_sort b) in
  if width_a = width_b then
    (a, b)
  else if width_a < width_b then
    (* Extend a to match b *)
    let ext_a = if signed then
      Z3.BitVector.mk_sign_ext ctx (width_b - width_a) a
    else
      Z3.BitVector.mk_zero_ext ctx (width_b - width_a) a
    in
    (ext_a, b)
  else
    (* Extend b to match a *)
    let ext_b = if signed then
      Z3.BitVector.mk_sign_ext ctx (width_a - width_b) b
    else
      Z3.BitVector.mk_zero_ext ctx (width_a - width_b) b
    in
    (a, ext_b)

(* Helper to adjust Z3 bitvector to target width *)
let adjust_to_width ctx signed expr target_width =
  let current_width = Z3.BitVector.get_size (Z3.Expr.get_sort expr) in
  if current_width = target_width then
    expr
  else if current_width < target_width then
    (* Extend to target width *)
    if signed then
      Z3.BitVector.mk_sign_ext ctx (target_width - current_width) expr
    else
      Z3.BitVector.mk_zero_ext ctx (target_width - current_width) expr
  else
    (* Truncate to target width *)
    Z3.BitVector.mk_extract ctx (target_width - 1) 0 expr

(* Convert IR operation to Z3 expression *)
let rec ir_op_to_z3 ir node =
  let get_input_z3 input_id =
    match Hashtbl.find_opt ir.ir_nodes input_id with
    | Some inp_node -> ir_op_to_z3 ir inp_node
    | None ->
        (* Input is a constant or primary input *)
        (* Search for input by id *)
        let input_opt = Hashtbl.fold (fun _key value acc ->
          match value with
          | Input { id; name; width } when id = input_id -> Some (id, name, width)
          | _ -> acc
        ) ir.ir_inputs None in
        (match input_opt with
         | Some (id, _name, width) -> get_z3_var ir id width
         | None ->
             (* Check if it's a constant *)
             let const_opt = Hashtbl.fold (fun value id acc ->
               if id = input_id then Some value else acc
             ) ir.ir_constants None in
             (match const_opt with
              | Some value -> Z3.BitVector.mk_numeral ctx (string_of_int value) 32
              | None ->
                  Printf.eprintf "Warning: Unknown input node %d\n" input_id;
                  Z3.BitVector.mk_numeral ctx "0" 1))
  in

  let inputs_z3 = List.map get_input_z3 node.node_inputs in

  match node.node_op with
  | Add { width; signed } ->
      if List.length inputs_z3 >= 2 then
        let (a, b) = extend_to_match_width ctx signed (List.nth inputs_z3 0) (List.nth inputs_z3 1) in
        let result = Z3.BitVector.mk_add ctx a b in
        adjust_to_width ctx signed result width
      else
        Z3.BitVector.mk_numeral ctx "0" width

  | Sub { width; signed } ->
      if List.length inputs_z3 >= 2 then
        let (a, b) = extend_to_match_width ctx signed (List.nth inputs_z3 0) (List.nth inputs_z3 1) in
        let result = Z3.BitVector.mk_sub ctx a b in
        adjust_to_width ctx signed result width
      else
        Z3.BitVector.mk_numeral ctx "0" width

  | Mul { width; signed } ->
      if List.length inputs_z3 >= 2 then
        let (a, b) = extend_to_match_width ctx signed (List.nth inputs_z3 0) (List.nth inputs_z3 1) in
        let result = Z3.BitVector.mk_mul ctx a b in
        adjust_to_width ctx signed result width
      else
        Z3.BitVector.mk_numeral ctx "0" width

  | Div { width; signed } ->
      if List.length inputs_z3 >= 2 then
        let (a, b) = extend_to_match_width ctx signed (List.nth inputs_z3 0) (List.nth inputs_z3 1) in
        let result = if signed then
          Z3.BitVector.mk_sdiv ctx a b
        else
          Z3.BitVector.mk_udiv ctx a b
        in
        adjust_to_width ctx signed result width
      else
        Z3.BitVector.mk_numeral ctx "0" width

  | And { width } ->
      if List.length inputs_z3 >= 2 then
        let (a, b) = extend_to_match_width ctx false (List.nth inputs_z3 0) (List.nth inputs_z3 1) in
        let result = Z3.BitVector.mk_and ctx a b in
        adjust_to_width ctx false result width
      else if List.length inputs_z3 = 1 then
        adjust_to_width ctx false (List.nth inputs_z3 0) width
      else
        Z3.BitVector.mk_numeral ctx "0" width

  | Or { width } ->
      if List.length inputs_z3 >= 2 then
        let (a, b) = extend_to_match_width ctx false (List.nth inputs_z3 0) (List.nth inputs_z3 1) in
        let result = Z3.BitVector.mk_or ctx a b in
        adjust_to_width ctx false result width
      else if List.length inputs_z3 = 1 then
        adjust_to_width ctx false (List.nth inputs_z3 0) width
      else
        Z3.BitVector.mk_numeral ctx "0" width

  | Xor { width } ->
      if List.length inputs_z3 >= 2 then
        let (a, b) = extend_to_match_width ctx false (List.nth inputs_z3 0) (List.nth inputs_z3 1) in
        let result = Z3.BitVector.mk_xor ctx a b in
        adjust_to_width ctx false result width
      else if List.length inputs_z3 = 1 then
        adjust_to_width ctx false (List.nth inputs_z3 0) width
      else
        Z3.BitVector.mk_numeral ctx "0" width

  | Not { width } ->
      if List.length inputs_z3 >= 1 then
        Z3.BitVector.mk_not ctx (List.nth inputs_z3 0)
      else
        Z3.BitVector.mk_numeral ctx "0" width

  | Compare { width; cmp_op; signed } ->
      if List.length inputs_z3 >= 2 then begin
        let (a, b) = extend_to_match_width ctx signed (List.nth inputs_z3 0) (List.nth inputs_z3 1) in
        let cmp = match cmp_op, signed with
          | `Eq, _ -> Z3.Boolean.mk_eq ctx a b
          | `Ne, _ -> Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx a b)
          | `Lt, true -> Z3.BitVector.mk_slt ctx a b
          | `Lt, false -> Z3.BitVector.mk_ult ctx a b
          | `Le, true -> Z3.BitVector.mk_sle ctx a b
          | `Le, false -> Z3.BitVector.mk_ule ctx a b
          | `Gt, true -> Z3.BitVector.mk_sgt ctx a b
          | `Gt, false -> Z3.BitVector.mk_ugt ctx a b
          | `Ge, true -> Z3.BitVector.mk_sge ctx a b
          | `Ge, false -> Z3.BitVector.mk_uge ctx a b
        in
        (* Convert boolean to 1-bit bitvector *)
        Z3.Boolean.mk_ite ctx cmp
          (Z3.BitVector.mk_numeral ctx "1" 1)
          (Z3.BitVector.mk_numeral ctx "0" 1)
      end else
        Z3.BitVector.mk_numeral ctx "0" 1

  | Mux { width } ->
      if List.length inputs_z3 >= 3 then begin
        let sel = List.nth inputs_z3 0 in
        let true_val = List.nth inputs_z3 1 in
        let false_val = List.nth inputs_z3 2 in
        (* Convert select to boolean *)
        let sel_bool = Z3.Boolean.mk_eq ctx sel (Z3.BitVector.mk_numeral ctx "1" 1) in
        Z3.Boolean.mk_ite ctx sel_bool true_val false_val
      end else
        Z3.BitVector.mk_numeral ctx "0" width

  | Shift { width; direction; arithmetic; amount } ->
      if List.length inputs_z3 >= 1 then begin
        let value = List.nth inputs_z3 0 in
        let shift_amount_raw = match amount with
          | Some amt -> Z3.BitVector.mk_numeral ctx (string_of_int amt) width
          | None ->
              if List.length inputs_z3 >= 2 then List.nth inputs_z3 1
              else Z3.BitVector.mk_numeral ctx "0" width
        in
        (* Ensure shift amount has same width as value for Z3 *)
        let (value_adj, shift_amount) = extend_to_match_width ctx false value shift_amount_raw in
        match direction, arithmetic with
        | `Left, _ -> Z3.BitVector.mk_shl ctx value_adj shift_amount
        | `Right, true -> Z3.BitVector.mk_ashr ctx value_adj shift_amount
        | `Right, false -> Z3.BitVector.mk_lshr ctx value_adj shift_amount
      end else
        Z3.BitVector.mk_numeral ctx "0" width

  | Concat { widths } ->
      (* Concatenate multiple inputs *)
      if List.length inputs_z3 >= 2 then
        List.fold_left (fun acc inp ->
          Z3.BitVector.mk_concat ctx acc inp
        ) (List.hd inputs_z3) (List.tl inputs_z3)
      else if List.length inputs_z3 = 1 then
        List.nth inputs_z3 0
      else
        Z3.BitVector.mk_numeral ctx "0" (List.fold_left (+) 0 widths)

  | Extract { width; lsb; msb } ->
      if List.length inputs_z3 >= 1 then begin
        let input = List.nth inputs_z3 0 in
        let input_width = Z3.BitVector.get_size (Z3.Expr.get_sort input) in
        (* Validate extract bounds *)
        if msb >= input_width || lsb < 0 || msb < lsb then begin
          Printf.eprintf "Error: Invalid extract [%d:%d] from width %d\n" msb lsb input_width;
          Z3.BitVector.mk_numeral ctx "0" width
        end else
          Z3.BitVector.mk_extract ctx msb lsb input
      end else
        Z3.BitVector.mk_numeral ctx "0" width

  | ZeroExtend { from_width; to_width } ->
      if List.length inputs_z3 >= 1 then
        Z3.BitVector.mk_zero_ext ctx (to_width - from_width) (List.nth inputs_z3 0)
      else
        Z3.BitVector.mk_numeral ctx "0" to_width

  | SignExtend { from_width; to_width } ->
      if List.length inputs_z3 >= 1 then
        Z3.BitVector.mk_sign_ext ctx (to_width - from_width) (List.nth inputs_z3 0)
      else
        Z3.BitVector.mk_numeral ctx "0" to_width

  | Register { width; clock; reset; enable } ->
      (* For combinational verification, treat register as wire *)
      if List.length inputs_z3 >= 1 then
        List.nth inputs_z3 0
      else
        Z3.BitVector.mk_numeral ctx "0" width

(* Build Z3 expressions for all IR outputs *)
let build_ir_z3_outputs ir =
  let outputs = Hashtbl.fold (fun name out acc ->
    match out with
    | Output { id; name; width } ->
        (* Find the node that drives this output *)
        (match Hashtbl.find_opt ir.ir_value_to_node id with
         | Some node_id ->
             (match Hashtbl.find_opt ir.ir_nodes node_id with
              | Some node ->
                  let z3_expr = ir_op_to_z3 ir node in
                  (name, z3_expr, width) :: acc
              | None -> acc)
         | None ->
             (* Output directly connected to input *)
             (match Hashtbl.find_opt ir.ir_inputs name with
              | Some (Input { id; name; width }) ->
                  let z3_var = get_z3_var ir id width in
                  (name, z3_var, width) :: acc
              | Some _ | None -> acc))
    | _ -> acc
  ) ir.ir_outputs [] in
  List.rev outputs

(* Verify equivalence of two IRs *)
let verify_ir_equivalence ir1 ir2 =
  clear_cache ();
  Printf.printf "Building Z3 expressions for IR1: %s\n" ir1.ir_name;
  let outputs1 = build_ir_z3_outputs ir1 in
  Printf.printf "  Found %d outputs\n" (List.length outputs1);

  Printf.printf "Building Z3 expressions for IR2: %s\n" ir2.ir_name;
  let outputs2 = build_ir_z3_outputs ir2 in
  Printf.printf "  Found %d outputs\n" (List.length outputs2);

  (* Match outputs by name and create equivalence constraints *)
  let matched = ref 0 in
  let constraints = List.fold_left (fun acc (name1, expr1, width1) ->
    match List.find_opt (fun (name2, _, _) -> name1 = name2) outputs2 with
    | Some (_, expr2, width2) ->
        if width1 = width2 then begin
          matched := !matched + 1;
          let eq = Z3.Boolean.mk_eq ctx expr1 expr2 in
          eq :: acc
        end else begin
          Printf.eprintf "Warning: Output %s has different widths: %d vs %d\n"
            name1 width1 width2;
          acc
        end
    | None ->
        Printf.eprintf "Warning: Output %s not found in second IR\n" name1;
        acc
  ) [] outputs1 in

  Printf.printf "Matched %d outputs\n" !matched;

  if constraints = [] then begin
    Printf.printf "No outputs to compare!\n";
    false
  end else begin
    (* Create conjunction of all equivalence constraints *)
    let all_eq = Z3.Boolean.mk_and ctx constraints in

    (* We want to find if there exists any input where outputs differ *)
    (* So we assert the NEGATION and check for SAT *)
    let not_eq = Z3.Boolean.mk_not ctx all_eq in
    Z3.Solver.add solver [not_eq];

    Printf.printf "Checking equivalence with Z3...\n";
    match Z3.Solver.check solver [] with
    | Z3.Solver.UNSATISFIABLE ->
        Printf.printf "✓ IRs are EQUIVALENT (no counterexample found)\n";
        true
    | Z3.Solver.SATISFIABLE ->
        Printf.printf "✗ IRs are NOT EQUIVALENT (counterexample exists)\n";
        (match Z3.Solver.get_model solver with
         | Some model ->
             Printf.printf "Counterexample:\n";
             Printf.printf "%s\n" (Z3.Model.to_string model)
         | None -> ());
        false
    | Z3.Solver.UNKNOWN ->
        Printf.printf "? Z3 returned UNKNOWN (timeout or complexity)\n";
        false
  end

(* Print IR statistics *)
let print_ir_stats ir =
  Printf.printf "\nIR Statistics for %s:\n" ir.ir_name;
  Printf.printf "  Inputs: %d\n" (Hashtbl.length ir.ir_inputs);
  Printf.printf "  Outputs: %d\n" (Hashtbl.length ir.ir_outputs);
  Printf.printf "  Nodes: %d\n" (Hashtbl.length ir.ir_nodes);
  Printf.printf "  Constants: %d\n" (Hashtbl.length ir.ir_constants);

  (* Count operation types *)
  let op_counts = Hashtbl.create 20 in
  Hashtbl.iter (fun _ node ->
    let op_name = match node.node_op with
      | Add _ -> "Add"
      | Sub _ -> "Sub"
      | Mul _ -> "Mul"
      | Div _ -> "Div"
      | And _ -> "And"
      | Or _ -> "Or"
      | Xor _ -> "Xor"
      | Not _ -> "Not"
      | Compare _ -> "Compare"
      | Mux _ -> "Mux"
      | Shift _ -> "Shift"
      | Concat _ -> "Concat"
      | Extract _ -> "Extract"
      | ZeroExtend _ -> "ZeroExtend"
      | SignExtend _ -> "SignExtend"
      | Register _ -> "Register"
    in
    let count = try Hashtbl.find op_counts op_name with Not_found -> 0 in
    Hashtbl.replace op_counts op_name (count + 1)
  ) ir.ir_nodes;

  Printf.printf "  Operation counts:\n";
  Hashtbl.iter (fun op_name count ->
    Printf.printf "    %s: %d\n" op_name count
  ) op_counts
