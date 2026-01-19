open Z3

open Sv_ast  (* uses your provided .mli *)

(* ----------------------------------------------------------- *)
(* Context management                                           *)
(* ----------------------------------------------------------- *)

let ctx = Z3.mk_context [("model", "true")]

(* cache for Z3 variables *)
let signal_table : (string, Z3.Expr.expr) Hashtbl.t = Hashtbl.create 97

let bv_width_of_type = function
  | Some (BasicType { range = Some r; _ }) -> (
      try
        let parts = String.split_on_char ':' r in
        match parts with
        | [msb; lsb] -> abs (int_of_string msb - int_of_string lsb) + 1
        | _ -> 1
      with _ -> 1 )
  | _ -> 1

(* create or lookup a bitvector variable *)
let bv_var name width =
  match Hashtbl.find_opt signal_table name with
  | Some v -> v
  | None ->
      let v = Z3.BitVector.mk_const_s ctx name width in
      Hashtbl.add signal_table name v;
      v

(* ----------------------------------------------------------- *)
(* Expression translation                                       *)
(* ----------------------------------------------------------- *)

let rec expr_to_z3 (n : sv_node) : Z3.Expr.expr =
  match n with
  | Var { name; dtype_ref; _ } ->
      bv_var name (bv_width_of_type dtype_ref)

  | VarRef { name; dtype_ref; _ } ->
      bv_var name (bv_width_of_type dtype_ref)

  | Const { dtype_ref = Some (BasicType { range = Some r; _ }); _ } ->
      let w = bv_width_of_type (Some (BasicType { keyword = "logic"; range = Some r })) in
      Z3.BitVector.mk_numeral ctx "0" w

  | Text { text } -> (
      try
        (* numeric constant, e.g. 8'hFF *)
        if String.contains text '\'' then
          let parts = String.split_on_char '\'' text in
          match parts with
          | [w; _; v] ->
              let width = int_of_string w in
              Z3.BitVector.mk_numeral ctx v width
          | _ -> Z3.BitVector.mk_numeral ctx text 32
        else
          Z3.BitVector.mk_numeral ctx text 32
      with _ -> Z3.BitVector.mk_numeral ctx "0" 1 )

  | BinaryOp { op; lhs; rhs; _ } -> (
      let a = expr_to_z3 lhs in
      let b = expr_to_z3 rhs in
      match op with
      | "+" -> Z3.BitVector.mk_add ctx a b
      | "-" -> Z3.BitVector.mk_sub ctx a b
      | "&" -> Z3.BitVector.mk_and ctx a b
      | "|" -> Z3.BitVector.mk_or ctx a b
      | "^" -> Z3.BitVector.mk_xor ctx a b
      | "==" -> Z3.Boolean.mk_eq ctx a b
      | "!=" -> Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx a b)
      | _ -> failwith ("unsupported binary op: " ^ op)
    )

  | UnaryOp { op; operand; _ } -> (
      let a = expr_to_z3 operand in
      match op with
      | "~" -> Z3.BitVector.mk_not ctx a
      | "-" -> Z3.BitVector.mk_neg ctx a
      | _ -> failwith ("unsupported unary op: " ^ op)
    )

  | Cond { condition; then_val; else_val } ->
      let c = expr_to_z3 condition in
      let t = expr_to_z3 then_val in
      let e = expr_to_z3 else_val in
      Z3.Boolean.mk_ite ctx c t e

  | _ ->
      failwith ("unsupported node in expr_to_z3: " ^
                (match n with Unknown (s, _) -> s | _ -> "complex"))
;;

(* ----------------------------------------------------------- *)
(* Build solver constraints from Assign etc.                    *)
(* ----------------------------------------------------------- *)

let rec add_constraints solver (stmts : sv_node list) =
  List.iter (fun s ->
      match s with
      | Assign { lhs; rhs; _ } ->
          let l = expr_to_z3 lhs in
          let r = expr_to_z3 rhs in
          let eq = Z3.Boolean.mk_eq ctx l r in
          Z3.Solver.add solver [eq]
      | _ -> ()
    ) stmts
;;

(* ----------------------------------------------------------- *)
(* Encode a module                                              *)
(* ----------------------------------------------------------- *)

let encode_module (m : sv_node) : Z3.Solver.solver =
  let solver = Z3.Solver.mk_simple_solver ctx in
  match m with
  | Module { name = _; stmts }
  | Netlist [Module { name = _; stmts }] ->
      add_constraints solver stmts;
      solver
  | _ -> failwith "expected a Module node"
;;

(* ----------------------------------------------------------- *)
(* Equivalence check between two modules                        *)
(* ----------------------------------------------------------- *)

(* Helper: extract bit width from a range string like "7:0" *)
let width_of_range = function
  | Some (ArrayType { range; _ }) -> (
      match String.split_on_char ':' range with
      | [hi; lo] ->
          let hi = int_of_string (String.trim hi)
          and lo = int_of_string (String.trim lo) in
          abs (hi - lo) + 1
      | _ -> 1)
  | _ -> 1

(* Helper: find all output variables in a module *)
let get_output_ports (m : sv_node) : (string * int) list =
  match m with
  | Module { stmts; _ }
  | Netlist [Module { name = _; stmts }] ->
      stmts
      |> List.filter_map (function
             | Var { name; dtype_ref; var_type; direction; _ }
               when var_type = "PORT" && direction = "OUTPUT" ->
                 Some (name, width_of_range dtype_ref)
             | _ -> None)
  | _ -> []

(* Main equivalence check for all outputs *)
let check_equiv_all_outputs mod1 mod2 =
  let outs1 = get_output_ports mod1 in
  let outs2 = get_output_ports mod2 in
  print_endline ("out ports: "^string_of_int (List.length outs1)^" : "^string_of_int (List.length outs1));
  (* sanity check: outputs should match *)
  let mismatch =
    List.filter (fun (n, _) -> not (List.mem_assoc n outs2)) outs1
  in
  if mismatch <> [] then
    Printf.printf "Warning: outputs missing in second module: %s\n"
      (String.concat ", " (List.map fst mismatch));

  let solver = Z3.Solver.mk_simple_solver ctx in

  (* encode both modules into solver constraints *)
  let s1 = encode_module mod1 in
  let s2 = encode_module mod2 in
  let all1 = Z3.Solver.get_assertions s1 in
  let all2 = Z3.Solver.get_assertions s2 in
  Z3.Solver.add solver (all1 @ all2);

  (* iterate over each output bit *)
  List.iter
    (fun (out_name, width) ->
      Printf.printf "Checking output %s (%d bits)\n%!" out_name width;
      for bit = 0 to width - 1 do
        let out1 =
          BitVector.mk_extract ctx bit bit (bv_var (out_name ^ "_1") width)
        in
        let out2 =
          BitVector.mk_extract ctx bit bit (bv_var (out_name ^ "_2") width)
        in
        let neq = Boolean.mk_not ctx (Boolean.mk_eq ctx out1 out2) in
        Solver.push solver;
        Solver.add solver [neq];
        match Solver.check solver [] with
        | Solver.SATISFIABLE ->
            Printf.printf "  ❌ Bit %d of %s differs!\n%!" bit out_name;
            (match Solver.get_model solver with Some model ->
                Printf.printf "  Model: %s\n%!" (Model.to_string model) | None -> ());
            Solver.pop solver 1
        | _ ->
            Solver.pop solver 1
      done)
    outs1
 
