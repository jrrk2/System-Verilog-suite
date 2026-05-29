(* Test what Z3 thinks about the multiplication case *)

let ctx = Z3.mk_context [("model", "true")]

let () =
  (* Create 8-bit variables *)
  let a = Z3.BitVector.mk_const_s ctx "a" 8 in
  let b = Z3.BitVector.mk_const_s ctx "b" 8 in
  let op = Z3.BitVector.mk_const_s ctx "op" 4 in
  
  (* Constrain to the failing case *)
  let solver = Z3.Solver.mk_simple_solver ctx in
  Z3.Solver.add solver [
    Z3.Boolean.mk_eq ctx a (Z3.BitVector.mk_numeral ctx "64" 8);   (* 0x40 *)
    Z3.Boolean.mk_eq ctx b (Z3.BitVector.mk_numeral ctx "2" 8);    (* 0x02 *)
    Z3.Boolean.mk_eq ctx op (Z3.BitVector.mk_numeral ctx "2" 4);   (* 0x2 *)
  ];
  
  (* Create the multiplication result *)
  let mul_result = Z3.BitVector.mk_mul ctx a b in
  
  Printf.printf "Checking: a=0x40, b=0x02, op=0x2\n";
  Printf.printf "Expected multiplication result: 0x80 (128)\n\n";
  
  (* Check what Z3 computes *)
  match Z3.Solver.check solver [] with
  | Z3.Solver.SATISFIABLE ->
      (match Z3.Solver.get_model solver with
       | Some model ->
           Printf.printf "Z3 model:\n";
           Printf.printf "  a = %s\n" 
             (match Z3.Model.eval model a true with Some v -> Z3.Expr.to_string v | None -> "?");
           Printf.printf "  b = %s\n"
             (match Z3.Model.eval model b true with Some v -> Z3.Expr.to_string v | None -> "?");
           Printf.printf "  a * b = %s\n"
             (match Z3.Model.eval model mul_result true with Some v -> Z3.Expr.to_string v | None -> "?");
       | None -> Printf.printf "No model\n")
  | _ -> Printf.printf "UNSAT or UNKNOWN\n"
