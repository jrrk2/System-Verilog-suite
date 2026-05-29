(* Dump BIR for the cpu__cpu_state next-state after iflift+blocking_subst,
 * to see whether the trap-target leakage exists before hardcaml. *)
let () =
  Unix.putenv "MEMLOWER_FPGA" "1";
  let top = Sys.argv.(1) in
  let files = Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2)) in
  let prog = Verible_to_behavioral.convert_files ~top files in
  let flat = Behavioral_hier.flatten_for_z3 prog ~top in
  let prog = { Behavioral_ir.modules = [flat]; library_cells = prog.library_cells } in
  let prog =
    prog
    |> Behavioral_unroll.unroll_program
    |> Behavioral_inline.inline_program
    |> Behavioral_iflift.lift_program
    |> Behavioral_blocking_subst.blocking_subst_program
  in
  let m = List.find (fun (m : Behavioral_ir.bmodule) -> m.name = top) prog.modules in
  Printf.printf "module %s: %d signals, %d processes\n" m.name
    (List.length m.signals) (List.length m.processes);
  (* Find any BAssign whose LHS contains cpu__cpu_state or cpu__mem_do_rinst,
   * print the assignment's RHS pretty. *)
  let open Behavioral_ir in
  let interest =
    match Sys.getenv_opt "FSM_INTEREST" with
    | Some s -> String.split_on_char ',' s
    | None -> ["cpu__cpu_state"; "cpu__mem_do_rinst"; "cpu__trap"]
  in
  let rec walk_stmt path s = match s with
    | BAssign { lhs; rhs } when List.mem lhs interest ->
        Printf.printf "─── %s = ───\n%s\n" lhs (string_of_bexpr rhs);
        Printf.printf "(under: %s)\n\n" path
    | BAssign _ -> ()
    | BIf { condition; then_stmts; else_stmts } ->
        let cs = string_of_bexpr condition in
        List.iter (walk_stmt (path^" / if("^cs^")")) then_stmts;
        List.iter (walk_stmt (path^" / else of if("^cs^")")) else_stmts
    | BCase { selector; cases; default } ->
        let sel = string_of_bexpr selector in
        List.iter (fun (k, ss) ->
          let ks = string_of_bexpr k in
          List.iter (walk_stmt (path^" / case("^sel^") k="^ks)) ss
        ) cases;
        List.iter (walk_stmt (path^" / case("^sel^") default")) default
    | BBlock ss -> List.iter (walk_stmt path) ss
    | BWhile _ | BFor _ | BCallStmt _ | BReturn _ -> ()
  in
  List.iter (function
    | BSequential { name; body; _ } -> List.iter (walk_stmt ("seq "^name)) body
    | BCombinational { name; body; _ } -> List.iter (walk_stmt ("comb "^name)) body
  ) m.processes
