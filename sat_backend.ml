(* External SAT back end for the Z3 miter (same arrangement as fasm2netlist's
 * LVS: bit-blast the miter to DIMACS CNF, run a dedicated CDCL solver as a
 * subprocess).
 *
 * Z3 stays the front end: the miter is built as Z3 bit-vector / UF terms as
 * before.  Here a tactic pipeline reduces those assertions to propositional
 * CNF --
 *   simplify; propagate-values; solve-eqs; ackermannize_bv; simplify;
 *   bit-blast; tseitin-cnf
 * -- (ackermannize_bv turns the uninterpreted functions that abstract
 * already-proven children into fresh variables plus congruence constraints,
 * which is what lets the bit-blaster accept them), the clauses are written
 * as DIMACS and handed to `cadical`.
 *
 * Only the verdict comes back.  UNSAT = the designs are equivalent.  SAT =
 * they differ; the caller re-runs Z3 to get a model for the counterexample
 * report (finding a model is the cheap direction).  Anything unexpected (a
 * formula the converter does not recognise as a clause, a missing binary, a
 * solver crash) is `Unavailable` and the caller falls back to Z3, so the
 * back end can never turn an unknown into a verdict.
 *
 * Selection: SVS_SAT=cadical | kissat | z3 (default: cadical when a binary is
 * found, else z3).  SVS_CADICAL=<path> overrides the binary.  The time limit
 * is Z3_MITER_TIMEOUT_MS, as for Z3. *)

(* [Sat witness]: when the caller passed witness terms, the satisfying values
   for them (term = numeral equalities) -- Z3 can then produce the full
   counterexample model by evaluation instead of search. *)
type result = Unsat | Sat of Z3.Expr.expr list | Unknown of string | Unavailable of string

let home () = Option.value (Sys.getenv_opt "HOME") ~default:""

let find_binary name =
  let env = match name with "cadical" -> "SVS_CADICAL" | "kissat" -> "SVS_KISSAT" | _ -> "" in
  match Sys.getenv_opt env with
  | Some p when p <> "" && Sys.file_exists p -> Some p
  | _ ->
    let cands =
      [ home () ^ "/bytecode-work/cadical-src/build/" ^ name
      ; "/usr/local/bin/" ^ name; "/usr/bin/" ^ name ] in
    match List.find_opt Sys.file_exists cands with
    | Some p -> Some p
    | None ->
      if Sys.command (Printf.sprintf "command -v %s > /dev/null 2>&1" name) = 0
      then Some name else None

(* Which back end the miter uses for its final check. *)
let selected () : [ `Z3 | `Ext of string * string ] =
  match Sys.getenv_opt "SVS_SAT" with
  | Some "z3" -> `Z3
  | Some (("cadical" | "kissat") as s) ->
      (match find_binary s with Some b -> `Ext (s, b) | None -> `Z3)
  | _ ->
      (match find_binary "cadical" with Some b -> `Ext ("cadical", b) | None -> `Z3)

let describe () =
  match selected () with `Z3 -> "z3" | `Ext (n, b) -> Printf.sprintf "%s (%s)" n b

let timeout_s () =
  let ms = try int_of_string (Sys.getenv "Z3_MITER_TIMEOUT_MS") with _ -> 30000 in
  max 1 ((ms + 999) / 1000)

exception Not_clausal of string

(* Tseitin encoding of the bit-blasted Boolean formulas into DIMACS.  Every
   Boolean sub-term gets one variable (cached by AST id, so the DAG sharing
   Z3 built survives); top-level formulas are asserted as unit clauses, with
   top-level conjunctions and disjunctions flattened so the common case adds
   no auxiliary variables. *)
let to_dimacs ?(named : (string, int) Hashtbl.t option) ctx (formulas : Z3.Expr.expr list) : string * int * int =
  ignore ctx;
  let var_of : (int, int) Hashtbl.t = Hashtbl.create 65536 in
  let nvars = ref 0 in
  let fresh () = incr nvars; !nvars in
  let buf = Buffer.create (1 lsl 22) in
  let nclauses = ref 0 in
  let clause ls =
    incr nclauses;
    List.iter (fun x -> Buffer.add_string buf (string_of_int x); Buffer.add_char buf ' ') ls;
    Buffer.add_string buf "0\n" in
  let tvar = lazy (let t = fresh () in clause [ t ]; t) in   (* constant true *)
  let id e = Z3.AST.get_id (Z3.Expr.ast_of_expr e) in
  let args = Z3.Expr.get_args in
  let rec enc e : int =
    if Z3.Boolean.is_true e then Lazy.force tvar
    else if Z3.Boolean.is_false e then - (Lazy.force tvar)
    else if Z3.Boolean.is_not e then
      (match args e with [ x ] -> - (enc x) | _ -> raise (Not_clausal "not/arity"))
    else match Hashtbl.find_opt var_of (id e) with
    | Some v -> v
    | None ->
      let v =
        if Z3.Expr.is_const e && Z3.Boolean.is_bool e then begin
          let v = fresh () in
          (match named with
           | Some h -> Hashtbl.replace h (Z3.Symbol.to_string (Z3.FuncDecl.get_name (Z3.Expr.get_func_decl e))) v
           | None -> ());
          v
        end
        else if Z3.Boolean.is_and e then begin
          let xs = List.map enc (args e) in
          let v = fresh () in
          List.iter (fun x -> clause [ - v; x ]) xs;
          clause (v :: List.map (fun x -> - x) xs); v
        end else if Z3.Boolean.is_or e then begin
          let xs = List.map enc (args e) in
          let v = fresh () in
          List.iter (fun x -> clause [ v; - x ]) xs;
          clause (- v :: xs); v
        end else if Z3.Boolean.is_ite e then begin
          (match args e with
           | [ c; t; f ] when Z3.Boolean.is_bool t ->
               let c = enc c and t = enc t and f = enc f in
               let v = fresh () in
               clause [ - c; - t; v ]; clause [ - c; t; - v ];
               clause [ c; - f; v ]; clause [ c; f; - v ]; v
           | _ -> raise (Not_clausal "ite on non-Boolean"))
        end else if (Z3.Boolean.is_eq e || Z3.Boolean.is_iff e)
                && (match args e with [ a; _ ] -> Z3.Boolean.is_bool a | _ -> false) then begin
          (match args e with
           | [ a; b ] ->
               let a = enc a and b = enc b in
               let v = fresh () in
               clause [ - v; - a; b ]; clause [ - v; a; - b ];
               clause [ v; a; b ]; clause [ v; - a; - b ]; v
           | _ -> raise (Not_clausal "eq/arity"))
        end else if Z3.Boolean.is_xor e then begin
          (match List.map enc (args e) with
           | x :: rest ->
               List.fold_left (fun a b ->
                 let v = fresh () in
                 clause [ - v; a; b ]; clause [ - v; - a; - b ];
                 clause [ v; - a; b ]; clause [ v; a; - b ]; v) x rest
           | [] -> raise (Not_clausal "xor/arity"))
        end else
          raise (Not_clausal (let s = Z3.Expr.to_string e in
                              if String.length s > 80 then String.sub s 0 80 else s))
      in
      Hashtbl.replace var_of (id e) v; v
  in
  (* top level: a conjunction splits into separate assertions, a disjunction
     is a clause directly *)
  let rec assert_top e =
    if Z3.Boolean.is_true e then ()
    else if Z3.Boolean.is_and e then List.iter assert_top (args e)
    else if Z3.Boolean.is_or e then clause (List.map enc (args e))
    else clause [ enc e ] in
  List.iter assert_top formulas;
  (Printf.sprintf "p cnf %d %d\n" (max 1 !nvars) !nclauses ^ Buffer.contents buf, !nvars, !nclauses)

let bitblast ?(keep_vars = false) ctx (assertions : Z3.Expr.expr list) =
  let g = Z3.Goal.mk_goal ctx false false false in
  Z3.Goal.add g assertions;
  let t n = Z3.Tactic.mk_tactic ctx n in
  (* solve-eqs eliminates defined variables -- including the witness tags,
     so the witness pass leaves it out *)
  let pipeline =
    Z3.Tactic.and_then ctx (t "simplify") (t "propagate-values")
      ((if keep_vars then [] else [ t "solve-eqs" ])
       @ [ t "ackermannize_bv"; t "simplify"; t "bit-blast"; t "simplify" ]) in
  let r = Z3.Tactic.apply pipeline g None in
  Z3.Tactic.ApplyResult.get_subgoals r

(* Run the external solver on a CNF; returns the "s ..." line and the
   assignment (variable -> value) when it printed one. *)
let run_solver ~name ~bin dimacs =
  let cnf = Filename.temp_file "svs_miter_" ".cnf" in
  let oc = open_out cnf in output_string oc dimacs; close_out oc;
  let out = cnf ^ ".out" in
  let limit = timeout_s () in
  let cmd = match name with
    | "kissat" -> Printf.sprintf "%s -q --time=%d %s > %s 2>&1" (Filename.quote bin) limit (Filename.quote cnf) (Filename.quote out)
    | _ -> Printf.sprintf "%s -q -t %d %s > %s 2>&1" (Filename.quote bin) limit (Filename.quote cnf) (Filename.quote out) in
  let rc = Sys.command cmd in
  let assign : (int, bool) Hashtbl.t = Hashtbl.create 1024 in
  let status = ref None in
  let ic = open_in out in
  (try while true do
     let l = input_line ic in
     if String.length l >= 2 && String.sub l 0 2 = "s " then status := Some l
     else if String.length l >= 2 && String.sub l 0 2 = "v " then
       List.iter (fun tok -> match int_of_string_opt tok with
         | Some 0 | None -> ()
         | Some x -> Hashtbl.replace assign (abs x) (x > 0))
         (String.split_on_char ' ' (String.sub l 2 (String.length l - 2)))
   done with End_of_file -> ());
  close_in ic;
  (try Sys.remove cnf; Sys.remove out with _ -> ());
  (!status, assign, rc, limit)

(* Decide [assertions] (conjunction) with the external solver.  [witness]:
   bit-vector terms whose values to report when the result is SAT. *)
let check ?(witness : Z3.Expr.expr list = []) ctx (assertions : Z3.Expr.expr list) : result =
  match selected () with
  | `Z3 -> Unavailable "SVS_SAT=z3"
  | `Ext (name, bin) ->
    try
      let t0 = Unix.gettimeofday () in
      let goals = bitblast ctx assertions in
      let t1 = Unix.gettimeofday () in
      if List.exists Z3.Goal.is_decided_unsat goals && List.length goals = 1 then begin
        Printf.printf "[sat] %s: decided UNSAT during bit-blast (%.2fs)\n%!" name (t1 -. t0);
        Unsat
      end else begin
        let formulas = List.concat_map Z3.Goal.get_formulas goals in
        let dimacs, nv, nc = to_dimacs ctx formulas in
        let t2 = Unix.gettimeofday () in
        let status, _, rc, limit = run_solver ~name ~bin dimacs in
        let t3 = Unix.gettimeofday () in
        Printf.printf "[sat] %s: %d vars, %d clauses; bit-blast %.2fs, write %.2fs, solve %.2fs (rc %d)\n%!"
          name nv nc (t1 -. t0) (t2 -. t1) (t3 -. t2) rc;
        match status with
        | Some "s UNSATISFIABLE" -> Unsat
        | Some "s SATISFIABLE" ->
            (* witness pass: tag every bit of every witness term with a named
               Boolean, bit-blast again keeping variables, read the tags *)
            if witness = [] then Sat [] else begin
              let tags = List.mapi (fun k x ->
                let w = Z3.BitVector.get_size (Z3.Expr.get_sort x) in
                (x, w, List.init w (fun i ->
                   let nm = Printf.sprintf "__svs_w%d_%d" k i in
                   let b = Z3.Boolean.mk_const_s ctx nm in
                   let bit = Z3.BitVector.mk_extract ctx i i x in
                   (nm, Z3.Boolean.mk_eq ctx b
                          (Z3.Boolean.mk_eq ctx bit (Z3.BitVector.mk_numeral ctx "1" 1)))))) witness in
              let defs = List.concat_map (fun (_, _, l) -> List.map snd l) tags in
              let goals' = bitblast ~keep_vars:true ctx (assertions @ defs) in
              let named = Hashtbl.create 256 in
              let dimacs', _, _ = to_dimacs ~named ctx (List.concat_map Z3.Goal.get_formulas goals') in
              let st', assign, _, _ = run_solver ~name ~bin dimacs' in
              if st' <> Some "s SATISFIABLE" then Sat [] else
              Sat (List.filter_map (fun (x, w, l) ->
                let v = List.fold_left (fun (acc, i) (nm, _) ->
                  let bitv = match Hashtbl.find_opt named nm with
                    | Some var -> Option.value (Hashtbl.find_opt assign var) ~default:false
                    | None -> false in
                  ((if bitv then Z.logor acc (Z.shift_left Z.one i) else acc), i + 1)) (Z.zero, 0) l
                  |> fst in
                Some (Z3.Boolean.mk_eq ctx x (Z3.BitVector.mk_numeral ctx (Z.to_string v) w))) tags)
            end
        | Some s -> Unknown (Printf.sprintf "%s: %s" name s)
        | None -> Unknown (Printf.sprintf "%s: no result after %ds (rc %d)" name limit rc)
      end
    with
    | Not_clausal what -> Unavailable ("CNF conversion: unexpected formula " ^ what)
    | Z3.Error msg -> Unavailable ("tactic: " ^ msg)
    | Sys_error msg -> Unavailable msg
