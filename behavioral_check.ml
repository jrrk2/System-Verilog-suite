(* IR invariants, checked right after every front end and on demand.
 *
 * The passes and the gate lowering assume a handful of things about a bmodule
 * that no importer was ever made to guarantee.  Each one of these was found
 * the expensive way (a `create_circuit lossage` or a wrong netlist, far from
 * the cause) on the cva6 modules:
 *
 *   - every name an expression reads is declared (signal, port, mem, function
 *     formal/local, or the function's own result variable)
 *   - every call names a module function with the right arity, or one of the
 *     `@`/`$` intrinsics
 *   - a write pseudo-op's first argument is the NAME of a declared signal
 *   - every declared type has a positive width; no duplicate declarations
 *   - a clocked process names a declared clock; its blocking_vars are assigned
 *     in its body
 *   - after unroll (stage "unrolled"): no BWhile/BFor remains
 *
 * Reported as `[check_ir] <module>: <what>` on stderr.  SVS_CHECK_IR=0 is
 * silent, SVS_CHECK_IR=strict fails the load on the first error, anything else
 * reports and continues.  Warnings (multiple drivers, out-of-range slices --
 * both have legitimate uses in this IR) print only with SVS_CHECK_IR=verbose
 * or strict. *)

open Behavioral_ir

type severity = Error | Warn

type issue = { severity : severity; where : string; what : string }

let intrinsic f =
  String.length f > 0 && (f.[0] = '@' || f.[0] = '$')

(* every bexpr inside a statement, with a callback *)
let rec iter_expr f = function
  | BVar _ | BConst _ as e -> f e
  | BBinOp { lhs; rhs; _ } as e -> f e; iter_expr f lhs; iter_expr f rhs
  | BUnOp { operand; _ } as e -> f e; iter_expr f operand
  | BSelect { array; index } as e -> f e; iter_expr f array; iter_expr f index
  | BSlice { signal; _ } as e -> f e; iter_expr f signal
  | BConcat l as e -> f e; List.iter (iter_expr f) l
  | BReplicate { value; _ } as e -> f e; iter_expr f value
  | BCond { condition; then_val; else_val } as e ->
      f e; iter_expr f condition; iter_expr f then_val; iter_expr f else_val
  | BCall { args; _ } as e -> f e; List.iter (iter_expr f) args

let rec iter_stmt ~expr ~stmt s =
  stmt s;
  match s with
  | BAssign { rhs; _ } -> iter_expr expr rhs
  | BIf { condition; then_stmts; else_stmts } ->
      iter_expr expr condition;
      List.iter (iter_stmt ~expr ~stmt) then_stmts;
      List.iter (iter_stmt ~expr ~stmt) else_stmts
  | BCase { selector; cases; default } ->
      iter_expr expr selector;
      List.iter (fun (k, ss) -> iter_expr expr k; List.iter (iter_stmt ~expr ~stmt) ss) cases;
      List.iter (iter_stmt ~expr ~stmt) default
  | BWhile { condition; body } ->
      iter_expr expr condition; List.iter (iter_stmt ~expr ~stmt) body
  | BFor { init; condition; update; body } ->
      iter_stmt ~expr ~stmt init; iter_expr expr condition;
      iter_stmt ~expr ~stmt update; List.iter (iter_stmt ~expr ~stmt) body
  | BBlock ss -> List.iter (iter_stmt ~expr ~stmt) ss
  | BCallStmt { func; args } ->
      (* the first argument of a write pseudo-op is a NAME, not a read *)
      let args = match args with
        | BVar _ :: rest when intrinsic func && func.[0] = '@' -> rest
        | _ -> args in
      List.iter (iter_expr expr) args
  | BReturn (Some e) -> iter_expr expr e
  | BReturn None -> ()

let rec width_ok = function
  | BInt { width; _ } -> width > 0
  | BBool -> true
  | BArray { element; size } -> size > 0 && width_ok element
  | BStruct { fields } -> List.for_all (fun (_, t) -> width_ok t) fields

let check_module ?(stage = "loaded") (prog : bprogram) (m : bmodule) : issue list =
  let issues = ref [] in
  let add severity what = issues := { severity; where = m.name; what } :: !issues in
  (* declarations *)
  let declared = Hashtbl.create 64 in
  let widths = Hashtbl.create 64 in
  let seen = Hashtbl.create 64 in
  List.iter (fun (s : bsignal) ->
    if Hashtbl.mem seen s.name then add Error (Printf.sprintf "signal %s declared twice" s.name);
    Hashtbl.replace seen s.name ();
    Hashtbl.replace declared s.name ();
    Hashtbl.replace widths s.name (width_of_btype s.stype);
    if not (width_ok s.stype) then
      add Error (Printf.sprintf "signal %s has a non-positive width" s.name)) m.signals;
  List.iter (fun (n, _) -> Hashtbl.replace declared n ()) m.params;
  List.iter (fun (mm : bmem) -> Hashtbl.replace declared mm.mname ()) m.mems;
  Hashtbl.replace declared "<const0>" (); Hashtbl.replace declared "<const1>" ();
  let funcs = Hashtbl.create 16 in
  List.iter (fun (f : bfunc) -> Hashtbl.replace funcs f.fname (List.length f.params)) m.funcs;
  (* expression-level checks, with an extra scope for function bodies *)
  let check_exprs ~scope ~ctx stmts =
    (* a for-loop's induction variable is declared by its init (`for (int i`)
       and lives only until unroll substitutes it *)
    let loop_vars = ref [] in
    List.iter (iter_stmt ~expr:(fun _ -> ()) ~stmt:(function
      | BFor { init = BAssign { lhs; _ }; _ } -> loop_vars := lhs :: !loop_vars
      | _ -> ())) stmts;
    let scope = !loop_vars @ scope in
    let is_declared n = Hashtbl.mem declared n || List.mem n scope in
    let expr = function
      | BVar n when not (is_declared n) ->
          add Error (Printf.sprintf "%s reads undeclared name %s" ctx n)
      | BCall { func; args } when not (intrinsic func) ->
          (match Hashtbl.find_opt funcs func with
           | None -> add Error (Printf.sprintf "%s calls unknown function %s/%d" ctx func (List.length args))
           | Some n when n <> List.length args ->
               add Error (Printf.sprintf "%s calls %s with %d args, declared %d" ctx func (List.length args) n)
           | Some _ -> ())
      | BSlice { signal = BVar n; msb; lsb } ->
          (match Hashtbl.find_opt widths n with
           | Some w when msb >= w || lsb < 0 ->
               add Warn (Printf.sprintf "%s slices %s[%d:%d] beyond its %d bits" ctx n msb lsb w)
           | _ -> ())
      | _ -> () in
    let stmt = function
      | BAssign { lhs; _ } when not (is_declared lhs) ->
          add Error (Printf.sprintf "%s assigns undeclared name %s" ctx lhs)
      | BCallStmt { func; args } when intrinsic func && func.[0] = '@' ->
          (match args with
           | BVar n :: _ when not (is_declared n) ->
               add Error (Printf.sprintf "%s: %s targets undeclared name %s" ctx func n)
           | BVar _ :: _ -> ()
           | _ -> add Error (Printf.sprintf "%s: %s target is not a name" ctx func))
      | BCallStmt { func; args } when not (intrinsic func) ->
          (match Hashtbl.find_opt funcs func with
           | None -> add Error (Printf.sprintf "%s calls unknown task %s/%d" ctx func (List.length args))
           | Some _ -> ())
      | (BWhile _ | BFor _) when stage = "unrolled" ->
          add Error (Printf.sprintf "%s still has a loop after unroll" ctx)
      | _ -> () in
    List.iter (iter_stmt ~expr ~stmt) stmts in
  (* processes *)
  let drivers = Hashtbl.create 64 in
  List.iter (fun p ->
    let name, body = match p with
      | BCombinational { name; body; _ } -> name, body
      | BSequential { name; body; _ } -> name, body in
    let ctx = "process " ^ name in
    check_exprs ~scope:[] ~ctx body;
    (match p with
     | BSequential { clock; blocking_vars; reset; reset_async; body; _ } ->
         if not (Hashtbl.mem declared clock) then
           add Error (Printf.sprintf "%s clocks on undeclared %s" ctx clock);
         (* the header's reset name and the body's outer reset test are two
            readings of one process; they must agree *)
         (match reset with
          | Some r when reset_async ->
              let rec outer = function
                | [] -> None
                | BBlock xs :: _ -> outer xs
                | BIf { condition; _ } :: _ -> Some condition
                | _ :: tl -> outer tl in
              let mentions = ref false in
              (match outer body with
               | Some c -> iter_expr (function BVar n when n = r -> mentions := true | _ -> ()) c
               | None -> mentions := true);
              if not !mentions then
                add Error (Printf.sprintf "%s: async reset %s is not tested by the body's outer if" ctx r)
          | _ -> ());
         let assigned = Hashtbl.create 16 in
         List.iter (iter_stmt ~expr:(fun _ -> ()) ~stmt:(function
           | BAssign { lhs; _ } -> Hashtbl.replace assigned lhs ()
           | BCallStmt { func; args = BVar n :: _ } when intrinsic func -> Hashtbl.replace assigned n ()
           | _ -> ())) body;
         List.iter (fun v -> if not (Hashtbl.mem assigned v) then
           add Warn (Printf.sprintf "%s lists blocking var %s it never assigns" ctx v)) blocking_vars
     | _ -> ());
    (* who drives what *)
    List.iter (iter_stmt ~expr:(fun _ -> ()) ~stmt:(function
      | BAssign { lhs; _ } ->
          let cur = try Hashtbl.find drivers lhs with Not_found -> [] in
          if not (List.mem name cur) then Hashtbl.replace drivers lhs (name :: cur)
      | BCallStmt { func; args = BVar n :: _ } when intrinsic func && func.[0] = '@' ->
          let cur = try Hashtbl.find drivers n with Not_found -> [] in
          if not (List.mem name cur) then Hashtbl.replace drivers n (name :: cur)
      | _ -> ())) body) m.processes;
  Hashtbl.iter (fun n ps ->
    if List.length ps > 1 then
      add Warn (Printf.sprintf "%s driven by %d processes (%s)" n (List.length ps) (String.concat ", " ps)))
    drivers;
  (* functions: formals, locals and the result variable are in scope *)
  List.iter (fun (f : bfunc) ->
    let scope = f.fname :: List.map (fun (n, _, _) -> n) f.params @ List.map fst f.locals in
    check_exprs ~scope ~ctx:("function " ^ f.fname) f.body) m.funcs;
  (* instances: port expressions read declared names; the child exists or is a
     library cell / primitive (reported as a warning: primitives are expected) *)
  let known_module n =
    List.exists (fun (mm : bmodule) -> mm.name = n) prog.modules
    || List.exists (fun (c, _) -> c = n) prog.library_cells in
  List.iter (fun (i : binstance) ->
    List.iter (fun (_, e) -> iter_expr (function
      | BVar n when not (Hashtbl.mem declared n) ->
          add Error (Printf.sprintf "instance %s connects undeclared name %s" i.inst_name n)
      | _ -> ()) e) i.port_connections;
    if not (known_module i.module_name) then
      add Warn (Printf.sprintf "instance %s of unresolved module %s" i.inst_name i.module_name))
    m.instances;
  List.iter (fun (s : bsignal) -> match s.initial_value with
    | Some e -> iter_expr (function
        | BVar n when not (Hashtbl.mem declared n) ->
            add Error (Printf.sprintf "initial value of %s reads undeclared name %s" s.name n)
        | _ -> ()) e
    | None -> ()) m.signals;
  List.rev !issues

let check_program ?stage (p : bprogram) =
  List.concat_map (check_module ?stage p) p.modules

let string_of_issue i =
  Printf.sprintf "[check_ir] %s%s: %s"
    (match i.severity with Error -> "" | Warn -> "warning ") i.where i.what

(* The post-load hook.  Returns the program unchanged; prints / fails per
   SVS_CHECK_IR. *)
let report ?(tag = "") ?stage (p : bprogram) =
  let mode = Option.value (Sys.getenv_opt "SVS_CHECK_IR") ~default:"" in
  if mode = "0" then p
  else begin
    let issues = check_program ?stage p in
    let verbose = mode = "verbose" || mode = "strict" in
    let errors = List.filter (fun i -> i.severity = Error) issues in
    let shown = if verbose then issues else errors in
    List.iter (fun i -> prerr_endline (string_of_issue i)) shown;
    if errors <> [] then
      Printf.eprintf "[check_ir] %s%d error(s), %d warning(s) in %d module(s)%s\n%!"
        (if tag = "" then "" else tag ^ ": ")
        (List.length errors) (List.length issues - List.length errors)
        (List.length p.modules)
        (if verbose then "" else " (SVS_CHECK_IR=verbose for warnings)");
    if mode = "strict" && errors <> [] then
      failwith (Printf.sprintf "check_ir: %d invariant violation(s) (SVS_CHECK_IR=strict)" (List.length errors));
    p
  end

let summary ?stage (p : bprogram) =
  let issues = check_program ?stage p in
  let errors = List.length (List.filter (fun i -> i.severity = Error) issues) in
  String.concat "\n"
    (List.map string_of_issue issues
     @ [Printf.sprintf "CHECK-IR: %d error(s), %d warning(s)" errors (List.length issues - errors)])
