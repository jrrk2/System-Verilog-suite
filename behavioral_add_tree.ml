(* Add-chain rebalancing (structuring at the BIR level).
 *
 * A sum written as a chain of single-use wires --
 *
 *   s1 = e00 + e01;  s2 = s1 + e02;  ...  sum = s7 + e22;
 *
 * (cascade_mac's adder cascade, or the same thing written inside one
 * expression as ((a + b) + c) + d) -- lowers to N-1 carry chains in series:
 * logic depth grows linearly with the operand count.  Collecting the leaves of
 * every same-width add tree whose interior wires have no other reader and
 * rebuilding it as a balanced binary tree gives ceil(log2 N) adder levels for
 * the same N-1 adders.  Leaves are paired by alignment (known trailing zeros,
 * i.e. how far a zero-padded partial product is shifted up) so each adder
 * stays as narrow as the CARRY4 trimming in Bir_to_aig can make it.
 *
 * Exact: addition mod 2^W is associative and commutative, and every leaf
 * already sat in a W-bit add context.  The absorbed interior wires disappear
 * (their only reader was the next add).  GATE_MAP_BIR_NOADDTREE=1 skips. *)

open Behavioral_ir

let width_of_add = function
  | BBinOp { op = BAdd; result_type = BInt { width; _ }; _ } -> Some width
  | _ -> None

let rec uses_expr tbl = function
  | BVar v -> Hashtbl.replace tbl v (1 + (try Hashtbl.find tbl v with Not_found -> 0))
  | BConst _ -> ()
  | BBinOp { lhs; rhs; _ } -> uses_expr tbl lhs; uses_expr tbl rhs
  | BUnOp { operand; _ } -> uses_expr tbl operand
  | BSelect { array; index } -> uses_expr tbl array; uses_expr tbl index
  | BSlice { signal; _ } -> uses_expr tbl signal
  | BConcat l -> List.iter (uses_expr tbl) l
  | BReplicate { value; _ } -> uses_expr tbl value
  | BCond { condition; then_val; else_val } ->
      uses_expr tbl condition; uses_expr tbl then_val; uses_expr tbl else_val
  | BCall { args; _ } -> List.iter (uses_expr tbl) args

let rec uses_stmt tbl = function
  | BAssign { rhs; _ } -> uses_expr tbl rhs
  | BIf { condition; then_stmts; else_stmts } ->
      uses_expr tbl condition; List.iter (uses_stmt tbl) then_stmts;
      List.iter (uses_stmt tbl) else_stmts
  | BCase { selector; cases; default } ->
      uses_expr tbl selector;
      List.iter (fun (k, b) -> uses_expr tbl k; List.iter (uses_stmt tbl) b) cases;
      List.iter (uses_stmt tbl) default
  | BWhile { condition; body } -> uses_expr tbl condition; List.iter (uses_stmt tbl) body
  | BFor { init; condition; update; body } ->
      uses_stmt tbl init; uses_expr tbl condition; uses_stmt tbl update;
      List.iter (uses_stmt tbl) body
  | BBlock l -> List.iter (uses_stmt tbl) l
  | BCallStmt { args; _ } -> List.iter (uses_expr tbl) args
  | BReturn e -> Option.iter (uses_expr tbl) e

let rebalance_module (m : bmodule) : bmodule * int =
  let widths = Behavioral_const.signal_widths_of_module m in
  let uses = Hashtbl.create 64 in
  List.iter (function
    | BCombinational { body; _ } -> List.iter (uses_stmt uses) body
    | BSequential { body; clock; reset; _ } ->
        List.iter (uses_stmt uses) body;
        uses_expr uses (BVar clock); Option.iter (fun r -> uses_expr uses (BVar r)) reset)
    m.processes;
  List.iter (fun (i : binstance) -> List.iter (fun (_, e) -> uses_expr uses e) i.port_connections)
    m.instances;
  let internal_unsigned = Hashtbl.create 64 in
  List.iter (fun (s : bsignal) ->
    match s.direction, s.stype with
    | `Internal, BInt { signed = Unsigned; _ } -> Hashtbl.replace internal_unsigned s.name ()
    | _ -> ()) m.signals;
  (* single-statement combinational definitions; a signal written anywhere
     else is not a candidate *)
  let writes = Hashtbl.create 64 in
  let rec count_writes = function
    | BAssign { lhs; _ } -> Hashtbl.replace writes lhs (1 + (try Hashtbl.find writes lhs with Not_found -> 0))
    | BIf { then_stmts; else_stmts; _ } -> List.iter count_writes then_stmts; List.iter count_writes else_stmts
    | BCase { cases; default; _ } -> List.iter (fun (_, b) -> List.iter count_writes b) cases; List.iter count_writes default
    | BWhile { body; _ } | BFor { body; _ } -> List.iter count_writes body
    | BBlock l -> List.iter count_writes l
    | _ -> () in
  List.iter (function BCombinational { body; _ } | BSequential { body; _ } -> List.iter count_writes body)
    m.processes;
  let defs = Hashtbl.create 64 in
  List.iter (function
    | BCombinational { name; body = [ BAssign { lhs; rhs } ]; _ }
      when Hashtbl.find_opt writes lhs = Some 1 -> Hashtbl.replace defs lhs (name, rhs)
    | _ -> ()) m.processes;
  let absorbable w v =
    Hashtbl.mem defs v && Hashtbl.mem internal_unsigned v
    && Hashtbl.find_opt uses v = Some 1 && Hashtbl.find_opt widths v = Some w in
  (* does v (through copies) define a w-bit add? *)
  let rec is_add_def w depth v =
    depth < 64 && absorbable w v &&
    (match snd (Hashtbl.find defs v) with
     | BVar u -> is_add_def w (depth + 1) u
     | e -> width_of_add e = Some w) in
  (* leaves of the w-bit add tree rooted at e; [absorbed] collects the wires *)
  let rec leaves w absorbed e =
    match e with
    | BBinOp { op = BAdd; lhs; rhs; result_type = BInt { width; _ } } when width = w ->
        leaves w absorbed lhs @ leaves w absorbed rhs
    | BVar v when is_add_def w 0 v ->
        absorbed := v :: !absorbed;
        leaves w absorbed (snd (Hashtbl.find defs v))
    | e -> [ e ] in
  (* chain depth of the original tree (adder levels) *)
  let rec depth w e =
    match e with
    | BBinOp { op = BAdd; lhs; rhs; result_type = BInt { width; _ } } when width = w ->
        1 + max (depth w lhs) (depth w rhs)
    | BVar v when is_add_def w 0 v -> depth w (snd (Hashtbl.find defs v))
    | _ -> 0 in
  (* known trailing zero bits: alignment key for pairing *)
  let rec tz fuel e =
    if fuel = 0 then 0 else
    match e with
    | BConst { value; width } ->
        if Z.equal value Z.zero then width else min width (Z.trailing_zeros value)
    | BConcat l ->
        let rec go = function
          | [] -> 0
          | x :: rest ->
              let t = tz (fuel - 1) x in
              (match Behavioral_const.width_of_expr_with widths x with
               | Some xw when t >= xw -> xw + go rest
               | _ -> t) in
        go (List.rev l)
    | BVar v -> (match Hashtbl.find_opt defs v with
                 | Some (_, rhs) -> tz (fuel - 1) rhs | None -> 0)
    | _ -> 0 in
  let rec log2up n = if n <= 1 then 0 else 1 + log2up ((n + 1) / 2) in
  let rebuilt = Hashtbl.create 8 and absorbed_all = Hashtbl.create 32 in
  let n_trees = ref 0 in
  List.iter (function
    | BCombinational { body = [ BAssign { lhs; rhs } ]; _ } when Hashtbl.mem defs lhs ->
        (match width_of_add rhs with
         | Some w when w > 1 ->
             let absorbed = ref [] in
             let ls = leaves w absorbed rhs in
             let n = List.length ls in
             if n >= 3 && depth w rhs > log2up n then begin
               let keyed = List.stable_sort (fun (a, _) (b, _) -> compare a b)
                   (List.map (fun e -> (tz 8 e, e)) ls) in
               let rec level = function
                 | a :: b :: rest ->
                     BBinOp { op = BAdd; lhs = a; rhs = b
                            ; result_type = BInt { width = w; signed = Unsigned } }
                     :: level rest
                 | l -> l in
               let rec build = function
                 | [ e ] -> e
                 | l -> build (level l) in
               Hashtbl.replace rebuilt lhs (build (List.map snd keyed), !absorbed)
             end
         | _ -> ())
    | _ -> ()) m.processes;
  (* a tree absorbed into a larger one is not rebuilt on its own *)
  Hashtbl.iter (fun _ (_, ab) -> List.iter (fun v -> Hashtbl.replace absorbed_all v ()) ab) rebuilt;
  let processes =
    List.filter_map (fun p ->
      match p with
      | BCombinational ({ body = [ BAssign { lhs; _ } ]; _ } as r) ->
          if Hashtbl.mem absorbed_all lhs then None
          else (match Hashtbl.find_opt rebuilt lhs with
                | Some (e, _) -> incr n_trees; Some (BCombinational { r with body = [ BAssign { lhs; rhs = e } ] })
                | None -> Some p)
      | p -> Some p) m.processes in
  if !n_trees = 0 then m, 0
  else
    { m with processes
           ; signals = List.filter (fun (s : bsignal) -> not (Hashtbl.mem absorbed_all s.name)) m.signals },
    !n_trees

let rebalance_program (p : bprogram) : bprogram =
  { p with modules =
      List.map (fun m ->
        let m', n = rebalance_module m in
        if n > 0 then Printf.eprintf "[add_tree] %s: %d add chain(s) rebalanced\n%!" m.name n;
        m') p.modules }
