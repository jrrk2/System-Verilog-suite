(* Operator sharing by common-operand hoisting (structuring at the BIR level).
 *
 * A register updated in many mutually exclusive arms --
 *
 *   case (state) A: sp <= sp + 1;  B: sp <= sp - 3;  C: sp <= sp + imm; ...
 *
 * -- costs one adder per arm plus the output mux.  Threading the pending
 * value (Behavioral_nba_slice.thread) gives one next-state tree
 *   sp <= s==A ? sp + 1 : s==B ? sp - 3 : ... : sp
 * whose add/sub leaves all share the operand sp; hoisting it gives
 *   sp <= sp + (s==A ? 1 : s==B ? -3 : ... : 0)
 * i.e. ONE adder fed by a mux of the other operands (a subtraction of a
 * constant becomes an addition of its two's complement; a hold leaf is + 0).
 * The operand mux is a LUT per bit where the adders it replaces were a LUT
 * plus carry per bit each -- yosys gets the same effect from `share` /
 * `opt_share`.  Only leaves that are `X + e`, `X - const`, or `X` itself
 * (with the same X, textually) are hoisted; anything else stays a mux leaf.
 * The rewrite is exact (pure algebra on the threaded tree), so the
 * hierarchical netlist miter verifies it against the untouched source. *)

open Behavioral_ir

let key = string_of_bexpr

let bint w = BInt { width = w; signed = Unsigned }

(* zero-extend / truncate e to w bits (needed so the operand mux is uniform) *)
let fit ~widths w e =
  match Behavioral_const.width_of_expr_with widths e with
  | Some ew when ew = w -> e
  | Some ew when ew < w -> BConcat [ bconst_int 0 (w - ew); e ]
  | Some _ -> BSlice { signal = e; msb = w - 1; lsb = 0 }
  | None -> e

(* classify a leaf against the shared operand x: Some other-operand *)
let leaf_of ?(hold_ok = false) ~widths ~w ~x e =
  match e with
  | e when hold_ok && key e = x -> Some (bconst_int 0 w)
  | BBinOp { op = BAdd; lhs; rhs; result_type = BInt { width; _ } } when width = w ->
      if key lhs = x then Some (fit ~widths w rhs)
      else if key rhs = x then Some (fit ~widths w lhs)
      else None
  | BBinOp { op = BSub; lhs; rhs = BConst { value; _ }; result_type = BInt { width; _ } }
    when width = w && key lhs = x ->
      let m = Z.shift_left Z.one w in
      Some (BConst { value = Z.erem (Z.sub m value) m; width = w })
  (* a bare `x` (register hold) is deliberately NOT a `+ 0` leaf: keeping it
     as a mux leaf lets the lowering lift the enable into the FF's CE pin
     (uart_receiver's counter: 64 -> 72 LUTs when it was absorbed) *)
  | _ -> None

(* count add/sub leaves with operand x in a mux tree *)
let rec count ~widths ~w ~x = function
  | BCond { then_val; else_val; _ } -> count ~widths ~w ~x then_val + count ~widths ~w ~x else_val
  | e -> (match leaf_of ~widths ~w ~x e with
          | Some _ when key e <> x -> 1
          | _ -> 0)

(* hoist: returns Some operand-mux when EVERY leaf of the tree is x-shaped *)
let rec hoist ?(hold_ok = false) ~widths ~w ~x e =
  match e with
  | BCond { condition; then_val; else_val } ->
      (match hoist ~hold_ok ~widths ~w ~x then_val, hoist ~hold_ok ~widths ~w ~x else_val with
       | Some a, Some b -> Some (BCond { condition; then_val = a; else_val = b })
       | _ -> None)
  | e -> leaf_of ~hold_ok ~widths ~w ~x e

(* partial hoist: sub-trees that are fully x-shaped collapse to x + mux; mixed
   trees keep the outer mux (so `s==A ? sp+1 : s==B ? io_data : sp-3` keeps
   one leaf for io_data and shares the rest) *)
(* hold leaves (`x` itself) are absorbed as `+ 0` only when the tree has at
   least three real add/sub leaves: on a 2-leaf up/down counter the absorbed
   hold defeats clock-enable lifting (uart_receiver 64 -> 72 LUTs), on a
   20-leaf state machine it is what lets the whole tree collapse to one adder
   (picorv32 1377 -> 1312, the OCaml VM 18604 -> 17760). *)
let rec all_const_leaves ~widths ~w ~x = function
  | BCond { then_val; else_val; _ } ->
      all_const_leaves ~widths ~w ~x then_val && all_const_leaves ~widths ~w ~x else_val
  | e -> (match leaf_of ~widths ~w ~x e with
          | Some (BConst _) -> true
          | Some _ -> false
          | None -> key e = x)

(* does the tree have a bare-x (hold) leaf? *)
let rec has_hold ~x = function
  | BCond { then_val; else_val; _ } -> has_hold ~x then_val || has_hold ~x else_val
  | e -> key e = x

(* returns the rewritten tree and the number of adders it removed.  [reg] is
   the register being updated: a hold of x <> reg is a mux leaf that no clock
   enable can absorb, so even a single add leaf is worth hoisting there --
   `c ? X : X + y` -> `X + (c ? 0 : y)` folds the operand gating into the
   adder's own per-bit LUT and drops the output mux (cascade_mac's
   accumulator: 48 LUT3 muxes gone). *)
let rec rewrite ~reg ~widths ~w ~x e =
  let n = count ~widths ~w ~x e in
  (* a bare up/down pair (`c ? x + 1 : x - 1`, both constants) is two
     half-adder chains already; merging them into one full adder + operand
     mux measured 3 LUTs worse on a 5-bit counter, so leave n = 2 all-constant
     trees alone.  Everything else: absorb the hold leaf as + 0 too, which is
     what lets a many-arm state machine collapse to one adder (picorv32
     1377 -> 1312 LUTs, the OCaml VM 18604 -> 17760). *)
  let skip = n = 2 && all_const_leaves ~widths ~w ~x e in
  let gated_single =
    n = 1 && x <> key (BVar reg) && has_hold ~x e
    && not (all_const_leaves ~widths ~w ~x e) in
  match (if skip then None else hoist ~hold_ok:true ~widths ~w ~x e) with
  | Some m when n >= 2 || gated_single ->
      BBinOp { op = BAdd; lhs = BVar x_var_placeholder; rhs = m; result_type = bint w },
      max 1 (n - 1)
  | _ ->
    (match e with
     | BCond { condition; then_val; else_val } ->
         let t, a = rewrite ~reg ~widths ~w ~x then_val in
         let f, b = rewrite ~reg ~widths ~w ~x else_val in
         BCond { condition; then_val = t; else_val = f }, a + b
     | e -> e, 0)
and x_var_placeholder = "__share_x__"

(* the shared operand expression itself, recovered from the first leaf *)
let rec find_x_expr ~x = function
  | BCond { then_val; else_val; _ } ->
      (match find_x_expr ~x then_val with Some e -> Some e | None -> find_x_expr ~x else_val)
  | BBinOp { lhs; rhs; _ } as e ->
      if key lhs = x then Some lhs else if key rhs = x then Some rhs
      else if key e = x then Some e else None
  | e -> if key e = x then Some e else None

let rec subst_placeholder xe = function
  | BVar n when n = x_var_placeholder -> xe
  | BBinOp r -> BBinOp { r with lhs = subst_placeholder xe r.lhs; rhs = subst_placeholder xe r.rhs }
  | BCond r -> BCond { condition = subst_placeholder xe r.condition
                     ; then_val = subst_placeholder xe r.then_val
                     ; else_val = subst_placeholder xe r.else_val }
  | e -> e

(* candidate (x, w) pairs for a register's tree: operands of add/sub leaves,
   most frequent first *)
let candidates ~widths tree =
  let tbl = Hashtbl.create 8 in
  let rec go = function
    | BCond { then_val; else_val; _ } -> go then_val; go else_val
    | BBinOp { op = (BAdd | BSub); lhs; rhs; result_type = BInt { width; _ } } ->
        List.iter (fun o ->
          match o with
          | BConst _ -> ()
          | o -> let k = (key o, width) in
                 Hashtbl.replace tbl k (1 + (try Hashtbl.find tbl k with Not_found -> 0)))
          [ lhs; rhs ]
    | _ -> () in
  go tree;
  Hashtbl.fold (fun (x, w) n acc -> (x, w, n) :: acc) tbl []
  |> List.sort (fun (_, _, a) (_, _, b) -> compare b a)
  |> List.map (fun (x, w, _) -> (x, w))

(* Group sharing over a whole next-state tree (GATE_MAP_BIR_NOGROUP=1 skips).
 * [rewrite] above only collapses sub-trees whose every leaf has the shape
 * `x + e`; in a big state machine the families are interleaved --
 * temp_heap_addr's leaves are `(accu[19:2] + 1) + K` for ten K and
 * `(env[19:2] + 1) + K` for six more, between holds and plain copies -- so
 * no sub-tree is uniform.  Here every add/sub leaf is normalised to
 * `X + o (+ cin)` at the tree width w (an add whose result is at least w
 * bits, or a [w-1:0] slice of one, is exact mod 2^w on its operands cut to
 * w bits; `X - e` is `X + ~e + 1`, `X - K` is `X + (-K)`), leaves are grouped
 * by X, and each group with two or more distinct leaves becomes ONE adder
 *   S = X + M (+ C)
 * where M (C) is the tree itself with every branch holding no group leaf
 * collapsed away and each group leaf replaced by its operand (carry-in).
 * Whenever the tree selects a group leaf, M takes the same path, so S equals
 * that leaf: replacing every group leaf by S is exact.  The copies of S are
 * identical, so the AIG hashes them (and the CARRY4 CSE their chains) into
 * one adder, and the AND-OR mux lowering sees one leaf value. *)
type gleaf = { gx : string; gxe : bexpr; gop : bexpr; gcin : bool }

let group_leaf ~widths ~w e : gleaf list =
  let cut e = fit ~widths w e in
  let wide_enough = function
    | BInt { width; _ } -> width >= w
    | _ -> false in
  let add_like e =
    match e with
    | BBinOp { op = (BAdd | BSub) as op; lhs; rhs; result_type } when wide_enough result_type ->
        Some (op, lhs, rhs)
    | BSlice { signal = BBinOp { op = (BAdd | BSub) as op; lhs; rhs; result_type = BInt _ }
             ; msb; lsb = 0 } when msb + 1 = w ->
        Some (op, lhs, rhs)
    | _ -> None in
  let not_const = function BConst _ -> false | _ -> true in
  match add_like e with
  | Some (BAdd, l, r) ->
      List.filter_map (fun (x, o) ->
        if not_const x then
          Some { gx = key (cut x); gxe = cut x; gop = cut o; gcin = false }
        else None) [ (l, r); (r, l) ]
  | Some (_, l, BConst { value; _ }) when not_const l ->
      let m = Z.shift_left Z.one w in
      [ { gx = key (cut l); gxe = cut l
        ; gop = BConst { value = Z.erem (Z.sub m value) m; width = w }; gcin = false } ]
  | Some (_, l, r) when not_const l ->
      [ { gx = key (cut l); gxe = cut l
        ; gop = BUnOp { op = BNot; operand = cut r; result_type = bint w }; gcin = true } ]
  | _ -> []

let group_share ~widths ~w tree : bexpr * int =
  (* every leaf (by key) with its candidate decompositions *)
  let leaves = Hashtbl.create 64 in
  let rec collect = function
    | BCond { then_val; else_val; _ } -> collect then_val; collect else_val
    | e ->
        let k = key e in
        if not (Hashtbl.mem leaves k) then Hashtbl.replace leaves k (group_leaf ~widths ~w e) in
  collect tree;
  (* greedy: each leaf joins the X shared by most distinct leaves *)
  let freq = Hashtbl.create 32 in
  Hashtbl.iter (fun _ ds ->
    List.iter (fun d -> Hashtbl.replace freq d.gx (1 + Option.value ~default:0 (Hashtbl.find_opt freq d.gx)))
      (List.sort_uniq (fun a b -> compare a.gx b.gx) ds)) leaves;
  let assign = Hashtbl.create 64 in        (* leaf key -> chosen decomposition *)
  Hashtbl.iter (fun k ds ->
    match List.sort (fun a b -> compare (Hashtbl.find freq b.gx) (Hashtbl.find freq a.gx)) ds with
    | d :: _ when Hashtbl.find freq d.gx >= 2 -> Hashtbl.replace assign k d
    | _ -> ()) leaves;
  let groups = Hashtbl.create 8 in
  Hashtbl.iter (fun k d ->
    Hashtbl.replace groups d.gx (k :: Option.value ~default:[] (Hashtbl.find_opt groups d.gx))) assign;
  let gain = ref 0 and tree = ref tree in
  Hashtbl.iter (fun gx members ->
    if List.length members >= 2 then begin
      let in_g e = match Hashtbl.find_opt assign (key e) with
        | Some d -> d.gx = gx | None -> false in
      let rec has_g = function
        | BCond { then_val; else_val; _ } -> has_g then_val || has_g else_val
        | e -> in_g e in
      (* the tree restricted to the group's branches, leaves mapped by f *)
      let rec restrict f = function
        | BCond { condition; then_val; else_val } ->
            (match has_g then_val, has_g else_val with
             | true, true -> BCond { condition; then_val = restrict f then_val
                                   ; else_val = restrict f else_val }
             | true, false -> restrict f then_val
             | _ -> restrict f else_val)
        | e -> f (Hashtbl.find assign (key e)) in
      let t = !tree in
      if has_g t then begin
        let xe = (Hashtbl.find assign (List.hd members)).gxe in
        let m = restrict (fun d -> d.gop) t in
        let any_cin = Hashtbl.fold (fun k d a -> a || (d.gx = gx && d.gcin && List.mem k members)) assign false in
        let sum = BBinOp { op = BAdd; lhs = xe; rhs = m; result_type = bint w } in
        let s =
          if any_cin then
            BBinOp { op = BAdd; lhs = sum
                   ; rhs = restrict (fun d -> bconst_int (if d.gcin then 1 else 0) 1) t
                   ; result_type = bint w }
          else sum in
        let rec repl = function
          | BCond r -> BCond { r with then_val = repl r.then_val; else_val = repl r.else_val }
          | e -> if in_g e then s else e in
        tree := repl t;
        gain := !gain + List.length members - 1
      end
    end) groups;
  !tree, !gain

let group_off = Sys.getenv_opt "GATE_MAP_BIR_NOGROUP" = Some "1"

(* Does any statement other than [r]'s own writes read [r]?  A register
   assigned with blocking `=` can only have its writes gathered at the end
   of the body when nothing else in the body reads it (a later read would
   see the moved write's value disappear). *)
let read_elsewhere r stmts =
  let m e = Behavioral_nba_slice.mentions r e in
  let rec st = function
    | BAssign { lhs; rhs } -> lhs <> r && m rhs
    | BCallStmt { args = BVar t :: rest; _ } when t = r -> ignore rest; false
    | BCallStmt { args; _ } -> List.exists m args
    | BIf { condition; then_stmts; else_stmts } ->
        m condition || List.exists st then_stmts || List.exists st else_stmts
    | BCase { selector; cases; default } ->
        m selector || List.exists (fun (k, b) -> m k || List.exists st b) cases
        || List.exists st default
    | BBlock b -> List.exists st b
    | BWhile { condition; body } -> m condition || List.exists st body
    | BFor { init; condition; update; body } ->
        st init || m condition || st update || List.exists st body
    | BReturn e -> (match e with Some e -> m e | None -> false) in
  List.exists st stmts

(* Behavioral_nba_slice.thread, but aware of the process's blocking
   temporaries: their current symbolic values travel with the pending value
   of [r] and are substituted wherever the body reads them (right-hand
   sides, conditions, case selectors), and an if/case merges them with a mux
   on its condition.  The threaded tree is assigned at the END of the body;
   without the substitution a temporary written in one arm and read by [r]'s
   leaf in that arm (`quotient = ...; accu <= f(quotient)`) is read on every
   path, so it has to hold its old value and becomes a register.  A read
   before any write on its path keeps `BVar t` (the old value), which is
   what it means.  None when a temporary is written in a loop or by
   slice/bit -- the caller leaves that register alone. *)
exception No_env of string
let poison_name t = "__share_poison_" ^ t
let poison t = BVar (poison_name t)
let rec stmt_writes_name t = function
  | BAssign { lhs; _ } -> lhs = t
  | BCallStmt { args = BVar x :: _; _ } -> x = t
  | BIf { then_stmts; else_stmts; _ } ->
      List.exists (stmt_writes_name t) then_stmts || List.exists (stmt_writes_name t) else_stmts
  | BCase { cases; default; _ } ->
      List.exists (fun (_, b) -> List.exists (stmt_writes_name t) b) cases
      || List.exists (stmt_writes_name t) default
  | BBlock b | BWhile { body = b; _ } | BFor { body = b; _ } -> List.exists (stmt_writes_name t) b
  | BCallStmt _ | BReturn _ -> false
let no_env_why = ref ""

let thread_env ~w ~bvars r stmts : bexpr option =
  let is_temp n = List.mem n bvars in
  let subst env e =
    if env = [] then e
    else
      let rec go e =
        match e with
        | BVar n -> (match List.assoc_opt n env with Some v -> v | None -> e)
        | BConst _ -> e
        | BBinOp x -> BBinOp { x with lhs = go x.lhs; rhs = go x.rhs }
        | BUnOp x -> BUnOp { x with operand = go x.operand }
        | BSelect { array; index } -> BSelect { array = go array; index = go index }
        | BSlice x -> BSlice { x with signal = go x.signal }
        | BConcat l -> BConcat (List.map go l)
        | BReplicate x -> BReplicate { x with value = go x.value }
        | BCond x ->
            BCond { condition = go x.condition; then_val = go x.then_val; else_val = go x.else_val }
        | BCall x -> BCall { x with args = List.map go x.args } in
      go e in
  let same a b = a == b || a = b in
  let merge c (ea, ca) (eb, cb) =
    let names = List.sort_uniq compare (List.map fst ea @ List.map fst eb) in
    let get env n = match List.assoc_opt n env with Some v -> v | None -> BVar n in
    let env = List.map (fun n ->
      let va = get ea n and vb = get eb n in
      (n, if same va vb then va else BCond { condition = c; then_val = va; else_val = vb })) names in
    env, (if same ca cb then ca else BCond { condition = c; then_val = ca; else_val = cb }) in
  let rec writes_temp = function
    | BAssign { lhs; _ } -> is_temp lhs
    | BCallStmt { args = BVar t :: _; _ } -> is_temp t
    | BIf { then_stmts; else_stmts; _ } ->
        List.exists writes_temp then_stmts || List.exists writes_temp else_stmts
    | BCase { cases; default; _ } ->
        List.exists (fun (_, b) -> List.exists writes_temp b) cases || List.exists writes_temp default
    | BBlock b | BWhile { body = b; _ } | BFor { body = b; _ } -> List.exists writes_temp b
    | BCallStmt _ | BReturn _ -> false in
  let rec block st l = List.fold_left stmt st l
  and stmt ((env, cur) as st) s =
    match s with
    | BAssign { lhs; rhs } when lhs = r ->
        (* a blocking-assigned register reads its own pending value *)
        let env' = if is_temp r then (r, cur) :: List.remove_assoc r env else env in
        (env, subst env' rhs)
    | BAssign { lhs; rhs } when is_temp lhs ->
        ((lhs, subst env rhs) :: List.remove_assoc lhs env, cur)
    | BAssign _ -> st
    | BCallStmt { func; args = BVar t :: rest } when t = r ->
        let env' = if is_temp r then (r, cur) :: List.remove_assoc r env else env in
        (env, Behavioral_nba_slice.thread_stmt ~w r cur
                (BCallStmt { func; args = BVar t :: List.map (subst env') rest }))
    | BCallStmt { args = BVar t :: _; _ } when is_temp t ->
        (* a slice / part-select write to a temporary: its value is no longer
           an expression we track; poison it, and only a tree that reads it
           afterwards is given up *)
        ((t, poison t) :: List.remove_assoc t env, cur)
    | BCallStmt _ | BReturn _ -> st
    | BIf { condition; then_stmts; else_stmts } ->
        merge (subst env condition) (block st then_stmts) (block st else_stmts)
    | BCase { selector; cases; default } ->
        let sel = subst env selector in
        List.fold_right (fun (k, b) acc ->
          merge (BBinOp { op = BEq; lhs = sel; rhs = subst env k; result_type = Behavioral_nba_slice.b1 })
            (block st b) acc) cases (block st default)
    | BBlock b -> block st b
    | BWhile { body; _ } | BFor { body; _ } ->
        if List.exists (Behavioral_nba_slice.stmt_writes r) body then raise (No_env "loop")
        else
          (List.fold_left (fun env t ->
             if List.exists (fun s -> writes_temp s && stmt_writes_name t s) body
             then (t, poison t) :: List.remove_assoc t env else env) env bvars, cur) in
  try
    let t = snd (block ([], BVar r) stmts) in
    if List.exists (fun v -> Behavioral_nba_slice.mentions (poison_name v) t) bvars
    then (no_env_why := "reads a poisoned temporary"; None) else Some t
  with No_env why -> no_env_why := why; None

let share_process ~widths (p : bprocess) : bprocess * int =
  match p with
  | BCombinational _ -> p, 0
  | BSequential r ->
      let regs =
        List.sort_uniq compare
          (List.concat_map (fun s ->
             let acc = ref [] in
             let rec w = function
               | BAssign { lhs; _ } -> acc := lhs :: !acc
               | BIf { then_stmts; else_stmts; _ } -> List.iter w then_stmts; List.iter w else_stmts
               | BCase { cases; default; _ } -> List.iter (fun (_, b) -> List.iter w b) cases; List.iter w default
               | BBlock b -> List.iter w b
               | _ -> () in
             w s; !acc) r.body) in
      let saved = ref 0 in
      (* share inside ONE statement list; returns the list rewritten *)
      let share_in stmts =
        List.fold_left (fun stmts reg ->
          match Hashtbl.find_opt widths reg with
          | Some w when w >= 4 && Behavioral_nba_slice.threadable ~w reg stmts
                        && List.exists (Behavioral_nba_slice.stmt_writes reg) stmts ->
              let tree =
                match thread_env ~w ~bvars:r.blocking_vars reg stmts with
                | Some t -> t
                | None -> Behavioral_nba_slice.thread ~w reg (BVar reg) stmts in
              (* The rewritten tree is assigned at the END of the body, so a
                 blocking temporary it still reads (thread_env could not
                 resolve it) would be read on paths that never wrote it this
                 cycle and have to hold its old value: a register where there
                 was none (the OCaml VM grew 93 FFs).  Leave such registers
                 alone. *)
              let reads_temp =
                List.exists (fun v -> Behavioral_nba_slice.mentions v tree)
                  (List.filter (fun v -> v <> reg) r.blocking_vars) in
              let blocking_reg = List.mem reg r.blocking_vars && read_elsewhere reg stmts in
              if Sys.getenv_opt "SHARE_DEBUG" <> None && (reads_temp || blocking_reg) then
                Printf.eprintf "[share_accum] skip %s: env=%s temps=[%s]%s\n%!" reg
                  (if thread_env ~w ~bvars:r.blocking_vars reg stmts <> None then "ok" else !no_env_why)
                  (String.concat "," (List.filter (fun v -> Behavioral_nba_slice.mentions v tree) r.blocking_vars))
                  (if blocking_reg then " (blocking, read elsewhere)" else "");
              if reads_temp || blocking_reg then stmts else
              let before = ref tree and gain = ref 0 in
              List.iter (fun (x, xw) ->
                let n = count ~widths ~w:xw ~x !before in
                if n >= 1 then begin
                  let t', g = rewrite ~reg ~widths ~w:xw ~x !before in
                  match find_x_expr ~x !before with
                  | Some xe when g > 0 ->
                      gain := !gain + g; before := subst_placeholder xe t'
                  | _ -> ()
                end) (candidates ~widths tree);
              if not group_off then begin
                let t', g = group_share ~widths ~w !before in
                if g > 0 then begin gain := !gain + g; before := t' end
              end;
              if !gain > 0 then begin
                saved := !saved + !gain;
                Behavioral_nba_slice.strip reg stmts @ [ BAssign { lhs = reg; rhs = !before } ]
              end else stmts
          | _ -> stmts) stmts regs in
      (* keep the reset arm untouched (its constant reset values must stay
         visible to the lowering's reset extraction): share inside the else
         branch of `if (reset) ... else ...`, else over the whole body *)
      let body =
        match r.body, r.reset with
        | [ BIf { condition; then_stmts; else_stmts } ], Some rst
          when Behavioral_nba_slice.mentions rst condition ->
            [ BIf { condition; then_stmts; else_stmts = share_in else_stmts } ]
        | body, _ -> share_in body in
      BSequential { r with body }, !saved

let share_module (m : bmodule) : bmodule =
  let widths = Behavioral_const.signal_widths_of_module m in
  let total = ref 0 in
  let processes = List.map (fun p -> let p', n = share_process ~widths p in total := !total + n; p') m.processes in
  if !total > 0 then
    Printf.eprintf "[share_accum] %s: %d add/sub operator(s) folded into shared adders\n%!" m.name !total;
  { m with processes }

let share_program (p : bprogram) : bprogram =
  { p with modules = List.map share_module p.modules }

(* Add/sub fusion: `c ? x - y : x + y` (either arm order, either operand
 * order on the add) -> `(x + (c ? ~y : y)) + c`.  x - y = x + ~y + 1, so the
 * two adders and their output mux become one adder whose second operand is
 * conditionally inverted (folded into the per-bit S LUT) and whose carry-in
 * is c; Bir_to_aig fuses the trailing 1-bit add into the chain's CYINIT.
 * picorv32's alu_add_sub is exactly this shape.  Exact mod 2^w.
 * GATE_MAP_BIR_NOADDSUB=1 skips. *)
let fuse_addsub_module (m : bmodule) : bmodule * int =
  let widths = Behavioral_const.signal_widths_of_module m in
  let wof e = Behavioral_const.width_of_expr_with widths e in
  let n = ref 0 in
  let bt w = BInt { width = w; signed = Unsigned } in
  let rec rw e =
    match e with
    | BCond { condition; then_val; else_val } ->
        let condition = rw condition and then_val = rw then_val and else_val = rw else_val in
        let fused =
          match then_val, else_val with
          | BBinOp { op = BSub; lhs = x; rhs = y; result_type = BInt { width = w; _ } },
            BBinOp { op = BAdd; lhs = a; rhs = b; result_type = BInt { width = w'; _ } }
          | BBinOp { op = BAdd; lhs = a; rhs = b; result_type = BInt { width = w'; _ } },
            BBinOp { op = BSub; lhs = x; rhs = y; result_type = BInt { width = w; _ } }
            when w = w' && w >= 2 && wof condition = Some 1
                 && wof x = Some w && wof y = Some w
                 && ((key a = key x && key b = key y) || (key a = key y && key b = key x)) ->
              let sub_when_c = (match then_val with BBinOp { op = BSub; _ } -> true | _ -> false) in
              let ny = BUnOp { op = BNot; operand = y; result_type = bt w } in
              let cin = if sub_when_c then condition
                        else BUnOp { op = BNot; operand = condition; result_type = bt 1 } in
              let m_ = if sub_when_c then BCond { condition; then_val = ny; else_val = y }
                       else BCond { condition; then_val = y; else_val = ny } in
              incr n;
              Some (BBinOp { op = BAdd
                           ; lhs = BBinOp { op = BAdd; lhs = x; rhs = m_; result_type = bt w }
                           ; rhs = cin; result_type = bt w })
          | _ -> None in
        (match fused with Some f -> f | None -> BCond { condition; then_val; else_val })
    | BBinOp r -> BBinOp { r with lhs = rw r.lhs; rhs = rw r.rhs }
    | BUnOp r -> BUnOp { r with operand = rw r.operand }
    | BSelect { array; index } -> BSelect { array = rw array; index = rw index }
    | BSlice r -> BSlice { r with signal = rw r.signal }
    | BConcat l -> BConcat (List.map rw l)
    | BReplicate r -> BReplicate { r with value = rw r.value }
    | BCall r -> BCall { r with args = List.map rw r.args }
    | BVar _ | BConst _ -> e in
  let rec rs s =
    match s with
    | BAssign { lhs; rhs } -> BAssign { lhs; rhs = rw rhs }
    | BIf { condition; then_stmts; else_stmts } ->
        BIf { condition = rw condition; then_stmts = List.map rs then_stmts
            ; else_stmts = List.map rs else_stmts }
    | BCase { selector; cases; default } ->
        BCase { selector = rw selector
              ; cases = List.map (fun (k, b) -> (k, List.map rs b)) cases
              ; default = List.map rs default }
    | BBlock l -> BBlock (List.map rs l)
    | s -> s in
  let processes = List.map (function
    | BCombinational r -> BCombinational { r with body = List.map rs r.body }
    | BSequential r -> BSequential { r with body = List.map rs r.body }) m.processes in
  { m with processes }, !n

let fuse_addsub_program (p : bprogram) : bprogram =
  { p with modules =
      List.map (fun m ->
        let m', n = fuse_addsub_module m in
        if n > 0 then Printf.eprintf "[addsub] %s: %d add/sub pair(s) fused into one adder\n%!" m.name n;
        m') p.modules }
