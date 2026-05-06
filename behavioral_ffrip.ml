(* Flip-flop ripping: turn every BSequential body into the
 * combinational function that drives its FF inputs (D pins).
 *
 * After this pass:
 *   - Every state-holding signal Q becomes a primary INPUT
 *     (matched 1:1 across the two designs in the miter).
 *   - Every FF's D-pin combinational expression becomes a fresh
 *     primary OUTPUT named `<Q>__D`, also matched in the miter.
 *   - The original BSequential disappears, replaced by a
 *     BCombinational { body = [BAssign { lhs = "<Q>__D"; rhs = D_expr }] }.
 *
 * The Z3 miter then needs no special sequential handling —
 * combinational equivalence between the two D-pin functions
 * (with current Q's equated as primary inputs) implies cycle-by-
 * cycle sequential equivalence at depth-1.
 *
 * Why this is the right encoding:
 *   - EDFF (`if (en) q <= d`) and DFF+MUX (`q <= en ? d : q`) both
 *     produce the same D-pin expression `en ? d : q`, so the miter
 *     proves them equal automatically.
 *   - Reset: `if (rst) q <= 0; else q <= d` produces D-pin
 *     `rst ? 0 : d`, identical regardless of how the source code
 *     spelled it.
 *
 * Caveats:
 *   - Multiple writes to Q in different conditional branches need
 *     to be lowered to a single BCond first (which our existing
 *     behavioral_iflift pass does). This pass assumes the body
 *     reduces to a single BAssign per Q. *)

open Behavioral_ir

let d_pin_name s = s ^ "__D"

(* For each Q reachable from a body, derive the next-state
 * expression by interpreting the body symbolically:
 *
 *   `Q := e`                    →  D(Q) = e         (overwrites prior)
 *   `if (cond) <then> else <else>`
 *                              →  D(Q) = cond ? D_then(Q) : D_else(Q)
 *                                 (with the branch that doesn't write
 *                                 Q falling back to the prior value of Q)
 *   `case (sel) k1: s1; ... default: sd; endcase`
 *                              →  chain of ?: with sel == ki guards
 *
 * We build a per-Q expression starting from `BVar Q` (the current
 * state) and fold writes through. Multiple writes within the same
 * branch overwrite (last-write-wins); writes inside conditionals
 * become BConds; unwritten branches keep the prior value. *)
let rec next_state_after stmts q current =
  List.fold_left (next_state_after_one q) current stmts
and next_state_after_one q current = function
  | BAssign { lhs; rhs } when lhs = q -> rhs
  | BAssign _ -> current
  | BBlock ss -> next_state_after ss q current
  | BIf { condition; then_stmts; else_stmts } ->
      let t = next_state_after then_stmts q current in
      let e = next_state_after else_stmts q current in
      if t == current && e == current then current
      else BCond { condition; then_val = t; else_val = e }
  | BCase { selector; cases; default } ->
      let def_expr = next_state_after default q current in
      List.fold_right (fun (key, ss) acc ->
        let branch = next_state_after ss q current in
        if branch == acc then acc
        else BCond {
          condition = BBinOp {
            op = BEq; lhs = selector; rhs = key;
            result_type = BBool;
          };
          then_val = branch;
          else_val = acc;
        }
      ) cases def_expr
  | BWhile { body; _ } | BFor { body; _ } ->
      next_state_after body q current
  | BCallStmt _ | BReturn _ -> current

(* Collect every Q that appears as a write target anywhere in the
 * body — these are the state-holding signals. Then derive each
 * one's full D expression via next_state_after. *)
let rec collect_lhs acc = function
  | BAssign { lhs; _ } ->
      if List.mem lhs acc then acc else lhs :: acc
  | BBlock ss -> List.fold_left collect_lhs acc ss
  | BIf { then_stmts; else_stmts; _ } ->
      let acc = List.fold_left collect_lhs acc then_stmts in
      List.fold_left collect_lhs acc else_stmts
  | BCase { cases; default; _ } ->
      let acc = List.fold_left (fun a (_, ss) ->
        List.fold_left collect_lhs a ss) acc cases in
      List.fold_left collect_lhs acc default
  | BWhile { body; _ } | BFor { body; _ } ->
      List.fold_left collect_lhs acc body
  | BCallStmt _ | BReturn _ -> acc

let derive_d_pins body =
  let qs =
    List.fold_left collect_lhs [] body
    |> List.sort_uniq compare
  in
  List.map (fun q ->
    let d_expr = next_state_after body q (BVar q) in
    (q, d_expr)
  ) qs

(* Substitute every BVar of name `old_n` in expr with `new_e`. Used when
 * an output-port FF is ripped: D-cone references to the prior value of Q
 * (which has become a pass-through output) must redirect to the fresh
 * primary-input current-state signal `Q__Q`. *)
let rec subst_var old_n new_e e =
  match e with
  | BVar n when n = old_n -> new_e
  | BVar _ | BConst _ -> e
  | BBinOp r ->
      BBinOp { r with lhs = subst_var old_n new_e r.lhs;
                      rhs = subst_var old_n new_e r.rhs }
  | BUnOp r ->
      BUnOp { r with operand = subst_var old_n new_e r.operand }
  | BSelect { array; index } ->
      BSelect { array = subst_var old_n new_e array;
                index = subst_var old_n new_e index }
  | BSlice r ->
      BSlice { r with signal = subst_var old_n new_e r.signal }
  | BConcat es ->
      BConcat (List.map (subst_var old_n new_e) es)
  | BReplicate r ->
      BReplicate { r with value = subst_var old_n new_e r.value }
  | BCond { condition; then_val; else_val } ->
      BCond { condition = subst_var old_n new_e condition;
              then_val  = subst_var old_n new_e then_val;
              else_val  = subst_var old_n new_e else_val }
  | BCall r ->
      BCall { r with args = List.map (subst_var old_n new_e) r.args }

let rip_module (m : bmodule) : bmodule =
  let new_processes = ref [] in
  let new_signals = ref m.signals in

  let rec width_of_btype = function
    | BInt { width; _ } -> width
    | BBool -> 1
    | BArray { element; size } -> size * width_of_btype element
    | _ -> 1
  in
  let lookup_width name =
    match List.find_opt (fun (s : bsignal) -> s.name = name) m.signals with
    | Some s -> width_of_btype s.stype
    | None -> 1
  in

  let signal_exists name =
    List.exists (fun (s : bsignal) -> s.name = name) !new_signals
  in
  let upgrade_or_add_input name width =
    if signal_exists name then
      new_signals := List.map (fun (s : bsignal) ->
        if s.name = name && s.direction <> `Output
        then { s with direction = `Input }
        else s
      ) !new_signals
    else
      new_signals := {
        name;
        stype = BInt { width; signed = Unsigned };
        direction = `Input;
        initial_value = None;
        attrs = [];
      } :: !new_signals
  in
  let add_signal name width dir =
    if not (signal_exists name) then
      new_signals := {
        name;
        stype = BInt { width; signed = Unsigned };
        direction = dir;
        initial_value = None;
        attrs = [];
      } :: !new_signals
  in

  List.iter (fun p ->
    match p with
    | BCombinational _ -> new_processes := p :: !new_processes
    | BSequential s ->
        let pairs = derive_d_pins s.body in
        (* Every Q gets ripped. For Q's that are primary OUTPUT ports we
         * additionally insert a pass-through (Q = Q__Q) so the port
         * remains driven; the input cone still feeds the miter via
         * Q__D. This way EDFF and DFF+MUX implementations match purely
         * by name equivalence, regardless of whether Q is internal or
         * a port. *)
        let direction_of q =
          match List.find_opt (fun (sg : bsignal) -> sg.name = q)
                  m.signals with
          | None -> `Internal
          | Some sg -> sg.direction
        in
        if Sys.getenv_opt "MITER_FFRIP_RHS" <> None then
          List.iter (fun (q, e) ->
            let rec render = function
              | BVar n -> n
              | BConst { value; width } ->
                  Printf.sprintf "%d'd%d" width value
              | BBinOp { op; lhs; rhs; _ } ->
                  let s = match op with
                    | BEq -> "==" | BNe -> "!=" | BAdd -> "+"
                    | BSub -> "-" | BAnd -> "&" | BOr -> "|"
                    | _ -> "?op?"
                  in
                  Printf.sprintf "(%s %s %s)" (render lhs) s (render rhs)
              | BUnOp { op = BNot; operand; _ } ->
                  Printf.sprintf "~%s" (render operand)
              | BCond { condition; then_val; else_val } ->
                  Printf.sprintf "(%s ? %s : %s)"
                    (render condition) (render then_val) (render else_val)
              | _ -> "?"
            in
            Printf.eprintf "[ffrip-rhs] %s.%s__D = %s\n%!"
              m.name q (render e)
          ) pairs;
        if Sys.getenv_opt "MITER_FFRIP_DEBUG" <> None then begin
          let rec dump d s =
            let pad = String.make (d * 2) ' ' in
            match s with
            | BAssign { lhs; _ } -> Printf.eprintf "%s[ffrip]   %sAssign(%s)\n%!" pad pad lhs
            | BIf { then_stmts; else_stmts; _ } ->
                Printf.eprintf "%s[ffrip]   %sIf\n%!" pad pad;
                List.iter (dump (d+1)) then_stmts;
                Printf.eprintf "%s[ffrip]   %s else\n%!" pad pad;
                List.iter (dump (d+1)) else_stmts
            | BBlock ss ->
                Printf.eprintf "%s[ffrip]   %sBlock(%d)\n%!" pad pad (List.length ss);
                List.iter (dump (d+1)) ss
            | _ -> Printf.eprintf "%s[ffrip]   %sother\n%!" pad pad
          in
          Printf.eprintf "[ffrip] %s seq %s: %d Q pins (%s)\n%!"
            m.name s.name (List.length pairs)
            (String.concat ", " (List.map fst pairs));
          List.iter (dump 1) s.body
        end;
        List.iter (fun (q, d_expr) ->
          let w = lookup_width q in
          let d_name = d_pin_name q in
          match direction_of q with
          | `Output ->
              (* Output-port FF: insert a fresh primary input Q__Q
               * carrying the current state, drive Q (output port)
               * combinationally from Q__Q, and rewrite Q references
               * inside d_expr to use Q__Q. *)
              let q_state = q ^ "__Q" in
              add_signal q_state w `Input;
              add_signal d_name w `Output;
              let d_expr' = subst_var q (BVar q_state) d_expr in
              new_processes := BCombinational {
                name = "ffrip_d_" ^ q;
                sensitivity = [BAny];
                body = [BAssign { lhs = d_name; rhs = d_expr' }];
              } :: !new_processes;
              new_processes := BCombinational {
                name = "ffrip_q_" ^ q;
                sensitivity = [BAny];
                body = [BAssign { lhs = q; rhs = BVar q_state }];
              } :: !new_processes
          | _ ->
              (* Internal Q: promote to primary input, expose Q__D. *)
              upgrade_or_add_input q w;
              add_signal d_name w `Output;
              new_processes := BCombinational {
                name = "ffrip_" ^ q;
                sensitivity = [BAny];
                body = [BAssign { lhs = d_name; rhs = d_expr }];
              } :: !new_processes
        ) pairs
  ) m.processes;

  { m with
    signals   = List.rev !new_signals;
    processes = List.rev !new_processes }

let rip_program (p : bprogram) : bprogram =
  { p with modules = List.map rip_module p.modules }
