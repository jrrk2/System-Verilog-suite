(* SSA Construction for Behavioral IR
 *
 * Transforms behavioral IR to Static Single Assignment (SSA) form.
 * In SSA form, each variable is assigned exactly once.
 * Phi nodes are inserted at control flow join points.
 *
 * Example:
 *   Before SSA:                After SSA:
 *   x := 0                     x_1 := 0
 *   if cond then               if cond then
 *     x := 1                     x_2 := 1
 *   else                       else
 *     x := 2                     x_3 := 2
 *   use x                      x_4 := φ(x_2, x_3)
 *                              use x_4
 *)

open Behavioral_ir

(* SSA context tracks variable versions *)
type ssa_context = {
  mutable next_version: int;
  (* Current version of each variable in each scope *)
  versions: (string, int Stack.t) Hashtbl.t;
  (* Declared width of each signal — needed by @slice_write /
     @part_sel_write_* expansion so the new version's RHS can stitch
     the previous version's untouched bits via BSlice + BConcat.  When
     absent (legacy callers), the slice-write branch falls through to
     leaving the BCallStmt as-is and the cycle survives — matching
     pre-extension behaviour. *)
  widths: (string, int) Hashtbl.t;
  (* Phi nodes to insert at join points *)
  mutable phi_nodes: (string * string list * string) list;
}

let create_ssa_context ?(widths = Hashtbl.create 0) () = {
  next_version = 0;
  versions = Hashtbl.create 50;
  widths;
  phi_nodes = [];
}

(* Get current version of a variable, or None if it hasn't been
   assigned yet in this scope (the read should resolve to the original
   un-versioned name). *)
let current_version_opt ctx var =
  match Hashtbl.find_opt ctx.versions var with
  | Some stack when not (Stack.is_empty stack) -> Some (Stack.top stack)
  | _ -> None

(* Legacy API — kept for callers that expect an int.  Returns 0 when
   no version has been pushed; callers that need the "original name"
   semantics should switch to [current_version_opt]. *)
let current_version ctx var =
  match current_version_opt ctx var with
  | Some v -> v
  | None -> 0

(* Push new version of variable *)
let push_version ctx var =
  let version = ctx.next_version in
  ctx.next_version <- version + 1;

  let stack = try Hashtbl.find ctx.versions var
            with Not_found ->
              let s = Stack.create () in
              Hashtbl.add ctx.versions var s;
              s
  in
  Stack.push version stack;
  version

(* Pop version (when exiting scope) *)
let pop_version ctx var =
  try
    let stack = Hashtbl.find ctx.versions var in
    if not (Stack.is_empty stack) then
      ignore (Stack.pop stack)
  with Not_found -> ()

(* Generate SSA name for variable *)
let ssa_name var version =
  Printf.sprintf "%s_%d" var version

(* Rename variable references in expressions.  Reads of variables
   that haven't been assigned in this scope resolve to the original
   name (no version suffix) so they pick up the FF/wire output from
   outside the always block. *)
let rec rename_expr ctx = function
  | BVar var ->
      (match current_version_opt ctx var with
       | Some version -> BVar (ssa_name var version)
       | None -> BVar var)

  | BConst _ as c -> c

  | BBinOp { op; lhs; rhs; result_type } ->
      BBinOp {
        op;
        lhs = rename_expr ctx lhs;
        rhs = rename_expr ctx rhs;
        result_type;
      }

  | BUnOp { op; operand; result_type } ->
      BUnOp {
        op;
        operand = rename_expr ctx operand;
        result_type;
      }

  | BSelect { array; index } ->
      BSelect {
        array = rename_expr ctx array;
        index = rename_expr ctx index;
      }

  | BSlice { signal; msb; lsb } ->
      BSlice {
        signal = rename_expr ctx signal;
        msb;
        lsb;
      }

  | BConcat exprs ->
      BConcat (List.map (rename_expr ctx) exprs)

  | BReplicate { count; value } ->
      BReplicate {
        count;
        value = rename_expr ctx value;
      }

  | BCond { condition; then_val; else_val } ->
      BCond {
        condition = rename_expr ctx condition;
        then_val = rename_expr ctx then_val;
        else_val = rename_expr ctx else_val;
      }

  | BCall { func; args } ->
      BCall {
        func;
        args = List.map (rename_expr ctx) args;
      }

(* Convert statements to SSA form *)
let rec stmt_to_ssa ctx = function
  | BAssign { lhs; rhs } ->
      (* Rename RHS first (uses old version) *)
      let rhs' = rename_expr ctx rhs in

      (* Create new version for LHS *)
      let version = push_version ctx lhs in
      let lhs' = ssa_name lhs version in

      [BAssign { lhs = lhs'; rhs = rhs' }]

  | BIf { condition; then_stmts; else_stmts } ->
      let condition' = rename_expr ctx condition in

      (* Track variables assigned in each branch *)
      let assigned_vars = ref [] in

      (* Save current versions *)
      let saved_versions = Hashtbl.copy ctx.versions in

      (* Process then branch *)
      let then_stmts' = List.concat (List.map (stmt_to_ssa ctx) then_stmts) in
      let then_versions = Hashtbl.copy ctx.versions in

      (* Restore and process else branch *)
      Hashtbl.iter (fun var stack ->
        Hashtbl.replace ctx.versions var (Stack.copy stack)
      ) saved_versions;

      let else_stmts' = List.concat (List.map (stmt_to_ssa ctx) else_stmts) in
      let else_versions = Hashtbl.copy ctx.versions in

      (* Find variables assigned in either branch *)
      let find_assigned versions =
        let vars = ref [] in
        Hashtbl.iter (fun var then_stack ->
          try
            let saved_stack = Hashtbl.find saved_versions var in
            if Stack.length then_stack > Stack.length saved_stack then
              vars := var :: !vars
          with Not_found ->
            vars := var :: !vars
        ) versions;
        !vars
      in

      assigned_vars := List.sort_uniq String.compare
        (find_assigned then_versions @ find_assigned else_versions);

      (* Create phi nodes for assigned variables *)
      let phi_assigns = List.map (fun var ->
        (* Get versions from each branch *)
        let then_ver = try
          let stack = Hashtbl.find then_versions var in
          if Stack.is_empty stack then 0 else Stack.top stack
        with Not_found -> current_version ctx var in

        let else_ver = try
          let stack = Hashtbl.find else_versions var in
          if Stack.is_empty stack then 0 else Stack.top stack
        with Not_found -> current_version ctx var in

        (* Create new merged version *)
        let new_version = push_version ctx var in
        let merged_name = ssa_name var new_version in
        let then_name = ssa_name var then_ver in
        let else_name = ssa_name var else_ver in

        (* Record phi node (conceptual - we'll represent as assignment for now) *)
        ctx.phi_nodes <- (merged_name, [then_name; else_name], var) :: ctx.phi_nodes;

        (* For now, represent phi as conditional assignment *)
        BAssign {
          lhs = merged_name;
          rhs = BCond {
            condition = condition';
            then_val = BVar then_name;
            else_val = BVar else_name;
          }
        }
      ) !assigned_vars in

      (* Return if statement followed by phi assignments *)
      [BIf { condition = condition'; then_stmts = then_stmts'; else_stmts = else_stmts' }]
      @ phi_assigns

  | BCase { selector; cases; default } ->
      let selector' = rename_expr ctx selector in

      (* Save current versions *)
      let saved_versions = Hashtbl.copy ctx.versions in

      (* Process each case *)
      let cases' = List.map (fun (value, stmts) ->
        (* Restore versions for each case *)
        Hashtbl.iter (fun var stack ->
          Hashtbl.replace ctx.versions var (Stack.copy stack)
        ) saved_versions;

        let value' = rename_expr ctx value in
        let stmts' = List.concat (List.map (stmt_to_ssa ctx) stmts) in
        (value', stmts')
      ) cases in

      (* Process default case *)
      Hashtbl.iter (fun var stack ->
        Hashtbl.replace ctx.versions var (Stack.copy stack)
      ) saved_versions;
      let default' = List.concat (List.map (stmt_to_ssa ctx) default) in

      (* TODO: Insert phi nodes for case statements *)
      [BCase { selector = selector'; cases = cases'; default = default' }]

  | BWhile { condition; body } ->
      (* While loops need special handling with loop phi nodes *)
      let condition' = rename_expr ctx condition in
      let body' = List.concat (List.map (stmt_to_ssa ctx) body) in
      [BWhile { condition = condition'; body = body' }]

  | BFor { init; condition; update; body } ->
      let init' = List.hd (stmt_to_ssa ctx init) in
      let condition' = rename_expr ctx condition in
      let body' = List.concat (List.map (stmt_to_ssa ctx) body) in
      let update' = List.hd (stmt_to_ssa ctx update) in
      [BFor { init = init'; condition = condition'; update = update'; body = body' }]

  | BBlock stmts ->
      let stmts' = List.concat (List.map (stmt_to_ssa ctx) stmts) in
      [BBlock stmts']

  | BCallStmt { func = "@slice_write";
                args = [BVar lhs; m_e; l_e; data] } as orig ->
      ssa_slice_write ctx ~lhs ~m_e ~l_e ~data ~fallback:[orig]
  | BCallStmt { func; args = [BVar lhs; base_e; w_e; data] } as orig
    when func = "@part_sel_write_up" || func = "@part_sel_write_down" ->
      ssa_part_sel_write ctx ~func ~lhs ~base_e ~w_e ~data ~fallback:[orig]
  | BCallStmt _ as s -> [s]
  | BReturn _ as s -> [s]

(* @slice_write(name, msb, lsb, data): name[msb:lsb] = data.  In
   SSA form, we push a new version of `name` whose RHS is a concat of
   the previous version's untouched bits and `data` (renamed via
   current versions).  Falls back to leaving the call as-is when m,
   l aren't constants or the signal width isn't known. *)
and ssa_slice_write ctx ~lhs ~m_e ~l_e ~data ~fallback =
  let m' = rename_expr ctx m_e in
  let l' = rename_expr ctx l_e in
  let data' = rename_expr ctx data in
  let const_of = function BConst { value; _ } -> Some value | _ -> None in
  match const_of m', const_of l',
        Hashtbl.find_opt ctx.widths lhs with
  | Some msb, Some lsb, Some total_w when total_w > 0 && msb >= lsb ->
      let prev_name = match current_version_opt ctx lhs with
        | Some v -> ssa_name lhs v
        | None   -> lhs in
      let parts = ref [] in
      if lsb >= 1 then
        parts := BSlice { signal = BVar prev_name; msb = lsb - 1; lsb = 0 } :: !parts;
      parts := data' :: !parts;
      if msb + 1 <= total_w - 1 then
        parts := BSlice { signal = BVar prev_name;
                          msb = total_w - 1; lsb = msb + 1 } :: !parts;
      let rhs = match !parts with
        | [single] -> single
        | many -> BConcat many in
      let version = push_version ctx lhs in
      [ BAssign { lhs = ssa_name lhs version; rhs } ]
  | _ -> fallback

(* @part_sel_write_up(name, base, width, data)  ⇒  name[base+:width] = data.
   @part_sel_write_down(name, base, width, data) ⇒  name[base-:width] = data.
   Constant base/width get converted to (msb, lsb) and reused via
   ssa_slice_write's per-version-concat expansion. *)
and ssa_part_sel_write ctx ~func ~lhs ~base_e ~w_e ~data ~fallback =
  let base' = rename_expr ctx base_e in
  let w'    = rename_expr ctx w_e in
  let const_of = function BConst { value; _ } -> Some value | _ -> None in
  match const_of base', const_of w' with
  | Some base, Some w when w > 0 ->
      let msb, lsb = match func with
        | "@part_sel_write_up"   -> base + w - 1, base
        | "@part_sel_write_down" -> base, base - w + 1
        | _ -> base + w - 1, base in
      let bw = match w' with BConst { width; _ } -> width | _ -> 32 in
      let m_e = BConst { value = msb; width = bw } in
      let l_e = BConst { value = lsb; width = bw } in
      ssa_slice_write ctx ~lhs ~m_e ~l_e ~data ~fallback
  | _ -> fallback

(* Add a final-version writeback at the end of an SSA-converted body:
   for each variable that was ever assigned (final-version > 0), emit
   `BAssign { lhs = original_name; rhs = BVar (name_<final_v>) }` so
   external readers (other processes referencing `name`) see the post-
   body composed value.  Without this, the SSA-renamed `name_K`
   signals are dangling and the original `name` has no driver. *)
let final_writebacks ctx =
  Hashtbl.fold (fun var stack acc ->
    if Stack.is_empty stack then acc
    else
      let v = Stack.top stack in
      BAssign { lhs = var; rhs = BVar (ssa_name var v) } :: acc
  ) ctx.versions []

(* Convert process to SSA form.  When [widths] is supplied, slice-
   write / part-sel-write calls expand to versioned BAssigns whose
   RHS stitches the previous version's untouched bits via BConcat. *)
let process_to_ssa ?widths = function
  | BCombinational { name; sensitivity; body } ->
      let ctx = create_ssa_context ?widths () in
      let body' = List.concat (List.map (stmt_to_ssa ctx) body) in
      let tail = final_writebacks ctx in
      BCombinational { name; sensitivity; body = body' @ tail }

  | BSequential { name; clock; clock_edge; reset; reset_edge; reset_async; body; blocking_vars } ->
      let ctx = create_ssa_context ?widths () in
      let body' = List.concat (List.map (stmt_to_ssa ctx) body) in
      let tail = final_writebacks ctx in
      BSequential { name; clock; clock_edge; reset; reset_edge; reset_async;
                    body = body' @ tail; blocking_vars }

(* Convert module to SSA form.  Builds the [widths] table from the
   module's declared signals so slice-write expansion has the right
   total width per target. *)
let module_to_ssa bmod =
  let widths = Hashtbl.create (List.length bmod.signals) in
  let rec w_of = function
    | BInt { width; _ } -> width
    | BBool -> 1
    | BArray { element; size } -> size * w_of element
    | BStruct _ -> 32 in
  List.iter (fun (s : bsignal) -> Hashtbl.replace widths s.name (w_of s.stype))
    bmod.signals;
  let processes' = List.map (process_to_ssa ~widths) bmod.processes in
  (* New signals introduced by SSA renaming get added to the module's
     signal list so downstream emit (behavioral_to_hardcaml) has a
     declaration for each versioned name.  We collect the names that
     appear as a BAssign LHS in any process but aren't already in the
     module's signal list. *)
  let known = Hashtbl.create 64 in
  List.iter (fun (s : bsignal) -> Hashtbl.replace known s.name s) bmod.signals;
  let add_if_new name =
    if Hashtbl.mem known name then ()
    else begin
      (* Width of the new name: peel back the _<digits> suffix and look
         up the original signal's stype. *)
      let base = match String.rindex_opt name '_' with
        | Some i -> String.sub name 0 i
        | None -> name in
      match Hashtbl.find_opt known base with
      | Some src ->
          let sig_ : bsignal = { src with name; direction = `Internal;
                                          initial_value = None } in
          Hashtbl.replace known name sig_
      | None -> ()
    end in
  let rec walk_stmt = function
    | BAssign { lhs; _ } -> add_if_new lhs
    | BIf { then_stmts; else_stmts; _ } ->
        List.iter walk_stmt then_stmts;
        List.iter walk_stmt else_stmts
    | BCase { cases; default; _ } ->
        List.iter (fun (_, ss) -> List.iter walk_stmt ss) cases;
        List.iter walk_stmt default
    | BBlock ss -> List.iter walk_stmt ss
    | BWhile { body; _ } | BFor { body; _ } -> List.iter walk_stmt body
    | _ -> () in
  List.iter (function
    | BCombinational { body; _ } | BSequential { body; _ } ->
        List.iter walk_stmt body
  ) processes';
  let signals' = Hashtbl.fold (fun _ s acc -> s :: acc) known [] in
  { bmod with processes = processes'; signals = signals' }

(* Convert program to SSA form *)
let program_to_ssa prog =
  let modules' = List.map module_to_ssa prog.modules in
  { modules = modules'; library_cells = prog.library_cells }

(* Pretty print SSA info *)
let print_ssa_stats ctx =
  Printf.printf "SSA Statistics:\n";
  Printf.printf "  Total versions created: %d\n" ctx.next_version;
  Printf.printf "  Variables in SSA form: %d\n" (Hashtbl.length ctx.versions);
  Printf.printf "  Phi nodes created: %d\n" (List.length ctx.phi_nodes);

  if List.length ctx.phi_nodes > 0 then begin
    Printf.printf "\nPhi nodes:\n";
    List.iter (fun (dest, sources, orig_var) ->
      Printf.printf "  %s := φ(%s) [original: %s]\n"
        dest (String.concat ", " sources) orig_var
    ) (List.rev ctx.phi_nodes)
  end
