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

(* SSA context.  [versions] maps each variable to its CURRENT top
   version — flat ints, not a stack.  Scope handling (BIf, BCase) is
   done by snapshotting and restoring [versions], not by stack
   push/pop, so each branch evolves from a clean copy of the entering
   state.  This is simpler than the previous Stack-of-versions
   approach and avoids the aliasing bug where [Hashtbl.copy] returned
   shallow copies sharing the same Stack.t values — pushing inside a
   branch mutated the "saved" snapshot too, and the BIf phi ended up
   reading the post-then state instead of pre-then. *)
module StringMap = Map.Make (String)
type ssa_context = {
  mutable next_version: int;
  mutable versions: int StringMap.t;
  widths: (string, int) Hashtbl.t;
  mutable phi_nodes: (string * string list * string) list;
}

let create_ssa_context ?(widths = Hashtbl.create 0) () = {
  next_version = 0;
  versions = StringMap.empty;
  widths;
  phi_nodes = [];
}

let current_version_opt ctx var = StringMap.find_opt var ctx.versions

let current_version ctx var =
  match current_version_opt ctx var with
  | Some v -> v
  | None -> 0

let push_version ctx var =
  let version = ctx.next_version in
  ctx.next_version <- version + 1;
  ctx.versions <- StringMap.add var version ctx.versions;
  version

let pop_version _ctx _var = ()  (* no-op: scoping is via snapshot/restore *)

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

(* Find every signal that is *ever* the target of @slice_write or
   @part_sel_write in a statement list (recursively).  Used to decide
   which BAssigns need SSA versioning.  Plain BAssigns to a name that
   is never slice-written can stay un-versioned: SV's last-wins
   semantics on hardcaml Always.Variable converges to one FF for
   case-arm-style writes and we save the N-stage shift register that
   versioning would produce.  Names that *are* slice-written need
   their BAssigns versioned too, otherwise the first slice-write
   would read `BVar arr` (the final composite) and form a cycle. *)
let sliced_targets stmts =
  let h = Hashtbl.create 8 in
  let rec go = function
    | BAssign _ -> ()
    | BIf { then_stmts; else_stmts; _ } ->
        List.iter go then_stmts; List.iter go else_stmts
    | BCase { cases; default; _ } ->
        List.iter (fun (_, ss) -> List.iter go ss) cases;
        List.iter go default
    | BBlock ss -> List.iter go ss
    | BWhile { body; _ } | BFor { body; _ } -> List.iter go body
    | BCallStmt { func; args = (BVar n) :: _ }
      when func = "@slice_write"
        || func = "@part_sel_write_up"
        || func = "@part_sel_write_down" ->
        Hashtbl.replace h n ()
    | BCallStmt _ | BReturn _ -> ()
  in
  List.iter go stmts;
  h

(* Convert statements to SSA form. *)
let rec stmt_to_ssa ?(sliced = Hashtbl.create 0) ctx stmt =
  match stmt with
  | BAssign { lhs; rhs } ->
      let rhs' = rename_expr ctx rhs in
      if Hashtbl.mem sliced lhs then begin
        let version = push_version ctx lhs in
        [BAssign { lhs = ssa_name lhs version; rhs = rhs' }]
      end else
        [BAssign { lhs; rhs = rhs' }]

  | BIf { condition; then_stmts; else_stmts } ->
      let condition' = rename_expr ctx condition in
      let saved = ctx.versions in
      let then_stmts' = List.concat (List.map (stmt_to_ssa ~sliced ctx) then_stmts) in
      let then_versions = ctx.versions in
      ctx.versions <- saved;
      let else_stmts' = List.concat (List.map (stmt_to_ssa ~sliced ctx) else_stmts) in
      let else_versions = ctx.versions in
      ctx.versions <- saved;
      (* Variables assigned in either branch = those whose version
         in then/else differs from the entering version.  Restrict to
         sliced_targets — phi-merging an UNVERSIONED variable would
         spuriously create new SSA names that hardcaml's Always
         compile emits as additional FFs, producing N-stage pipelines
         and losing reset values (picorv32 reg_pc grew reg_pc_7→_29→
         _32→_38→reg_pc; the reset BIf landed in _7 but never reached
         the visible reg_pc).  Non-sliced multi-write targets are
         handled correctly by hardcaml's last-write-wins. *)
      let bumped versions =
        StringMap.fold (fun k v acc ->
          if Hashtbl.mem sliced k &&
             StringMap.find_opt k saved <> Some v
          then k :: acc else acc
        ) versions [] in
      let assigned_vars =
        List.sort_uniq String.compare (bumped then_versions @ bumped else_versions) in
      (* For each var, the then-/else-side value is its version at
         the END of that branch, or the entering version if the
         branch didn't push, or the original un-versioned name. *)
      let name_at versions var =
        match StringMap.find_opt var versions with
        | Some v -> BVar (ssa_name var v)
        | None ->
            (match StringMap.find_opt var saved with
             | Some v -> BVar (ssa_name var v)
             | None -> BVar var) in
      let phi_assigns = List.map (fun var ->
        let then_expr = name_at then_versions var in
        let else_expr = name_at else_versions var in
        let new_version = push_version ctx var in
        let merged_name = ssa_name var new_version in
        let pretty_of = function BVar n -> n | _ -> "?" in
        ctx.phi_nodes <-
          (merged_name, [pretty_of then_expr; pretty_of else_expr], var)
          :: ctx.phi_nodes;
        BAssign {
          lhs = merged_name;
          rhs = BCond { condition = condition';
                        then_val = then_expr;
                        else_val = else_expr }
        }
      ) assigned_vars in
      [BIf { condition = condition'; then_stmts = then_stmts'; else_stmts = else_stmts' }]
      @ phi_assigns

  | BCase { selector; cases; default } ->
      (* Keep BCase intact for the emit but build phi merges per
         assigned variable.  Each case-arm is processed from a fresh
         copy of the entering map; we capture the exit map and use
         per-arm versions in a single ITE chain emitted *after* the
         BCase node.  Crucially the ITE chain reads only branch-end
         versions and the pre-case version — no inter-variable
         dependencies — so the merges don't form mutual cycles. *)
      let selector' = rename_expr ctx selector in
      let saved = ctx.versions in
      let cases' = List.map (fun (value, stmts) ->
        ctx.versions <- saved;
        let value' = rename_expr ctx value in
        let stmts' = List.concat (List.map (stmt_to_ssa ~sliced ctx) stmts) in
        (value', stmts', ctx.versions)
      ) cases in
      ctx.versions <- saved;
      let default' = List.concat (List.map (stmt_to_ssa ~sliced ctx) default) in
      let default_versions = ctx.versions in
      ctx.versions <- saved;
      let bumped vs =
        StringMap.fold (fun k v acc ->
          if Hashtbl.mem sliced k &&
             StringMap.find_opt k saved <> Some v
          then k :: acc else acc
        ) vs [] in
      let all_bumped =
        List.sort_uniq String.compare
          (List.concat_map (fun (_, _, v) -> bumped v) cases'
           @ bumped default_versions) in
      let name_at versions var =
        match StringMap.find_opt var versions with
        | Some v -> BVar (ssa_name var v)
        | None ->
            (match StringMap.find_opt var saved with
             | Some v -> BVar (ssa_name var v)
             | None -> BVar var) in
      let phi_assigns = List.map (fun var ->
        let default_expr = name_at default_versions var in
        let rhs = List.fold_right (fun (value', _, vs) acc ->
          let case_expr = name_at vs var in
          BCond { condition =
                    BBinOp { op = BEq; lhs = selector'; rhs = value';
                             result_type = BInt { width = 1;
                                                  signed = Unsigned } };
                  then_val = case_expr;
                  else_val = acc }
        ) cases' default_expr in
        let new_version = push_version ctx var in
        BAssign { lhs = ssa_name var new_version; rhs }
      ) all_bumped in
      let stripped = List.map (fun (v, s, _) -> (v, s)) cases' in
      [ BCase { selector = selector'; cases = stripped; default = default' } ]
      @ phi_assigns

  | BWhile { condition; body } ->
      (* While loops need special handling with loop phi nodes *)
      let condition' = rename_expr ctx condition in
      let body' = List.concat (List.map (stmt_to_ssa ~sliced ctx) body) in
      [BWhile { condition = condition'; body = body' }]

  | BFor { init; condition; update; body } ->
      let init' = List.hd (stmt_to_ssa ~sliced ctx init) in
      let condition' = rename_expr ctx condition in
      let body' = List.concat (List.map (stmt_to_ssa ~sliced ctx) body) in
      let update' = List.hd (stmt_to_ssa ~sliced ctx update) in
      [BFor { init = init'; condition = condition'; update = update'; body = body' }]

  | BBlock stmts ->
      let stmts' = List.concat (List.map (stmt_to_ssa ~sliced ctx) stmts) in
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
  let const_of = function BConst { value; _ } -> Some (Z.to_int value) | _ -> None in
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
  let const_of = function BConst { value; _ } -> Some (Z.to_int value) | _ -> None in
  match const_of base', const_of w' with
  | Some base, Some w when w > 0 ->
      let msb, lsb = match func with
        | "@part_sel_write_up"   -> base + w - 1, base
        | "@part_sel_write_down" -> base, base - w + 1
        | _ -> base + w - 1, base in
      let bw = match w' with BConst { width; _ } -> width | _ -> 32 in
      let m_e = BConst { value = Z.of_int msb; width = bw } in
      let l_e = BConst { value = Z.of_int lsb; width = bw } in
      ssa_slice_write ctx ~lhs ~m_e ~l_e ~data ~fallback
  | _ -> fallback

(* Add a final-version writeback at the end of an SSA-converted body:
   for each variable that was ever assigned (final-version > 0), emit
   `BAssign { lhs = original_name; rhs = BVar (name_<final_v>) }` so
   external readers (other processes referencing `name`) see the post-
   body composed value.  Without this, the SSA-renamed `name_K`
   signals are dangling and the original `name` has no driver. *)
let final_writebacks ctx =
  StringMap.fold (fun var v acc ->
    BAssign { lhs = var; rhs = BVar (ssa_name var v) } :: acc
  ) ctx.versions []

(* Convert process to SSA form.  When [widths] is supplied, slice-
   write / part-sel-write calls expand to versioned BAssigns whose
   RHS stitches the previous version's untouched bits via BConcat. *)
let process_to_ssa ?widths = function
  | BCombinational { name; sensitivity; body } ->
      let ctx = create_ssa_context ?widths () in
      let sliced = sliced_targets body in
      let body' = List.concat (List.map (stmt_to_ssa ~sliced ctx) body) in
      let tail = final_writebacks ctx in
      BCombinational { name; sensitivity; body = body' @ tail }

  | BSequential { name; clock; clock_edge; reset; reset_edge; reset_async; body; blocking_vars } ->
      let ctx = create_ssa_context ?widths () in
      let sliced = sliced_targets body in
      let body' = List.concat (List.map (stmt_to_ssa ~sliced ctx) body) in
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
          (* Tag the minted version.  Downstream (behavioral_to_hardcaml's
             register/wire classification) must be able to tell an SSA
             temporary from an ordinary RTL name that merely LOOKS like one
             (`IDLE_MATCH_2` alongside `IDLE_MATCH` is real Xilinx PCS RTL,
             not SSA output) -- guessing from the `_<digits>` name shape
             forced a real register to a wire and turned its feedback into a
             combinational loop. *)
          let sig_ : bsignal = { src with name; direction = `Internal;
                                          initial_value = None;
                                          attrs = ("ssa", "1") :: src.attrs } in
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
  Printf.printf "  Variables in SSA form: %d\n" (StringMap.cardinal ctx.versions);
  Printf.printf "  Phi nodes created: %d\n" (List.length ctx.phi_nodes);

  if List.length ctx.phi_nodes > 0 then begin
    Printf.printf "\nPhi nodes:\n";
    List.iter (fun (dest, sources, orig_var) ->
      Printf.printf "  %s := φ(%s) [original: %s]\n"
        dest (String.concat ", " sources) orig_var
    ) (List.rev ctx.phi_nodes)
  end
