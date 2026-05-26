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
  (* Phi nodes to insert at join points *)
  mutable phi_nodes: (string * string list * string) list;
}

let create_ssa_context () = {
  next_version = 0;
  versions = Hashtbl.create 50;
  phi_nodes = [];
}

(* Get current version of a variable *)
let current_version ctx var =
  try
    let stack = Hashtbl.find ctx.versions var in
    if Stack.is_empty stack then 0
    else Stack.top stack
  with Not_found -> 0

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

(* Rename variable references in expressions *)
let rec rename_expr ctx = function
  | BVar var ->
      let version = current_version ctx var in
      BVar (ssa_name var version)

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

  | BCallStmt _ as s -> [s]
  | BReturn _ as s -> [s]

(* Convert process to SSA form *)
let process_to_ssa = function
  | BCombinational { name; sensitivity; body } ->
      let ctx = create_ssa_context () in
      let body' = List.concat (List.map (stmt_to_ssa ctx) body) in
      BCombinational { name; sensitivity; body = body' }

  | BSequential { name; clock; clock_edge; reset; reset_edge; reset_async; body; blocking_vars } ->
      let ctx = create_ssa_context () in
      let body' = List.concat (List.map (stmt_to_ssa ctx) body) in
      BSequential { name; clock; clock_edge; reset; reset_edge; reset_async; body = body'; blocking_vars }

(* Convert module to SSA form *)
let module_to_ssa bmod =
  let processes' = List.map process_to_ssa bmod.processes in
  { bmod with processes = processes' }

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
