(* ============================================================================
   sv_transform.ml - Transformation pass to convert non-synth to synth
   ============================================================================ *)

open Sv_ast

(* Configuration *)
let max_unroll_iterations = 1024
let debug = ref false

(* Statistics *)
type transform_stats = {
  mutable loops_unrolled: int;
  mutable functions_inlined: int;
  mutable tasks_inlined: int;
}

let stats = {
  loops_unrolled = 0;
  functions_inlined = 0;
  tasks_inlined = 0;
}

(* ============================================================================
   SSA CONVERSION
   ============================================================================ *)

type ssa_context = {
  versions: (string, int) Hashtbl.t;
  new_vars: sv_node list ref;  (* Collect new variable declarations *)
}

let create_ssa_context () = {
  versions = Hashtbl.create 50;
  new_vars = ref [];
}

let get_version ctx var_name =
  try Hashtbl.find ctx.versions var_name
  with Not_found -> 0

let new_version ctx var_name dtype_ref =
  let current = get_version ctx var_name in
  let next = current + 1 in
  Hashtbl.replace ctx.versions var_name next;
  let new_name = Printf.sprintf "%s_%d" var_name next in
  
  (* Add variable declaration with CORRECT dtype *)
  (* If dtype_ref is ArrayType, it represents the signal's width, not an unpacked array *)
  let corrected_dtype = match dtype_ref with
    | Some (ArrayType { base; range }) ->
        (* This is actually a packed vector like logic [3:0] *)
        Some (BasicType { keyword = (match base with 
          | BasicType { keyword; _ } -> keyword 
          | _ -> "logic"); 
          range = Some range })
    | _ -> dtype_ref
  in
  
  let var_decl = Var {
    name = new_name;
    dtype_ref = corrected_dtype;
    var_type = "VAR";
    direction = "NONE";
    value = None;
    dtype_name = "";
    is_param = false;
  } in
  ctx.new_vars := var_decl :: !(ctx.new_vars);
  
  if !debug then
    Printf.eprintf "    SSA: %s -> %s\n" var_name new_name;
  
  new_name

let get_versioned_name ctx var_name =
  let version = get_version ctx var_name in
  if version = 0 then var_name
  else Printf.sprintf "%s_%d" var_name version

(* Flatten Begin blocks *)
let rec flatten_begins stmts =
  List.concat_map (function
    | Begin { stmts = inner; _ } -> flatten_begins inner
    | stmt -> [stmt]
  ) stmts

(* Detect multi-assigned variables *)
let rec find_multi_assigned stmts =
  let assignments = Hashtbl.create 20 in
  let rec count stmt =
    match stmt with
    | Assign { lhs = VarRef { name; _ }; is_blocking = true; _ } ->
        let c = try Hashtbl.find assignments name with Not_found -> 0 in
        Hashtbl.replace assignments name (c + 1)
    | Begin { stmts; _ } | Always { stmts; _ } ->
        List.iter count stmts
    | If { then_stmt; else_stmt; _ } ->
        count then_stmt;
        Option.iter count else_stmt
    | For { stmts; _ } ->
        List.iter count stmts
    | _ -> ()
  in
  List.iter count (flatten_begins stmts);
  Hashtbl.fold (fun k v acc -> if v > 1 then k :: acc else acc) assignments []

(* SSA expression *)
let rec ssa_expr ctx expr =
  match expr with
  | VarRef { name; access = "RD"; dtype_ref } ->
      VarRef { name = get_versioned_name ctx name; access = "RD"; dtype_ref }
  | VarRef _ as v -> v
  | BinaryOp { op; lhs; rhs; dtype_ref } ->
      BinaryOp { op; lhs = ssa_expr ctx lhs; rhs = ssa_expr ctx rhs; dtype_ref }
  | UnaryOp { op; operand; dtype_ref } ->
      UnaryOp { op; operand = ssa_expr ctx operand; dtype_ref }
  | Cond { condition; then_val; else_val } ->
      Cond {
        condition = ssa_expr ctx condition;
        then_val = ssa_expr ctx then_val;
        else_val = ssa_expr ctx else_val;
      }
  | Sel { expr; lsb; width; width_const; range } ->
      Sel {
        expr = ssa_expr ctx expr;
        lsb = Option.map (ssa_expr ctx) lsb;
        width = Option.map (ssa_expr ctx) width;
        width_const;
        range;
      }
  | ArraySel { expr; index } ->
      ArraySel { expr = ssa_expr ctx expr; index = ssa_expr ctx index }
  | Concat { parts } ->
      Concat { parts = List.map (ssa_expr ctx) parts }
  | Replicate { src; count; dtype_ref } ->
      Replicate { src = ssa_expr ctx src; count = ssa_expr ctx count; dtype_ref }
  | _ -> expr

(* SSA statement *)
let rec ssa_stmt ctx multi stmt =
  match stmt with
  | Assign { lhs = VarRef { name; access; dtype_ref }; rhs; is_blocking = true } ->
      let rhs_ssa = ssa_expr ctx rhs in
      if List.mem name multi then
        let new_name = new_version ctx name dtype_ref in
        Assign { lhs = VarRef { name = new_name; access; dtype_ref }; rhs = rhs_ssa; is_blocking = true }
      else
        Assign { lhs = VarRef { name; access; dtype_ref }; rhs = rhs_ssa; is_blocking = true }
  
  | Assign { lhs; rhs; is_blocking } ->
      Assign { lhs = ssa_expr ctx lhs; rhs = ssa_expr ctx rhs; is_blocking }
  
  | AssignW { lhs; rhs } ->
      AssignW { lhs = ssa_expr ctx lhs; rhs = ssa_expr ctx rhs }
  
  | Begin { name; stmts; is_generate } ->
      Begin { name; stmts = List.map (ssa_stmt ctx multi) stmts; is_generate }
  
  | If { condition; then_stmt; else_stmt } ->
      If {
        condition = ssa_expr ctx condition;
        then_stmt = ssa_stmt ctx multi then_stmt;
        else_stmt = Option.map (ssa_stmt ctx multi) else_stmt;
      }
  
  | _ -> stmt

(* Add final assignment back to original variable *)
let add_final_assignments ctx multi stmts =
  let finals = List.filter_map (fun var_name ->
    let version = get_version ctx var_name in
    if version > 0 then
      Some (Assign {
        lhs = VarRef { name = var_name; access = "WR"; dtype_ref = None };
        rhs = VarRef { name = Printf.sprintf "%s_%d" var_name version; access = "RD"; dtype_ref = None };
        is_blocking = true;
      })
    else
      None
  ) multi in
  stmts @ finals

(* Convert to SSA *)
let convert_to_ssa stmts =
  let flat = flatten_begins stmts in
  let multi = find_multi_assigned flat in
  
  if multi = [] then begin
    if !debug then Printf.eprintf "    No multi-assigned variables\n";
    (stmts, [])
  end else begin
    if !debug then
      Printf.eprintf "    Multi-assigned variables: %s\n" (String.concat ", " multi);
    
    let ctx = create_ssa_context () in
    let ssa_stmts = List.map (ssa_stmt ctx multi) flat in
    let final_stmts = add_final_assignments ctx multi ssa_stmts in
    
    if !debug then
      Printf.eprintf "    SSA complete: %d vars added\n" (List.length !(ctx.new_vars));
    
    (final_stmts, List.rev !(ctx.new_vars))
  end

(* ============================================================================
   LOOP UNROLLING
   ============================================================================ *)

let rec try_eval_const expr =
  match expr with
  | Const { name; _ } ->
      (try
        if String.contains name '\'' then
          let parts = String.split_on_char '\'' name in
          match parts with
          | width :: rest ->
              let value_str = String.concat "'" rest in
              if String.length value_str >= 2 then
                let result = match value_str.[0], value_str.[1] with
                  | 's', 'h' -> int_of_string ("0x" ^ String.sub value_str 2 (String.length value_str - 2))
                  | 'h', _ -> int_of_string ("0x" ^ String.sub value_str 1 (String.length value_str - 1))
                  | 's', 'd' -> int_of_string (String.sub value_str 2 (String.length value_str - 2))
                  | 'd', _ -> int_of_string (String.sub value_str 1 (String.length value_str - 1))
                  | _ -> int_of_string value_str
                in Some result
              else Some (int_of_string value_str)
          | _ -> Some (int_of_string name)
        else Some (int_of_string name)
      with _ -> None)
  | BinaryOp { op; lhs; rhs; dtype_ref } ->
      (match try_eval_const lhs, try_eval_const rhs with
      | Some l, Some r ->
          (match op with
          | "ADD" -> Some (l + r)
          | "SUB" -> Some (l - r)
          | "MUL" | "MULS" -> Some (l * r)
          | "DIV" | "DIVS" when r <> 0 -> Some (l / r)
          | _ -> None)
      | _ -> None)
  | _ -> None

let rec substitute_var var_name value expr =
  match expr with
  | VarRef { name; access; dtype_ref } when name = var_name ->
      Const { name = string_of_int value; dtype_ref = None }
  | BinaryOp { op; lhs; rhs; dtype_ref } ->
      BinaryOp { op; lhs = substitute_var var_name value lhs; rhs = substitute_var var_name value rhs; dtype_ref }
  | UnaryOp { op; operand; dtype_ref } ->
      UnaryOp { op; operand = substitute_var var_name value operand; dtype_ref }
  | Cond { condition; then_val; else_val } ->
      Cond {
        condition = substitute_var var_name value condition;
        then_val = substitute_var var_name value then_val;
        else_val = substitute_var var_name value else_val;
      }
  | Sel { expr; lsb; width; width_const; range } ->
      let substituted_expr = substitute_var var_name value expr in
      let substituted_lsb = Option.map (substitute_var var_name value) lsb in
      let substituted_width = Option.map (substitute_var var_name value) width in
      Sel { expr = substituted_expr; lsb = substituted_lsb; width = substituted_width; width_const; range }
  | ArraySel { expr; index } ->
      let substituted_index = substitute_var var_name value index in
      (* Simplify Sel nodes with lsb=0 *)
      let simplified_index = match substituted_index with
        | Sel { expr = index_expr; lsb = Some (Const { name; _ }); width = None; _ }
          when name = "0" || name = "32'h0" || name = "32'sh0" ->
            index_expr
        | _ -> substituted_index
      in
      ArraySel { expr = substitute_var var_name value expr; index = simplified_index }
  | Concat { parts } ->
      Concat { parts = List.map (substitute_var var_name value) parts }
  | Replicate { src; count; dtype_ref } ->
      Replicate { src = substitute_var var_name value src; count = substitute_var var_name value count; dtype_ref }
  | _ -> expr

let rec substitute_var_stmt var_name value stmt =
  match stmt with
  | Assign { lhs; rhs; is_blocking } ->
      Assign { lhs = substitute_var var_name value lhs; rhs = substitute_var var_name value rhs; is_blocking }
  | AssignW { lhs; rhs } ->
      AssignW { lhs = substitute_var var_name value lhs; rhs = substitute_var var_name value rhs }
  | If { condition; then_stmt; else_stmt } ->
      If {
        condition = substitute_var var_name value condition;
        then_stmt = substitute_var_stmt var_name value then_stmt;
        else_stmt = Option.map (substitute_var_stmt var_name value) else_stmt;
      }
  | Begin { name; stmts; is_generate } ->
      Begin { name; stmts = List.map (substitute_var_stmt var_name value) stmts; is_generate }
  | _ -> stmt

let unroll_for_loop for_node =
  match for_node with
  | For { name = loop_var; rhs; condition; stmts = body; incs; _ } ->
      if !debug then Printf.eprintf "  Analyzing for-loop: %s\n" loop_var;
      
      let start_val = try_eval_const rhs in
      
      let inc_amount = match incs with
        | [Assign { rhs = BinaryOp { op = "ADD"; lhs; rhs; dtype_ref }; _ }] ->
            (match try_eval_const lhs, try_eval_const rhs with
            | Some n, _ when n > 0 -> Some n
            | _, Some n when n > 0 -> Some n
            | _ -> None)
        | _ -> None
      in
      
      (* FIXED: Handle both directions of comparison *)
      let (bound_val, comparison_op) = match condition with
        | BinaryOp { op; lhs; rhs; dtype_ref } ->
            let lhs_c = try_eval_const lhs in
            let rhs_c = try_eval_const rhs in
            (match op, lhs_c, rhs_c with
            (* var < bound *)
            | "LTS", None, Some b -> (Some b, "LTS")
            | "LT", None, Some b -> (Some b, "LT")
            | "LTES", None, Some b -> (Some b, "LTES")
            | "LTE", None, Some b -> (Some b, "LTE")
            (* bound > var - SWAP to match var < bound pattern *)
            | "GTS", Some b, None -> (Some b, "LTS")
            | "GT", Some b, None -> (Some b, "LT")
            | "GTES", Some b, None -> (Some b, "LTES")
            | "GTE", Some b, None -> (Some b, "LTE")
            | _ -> (None, ""))
        | _ -> (None, "")
      in
      
      (match start_val, bound_val, inc_amount with
      | Some start, Some bound, Some inc when inc <> 0 ->
          (* Calculate iterations based on comparison operator *)
          let num_iters = match comparison_op with
            | "LTS" | "LT" -> (bound - start) / inc
            | "LTES" | "LTE" -> ((bound - start) / inc) + 1
            | _ -> 0
          in
          
          if num_iters > 0 && num_iters < max_unroll_iterations then begin
            if !debug then
              Printf.eprintf "  => Unrolling: %d iterations (start=%d, bound=%d, inc=%d)\n" 
                num_iters start bound inc;
            
            let rec gen_iter i remaining acc =
              if remaining <= 0 then acc
              else
                let iteration = List.map (substitute_var_stmt loop_var i) body in
                gen_iter (i + inc) (remaining - 1) (acc @ iteration)
            in
            
            let unrolled = gen_iter start num_iters [] in
            stats.loops_unrolled <- stats.loops_unrolled + 1;
            Some unrolled
          end else begin
            if !debug then
              Printf.eprintf "  => Cannot unroll: num_iters=%d (max=%d)\n" 
                num_iters max_unroll_iterations;
            None
          end
      | _ ->
          if !debug then 
            Printf.eprintf "  => Cannot unroll (non-constant bounds): start=%s, bound=%s, inc=%s\n"
              (match start_val with Some v -> string_of_int v | None -> "?")
              (match bound_val with Some v -> string_of_int v | None -> "?")
              (match inc_amount with Some v -> string_of_int v | None -> "?");
          None)
  | _ -> None
  
(* ============================================================================
   FUNCTION AND TASK INLINING
   ============================================================================ *)

type symbol_table = {
  functions: (string, sv_node) Hashtbl.t;
  tasks: (string, sv_node) Hashtbl.t;
}

let create_symbol_table () = {
  functions = Hashtbl.create 50;
  tasks = Hashtbl.create 50;
}

let rec collect_functions_tasks symtab stmts =
  List.iter (function
    | Func { name; _ } as f -> Hashtbl.replace symtab.functions name f
    | Task { name; _ } as t -> Hashtbl.replace symtab.tasks name t
    | Begin { stmts; _ } | Always { stmts; _ } -> collect_functions_tasks symtab stmts
    | _ -> ()
  ) stmts

(* ============================================================================
   TRANSFORMATION PASS
   ============================================================================ *)

let rec transform_stmt symtab stmt =
  match stmt with
  | Assign { lhs; rhs; is_blocking } ->
      Assign { lhs; rhs = transform_expr symtab rhs; is_blocking }
  
  | AssignW { lhs; rhs } ->
      AssignW { lhs; rhs = transform_expr symtab rhs }
  
  | Always { always; senses; stmts } ->
      if !debug then Printf.eprintf "  Processing Always block: %s\n" always;
      
      let transformed = List.map (transform_stmt symtab) stmts in
      let (ssa_stmts, ssa_vars) = convert_to_ssa transformed in
      
      (* Return Always block with SSA vars prepended *)
      Always { always; senses; stmts = ssa_vars @ ssa_stmts }
  
  | Begin { name; stmts; is_generate } ->
      Begin { name; stmts = List.map (transform_stmt symtab) stmts; is_generate }
  
  | For _ as for_node ->
      (match unroll_for_loop for_node with
      | Some unrolled -> Begin { name = ""; stmts = unrolled; is_generate = false }
      | None -> for_node)
  
  | If { condition; then_stmt; else_stmt } ->
      If {
        condition = transform_expr symtab condition;
        then_stmt = transform_stmt symtab then_stmt;
        else_stmt = Option.map (transform_stmt symtab) else_stmt;
      }
  
  | _ -> stmt

and transform_expr symtab expr =
  match expr with
  | BinaryOp { op; lhs; rhs; dtype_ref } ->
      BinaryOp { op; lhs = transform_expr symtab lhs; rhs = transform_expr symtab rhs; dtype_ref }
  | UnaryOp { op; operand; dtype_ref } ->
      UnaryOp { op; operand = transform_expr symtab operand; dtype_ref }
  | Cond { condition; then_val; else_val } ->
      Cond {
        condition = transform_expr symtab condition;
        then_val = transform_expr symtab then_val;
        else_val = transform_expr symtab else_val;
      }
  | Sel { expr; lsb; width; width_const; range } ->
      Sel {
        expr = transform_expr symtab expr;
        lsb = Option.map (transform_expr symtab) lsb;
        width = Option.map (transform_expr symtab) width;
        width_const;
        range;
      }
  | ArraySel { expr; index } ->
      ArraySel { expr = transform_expr symtab expr; index = transform_expr symtab index }
  | Concat { parts } ->
      Concat { parts = List.map (transform_expr symtab) parts }
  | Replicate { src; count; dtype_ref } ->
      Replicate { src = transform_expr symtab src; count = transform_expr symtab count; dtype_ref }
  | _ -> expr

let transform_module stmts =
  let symtab = create_symbol_table () in
  collect_functions_tasks symtab stmts;
  if !debug then
    Printf.eprintf "Found %d functions and %d tasks\n"
      (Hashtbl.length symtab.functions)
      (Hashtbl.length symtab.tasks);
  
  List.map (transform_stmt symtab) stmts
(* ============================================================================
   FUNCTION AND TASK INLINING - Add to sv_transform.ml
   ============================================================================ *)

(* Substitute variable in expression - you already have this *)
(* Just making sure it handles all cases *)

(* Recursively search for assignments to a variable in a single statement *)
let rec find_assignment_in_stmt target_name stmt =
  match stmt with
  | Assign { lhs = VarRef { name; _ }; rhs; _ } when name = target_name -> Some rhs
  | Case { items; _ } ->
      (* Search in each case item's statements *)
      List.find_map (fun item -> find_assignment_in_stmts target_name item.statements) items
  | If { then_stmt; else_stmt; _ } ->
      (* Search in then branch *)
      (match find_assignment_in_stmt target_name then_stmt with
      | Some expr -> Some expr
      | None ->
          match else_stmt with
          | Some e_stmt -> find_assignment_in_stmt target_name e_stmt
          | None -> None)
  | JumpBlock { stmt; _ } ->
      (* Search in jump block (used for return statements) *)
      find_assignment_in_stmts target_name stmt
  | Begin { stmts = inner_stmts; _ } ->
      (* Search in begin blocks *)
      find_assignment_in_stmts target_name inner_stmts
  | Always { stmts = inner_stmts; _ } ->
      (* Search in always blocks *)
      find_assignment_in_stmts target_name inner_stmts
  | _ -> None

(* Recursively search for assignments to a variable in a list of statements *)
and find_assignment_in_stmts target_name stmts =
  List.find_map (find_assignment_in_stmt target_name) stmts

(* Inline a function call by substituting parameters and returning the body *)
let rec inline_function symtab func_name args =
  match Hashtbl.find_opt symtab.functions func_name with
  | Some (Func { stmts; vars; _ }) ->
      if !debug then
        Printf.eprintf "  Inlining function: %s\n" func_name;

      (* Extract input parameters in order *)
      let params = List.filter_map (function
        | Var { name; var_type = "PORT"; direction = "INPUT"; _ } -> Some name
        | _ -> None
      ) stmts in

      (* Find the return value assignment (can be nested in case/if blocks) *)
      let return_expr = find_assignment_in_stmts func_name stmts in

      (match return_expr with
      | Some expr ->
          (* Substitute each parameter with its argument *)
          let substituted = List.fold_left2
            (fun acc_expr param arg ->
              (* First evaluate the argument expression *)
              let arg_expr = transform_expr symtab arg in
              (* Then substitute the parameter name with the argument *)
              substitute_var_expr param arg_expr acc_expr
            ) expr params args
          in
          stats.functions_inlined <- stats.functions_inlined + 1;
          Some substituted
      | None ->
          Printf.eprintf "Could not find return statement in function %s" func_name;
          None)
  | Some _ ->
      if !debug then
        Printf.eprintf "  Function %s symbol table incorrect, internal error\n" func_name;
      None
  | None ->
      if !debug then
        Printf.eprintf "  Function %s not found in symbol table\n" func_name;
      None

(* Substitute variable in expression - enhanced version *)
and substitute_var_expr var_name replacement expr =
  match expr with
  | VarRef { name; access; dtype_ref } when name = var_name ->
      replacement
  | VarRef _ as v -> v
  | Const _ as c -> c
  | BinaryOp { op; lhs; rhs; dtype_ref } ->
      BinaryOp { 
        op; 
        lhs = substitute_var_expr var_name replacement lhs; 
        rhs = substitute_var_expr var_name replacement rhs;
        dtype_ref 
      }
  | UnaryOp { op; operand; dtype_ref } ->
      UnaryOp { 
        op; 
        operand = substitute_var_expr var_name replacement operand;
        dtype_ref 
      }
  | Cond { condition; then_val; else_val } ->
      Cond {
        condition = substitute_var_expr var_name replacement condition;
        then_val = substitute_var_expr var_name replacement then_val;
        else_val = substitute_var_expr var_name replacement else_val;
      }
  | Sel { expr; lsb; width; width_const; range } ->
      Sel {
        expr = substitute_var_expr var_name replacement expr;
        lsb = Option.map (substitute_var_expr var_name replacement) lsb;
        width = Option.map (substitute_var_expr var_name replacement) width;
        width_const;
        range;
      }
  | ArraySel { expr; index } ->
      ArraySel {
        expr = substitute_var_expr var_name replacement expr;
        index = substitute_var_expr var_name replacement index;
      }
  | Concat { parts } ->
      Concat { parts = List.map (substitute_var_expr var_name replacement) parts }
  | Replicate { src; count; dtype_ref } ->
      Replicate {
        src = substitute_var_expr var_name replacement src;
        count = substitute_var_expr var_name replacement count;
        dtype_ref;
      }
  | other -> other

(* Inline a task call - tasks can have output parameters *)
let inline_task symtab task_name args =
  match Hashtbl.find_opt symtab.tasks task_name with
  | Some (Task { stmts; _ }) ->
      if !debug then
        Printf.eprintf "  Inlining task: %s\n" task_name;
      
      (* Extract parameters with their directions *)
      let params = List.filter_map (function
        | Var { name; var_type = "PORT"; direction; _ } -> Some (name, direction)
        | _ -> None
      ) stmts in
      
      (* Find assignments in the task body *)
      let task_assigns = List.filter_map (function
        | Assign { lhs; rhs; is_blocking } -> Some (lhs, rhs, is_blocking)
        | _ -> None
      ) stmts in
      
      (* Create substitution: for each input param, substitute with arg *)
      let param_map = List.combine params args in
      
      (* Generate new assignments with substituted values *)
      let new_stmts = List.map (fun (lhs, rhs, is_blocking) ->
        (* Substitute input parameters in RHS *)
        let substituted_rhs = List.fold_left 
          (fun acc_expr ((param_name, direction), arg) ->
            if direction = "INPUT" then
              substitute_var_expr param_name arg acc_expr
            else
              acc_expr
          ) rhs param_map
        in
        
        (* For output parameters, substitute the LHS with the output argument *)
        let substituted_lhs = match lhs with
          | VarRef { name; _ } ->
              (match List.find_opt (fun ((pname, dir), _) -> pname = name && dir = "OUTPUT") param_map with
              | Some (_, out_arg) -> out_arg
              | None -> lhs)
          | _ -> lhs
        in
        
        Assign { lhs = substituted_lhs; rhs = substituted_rhs; is_blocking }
      ) task_assigns in
      
      stats.tasks_inlined <- stats.tasks_inlined + 1;
      Some new_stmts
  | Some _ ->
      if !debug then
        Printf.eprintf "  Task %s symbol table incorrect, internal error\n" task_name;
      None
  | None ->
      if !debug then
        Printf.eprintf "  Task %s not found in symbol table\n" task_name;
      None

(* Update transform_expr to inline function calls *)
let rec transform_expr symtab expr =
  match expr with
  | FuncRef { name; args } ->
      (match inline_function symtab name args with
      | Some inlined_expr -> transform_expr symtab inlined_expr
      | None -> 
          (* Keep as FuncRef if inlining failed *)
          FuncRef { name; args = List.map (transform_expr symtab) args })
  
  | BinaryOp { op; lhs; rhs; dtype_ref } ->
      BinaryOp { op; lhs = transform_expr symtab lhs; rhs = transform_expr symtab rhs; dtype_ref }
  
  | UnaryOp { op; operand; dtype_ref } ->
      UnaryOp { op; operand = transform_expr symtab operand; dtype_ref }
  
  | Cond { condition; then_val; else_val } ->
      Cond {
        condition = transform_expr symtab condition;
        then_val = transform_expr symtab then_val;
        else_val = transform_expr symtab else_val;
      }
  
  | Sel { expr; lsb; width; width_const; range } ->
      Sel {
        expr = transform_expr symtab expr;
        lsb = Option.map (transform_expr symtab) lsb;
        width = Option.map (transform_expr symtab) width;
        width_const;
        range;
      }
  
  | ArraySel { expr; index } ->
      ArraySel { expr = transform_expr symtab expr; index = transform_expr symtab index }
  
  | Concat { parts } ->
      Concat { parts = List.map (transform_expr symtab) parts }
  
  | Replicate { src; count; dtype_ref } ->
      Replicate { src = transform_expr symtab src; count = transform_expr symtab count; dtype_ref }
  
  | _ -> expr

(* Update transform_stmt to inline task calls *)
let rec transform_stmt symtab stmt =
  match stmt with
  | StmtExpr { expr = TaskRef { name; args } } ->
      (match inline_task symtab name args with
      | Some inlined_stmts -> Begin { name = ""; stmts = inlined_stmts; is_generate = false }
      | None -> StmtExpr { expr = TaskRef { name; args } })
  
  | Assign { lhs; rhs; is_blocking } ->
      Assign { lhs; rhs = transform_expr symtab rhs; is_blocking }
  
  | AssignW { lhs; rhs } ->
      AssignW { lhs; rhs = transform_expr symtab rhs }
  
  | Initial { suspend; stmts } ->
      Initial { suspend; stmts = List.map (transform_stmt symtab) stmts }
  
  | Always { always; senses; stmts } ->
      if !debug then Printf.eprintf "  Processing Always block: %s\n" always;
      
      let transformed = List.map (transform_stmt symtab) stmts in
      let (ssa_stmts, ssa_vars) = convert_to_ssa transformed in
      
      Always { always; senses; stmts = ssa_vars @ ssa_stmts }
  
  | Begin { name; stmts; is_generate } ->
      Begin { name; stmts = List.map (transform_stmt symtab) stmts; is_generate }
  
  | For _ as for_node ->
      (match unroll_for_loop for_node with
      | Some unrolled -> Begin { name = ""; stmts = unrolled; is_generate = false }
      | None -> for_node)
  
  | If { condition; then_stmt; else_stmt } ->
      If {
        condition = transform_expr symtab condition;
        then_stmt = transform_stmt symtab then_stmt;
        else_stmt = Option.map (transform_stmt symtab) else_stmt;
      }
  
  | _ -> stmt

(* Update collect_functions_tasks to handle packages *)
let rec collect_functions_tasks symtab node =
  match node with
  | Func { name; _ } as f -> 
      Hashtbl.replace symtab.functions name f
  | Task { name; _ } as t -> 
      Hashtbl.replace symtab.tasks name t
  | Package { stmts; _ } ->
      List.iter (collect_functions_tasks symtab) stmts
  | Module { stmts; _ } ->
      List.iter (collect_functions_tasks symtab) stmts
  | Begin { stmts; _ } | Always { stmts; _ } | Initial { stmts; _ } ->
      List.iter (collect_functions_tasks symtab) stmts
  | _ -> ()

(* Update transform_ast to collect from packages *)
let rec transform_ast node =
  match node with
  | Netlist modules -> 
      (* First pass: collect all functions and tasks from packages *)
      let symtab = create_symbol_table () in
      List.iter (collect_functions_tasks symtab) modules;
      
      if !debug then
        Printf.eprintf "Found %d functions and %d tasks across all modules/packages\n"
          (Hashtbl.length symtab.functions)
          (Hashtbl.length symtab.tasks);
      
      (* Second pass: transform modules *)
      Netlist (List.map (transform_with_symtab symtab) modules)
  
  | _ -> node

and transform_with_symtab symtab node =
  match node with
  | Module { name; stmts } ->
      if !debug then Printf.eprintf "Transforming module: %s\n" name;
      Module { name; stmts = List.map (transform_stmt symtab) stmts }
  | Package _ as p -> p  (* Keep packages as-is *)
  | _ -> node

let transform ?(verbose=false) ast =
  debug := verbose;
  stats.loops_unrolled <- 0;
  stats.functions_inlined <- 0;
  stats.tasks_inlined <- 0;
  
  let transformed = transform_ast ast in
  
  if verbose then begin
    Printf.printf "Transformation Statistics:\n";
    Printf.printf "  Loops unrolled: %d\n" stats.loops_unrolled;
    Printf.printf "  Functions inlined: %d\n" stats.functions_inlined;
    Printf.printf "  Tasks inlined: %d\n" stats.tasks_inlined;
  end;
  
  transformed
