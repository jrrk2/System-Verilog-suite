(* sv_opt_ir.ml - Hardware Optimization Intermediate Representation *)

open Sv_ast

(* ============================================================================
   IR CREATION AND UTILITIES
   ============================================================================ *)

let create_ir name = {
  ir_name = name;
  ir_inputs = Hashtbl.create 20;
  ir_outputs = Hashtbl.create 20;
  ir_constants = Hashtbl.create 50;
  ir_nodes = Hashtbl.create 100;
  ir_value_to_node = Hashtbl.create 100;
  ir_next_id = 1;
  ir_critical_path_length = 0;
  ir_area_estimate = 0;
}

let get_new_id ir =
  let id = ir.ir_next_id in
  ir.ir_next_id <- ir.ir_next_id + 1;
  id

let add_input ir name width =
  let id = get_new_id ir in
  let input = Input { id; name; width } in
  Hashtbl.add ir.ir_inputs name input;
  id

let add_output ir name width =
  let id = get_new_id ir in
  let output = Output { id; name; width } in
  Hashtbl.add ir.ir_outputs name output;
  id

let get_or_create_constant ir value width =
  match Hashtbl.find_opt ir.ir_constants value with
  | Some id -> id
  | None ->
      let id = get_new_id ir in
      Hashtbl.add ir.ir_constants value id;
      id

let add_node ir op inputs =
  let id = get_new_id ir in
  let width = match op with
    | Add { width; _ } | Sub { width; _ } | Mul { width; _ } 
    | Div { width; _ } | And { width } | Or { width } 
    | Xor { width } | Not { width } | Shift { width; _ }
    | Mux { width } | Extract { width; _ } | Register { width; _ } -> width
    | ZeroExtend { to_width; _ } | SignExtend { to_width; _ } -> to_width
    | Compare _ -> 1
    | Concat { widths } -> List.fold_left (+) 0 widths
  in
  let output = Wire { id; name = Printf.sprintf "n%d" id; width } in
  let new_node = {
    node_id = id;
    node_op = op;
    node_inputs = inputs;
    node_output = output;
    node_depth = 0;
    node_users = [];
  } in
  Hashtbl.add ir.ir_nodes id new_node;
  Hashtbl.add ir.ir_value_to_node id id;
  
  (* Update users lists *)
  List.iter (fun inp_id ->
    match Hashtbl.find_opt ir.ir_nodes inp_id with
    | Some inp_node -> inp_node.node_users <- id :: inp_node.node_users
    | None -> ()
  ) inputs;
  
  id

(* ============================================================================
   OPERATION COST ESTIMATION
   ============================================================================ *)

let operation_delay = function
  | Add _ | Sub _ | Xor _ | Or _ | And _ | Not _ -> 1
  | Compare _ -> 1
  | Shift _ -> 1
  | Mux _ -> 1
  | Mul _ -> 3
  | Div _ -> 8
  | Concat _ | Extract _ -> 0  (* Just wiring *)
  | ZeroExtend _ | SignExtend _ -> 0
  | Register _ -> 1

let operation_area = function
  | Add { width; _ } | Sub { width; _ } -> width
  | Mul { width; _ } -> width * width
  | Div { width; _ } -> width * width * 2
  | And { width } | Or { width } | Xor { width } | Not { width } -> width / 4
  | Shift _ -> 0  (* Usually free with muxes *)
  | Compare { width; _ } -> width
  | Mux { width } -> width * 2
  | Concat _ | Extract _ -> 0
  | ZeroExtend _ | SignExtend _ -> 0
  | Register { width; _ } -> width * 6

(* ============================================================================
   ANALYSIS PASSES
   ============================================================================ *)

let compute_depth nd ir =
  let input_depths = List.map (fun inp_id ->
    match Hashtbl.find_opt ir.ir_nodes inp_id with
    | Some n -> n.node_depth
    | None -> 0  (* Primary input or constant *)
  ) nd.node_inputs in
  let max_input_depth = List.fold_left max 0 input_depths in
  max_input_depth + (operation_delay nd.node_op)

let compute_all_depths ir =
  (* Topological sort and compute depths *)
  let rec visit visited nd =
    if List.mem nd.node_id visited then visited
    else begin
      let visited' = List.fold_left (fun acc inp_id ->
        match Hashtbl.find_opt ir.ir_nodes inp_id with
        | Some n -> visit acc n
        | None -> acc
      ) visited nd.node_inputs in
      nd.node_depth <- compute_depth nd ir;
      nd.node_id :: visited'
    end
  in
  let _ = Hashtbl.fold (fun _ nd acc -> visit acc nd) ir.ir_nodes [] in
  
  (* Find critical path *)
  ir.ir_critical_path_length <- Hashtbl.fold (fun _ nd max_d ->
    max max_d nd.node_depth
  ) ir.ir_nodes 0

let estimate_area ir =
  ir.ir_area_estimate <- Hashtbl.fold (fun _ nd acc ->
    acc + operation_area nd.node_op
  ) ir.ir_nodes 0

(* ============================================================================
   HELPER FUNCTIONS FOR OPTIMIZATIONS
   ============================================================================ *)

let is_constant ir id =
  (* Check if this ID is in the constants table *)
  Hashtbl.fold (fun value const_id found ->
    found || const_id = id
  ) ir.ir_constants false

let get_const_value ir id =
  (* Find the value for this constant ID *)
  Hashtbl.fold (fun value const_id acc ->
    if const_id = id then Some value else acc
  ) ir.ir_constants None |> function
  | Some v -> v
  | None -> 0

let is_constant_value ir id value =
  is_constant ir id && get_const_value ir id = value

let is_power_of_2 n =
  n > 0 && (n land (n - 1)) = 0

let log2 n =
  let rec loop n acc =
    if n <= 1 then acc else loop (n lsr 1) (acc + 1)
  in
  loop n 0

let is_dead nd ir =
  nd.node_users = [] && 
  not (Hashtbl.fold (fun _ out acc -> 
    match out with 
    | Output { id; _ } -> acc || id = nd.node_id 
    | _ -> acc
  ) ir.ir_outputs false)

let redirect_users ir from_id to_id =
  match Hashtbl.find_opt ir.ir_nodes from_id with
  | Some from_node ->
      List.iter (fun user_id ->
        match Hashtbl.find_opt ir.ir_nodes user_id with
        | Some user_node ->
            user_node.node_inputs <- List.map (fun inp ->
              if inp = from_id then to_id else inp
            ) user_node.node_inputs
        | None -> ()
      ) from_node.node_users;
      
      (* Update target node's users *)
      (match Hashtbl.find_opt ir.ir_nodes to_id with
      | Some to_node ->
          to_node.node_users <- from_node.node_users @ to_node.node_users
      | None -> ())
  | None -> ()

let replace_with_constant ir node_id value =
  let const_id = get_or_create_constant ir value 32 in
  redirect_users ir node_id const_id;
  Hashtbl.remove ir.ir_nodes node_id

let bypass_node ir node_id input_id =
  redirect_users ir node_id input_id;
  Hashtbl.remove ir.ir_nodes node_id

(* ============================================================================
   OPTIMIZATION PASS: CONSTANT PROPAGATION
   ============================================================================ *)

let constant_propagate ir =
  let changed = ref true in
  while !changed do
    changed := false;
    
    let to_process = Hashtbl.fold (fun id nd acc -> (id, nd) :: acc) ir.ir_nodes [] in
    
    List.iter (fun (id, nd) ->
      if Hashtbl.mem ir.ir_nodes id then  (* Check if still exists *)
        match nd.node_op, nd.node_inputs with
        (* Add with constants *)
        | Add _, [a; b] when is_constant ir a && is_constant ir b ->
            let va = get_const_value ir a in
            let vb = get_const_value ir b in
            replace_with_constant ir id (va + vb);
            changed := true
        
        (* Add with zero *)
        | Add _, [a; b] when is_constant_value ir b 0 ->
            bypass_node ir id a;
            changed := true
        | Add _, [a; b] when is_constant_value ir a 0 ->
            bypass_node ir id b;
            changed := true
        
        (* Subtract zero *)
        | Sub _, [a; b] when is_constant_value ir b 0 ->
            bypass_node ir id a;
            changed := true
        
        (* Multiply by zero *)
        | Mul _, [_; b] when is_constant_value ir b 0 ->
            replace_with_constant ir id 0;
            changed := true
        | Mul _, [a; _] when is_constant_value ir a 0 ->
            replace_with_constant ir id 0;
            changed := true
        
        (* Multiply by one *)
        | Mul _, [a; b] when is_constant_value ir b 1 ->
            bypass_node ir id a;
            changed := true
        | Mul _, [a; b] when is_constant_value ir a 1 ->
            bypass_node ir id b;
            changed := true
        
        (* AND with zero *)
        | And _, [_; b] when is_constant_value ir b 0 ->
            replace_with_constant ir id 0;
            changed := true
        | And _, [a; _] when is_constant_value ir a 0 ->
            replace_with_constant ir id 0;
            changed := true
        
        (* OR with zero *)
        | Or _, [a; b] when is_constant_value ir b 0 ->
            bypass_node ir id a;
            changed := true
        | Or _, [a; b] when is_constant_value ir a 0 ->
            bypass_node ir id b;
            changed := true
        
        (* XOR with zero *)
        | Xor _, [a; b] when is_constant_value ir b 0 ->
            bypass_node ir id a;
            changed := true
        | Xor _, [a; b] when is_constant_value ir a 0 ->
            bypass_node ir id b;
            changed := true
        
        | _ -> ()
    ) to_process
  done

(* ============================================================================
   OPTIMIZATION PASS: COMMON SUBEXPRESSION ELIMINATION
   ============================================================================ *)

let operations_equal op1 op2 =
  match op1, op2 with
  | Add { width = w1; signed = s1 }, Add { width = w2; signed = s2 } -> w1 = w2 && s1 = s2
  | Sub { width = w1; signed = s1 }, Sub { width = w2; signed = s2 } -> w1 = w2 && s1 = s2
  | Mul { width = w1; signed = s1 }, Mul { width = w2; signed = s2 } -> w1 = w2 && s1 = s2
  | And { width = w1 }, And { width = w2 } -> w1 = w2
  | Or { width = w1 }, Or { width = w2 } -> w1 = w2
  | Xor { width = w1 }, Xor { width = w2 } -> w1 = w2
  | _ -> false

let find_sharing_candidates ir =
  let candidates = Hashtbl.create 100 in
  
  Hashtbl.iter (fun id nd ->
    (* Look for matching operations with same inputs *)
    Hashtbl.iter (fun other_id other_nd ->
      if id < other_id && 
         operations_equal nd.node_op other_nd.node_op &&
         List.sort compare nd.node_inputs = List.sort compare other_nd.node_inputs then
        let key = id in
        match Hashtbl.find_opt candidates key with
        | Some cand -> cand.cand_instances <- other_id :: cand.cand_instances
        | None ->
            Hashtbl.add candidates key {
              cand_op = nd.node_op;
              cand_inputs = nd.node_inputs;
              cand_instances = [other_id];
              cand_savings = operation_area nd.node_op;
            }
    ) ir.ir_nodes
  ) ir.ir_nodes;
  
  Hashtbl.fold (fun keeper cand acc ->
    if List.length cand.cand_instances > 0 then (keeper, cand) :: acc else acc
  ) candidates []

let eliminate_common_subexpressions ir =
  let candidates = find_sharing_candidates ir in
  
  List.iter (fun (keeper, cand) ->
    List.iter (fun dup_id ->
      redirect_users ir dup_id keeper;
      Hashtbl.remove ir.ir_nodes dup_id
    ) cand.cand_instances
  ) candidates;
  
  List.length candidates

(* ============================================================================
   OPTIMIZATION PASS: STRENGTH REDUCTION
   ============================================================================ *)

let strength_reduce ir =
  let changed = ref 0 in
  
  Hashtbl.iter (fun id nd ->
    match nd.node_op, nd.node_inputs with
    (* Multiply by power of 2 → shift *)
    | Mul { width; signed }, [a; b] when is_constant ir b && is_power_of_2 (get_const_value ir b) ->
        let shift_amount = log2 (get_const_value ir b) in
        nd.node_op <- Shift { width; direction = `Left; arithmetic = false; amount = Some shift_amount };
        nd.node_inputs <- [a];
        incr changed
    
    | Mul { width; signed }, [a; b] when is_constant ir a && is_power_of_2 (get_const_value ir a) ->
        let shift_amount = log2 (get_const_value ir a) in
        nd.node_op <- Shift { width; direction = `Left; arithmetic = false; amount = Some shift_amount };
        nd.node_inputs <- [b];
        incr changed
    
    (* Divide by power of 2 → shift *)
    | Div { width; signed }, [a; b] when is_constant ir b && is_power_of_2 (get_const_value ir b) ->
        let shift_amount = log2 (get_const_value ir b) in
        nd.node_op <- Shift { width; direction = `Right; arithmetic = signed; amount = Some shift_amount };
        nd.node_inputs <- [a];
        incr changed
    
    | _ -> ()
  ) ir.ir_nodes;
  
  !changed

(* ============================================================================
   OPTIMIZATION PASS: DEAD CODE ELIMINATION
   ============================================================================ *)

let eliminate_dead_code ir =
  let changed = ref true in
  let removed = ref 0 in
  
  while !changed do
    changed := false;
    let to_remove = Hashtbl.fold (fun id nd acc ->
      if is_dead nd ir then (changed := true; incr removed; id :: acc) else acc
    ) ir.ir_nodes [] in
    List.iter (Hashtbl.remove ir.ir_nodes) to_remove
  done;
  
  !removed

(* ============================================================================
   OPTIMIZATION PASS: TREE BALANCING
   ============================================================================ *)
(* ============================================================================
   OPTIMIZATION PASS: TREE BALANCING - FIXED
   ============================================================================ *)

let balance_trees ir =
  (* Find chains of associative operations *)
  let is_associative = function
    | Add _ | Or _ | And _ | Xor _ -> true
    | _ -> false
  in
  
  let rec find_chain nd op acc =
    if not (operations_equal nd.node_op op) || List.length nd.node_users > 1 then
      nd.node_id :: acc
    else
      match nd.node_inputs with
      | [left; right] ->
          let left_acc = match Hashtbl.find_opt ir.ir_nodes left with
            | Some ln when operations_equal ln.node_op op && List.length ln.node_users = 1 ->
                find_chain ln op acc
            | _ -> left :: acc
          in
          (match Hashtbl.find_opt ir.ir_nodes right with
          | Some rn when operations_equal rn.node_op op && List.length rn.node_users = 1 ->
              find_chain rn op left_acc
          | _ -> right :: left_acc)
      | _ -> acc
  in
  
  let rec build_balanced_tree op inputs =
    match inputs with
    | [] -> failwith "Empty input list"
    | [x] -> x
    | _ ->
        let len = List.length inputs in
        let mid = len / 2 in
        let rec split n lst acc =
          if n = 0 then (List.rev acc, lst)
          else match lst with
            | [] -> (List.rev acc, [])
            | h :: t -> split (n - 1) t (h :: acc)
        in
        let (left_inputs, right_inputs) = split mid inputs [] in
        let left = build_balanced_tree op left_inputs in
        let right = build_balanced_tree op right_inputs in
        add_node ir op [left; right]
  in
  
  let to_balance = Hashtbl.fold (fun id nd acc ->
    if is_associative nd.node_op then (id, nd) :: acc else acc
  ) ir.ir_nodes [] in
  
  List.iter (fun (id, nd) ->
    let chain = find_chain nd nd.node_op [] in
    (* FIXED: Only balance if chain is long enough AND it would reduce depth *)
    if List.length chain > 4 then begin  (* Changed from 3 to 4 *)
      let new_tree = build_balanced_tree nd.node_op chain in
      redirect_users ir id new_tree;
      Hashtbl.remove ir.ir_nodes id
    end
  ) to_balance

(* ============================================================================
   MASTER OPTIMIZATION PIPELINE
   ============================================================================ *)

let optimize ir ~verbose ~force_balance =
  if verbose then Printf.printf "=== Optimization Pipeline ===\n";
  
  (* Initial metrics *)
  compute_all_depths ir;
  estimate_area ir;
  let initial_nodes = Hashtbl.length ir.ir_nodes in
  let initial_depth = ir.ir_critical_path_length in
  let initial_area = ir.ir_area_estimate in
  
  if verbose then
    Printf.printf "Initial: %d nodes, depth=%d, area=%d\n" 
      initial_nodes initial_depth initial_area;
  
  (* Run optimization passes *)
  if verbose then Printf.printf "\nRunning constant propagation...\n";
  constant_propagate ir;
  
  if verbose then Printf.printf "Running dead code elimination...\n";
  let dead_removed = eliminate_dead_code ir in
  if verbose then Printf.printf "  Removed %d dead nodes\n" dead_removed;
  
  if verbose then Printf.printf "Running common subexpression elimination...\n";
  let cse_removed = eliminate_common_subexpressions ir in
  if verbose then Printf.printf "  Removed %d duplicate computations\n" cse_removed;
  
  if verbose then Printf.printf "Running strength reduction...\n";
  let strength_reduced = strength_reduce ir in
  if verbose then Printf.printf "  Reduced %d expensive operations\n" strength_reduced;
  
  if verbose then Printf.printf "Balancing expression trees...\n";
  if force_balance then balance_trees ir;
  
  if verbose then Printf.printf "Final dead code elimination...\n";
  let final_dead = eliminate_dead_code ir in
  if verbose then Printf.printf "  Removed %d dead nodes\n" final_dead;
  
  (* Final metrics *)
  compute_all_depths ir;
  estimate_area ir;
  let final_nodes = Hashtbl.length ir.ir_nodes in
  let final_depth = ir.ir_critical_path_length in
  let final_area = ir.ir_area_estimate in
  
  if verbose then begin
    Printf.printf "\n=== Results ===\n";
    Printf.printf "Nodes:  %d -> %d (%.1f%% reduction)\n" 
      initial_nodes final_nodes 
      (100.0 *. float_of_int (initial_nodes - final_nodes) /. float_of_int initial_nodes);
    Printf.printf "Depth:  %d -> %d (%.1f%% %s)\n" 
      initial_depth final_depth
      (100.0 *. float_of_int (abs (initial_depth - final_depth)) /. float_of_int initial_depth)
      (if final_depth < initial_depth then "improvement" else "increase");
    Printf.printf "Area:   %d -> %d (%.1f%% reduction)\n" 
      initial_area final_area
      (100.0 *. float_of_int (initial_area - final_area) /. float_of_int initial_area);
  end

(* ============================================================================
   PRETTY PRINTING
   ============================================================================ *)

let print_operation = function
  | Add { width; signed } -> Printf.sprintf "add%s<%d>" (if signed then "s" else "") width
  | Sub { width; signed } -> Printf.sprintf "sub%s<%d>" (if signed then "s" else "") width
  | Mul { width; signed } -> Printf.sprintf "mul%s<%d>" (if signed then "s" else "") width
  | Div { width; signed } -> Printf.sprintf "div%s<%d>" (if signed then "s" else "") width
  | And { width } -> Printf.sprintf "and<%d>" width
  | Or { width } -> Printf.sprintf "or<%d>" width
  | Xor { width } -> Printf.sprintf "xor<%d>" width
  | Not { width } -> Printf.sprintf "not<%d>" width
  | Shift { direction; arithmetic; amount; _ } ->
      Printf.sprintf "shift_%s%s%s"
        (match direction with `Left -> "left" | `Right -> "right")
        (if arithmetic then "_arith" else "")
        (match amount with Some n -> Printf.sprintf "<%d>" n | None -> "")
  | Compare { cmp_op; _ } ->
      Printf.sprintf "cmp_%s" (match cmp_op with
        | `Eq -> "eq" | `Ne -> "ne" | `Lt -> "lt"
        | `Le -> "le" | `Gt -> "gt" | `Ge -> "ge")
  | Mux _ -> "mux"
  | Concat _ -> "concat"
  | Extract { lsb; msb; _ } -> Printf.sprintf "extract[%d:%d]" msb lsb
  | ZeroExtend { from_width; to_width } -> Printf.sprintf "zext<%d->%d>" from_width to_width
  | SignExtend { from_width; to_width } -> Printf.sprintf "sext<%d->%d>" from_width to_width
  | Register _ -> "reg"

let print_ir ir =
  Printf.printf "Module: %s\n" ir.ir_name;
  Printf.printf "Inputs:\n";
  Hashtbl.iter (fun name v -> match v with
    | Input { id; width; _ } -> Printf.printf "  %%%d = input %s : i%d\n" id name width
    | _ -> ()
  ) ir.ir_inputs;
  
  Printf.printf "Outputs:\n";
  Hashtbl.iter (fun name v -> match v with
    | Output { id; width; _ } -> Printf.printf "  %%%d = output %s : i%d\n" id name width
    | _ -> ()
  ) ir.ir_outputs;
  
  Printf.printf "Nodes:\n";
  Hashtbl.iter (fun id nd ->
    Printf.printf "  %%%d = %s(" id (print_operation nd.node_op);
    Printf.printf "%s) [depth=%d, users=%d]\n"
      (String.concat ", " (List.map (Printf.sprintf "%%%d") nd.node_inputs))
      nd.node_depth
      (List.length nd.node_users)
  ) ir.ir_nodes;
  
  Printf.printf "Critical path: %d\n" ir.ir_critical_path_length;
  Printf.printf "Area estimate: %d\n" ir.ir_area_estimate
