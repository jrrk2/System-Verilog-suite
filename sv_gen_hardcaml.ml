(* sv_gen_hardcaml.ml - HardCaml circuit generation from SystemVerilog AST
   
   This backend constructs HardCaml circuits directly using Signal/Always API,
   following the Input_hardcaml.ml pattern of direct circuit construction.
*)

open Hardcaml
open Signal
open Always

(* Remap types track actual HardCaml values *)
type remap =
  | Invalid
  | Con of Constant.t
  | Sig of Signal.t
  | Sigs of Signed.v
  | Var of Variable.t
  | Alw of Always.t

(* Extract width from range string like "7:0" or "15:0" *)
let width_of_range range_str =
  try
    match String.split_on_char ':' range_str with
    | [msb_str; lsb_str] ->
        let msb = int_of_string (String.trim msb_str) in
        let lsb = int_of_string (String.trim lsb_str) in
        msb - lsb + 1
    | _ -> 1
  with _ -> 1

(* Extract width from sv_type *)
let rec extract_width = function
  | Some (Sv_ast.BasicType { range = Some r; _ }) -> width_of_range r
  | Some (Sv_ast.ArrayType { range; _ }) -> width_of_range range
  | Some (Sv_ast.PackArrayType { range; _ }) -> width_of_range range
  | Some (Sv_ast.RefType { refdtype_ref = Some dtype; _ }) -> extract_width (Some dtype)
  | _ -> 1

(* Check if type is signed *)
let is_signed = function
  | Some (Sv_ast.BasicType { keyword = "int" | "integer" | "shortint" | "longint"; _ }) -> true
  | _ -> false

(* Extract ports from module statements *)
let extract_ports stmts =
  List.filter_map (function
    | Sv_ast.Var { name; dtype_ref; direction = "input"; _ } ->
        let width = extract_width dtype_ref in
        Some (name, width, `Input)
    | Sv_ast.Var { name; dtype_ref; direction = "output"; _ } ->
        let width = extract_width dtype_ref in
        Some (name, width, `Output)
    | Sv_ast.Var { name; dtype_ref; direction = "inout"; _ } ->
        let width = extract_width dtype_ref in
        Some (name, width, `Inout)
    | _ -> None
  ) stmts

(* Extract internal signal declarations *)
let extract_signals stmts =
  List.filter_map (function
    | Sv_ast.Var { name; dtype_ref; var_type = "WIRE" | "VAR" | "REG"; direction = ""; _ } ->
        let width = extract_width dtype_ref in
        let signed = is_signed dtype_ref in
        Some (name, width, signed)
    | _ -> None
  ) stmts

(* Helper functions *)
let sig' = function
  | Con x -> Signal.of_constant x
  | Sig x -> x
  | Sigs x -> Signed.to_signal x
  | Var x -> x.value
  | _ -> Signal.zero 1

let var' = function
  | Var x -> x
  | _ -> failwith "var': not a variable"

(* Width-aware operations *)
let width_match op lhs rhs =
  let wlhs = width lhs in
  let wrhs = width rhs in
  if wlhs = wrhs then
    op lhs rhs
  else if wlhs < wrhs then
    op (uresize lhs wrhs) rhs
  else
    op lhs (uresize rhs wlhs)

(* Convert expression to HardCaml Signal *)
let rec expr_to_remap decls = function
  | Sv_ast.VarRef { name; _ } | Sv_ast.VarRef' { name; _ } ->
      (try Hashtbl.find decls name
       with Not_found -> Invalid)
       
  | Sv_ast.Text { text } ->
      (* Try to parse as number *)
      (try
        let n = int_of_string text in
        Con (Constant.of_int ~width:32 n)
       with _ -> Invalid)
       
  | Sv_ast.Sel { expr; lsb = Some lsb_node; width = Some width_node; _ } ->
      let base = expr_to_remap decls expr in
      (try
        (* Extract bit range *)
        match (expr_to_remap decls lsb_node, expr_to_remap decls width_node) with
        | (Con lsb_const, Con width_const) ->
            let lsb = Constant.to_int lsb_const in
            let w = Constant.to_int width_const in
            Sig (select (sig' base) lsb w)
        | _ -> Invalid
       with _ -> Invalid)
       
  | Sv_ast.ArraySel { expr; index } ->
      let base = expr_to_remap decls expr in
      let idx = expr_to_remap decls index in
      (* Simplified - treat as bit select *)
      (try
        match idx with
        | Con c -> Sig (bit (sig' base) (Constant.to_int c))
        | _ -> Invalid
       with _ -> Invalid)
       
  | Sv_ast.BinaryOp { op; lhs; rhs; _ } | Sv_ast.BinaryOp' { op; lhs; rhs; _ } ->
      let lhs_sig = sig' (expr_to_remap decls lhs) in
      let rhs_sig = sig' (expr_to_remap decls rhs) in
      (match op with
       | "Add" | "VADD" -> Sig (width_match (+:) lhs_sig rhs_sig)
       | "Sub" | "VSUB" -> Sig (width_match (-:) lhs_sig rhs_sig)
       | "Mul" | "VMUL" -> Sig (width_match ( *: ) lhs_sig rhs_sig)
       | "And" | "VAND" -> Sig (width_match (&:) lhs_sig rhs_sig)
       | "Or" | "VOR" -> Sig (width_match (|:) lhs_sig rhs_sig)
       | "Xor" | "VXOR" -> Sig (width_match (^:) lhs_sig rhs_sig)
       | "Eq" | "VEQ" -> Sig (width_match (==:) lhs_sig rhs_sig)
       | "Neq" | "VNEQ" -> Sig (width_match (<>:) lhs_sig rhs_sig)
       | "Lt" | "VLT" -> Sig (width_match (<:) lhs_sig rhs_sig)
       | "Lte" | "VLTE" -> Sig (width_match (<=:) lhs_sig rhs_sig)
       | "Gt" | "VGT" -> Sig (width_match (>:) lhs_sig rhs_sig)
       | "Gte" | "VGTE" -> Sig (width_match (>=:) lhs_sig rhs_sig)
       | "ShiftL" | "VSHIFTL" -> Sig (log_shift sll lhs_sig rhs_sig)
       | "ShiftR" | "VSHIFTR" -> Sig (log_shift srl lhs_sig rhs_sig)
       | "ShiftRS" | "VSHIFTRS" -> Sig (log_shift sra lhs_sig rhs_sig)
       | _ -> Invalid)
       
  | Sv_ast.UnaryOp { op; operand; _ } | Sv_ast.UnaryOp' { op; operand; _ } ->
      let op_sig = sig' (expr_to_remap decls operand) in
      (match op with
       | "Not" | "VNOT" -> Sig (~: op_sig)
       | "Negate" | "VNEGATE" -> Sig (zero (width op_sig) -: op_sig)
       | "RedAnd" -> Sig (reduce ~f:(&:) [op_sig])
       | "RedOr" -> Sig (reduce ~f:(|:) [op_sig])
       | "RedXor" -> Sig (reduce ~f:(^:) [op_sig])
       | _ -> Invalid)
       
  | Sv_ast.Concat { parts } ->
      let sigs = List.filter_map (fun p ->
        match expr_to_remap decls p with
        | Sig s -> Some s
        | Con c -> Some (Signal.of_constant c)
        | _ -> None
      ) parts in
      if List.length sigs > 0 then
        Sig (concat_msb sigs)
      else
        Invalid
        
  | Sv_ast.Cond { condition; then_val; else_val } ->
      let cond_sig = sig' (expr_to_remap decls condition) in
      let then_sig = sig' (expr_to_remap decls then_val) in
      let else_sig = sig' (expr_to_remap decls else_val) in
      (* Ensure condition is single bit *)
      let cond_bit = if width cond_sig = 1 then cond_sig 
                     else reduce ~f:(|:) [cond_sig] in
      Sig (mux2 cond_bit then_sig else_sig)
      
  | Sv_ast.Replicate { count; src; _ } | Sv_ast.Replicate' { count; src; _ } ->
      (match expr_to_remap decls count with
       | Con c ->
           let n = Constant.to_int c in
           let src_sig = sig' (expr_to_remap decls src) in
           let replicated = List.init n (fun _ -> src_sig) in
           Sig (concat_msb replicated)
       | _ -> Invalid)
       
  | _ -> Invalid

(* Process assignment statement *)
let process_assign decls lhs rhs =
  match expr_to_remap decls lhs with
  | Var v ->
      let rhs_sig = sig' (expr_to_remap decls rhs) in
      let wlhs = width v.value in
      let wrhs = width rhs_sig in
      let rhs_resized = if wlhs = wrhs then rhs_sig
                        else if wlhs > wrhs then uresize rhs_sig wlhs
                        else select rhs_sig 0 wlhs in
      Some (v <-- rhs_resized)
  | _ -> None

(* Process statement to Always.t *)
let rec process_stmt decls = function
  | Sv_ast.Assign { lhs; rhs; is_blocking = true } ->
      process_assign decls lhs rhs
      
  | Sv_ast.Assign { lhs; rhs; is_blocking = false } ->
      (* Non-blocking - for registers *)
      (match expr_to_remap decls lhs with
       | Var v ->
           let rhs_sig = sig' (expr_to_remap decls rhs) in
           let wlhs = width v.value in
           let wrhs = width rhs_sig in
           let rhs_resized = if wlhs = wrhs then rhs_sig
                             else if wlhs > wrhs then uresize rhs_sig wlhs
                             else select rhs_sig 0 wlhs in
           Some (v <-- rhs_resized)
       | _ -> None)
       
  | Sv_ast.If { condition; then_stmt; else_stmt = Some else_stmt } ->
      let cond_raw = sig' (expr_to_remap decls condition) in
      let cond = if width cond_raw = 1 then cond_raw 
                 else reduce ~f:(|:) [cond_raw] in
      (match (process_stmt decls then_stmt, process_stmt decls else_stmt) with
       | (Some t, Some e) -> Some (if_ cond [t] [e])
       | (Some t, None) -> Some (when_ cond [t])
       | _ -> None)
       
  | Sv_ast.If { condition; then_stmt; else_stmt = None } ->
      let cond_raw = sig' (expr_to_remap decls condition) in
      let cond = if width cond_raw = 1 then cond_raw 
                 else reduce ~f:(|:) [cond_raw] in
      (match process_stmt decls then_stmt with
       | Some t -> Some (when_ cond [t])
       | None -> None)
       
  | Sv_ast.Begin { stmts; _ } ->
      let alws = List.filter_map (process_stmt decls) stmts in
      if List.length alws > 0 then
        Some (proc alws)
      else
        None
        
  | Sv_ast.Case { expr; items } ->
      let expr_sig = sig' (expr_to_remap decls expr) in
      let cases = List.filter_map (fun item ->
        match item.Sv_ast.conditions with
        | [cond] ->
            (match expr_to_remap decls cond with
             | Con c ->
                 let cond_sig = Signal.of_constant c in
                 let stmts = List.filter_map (process_stmt decls) item.statements in
                 if List.length stmts > 0 then
                   Some (cond_sig, stmts)
                 else
                   None
             | Sig s ->
                 let stmts = List.filter_map (process_stmt decls) item.statements in
                 if List.length stmts > 0 then
                   Some (s, stmts)
                 else
                   None
             | _ -> None)
        | _ -> None
      ) items in
      if List.length cases > 0 then
        Some (switch expr_sig cases)
      else
        None
        
  | _ -> None

(* Process always block *)
let process_always decls clock_opt reset_opt stmts =
  List.filter_map (process_stmt decls) stmts

(* Build HardCaml circuit for module *)
let build_circuit module_name stmts =
  try
    (* Extract ports and signals *)
    let ports = extract_ports stmts in
    let signals = extract_signals stmts in
    
    let decls = Hashtbl.create 64 in
    
    (* Create inputs *)
    List.iter (fun (name, width, dir) ->
      match dir with
      | `Input -> Hashtbl.add decls name (Sig (input name width))
      | _ -> ()
    ) ports;
    
    (* Create internal signals/registers *)
    List.iter (fun (name, width, _signed) ->
      if not (Hashtbl.mem decls name) then
        Hashtbl.add decls name (Var (Variable.wire ~default:(zero width)))
    ) signals;
    
    (* Process continuous assignments *)
    let cont_assigns = List.filter_map (function
      | Sv_ast.AssignW { lhs; rhs } -> Some (lhs, rhs)
      | _ -> None
    ) stmts in
    
    List.iter (fun (lhs, rhs) ->
      match process_assign decls lhs rhs with
      | Some _ -> () (* Assignment processed *)
      | None -> ()
    ) cont_assigns;
    
    (* Process always blocks *)
    let always_blocks = List.filter_map (function
      | Sv_ast.Always { senses; stmts; _ } ->
          (* Extract clock/reset from sensitivity list if present *)
          let alws = process_always decls None None stmts in
          if List.length alws > 0 then Some alws else None
      | _ -> None
    ) stmts in
    
    (* Compile all always blocks *)
    List.iter compile (List.filter (fun l -> List.length l > 0) always_blocks);
    
    (* Build outputs *)
    let outputs = List.filter_map (fun (name, width, dir) ->
      match dir with
      | `Output ->
          (match Hashtbl.find_opt decls name with
           | Some (Var v) -> Some (output name v.value)
           | Some (Sig s) -> Some (output name s)
           | _ -> 
               (* Create default output if not found *)
               Some (output name (zero width))
          )
      | _ -> None
    ) ports in
    
    if List.length outputs = 0 then
      (* Ensure at least one output for valid circuit *)
      Circuit.create_exn ~name:module_name [output "dummy" (zero 1)]
    else
      Circuit.create_exn ~name:module_name outputs
      
  with e ->
    (* On error, create minimal valid circuit *)
    Printf.eprintf "Warning: Circuit build failed for %s: %s\n" 
      module_name (Printexc.to_string e);
    Circuit.create_exn ~name:(module_name ^ "_error") [output "error" (zero 1)]

(* Generate Verilog from circuit *)
let circuit_to_verilog circuit =
  let buffer = Buffer.create 8192 in
  Rtl.output ~output_mode:(Rtl.Output_mode.To_buffer buffer) Verilog circuit;
  Buffer.contents buffer

(* Main entry point *)
let generate_hardcaml_with_warnings ast indent =
  let warnings = ref [] in
  let circuits = ref [] in
  print_endline "Processing nodes";

  (* Process each module *)
  let rec process_node = function
    | Sv_ast.Netlist nodes ->
        List.iter process_node nodes
    | Sv_ast.Module { name; stmts } ->
        let circuit = build_circuit name stmts in
        circuits := circuit :: !circuits
    | _ -> ()
  in
  
  process_node ast;
  
  let parts, warn = if List.length !circuits = 0 then begin
    warnings := "No modules found in AST" :: !warnings;
    ("(* No modules to generate *)\n", !warnings)
  end else begin
    let verilog_parts = List.map circuit_to_verilog !circuits in
    let header = "(* Generated via HardCaml circuit construction *)\n" ^
                 "(* Using Signal/Always API directly *)\n\n" in
    (header ^ String.concat "\n\n" verilog_parts, !warnings)
  end in
  List.iter print_endline warn;
  parts, warn
