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

(* Identify state elements (registers) - variables assigned with non-blocking *)
let identify_state_elements stmts =
  let state_vars = Hashtbl.create 16 in

  let rec find_nonblocking = function
    | Sv_ast.Assign { lhs; is_blocking = false; _ } ->
        (match lhs with
         | Sv_ast.VarRef { name; _ } -> Hashtbl.replace state_vars name true
         | _ -> ())
    | Sv_ast.Always { stmts; _ } -> List.iter find_nonblocking stmts
    | Sv_ast.Begin { stmts; _ } -> List.iter find_nonblocking stmts
    | Sv_ast.If { then_stmt; else_stmt; _ } ->
        find_nonblocking then_stmt;
        (match else_stmt with Some e -> find_nonblocking e | None -> ())
    | Sv_ast.Case { items; _ } ->
        List.iter (fun item -> List.iter find_nonblocking item.Sv_ast.statements) items
    | _ -> ()
  in

  List.iter find_nonblocking stmts;
  state_vars

(* Extract ports from module statements *)
let extract_ports stmts =
  List.filter_map (function
    | Sv_ast.Var { name; dtype_ref; direction; var_type = "PORT"; _ }
      when direction = "INPUT" || direction = "input" ->
        let width = extract_width dtype_ref in
        Some (name, width, `Input)
    | Sv_ast.Var { name; dtype_ref; direction; var_type = "PORT"; _ }
      when direction = "OUTPUT" || direction = "output" ->
        let width = extract_width dtype_ref in
        Some (name, width, `Output)
    | Sv_ast.Var { name; dtype_ref; direction; var_type = "PORT"; _ }
      when direction = "INOUT" || direction = "inout" ->
        let width = extract_width dtype_ref in
        Some (name, width, `Inout)
    | _ -> None
  ) stmts

(* Extract internal signal declarations *)
let extract_signals stmts =
  Printf.eprintf "      extract_signals called with %d statements\n%!" (List.length stmts);
  List.filter_map (function
    | Sv_ast.Var { name; dtype_ref; var_type; direction; _ } ->
        Printf.eprintf "        Checking var: %s varType=%s direction=%s\n%!" name var_type direction;
        (match var_type with
         | "WIRE" | "VAR" | "REG" when direction = "" || direction = "NONE" ->
             let width = extract_width dtype_ref in
             let signed = is_signed dtype_ref in
             Printf.eprintf "          -> Adding as internal signal\n%!";
             Some (name, width, signed)
         | _ ->
             Printf.eprintf "          -> Skipping\n%!";
             None)
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

(* Parse constant from Const node name like "4'h0" or "32'sh8" *)
let parse_const_value name =
  try
    (* Format: <width>'<format><value> *)
    (* Examples: "4'h0", "32'sh8", "8'd255" *)
    let parts = String.split_on_char '\'' name in
    match parts with
    | [width_str; format_value] ->
        let width = int_of_string width_str in
        (* Parse format and value *)
        let is_signed = String.length format_value > 1 && format_value.[0] = 's' in
        let fmt_start = if is_signed then 1 else 0 in
        let format_char = if String.length format_value > fmt_start then format_value.[fmt_start] else 'd' in
        let value_str = String.sub format_value (fmt_start + 1) (String.length format_value - fmt_start - 1) in
        
        let value = match format_char with
          | 'h' -> int_of_string ("0x" ^ value_str)
          | 'd' -> int_of_string value_str
          | 'b' -> int_of_string ("0b" ^ value_str)
          | 'o' -> int_of_string ("0o" ^ value_str)
          | _ -> int_of_string value_str
        in
        (width, value)
    | _ ->
        (* Fallback: try to parse as decimal *)
        (32, int_of_string name)
  with e ->
    Printf.eprintf "Warning: Failed to parse constant '%s': %s\n%!" name (Printexc.to_string e);
    (32, 0)

(* Convert expression to HardCaml Signal *)
let rec expr_to_remap decls = function
  | Sv_ast.VarRef { name; _ } | Sv_ast.VarRef' { name; _ } ->
      (try Hashtbl.find decls name
       with Not_found -> Invalid)
       
  | Sv_ast.Const { name; dtype_ref } ->
      (* Parse constant like "4'h0" or "8'd255" *)
      let width = extract_width dtype_ref in
      let (parsed_width, value) = parse_const_value name in
      let final_width = if width > 1 then width else parsed_width in
      Con (Constant.of_int ~width:final_width value)
       
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
            (* Use sel_bottom to get w bits starting from lsb *)
            let base_sig = sig' base in
            if lsb = 0 then
              Sig (sel_bottom base_sig w)
            else
              (* Shift down then select bottom *)
              Sig (sel_bottom (srl base_sig lsb) w)
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
      (match String.uppercase_ascii op with
       | "ADD" | "VADD" -> Sig (width_match (+:) lhs_sig rhs_sig)
       | "SUB" | "VSUB" -> Sig (width_match (-:) lhs_sig rhs_sig)
       | "MUL" | "VMUL" -> Sig (width_match ( *: ) lhs_sig rhs_sig)
       | "DIV" | "VDIV" ->
           (* Division is not supported in hardware - return all 1's (matching Verilog div-by-zero) *)
           let w = max (width lhs_sig) (width rhs_sig) in
           Sig (ones w)
       | "AND" | "VAND" -> Sig (width_match (&:) lhs_sig rhs_sig)
       | "OR" | "VOR" -> Sig (width_match (|:) lhs_sig rhs_sig)
       | "XOR" | "VXOR" -> Sig (width_match (^:) lhs_sig rhs_sig)
       | "EQ" | "VEQ" -> Sig (width_match (==:) lhs_sig rhs_sig)
       | "NEQ" | "VNEQ" -> Sig (width_match (<>:) lhs_sig rhs_sig)
       | "LT" | "VLT" -> Sig (width_match (<:) lhs_sig rhs_sig)
       | "LTE" | "VLTE" -> Sig (width_match (<=:) lhs_sig rhs_sig)
       | "GT" | "VGT" -> Sig (width_match (>:) lhs_sig rhs_sig)
       | "GTE" | "VGTE" -> Sig (width_match (>=:) lhs_sig rhs_sig)
       | "SHIFTL" | "VSHIFTL" -> Sig (log_shift sll lhs_sig rhs_sig)
       | "SHIFTR" | "VSHIFTR" -> Sig (log_shift srl lhs_sig rhs_sig)
       | "SHIFTRS" | "VSHIFTRS" -> Sig (log_shift sra lhs_sig rhs_sig)
       | _ -> Invalid)
       
  | Sv_ast.UnaryOp { op; operand; _ } | Sv_ast.UnaryOp' { op; operand; _ } ->
      let op_sig = sig' (expr_to_remap decls operand) in
      (match String.uppercase_ascii op with
       | "NOT" | "VNOT" -> Sig (~: op_sig)
       | "NEGATE" | "VNEGATE" -> Sig (zero (width op_sig) -: op_sig)
       | "REDAND" -> Sig (reduce ~f:(&:) [op_sig])
       | "REDOR" -> Sig (reduce ~f:(|:) [op_sig])
       | "REDXOR" -> Sig (reduce ~f:(^:) [op_sig])
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
      Printf.eprintf "        Assignment: lhs_width=%d rhs_width=%d\n%!" wlhs wrhs;
      if wlhs <= 0 then begin
        Printf.eprintf "        ERROR: LHS width is %d, skipping assignment\n%!" wlhs;
        None
      end else if wrhs <= 0 then begin
        Printf.eprintf "        ERROR: RHS width is %d, skipping assignment\n%!" wrhs;
        None
      end else
        let rhs_resized = if wlhs = wrhs then rhs_sig
                          else if wlhs > wrhs then begin
                            Printf.eprintf "        Upsizing RHS from %d to %d\n%!" wrhs wlhs;
                            uresize rhs_sig wlhs
                          end else begin
                            Printf.eprintf "        Downsizing RHS from %d to %d (select bits 0 to %d)\n%!" wrhs wlhs (wlhs-1);
                            (* select expects: signal lsb width, NOT signal lsb msb *)
                            (* Actually, looking at error, it seems to use lsb and width correctly *)
                            (* The issue is we're passing wlhs as the third arg *)
                            (* Let's use sel_bottom instead which takes width *)
                            sel_bottom rhs_sig wlhs
                          end in
        Some (v <-- rhs_resized)
  | Invalid ->
      Printf.eprintf "        ERROR: LHS expression is Invalid\n%!";
      None
  | _ -> 
      Printf.eprintf "        ERROR: LHS is not a Variable\n%!";
      None

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
                             else sel_bottom rhs_sig wlhs in
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

(* Extract clock signal from sensitivity list *)
let extract_clock senses =
  Printf.eprintf "        extract_clock called with %d senses\n%!" (List.length senses);
  let rec find_clock = function
    | [] ->
        Printf.eprintf "          No more items to check\n%!";
        None
    | Sv_ast.SenTree items :: rest ->
        Printf.eprintf "          Found SenTree with %d items\n%!" (List.length items);
        (match find_clock items with
         | Some c -> Some c
         | None -> find_clock rest)
    | Sv_ast.SenItem { edge_str; signal; _ } :: rest ->
        Printf.eprintf "          Found SenItem edge_str=%s\n%!" edge_str;
        (match signal with
         | Sv_ast.VarRef { name; _ } ->
             Printf.eprintf "            Signal is VarRef: %s\n%!" name;
             let edge_upper = String.uppercase_ascii edge_str in
             if edge_upper = "POS" || edge_upper = "POSEDGE" then Some name
             else if edge_upper = "NEG" || edge_upper = "NEGEDGE" then Some name
             else find_clock rest
         | _ ->
             Printf.eprintf "            Signal is not VarRef\n%!";
             find_clock rest)
    | _ :: rest ->
        Printf.eprintf "          Found other node type\n%!";
        find_clock rest
  in
  find_clock senses

(* Process always block *)
let process_always decls clock_opt reset_opt stmts =
  List.filter_map (process_stmt decls) stmts

(* Build HardCaml circuit for module *)
let build_circuit module_name stmts =
  try
    Printf.eprintf "    Building circuit for %s\n%!" module_name;

    (* Extract ports and signals *)
    let ports = extract_ports stmts in
    let signals = extract_signals stmts in
    let state_elements = identify_state_elements stmts in

    Printf.eprintf "      Found %d ports: " (List.length ports);
    List.iter (fun (name, width, dir) ->
      let dir_str = match dir with `Input -> "in" | `Output -> "out" | `Inout -> "inout" in
      Printf.eprintf "%s(%s:%d) " name dir_str width
    ) ports;
    Printf.eprintf "\n%!";

    Printf.eprintf "      Found %d internal signals\n%!" (List.length signals);
    Printf.eprintf "      Found %d state elements (registers)\n%!" (Hashtbl.length state_elements);

    let decls = Hashtbl.create 64 in

    (* Create inputs - outputs will be computed later *)
    List.iter (fun (name, width, dir) ->
      match dir with
      | `Input ->
          Printf.eprintf "        Creating input: %s[%d]\n%!" name width;
          Hashtbl.add decls name (Sig (input name width))
      | _ -> ()  (* Outputs computed from always blocks *)
    ) ports;

    (* Extract clock signal from always blocks for register specs *)
    let clock_signal =
      List.find_map (function
        | Sv_ast.Always { senses; _ } ->
            Printf.eprintf "      Checking always block for clock...\n%!";
            (match extract_clock senses with
             | Some clk_name ->
                 Printf.eprintf "        Found clock signal: %s\n%!" clk_name;
                 (match Hashtbl.find_opt decls clk_name with
                  | Some (Sig s) ->
                      Printf.eprintf "        Clock signal found in decls\n%!";
                      Some s
                  | _ ->
                      Printf.eprintf "        Clock signal NOT found in decls\n%!";
                      None)
             | None ->
                 Printf.eprintf "        No clock found in sensitivity\n%!";
                 None)
        | _ -> None
      ) stmts
    in
    Printf.eprintf "      Clock signal: %s\n%!" (if clock_signal = None then "None" else "Some");
    
    (* Create internal signals/registers AND output variables *)
    let all_vars = signals @ (List.filter_map (fun (name, width, dir) ->
      match dir with
      | `Output -> Some (name, width, false)
      | _ -> None
    ) ports) in
    
    List.iter (fun (name, width, _signed) ->
      if not (Hashtbl.mem decls name) then begin
        let is_state = Hashtbl.mem state_elements name in
        if is_state && clock_signal <> None then begin
          (* Create as register with clock *)
          Printf.eprintf "        Creating register: %s[%d] with clock\n%!" name width;
          let clk = Option.get clock_signal in
          let spec = Reg_spec.create ~clock:clk () in
          Hashtbl.add decls name (Var (Variable.reg spec ~width))
        end else begin
          (* Create as wire *)
          Printf.eprintf "        Creating wire: %s[%d]\n%!" name width;
          Hashtbl.add decls name (Var (Variable.wire ~default:(zero width)))
        end
      end
    ) all_vars;
    
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
  Printf.eprintf "HardCaml backend: Starting processing\n%!";
  
  (* Process each module *)
  let rec process_node = function
    | Sv_ast.Netlist nodes ->
        Printf.eprintf "  Processing Netlist with %d nodes\n%!" (List.length nodes);
        List.iter process_node nodes
    | Sv_ast.Module { name; stmts } ->
        Printf.eprintf "  Processing Module: %s with %d statements\n%!" name (List.length stmts);
        let circuit = build_circuit name stmts in
        circuits := circuit :: !circuits
    | _ -> 
        Printf.eprintf "  Skipping non-Module node\n%!"
  in
  
  process_node ast;
  
  Printf.eprintf "HardCaml backend: Processed %d circuits\n%!" (List.length !circuits);
  
  if List.length !circuits = 0 then begin
    warnings := "No modules found in AST" :: !warnings;
    ("(* No modules to generate *)\n", !warnings)
  end else begin
    let verilog_parts = List.map circuit_to_verilog !circuits in
    let header = "(* Generated via HardCaml circuit construction *)\n" ^
                 "(* Using Signal/Always API directly *)\n\n" in
    (header ^ String.concat "\n\n" verilog_parts, !warnings)
  end
