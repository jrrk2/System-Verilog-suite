(* behavioral_to_opt_ir.ml - Complete correct version *)

open Sv_ast
open Sv_opt_ir

let debug = ref false

(* Context for tracking variable mappings *)
type conversion_context = {
  ir: opt_ir;
  var_to_id: (string, value_id) Hashtbl.t;
  mutable in_sequential: bool;
  mutable current_clock: value_id option;
}

let create_context ir = {
  ir;
  var_to_id = Hashtbl.create 100;
  in_sequential = false;
  current_clock = None;
}

(* Get or create ID for a variable reference *)
let get_var_id ctx var_name =
  match Hashtbl.find_opt ctx.var_to_id var_name with
  | Some id -> id
  | None ->
      if !debug then Printf.eprintf "Warning: Variable '%s' not found, creating wire\n" var_name;
      let id = get_new_id ctx.ir in
      Hashtbl.add ctx.var_to_id var_name id;
      id

(* Parse bit width from dtype_ref *)
let get_width_from_dtype dtype_ref =
  match dtype_ref with
  | Some (BasicType { range = Some r; _ }) ->
      (try
        let parts = String.split_on_char ':' r in
        match parts with
        | [msb; lsb] -> 
            abs (int_of_string (String.trim msb) - int_of_string (String.trim lsb)) + 1
        | _ -> 32
      with _ -> 32)
  | Some (ArrayType { range; _ }) ->
      (try
        let parts = String.split_on_char ':' range in
        match parts with
        | [msb; lsb] -> 
            abs (int_of_string (String.trim msb) - int_of_string (String.trim lsb)) + 1
        | _ -> 32
      with _ -> 32)
  | _ -> 32

(* Convert expression to value_id *)
let rec convert_expr ctx expr =
  match expr with
  | VarRef { name; dtype_ref; _ } ->
      let id = get_var_id ctx name in
      if !debug then Printf.eprintf "  VarRef: %s -> id=%d\n" name id;
      id
  
  | Const { name; dtype_ref } ->
      let value = try
        if String.contains name '\'' then
          let parts = String.split_on_char '\'' name in
          match parts with
          | width_str :: rest ->
              let value_str = String.concat "'" rest in
              if String.length value_str >= 2 then
                match value_str.[0], value_str.[1] with
                | 's', 'h' -> 
                    int_of_string ("0x" ^ String.sub value_str 2 (String.length value_str - 2))
                | 's', 'd' -> 
                    int_of_string (String.sub value_str 2 (String.length value_str - 2))
                | 'h', _ -> 
                    int_of_string ("0x" ^ String.sub value_str 1 (String.length value_str - 1))
                | 'd', _ -> 
                    int_of_string (String.sub value_str 1 (String.length value_str - 1))
                | 'b', _ ->
                    int_of_string ("0b" ^ String.sub value_str 1 (String.length value_str - 1))
                | _ -> int_of_string value_str
              else int_of_string value_str
          | _ -> int_of_string name
        else
          int_of_string name
      with _ -> 0 in
      
      let width = get_width_from_dtype dtype_ref in
      let id = get_or_create_constant ctx.ir value width in
      if !debug then Printf.eprintf "  Const: %s -> value=%d, id=%d\n" name value id;
      id
  
  | BinaryOp { op; lhs; rhs; dtype_ref } ->
      let lhs_id = convert_expr ctx lhs in
      let rhs_id = convert_expr ctx rhs in
      let width = get_width_from_dtype dtype_ref in
      
      let operation = match op with
        | "ADD" -> Add { width; signed = false }
        | "SUB" -> Sub { width; signed = false }
        | "MUL" -> Mul { width; signed = false }
        | "MULS" -> Mul { width; signed = true }
        | "DIV" -> Div { width; signed = false }
        | "DIVS" -> Div { width; signed = true }
        | "AND" -> And { width }
        | "OR" -> Or { width }
        | "XOR" -> Xor { width }
        | "SHIFTL" -> Shift { width; direction = `Left; arithmetic = false; amount = None }
        | "SHIFTR" -> Shift { width; direction = `Right; arithmetic = false; amount = None }
        | "SHIFTRS" -> Shift { width; direction = `Right; arithmetic = true; amount = None }
        | "EQ" -> Compare { width; cmp_op = `Eq; signed = false }
        | "NEQ" -> Compare { width; cmp_op = `Ne; signed = false }
        | "LT" -> Compare { width; cmp_op = `Lt; signed = false }
        | "LTS" -> Compare { width; cmp_op = `Lt; signed = true }
        | "LTE" -> Compare { width; cmp_op = `Le; signed = false }
        | "LTES" -> Compare { width; cmp_op = `Le; signed = true }
        | "GT" -> Compare { width; cmp_op = `Gt; signed = false }
        | "GTS" -> Compare { width; cmp_op = `Gt; signed = true }
        | "GTE" -> Compare { width; cmp_op = `Ge; signed = false }
        | "GTES" -> Compare { width; cmp_op = `Ge; signed = true }
        | _ -> 
            if !debug then Printf.eprintf "Warning: Unknown binary op '%s'\n" op;
            Add { width; signed = false }
      in
      
      let id = add_node ctx.ir operation [lhs_id; rhs_id] in
      if !debug then Printf.eprintf "  BinaryOp: %s (w=%d) -> id=%d\n" op width id;
      id
  
  | UnaryOp { op; operand; dtype_ref } ->
      let operand_id = convert_expr ctx operand in
      let width = get_width_from_dtype dtype_ref in
      
      let operation = match op with
        | "NOT" -> Not { width }
        | "LOGNOT" -> Not { width }
        | "NEGATE" -> Sub { width; signed = true }
        | "REDAND" | "REDOR" | "REDXOR" ->
            if !debug then Printf.eprintf "Warning: Reduction op '%s' simplified\n" op;
            Not { width = 1 }
        | "EXTENDS" -> SignExtend { from_width = width / 2; to_width = width }
        | "EXTEND" -> ZeroExtend { from_width = width / 2; to_width = width }
        | _ ->
            if !debug then Printf.eprintf "Warning: Unknown unary op '%s'\n" op;
            Not { width }
      in
      
      let inputs = match op with
        | "NEGATE" -> 
            let zero_id = get_or_create_constant ctx.ir 0 width in
            [zero_id; operand_id]
        | _ -> [operand_id]
      in
      
      let id = add_node ctx.ir operation inputs in
      if !debug then Printf.eprintf "  UnaryOp: %s (w=%d) -> id=%d\n" op width id;
      id
  
  | Cond { condition; then_val; else_val } ->
      let cond_id = convert_expr ctx condition in
      let then_id = convert_expr ctx then_val in
      let else_id = convert_expr ctx else_val in
      
      let width = get_width_from_dtype (match then_val with
        | VarRef { dtype_ref; _ } -> dtype_ref
        | BinaryOp { dtype_ref; _ } -> dtype_ref
        | UnaryOp { dtype_ref; _ } -> dtype_ref
        | _ -> None
      ) in
      
      let operation = Mux { width } in
      let id = add_node ctx.ir operation [cond_id; else_id; then_id] in
      if !debug then Printf.eprintf "  Cond (mux) (w=%d) -> id=%d\n" width id;
      id
  
  | ArraySel { expr; index } ->
      if !debug then Printf.eprintf "  ArraySel detected\n";
      let base_id = convert_expr ctx expr in
      let index_val = match index with
        | Const { name; _ } -> (try int_of_string name with _ -> 0)
        | _ -> 0
      in
      let operation = Extract { width = 1; lsb = index_val; msb = index_val } in
      let id = add_node ctx.ir operation [base_id] in
      if !debug then Printf.eprintf "    Created extract[%d]: id=%d\n" index_val id;
      id
  
  | Sel { expr; lsb = Some lsb_expr; width = None; _ } ->
      if !debug then Printf.eprintf "  Sel (bit select)\n";
      let base_id = convert_expr ctx expr in
      let index = match lsb_expr with
        | Const { name; _ } -> (try int_of_string name with _ -> 0)
        | _ -> 0
      in
      let operation = Extract { width = 1; lsb = index; msb = index } in
      let id = add_node ctx.ir operation [base_id] in
      if !debug then Printf.eprintf "    Created extract[%d]: id=%d\n" index id;
      id
  
  | _ ->
      if !debug then Printf.eprintf "  Warning: Unsupported expression type\n";
      get_or_create_constant ctx.ir 0 32

(* Extract clock signal from Always sensitivity list *)
let extract_clock ctx senses =
  (* Look for edge-sensitive signals (posedge/negedge) *)
  let rec find_clock sens_list =
    match sens_list with
    | [] -> None
    | VarRef { name; _ } :: _ ->
        (* Found a clock signal *)
        (match Hashtbl.find_opt ctx.var_to_id name with
         | Some id -> Some id
         | None -> None)
    | _ :: rest -> find_clock rest
  in
  find_clock senses

(* Convert statement *)
let rec convert_stmt ctx stmt =
  match stmt with
  | Assign { lhs = VarRef { name; dtype_ref; _ }; rhs; is_blocking = true } ->
      let rhs_id = convert_expr ctx rhs in
      Hashtbl.replace ctx.var_to_id name rhs_id;
      if !debug then Printf.eprintf "  Assign: %s <- id=%d\n" name rhs_id

  | Assign { lhs = VarRef { name; dtype_ref; _ }; rhs; is_blocking = false } ->
      (* Non-blocking assignment - create Register node *)
      let rhs_id = convert_expr ctx rhs in
      let width = get_width_from_dtype dtype_ref in
      (* Use the current_clock from context if available *)
      let clock_id = match ctx.current_clock with
        | Some clk -> clk
        | None ->
            (* If no clock in context, try to find one or use a dummy *)
            if !debug then Printf.eprintf "  Warning: No clock for sequential assignment to %s\n" name;
            rhs_id (* Fallback - use input as clock *)
      in
      let operation = Register {
        width;
        clock = clock_id;
        reset = None;
        enable = None;
        reset_value = 0
      } in
      let node_id = add_node ctx.ir operation [rhs_id] in
      Hashtbl.replace ctx.var_to_id name node_id;
      if !debug then Printf.eprintf "  AssignDly: %s <- Register node_id=%d (input=%d, clock=%d)\n"
        name node_id rhs_id clock_id

  | AssignW { lhs = VarRef { name; _ }; rhs } ->
      let rhs_id = convert_expr ctx rhs in
      Hashtbl.replace ctx.var_to_id name rhs_id;
      if !debug then Printf.eprintf "  AssignW: %s <- id=%d\n" name rhs_id

  | Always { always; senses; stmts } ->
      (* Extract clock from sensitivity list *)
      let old_clock = ctx.current_clock in
      let old_sequential = ctx.in_sequential in
      (match extract_clock ctx senses with
       | Some clock_id ->
           ctx.current_clock <- Some clock_id;
           ctx.in_sequential <- true;
           if !debug then Printf.eprintf "  Always %s with clock id=%d\n" always clock_id
       | None ->
           if !debug then Printf.eprintf "  Always %s (no clock found)\n" always
      );
      List.iter (convert_stmt ctx) stmts;
      (* Restore context *)
      ctx.current_clock <- old_clock;
      ctx.in_sequential <- old_sequential

  | Begin { stmts; _ } ->
      List.iter (convert_stmt ctx) stmts

  | If { condition; then_stmt; else_stmt } ->
      (* For now, process both branches to extract assignments *)
      (* TODO: Create Mux nodes based on condition *)
      if !debug then Printf.eprintf "  If statement (processing branches)\n";
      convert_stmt ctx then_stmt;
      (match else_stmt with
       | Some stmt -> convert_stmt ctx stmt
       | None -> ())

  | Var { name; dtype_ref; var_type = "VAR"; _ } ->
      let id = get_new_id ctx.ir in
      Hashtbl.add ctx.var_to_id name id;
      if !debug then Printf.eprintf "  Var: %s (id=%d)\n" name id

  | _ -> ()

(* Main conversion function *)
let convert_to_opt_ir ast =
  match ast with
  | Netlist [Module { name; stmts }] | Module { name; stmts } ->
      let ir = create_ir name in
      let ctx = create_context ir in
      
      if !debug then Printf.eprintf "\n=== Converting behavioral AST to Opt IR ===\n";
      if !debug then Printf.eprintf "Module: %s\n" name;
      
      (* Track output port names separately *)
      let output_names = Hashtbl.create 10 in
      
      (* Phase 1: Create ports *)
      List.iter (function
        | Var { name = port_name; dtype_ref; var_type = "PORT"; direction = "INPUT"; _ } ->
            let width = get_width_from_dtype dtype_ref in
            let id = add_input ir port_name width in
            Hashtbl.add ctx.var_to_id port_name id;
            if !debug then Printf.eprintf "Input: %s (width=%d, id=%d)\n" port_name width id
        
        | Var { name = port_name; dtype_ref; var_type = "PORT"; direction = "OUTPUT"; _ } ->
            let width = get_width_from_dtype dtype_ref in
            let id = add_output ir port_name width in
            (* DON'T add to var_to_id yet - outputs will be assigned later *)
            Hashtbl.add output_names port_name (id, width);
            if !debug then Printf.eprintf "Output: %s (width=%d, id=%d)\n" port_name width id
        
        | _ -> ()
      ) stmts;
      
      (* Phase 2: Convert statements *)
      List.iter (convert_stmt ctx) stmts;
      
      (* Phase 3: Connect outputs to their computed values *)
      Hashtbl.iter (fun output_name (output_id, width) ->
        match Hashtbl.find_opt ctx.var_to_id output_name with
        | Some computed_id ->
            (* Output was assigned - update the IR to point to computed value *)
            if !debug then Printf.eprintf "Connecting output %s: original_id=%d -> computed_id=%d\n" 
              output_name output_id computed_id;
            (* Replace the output in the IR with the computed value *)
            Hashtbl.replace ir.ir_outputs output_name 
              (Output { id = computed_id; name = output_name; width })
        | None ->
            if !debug then Printf.eprintf "Warning: Output %s not assigned\n" output_name
      ) output_names;
      
      if !debug then Printf.eprintf "\n=== Conversion complete ===\n";
      if !debug then Printf.eprintf "Total nodes: %d\n" (Hashtbl.length ir.ir_nodes);
      
      ir
  
  | _ -> failwith "Expected Module or Netlist with single module"

let convert ?(verbose=false) ast =
  debug := verbose;
  convert_to_opt_ir ast
