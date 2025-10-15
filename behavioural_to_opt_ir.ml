(* behavioral_to_opt_ir.ml - Convert behavioral (transformed) AST to optimization IR *)

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
      (* Parse constant value *)
      let value = try
        if String.contains name '\'' then
          (* Parse SystemVerilog constants like "32'h0", "8'd5", etc. *)
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
                | c, _ when c >= '0' && c <= '9' ->
                    int_of_string value_str
                | _ -> 0
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
        | "LOGNOT" -> Not { width }  (* Logical not can be bitwise not for single bit *)
        | "NEGATE" -> Sub { width; signed = true }  (* -x = 0 - x *)
        | "REDAND" | "REDOR" | "REDXOR" ->
            (* Reduction operators: could be implemented as tree of ops *)
            if !debug then Printf.eprintf "Warning: Reduction op '%s' simplified\n" op;
            Not { width = 1 }  (* Simplified for now *)
        | "EXTENDS" -> SignExtend { from_width = width / 2; to_width = width }
        | "EXTEND" -> ZeroExtend { from_width = width / 2; to_width = width }
        | _ ->
            if !debug then Printf.eprintf "Warning: Unknown unary op '%s'\n" op;
            Not { width }
      in
      
      let inputs = match op with
        | "NEGATE" -> 
            (* Create 0 - operand *)
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
      
      (* Infer width from then/else values *)
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
  
  | Sel { expr; lsb = Some lsb_expr; width = Some width_expr; _ } ->
      (* Part select: expr[lsb +: width] *)
      let base_id = convert_expr ctx expr in
      let lsb_id = convert_expr ctx lsb_expr in
      let width_id = convert_expr ctx width_expr in
      
      (* For now, create an Extract operation *)
      (* TODO: Handle dynamic indexing properly *)
      let width = 8 in  (* Default *)
      let operation = Extract { width; lsb = 0; msb = width - 1 } in
      let id = add_node ctx.ir operation [base_id] in
      if !debug then Printf.eprintf "  Sel (extract) -> id=%d\n" id;
      id
  
  | Sel { expr; lsb = Some lsb_expr; width = None; _ } ->
      (* Single bit select: expr[bit] *)
      let base_id = convert_expr ctx expr in
      let lsb_id = convert_expr ctx lsb_expr in
      
      let operation = Extract { width = 1; lsb = 0; msb = 0 } in
      let id = add_node ctx.ir operation [base_id] in
      if !debug then Printf.eprintf "  Sel (bit) -> id=%d\n" id;
      id
  
  | ArraySel { expr; index } ->
      (* Array element select *)
      let base_id = convert_expr ctx expr in
      let index_id = convert_expr ctx index in
      
      (* For now, treat as extract *)
      let operation = Extract { width = 8; lsb = 0; msb = 7 } in
      let id = add_node ctx.ir operation [base_id] in
      if !debug then Printf.eprintf "  ArraySel -> id=%d\n" id;
      id
  
  | Concat { parts } ->
      let part_ids = List.map (convert_expr ctx) parts in
      
      (* Calculate widths *)
      let widths = List.map (fun part ->
        get_width_from_dtype (match part with
          | VarRef { dtype_ref; _ } -> dtype_ref
          | _ -> None
        )
      ) parts in
      
      let operation = Concat { widths } in
      let id = add_node ctx.ir operation part_ids in
      if !debug then Printf.eprintf "  Concat -> id=%d\n" id;
      id
  
  | Replicate { src; count; _ } ->
      (* {count{src}} - replicate src, count times *)
      let src_id = convert_expr ctx src in
      let count_val = match count with
        | Const { name; _ } -> 
            (try int_of_string name with _ -> 1)
        | _ -> 1
      in
      
      (* Create multiple copies and concatenate *)
      let src_width = get_width_from_dtype (match src with
        | VarRef { dtype_ref; _ } -> dtype_ref
        | _ -> None
      ) in
      
      let widths = List.init count_val (fun _ -> src_width) in
      let inputs = List.init count_val (fun _ -> src_id) in
      
      let operation = Concat { widths } in
      let id = add_node ctx.ir operation inputs in
      if !debug then Printf.eprintf "  Replicate -> id=%d\n" id;
      id
  
  | _ ->
      if !debug then Printf.eprintf "Warning: Unsupported expression type\n";
      get_or_create_constant ctx.ir 0 32

(* Convert statement *)
let rec convert_stmt ctx stmt =
  match stmt with
  | Assign { lhs = VarRef { name; dtype_ref; _ }; rhs; is_blocking = true } ->
      let rhs_id = convert_expr ctx rhs in
      
      if ctx.in_sequential then begin
        (* Sequential assignment: create register *)
        let width = get_width_from_dtype dtype_ref in
        
        match ctx.current_clock with
        | Some clk_id ->
            let operation = Register {
              width;
              clock = clk_id;
              reset = None;
              enable = None;
              reset_value = 0;
            } in
            let reg_id = add_node ctx.ir operation [rhs_id] in
            Hashtbl.replace ctx.var_to_id name reg_id;
            if !debug then Printf.eprintf "  Register: %s <- id=%d\n" name reg_id
        
        | None ->
            (* No clock? Just wire assignment *)
            Hashtbl.replace ctx.var_to_id name rhs_id;
            if !debug then Printf.eprintf "  Assign (no clock): %s <- id=%d\n" name rhs_id
      end else begin
        (* Combinational assignment *)
        Hashtbl.replace ctx.var_to_id name rhs_id;
        if !debug then Printf.eprintf "  Assign: %s <- id=%d\n" name rhs_id
      end
  
  | AssignW { lhs = VarRef { name; _ }; rhs } ->
      let rhs_id = convert_expr ctx rhs in
      Hashtbl.replace ctx.var_to_id name rhs_id;
      if !debug then Printf.eprintf "  AssignW: %s <- id=%d\n" name rhs_id
  
  | Always { always; senses; stmts } ->
      if !debug then Printf.eprintf "Always block: %s\n" always;
      
      (* Detect if sequential or combinational *)
      let is_sequential = always = "always_ff" || always = "always" && begin
        List.exists (function
          | SenTree items -> List.exists (function
              | SenItem { edge_str; _ } -> 
                  String.contains (String.lowercase_ascii edge_str) 'p' ||
                  String.contains (String.lowercase_ascii edge_str) 'n'
              | _ -> false
            ) items
          | _ -> false
        ) senses
      end in
      
      (* Extract clock if sequential *)
      let clock_id = if is_sequential then
        List.find_map (function
          | SenTree items -> List.find_map (function
              | SenItem { signal = VarRef { name = clk_name; _ }; _ } ->
                  Some (get_var_id ctx clk_name)
              | _ -> None
            ) items
          | _ -> None
        ) senses
      else None in
      
      let old_sequential = ctx.in_sequential in
      let old_clock = ctx.current_clock in
      
      ctx.in_sequential <- is_sequential;
      ctx.current_clock <- clock_id;
      
      List.iter (convert_stmt ctx) stmts;
      
      ctx.in_sequential <- old_sequential;
      ctx.current_clock <- old_clock
  
  | Begin { stmts; _ } ->
      List.iter (convert_stmt ctx) stmts
  
  | If { condition; then_stmt; else_stmt } ->
      (* For now, we don't handle control flow in IR *)
      (* This should have been converted to muxes by sv_transform *)
      if !debug then Printf.eprintf "Warning: If statement in behavioral code - should be transformed first\n";
      convert_stmt ctx then_stmt;
      Option.iter (convert_stmt ctx) else_stmt
  
  | Var { name; dtype_ref; var_type = "VAR"; _ } ->
      (* Internal variable declaration *)
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
            Hashtbl.add ctx.var_to_id port_name id;
            if !debug then Printf.eprintf "Output: %s (width=%d, id=%d)\n" port_name width id
        
        | _ -> ()
      ) stmts;
      
      (* Phase 2: Convert statements *)
      List.iter (convert_stmt ctx) stmts;
      
      if !debug then Printf.eprintf "\n=== Conversion complete ===\n";
      if !debug then Printf.eprintf "Total nodes: %d\n" (Hashtbl.length ir.ir_nodes);
      
      ir
  
  | _ -> failwith "Expected Module or Netlist with single module"

let convert ?(verbose=false) ast =
  debug := verbose;
  convert_to_opt_ir ast
