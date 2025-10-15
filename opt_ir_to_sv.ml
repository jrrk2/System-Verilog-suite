(* opt_ir_to_sv.ml - Convert optimization IR back to structural AST *)

open Sv_ast
open Sv_opt_ir

let debug = ref false

(* Map operation back to module name and parameters *)
let operation_to_module_info op =
  match op with
  | Add { width; signed = false } -> ("adder", width, [])
  | Add { width; signed = true } -> ("adder_signed", width, [])
  | Sub { width; signed = false } -> ("subtractor", width, [])
  | Sub { width; signed = true } -> ("subtractor_signed", width, [])
  | Mul { width; signed = false } -> ("multiplier", width, [])
  | Mul { width; signed = true } -> ("multiplier_signed", width, [])
  | Div { width; signed = false } -> ("divider", width, [])
  | Div { width; signed = true } -> ("divider_signed", width, [])
  | And { width } -> ("bitwise_and", width, [])
  | Or { width } -> ("bitwise_or", width, [])
  | Xor { width } -> ("bitwise_xor", width, [])
  | Not { width } -> ("bitwise_not", width, [])
  | Shift { width; direction = `Left; arithmetic = false; amount } -> 
      ("shifter_left", width, 
       match amount with Some n -> [("AMOUNT", n)] | None -> [])
  | Shift { width; direction = `Right; arithmetic = false; amount } -> 
      ("shifter_right", width,
       match amount with Some n -> [("AMOUNT", n)] | None -> [])
  | Shift { width; direction = `Right; arithmetic = true; amount } -> 
      ("shifter_right_arith", width,
       match amount with Some n -> [("AMOUNT", n)] | None -> [])
  | Compare { width; cmp_op = `Eq; _ } -> ("comparator_eq", width, [])
  | Compare { width; cmp_op = `Ne; _ } -> ("comparator_neq", width, [])
  | Compare { width; cmp_op = `Lt; signed = false; _ } -> ("comparator_lt", width, [])
  | Compare { width; cmp_op = `Lt; signed = true; _ } -> ("comparator_lt_signed", width, [])
  | Compare { width; cmp_op = `Le; signed = false; _ } -> ("comparator_le", width, [])
  | Compare { width; cmp_op = `Le; signed = true; _ } -> ("comparator_le_signed", width, [])
  | Compare { width; cmp_op = `Gt; signed = false; _ } -> ("comparator_gt", width, [])
  | Compare { width; cmp_op = `Gt; signed = true; _ } -> ("comparator_gt_signed", width, [])
  | Compare { width; cmp_op = `Ge; signed = false; _ } -> ("comparator_ge", width, [])
  | Compare { width; cmp_op = `Ge; signed = true; _ } -> ("comparator_ge_signed", width, [])
  | Mux { width } -> ("mux2", width, [])
  | Register { width; reset_value; _ } -> ("dff_en", width, [("RESET_VAL", reset_value)])
  | ZeroExtend { from_width; to_width } -> ("zero_extender", to_width, [("WIDTH_IN", from_width); ("WIDTH_OUT", to_width)])
  | SignExtend { from_width; to_width } -> ("sign_extender", to_width, [("WIDTH_IN", from_width); ("WIDTH_OUT", to_width)])
  | Extract { width; lsb; msb } -> ("extractor", width, [("LSB", lsb); ("MSB", msb)])
  | Concat { widths } -> 
      let total_width = List.fold_left (+) 0 widths in
      ("concatenator", total_width, [("NUM_INPUTS", List.length widths)])

(* Get wire name for a value ID *)
let id_to_wire_name id_to_name_tbl id =
  match Hashtbl.find_opt id_to_name_tbl id with
  | Some name -> name
  | None -> Printf.sprintf "n%d" id

(* Convert optimization IR back to structural AST *)
let convert_to_structural ir =
  if !debug then Printf.eprintf "\n=== Converting Opt IR to Structural AST ===\n";
  
  let stmts = ref [] in
  let id_to_name = Hashtbl.create 200 in
  
  (* Phase 1: Map input/output IDs to their names *)
  Hashtbl.iter (fun name value ->
    match value with
    | Input { id; _ } -> Hashtbl.add id_to_name id name
    | _ -> ()
  ) ir.ir_inputs;
  
  Hashtbl.iter (fun name value ->
    match value with
    | Output { id; _ } -> Hashtbl.add id_to_name id name
    | _ -> ()
  ) ir.ir_outputs;
  
  (* Phase 2: Generate input ports *)
  let input_ports = Hashtbl.fold (fun name value acc ->
    match value with
    | Input { id; width; _ } ->
        let dtype_ref = Some (ArrayType {
          base = BasicType { keyword = "logic"; range = None };
          range = if width > 1 then Printf.sprintf "%d:0" (width - 1) else "0:0"
        }) in
        let port = Var {
          name;
          dtype_ref;
          var_type = "PORT";
          direction = "INPUT";
          value = None;
          dtype_name = "";
          is_param = false;
        } in
        if !debug then Printf.eprintf "Input port: %s (width=%d)\n" name width;
        port :: acc
    | _ -> acc
  ) ir.ir_inputs [] in
  
  stmts := List.rev input_ports @ !stmts;
  
  (* Phase 3: Generate output ports *)
  let output_ports = Hashtbl.fold (fun name value acc ->
    match value with
    | Output { id; width; _ } ->
        let dtype_ref = Some (ArrayType {
          base = BasicType { keyword = "logic"; range = None };
          range = if width > 1 then Printf.sprintf "%d:0" (width - 1) else "0:0"
        }) in
        let port = Var {
          name;
          dtype_ref;
          var_type = "PORT";
          direction = "OUTPUT";
          value = None;
          dtype_name = "";
          is_param = false;
        } in
        if !debug then Printf.eprintf "Output port: %s (width=%d)\n" name width;
        port :: acc
    | _ -> acc
  ) ir.ir_outputs [] in
  
  stmts := List.rev output_ports @ !stmts;
  
  (* Phase 4: Generate wire declarations for all nodes *)
  Hashtbl.iter (fun id node ->
    let wire_name = Printf.sprintf "n%d" id in
    Hashtbl.add id_to_name id wire_name;
    
    let width = match node.node_output with
      | Wire { width; _ } -> width
      | _ -> 32
    in
    
    let dtype_ref = Some (BasicType {
      keyword = "logic";
      range = if width > 1 then Some (Printf.sprintf "%d:0" (width - 1)) else None
    }) in
    
    let wire_decl = Var {
      name = wire_name;
      dtype_ref;
      var_type = "VAR";
      direction = "NONE";
      value = None;
      dtype_name = "";
      is_param = false;
    } in
    
    if !debug then Printf.eprintf "Wire: %s (width=%d)\n" wire_name width;
    stmts := wire_decl :: !stmts
  ) ir.ir_nodes;
  
  (* Phase 5: Handle constants - create wire assignments *)
  Hashtbl.iter (fun const_val const_id ->
    let wire_name = Printf.sprintf "const_%d" const_id in
    Hashtbl.add id_to_name const_id wire_name;
    
    (* Create wire declaration *)
    let wire_decl = Var {
      name = wire_name;
      dtype_ref = Some (BasicType { keyword = "logic"; range = Some "31:0" });
      var_type = "VAR";
      direction = "NONE";
      value = None;
      dtype_name = "";
      is_param = false;
    } in
    
    (* Create assignment *)
    let assign = AssignW {
      lhs = VarRef { name = wire_name; access = "WR"; dtype_ref = None };
      rhs = Const { name = Printf.sprintf "32'd%d" const_val; dtype_ref = None };
    } in
    
    if !debug then Printf.eprintf "Constant: %s = %d\n" wire_name const_val;
    stmts := assign :: wire_decl :: !stmts
  ) ir.ir_constants;
  
  (* Phase 6: Generate Cell instances for each node *)
  Hashtbl.iter (fun id node ->
    let output_name = id_to_wire_name id_to_name id in
    let (module_name, width, extra_params) = operation_to_module_info node.node_op in
    
    (* Create parameter list *)
    let param_stmts = 
      (Var {
        name = "WIDTH";
        dtype_ref = None;
        var_type = "GPARAM";
        direction = "NONE";
        value = Some (Const { name = string_of_int width; dtype_ref = None });
        dtype_name = "";
        is_param = true;
      }) ::
      (List.map (fun (pname, pval) ->
        Var {
          name = pname;
          dtype_ref = None;
          var_type = "GPARAM";
          direction = "NONE";
          value = Some (Const { name = string_of_int pval; dtype_ref = None });
          dtype_name = "";
          is_param = true;
        }
      ) extra_params)
    in
    
    (* Create pins based on operation type *)
    let pins = match node.node_op with
      | Add _ | Sub _ | Mul _ | Div _ | And _ | Or _ | Xor _ | Compare _ ->
          (match node.node_inputs with
          | [a; b] ->
              [
                Pin { name = "a"; expr = Some (VarRef { 
                  name = id_to_wire_name id_to_name a; 
                  access = "RD"; 
                  dtype_ref = None 
                })};
                Pin { name = "b"; expr = Some (VarRef { 
                  name = id_to_wire_name id_to_name b; 
                  access = "RD"; 
                  dtype_ref = None 
                })};
                Pin { name = "out"; expr = Some (VarRef { 
                  name = output_name; 
                  access = "WR"; 
                  dtype_ref = None 
                })};
              ]
          | _ -> 
              if !debug then Printf.eprintf "Warning: Binary op with %d inputs\n" (List.length node.node_inputs);
              [])
      
      | Not _ | ZeroExtend _ | SignExtend _ | Extract _ ->
          (match node.node_inputs with
          | [a] ->
              [
                Pin { name = "in"; expr = Some (VarRef { 
                  name = id_to_wire_name id_to_name a; 
                  access = "RD"; 
                  dtype_ref = None 
                })};
                Pin { name = "out"; expr = Some (VarRef { 
                  name = output_name; 
                  access = "WR"; 
                  dtype_ref = None 
                })};
              ]
          | _ -> 
              if !debug then Printf.eprintf "Warning: Unary op with %d inputs\n" (List.length node.node_inputs);
              [])
      
      | Shift _ ->
          (match node.node_inputs with
          | [a] ->
              [
                Pin { name = "in"; expr = Some (VarRef { 
                  name = id_to_wire_name id_to_name a; 
                  access = "RD"; 
                  dtype_ref = None 
                })};
                Pin { name = "out"; expr = Some (VarRef { 
                  name = output_name; 
                  access = "WR"; 
                  dtype_ref = None 
                })};
              ]
          | [a; shift_amount] ->
              [
                Pin { name = "in"; expr = Some (VarRef { 
                  name = id_to_wire_name id_to_name a; 
                  access = "RD"; 
                  dtype_ref = None 
                })};
                Pin { name = "shift"; expr = Some (VarRef { 
                  name = id_to_wire_name id_to_name shift_amount; 
                  access = "RD"; 
                  dtype_ref = None 
                })};
                Pin { name = "out"; expr = Some (VarRef { 
                  name = output_name; 
                  access = "WR"; 
                  dtype_ref = None 
                })};
              ]
          | _ -> [])
      
      | Mux _ ->
          (match node.node_inputs with
          | [sel; in0; in1] ->
              [
                Pin { name = "sel"; expr = Some (VarRef { 
                  name = id_to_wire_name id_to_name sel; 
                  access = "RD"; 
                  dtype_ref = None 
                })};
                Pin { name = "in0"; expr = Some (VarRef { 
                  name = id_to_wire_name id_to_name in0; 
                  access = "RD"; 
                  dtype_ref = None 
                })};
                Pin { name = "in1"; expr = Some (VarRef { 
                  name = id_to_wire_name id_to_name in1; 
                  access = "RD"; 
                  dtype_ref = None 
                })};
                Pin { name = "out"; expr = Some (VarRef { 
                  name = output_name; 
                  access = "WR"; 
                  dtype_ref = None 
                })};
              ]
          | _ -> [])
      
      | Register { clock; reset; enable; _ } ->
          let base_pins = [
            Pin { name = "clk"; expr = Some (VarRef { 
              name = id_to_wire_name id_to_name clock; 
              access = "RD"; 
              dtype_ref = None 
            })};
            Pin { name = "d"; expr = Some (VarRef { 
              name = id_to_wire_name id_to_name (List.hd node.node_inputs); 
              access = "RD"; 
              dtype_ref = None 
            })};
            Pin { name = "q"; expr = Some (VarRef { 
              name = output_name; 
              access = "WR"; 
              dtype_ref = None 
            })};
          ] in
          let with_reset = match reset with
            | Some rst_id -> 
                (Pin { name = "rst"; expr = Some (VarRef { 
                  name = id_to_wire_name id_to_name rst_id; 
                  access = "RD"; 
                  dtype_ref = None 
                })}) :: base_pins
            | None -> 
                (Pin { name = "rst"; expr = Some (Const { 
                  name = "1'b0"; 
                  dtype_ref = None 
                })}) :: base_pins
          in
          let with_enable = match enable with
            | Some en_id ->
                (Pin { name = "en"; expr = Some (VarRef { 
                  name = id_to_wire_name id_to_name en_id; 
                  access = "RD"; 
                  dtype_ref = None 
                })}) :: with_reset
            | None ->
                (Pin { name = "en"; expr = Some (Const { 
                  name = "1'b1"; 
                  dtype_ref = None 
                })}) :: with_reset
          in
          with_enable
      
      | Concat _ ->
          (* Concatenator with multiple inputs *)
          List.mapi (fun i inp_id ->
            Pin { name = Printf.sprintf "in%d" i; expr = Some (VarRef {
              name = id_to_wire_name id_to_name inp_id;
              access = "RD";
              dtype_ref = None
            })}
          ) node.node_inputs @
          [Pin { name = "out"; expr = Some (VarRef {
            name = output_name;
            access = "WR";
            dtype_ref = None
          })}]
    in
    
    let cell = Cell {
      name = Printf.sprintf "%s_%d" module_name id;
      modp_addr = Some (Module {
        name = module_name;
        stmts = param_stmts;
      });
      pins;
    } in
    
    if !debug then Printf.eprintf "Cell: %s_%d (%s)\n" module_name id module_name;
    stmts := cell :: !stmts
  ) ir.ir_nodes;
  
  if !debug then Printf.eprintf "=== Conversion complete ===\n";
  if !debug then Printf.eprintf "Total statements: %d\n" (List.length !stmts);
  
  (* FIXED: Wrap in Netlist for Sv_gen compatibility *)
  let module_ast = Module { name = ir.ir_name; stmts = List.rev !stmts } in
  Netlist [module_ast]

let convert ?(verbose=false) ir =
  debug := verbose;
  convert_to_structural ir
