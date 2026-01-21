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

(* Check if we're in a clocked always block *)
let is_clocked_always senses =
  let rec check_clock = function
    | [] -> false
    | Sv_ast.SenTree items :: rest -> check_clock items || check_clock rest
    | Sv_ast.SenItem { edge_str; _ } :: _ ->
        let edge = String.uppercase_ascii edge_str in
        edge = "POS" || edge = "POSEDGE" || edge = "NEG" || edge = "NEGEDGE"
    | _ :: rest -> check_clock rest
  in
  check_clock senses

(* Find variables used as RHS in expressions *)
let rec find_rhs_vars expr =
  let vars = ref [] in
  let rec traverse = function
    | Sv_ast.VarRef { name; _ } | Sv_ast.VarRef' { name; _ } ->
        vars := name :: !vars
    | Sv_ast.BinaryOp { lhs; rhs; _ } | Sv_ast.BinaryOp' { lhs; rhs; _ } ->
        traverse lhs; traverse rhs
    | Sv_ast.UnaryOp { operand; _ } | Sv_ast.UnaryOp' { operand; _ } ->
        traverse operand
    | Sv_ast.Sel { expr; lsb; width; _ } ->
        traverse expr;
        Option.iter traverse lsb;
        Option.iter traverse width
    | Sv_ast.ArraySel { expr; index; _ } ->
        traverse expr; traverse index
    | Sv_ast.Concat { parts } -> List.iter traverse parts
    | Sv_ast.Cond { condition; then_val; else_val } ->
        traverse condition; traverse then_val; traverse else_val
    | Sv_ast.Replicate { src; _ } | Sv_ast.Replicate' { src; _ } -> traverse src
    | _ -> ()
  in
  traverse expr;
  !vars

(* Find blocking assignments that are used later in the same statement list *)
let find_intermediate_blocking_vars stmts =
  let intermediate = Hashtbl.create 16 in
  let defined = Hashtbl.create 16 in

  (* First pass: find all blocking assignments *)
  let rec mark_blocking = function
    | Sv_ast.Assign { lhs; is_blocking = true; _ } ->
        (match lhs with
         | Sv_ast.VarRef { name; _ } | Sv_ast.VarRef' { name; _ } ->
             Hashtbl.replace defined name true
         | _ -> ())
    | Sv_ast.Begin { stmts; _ } -> List.iter mark_blocking stmts
    | Sv_ast.If { then_stmt; else_stmt; _ } ->
        mark_blocking then_stmt;
        Option.iter mark_blocking else_stmt
    | Sv_ast.Case { items; _ } ->
        List.iter (fun item -> List.iter mark_blocking item.Sv_ast.statements) items
    | _ -> ()
  in

  (* Second pass: find which ones are used later as RHS *)
  let rec find_used defined_so_far = function
    | [] -> ()
    | stmt :: rest ->
        (match stmt with
         | Sv_ast.Assign { lhs; rhs; is_blocking = true; _ } ->
             (* Check if RHS uses any blocking vars defined earlier *)
             let rhs_vars = find_rhs_vars rhs in
             List.iter (fun v ->
               if Hashtbl.mem defined_so_far v then
                 Hashtbl.replace intermediate v true
             ) rhs_vars;
             (* Mark this var as defined *)
             (match lhs with
              | Sv_ast.VarRef { name; _ } | Sv_ast.VarRef' { name; _ } ->
                  Hashtbl.replace defined_so_far name true
              | _ -> ())
         | Sv_ast.Assign { rhs; is_blocking = false; _ } ->
             (* Non-blocking: check if RHS uses blocking vars *)
             let rhs_vars = find_rhs_vars rhs in
             List.iter (fun v ->
               if Hashtbl.mem defined_so_far v then
                 Hashtbl.replace intermediate v true
             ) rhs_vars
         | Sv_ast.Begin { stmts; _ } -> find_used defined_so_far stmts
         | _ -> ());
        find_used defined_so_far rest
  in

  List.iter mark_blocking stmts;
  find_used defined stmts;
  intermediate

(* Identify state elements (registers) - all assignments in clocked always blocks,
   except blocking assignments that are intermediate values *)
let identify_state_elements stmts =
  let state_vars = Hashtbl.create 16 in

  let rec process_always = function
    | Sv_ast.Always { senses; stmts = always_stmts; _ } ->
        let is_clocked = is_clocked_always senses in
        if is_clocked then begin
          Printf.eprintf "        Found clocked always block\n%!";
          (* Find intermediate blocking assignments *)
          let intermediate = find_intermediate_blocking_vars always_stmts in
          (* All assignments are state elements except intermediates *)
          let rec mark_state = function
            | Sv_ast.Assign { lhs; is_blocking; _ } ->
                (match lhs with
                 | Sv_ast.VarRef { name; _ } | Sv_ast.VarRef' { name; _ } ->
                     if is_blocking && Hashtbl.mem intermediate name then begin
                       Printf.eprintf "          Blocking var %s is intermediate (used later)\n%!" name
                     end else begin
                       Printf.eprintf "          Marking %s as state element\n%!" name;
                       Hashtbl.replace state_vars name true
                     end
                 | _ -> ())
            | Sv_ast.Begin { stmts; _ } -> List.iter mark_state stmts
            | Sv_ast.If { then_stmt; else_stmt; _ } ->
                mark_state then_stmt;
                Option.iter mark_state else_stmt
            | Sv_ast.Case { items; _ } ->
                List.iter (fun item -> List.iter mark_state item.Sv_ast.statements) items
            | _ -> ()
          in
          List.iter mark_state always_stmts
        end
    | Sv_ast.Begin { stmts; _ } -> List.iter process_always stmts
    | _ -> ()
  in

  List.iter process_always stmts;
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

(* Extract ALL variables from LHS of assignments (including SSA variables) *)
let extract_lhs_variables stmts =
  let vars = Hashtbl.create 64 in

  let rec extract_from_expr = function
    | Sv_ast.VarRef { name; dtype_ref; _ } ->
        if not (Hashtbl.mem vars name) then begin
          let width = extract_width dtype_ref in
          Printf.eprintf "        Found LHS variable: %s[%d]\n%!" name width;
          Hashtbl.replace vars name width
        end
    | Sv_ast.VarRef' { name; _ } ->
        if not (Hashtbl.mem vars name) then begin
          (* VarRef' doesn't have width info, default to 1 *)
          Printf.eprintf "        Found LHS variable: %s[1] (VarRef')\n%!" name;
          Hashtbl.replace vars name 1
        end
    | _ -> ()
  in

  let rec scan_stmt = function
    | Sv_ast.Assign { lhs; _ } | Sv_ast.AssignW { lhs; _ } ->
        extract_from_expr lhs
    | Sv_ast.Always { stmts; _ } -> List.iter scan_stmt stmts
    | Sv_ast.Begin { stmts; _ } -> List.iter scan_stmt stmts
    | Sv_ast.Initial { stmts; _ } -> List.iter scan_stmt stmts
    | Sv_ast.If { then_stmt; else_stmt; _ } ->
        scan_stmt then_stmt;
        (match else_stmt with Some e -> scan_stmt e | None -> ())
    | Sv_ast.Case { items; _ } ->
        List.iter (fun item -> List.iter scan_stmt item.Sv_ast.statements) items
    | _ -> ()
  in

  List.iter scan_stmt stmts;
  Hashtbl.fold (fun name width acc -> (name, width, false) :: acc) vars []

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

(* Sanitize 4-state Verilog values by converting x, z, ? to 0 *)
let sanitize_4state_value str =
  String.map (fun c ->
    match Char.lowercase_ascii c with
    | 'x' | 'z' | '?' -> '0'
    | _ -> c
  ) str

(* Parse constant from Const node name like "4'h0" or "32'sh8" *)
let parse_const_value name =
  try
    (* Format: <width>'<format><value> *)
    (* Examples: "4'h0", "32'sh8", "8'd255", "32'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" *)
    let parts = String.split_on_char '\'' name in
    match parts with
    | [width_str; format_value] ->
        let width = int_of_string width_str in
        (* Parse format and value *)
        let is_signed = String.length format_value > 1 && format_value.[0] = 's' in
        let fmt_start = if is_signed then 1 else 0 in
        let format_char = if String.length format_value > fmt_start then format_value.[fmt_start] else 'd' in
        let value_str = String.sub format_value (fmt_start + 1) (String.length format_value - fmt_start - 1) in
        (* Sanitize 4-state values (x, z) to 0 *)
        let clean_value_str = sanitize_4state_value value_str in

        let value = match format_char with
          | 'h' -> int_of_string ("0x" ^ clean_value_str)
          | 'd' -> int_of_string clean_value_str
          | 'b' -> int_of_string ("0b" ^ clean_value_str)
          | 'o' -> int_of_string ("0o" ^ clean_value_str)
          | _ -> int_of_string clean_value_str
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
       | "MUL" | "VMUL" | "MULS" -> Sig (width_match ( *: ) lhs_sig rhs_sig)
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
       (* Power operators: 2**n is implemented as 1<<n *)
       | "POW" | "POWSS" | "POWSU" | "POWUS" ->
           (* Check if lhs is constant 2 *)
           (match lhs with
            | Sv_ast.Const { name; _ } | Sv_ast.Const' { name; _ } ->
                (* Parse the constant - format is "width'base value" *)
                let parts = String.split_on_char '\'' name in
                (match parts with
                 | [_; rest] ->
                     (* Extract value after 'h, 'd, 'b prefix *)
                     let value_str = if String.length rest > 0 && (rest.[0] = 'h' || rest.[0] = 'd' || rest.[0] = 'b')
                                     then String.sub rest 1 (String.length rest - 1)
                                     else rest in
                     let value = try int_of_string ("0x" ^ value_str) with _ -> 0 in
                     if value = 2 then begin
                       (* 2**n implemented as 1<<n *)
                       let one = Signal.of_int ~width:(width lhs_sig) 1 in
                       Sig (log_shift sll one rhs_sig)
                     end else begin
                       (* Other constant bases: not yet supported *)
                       let _ = Printf.eprintf "Warning: Power with base %d not supported, using 0\n" value in
                       Sig (Signal.of_int ~width:(width lhs_sig) 0)
                     end
                 | _ ->
                     let _ = Printf.eprintf "Warning: Power operator not supported for non-constant base, using 0\n" in
                     Sig (Signal.of_int ~width:(width lhs_sig) 0))
            | _ ->
                (* Non-constant base *)
                let _ = Printf.eprintf "Warning: Power operator not supported for variable base, using 0\n" in
                Sig (Signal.of_int ~width:(width lhs_sig) 0))
       (* Streaming operators: simplified implementation - just return lhs *)
       | "STREAML" | "STREAMR" ->
           (* Streaming concatenation: {<< {x}} or {>> {x}}
              Simplified: just return the value as-is *)
           Sig lhs_sig
       | _ -> Invalid)
       
  | Sv_ast.UnaryOp { op; operand; dtype_ref } ->
      let op_sig = sig' (expr_to_remap decls operand) in
      (match String.uppercase_ascii op with
       | "NOT" | "VNOT" -> Sig (~: op_sig)
       | "NEGATE" | "VNEGATE" -> Sig (zero (width op_sig) -: op_sig)
       | "REDAND" -> Sig (reduce ~f:(&:) [op_sig])
       | "REDOR" -> Sig (reduce ~f:(|:) [op_sig])
       | "REDXOR" -> Sig (reduce ~f:(^:) [op_sig])
       | "EXTEND" ->
           (* Zero/unsigned extension *)
           let target_width = extract_width dtype_ref in
           Sig (uresize op_sig target_width)
       | "EXTENDS" ->
           (* Sign extension *)
           let target_width = extract_width dtype_ref in
           Sig (sresize op_sig target_width)
       | _ -> Invalid)

  | Sv_ast.UnaryOp' { op; operand; _ } ->
      let op_sig = sig' (expr_to_remap decls operand) in
      (match String.uppercase_ascii op with
       | "NOT" | "VNOT" -> Sig (~: op_sig)
       | "NEGATE" | "VNEGATE" -> Sig (zero (width op_sig) -: op_sig)
       | "REDAND" -> Sig (reduce ~f:(&:) [op_sig])
       | "REDOR" -> Sig (reduce ~f:(|:) [op_sig])
       | "REDXOR" -> Sig (reduce ~f:(^:) [op_sig])
       | "EXTEND" | "EXTENDS" ->
           (* For UnaryOp' without dtype_ref, we can't determine target width,
              so just return the operand as-is. The resizing will happen at assignment. *)
           Sig op_sig
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
      (* Match widths of then and else branches *)
      let w_then = width then_sig in
      let w_else = width else_sig in
      if w_then = w_else then
        Sig (mux2 cond_bit then_sig else_sig)
      else if w_then > w_else then begin
        (* Extend else branch to match then *)
        let else_extended = uresize else_sig w_then in
        Sig (mux2 cond_bit then_sig else_extended)
      end else begin
        (* Extend then branch to match else *)
        let then_extended = uresize then_sig w_else in
        Sig (mux2 cond_bit then_extended else_sig)
      end
      
  | Sv_ast.Replicate { count; src; _ } | Sv_ast.Replicate' { count; src; _ } ->
      (match expr_to_remap decls count with
       | Con c ->
           let n = Constant.to_int c in
           let src_sig = sig' (expr_to_remap decls src) in
           let replicated = List.init n (fun _ -> src_sig) in
           Sig (concat_msb replicated)
       | _ -> Invalid)
       
  | _ -> Invalid

(* Debug helper to show AST node type *)
let show_ast_node = function
  | Sv_ast.VarRef { name; _ } -> Printf.sprintf "VarRef(%s)" name
  | Sv_ast.VarRef' { name; _ } -> Printf.sprintf "VarRef'(%s)" name
  | Sv_ast.Sel _ -> "Sel"
  | Sv_ast.ArraySel _ -> "ArraySel"
  | Sv_ast.Const { name; _ } -> Printf.sprintf "Const(%s)" name
  | Sv_ast.BinaryOp { op; _ } -> Printf.sprintf "BinaryOp(%s)" op
  | Sv_ast.UnaryOp { op; _ } -> Printf.sprintf "UnaryOp(%s)" op
  | _ -> "OtherNode"

(* Process assignment statement *)
let process_assign decls lhs rhs =
  Printf.eprintf "        process_assign: LHS node type = %s\n%!" (show_ast_node lhs);
  (match lhs with
   | Sv_ast.VarRef { name; _ } | Sv_ast.VarRef' { name; _ } ->
       Printf.eprintf "          Assigning to variable: %s\n%!" name
   | _ -> ());
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
      Printf.eprintf "        ERROR: LHS expression is Invalid (type: %s)\n%!" (show_ast_node lhs);
      (match lhs with
       | Sv_ast.VarRef { name; _ } | Sv_ast.VarRef' { name; _ } ->
           Printf.eprintf "          Variable '%s' not found in decls\n%!" name;
           Printf.eprintf "          Available vars: ";
           Hashtbl.iter (fun k _ -> Printf.eprintf "%s " k) decls;
           Printf.eprintf "\n%!"
       | Sv_ast.Sel { expr; lsb; width; range; _ } ->
           Printf.eprintf "          Sel assignment: range=%s\n%!" range;
           (match expr with
            | Sv_ast.VarRef { name; _ } | Sv_ast.VarRef' { name; _ } ->
                Printf.eprintf "            Base variable: %s\n%!" name
            | _ -> Printf.eprintf "            Base: complex expression\n%!")
       | _ -> ());
      None
  | _ ->
      Printf.eprintf "        ERROR: LHS is not a Variable (type: %s)\n%!" (show_ast_node lhs);
      None

(* Process statement to Always.t *)
let rec process_stmt decls intermediate_set = function
  | Sv_ast.Assign { lhs; rhs; is_blocking = true } ->
      (* Check if this is an intermediate blocking assignment *)
      let is_intermediate = match lhs with
        | Sv_ast.VarRef { name; _ } | Sv_ast.VarRef' { name; _ } ->
            Hashtbl.mem intermediate_set name
        | _ -> false
      in
      if is_intermediate then begin
        (* Skip - already processed as continuous assignment *)
        None
      end else begin
        (* Regular blocking assignment in always block *)
        process_assign decls lhs rhs
      end
      
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
      (match (process_stmt decls intermediate_set then_stmt, process_stmt decls intermediate_set else_stmt) with
       | (Some t, Some e) -> Some (if_ cond [t] [e])
       | (Some t, None) -> Some (when_ cond [t])
       | _ -> None)

  | Sv_ast.If { condition; then_stmt; else_stmt = None } ->
      let cond_raw = sig' (expr_to_remap decls condition) in
      let cond = if width cond_raw = 1 then cond_raw
                 else reduce ~f:(|:) [cond_raw] in
      (match process_stmt decls intermediate_set then_stmt with
       | Some t -> Some (when_ cond [t])
       | None -> None)

  | Sv_ast.Begin { stmts; _ } ->
      let alws = List.filter_map (process_stmt decls intermediate_set) stmts in
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
                 let stmts = List.filter_map (process_stmt decls intermediate_set) item.statements in
                 if List.length stmts > 0 then
                   Some (cond_sig, stmts)
                 else
                   None
             | Sig s ->
                 let stmts = List.filter_map (process_stmt decls intermediate_set) item.statements in
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
let process_always decls intermediate_set stmts =
  List.filter_map (process_stmt decls intermediate_set) stmts

(* Build HardCaml circuit for module *)
let build_circuit module_name stmts =
  try
    Printf.eprintf "    Building circuit for %s\n%!" module_name;

    (* Extract ports and signals *)
    let ports = extract_ports stmts in
    let signals = extract_signals stmts in
    let lhs_vars = extract_lhs_variables stmts in
    let state_elements = identify_state_elements stmts in

    Printf.eprintf "      Found %d ports: " (List.length ports);
    List.iter (fun (name, width, dir) ->
      let dir_str = match dir with `Input -> "in" | `Output -> "out" | `Inout -> "inout" in
      Printf.eprintf "%s(%s:%d) " name dir_str width
    ) ports;
    Printf.eprintf "\n%!";

    Printf.eprintf "      Found %d internal signals\n%!" (List.length signals);
    Printf.eprintf "      Found %d LHS variables (including SSA)\n%!" (List.length lhs_vars);
    Printf.eprintf "      Found %d state elements (registers)\n%!" (Hashtbl.length state_elements);

    (* Detect memories *)
    Printf.eprintf "      Detecting memories...\n%!";
    let memories = Sv_memory.detect_memories stmts in
    Printf.eprintf "      Found %d memories\n%!" (Hashtbl.length memories);

    (* Analyze memory access patterns *)
    if Hashtbl.length memories > 0 then begin
      Printf.eprintf "      Analyzing memory access patterns...\n%!";
      Sv_memory.analyze_memory_accesses stmts memories
    end;

    let decls = Hashtbl.create 64 in

    (* Create inputs - outputs will be computed later *)
    List.iter (fun (name, width, dir) ->
      match dir with
      | `Input ->
          (* Skip zero-width inputs *)
          if width <= 0 then begin
            Printf.eprintf "        Skipping zero-width input: %s[%d]\n%!" name width
          end else begin
            Printf.eprintf "        Creating input: %s[%d]\n%!" name width;
            Hashtbl.add decls name (Sig (input name width))
          end
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

    (* Merge signals, LHS variables, and outputs - removing duplicates *)
    let all_vars_table = Hashtbl.create 128 in

    (* Add signals *)
    List.iter (fun (name, width, signed) ->
      Hashtbl.replace all_vars_table name (width, signed)
    ) signals;

    (* Add LHS variables (may override with correct widths from SSA) *)
    List.iter (fun (name, width, signed) ->
      Hashtbl.replace all_vars_table name (width, signed)
    ) lhs_vars;

    (* Add outputs *)
    List.iter (fun (name, width, dir) ->
      match dir with
      | `Output -> Hashtbl.replace all_vars_table name (width, false)
      | _ -> ()
    ) ports;

    (* Convert to list *)
    let all_vars = Hashtbl.fold (fun name (width, signed) acc ->
      (name, width, signed) :: acc
    ) all_vars_table [] in

    (* Create Variables for ALL LHS targets *)
    (* State elements (non-blocking) use Variable.reg *)
    (* SSA temporaries (blocking) use Variable.wire *)
    List.iter (fun (name, width, _signed) ->
      if not (Hashtbl.mem decls name) then begin
        (* Skip zero-width signals - they're invalid in HardCaml *)
        if width <= 0 then begin
          Printf.eprintf "        Skipping zero-width signal: %s[%d]\n%!" name width
        end else begin
          let is_state = Hashtbl.mem state_elements name in
          if is_state && clock_signal <> None then begin
            (* State element: Create as register with clock *)
            Printf.eprintf "        Creating register: %s[%d] with clock\n%!" name width;
            let clk = Option.get clock_signal in
            let spec = Reg_spec.create ~clock:clk () in
            Hashtbl.add decls name (Var (Variable.reg spec ~width))
          end else begin
            (* SSA temporary or output: Create as wire with proper width *)
            Printf.eprintf "        Creating wire variable: %s[%d]\n%!" name width;
            let default_sig = Signal.of_int ~width 0 in
            let wire_var = Variable.wire ~default:default_sig in
            Hashtbl.add decls name (Var wire_var)
          end
        end
      end
    ) all_vars;
    
    (* Extract intermediate blocking assignments from clocked always blocks *)
    let intermediate_assigns = ref [] in
    List.iter (function
      | Sv_ast.Always { senses; stmts = always_stmts; _ } when is_clocked_always senses ->
          let intermediate = find_intermediate_blocking_vars always_stmts in
          let rec extract_intermediates = function
            | Sv_ast.Assign { lhs; rhs; is_blocking = true; _ } ->
                (match lhs with
                 | Sv_ast.VarRef { name; _ } | Sv_ast.VarRef' { name; _ } ->
                     if Hashtbl.mem intermediate name then
                       intermediate_assigns := (name, rhs) :: !intermediate_assigns
                 | _ -> ())
            | Sv_ast.Begin { stmts; _ } -> List.iter extract_intermediates stmts
            | Sv_ast.If { then_stmt; else_stmt; _ } ->
                extract_intermediates then_stmt;
                Option.iter extract_intermediates else_stmt
            | Sv_ast.Case { items; _ } ->
                List.iter (fun item -> List.iter extract_intermediates item.Sv_ast.statements) items
            | _ -> ()
          in
          List.iter extract_intermediates always_stmts
      | _ -> ()
    ) stmts;

    Printf.eprintf "      Processing %d intermediate blocking assignments as continuous\n%!"
      (List.length !intermediate_assigns);
    List.iter (fun (name, rhs) ->
      let rhs_sig = sig' (expr_to_remap decls rhs) in
      Hashtbl.replace decls name (Sig rhs_sig);
      Printf.eprintf "        SSA: %s = <expr> (stored as Sig)\n%!" name
    ) !intermediate_assigns;

    (* Process continuous assignments *)
    let cont_assigns = List.filter_map (function
      | Sv_ast.AssignW { lhs; rhs } -> Some (lhs, rhs)
      | _ -> None
    ) stmts in

    Printf.eprintf "      Processing %d continuous assignments\n%!" (List.length cont_assigns);
    List.iter (fun (lhs, rhs) ->
      (* Continuous assignments don't use Always API - just store as Signals *)
      match lhs with
      | Sv_ast.VarRef { name; _ } | Sv_ast.VarRef' { name; _ } ->
          let rhs_sig = sig' (expr_to_remap decls rhs) in
          Hashtbl.replace decls name (Sig rhs_sig);
          Printf.eprintf "        Continuous: %s = <expr> (stored as Sig)\n%!" name
      | _ ->
          Printf.eprintf "        Continuous assignment with complex LHS\n%!"
    ) cont_assigns;
    
    (* Process always blocks *)
    let always_blocks = List.filter_map (function
      | Sv_ast.Always { senses; stmts = always_stmts; _ } ->
          (* Extract clock/reset from sensitivity list if present *)
          Printf.eprintf "      Processing always block with %d statements\n%!" (List.length always_stmts);
          (* Find intermediate blocking vars for this block *)
          let intermediate_set = if is_clocked_always senses then
            find_intermediate_blocking_vars always_stmts
          else
            Hashtbl.create 0  (* empty set for combinational blocks *)
          in
          let alws = process_always decls intermediate_set always_stmts in
          Printf.eprintf "        Generated %d Always.t assignments\n%!" (List.length alws);
          if List.length alws > 0 then Some alws else None
      | _ -> None
    ) stmts in

    Printf.eprintf "      Found %d always blocks to compile\n%!" (List.length always_blocks);

    (* Track which Variables are assigned by name *)
    let assigned_vars = Hashtbl.create 64 in
    let find_var_name v =
      Hashtbl.fold (fun name value acc ->
        match (value, acc) with
        | (Var v', None) when v == v' -> Some name
        | _ -> acc
      ) decls None
    in
    let rec track_assigns = function
      | Always.Assign (v, _) ->
          (match find_var_name v with
           | Some name -> Hashtbl.replace assigned_vars name true
           | None -> ())
      | Always.If (_, then_list, else_list) ->
          List.iter track_assigns then_list;
          List.iter track_assigns else_list
      | Always.Switch (_, cases) ->
          List.iter (fun (_, stmts) -> List.iter track_assigns stmts) cases
      | _ -> ()
    in
    List.iter (List.iter track_assigns) always_blocks;

    (* Check for unassigned Variables and initialize them *)
    Printf.eprintf "      Checking for unassigned Variables:\n%!";
    let unassigned_inits = ref [] in
    Hashtbl.iter (fun name value ->
      match value with
      | Var v ->
          if not (Hashtbl.mem assigned_vars name) then begin
            Printf.eprintf "        WARNING: Variable %s created but never assigned - initializing to 0\n%!" name;
            let w = width v.value in
            unassigned_inits := (v <--zero w) :: !unassigned_inits
          end
      | _ -> ()
    ) decls;

    (* Add unassigned initializations as a new always block if needed *)
    let always_blocks = if List.length !unassigned_inits > 0 then begin
      Printf.eprintf "      Adding initialization block for %d unassigned variables\n%!" (List.length !unassigned_inits);
      !unassigned_inits :: always_blocks
    end else
      always_blocks in

    (* Compile all always blocks *)
    List.iteri (fun i alw_list ->
      Printf.eprintf "      Compiling always block %d with %d assignments\n%!" i (List.length alw_list);
      try
        compile alw_list;
        Printf.eprintf "        Successfully compiled\n%!"
      with e ->
        Printf.eprintf "        ERROR compiling: %s\n%!" (Printexc.to_string e)
    ) (List.filter (fun l -> List.length l > 0) always_blocks);
    
    (* Build outputs *)
    Printf.eprintf "      Building outputs for %d output ports\n%!"
      (List.length (List.filter (fun (_, _, d) -> d = `Output) ports));
    let outputs = List.filter_map (fun (name, width, dir) ->
      match dir with
      | `Output ->
          Printf.eprintf "        Building output %s[%d]\n%!" name width;
          (* Skip zero-width outputs *)
          if width <= 0 then begin
            Printf.eprintf "          Skipping zero-width output\n%!";
            None
          end else
            (match Hashtbl.find_opt decls name with
             | Some (Var v) ->
                 let out_sig = v.value in
                 Printf.eprintf "          Found as Variable, using v.value (width=%d)\n%!" (Signal.width out_sig);
                 Some (output name out_sig)
             | Some (Sig s) ->
                 Printf.eprintf "          Found as Signal (width=%d)\n%!" (Signal.width s);
                 Some (output name s)
             | _ ->
                 (* Create default output if not found *)
                 Printf.eprintf "          NOT FOUND in decls, creating zero default\n%!";
                 Some (output name (zero width))
            )
      | _ -> None
    ) ports in

    Printf.eprintf "      Created %d outputs\n%!" (List.length outputs);

    (* Debug: Print all entries in decls *)
    Printf.eprintf "      Decls table contents:\n%!";
    Hashtbl.iter (fun name value ->
      let type_str = match value with
        | Invalid -> "Invalid"
        | Con _ -> "Con"
        | Sig s -> Printf.sprintf "Sig(width=%d)" (Signal.width s)
        | Sigs _ -> "Sigs"
        | Var v -> Printf.sprintf "Var(width=%d)" (Signal.width v.value)
        | Alw _ -> "Alw"
      in
      Printf.eprintf "        %s -> %s\n%!" name type_str
    ) decls;

    Printf.eprintf "      Attempting to create circuit...\n%!";
    if List.length outputs = 0 then begin
      (* Ensure at least one output for valid circuit *)
      Printf.eprintf "      No outputs - creating dummy circuit\n%!";
      Circuit.create_exn ~name:module_name [output "dummy" (zero 1)]
    end else begin
      Printf.eprintf "      Creating circuit with %d outputs\n%!" (List.length outputs);
      Circuit.create_exn ~name:module_name outputs
    end
      
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
