(* ============================================================================
   sv_gen_struct.ml - Complete structural code generator with full type support
   ============================================================================
   DEPRECATED: this file is no longer on the active code path. The live
   structural pipeline lives in sv_tran_struct.ml; sv_main_unified.ml dispatches
   to it for both the `structural` and `xilinx_rtl` backends.

   The local helpers below (is_clock_signal, is_reset_signal, detect_edge,
   analyze_sensitivity_detailed, detect_reset_pattern, classify_always_block)
   contain name-based clock/reset detection. DO NOT extend or revive them.
   Any new clock/reset classification work should reuse the structural
   classifier in sv_tran_struct.ml, which derives clock/reset/sync/async
   purely from the always-block sensitivity list and body if-pattern.
   ============================================================================ *)

open Sv_ast

let debug = ref false
let warnings = ref []
let inst_counter = ref 0

(* ============================================================================
   CONTEXT AND VARIABLE TRACKING
   ============================================================================ *)

type var_info = {
  name: string;
  dtype_ref: sv_type option;
  dtype_name: string;
  is_reg: bool;
  width: int;
}

type structural_context = {
  variables: (string, var_info) Hashtbl.t;
  wires: string list ref;
  regs: string list ref;
  instances: string list ref;
  mutable in_sequential: bool;
  mutable current_clock: string option;
  mutable current_clock_edge: [`Posedge | `Negedge] option;
  mutable current_reset: string option;
}

let create_context () = {
  variables = Hashtbl.create 100;
  wires = ref [];
  regs = ref [];
  instances = ref [];
  in_sequential = false;
  current_clock = None;
  current_clock_edge = None;
  current_reset = None;
}

(* ============================================================================
   UTILITY FUNCTIONS
   ============================================================================ *)

(* Generate unique instance names *)
let gen_inst_name prefix =
  incr inst_counter;
  Printf.sprintf "%s_%d" prefix !inst_counter

(* Add a warning message *)
let add_warning msg =
  warnings := msg :: !warnings;
  if !debug then Printf.eprintf "WARNING: %s\n" msg

(* Get all warnings *)
let get_warnings () = List.rev !warnings

(* Clear warnings *)
let clear_warnings () = 
  warnings := [];
  inst_counter := 0

(* Try to evaluate constant expressions *)
let rec try_eval_const expr =
  match expr with
  | Const { name; _ } ->
      (try
        if String.contains name '\'' then
          let parts = String.split_on_char '\'' name in
          match parts with
          | _ :: rest ->
              let value_str = String.concat "'" rest in
              if String.length value_str >= 2 then
                let base = value_str.[0] in
                let num_str = String.sub value_str 1 (String.length value_str - 1) in
                (match base with
                | 's' when String.length value_str > 2 ->
                    let actual_base = value_str.[1] in
                    let actual_num = String.sub value_str 2 (String.length value_str - 2) in
                    (match actual_base with
                    | 'h' -> Some (int_of_string ("0x" ^ actual_num))
                    | 'd' -> Some (int_of_string actual_num)
                    | _ -> None)
                | 'h' -> Some (int_of_string ("0x" ^ num_str))
                | 'd' -> Some (int_of_string num_str)
                | 'b' -> Some (int_of_string ("0b" ^ num_str))
                | _ -> Some (int_of_string value_str))
              else Some (int_of_string value_str)
          | _ -> Some (int_of_string name)
        else Some (int_of_string name)
      with _ -> None)
  | BinaryOp { op; lhs; rhs; _ } ->
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

(* ============================================================================
   TYPE RESOLUTION
   ============================================================================ *)

(* Extract packages from netlist *)
let extract_packages = function
  | Netlist modules -> 
      List.filter (function Package _ -> true | _ -> false) modules
  | _ -> []

(* Extract type parameters - look for ParamTypeType in RefType *)
let extract_type_params stmts =
  List.filter_map (function
    | Var { name; dtype_ref = Some (RefType { refdtype_ref = Some (ParamTypeType _); _ }); 
            var_type = "GPARAM"; _ } ->
        Some name
    | _ -> None
  ) stmts

(* Resolve enum constant name to its integer value *)
let resolve_enum_constant packages const_name =
  List.find_map (function
    | Package { stmts; _ } ->
        List.find_map (function
          | Typedef { dtype_ref = Some (EnumType { items; _ }); _ } ->
              List.find_map (fun (name, value) ->
                if name = const_name then
                  try_eval_const (Const { name = value; dtype_ref = None })
                else None
              ) items
          | _ -> None
        ) stmts
    | _ -> None
  ) packages

(* Fallback: Resolve type from packages *)
let resolve_type_from_packages packages type_name =
  List.find_map (function
    | Package { stmts; _ } ->
        List.find_map (function
          | Typedef { name; dtype_ref } when name = type_name -> dtype_ref
          | _ -> None
        ) stmts
    | _ -> None
  ) packages

(* Parse range string to width *)
let parse_range_width range =
  try
    let parts = String.split_on_char ':' range in
    match parts with
    | [msb; lsb] -> 
        let m = int_of_string (String.trim msb) in
        let l = int_of_string (String.trim lsb) in
        abs (m - l) + 1
    | _ -> 1
  with _ -> 1

(* Get width string with full resolution *)
let rec get_width_str_resolved packages type_params dtype_ref dtype_name =
  match dtype_ref with
  (* Type parameter - no explicit width *)
  | Some (RefType { refdtype_ref = Some (ParamTypeType _); _ }) ->
      ""
  
  (* RefType with resolved type - use it directly! *)
  | Some (RefType { dtype_ref = Some resolved_type; _ }) ->
      get_width_str_resolved packages type_params (Some resolved_type) ""
  
  (* RefType without resolved type - try package lookup *)
  | Some (RefType { name; _ }) ->
      (match resolve_type_from_packages packages name with
      | Some resolved -> get_width_str_resolved packages type_params (Some resolved) ""
      | None -> "")
  
  (* Array types *)
  | Some (ArrayType { range; _ }) -> Printf.sprintf " [%s]" range
  | Some (PackArrayType { range; _ }) -> Printf.sprintf " [%s]" range
  
  (* Basic type with range *)
  | Some (BasicType { range = Some r; _ }) -> Printf.sprintf " [%s]" r
  
  (* Enum type - calculate width from number of items *)
  | Some (EnumType { items; _ }) ->
      let num_items = List.length items in
      let width = max 1 (int_of_float (ceil (log (float_of_int num_items) /. log 2.0))) in
      if width > 1 then Printf.sprintf " [%d:0]" (width - 1) else ""
  
  (* Struct type - calculate total width for packed structs *)
  | Some (StructType { members; packed; _ }) when packed ->
      let total_width = List.fold_left (fun acc member ->
        match member with
        | MemberType { dtype_ref = Some (BasicType { range = Some r; _ }); _ } ->
            acc + parse_range_width r
        | MemberType { dtype_ref = Some (BasicType { range = None; _ }); _ } ->
            acc + 1
        | _ -> acc
      ) 0 members in
      if total_width > 1 then Printf.sprintf " [%d:0]" (total_width - 1) else ""
  
  (* ParamTypeType - no explicit width *)
  | Some (ParamTypeType _) -> ""
  
  (* Fallback to dtype_name *)
  | _ when dtype_name <> "" && String.contains dtype_name '[' ->
      (try
        let start_idx = String.index dtype_name '[' in
        let end_idx = String.index dtype_name ']' in
        " " ^ String.sub dtype_name start_idx (end_idx - start_idx + 1)
      with _ -> "")
  
  | _ -> ""

(* Get base type keyword *)
let get_base_type_resolved packages type_params dtype_ref dtype_name =
  match dtype_ref with
  (* Array types - return element type *)
  | Some (ArrayType { base = BasicType { keyword; _ }; _ }) -> keyword
  | Some (PackArrayType { base = BasicType { keyword; _ }; _ }) -> keyword
  
  (* Basic type *)
  | Some (BasicType { keyword; _ }) -> keyword
  
  (* ParamTypeType - return the type parameter name *)
  | Some (ParamTypeType { name; _ }) -> name
  
  (* RefType with ParamTypeType - it's a type parameter *)
  | Some (RefType { name; refdtype_ref = Some (ParamTypeType _); _ }) -> name
  
  (* RefType with resolved BasicType - use keyword *)
  | Some (RefType { dtype_ref = Some (BasicType { keyword; _ }); _ }) -> keyword
  
  (* RefType - use name *)
  | Some (RefType { name; _ }) -> name
  
  (* Enum/Struct - strip package prefix *)
  | Some (EnumType { name; _ }) | Some (StructType { name; _ }) -> 
      (match String.split_on_char ':' name with
      | [_; _; tname] -> tname
      | _ -> name)
  
  (* Fallback to dtype_name *)
  | _ when dtype_name <> "" && dtype_name <> "logic" -> 
      if String.contains dtype_name '[' then
        String.trim (String.sub dtype_name 0 (String.index dtype_name '['))
      else
        dtype_name
  
  | _ -> "logic"

(* ============================================================================
   VARIABLE MANAGEMENT
   ============================================================================ *)

(* Extract bit width from type *)
let get_bit_width dtype_ref dtype_name =
  match dtype_ref with
  | Some (BasicType { range = Some r; _ }) -> parse_range_width r
  | Some (ArrayType { range; _ }) -> parse_range_width range
  | _ -> 1

(* Add variable to context *)
let add_var ctx name dtype_ref dtype_name is_reg =
  if Hashtbl.mem ctx.variables name then
    add_warning (Printf.sprintf "Variable redefinition: %s" name);
  let width = get_bit_width dtype_ref dtype_name in
  Hashtbl.replace ctx.variables name { name; dtype_ref; dtype_name; is_reg; width }

(* Get variable info *)
let get_var ctx name =
  try Some (Hashtbl.find ctx.variables name)
  with Not_found -> None

(* Infer width from expression - NOW USES dtype_ref! *)
let rec infer_width ctx = function
  (* Use dtype_ref when available - this is authoritative! *)
  | UnaryOp { dtype_ref = Some (BasicType { range = Some r; _ }); _ }
  | BinaryOp { dtype_ref = Some (BasicType { range = Some r; _ }); _ }
  | VarRef { dtype_ref = Some (BasicType { range = Some r; _ }); _ } ->
      parse_range_width r
  
  | VarRef { name; _ } ->
      (match get_var ctx name with
      | Some { width; _ } -> width
      | None -> 1)
  
  | Const { name; _ } ->
      if String.contains name '\'' then
        (try
          let parts = String.split_on_char '\'' name in
          match parts with
          | width_str :: _ -> int_of_string (String.trim width_str)
          | _ -> 32
        with _ -> 32)
      else 32
  
  (* Fallback cases when dtype_ref not available *)
  | BinaryOp { op = "MUL" | "MULS"; lhs; rhs; _ } ->
      (* Multiplication result can be wider *)
      infer_width ctx lhs + infer_width ctx rhs
  
  | BinaryOp { op = "SHIFTL" | "SHIFTR" | "SHIFTRS"; lhs; _ } ->
      (* Shift result width = LHS width *)
      infer_width ctx lhs
  
  | BinaryOp { lhs; rhs; _ } ->
      max (infer_width ctx lhs) (infer_width ctx rhs)
  
  | UnaryOp { operand; _ } ->
      infer_width ctx operand
  
  | Sel { width = Some w; _ } ->
      (match try_eval_const w with Some n -> n | None -> 1)
  
  | Sel { lsb = Some _; width = None; _ } ->
      1  (* Single bit select *)
  
  | ArraySel { expr; _ } ->
      (* Array element access returns base element width *)
      (match get_var ctx (match expr with VarRef { name; _ } -> name | _ -> "") with
      | Some { dtype_ref = Some (ArrayType { base = BasicType { range = Some r; _ }; _ }); _ } ->
          parse_range_width r
      | _ -> infer_width ctx expr)
  
  | Cond { then_val; else_val; _ } ->
      max (infer_width ctx then_val) (infer_width ctx else_val)
  
  | Concat { parts } ->
      (* Sum of all part widths *)
      List.fold_left (fun acc p -> acc + infer_width ctx p) 0 parts
  
  | Replicate { src; count; _ } ->
      (match try_eval_const count with
      | Some n -> n * infer_width ctx src
      | None -> infer_width ctx src)
  
  | _ -> 1

(* Check if an expression depends on a variable (for loop detection) *)
let rec expr_depends_on ctx var_name = function
  | VarRef { name; _ } -> name = var_name
  | BinaryOp { lhs; rhs; _ } -> 
      expr_depends_on ctx var_name lhs || expr_depends_on ctx var_name rhs
  | UnaryOp { operand; _ } -> 
      expr_depends_on ctx var_name operand
  | Cond { condition; then_val; else_val } ->
      expr_depends_on ctx var_name condition ||
      expr_depends_on ctx var_name then_val ||
      expr_depends_on ctx var_name else_val
  | Sel { expr; _ } | ArraySel { expr; _ } ->
      expr_depends_on ctx var_name expr
  | Concat { parts } ->
      List.exists (expr_depends_on ctx var_name) parts
  | Replicate { src; _ } ->
      expr_depends_on ctx var_name src
  | _ -> false

(* ============================================================================
   ALWAYS BLOCK CLASSIFICATION
   ============================================================================ *)

type always_block_type =
  | Sequential of {
      clock: string;
      clock_edge: [`Posedge | `Negedge];
      reset: (string * [`Async | `Sync]) option;
    }
  | Combinational
  | Latch
  | Unsynthesizable of string

(* Detect edge type from sensitivity *)
let detect_edge edge_str =
  let lower = String.lowercase_ascii edge_str in
  if String.contains lower 'p' || lower = "pos" || lower = "posedge" then `Posedge
  else if String.contains lower 'n' || lower = "neg" || lower = "negedge" then `Negedge
  else failwith ("Unknown edge type: " ^ edge_str)

(* Detect clock signals *)
let is_clock_signal name =
  let lower = String.lowercase_ascii name in
  lower = "clk" || lower = "clock" || 
  String.starts_with ~prefix:"clk" lower

(* Detect reset signals *)
let is_reset_signal name =
  let lower = String.lowercase_ascii name in
  lower = "rst" || lower = "reset" || lower = "rstn" || lower = "rst_n" ||
  String.starts_with ~prefix:"rst" lower

(* Analyze sensitivity list *)
let analyze_sensitivity_detailed senses =
  let clocks = ref [] in
  let resets = ref [] in
  let other_signals = ref [] in
  
  let rec process_sense = function
    | SenTree items ->
        List.iter process_sense items
    | SenItem { edge_str; signal } ->
        (match signal with
        | VarRef { name; _ } ->
            let edge = detect_edge edge_str in
            if is_clock_signal name then
              clocks := (name, edge) :: !clocks
            else if is_reset_signal name then
              resets := (name, edge) :: !resets
            else
              other_signals := (name, edge) :: !other_signals
        | _ -> ())
    | _ -> ()
  in
  
  List.iter process_sense senses;
  (!clocks, !resets, !other_signals)

(* Detect reset pattern from statements *)
let rec detect_reset_pattern stmt =
  let rec has_only_resets = function
    | Assign { rhs = Const { name; _ }; _ } when name = "0" || name = "'0" || name = "1'b0" -> true
    | Begin { stmts; _ } -> List.for_all has_only_resets stmts
    | _ -> false
  in
  
  match stmt with
  | If { condition; then_stmt; _ } ->
      (match condition with
      | VarRef { name; _ } when is_reset_signal name ->
          if has_only_resets then_stmt then Some (name, `Sync) else None
      | UnaryOp { op = "LOGNOT"; operand = VarRef { name; _ }; _ } when is_reset_signal name ->
          if has_only_resets then_stmt then Some (name, `Sync) else None
      | _ -> None)
  | Begin { stmts; _ } ->
      (match stmts with first :: _ -> detect_reset_pattern first | [] -> None)
  | _ -> None

(* Classify always block type *)
let classify_always_block always_type senses stmts =
  match always_type with
  | "always_comb" -> Combinational
  | "always_latch" -> Latch
  | "always_ff" ->
      let (clocks, resets, _) = analyze_sensitivity_detailed senses in
      (match clocks, resets with
      | (clk, clk_edge) :: [], [] ->
          Sequential { clock = clk; clock_edge = clk_edge; reset = None }
      | (clk, clk_edge) :: [], (rst, _) :: [] ->
          Sequential { clock = clk; clock_edge = clk_edge; reset = Some (rst, `Async) }
      | _ -> Unsynthesizable "always_ff must have exactly one clock")
  | "always" ->
      let (clocks, resets, others) = analyze_sensitivity_detailed senses in
      if List.length clocks > 0 then
        (match clocks, resets with
        | (clk, clk_edge) :: [], [] ->
            let sync_reset = detect_reset_pattern (Begin { name = ""; stmts; is_generate = false }) in
            Sequential { clock = clk; clock_edge = clk_edge; reset = sync_reset }
        | (clk, clk_edge) :: [], (rst, _) :: [] ->
            Sequential { clock = clk; clock_edge = clk_edge; reset = Some (rst, `Async) }
        | _ :: _ :: _, _ -> Unsynthesizable "Multiple clocks not supported"
        | _, _ :: _ :: _ -> Unsynthesizable "Multiple resets not supported"
        | _ -> Unsynthesizable "Invalid clock configuration")
      else if List.length others > 0 || List.length senses = 0 then
        Combinational
      else
        Unsynthesizable "Invalid sensitivity list"
  | _ -> Unsynthesizable ("Unknown always type: " ^ always_type)

(* ============================================================================
   HARDWARE PRIMITIVES
   ============================================================================ *)

(* Generate structural primitive for binary operation *)
let gen_binary_op ctx op lhs rhs result_wire width =
  let inst_name = gen_inst_name "op" in
  let module_name = match op with
    | "ADD" -> "adder"
    | "SUB" -> "subtractor"
    | "MUL" -> "multiplier"
    | "MULS" -> "multiplier_signed"
    | "DIV" -> "divider"
    | "DIVS" -> "divider_signed"
    | "AND" -> "bitwise_and"
    | "OR" -> "bitwise_or"
    | "XOR" -> "bitwise_xor"
    | "EQ" -> "comparator_eq"
    | "NEQ" -> "comparator_neq"
    | "LT" -> "comparator_lt"
    | "LTS" -> "comparator_lt_signed"
    | "GT" -> "comparator_gt"
    | "GTS" -> "comparator_gt_signed"
    | "LTE" -> "comparator_lte"
    | "LTES" -> "comparator_lte_signed"
    | "GTE" -> "comparator_gte"
    | "GTES" -> "comparator_gte_signed"
    | "SHIFTL" -> "shifter_left"
    | "SHIFTR" -> "shifter_right"
    | "SHIFTRS" -> "shifter_right_arith"
    | _ -> "binary_op"
  in
  ctx.instances := 
    (Printf.sprintf "  %s #(.WIDTH(%d)) %s (.a(%s), .b(%s), .out(%s));" 
      module_name width inst_name lhs rhs result_wire) :: !(ctx.instances);
  result_wire

(* Generate structural primitive for unary operation *)
let gen_unary_op ctx op operand result_wire width =
  let inst_name = gen_inst_name "op" in
  let module_name = match op with
    | "NOT" -> "bitwise_not"
    | "LOGNOT" -> "logical_not"
    | "REDAND" -> "reduction_and"
    | "REDOR" -> "reduction_or"
    | "REDXOR" -> "reduction_xor"
    | "NEGATE" -> "negator"
    | _ -> "unary_op"
  in
  ctx.instances := 
    (Printf.sprintf "  %s #(.WIDTH(%d)) %s (.in(%s), .out(%s));" 
      module_name width inst_name operand result_wire) :: !(ctx.instances);
  result_wire

(* Generate structural multiplexer *)
let gen_mux ctx sel in0 in1 result_wire width =
  let inst_name = gen_inst_name "mux" in
  ctx.instances := 
    (Printf.sprintf "  mux2 #(.WIDTH(%d)) %s (.sel(%s), .in0(%s), .in1(%s), .out(%s));" 
      width inst_name sel in0 in1 result_wire) :: !(ctx.instances);
  result_wire

(* Generate D flip-flop with enable *)
let gen_dff_en ctx clk clk_edge rst en d q width reset_val =
  let inst_name = gen_inst_name "dff" in
  
  let (clk_sig, module_name) = match clk_edge with
    | Some `Posedge -> (clk, "dff_en")
    | Some `Negedge -> (clk, "dffn_en")
    | None -> (clk, "dff_en")
  in
  
  let rst_port = match rst with
    | Some r -> Printf.sprintf ".rst(%s)" r
    | None -> ".rst(1'b0)"
  in
  
  let en_port = match en with
    | Some e -> Printf.sprintf ".en(%s)" e
    | None -> ".en(1'b1)"
  in
  
  ctx.instances := 
    (Printf.sprintf "  %s #(.WIDTH(%d), .RESET_VAL(%d)) %s (.clk(%s), %s, %s, .d(%s), .q(%s));" 
      module_name width reset_val inst_name clk_sig rst_port en_port d q) :: !(ctx.instances);
  q

(* Generate latch primitive *)
let gen_latch_en ctx en d q width =
  let inst_name = gen_inst_name "latch" in
  
  let en_port = match en with
    | Some e -> Printf.sprintf ".en(%s)" e
    | None -> ".en(1'b1)"
  in
  
  ctx.instances := 
    (Printf.sprintf "  latch_en #(.WIDTH(%d)) %s (%s, .d(%s), .q(%s));" 
      width inst_name en_port d q) :: !(ctx.instances);
  q

(* ============================================================================
   EXPRESSION CONVERSION
   ============================================================================ *)

(* Convert expression to structural form *)
let rec structural_expr ctx expr =
  match expr with
  | VarRef { name; _ } -> name
  
  | Const { name; _ } -> name
  
  | UnaryOp { op = "EXTENDS"; operand; dtype_ref } ->
      let operand_wire = structural_expr ctx operand in
      let result_wire = gen_inst_name "wire" in
      
      let input_width = infer_width ctx operand in
      let output_width = infer_width ctx (UnaryOp { op = "EXTENDS"; operand; dtype_ref }) in
      
      ctx.wires := (Printf.sprintf "  logic [%d:0] %s;" (output_width-1) result_wire) :: !(ctx.wires);
      
      let inst_name = gen_inst_name "ext" in
      ctx.instances := 
        (Printf.sprintf "  sign_extender #(.WIDTH_IN(%d), .WIDTH_OUT(%d)) %s (.in(%s), .out(%s));" 
          input_width output_width inst_name operand_wire result_wire) :: !(ctx.instances);
      result_wire
  
  | UnaryOp { op = "EXTEND"; operand; dtype_ref } ->
      let operand_wire = structural_expr ctx operand in
      let result_wire = gen_inst_name "wire" in
      
      let input_width = infer_width ctx operand in
      let output_width = infer_width ctx (UnaryOp { op = "EXTEND"; operand; dtype_ref }) in
      
      ctx.wires := (Printf.sprintf "  logic [%d:0] %s;" (output_width-1) result_wire) :: !(ctx.wires);
      
      let inst_name = gen_inst_name "ext" in
      ctx.instances := 
        (Printf.sprintf "  zero_extender #(.WIDTH_IN(%d), .WIDTH_OUT(%d)) %s (.in(%s), .out(%s));" 
          input_width output_width inst_name operand_wire result_wire) :: !(ctx.instances);
      result_wire
  
  | UnaryOp { op; operand; dtype_ref } ->
      let operand_wire = structural_expr ctx operand in
      let result_wire = gen_inst_name "wire" in
      let width = infer_width ctx (UnaryOp { op; operand; dtype_ref }) in
      ctx.wires := (Printf.sprintf "  logic [%d:0] %s;" (width-1) result_wire) :: !(ctx.wires);
      gen_unary_op ctx op operand_wire result_wire width
  
  | BinaryOp { op; lhs; rhs; dtype_ref } ->
      let lhs_wire = structural_expr ctx lhs in
      let rhs_wire = structural_expr ctx rhs in
      let result_wire = gen_inst_name "wire" in
      let width = infer_width ctx (BinaryOp { op; lhs; rhs; dtype_ref }) in
      ctx.wires := (Printf.sprintf "  logic [%d:0] %s;" (width-1) result_wire) :: !(ctx.wires);
      gen_binary_op ctx op lhs_wire rhs_wire result_wire width
  
  | Cond { condition; then_val; else_val } ->
      let sel = structural_expr ctx condition in
      let in1 = structural_expr ctx then_val in
      let in0 = structural_expr ctx else_val in
      let result_wire = gen_inst_name "wire" in
      let width = max (infer_width ctx then_val) (infer_width ctx else_val) in
      ctx.wires := (Printf.sprintf "  logic [%d:0] %s;" (width-1) result_wire) :: !(ctx.wires);
      gen_mux ctx sel in0 in1 result_wire width
  
  | Sel { expr; lsb; width; _ } ->
      let base = structural_expr ctx expr in
      (match lsb, width with
      | Some l, Some w ->
          let lsb_str = structural_expr ctx l in
          let width_str = structural_expr ctx w in
          Printf.sprintf "%s[%s +: %s]" base lsb_str width_str
      | Some l, None ->
          let lsb_str = structural_expr ctx l in
          Printf.sprintf "%s[%s]" base lsb_str
      | _ -> base)
  
  | ArraySel { expr; index } ->
      let base = structural_expr ctx expr in
      let idx = structural_expr ctx index in
      Printf.sprintf "%s[%s]" base idx
  
  | Concat { parts } ->
      let part_strs = List.map (structural_expr ctx) parts in
      Printf.sprintf "{%s}" (String.concat ", " part_strs)
  
  | Replicate { src; count; _ } ->
      let src_str = structural_expr ctx src in
      let count_str = structural_expr ctx count in
      Printf.sprintf "{%s{%s}}" count_str src_str
  
  | FuncRef { name = "clog2"; args } ->
      let arg_str = match args with
        | arg :: _ -> structural_expr ctx arg
        | [] -> "1"
      in
      Printf.sprintf "$clog2(%s)" arg_str
  
  | FuncRef { name; _ } ->
      add_warning (Printf.sprintf "Unsupported function call: %s" name);
      ctx.instances := 
        (Printf.sprintf "  // UNSUPPORTED: function call %s(...)" name) :: !(ctx.instances);
      "/* func_result */"
  
  | TaskRef { name; _ } ->
      add_warning (Printf.sprintf "Unsupported task call: %s" name);
      ctx.instances := 
        (Printf.sprintf "  // UNSUPPORTED: task call %s(...)" name) :: !(ctx.instances);
      "/* task_result */"
  
  | _ ->
      add_warning "Unsupported expression in structural conversion";
      "/* unsupported */"

(* ============================================================================
   STATEMENT CONVERSION
   ============================================================================ *)

(* Convert assignment to structural form *)
let structural_assign ctx lhs rhs is_sequential =
  let lhs_name = match lhs with
    | VarRef { name; _ } -> name
    | Sel _ | ArraySel _ -> structural_expr ctx lhs
    | _ -> "unknown"
  in
  let rhs_wire = structural_expr ctx rhs in
  
  if is_sequential then begin
    let width = match get_var ctx lhs_name with
      | Some { width; _ } -> width
      | None -> infer_width ctx rhs
    in
    let d_wire = rhs_wire in
    let q_wire = lhs_name in
    match ctx.current_clock with
    | Some clk ->
        let _ = gen_dff_en ctx clk ctx.current_clock_edge ctx.current_reset None d_wire q_wire width 0 in
        ()
    | None ->
        add_warning "Sequential assignment without clock context"
  end else begin
    (* Only create assign if RHS is not the same as LHS *)
    if rhs_wire <> lhs_name then
      ctx.instances := 
        (Printf.sprintf "  assign %s = %s;" lhs_name rhs_wire) :: !(ctx.instances)
  end

(* Convert if statement to structural muxes *)
let rec structural_if ctx condition then_stmt else_stmt =
  if ctx.in_sequential then begin
    let is_enable_condition = match condition with
      | VarRef { name; _ } when not (is_reset_signal name) -> true
      | _ -> false
    in
    
    if is_enable_condition then begin
      let enable_sig = match condition with
        | VarRef { name; _ } -> Some name
        | _ -> None
      in
      
      match then_stmt with
      | Assign { lhs; rhs; _ } ->
          let lhs_name = match lhs with
            | VarRef { name; _ } -> name
            | _ -> structural_expr ctx lhs
          in
          let rhs_wire = structural_expr ctx rhs in
          let width = match get_var ctx lhs_name with
            | Some { width; _ } -> width
            | None -> infer_width ctx rhs
          in
          
          (match ctx.current_clock, enable_sig with
          | Some clk, Some en ->
              let _ = gen_dff_en ctx clk ctx.current_clock_edge ctx.current_reset (Some en) rhs_wire lhs_name width 0 in
              ()
          | Some clk, None ->
              let _ = gen_dff_en ctx clk ctx.current_clock_edge ctx.current_reset None rhs_wire lhs_name width 0 in
              ()
          | None, _ ->
              add_warning "Enable condition in sequential block without clock")
          
      | Begin { stmts; _ } ->
          List.iter (function
            | Assign { lhs; rhs; _ } ->
                let lhs_name = match lhs with
                  | VarRef { name; _ } -> name
                  | _ -> structural_expr ctx lhs
                in
                let rhs_wire = structural_expr ctx rhs in
                let width = match get_var ctx lhs_name with
                  | Some { width; _ } -> width
                  | None -> infer_width ctx rhs
                in
                (match ctx.current_clock, enable_sig with
                | Some clk, Some en ->
                    let _ = gen_dff_en ctx clk ctx.current_clock_edge ctx.current_reset (Some en) rhs_wire lhs_name width 0 in
                    ()
                | _ -> ())
            | _ -> ()
          ) stmts
          
      | _ -> ()
    end else begin
      structural_if_as_mux ctx condition then_stmt else_stmt
    end
  end else begin
    structural_if_as_mux ctx condition then_stmt else_stmt
  end

(* Helper: Convert if to mux tree *)
and structural_if_as_mux ctx condition then_stmt else_stmt =
  let cond_wire = structural_expr ctx condition in
  
  let extract_assigns stmt =
    match stmt with
    | Assign { lhs; rhs; _ } -> [(lhs, rhs)]
    | Begin { stmts; _ } ->
        List.filter_map (function
          | Assign { lhs; rhs; _ } -> Some (lhs, rhs)
          | _ -> None
        ) stmts
    | _ -> []
  in
  
  let then_assigns = extract_assigns then_stmt in
  let else_assigns = match else_stmt with
    | Some stmt -> extract_assigns stmt
    | None -> []
  in
  
  let process_var (lhs, then_rhs) =
    let lhs_name = match lhs with
      | VarRef { name; _ } -> name
      | _ -> structural_expr ctx lhs
    in
    
    let else_rhs = 
      try
        let (_, rhs) = List.find (fun (l, _) ->
          match l with
          | VarRef { name; _ } -> name = lhs_name
          | _ -> false
        ) else_assigns in
        rhs
      with Not_found ->
        if ctx.in_sequential then
          VarRef { name = lhs_name; access = "RD"; dtype_ref = None }
        else begin
          add_warning (Printf.sprintf "Incomplete if for %s - using default 0" lhs_name);
          Const { name = "'0"; dtype_ref = None }
        end
    in
    
    let then_wire = structural_expr ctx then_rhs in
    let else_wire = structural_expr ctx else_rhs in
    
    if not ctx.in_sequential then begin
      if expr_depends_on ctx lhs_name then_rhs then
        failwith (Printf.sprintf "Combinational loop: %s depends on itself in then branch" lhs_name);
      if expr_depends_on ctx lhs_name else_rhs then
        failwith (Printf.sprintf "Combinational loop: %s depends on itself in else branch" lhs_name)
    end;
    
    let result_wire = gen_inst_name "wire" in
    let width = match get_var ctx lhs_name with
      | Some { width; _ } -> width
      | None -> max (infer_width ctx then_rhs) (infer_width ctx else_rhs)
    in
    ctx.wires := (Printf.sprintf "  logic [%d:0] %s;" (width-1) result_wire) :: !(ctx.wires);
    
    let _ = gen_mux ctx cond_wire else_wire then_wire result_wire width in
    
    structural_assign ctx lhs (VarRef { name = result_wire; access = "RD"; dtype_ref = None }) ctx.in_sequential
  in
  
  List.iter process_var then_assigns

(* Convert case statement to structural mux tree *)
let rec structural_case ctx expr items =
  let expr_wire = structural_expr ctx expr in
  
  let assigned_vars = ref [] in
  List.iter (fun item ->
    List.iter (function
      | Assign { lhs = VarRef { name; _ }; _ } ->
          if not (List.mem name !assigned_vars) then
            assigned_vars := name :: !assigned_vars
      | _ -> ()
    ) item.statements
  ) items;
  
  List.iter (fun var_name ->
    let width = match get_var ctx var_name with
      | Some { width; _ } -> width
      | None -> 
          add_warning (Printf.sprintf "Cannot determine width for %s, using 32" var_name);
          32
    in
    
    let rec build_mux_chain items default_val =
      match items with
      | [] -> default_val
      | item :: rest ->
          match item.conditions with
          | [] ->
              let case_val = List.find_map (function
                | Assign { lhs = VarRef { name; _ }; rhs; _ } when name = var_name -> Some rhs
                | _ -> None
              ) item.statements in
              (match case_val with
              | Some rhs -> structural_expr ctx rhs
              | None -> default_val)
          | cond :: _ ->
              let cond_wire = structural_expr ctx cond in
              let eq_wire = gen_inst_name "wire" in
              ctx.wires := (Printf.sprintf "  logic %s;" eq_wire) :: !(ctx.wires);
              let _ = gen_binary_op ctx "EQ" expr_wire cond_wire eq_wire 1 in
              
              let case_val = List.find_map (function
                | Assign { lhs = VarRef { name; _ }; rhs; _ } when name = var_name -> Some rhs
                | _ -> None
              ) item.statements in
              
              match case_val with
              | Some rhs ->
                  let rhs_wire = structural_expr ctx rhs in
                  let next_default = build_mux_chain rest default_val in
                  let result_wire = gen_inst_name "wire" in
                  ctx.wires := (Printf.sprintf "  logic [%d:0] %s;" (width-1) result_wire) :: !(ctx.wires);
                  let _ = gen_mux ctx eq_wire next_default rhs_wire result_wire width in
                  result_wire
              | None ->
                  build_mux_chain rest default_val
    in
    
    let default_wire = 
      if ctx.in_sequential then begin
        var_name
      end else begin
        let temp_wire = gen_inst_name "wire" in
        ctx.wires := (Printf.sprintf "  logic [%d:0] %s;" (width-1) temp_wire) :: !(ctx.wires);
        ctx.instances := (Printf.sprintf "  assign %s = %d'h0;" temp_wire width) :: !(ctx.instances);
        temp_wire
      end
    in
    
    let final_wire = build_mux_chain items default_wire in
    
    if final_wire <> var_name then begin
      ctx.instances := 
        (Printf.sprintf "  assign %s = %s;" var_name final_wire) :: !(ctx.instances)
    end
  ) !assigned_vars

(* Convert always_latch block to structural latches *)
let structural_latch ctx stmts =
  add_warning "Converting always_latch block - ensure this is intentional";
  
  let enable_sig = match stmts with
    | If { condition; _ } :: _ ->
        (match condition with
        | VarRef { name; _ } -> Some name
        | UnaryOp { op = "LOGNOT"; operand = VarRef { name; _ }; _ } -> Some name
        | _ -> None)
    | _ -> None
  in
  
  let rec extract_latch_assigns = function
    | If { then_stmt; _ } -> extract_latch_assigns then_stmt
    | Assign { lhs; rhs; _ } -> [(lhs, rhs)]
    | Begin { stmts; _ } ->
        List.concat (List.map extract_latch_assigns stmts)
    | _ -> []
  in
  
  let assigns = List.concat (List.map extract_latch_assigns stmts) in
  
  List.iter (fun (lhs, rhs) ->
    let lhs_name = match lhs with
      | VarRef { name; _ } -> name
      | _ -> structural_expr ctx lhs
    in
    let rhs_wire = structural_expr ctx rhs in
    let width = match get_var ctx lhs_name with
      | Some { width; _ } -> width
      | None -> infer_width ctx rhs
    in
    
    let _ = gen_latch_en ctx enable_sig rhs_wire lhs_name width in
    ()
  ) assigns

(* Validate that statements can be converted to hardware *)
let rec validate_hardware_stmt ctx = function
  | Assign _ | AssignW _ -> true

  | If { condition; then_stmt; else_stmt } ->
      let cond_valid = validate_hardware_expr ctx condition in
      let then_valid = validate_hardware_stmt ctx then_stmt in
      let else_valid = match else_stmt with 
        | Some s -> validate_hardware_stmt ctx s 
        | None -> true
      in
      cond_valid && then_valid && else_valid

  | Case { expr; items } ->
      validate_hardware_expr ctx expr &&
      List.for_all (fun { conditions; statements } ->
        List.for_all (validate_hardware_expr ctx) conditions &&
        List.for_all (validate_hardware_stmt ctx) statements
      ) items
      
  | Begin { stmts; _ } -> List.for_all (validate_hardware_stmt ctx) stmts
  | For _ -> false
  | EventCtrl _ | Delay _ | Initial _ | InitialStatic _ | Final _ -> false
  | Display _ | Finish | Stop _ -> false
  | _ -> true

and validate_hardware_expr ctx = function
  | VarRef _ | Const _ -> true
  | BinaryOp { lhs; rhs; _ } -> validate_hardware_expr ctx lhs && validate_hardware_expr ctx rhs
  | UnaryOp { operand; _ } -> validate_hardware_expr ctx operand
  | Cond { condition; then_val; else_val } ->
      validate_hardware_expr ctx condition &&
      validate_hardware_expr ctx then_val &&
      validate_hardware_expr ctx else_val
  | Sel { expr; lsb; width; _ } ->
      validate_hardware_expr ctx expr &&
      (match lsb with Some e -> validate_hardware_expr ctx e | None -> true) &&
      (match width with Some e -> validate_hardware_expr ctx e | None -> true)
  | ArraySel { expr; index } -> validate_hardware_expr ctx expr && validate_hardware_expr ctx index
  | Concat { parts } -> List.for_all (validate_hardware_expr ctx) parts
  | FuncRef { name; args } when name = "clog2" -> List.for_all (validate_hardware_expr ctx) args
  | FuncRef _ -> false
  | _ -> false

(* Convert statement to structural form *)
let rec structural_stmt ctx stmt =
  match stmt with
  | Assign { lhs; rhs; _ } ->
      structural_assign ctx lhs rhs ctx.in_sequential
  
  | AssignW { lhs; rhs } ->
      structural_assign ctx lhs rhs false
  
  | If { condition; then_stmt; else_stmt } ->
      structural_if ctx condition then_stmt else_stmt
  
  | Case { expr; items } ->
      structural_case ctx expr items
  
  | Begin { stmts; _ } ->
      List.iter (structural_stmt ctx) stmts

  | Always { always; senses; stmts } ->
      let block_type = classify_always_block always senses stmts in
      
      (match block_type with
      | Unsynthesizable reason ->
          add_warning ("Unsynthesizable always block: " ^ reason);
          ctx.instances := 
            (Printf.sprintf "  // UNSYNTHESIZABLE: %s" reason) :: !(ctx.instances)
      
      | Sequential { clock; clock_edge; reset } ->
          if not (List.for_all (validate_hardware_stmt ctx) stmts) then begin
            add_warning "Sequential block contains unsynthesizable statements";
            ctx.instances := 
              (Printf.sprintf "  // UNSYNTHESIZABLE: sequential block with invalid statements") :: !(ctx.instances)
          end else begin
            let old_sequential = ctx.in_sequential in
            let old_clock = ctx.current_clock in
            let old_clock_edge = ctx.current_clock_edge in
            let old_reset = ctx.current_reset in
            
            ctx.in_sequential <- true;
            ctx.current_clock <- Some clock;
            ctx.current_clock_edge <- Some clock_edge;
            ctx.current_reset <- (match reset with Some (r, _) -> Some r | None -> None);
            
            List.iter (structural_stmt ctx) stmts;
            
            ctx.in_sequential <- old_sequential;
            ctx.current_clock <- old_clock;
            ctx.current_clock_edge <- old_clock_edge;
            ctx.current_reset <- old_reset
          end
      
      | Combinational ->
          if not (List.for_all (validate_hardware_stmt ctx) stmts) then begin
            add_warning "Combinational block contains unsynthesizable statements";
            ctx.instances := 
              (Printf.sprintf "  // UNSYNTHESIZABLE: combinational block with invalid statements") :: !(ctx.instances)
          end else
            List.iter (structural_stmt ctx) stmts
      
      | Latch ->
          add_warning "Converting always_latch block - ensure this is intentional";
          structural_latch ctx stmts)
    
  | Var { name; dtype_ref; dtype_name; var_type; _ } ->
      (match var_type with
      | "PORT" | "VAR" ->
          add_var ctx name dtype_ref dtype_name false
      | _ -> ())
  
  | _ -> ()

(* ============================================================================
   PACKAGE GENERATION
   ============================================================================ *)

let generate_package name stmts =
  let typedefs = List.filter_map (function
    | Typedef { name = tname; dtype_ref } ->
        (match dtype_ref with
        | Some (EnumType { items; _ }) ->
            let enum_items = List.map (fun (n, v) -> 
              Printf.sprintf "        %s = %s" n v
            ) items in
            let width = List.length items in
            let bit_width = max 1 (int_of_float (ceil (log (float_of_int width) /. log 2.0))) in
            Some (Printf.sprintf "  typedef enum logic [%d:0] {\n%s\n  } %s;" 
              (bit_width - 1)
              (String.concat ",\n" enum_items)
              tname)
        
        | Some (StructType { packed; members; _ }) ->
            let member_strs = List.map (function
              | MemberType { name = mname; dtype_ref = Some (BasicType { keyword; range }); _ } ->
                  let range_str = match range with
                    | Some r -> Printf.sprintf " [%s]" r
                    | None -> ""
                  in
                  Printf.sprintf "        %s%s %s;" keyword range_str mname
              | _ -> "        /* member */"
            ) members in
            Some (Printf.sprintf "  typedef struct %s{\n%s\n  } %s;" 
              (if packed then "packed " else "")
              (String.concat "\n" member_strs)
              tname)
        
        | _ ->
            Some (Printf.sprintf "  typedef /* type */ %s;" tname))
    | _ -> None
  ) stmts in
  
  let params = List.filter_map (function
    | Var { name = pname; dtype_ref; dtype_name; var_type = "LPARAM"; value; _ } ->
        let base_type = match dtype_ref with
          | Some (BasicType { keyword; range = Some r; _ }) -> 
              Printf.sprintf "%s [%s]" keyword r
          | Some (BasicType { keyword; _ }) -> keyword
          | _ when dtype_name <> "" -> dtype_name
          | _ -> "int"
        in
        let value_str = match value with
          | Some (Const { name = v; _ }) -> Printf.sprintf " = %s" v
          | _ -> ""
        in
        Some (Printf.sprintf "  localparam %s %s%s;" base_type pname value_str)
    | _ -> None
  ) stmts in
  
  let funcs = List.filter_map (function
    | Func { name = fname; dtype_ref; _ } ->
        let ret_type = match dtype_ref with
          | Some (BasicType { keyword; range = Some r }) -> 
              Printf.sprintf "%s [%s]" keyword r
          | Some (BasicType { keyword; _ }) -> keyword
          | _ -> "logic"
        in
        Some (Printf.sprintf "  function automatic %s %s(/* args */);\n  endfunction" 
          ret_type fname)
    | _ -> None
  ) stmts in
  
  if typedefs = [] && params = [] && funcs = [] then ""
  else
    Printf.sprintf "package %s;\n%s\nendpackage" 
      name 
      (String.concat "\n" (typedefs @ params @ funcs))

(* ============================================================================
   MODULE GENERATION
   ============================================================================ *)

let structural_module name stmts packages =
  add_warning (Printf.sprintf "Converting module '%s' to structural form" name);
  let ctx = create_context () in
  
  (* Extract type parameters *)
  let type_params = extract_type_params stmts in
  if !debug && type_params <> [] then
    Printf.eprintf "  Found %d type parameters: %s\n" 
      (List.length type_params)
      (String.concat ", " type_params);
  
  (* Recursive variable collection including SSA vars *)
  let rec collect_vars acc = function
    | [] -> acc
    | Var { name; dtype_ref; dtype_name; var_type; direction; value; is_param } :: rest ->
        let is_reg = var_type = "VAR" in
        add_var ctx name dtype_ref dtype_name is_reg;
        collect_vars (Var { name; dtype_ref; dtype_name; var_type; 
                           direction; value; is_param } :: acc) rest
    | Always { stmts = inner; _ } :: rest ->
        collect_vars (collect_vars acc inner) rest
    | Begin { stmts = inner; is_generate = false; _ } :: rest ->
        collect_vars (collect_vars acc inner) rest
    | _ :: rest ->
        collect_vars acc rest
  in
  
  let all_vars = collect_vars [] stmts in
  
  (* Convert logic *)
  List.iter (structural_stmt ctx) stmts;
  
  (* Generate parameter declarations *)
  let params = List.filter_map (function
    | Var { name; dtype_ref; dtype_name; var_type = "GPARAM"; value; _ } ->
        (match dtype_ref with
        | Some (RefType { refdtype_ref = Some (ParamTypeType _); _ }) ->
            (* Type parameter *)
            let base_type = get_base_type_resolved packages type_params dtype_ref dtype_name in
            let value_str = match value with
              | Some (Const { name = v; _ }) -> Printf.sprintf " = %s" v
              | _ -> ""
            in
            Some (Printf.sprintf "  parameter %s %s%s" base_type name value_str)
        
        | _ ->
            (* Regular parameter *)
            let base_type = get_base_type_resolved packages type_params dtype_ref dtype_name in
            let value_str = match value with
              | Some (Const { name = v; _ }) -> Printf.sprintf " = %s" v
              | _ -> ""
            in
            Some (Printf.sprintf "  parameter %s %s%s" base_type name value_str))
    | _ -> None
  ) stmts in
  
  (* Generate ports *)
  let ports = List.filter_map (function
    | Var { name; dtype_ref; dtype_name; var_type = "PORT"; direction; _ } ->
        let base_type = get_base_type_resolved packages type_params dtype_ref dtype_name in
        let width_str = get_width_str_resolved packages type_params dtype_ref dtype_name in
        Some (Printf.sprintf "  %s %s%s %s" 
               (String.lowercase_ascii direction)
               base_type
               width_str
               name)
    | _ -> None
  ) stmts in
  
  (* Generate internal variable declarations *)
  let internal_vars = List.filter_map (function
    | Var { name; dtype_ref; dtype_name; var_type; _ } 
      when var_type <> "PORT" && var_type <> "GPARAM" && var_type <> "GENVAR" ->
        let base_type = get_base_type_resolved packages type_params dtype_ref dtype_name in
        let width_str = get_width_str_resolved packages type_params dtype_ref dtype_name in
        
        (* Handle array declarations *)
        (match dtype_ref with
        | Some (ArrayType { base; range }) ->
            let elem_type = match base with
              | RefType { name; _ } -> name
              | BasicType { keyword; _ } -> keyword
              | _ -> "logic"
            in
            let elem_width = match base with
              | BasicType { range = Some r; _ } -> Printf.sprintf " [%s]" r
              | RefType _ -> get_width_str_resolved packages type_params (Some base) ""
              | _ -> ""
            in
            Some (Printf.sprintf "  %s%s %s [%s];" elem_type elem_width name range)
        | _ ->
            Some (Printf.sprintf "  %s%s %s;" base_type width_str name))
    | _ -> None
  ) all_vars in
  
  (* Build module string *)
  let param_str = if params = [] then "" 
    else "\n#(\n" ^ String.concat ",\n" params ^ "\n)" in
  
  let port_str = String.concat ",\n" ports in
  let internal_vars_str = String.concat "\n" internal_vars in
  let wires_str = String.concat "\n" (List.rev !(ctx.wires)) in
  let instances_str = String.concat "\n" (List.rev !(ctx.instances)) in
  
  let body_parts = [
    (if internal_vars_str <> "" then internal_vars_str else "");
    (if wires_str <> "" then wires_str else "");
    instances_str
  ] in
  let body_str = String.concat "\n\n" (List.filter (fun s -> s <> "") body_parts) in
  
  Printf.sprintf "module %s%s (\n%s\n);\n\n%s\n\nendmodule" 
    name param_str port_str body_str

(* ============================================================================
   MAIN ENTRY POINT
   ============================================================================ *)

let rec generate_structural node indent =
  match node with
  | Netlist modules ->
      clear_warnings ();
      
      (* Extract packages first *)
      let packages = extract_packages (Netlist modules) in
      
      (* Generate package declarations *)
      let package_strs = List.filter_map (function
        | Package { name; stmts } -> 
            let pkg_str = generate_package name stmts in
            if pkg_str = "" then None else Some pkg_str
        | _ -> None
      ) modules in
      
      (* Generate modules with package context *)
      let module_strs = List.filter_map (function
        | Module { name; stmts } -> 
            Some (structural_module name stmts packages)
        | _ -> None
      ) modules in
      
      String.concat "\n\n" (package_strs @ module_strs)
  
  | Module { name; stmts } ->
      structural_module name stmts []
  
  | _ -> ""

(* Public API with warnings *)
let generate_structural_with_warnings ast =
  clear_warnings ();
  let result = generate_structural ast 0 in
  (result, get_warnings ())
