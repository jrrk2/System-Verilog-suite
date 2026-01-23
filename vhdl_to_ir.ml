(* Iterative VHDL to IR Converter - Testing on UART Examples *)
(*
 * Strategy: Start simple, add patterns as we encounter them
 * Debug by showing what patterns we're missing
 * Now with JSON dumping for all unhandled cases!
 *)

open Vhd_front.VhdlTree
open Sv_ast

(* Track what patterns we see but don't handle yet *)
let unhandled_patterns = Hashtbl.create 100
let unhandled_dump_counter = ref 0

let record_unhandled tag =
  let count = try Hashtbl.find unhandled_patterns tag with Not_found -> 0 in
  Hashtbl.replace unhandled_patterns tag (count + 1)

(* Dump unhandled pattern to JSON for detailed analysis *)
let dump_unhandled_json context vhd =
  incr unhandled_dump_counter;
  let tag = Vhdl_dump_json.get_description vhd in
  record_unhandled tag;
  (* Only dump first few instances of each pattern to avoid file spam *)
  if !unhandled_dump_counter <= 10 then
    Vhdl_dump_json.dump_unhandled context (Printf.sprintf "pattern_%d" !unhandled_dump_counter) vhd

(* IR context *)
type ir_context = {
  mutable next_id: int;
  signals: (string, value_id * int) Hashtbl.t;
  mutable nodes: (value_id * operation * value_id list) list;
  mutable inputs: (string * int) list;
  mutable outputs: (string * int) list;
  mutable wires: (string * int) list;
  mutable constants: (value_id * int * int) list;
  mutable module_name: string;
}

let create_context () = {
  next_id = 0;
  signals = Hashtbl.create 50;
  nodes = [];
  inputs = [];
  outputs = [];
  wires = [];
  constants = [];
  module_name = "";
}

let fresh_id ctx =
  let id = ctx.next_id in
  ctx.next_id <- ctx.next_id + 1;
  id

let add_node ctx op inputs =
  let id = fresh_id ctx in
  ctx.nodes <- (id, op, inputs) :: ctx.nodes;
  id

let get_signal ctx name width =
  if Hashtbl.mem ctx.signals name then
    fst (Hashtbl.find ctx.signals name)
  else begin
    let id = fresh_id ctx in
    Hashtbl.add ctx.signals name (id, width);
    id
  end

let add_constant ctx value width =
  let id = fresh_id ctx in
  ctx.constants <- (id, value, width) :: ctx.constants;
  id

(* Extract entity information - from rewrite.ml lines 627-643 *)
let extract_entity ctx = function
  | Triple (Vhddesign_unit,
           _liblst,
           Double (VhdPrimaryUnit,
                  Double (VhdEntityDeclaration,
                         Quintuple (Vhdentity_declaration, Str design,
                                   Triple (Vhdentity_header, _generics, portlst),
                                   _decls, _stmts)))) ->
      ctx.module_name <- design;

      (* Extract ports - from rewrite.ml lines 507-540 *)
      let rec extract_ports = function
        | List ports -> List.iter extract_ports ports
        | Double (VhdInterfaceObjectDeclaration,
                 Double (VhdInterfaceDefaultDeclaration,
                        Sextuple (Vhdinterface_default_declaration, Str name,
                                 VhdInterfaceModeIn,
                                 _subtype_ind, _signal_kind, _default))) ->
            ctx.inputs <- (name, 1) :: ctx.inputs;
            ignore (get_signal ctx name 1)

        | Double (VhdInterfaceObjectDeclaration,
                 Double (VhdInterfaceDefaultDeclaration,
                        Sextuple (Vhdinterface_default_declaration, Str name,
                                 VhdInterfaceModeOut,
                                 _subtype_ind, _signal_kind, _default))) ->
            ctx.outputs <- (name, 1) :: ctx.outputs;
            ignore (get_signal ctx name 1)

        | other -> dump_unhandled_json "extract_ports" other
      in
      extract_ports portlst

  (* Architecture declaration - will be handled by convert_architecture *)
  | Triple (Vhddesign_unit,
           _liblst,
           Double (VhdSecondaryUnit, _arch_decl)) ->
      (* Skip, will process in convert_architecture *)
      ()

  (* Library/context clauses - ignore *)
  | Triple (Vhddesign_unit, _lib_clauses, VhdNone) ->
      ()

  (* Other design units we don't care about *)
  | VhdNone ->
      ()

  | other -> dump_unhandled_json "extract_entity" other

(* Convert expressions - based on match2' patterns *)
let rec expr_to_ir ctx = function
  (* Relations *)
  | Triple (VhdEqualRelation, lft, rght) ->
      let l_id = expr_to_ir ctx lft in
      let r_id = expr_to_ir ctx rght in
      add_node ctx (Compare { cmp_op = `Eq; width = 32; signed = false }) [l_id; r_id]

  | Triple (VhdNotEqualRelation, lft, rght) ->
      let l_id = expr_to_ir ctx lft in
      let r_id = expr_to_ir ctx rght in
      add_node ctx (Compare { cmp_op = `Ne; width = 32; signed = false }) [l_id; r_id]

  | Triple (VhdLessRelation, lft, rght) ->
      let l_id = expr_to_ir ctx lft in
      let r_id = expr_to_ir ctx rght in
      add_node ctx (Compare { cmp_op = `Lt; width = 32; signed = false }) [l_id; r_id]

  | Triple (VhdGreaterRelation, lft, rght) ->
      let l_id = expr_to_ir ctx lft in
      let r_id = expr_to_ir ctx rght in
      add_node ctx (Compare { cmp_op = `Gt; width = 32; signed = false }) [l_id; r_id]

  (* Arithmetic *)
  | Triple (VhdAddSimpleExpression, lft, rght) ->
      let l_id = expr_to_ir ctx lft in
      let r_id = expr_to_ir ctx rght in
      add_node ctx (Add { width = 32; signed = true }) [l_id; r_id]

  | Triple (VhdSubSimpleExpression, lft, rght) ->
      let l_id = expr_to_ir ctx lft in
      let r_id = expr_to_ir ctx rght in
      add_node ctx (Sub { width = 32; signed = true }) [l_id; r_id]

  (* Logical *)
  | Triple (VhdAndLogicalExpression, lft, rght) ->
      let l_id = expr_to_ir ctx lft in
      let r_id = expr_to_ir ctx rght in
      add_node ctx (And { width = 1 }) [l_id; r_id]

  | Triple (VhdOrLogicalExpression, lft, rght) ->
      let l_id = expr_to_ir ctx lft in
      let r_id = expr_to_ir ctx rght in
      add_node ctx (Or { width = 1 }) [l_id; r_id]

  | Triple (VhdXorLogicalExpression, lft, rght) ->
      let l_id = expr_to_ir ctx lft in
      let r_id = expr_to_ir ctx rght in
      add_node ctx (Xor { width = 1 }) [l_id; r_id]

  (* Not operator *)
  | Double (VhdNotFactor, expr) ->
      let expr_id = expr_to_ir ctx expr in
      add_node ctx (Not { width = 1 }) [expr_id]

  (* Parentheses *)
  | Double (VhdParenthesedPrimary, x) ->
      expr_to_ir ctx x

  (* Condition wrapper *)
  | Double (VhdCondition, x) ->
      expr_to_ir ctx x

  (* Character literals *)
  | Double (VhdCharPrimary, Char ch) ->
      let value = if ch = '1' then 1 else 0 in
      add_constant ctx value 1

  (* Integer literals *)
  | Double (VhdIntPrimary, Num n) ->
      (try
        add_constant ctx (int_of_string n) 32
      with _ ->
        add_constant ctx 0 32)

  (* Bit string literals: "0001", "1100", etc. *)
  | Double (VhdOperatorString, Str bitstring) ->
      (* Convert bit string to integer *)
      let value = try
        int_of_string ("0b" ^ bitstring)
      with _ ->
        (* If that fails, try interpreting each char *)
        let rec bits_to_int acc = function
          | [] -> acc
          | '0' :: rest -> bits_to_int (acc * 2) rest
          | '1' :: rest -> bits_to_int (acc * 2 + 1) rest
          | _ :: rest -> bits_to_int acc rest
        in
        bits_to_int 0 (List.init (String.length bitstring) (String.get bitstring))
      in
      add_constant ctx value (String.length bitstring)

  (* Simple names *)
  | Str name ->
      get_signal ctx name 32

  (* Aggregate with others => value *)
  | Double (VhdAggregatePrimary,
           Triple (Vhdelement_association, VhdChoiceOthers,
                  Double (VhdCharPrimary, Char ch))) ->
      (* (others => '0') or (others => '1') *)
      let value = if ch = '1' then 1 else 0 in
      add_constant ctx value 32

  | Double (VhdAggregatePrimary,
           Triple (Vhdelement_association, VhdChoiceOthers,
                  expr)) ->
      (* (others => expr) - evaluate the expression *)
      expr_to_ir ctx expr

  (* Type conversions - unsigned(x), signed(x), to_integer(x), etc. *)
  | Triple (VhdNameParametersPrimary, Str func_name,
           Triple (Vhdassociation_element, _,
                  Double (VhdActualExpression, arg)))
    when func_name = "unsigned" || func_name = "signed" ||
         func_name = "to_integer" || func_name = "to_unsigned" ||
         func_name = "to_signed" || func_name = "std_logic_vector" ->
      (* For now, just pass through the argument *)
      expr_to_ir ctx arg

  (* Indexed/sliced signal access: signal(index) or signal(high downto low) *)
  | Triple (VhdNameParametersPrimary, Str sig_name,
           Triple (Vhdassociation_element, VhdFormalIndexed,
                  Double (VhdActualExpression, idx))) ->
      (* For simplicity, return the signal itself (TODO: add Extract node) *)
      get_signal ctx sig_name 32

  (* Dotted target - like signal(0) in assignments *)
  | Double (VhdTargetDotted,
           Triple (VhdNameParametersPrimary, Str sig_name,
                  Triple (Vhdassociation_element, VhdFormalIndexed,
                         Double (VhdActualExpression, _idx)))) ->
      get_signal ctx sig_name 32

  (* Attribute names - like clk'event *)
  | Double (VhdAttributeName,
           Triple (Vhdattribute_name,
                  Double (VhdSuffixSimpleName, Str _sig),
                  Str _attr)) ->
      (* For now, just return a dummy signal *)
      fresh_id ctx

  | VhdNone ->
      fresh_id ctx

  | other ->
      dump_unhandled_json "expr_to_ir" other;
      fresh_id ctx

(* Convert sequential statements *)
let rec stmt_to_ir ctx = function
  (* Signal assignment: signal <= value *)
  | Double (VhdSequentialSignalAssignment,
           Double (VhdSimpleSignalAssignment,
                  Quintuple (Vhdsimple_signal_assignment_statement,
                            Str "", Str nam, VhdDelayNone,
                            Double (Vhdwaveform_element, rhs)))) ->
      let rhs_id = expr_to_ir ctx rhs in
      let lhs_id = get_signal ctx nam 32 in
      [(lhs_id, rhs_id)]

  (* Variable assignment: variable := value *)
  | Double (VhdSequentialVariableAssignment,
           Double (VhdSimpleVariableAssignment,
                  Quadruple (Vhdsimple_variable_assignment, Str "", Str nam, rhs))) ->
      let rhs_id = expr_to_ir ctx rhs in
      let lhs_id = get_signal ctx nam 32 in  (* Treat variables like signals for IR *)
      [(lhs_id, rhs_id)]

  (* Signal assignment with indexed target: signal(index) <= value *)
  | Double (VhdSequentialSignalAssignment,
           Double (VhdSimpleSignalAssignment,
                  Quintuple (Vhdsimple_signal_assignment_statement,
                            Str "",
                            Double (VhdTargetDotted,
                                   Triple (VhdNameParametersPrimary, Str nam,
                                          Triple (Vhdassociation_element, VhdFormalIndexed,
                                                 Double (VhdActualExpression, _idx)))),
                            VhdDelayNone,
                            Double (Vhdwaveform_element, rhs)))) ->
      let rhs_id = expr_to_ir ctx rhs in
      let lhs_id = get_signal ctx nam 32 in
      [(lhs_id, rhs_id)]

  (* List of statements *)
  | List stmts ->
      List.concat (List.map (stmt_to_ir ctx) stmts)

  (* If statement with elsif *)
  | Double (VhdSequentialIf,
           Quintuple (Vhdif_statement, Str "",
                     cond,
                     then_clause,
                     elsif_part)) ->
      let _cond_id = expr_to_ir ctx cond in
      let then_assigns = stmt_to_ir ctx then_clause in
      let elsif_assigns = stmt_to_ir ctx elsif_part in
      then_assigns @ elsif_assigns

  (* Elsif clause *)
  | Double (VhdElsif,
           Quintuple (Vhdif_statement, Str "",
                     cond,
                     then_clause,
                     else_part)) ->
      let _cond_id = expr_to_ir ctx cond in
      let then_assigns = stmt_to_ir ctx then_clause in
      let else_assigns = stmt_to_ir ctx else_part in
      then_assigns @ else_assigns

  (* Else clause *)
  | Double (VhdElse, stmts) ->
      stmt_to_ir ctx stmts

  (* Else none *)
  | VhdElseNone ->
      []

  (* VhdNone *)
  | VhdNone ->
      []

  (* Null statement: null; *)
  | Double (VhdSequentialNull, _) ->
      []

  (* Case statement: case signal is when ... => ... end case *)
  | Double (VhdSequentialCase,
           Quintuple (Vhdcase_statement, _label,
                     _selector, _selection_type,
                     List alternatives)) ->
      (* Process each case alternative and collect assignments *)
      let process_alternative = function
        | Triple (Vhdcase_statement_alternative, _choice, stmts) ->
            stmt_to_ir ctx stmts
        | _ -> []
      in
      List.concat (List.map process_alternative alternatives)

  | other ->
      dump_unhandled_json "stmt_to_ir" other;
      []

(* Extract signal name from comparison or attribute *)
let rec extract_signal_name = function
  | Str s -> Some s
  | Double (VhdSuffixSimpleName, Str s) -> Some s
  | Double (VhdAttributeName,
           Triple (Vhdattribute_name,
                  Double (VhdSuffixSimpleName, Str s), _)) ->
      Some s
  (* Unwrap conditions and parentheses *)
  | Double (VhdCondition, inner) -> extract_signal_name inner
  | Double (VhdParenthesedPrimary, inner) -> extract_signal_name inner
  (* Extract from comparisons *)
  | Triple (VhdEqualRelation, Str s, _) -> Some s
  | Triple (VhdEqualRelation, _, Str s) -> Some s
  | Triple (VhdNotEqualRelation, Str s, _) -> Some s
  | Triple (VhdNotEqualRelation, _, Str s) -> Some s
  | _ -> None

(* Extract clock signal from edge detection patterns *)
let rec find_clock_signal = function
  (* Pattern: signal'event and signal = '1' *)
  | Triple (VhdAndLogicalExpression,
           Double (VhdAttributeName,
                  Triple (Vhdattribute_name,
                         Double (VhdSuffixSimpleName, Str sig_name),
                         Str "event")),
           _comparison) ->
      Some sig_name

  (* Pattern: signal'event (attribute name alone) *)
  | Double (VhdAttributeName,
           Triple (Vhdattribute_name,
                  Double (VhdSuffixSimpleName, Str sig_name),
                  Str "event")) ->
      Some sig_name

  (* Pattern: rising_edge(signal) function call *)
  | Triple (VhdNameParametersPrimary, Str "rising_edge",
           Triple (Vhdassociation_element, _,
                  Double (VhdActualExpression, Str sig_name))) ->
      Some sig_name

  (* Pattern: falling_edge(signal) function call *)
  | Triple (VhdNameParametersPrimary, Str "falling_edge",
           Triple (Vhdassociation_element, _,
                  Double (VhdActualExpression, Str sig_name))) ->
      Some sig_name

  (* Unwrap parenthesized expressions *)
  | Double (VhdParenthesedPrimary, expr) ->
      find_clock_signal expr

  (* Unwrap conditions *)
  | Double (VhdCondition, expr) ->
      find_clock_signal expr

  (* Recursively search in compound expressions *)
  | Triple (VhdAndLogicalExpression, lft, rght) ->
      (match find_clock_signal lft with
       | Some clk -> Some clk
       | None -> find_clock_signal rght)

  | List items ->
      List.fold_left (fun acc item ->
        match acc with
        | Some _ -> acc
        | None -> find_clock_signal item
      ) None items

  | _ -> None

(* Extract clock and reset from process body structure *)
let analyze_process_body body =
  (* Look for if-statement with edge detection *)
  let clock_sig = ref None in
  let reset_sig = ref None in

  let rec scan = function
    (* Pattern: if reset = '1' then ... elsif rising_edge(clk) then ... *)
    | Double (VhdSequentialIf,
             Quintuple (Vhdif_statement, _, reset_cond, _reset_clause, elsif_part)) ->
        (* Try to extract reset signal from condition *)
        (match extract_signal_name reset_cond with
         | Some rst when !reset_sig = None -> reset_sig := Some rst
         | _ -> ());
        (* Look for clock in elsif *)
        (match find_clock_signal elsif_part with
         | Some clk -> clock_sig := Some clk
         | None -> ());
        (* Also scan elsif for reset in nested conditions *)
        scan elsif_part

    (* Pattern: elsif with clock edge detection *)
    | Double (VhdElsif,
             Quintuple (Vhdif_statement, _, cond, then_clause, else_part)) ->
        (* Try to find clock in condition *)
        (match find_clock_signal cond with
         | Some clk when !clock_sig = None -> clock_sig := Some clk
         | _ -> ());
        (* Continue scanning *)
        scan then_clause;
        scan else_part

    (* Pattern: if rising_edge(clk) then if reset = '1' then ... *)
    | Double (VhdSequentialIf,
             Quintuple (Vhdif_statement, _, cond, then_clause, else_part)) ->
        (* Try to find clock in condition *)
        (match find_clock_signal cond with
         | Some clk when !clock_sig = None -> clock_sig := Some clk
         | _ -> ());
        (* Try to find reset in condition if no clock found *)
        (match extract_signal_name cond with
         | Some sig_name when !reset_sig = None && !clock_sig = None ->
             reset_sig := Some sig_name
         | _ -> ());
        (* Continue scanning *)
        scan then_clause;
        scan else_part

    (* Recurse into lists *)
    | List items -> List.iter scan items

    (* Recurse into Double/Triple wrappers *)
    | Double (_, inner) -> scan inner
    | Triple (_, _, inner) -> scan inner

    | _ -> ()
  in
  scan body;
  (!clock_sig, !reset_sig)

(* Convert process - handle different patterns *)
let process_to_ir ctx = function
  (* Try to match synchronous process with reset *)
  | Sextuple (Vhdprocess_statement, Str process_name, Str _postponed,
             Double (VhdSensitivityExpressionList, List sens_list),
             decls,
             body) ->

      Printf.printf "   Process: %s\n" process_name;
      Printf.printf "      Sensitivity: %d signals\n" (List.length sens_list);

      (* Extract clock and reset by analyzing process body, not hardcoded names! *)
      let (clock_sig, reset_sig) = analyze_process_body body in

      (* Try to extract assignments from body *)
      let assigns = stmt_to_ir ctx body in
      Printf.printf "      Assignments: %d\n" (List.length assigns);

      (* If we have clock, create registers *)
      (match clock_sig with
       | Some clk ->
           Printf.printf "      Detected clock: %s\n" clk;
           (match reset_sig with
            | Some rst -> Printf.printf "      Detected reset: %s\n" rst
            | None -> ());

           let clk_id = get_signal ctx clk 1 in
           let reset_id = match reset_sig with
             | Some rst -> Some (get_signal ctx rst 1)
             | None -> None
           in
           List.iter (fun (dst_id, data_id) ->
             let _reg_id = add_node ctx
               (Register { width = 32; clock = clk_id; reset = reset_id;
                          enable = None; reset_value = 0 })
               [data_id] in
             ()
           ) assigns
       | None ->
           (* Combinational process - just wire assignments *)
           Printf.printf "      No clock detected - combinational process\n";
           ())

  | other ->
      record_unhandled "process";
      ()

(* Convert architecture *)
let convert_architecture ctx = function
  | Triple (Vhddesign_unit,
           _liblst,
           Double (VhdSecondaryUnit,
                  Double (VhdArchitectureBody,
                         Quintuple (Vhdarchitecture_body, Str arch, Str entity,
                                   decls, stmts)))) ->

      Printf.printf "\nArchitecture: %s of %s\n" arch entity;

      (* Process concurrent statements *)
      let rec process_concurrent = function
        | List lst -> List.iter process_concurrent lst

        (* Concurrent process statements *)
        | Double (VhdConcurrentProcessStatement, proc) ->
            process_to_ir ctx proc

        (* Concurrent signal assignments: signal <= expression *)
        | Double (VhdConcurrentSignalAssignmentStatement, _inner) ->
            (* TODO: Parse and convert concurrent assignments *)
            ()

        (* Concurrent selected signal assignment (case statement) *)
        | Double (VhdConcurrentSelectedSignalAssignment, _inner) ->
            (* TODO: Implement mux generation for case statements *)
            ()

        (* Concurrent conditional signal assignment *)
        | Double (VhdConcurrentConditionalSignalAssignment, _inner) ->
            (* TODO: Implement conditional assignment *)
            ()

        (* Component instantiation - ignore for now *)
        | Double (VhdConcurrentComponentInstantiationStatement, _inner) ->
            ()

        (* VhdNone *)
        | VhdNone ->
            ()

        | other ->
            dump_unhandled_json "concurrent_stmt" other;
            ()
      in
      process_concurrent stmts

  (* Entity declaration - we already extracted ports in extract_entity *)
  | Triple (Vhddesign_unit,
           _liblst,
           Double (VhdPrimaryUnit, _entity_decl)) ->
      (* Entity already processed, nothing to do *)
      ()

  (* Package declarations - ignore *)
  | Triple (Vhddesign_unit,
           _liblst,
           VhdNone) ->
      ()

  (* VhdNone *)
  | VhdNone ->
      ()

  | other ->
      dump_unhandled_json "convert_architecture" other;
      ()

(* Main conversion *)
let convert_vhdl_files files =
  Printf.printf "VHDL → IR Iterative Conversion\n";
  Printf.printf "%s\n" (String.make 70 '=');

  (* Parse *)
  Printf.printf "Parsing %d files...\n" (List.length files);
  let succ = ref true in
  Vhd_front.VhdlMain.main succ files;

  if not !succ then begin
    Printf.eprintf "Parsing failed\n";
    exit 1
  end;

  (* Extract vhdintf trees *)
  Printf.printf "Extracting design units...\n";
  let trees = ref [] in
  Hashtbl.iter (fun (k, _) _ ->
    let simplified = Vhd_front.Rewrite.abstraction (Vhd_front.Rewrite.abstraction k) in
    trees := simplified :: !trees
  ) !Vhd_front.Vabstraction.vhdlhash;

  Printf.printf "Processing %d design units\n" (List.length !trees);
  Printf.printf "%s\n" (String.make 70 '-');

  (* Convert to IR *)
  let ctx = create_context () in

  List.iter (fun tree ->
    extract_entity ctx tree;
    convert_architecture ctx tree
  ) !trees;

  Printf.printf "%s\n" (String.make 70 '=');
  Printf.printf "Results:\n";
  Printf.printf "  Module: %s\n" ctx.module_name;
  Printf.printf "  Inputs:  %d\n" (List.length ctx.inputs);
  Printf.printf "  Outputs: %d\n" (List.length ctx.outputs);
  Printf.printf "  Wires:   %d\n" (List.length ctx.wires);
  Printf.printf "  Nodes:   %d\n" (List.length ctx.nodes);
  Printf.printf "  Signals: %d\n" (Hashtbl.length ctx.signals);

  (* Show unhandled patterns *)
  if Hashtbl.length unhandled_patterns > 0 then begin
    Printf.printf "\nUnhandled patterns:\n";
    Hashtbl.iter (fun tag count ->
      Printf.printf "  %s: %d occurrences\n" tag count
    ) unhandled_patterns
  end;

  ctx

(* Convert ir_context to opt_ir format for Z3 verification *)
let context_to_opt_ir ctx module_name =
  let ir = {
    Sv_ast.ir_name = module_name;
    Sv_ast.ir_inputs = Hashtbl.create 16;
    Sv_ast.ir_outputs = Hashtbl.create 16;
    Sv_ast.ir_wires = Hashtbl.create 32;
    Sv_ast.ir_constants = Hashtbl.create 32;
    Sv_ast.ir_nodes = Hashtbl.create 64;
    Sv_ast.ir_value_to_node = Hashtbl.create 64;
    Sv_ast.ir_next_id = ctx.next_id;
    Sv_ast.ir_critical_path_length = 0;
    Sv_ast.ir_area_estimate = 0;
  } in

  (* Populate inputs *)
  List.iter (fun (name, width) ->
    let id = fst (Hashtbl.find ctx.signals name) in
    let input = Sv_ast.Input { id; name; width } in
    Hashtbl.add ir.Sv_ast.ir_inputs name input
  ) ctx.inputs;

  (* Populate outputs *)
  List.iter (fun (name, width) ->
    let id = fst (Hashtbl.find ctx.signals name) in
    let output = Sv_ast.Output { id; name; width } in
    Hashtbl.add ir.Sv_ast.ir_outputs name output
  ) ctx.outputs;

  (* Populate wires *)
  List.iter (fun (name, width) ->
    let id = fst (Hashtbl.find ctx.signals name) in
    let wire = Sv_ast.Wire { id; name; width } in
    Hashtbl.add ir.Sv_ast.ir_wires name wire
  ) ctx.wires;

  (* Populate constants *)
  List.iter (fun (id, value, width) ->
    Hashtbl.add ir.Sv_ast.ir_constants value id
  ) ctx.constants;

  (* Populate nodes *)
  List.iter (fun (node_id, op, inputs) ->
    let output = Sv_ast.Wire { id = node_id; name = Printf.sprintf "$%d" node_id; width = 32 } in
    let node = {
      Sv_ast.node_id;
      Sv_ast.node_op = op;
      Sv_ast.node_inputs = inputs;
      Sv_ast.node_output = output;
      Sv_ast.node_depth = 0;
      Sv_ast.node_users = [];
    } in
    Hashtbl.add ir.Sv_ast.ir_nodes node_id node;
    Hashtbl.add ir.Sv_ast.ir_value_to_node node_id node_id
  ) ctx.nodes;

  ir

(* Public API: Convert VHDL file to IR (for use by other modules) *)
let convert_vhdl_file_to_ir filename =
  (* Create a fresh hashtable for this parse to ensure isolation *)
  let fresh_hash = Hashtbl.create 256 in
  let old_hash = !Vhd_front.Vabstraction.vhdlhash in
  Vhd_front.Vabstraction.vhdlhash := fresh_hash;

  (* Also clear the settings filelists to prevent accumulation *)
  let old_settings = !Vhd_front.VhdlSettings.settings in
  Vhd_front.VhdlSettings.settings := {!Vhd_front.VhdlSettings.settings with
    fileparsedlist = [];
    filefailedlist = [];
  };

  try
    (* Parse the file *)
    let succ = ref true in
    Vhd_front.VhdlMain.main succ [filename];

    if not !succ then begin
      Vhd_front.Vabstraction.vhdlhash := old_hash;
      Vhd_front.VhdlSettings.settings := old_settings;
      None
    end else begin
      (* Extract vhdintf trees from our fresh hashtable *)
      let trees = ref [] in
      Hashtbl.iter (fun (k, _) _ ->
        let simplified = Vhd_front.Rewrite.abstraction (Vhd_front.Rewrite.abstraction k) in
        trees := simplified :: !trees
      ) fresh_hash;

      (* Convert to IR *)
      let ctx = create_context () in

      List.iter (fun tree ->
        extract_entity ctx tree;
        convert_architecture ctx tree
      ) !trees;

      (* Restore old hashtable and settings before returning *)
      Vhd_front.Vabstraction.vhdlhash := old_hash;
      Vhd_front.VhdlSettings.settings := old_settings;

      (* Convert to opt_ir format *)
      let ir = context_to_opt_ir ctx ctx.module_name in

      (* Report any unhandled patterns encountered during conversion *)
      if Sys.file_exists "unhandled_vhdl" && Sys.is_directory "unhandled_vhdl" then begin
        let files = Sys.readdir "unhandled_vhdl" |> Array.to_list in
        let json_files = List.filter (fun f -> Filename.check_suffix f ".json") files in
        if List.length json_files > 0 then begin
          Printf.printf "\n⚠ Warning: Encountered %d unhandled pattern(s) - see unhandled_vhdl/\n"
            (List.length json_files);
          Printf.printf "  Run: Vhdl_dump_json.create_summary() for details\n"
        end
      end;

      Some ir
    end
  with e ->
    (* Make sure to restore hashtable and settings even on error *)
    Vhd_front.Vabstraction.vhdlhash := old_hash;
    Vhd_front.VhdlSettings.settings := old_settings;
    Printf.eprintf "Error converting %s: %s\n" filename (Printexc.to_string e);
    None

