(* VHDL Process Extractor - Extract clock, reset, and assignments from processes *)
(*
 * Assumes hardwired std_logic defaults:
 * - All signals are std_logic or std_logic_vector by default
 * - Clock edges detected via 'event attribute or rising_edge()/falling_edge()
 * - Reset signals identified by first if condition in process
 * - No explicit type resolution needed
 *)

open Vhd_front.VhdlTypes
open Vhdl_expr_to_ir

(* Process information extracted from VHDL *)
type process_info = {
  clock_signal: string option;
  reset_signal: string option;
  reset_active_high: bool;
  reset_async: bool;
  assignments: (string * (vhdl_expression * vhdl_expression) list) list;
    (* signal_name -> [(condition, value)] *)
}

(* Condition context for tracking nested conditions *)
type condition_type =
  | PositiveCond of vhdl_expression
  | NegativeCond of vhdl_expression

type condition_context = {
  conditions: condition_type list;  (* Stack of conditions (AND together) *)
}

(* Create empty context *)
let empty_context () = {
  conditions = [];
}

(* Add positive condition to context *)
let push_condition ctx cond =
  { conditions = PositiveCond cond :: ctx.conditions }

(* Add negated condition to context *)
let push_negated_condition ctx cond =
  { conditions = NegativeCond cond :: ctx.conditions }

(* Extract name from primary *)
let rec primary_to_string = function
  | NamePrimary name -> name_to_string name
  | _ -> "<unknown>"

(* Extract name from target *)
let rec target_to_string = function
  | TargetName name -> name_to_string name
  | TargetDotted dotted -> dotted_to_string dotted
  | _ -> "<unknown_target>"

(* Extract name from dotted *)
and dotted_to_string = function
  | AtomDotted prim -> primary_to_string prim
  | Ldotted (p1, p2) -> primary_to_string p1 ^ "." ^ primary_to_string p2

(* Extract string from identifier *)
and string_of_identifier (name, _pos) = name

(* Extract name string *)
and name_to_string = function
  | SimpleName id -> string_of_identifier id
  | OperatorString (s, _) -> s
  | SelectedName suffixes ->
      (* Handle library.type references - extract the last suffix *)
      (match List.rev suffixes with
       | SuffixSimpleName name :: _ -> name_to_string name
       | SuffixCharLiteral (c, _) :: _ -> String.make 1 c
       | SuffixOpSymbol (s, _) :: _ -> s
       | SuffixAll :: _ -> "all"
       | [] -> "<selected>")
  | AttributeName attr ->
      (* Handle signal'attribute - extract the prefix (signal name) *)
      (match attr.attributeprefix with
       | SuffixSimpleName name -> name_to_string name
       | SuffixCharLiteral (c, _) -> String.make 1 c
       | SuffixOpSymbol (s, _) -> s
       | SuffixAll -> "all")
  | SubscriptName (id, _) -> string_of_identifier id

(* Check if expression is a clock edge: CLK'event and CLK='1' *)
let is_clock_edge expr =
  (* This is a simplified check - real implementation would parse the expression *)
  (* For now, return None - will be enhanced later *)
  None

(* Extract clock signal from sensitivity list and first condition *)
let extract_clock_from_sensitivity sensitivity first_if =
  match sensitivity with
  | SensitivityAll -> None
  | SensitivityExpressionList exprs ->
      (* Look for clock signal in sensitivity list *)
      (* For now, assume second signal in list is clock if there are 2 *)
      if List.length exprs >= 2 then
        Some "CLK"  (* Placeholder - should parse actual signal names *)
      else if List.length exprs = 1 then
        Some "CLK"  (* Single signal assumed to be clock *)
      else
        None

(* Extract reset signal and polarity from first if condition *)
let extract_reset_from_first_if first_if sensitivity =
  match first_if with
  | Some (SequentialIf if_stmt) ->
      (* The first if condition is typically the reset check *)
      (* For now, return placeholder - should parse actual condition *)
      let reset_sig = Some "RST" in
      let active_high = true in  (* Assume active high for now *)
      let async = (match sensitivity with
        | SensitivityExpressionList exprs -> List.length exprs > 1
        | _ -> false) in
      (reset_sig, active_high, async)
  | _ -> (None, true, false)

(* Extract value expression from waveform *)
let extract_value_from_waveform = function
  | WaveForms (elem :: _rest) -> Some elem.valueexpression
  | WaveForms [] -> None
  | Unaffected -> None

(* Walk sequential statements and collect assignments *)
let rec extract_assignments_from_statements stmts ctx acc =
  List.fold_left (fun acc stmt ->
    extract_assignments_from_statement stmt ctx acc
  ) acc stmts

(* Extract assignments from a single sequential statement *)
and extract_assignments_from_statement stmt ctx acc =
  match stmt with
  | SequentialSignalAssignment sig_assign ->
      extract_assignments_from_signal_assignment sig_assign ctx acc

  | SequentialIf if_stmt ->
      extract_assignments_from_if if_stmt ctx acc

  | _ ->
      (* Other statement types don't contain assignments *)
      acc

(* Extract assignments from signal assignment *)
and extract_assignments_from_signal_assignment sig_assign ctx acc =
  match sig_assign with
  | SimpleSignalAssignment simple ->
      let target = target_to_string simple.simplesignalassignmenttarget in
      let value_opt = extract_value_from_waveform simple.simplesignalassignmentwaveform in
      (match value_opt with
       | Some value ->
           (* Create condition expression from context *)
           let cond_expr = make_condition_expr ctx.conditions in
           (* Add assignment to accumulator *)
           add_assignment acc target cond_expr value
       | None -> acc)

  | _ ->
      (* Other signal assignment types not handled yet *)
      acc

(* Extract assignments from if statement *)
and extract_assignments_from_if if_stmt ctx acc =
  let (Condition cond_expr) = if_stmt.ifcondition in

  (* Process THEN branch with condition added *)
  let then_ctx = push_condition ctx cond_expr in
  let acc = extract_assignments_from_statements if_stmt.thenstatements then_ctx acc in

  (* Process ELSE/ELSIF branches with negated condition *)
  extract_assignments_from_else if_stmt.elsestatements ctx acc cond_expr

(* Extract assignments from else clause *)
and extract_assignments_from_else else_clause ctx acc cond_expr =
  match else_clause with
  | ElseNone -> acc

  | Else stmts ->
      (* ELSE branch: negate the current condition *)
      let else_ctx = push_negated_condition ctx cond_expr in
      extract_assignments_from_statements stmts else_ctx acc

  | Elsif if_stmt ->
      (* ELSIF is like ELSE + IF *)
      let else_ctx = push_negated_condition ctx cond_expr in
      extract_assignments_from_if if_stmt else_ctx acc

(* Create condition expression from list of condition types *)
and make_condition_expr conditions =
  match conditions with
  | [] ->
      (* No condition means always true - create constant true *)
      AtomExpression (
        AtomLogicalExpression (
          AtomRelation (
            AtomShiftExpression (
              AtomSimpleExpression (
                AtomTerm (
                  AtomFactor (
                    AtomDotted (
                      CharPrimary ('1', 0)
                    )
                  )
                )
              )
            )
          )
        )
      )
  | [PositiveCond expr] -> expr
  | [NegativeCond expr] ->
      (* For negated condition, just return the original for now *)
      (* The actual negation will be handled during IR conversion *)
      expr
  | _ ->
      (* Multiple conditions: for now just return the first one *)
      (* Full AND construction will be done during IR conversion *)
      match List.hd conditions with
      | PositiveCond expr -> expr
      | NegativeCond expr -> expr

(* Add assignment to accumulator *)
and add_assignment acc target cond_expr value =
  (* Find or create entry for this target *)
  let existing = try Some (List.assoc target acc) with Not_found -> None in
  match existing with
  | Some assignments ->
      (* Add to existing list *)
      let updated = (cond_expr, value) :: assignments in
      (target, updated) :: (List.remove_assoc target acc)
  | None ->
      (* Create new entry *)
      (target, [(cond_expr, value)]) :: acc

(* Extract all information from a process statement *)
let extract_process_info proc =
  let sensitivity = proc.processsensitivitylist in
  let stmts = proc.processstatements in

  (* Get first statement to extract reset *)
  let first_if = match stmts with
    | (SequentialIf _ as stmt) :: _ -> Some stmt
    | _ -> None
  in

  (* Extract clock and reset *)
  let clock = extract_clock_from_sensitivity sensitivity first_if in
  let (reset, reset_high, reset_async) = extract_reset_from_first_if first_if sensitivity in

  (* Extract assignments with empty initial context *)
  let ctx = empty_context () in
  let assignments = extract_assignments_from_statements stmts ctx [] in

  {
    clock_signal = clock;
    reset_signal = reset;
    reset_active_high = reset_high;
    reset_async = reset_async;
    assignments = assignments;
  }

(* Pretty print process info *)
let print_process_info info =
  Printf.printf "\n";
  Printf.printf "Process Information:\n";
  Printf.printf "  Clock: %s\n" (match info.clock_signal with Some s -> s | None -> "<none>");
  Printf.printf "  Reset: %s (%s, %s)\n"
    (match info.reset_signal with Some s -> s | None -> "<none>")
    (if info.reset_active_high then "active high" else "active low")
    (if info.reset_async then "async" else "sync");
  Printf.printf "  Assignments:\n";
  List.iter (fun (target, cond_vals) ->
    Printf.printf "    %s (%d assignments)\n" target (List.length cond_vals);
    List.iteri (fun i (cond, value) ->
      Printf.printf "      [%d] <condition> → <value>\n" i
    ) cond_vals
  ) info.assignments
