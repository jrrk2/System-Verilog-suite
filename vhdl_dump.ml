(* VHDL AST Dumper - Print structure of parsed VHDL *)

open VhdlTypes

let indent level = String.make (level * 2) ' '

(* Extract string from identifier *)
let string_of_identifier (name, _pos) = name

(* Extract string from label *)
let string_of_label = function
  | (name, _) -> name

(* Extract simple name from name *)
let rec name_to_string = function
  | SimpleName id -> string_of_identifier id
  | OperatorString (s, _) -> s
  | SelectedName _ -> "<selected>"
  | AttributeName _ -> "<attribute>"
  | SubscriptName (id, _) -> string_of_identifier id ^ "[...]"

(* Extract name from primary *)
let primary_to_string = function
  | NamePrimary name -> name_to_string name
  | _ -> "<primary>"

(* Extract name from dotted *)
let dotted_to_string = function
  | AtomDotted prim -> primary_to_string prim
  | Ldotted (p1, p2) -> primary_to_string p1 ^ "." ^ primary_to_string p2

(* Dump target (LHS of assignment) *)
let rec dump_target level = function
  | TargetName name ->
      Printf.printf "%sTarget: %s\n" (indent level) (name_to_string name)
  | TargetDotted dotted ->
      Printf.printf "%sTarget: %s\n" (indent level) (dotted_to_string dotted)
  | TargetNameParameters _ ->
      Printf.printf "%sTarget: <name_params>\n" (indent level)
  | TargetAggregate _ ->
      Printf.printf "%sTarget: <aggregate>\n" (indent level)
  | TargetInvalid _ ->
      Printf.printf "%sTarget: <invalid>\n" (indent level)
  | SelectTargetName _ ->
      Printf.printf "%sTarget: <select_name>\n" (indent level)
  | SelectTargetNameParameters _ ->
      Printf.printf "%sTarget: <select_name_params>\n" (indent level)

(* Dump waveform element *)
let dump_waveform_element level elem =
  Printf.printf "%sWaveform: <expression>\n" (indent level)

(* Dump waveform *)
let dump_waveform level = function
  | WaveForms elements ->
      List.iter (dump_waveform_element level) elements
  | Unaffected ->
      Printf.printf "%sWaveform: unaffected\n" (indent level)

(* Dump simple signal assignment *)
let dump_simple_signal_assignment level stmt =
  let (label, _) = stmt.simplesignalassignmentlabelname in
  if label <> "" then
    Printf.printf "%sLabel: %s\n" (indent level) label;
  dump_target level stmt.simplesignalassignmenttarget;
  dump_waveform level stmt.simplesignalassignmentwaveform

(* Dump signal assignment *)
let rec dump_signal_assignment level = function
  | SimpleSignalAssignment stmt ->
      Printf.printf "%sSimpleSignalAssignment:\n" (indent level);
      dump_simple_signal_assignment (level + 1) stmt
  | ConditionalSignalAssignment _ ->
      Printf.printf "%sConditionalSignalAssignment\n" (indent level)
  | SelectedSignalAssignment _ ->
      Printf.printf "%sSelectedSignalAssignment\n" (indent level)

(* Dump else statements *)
and dump_else_statements level = function
  | ElseNone ->
      ()
  | Else stmts ->
      Printf.printf "%sElse:\n" (indent level);
      List.iter (dump_sequential_statement (level + 1)) stmts
  | Elsif if_stmt ->
      Printf.printf "%sElsif:\n" (indent level);
      dump_if_statement (level + 1) if_stmt

(* Dump if statement *)
and dump_if_statement level stmt =
  Printf.printf "%sIf <condition>:\n" (indent level);
  Printf.printf "%sThen:\n" (indent (level + 1));
  List.iter (dump_sequential_statement (level + 2)) stmt.thenstatements;
  dump_else_statements (level + 1) stmt.elsestatements

(* Dump sequential statement *)
and dump_sequential_statement level = function
  | SequentialSignalAssignment stmt ->
      dump_signal_assignment level stmt
  | SequentialIf stmt ->
      dump_if_statement level stmt
  | SequentialWait _ ->
      Printf.printf "%sWait statement\n" (indent level)
  | SequentialAssertion _ ->
      Printf.printf "%sAssertion\n" (indent level)
  | SequentialReport _ ->
      Printf.printf "%sReport\n" (indent level)
  | SequentialVariableAssignment _ ->
      Printf.printf "%sVariable assignment\n" (indent level)
  | SequentialProcedureCall _ ->
      Printf.printf "%sProcedure call\n" (indent level)
  | SequentialCase _ ->
      Printf.printf "%sCase statement\n" (indent level)
  | SequentialLoop _ ->
      Printf.printf "%sLoop statement\n" (indent level)
  | SequentialNext _ ->
      Printf.printf "%sNext statement\n" (indent level)
  | SequentialExit _ ->
      Printf.printf "%sExit statement\n" (indent level)
  | SequentialReturn _ ->
      Printf.printf "%sReturn statement\n" (indent level)
  | SequentialNull _ ->
      Printf.printf "%sNull statement\n" (indent level)

(* Dump process statement *)
let dump_process_statement level proc =
  let (label, _) = proc.processlabelname in
  Printf.printf "%sProcess: %s\n" (indent level) (if label = "" then "<unnamed>" else label);

  (match proc.processsensitivitylist with
   | SensitivityAll ->
       Printf.printf "%s  Sensitivity: all\n" (indent level)
   | SensitivityExpressionList _ ->
       Printf.printf "%s  Sensitivity: <expression list>\n" (indent level));

  Printf.printf "%s  Statements:\n" (indent level);
  List.iter (dump_sequential_statement (level + 2)) proc.processstatements

(* Dump concurrent statement *)
let dump_concurrent_statement level = function
  | ConcurrentProcessStatement proc ->
      dump_process_statement level proc
  | ConcurrentBlockStatement _ ->
      Printf.printf "%sBlock statement\n" (indent level)
  | ConcurrentProcedureCallStatement _ ->
      Printf.printf "%sProcedure call\n" (indent level)
  | ConcurrentAssertionStatement _ ->
      Printf.printf "%sAssertion\n" (indent level)
  | ConcurrentSignalAssignmentStatement _ ->
      Printf.printf "%sSignal assignment\n" (indent level)
  | ConcurrentComponentInstantiationStatement _ ->
      Printf.printf "%sComponent instantiation\n" (indent level)
  | ConcurrentGenerateStatement _ ->
      Printf.printf "%sGenerate statement\n" (indent level)

(* Dump architecture *)
let dump_architecture arch =
  let (arch_name, _) = arch.archname in
  let (entity_name, _) = arch.archentityname in
  Printf.printf "\n";
  Printf.printf "Architecture: %s of entity %s\n" arch_name entity_name;
  Printf.printf "Concurrent statements:\n";
  List.iter (dump_concurrent_statement 1) arch.archstatements

(* Dump design file *)
let dump_design_file filename =
  Printf.printf "\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  VHDL AST Dump: %s\n" (Filename.basename filename);
  Printf.printf "═══════════════════════════════════════════════════════════════\n";

  match Vhdl_parse.parse_vhdl_file filename with
  | None ->
      Printf.printf "❌ Failed to parse\n";
      false
  | Some design_file ->
      (match Vhdl_elaborate.get_architecture_body design_file with
       | Some arch ->
           dump_architecture arch;
           true
       | None ->
           Printf.printf "No architecture found\n";
           false)
