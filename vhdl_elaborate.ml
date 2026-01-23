(* VHDL Elaborator - Extract processes and assignments from VHDL AST *)

open Vhd_front.VhdlTypes

(* Extract architecture body from design units *)
let get_architecture_body design_file =
  List.find_map (fun design_unit ->
    match design_unit.designunitlibraryunit with
    | SecondaryUnit (ArchitectureBody arch) -> Some arch
    | _ -> None
  ) design_file

(* Extract entity declaration *)
let get_entity design_file =
  List.find_map (fun design_unit ->
    match design_unit.designunitlibraryunit with
    | PrimaryUnit (EntityDeclaration entity) -> Some entity
    | _ -> None
  ) design_file

(* Print architecture information *)
let print_architecture arch =
  let (arch_name, _) = arch.archname in
  let (entity_name, _) = arch.archentityname in
  Printf.printf "Architecture: %s of entity %s\n" arch_name entity_name;
  Printf.printf "  Statements: %d\n" (List.length arch.archstatements)

(* Analyze a VHDL file *)
let analyze_file filename =
  Printf.printf "\nAnalyzing: %s\n" filename;
  Printf.printf "════════════════════════════════════════════════════════════\n";

  match Vhdl_parse.parse_vhdl_file filename with
  | None ->
      Printf.printf "❌ Failed to parse\n";
      false
  | Some design_file ->
      Printf.printf "✅ Parsed successfully (%d design units)\n" (List.length design_file);

      (match get_entity design_file with
       | Some entity ->
           let (entity_name, _) = entity.entityname in
           Printf.printf "\nEntity: %s\n" entity_name
       | None ->
           Printf.printf "  No entity found\n");

      (match get_architecture_body design_file with
       | Some arch ->
           Printf.printf "\n";
           print_architecture arch;
           Printf.printf "\nNext step: Extract process statements and convert to IR\n";
           true
       | None ->
           Printf.printf "  No architecture body found\n";
           false)
