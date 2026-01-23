(* VHDL Typed AST to IR - Simple converter using VhdlTypes directly *)
(*
 * Works directly with the typed AST from VhdlParser (VhdlTypes)
 * This is simpler than converting through vhdintf trees
 *
 * Assumptions:
 * - std_logic/std_logic_vector types
 * - Standard synchronous patterns
 * - Integer generics for parameterization
 *)

open Vhd_front.VhdlTypes
open Sv_ast

(* Extract module name and basic info from parsed VHDL *)
let extract_entity_info design_file =
  let rec find_entity = function
    | [] -> None
    | unit :: rest ->
        (match unit.design_unit_context_items, unit.design_unit_library_unit with
         | _, PrimaryUnit (EntityDeclaration entity) ->
             Some (fst entity.entity_declaration_identifier,
                   entity.entity_declaration_header,
                   entity.entity_declaration_declarative_items,
                   entity.entity_declaration_statements)
         | _ -> find_entity rest)
  in
  find_entity design_file

let () =
  Printf.printf "VHDL Typed AST to IR converter - placeholder\n";
  Printf.printf "This converter works directly with VhdlTypes (the typed AST)\n";
  Printf.printf "Next step: Implement entity/architecture extraction\n"
