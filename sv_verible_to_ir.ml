(* sv_verible_to_ir.ml - Convert Verible AST to IR *)

[@@@warning "-33"]

open Sv_ast
open Source_text_verible

(* Parse Verilog file using Verible parser *)
let parse_verible_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  try
    (* Create deflated lexer that converts token list to stream *)
    let deflated_lexer = Source_text_verible_lex.deflate Source_text_verible_lex.token in

    (* Parse with the deflated lexer to get TUPLE tree *)
    let parse_tree = Source_text_verible.ml_start deflated_lexer lexbuf in
    close_in ic;
    Some parse_tree
  with
  | Parsing.Parse_error ->
      close_in ic;
      Printf.eprintf "Parse error in %s\n" filename;
      None
  | e ->
      close_in ic;
      Printf.eprintf "Error parsing %s: %s\n" filename (Printexc.to_string e);
      Printexc.print_backtrace stderr;
      None

(* Convert expression tree to IR value_id *)
let rec expr_to_ir ir expr_cache expr =
  (* Check if we've already converted this expression *)
  try
    Hashtbl.find expr_cache expr
  with Not_found ->
    let result =
      match expr with
      | TUPLE4 (STRING "add_expr2", left, PLUS, right) ->
          let left_id = expr_to_ir ir expr_cache left in
          let right_id = expr_to_ir ir expr_cache right in
          (* TODO: Properly infer width and signedness from operands *)
          Sv_opt_ir.add_node ir (Add { width = 4; signed = false }) [left_id; right_id]

      | TUPLE4 (STRING "mul_expr2", left, STAR, right) ->
          let left_id = expr_to_ir ir expr_cache left in
          let right_id = expr_to_ir ir expr_cache right in
          (* TODO: Properly infer width and signedness from operands *)
          Sv_opt_ir.add_node ir (Mul { width = 8; signed = false }) [left_id; right_id]

      | TUPLE4 (STRING "add_expr3", left, HYPHEN, right) ->
          let left_id = expr_to_ir ir expr_cache left in
          let right_id = expr_to_ir ir expr_cache right in
          (* TODO: Properly infer width and signedness from operands *)
          Sv_opt_ir.add_node ir (Sub { width = 4; signed = false }) [left_id; right_id]

      | TUPLE3 (STRING "unqualified_id1", SymbolIdentifier name, _) ->
          (* Look up input or intermediate value *)
          (try
            let v = Hashtbl.find ir.ir_inputs name in
            (match v with
             | Input { id; _ } -> id
             | _ -> Sv_opt_ir.get_new_id ir)
          with Not_found ->
            Printf.eprintf "Warning: Unknown identifier '%s' in expression\n" name;
            Sv_opt_ir.get_new_id ir)

      | SymbolIdentifier name ->
          (* Look up input *)
          (try
            let v = Hashtbl.find ir.ir_inputs name in
            (match v with
             | Input { id; _ } -> id
             | _ -> Sv_opt_ir.get_new_id ir)
          with Not_found ->
            Printf.eprintf "Warning: Unknown identifier '%s'\n" name;
            Sv_opt_ir.get_new_id ir)

      | TK_DecNumber n ->
          (try
            let value = int_of_string n in
            (* For now, assume constants are 32-bit - TODO: infer proper width *)
            Sv_opt_ir.get_or_create_constant ir value 32
          with _ ->
            Printf.eprintf "Warning: Invalid number '%s'\n" n;
            Sv_opt_ir.get_new_id ir)

      | _ ->
          Printf.eprintf "Warning: Unhandled expression type\n";
          Sv_opt_ir.get_new_id ir
    in
    Hashtbl.add expr_cache expr result;
    result

(* Convert Verible AST to IR with elaboration *)
let verible_to_ir verible_ast module_name =
  (* Step 1: Elaborate - resolve parameters and widths *)
  let elab_ctx = Sv_elaborate.elaborate verible_ast in

  (* Debug: print elaboration context *)
  if !Sys.interactive then begin
    Printf.printf "=== Elaboration Context ===\n";
    Sv_elaborate.print_context elab_ctx
  end;

  (* Step 2: Create IR with elaborated information *)
  let ir = Sv_opt_ir.create_ir module_name in

  (* Step 3: Add ports to IR *)
  Printf.printf "\n=== Converting to IR ===\n\n";

  (* Add inputs *)
  List.iter (fun (port : Sv_elaborate.port_info) ->
    match port.port_direction with
    | "input" ->
        ignore (Sv_opt_ir.add_input ir port.port_name port.port_width);
        Printf.printf "Added input: %s[%d]\n" port.port_name port.port_width
    | _ -> ()
  ) (List.rev elab_ctx.ports);  (* Reverse to get original order *)

  (* Add outputs *)
  List.iter (fun (port : Sv_elaborate.port_info) ->
    match port.port_direction with
    | "output" ->
        ignore (Sv_opt_ir.add_output ir port.port_name port.port_width);
        Printf.printf "Added output: %s[%d]\n" port.port_name port.port_width
    | _ -> ()
  ) (List.rev elab_ctx.ports);

  (* Step 4: Convert assign statements to IR operations *)
  let expr_cache = Hashtbl.create 50 in
  List.iter (fun (assign : Sv_elaborate.assign_info) ->
    Printf.printf "Converting assign: %s = <expr>\n" assign.assign_lhs;
    let value_id = expr_to_ir ir expr_cache assign.assign_rhs in

    (* Connect the computed value to the output *)
    (try
      let output_val = Hashtbl.find ir.ir_outputs assign.assign_lhs in
      (match output_val with
       | Output { id; _ } ->
           (* Map output to the computed value *)
           Hashtbl.replace ir.ir_value_to_node id value_id;
           Printf.printf "  Connected to output %s (id=%d -> value_id=%d)\n" assign.assign_lhs id value_id
       | _ ->
           Printf.eprintf "Warning: '%s' is not an output\n" assign.assign_lhs)
    with Not_found ->
      Printf.eprintf "Warning: Output '%s' not found\n" assign.assign_lhs)
  ) (List.rev elab_ctx.assigns);

  Printf.printf "\n✓ IR conversion complete\n";
  Printf.printf "  Inputs: %d, Outputs: %d, Nodes: %d\n"
    (Hashtbl.length ir.ir_inputs)
    (Hashtbl.length ir.ir_outputs)
    (Hashtbl.length ir.ir_nodes);

  ir

(* Main entry point: parse Verilog file and convert to IR *)
let file_to_ir filename =
  match parse_verible_file filename with
  | None -> None
  | Some ast ->
      (* Extract module name from filename as fallback *)
      let module_name = Filename.chop_extension (Filename.basename filename) in
      Some (verible_to_ir ast module_name)
