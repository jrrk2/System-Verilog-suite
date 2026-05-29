(* Test the simple VHDL to IR converter *)

open Vhdl_simple_to_ir

let test_file filename =
  Printf.printf "Testing simple VHDL→IR conversion on: %s\n" filename;
  Printf.printf "%s\n" (String.make 60 '=');

  (* Parse VHDL file *)
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in

  try
    let ast = Vhd_front.VhdlParser.top_level_file Vhd_front.VhdlLexer.lexer lexbuf in
    close_in chan;

    Printf.printf "✅ Parsed successfully\n";
    Printf.printf "   Design units: %d\n" (List.length ast);

    (* Convert to IR *)
    match convert_vhdl_file ast with
    | Some ctx ->
        Printf.printf "\n✅ Converted to IR\n";
        Printf.printf "   Inputs:  %d\n" (List.length ctx.inputs);
        Printf.printf "   Outputs: %d\n" (List.length ctx.outputs);
        Printf.printf "   Wires:   %d\n" (List.length ctx.wires);
        Printf.printf "   Nodes:   %d\n" (List.length ctx.nodes);

        Printf.printf "\n📊 Signal Summary:\n";
        Printf.printf "   Inputs:\n";
        List.iter (fun (name, width) ->
          Printf.printf "      %s [%d]\n" name width
        ) (List.rev ctx.inputs);

        Printf.printf "   Outputs:\n";
        List.iter (fun (name, width) ->
          Printf.printf "      %s [%d]\n" name width
        ) (List.rev ctx.outputs);

        if List.length ctx.wires > 0 then begin
          Printf.printf "   Internal wires:\n";
          List.iter (fun (name, width) ->
            Printf.printf "      %s [%d]\n" name width
          ) (List.rev ctx.wires)
        end;

        Printf.printf "\n🔧 IR Nodes:\n";
        let show_op = function
          | Sv_ast.Register { width; clock; reset; enable; reset_value } ->
              Printf.sprintf "Register(w=%d, clk=%d, rst=%s, en=%s)"
                width clock
                (match reset with Some r -> string_of_int r | None -> "none")
                (match enable with Some e -> string_of_int e | None -> "none")
          | Sv_ast.Compare { cmp_op; width; signed } ->
              let op_str = match cmp_op with
                | `Eq -> "==" | `Ne -> "!="
                | `Lt -> "<"  | `Le -> "<="
                | `Gt -> ">"  | `Ge -> ">="
              in
              Printf.sprintf "Compare(%s, w=%d, %s)" op_str width
                (if signed then "signed" else "unsigned")
          | Sv_ast.Add { width; signed } ->
              Printf.sprintf "Add(w=%d, %s)" width (if signed then "signed" else "unsigned")
          | Sv_ast.Sub { width; signed } ->
              Printf.sprintf "Sub(w=%d, %s)" width (if signed then "signed" else "unsigned")
          | Sv_ast.And { width } -> Printf.sprintf "And(w=%d)" width
          | Sv_ast.Or { width } -> Printf.sprintf "Or(w=%d)" width
          | Sv_ast.Xor { width } -> Printf.sprintf "Xor(w=%d)" width
          | Sv_ast.Not { width } -> Printf.sprintf "Not(w=%d)" width
          | Sv_ast.Mux { width } -> Printf.sprintf "Mux(w=%d)" width
          | Sv_ast.Extract { width; lsb; msb } ->
              Printf.sprintf "Extract(w=%d, [%d:%d])" width msb lsb
          | Sv_ast.Shift { width; direction; arithmetic; amount } ->
              let dir_str = match direction with `Left -> "<<" | `Right -> ">>" in
              Printf.sprintf "Shift(%s, w=%d, %s)" dir_str width
                (if arithmetic then "arith" else "logical")
          | _ -> "Other"
        in

        let nodes = List.rev ctx.nodes in
        List.iter (fun (id, op, inputs) ->
          Printf.printf "   $%d = %s (" id (show_op op);
          List.iter (fun inp -> Printf.printf "$%d " inp) inputs;
          Printf.printf ")\n"
        ) nodes;

    | None ->
        Printf.printf "❌ Failed to convert to IR\n"

  with
  | Parsing.Parse_error ->
      close_in chan;
      Printf.printf "❌ Parse error\n";
      exit 1
  | e ->
      close_in chan;
      Printf.printf "❌ Error: %s\n" (Printexc.to_string e);
      exit 1

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.printf "Usage: %s <vhdl_file>\n" Sys.argv.(0);
    Printf.printf "\nExamples:\n";
    Printf.printf "  %s sysver_tests/slib_input_sync.vhd\n" Sys.argv.(0);
    Printf.printf "  %s sysver_tests/slib_edge_detect.vhd\n" Sys.argv.(0);
    exit 1
  end;

  test_file Sys.argv.(1);
  Printf.printf "\n%s\n" (String.make 60 '=');
  Printf.printf "✅ Test completed\n"
