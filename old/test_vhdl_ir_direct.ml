(* Test VHDL to IR Direct Conversion *)

open Vhdl_to_ir_direct

let test_conversion vhdl_file =
  Printf.printf "Testing Direct VHDL → IR Conversion\n";
  Printf.printf "%s\n" (String.make 70 '=');
  Printf.printf "File: %s\n\n" vhdl_file;

  let ctx = convert_vhdl_to_ir [vhdl_file] in

  Printf.printf "%s\n" (String.make 70 '=');
  Printf.printf "✅ Conversion Results\n\n";

  Printf.printf "📊 IR Statistics:\n";
  Printf.printf "   Nodes:     %d\n" (List.length ctx.nodes);
  Printf.printf "   Signals:   %d\n" (Hashtbl.length ctx.signals);
  Printf.printf "   Constants: %d\n" (List.length ctx.constants);
  Printf.printf "   Inputs:    %d\n" (List.length ctx.inputs);
  Printf.printf "   Outputs:   %d\n" (List.length ctx.outputs);
  Printf.printf "   Wires:     %d\n\n" (List.length ctx.wires);

  (* Show generated IR nodes *)
  if List.length ctx.nodes > 0 then begin
    Printf.printf "🔧 Sample IR Nodes:\n";
    let show_op = function
      | Sv_ast.Register { width; clock; reset; enable; reset_value } ->
          Printf.sprintf "Register(w=%d, clk=$%d, rst=%s)"
            width clock
            (match reset with Some r -> Printf.sprintf "$%d" r | None -> "none")
      | Sv_ast.Compare { cmp_op; width; signed } ->
          let op_str = match cmp_op with
            | `Eq -> "==" | `Ne -> "!=" | `Lt -> "<"
            | `Le -> "<=" | `Gt -> ">" | `Ge -> ">="
          in
          Printf.sprintf "Compare(%s, w=%d)" op_str width
      | Sv_ast.Add { width; signed } ->
          Printf.sprintf "Add(w=%d, %s)" width (if signed then "signed" else "unsigned")
      | Sv_ast.Sub { width; signed } ->
          Printf.sprintf "Sub(w=%d, %s)" width (if signed then "signed" else "unsigned")
      | Sv_ast.And { width } -> Printf.sprintf "And(w=%d)" width
      | Sv_ast.Or { width } -> Printf.sprintf "Or(w=%d)" width
      | Sv_ast.Xor { width } -> Printf.sprintf "Xor(w=%d)" width
      | Sv_ast.Mux { width } -> Printf.sprintf "Mux(w=%d)" width
      | Sv_ast.Shift { width; direction; arithmetic; amount } ->
          let dir = match direction with `Left -> "<<" | `Right -> ">>" in
          Printf.sprintf "Shift(%s, w=%d)" dir width
      | _ -> "Other"
    in

    List.iter (fun (id, op, inputs) ->
      Printf.printf "   $%d = %s (" id (show_op op);
      List.iter (fun inp -> Printf.printf "$%d " inp) inputs;
      Printf.printf ")\n"
    ) (List.rev ctx.nodes |> (fun lst ->
      if List.length lst > 10 then List.filteri (fun i _ -> i < 10) lst else lst));

    if List.length ctx.nodes > 10 then
      Printf.printf "   ... (%d more nodes)\n" (List.length ctx.nodes - 10)
  end;

  Printf.printf "\n%s\n" (String.make 70 '=');
  Printf.printf "💡 Key Advantage:\n";
  Printf.printf "   • Direct conversion: VHDL → IR (no intermediate SV)\n";
  Printf.printf "   • Uses proven match2' patterns from rewrite.ml\n";
  Printf.printf "   • Generates optimizable IR immediately\n"

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.printf "Usage: %s <vhdl_file>\n" Sys.argv.(0);
    Printf.printf "\nExample:\n";
    Printf.printf "  %s sysver_tests/slib_input_sync.vhd\n" Sys.argv.(0);
    exit 1
  end;

  test_conversion Sys.argv.(1)
