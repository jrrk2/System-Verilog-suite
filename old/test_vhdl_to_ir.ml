(* Test VHDL to IR Converter *)

open Vhdl_to_ir

(* Test conversion on a VHDL file *)
let test_file filename =
  Printf.printf "\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  Converting VHDL to IR: %s\n" (Filename.basename filename);
  Printf.printf "═══════════════════════════════════════════════════════════════\n";

  match convert_vhdl_file_to_ir filename with
  | None ->
      Printf.printf "❌ Conversion failed\n";
      false
  | Some ir ->
      print_ir ir;
      Printf.printf "✅ Conversion successful\n";
      true

let () =
  Printf.printf "\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  VHDL to IR Converter Tests\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";

  let vhd_dir = Sys.getenv_opt "HOME" |> Option.value ~default:"/Users/jonathan" in
  let test_files = [
    vhd_dir ^ "/gnusynthesis/vhd_front/slib_clock_div.vhd";
    vhd_dir ^ "/gnusynthesis/vhd_front/slib_input_filter.vhd";
    vhd_dir ^ "/gnusynthesis/vhd_front/slib_mv_filter.vhd";
    vhd_dir ^ "/gnusynthesis/vhd_front/uart_baudgen.vhd";
  ] in

  let results = List.map test_file test_files in
  let passed = List.filter (fun x -> x) results |> List.length in
  let total = List.length results in

  Printf.printf "\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  Summary: %d/%d tests passed\n" passed total;
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "\n"
