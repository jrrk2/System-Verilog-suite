(* Test VHDL Process Extractor *)

open Vhdl_process_extract
open VhdlTypes

(* Helper to get process from architecture *)
let get_process_from_arch arch =
  List.find_map (fun stmt ->
    match stmt with
    | ConcurrentProcessStatement proc -> Some proc
    | _ -> None
  ) arch.archstatements

(* Test process extraction on a VHDL file *)
let test_file filename =
  Printf.printf "\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  Testing Process Extractor: %s\n" (Filename.basename filename);
  Printf.printf "═══════════════════════════════════════════════════════════════\n";

  match Vhdl_parse.parse_vhdl_file filename with
  | None ->
      Printf.printf "❌ Failed to parse\n";
      false
  | Some design_file ->
      (match Vhdl_elaborate.get_architecture_body design_file with
       | Some arch ->
           (match get_process_from_arch arch with
            | Some proc ->
                let info = extract_process_info proc in
                print_process_info info;
                Printf.printf "\n✅ Process extraction successful\n";
                true
            | None ->
                Printf.printf "❌ No process found in architecture\n";
                false)
       | None ->
           Printf.printf "❌ No architecture found\n";
           false)

let () =
  Printf.printf "\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  VHDL Process Extractor Tests\n";
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
