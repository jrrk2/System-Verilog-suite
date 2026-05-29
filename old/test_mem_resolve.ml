(* CLI for the memory-macro resolver.
 *
 * Usage:
 *   test_mem_resolve sram <tech> <num_rw> <num_r> <word_size> <num_words>
 *   test_mem_resolve rom  <tech> <word_size> <num_words> [val val val …]
 *
 * <tech> ∈ {sky130, scn4m_subm, freepdk45, gf180mcu}
 *
 * Prints the cache directory and the canonical port shape on stdout.
 * Generates the macro on cache miss; subsequent runs of the same
 * shape skip OpenRAM. *)

let usage () =
  prerr_endline
    "usage: test_mem_resolve sram <tech> <n_rw> <n_r> <word_size> <num_words>";
  prerr_endline
    "       test_mem_resolve rom  <tech> <word_size> <num_words> [val …]";
  exit 2

let () =
  let argv = Sys.argv in
  if Array.length argv < 5 then usage ();
  let kind_s = argv.(1) in
  let tech = Mem_macro_resolve.tech_of_string argv.(2) in
  let req =
    match kind_s with
    | "sram" ->
        if Array.length argv < 7 then usage ();
        let n_rw = int_of_string argv.(3) in
        let n_r  = int_of_string argv.(4) in
        let word_size = int_of_string argv.(5) in
        let num_words = int_of_string argv.(6) in
        Mem_macro_resolve.{
          tech;
          kind = Sram { n_rw; n_r; n_w = 0 };
          word_size; num_words;
        }
    | "rom" ->
        let word_size = int_of_string argv.(3) in
        let num_words = int_of_string argv.(4) in
        let init_values =
          if Array.length argv > 5 then
            Array.to_list (Array.sub argv 5 (Array.length argv - 5))
            |> List.map (fun s ->
                if String.length s > 2 && (String.sub s 0 2 = "0x" || String.sub s 0 2 = "0X")
                then int_of_string s
                else int_of_string s)
          else []
        in
        Mem_macro_resolve.{
          tech;
          kind = Rom { init_values };
          word_size; num_words;
        }
    | _ -> usage ()
  in
  let a = Mem_macro_resolve.resolve req in
  Printf.printf "OK\n";
  Printf.printf "  module: %s\n" a.module_name;
  Printf.printf "  cache:  %s\n" a.cache_dir;
  Printf.printf "  v:      %s\n" a.verilog_path;
  Printf.printf "  lib:    %s\n" a.liberty_path;
  Printf.printf "  lef:    %s\n"
    (match a.lef_path with Some p -> p | None -> "(none)");
  Printf.printf "  gds:    %s\n"
    (match a.gds_path with Some p -> p | None -> "(none)");
  Printf.printf "  shape:  %d-bit × %d, addr_w=%d\n"
    a.port_shape.data_width
    (1 lsl a.port_shape.addr_width)
    a.port_shape.addr_width;
  let pp_list label = function
    | [] -> ()
    | xs ->
        Printf.printf "    %s: %s\n" label
          (String.concat "," (List.map (function
             | None -> "-" | Some s -> s) xs))
  in
  Printf.printf "    clk:  %s\n" (String.concat "," a.port_shape.clk);
  Printf.printf "    csb:  %s\n" (String.concat "," a.port_shape.csb);
  pp_list "web" a.port_shape.web;
  pp_list "wmask" a.port_shape.wmask;
  Printf.printf "    addr: %s\n" (String.concat "," a.port_shape.addr);
  pp_list "din " a.port_shape.din;
  Printf.printf "    dout: %s\n" (String.concat "," a.port_shape.dout)
