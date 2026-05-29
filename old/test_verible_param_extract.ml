(* Probe for Verible_elaborate with package-import resolution.
 *
 * Usage:
 *   test_verible_param_extract <top> <file.sv> [more.sv ...]
 *
 * Parses each file (modules and packages), walks the hierarchy from
 * the chosen top, and prints the specialised module list with their
 * parameter dictionaries. Package-level constants (e.g.
 * `config_pkg::XLEN`) are folded into integer values during the
 * walk. *)

let () =
  if Array.length Sys.argv < 3 then begin
    Printf.eprintf "usage: %s <top> <file.sv> [more.sv ...]\n" Sys.argv.(0);
    exit 2
  end;
  let top = Sys.argv.(1) in
  let files = Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2)) in
  let mods, pkgs = Verible_elaborate.parse_files_full files in
  Printf.printf "Parsed %d modules and %d packages from %d files.\n"
    (List.length mods) (List.length pkgs) (List.length files);
  List.iter (fun (p : Verible_elaborate.package_decl) ->
    Printf.printf "  package %s:\n" p.pkg_name;
    List.iter (fun (n, v) ->
      Printf.printf "    %s = %s\n" n (Verible_elaborate.string_of_pvalue v)
    ) p.pkg_params
  ) pkgs;
  List.iter (fun (m : Verible_elaborate.module_decl) ->
    Printf.printf "  module %s (params: %s)\n" m.m_name
      (String.concat ", " m.m_params)
  ) mods;
  Printf.printf "\nElaborating from top = %s ...\n\n" top;
  let specs = Verible_elaborate.specialise_design ~pkgs mods ~top_name:top in
  Printf.printf "%d specialised module(s):\n" (List.length specs);
  List.iter (fun (s : Verible_elaborate.specialised) ->
    Printf.printf "  %-40s base=%s%s%s\n"
      s.s_name s.s_base
      (if s.s_params = [] then ""
       else "  params=[" ^
            String.concat ", "
              (List.map (fun (k, v) -> k ^ "=" ^ v) s.s_params) ^ "]")
      (match s.s_inst with
       | Some i -> Printf.sprintf "  inst=%s" i
       | None -> "")
  ) specs
