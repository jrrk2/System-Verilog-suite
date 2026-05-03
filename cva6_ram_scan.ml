(* Scan a directory of .sv files for inferred memory arrays.
 *
 * For each file:
 *   1. Run Verilator --json-only to produce a parse-tree.
 *   2. Convert to BIR via Verilator_to_behavioral.
 *   3. Run Behavioral_meminfer.infer_program.
 *   4. Print every (module, mem) with kind_label.
 *
 * Then tally how many distinct categories were seen. The point is to
 * see what RAM patterns the cva6 regression suite actually uses, so
 * we know which sub-module abstractions are worth emitting (and which
 * primitives the ASIC backend must provide). *)

let inc_dirs = ref []
let lib_dirs = ref []
let extra_files = ref []

let run_verilator ~top ~src ~mdir =
  let _ = Sys.command (Printf.sprintf "rm -rf %s && mkdir -p %s" mdir mdir) in
  let inc = String.concat " " (List.rev_map (fun d -> "-I" ^ d) !inc_dirs) in
  let libs = String.concat " " (List.rev_map (fun d -> "-y " ^ d) !lib_dirs) in
  (* Preserve the order the user gave on the command line: argv-order
   * matters because earlier files define macros/packages used later. *)
  let extras_in_order = List.rev !extra_files in
  (* Drop any extra file that equals the file currently being scanned
   * — verilator rejects duplicate module definitions. *)
  let extras_filtered =
    List.filter (fun f ->
      try not (Sys.file_exists f) || not (Sys.file_exists src)
          || (Unix.stat f).st_ino <> (Unix.stat src).st_ino
      with _ -> true
    ) extras_in_order
  in
  let extra = String.concat " " extras_filtered in
  let cmd = Printf.sprintf
    "verilator --json-only -Wno-fatal -Wno-WIDTHTRUNC -Wno-WIDTHEXPAND \
     -Wno-UNOPTFLAT -Wno-ASCRANGE -Wno-MULTIDRIVEN -Wno-UNUSEDPARAM \
     -Wno-UNUSEDSIGNAL -Wno-IMPORTSTAR -Wno-IMPLICIT -Wno-PINMISSING \
     -Wno-CASEINCOMPLETE -Wno-LATCH -Wno-VARHIDDEN -Wno-ZEROREPL \
     -Wno-SYMRSVDWORD -Wno-CASTCONST -Wno-SELRANGE -Wno-PROFOUTOFDATE \
     -Wno-COMBDLY -Wno-INITIALDLY -Wno-STMTDLY -Wno-REALCVT -Wno-NULLPORT \
     -Wno-ENUMVALUE -Wno-CMPCONST -Wno-UNDRIVEN -Wno-IFDEPTH \
     %s %s --top-module %s %s %s --Mdir %s > /dev/null 2>&1"
    inc libs top extra src mdir in
  Sys.command cmd

(* Grep `^\s*module\s+<name>` (and `^\s*macromodule\s+...`) from a
 * source file. Lets us treat multi-module .sv files (e.g. SweRV's
 * mem_lib.sv with 41 RAM macros) by enumerating each module as its
 * own top. *)
let module_decls_in_file path =
  let names = ref [] in
  let re = Str.regexp "^[ \t]*\\(macromodule\\|module\\)[ \t]+\\([A-Za-z_][A-Za-z0-9_$]*\\)" in
  let ic = open_in path in
  (try
     while true do
       let line = input_line ic in
       if Str.string_match re line 0 then
         names := Str.matched_group 2 line :: !names
     done
   with End_of_file -> ());
  close_in ic;
  List.rev !names

let scan_one ~top ~path =
  let mdir = Printf.sprintf "/tmp/cva6_ram_scan_%s" top in
  let rc = run_verilator ~top ~src:path ~mdir in
  if rc <> 0 then begin
    Printf.printf "%-40s  [verilator failed]\n" top;
    []
  end else begin
    let json = Printf.sprintf "%s/V%s.tree.json" mdir top in
    if not (Sys.file_exists json) then begin
      Printf.printf "%-40s  [no JSON produced]\n" top;
      []
    end else
      match Verilator_to_behavioral.convert_verilator_json_to_behavioral json with
      | None ->
          Printf.printf "%-40s  [BIR conversion failed]\n" top;
          []
      | Some p ->
          let p = Behavioral_meminfer.infer_program p in
          let mems_in_top =
            List.concat_map (fun (m : Behavioral_ir.bmodule) ->
              List.map (fun mm -> (m.name, mm)) m.mems
            ) p.modules
          in
          if mems_in_top = [] then begin
            Printf.printf "%-40s  (no memories)\n" top;
            []
          end else begin
            List.iter (fun (mname, (mm : Behavioral_ir.bmem)) ->
              Printf.printf "%-40s  %-30s %dx%d  %s\n"
                top mname mm.depth mm.data_width
                (Behavioral_meminfer.kind_label mm)
            ) mems_in_top;
            mems_in_top
          end
  end

(* For each .sv/.v file, enumerate its `module` declarations and try
 * each as the verilator top. Single-module files behave as before
 * (one verilator run per file). Multi-module files (mem_lib.sv) get
 * one run per declared module. *)
let scan_file path =
  let base = Filename.basename path in
  let default_top = Filename.remove_extension base in
  match module_decls_in_file path with
  | [] | [_] -> scan_one ~top:default_top ~path
  | tops ->
      List.concat_map (fun t -> scan_one ~top:t ~path) tops

let () =
  let dir = ref "/home/jonathan/System-Verilog-decompiler/test/cva6_ram" in
  let usage = "cva6_ram_scan [-I dir] [-y dir] [-f file] [scan_dir]" in
  let speclist = [
    "-I", Arg.String (fun d -> inc_dirs := d :: !inc_dirs),
          "<dir>  add include search path";
    "-y", Arg.String (fun d -> lib_dirs := d :: !lib_dirs),
          "<dir>  add library search path (verilator -y)";
    "-f", Arg.String (fun f -> extra_files := f :: !extra_files),
          "<file>  prepend file to verilator (e.g. package definitions)";
  ] in
  Arg.parse speclist (fun s -> dir := s) usage;
  let dir = !dir in
  let files = Sys.readdir dir in
  Array.sort String.compare files;
  let svs = Array.to_list files
            |> List.filter (fun f ->
                 Filename.check_suffix f ".sv"
                 || Filename.check_suffix f ".v")
            |> List.map (fun f -> Filename.concat dir f) in
  Printf.printf "Scanning %d .sv files in %s\n\n" (List.length svs) dir;
  Printf.printf "%-40s  %-30s %s  %s\n"
    "file" "memory" "shape" "category";
  Printf.printf "%s\n" (String.make 100 '-');
  let all = List.concat_map scan_file svs in
  Printf.printf "\n=== Summary ===\n";
  Printf.printf "Total memories inferred: %d\n" (List.length all);
  let by_cat = Hashtbl.create 8 in
  List.iter (fun (_, mm) ->
    let cat = Behavioral_meminfer.kind_label mm in
    Hashtbl.replace by_cat cat (1 + (try Hashtbl.find by_cat cat
                                     with Not_found -> 0))
  ) all;
  let pairs = Hashtbl.fold (fun k v acc -> (k, v) :: acc) by_cat [] in
  let pairs = List.sort (fun (_, a) (_, b) -> compare b a) pairs in
  List.iter (fun (k, n) -> Printf.printf "  %4d  %s\n" n k) pairs
