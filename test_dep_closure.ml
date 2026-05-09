(* Stand-alone smoke test for the dependency-closure logic the GUI uses
   when the user opens a single .sv file.  Reproduces walk_sv_dir +
   close_verible_dependencies so we can check it without booting GTK. *)

let walk_sv_dir ?(max_depth = 4) root =
  let table = Hashtbl.create 256 in
  let rec walk depth dir =
    if depth > max_depth then ()
    else if Sys.file_exists dir && Sys.is_directory dir then begin
      let entries =
        try Array.to_list (Sys.readdir dir) with _ -> [] in
      List.iter (fun e ->
        if e = "" || e.[0] = '.' then ()
        else
          let p = Filename.concat dir e in
          if (try Sys.is_directory p with _ -> false)
          then walk (depth + 1) p
          else if Filename.check_suffix p ".sv"
               || Filename.check_suffix p ".v" then begin
            try
              let text = Sv_preproc.preprocess_file p in
              let names = Sv_preproc.find_module_names text in
              List.iter (fun n ->
                if not (Hashtbl.mem table n) then Hashtbl.add table n p
              ) names
            with _ -> ()
          end
      ) entries
    end
  in
  walk 0 root;
  table

let close_deps ~seed name_map =
  let files = ref [seed] in
  let prog  = ref (Verible_to_behavioral.convert_files_all !files) in
  let max_iters = 8 in
  let iter = ref 0 in
  let changed = ref true in
  while !changed && !iter < max_iters do
    incr iter;
    changed := false;
    let defined = List.fold_left (fun acc (m : Behavioral_ir.bmodule) ->
      m.name :: acc) [] (!prog).modules in
    let referenced =
      List.concat_map (fun (m : Behavioral_ir.bmodule) ->
        List.map (fun (i : Behavioral_ir.binstance) -> i.module_name)
          m.instances) (!prog).modules in
    let unresolved = List.filter (fun n -> not (List.mem n defined))
                       referenced in
    let added = List.fold_left (fun acc n ->
      match Hashtbl.find_opt name_map n with
      | Some path when not (List.mem path !files) ->
          files := path :: !files;
          changed := true;
          (n, path) :: acc
      | _ -> acc
    ) [] unresolved in
    if added <> [] then begin
      Printf.printf "[iter %d] added:\n" !iter;
      List.iter (fun (n, p) ->
        Printf.printf "  %s ← %s\n" n p
      ) added;
      prog := Verible_to_behavioral.convert_files_all !files
    end
  done;
  (List.rev !files, !prog)

let () =
  if Array.length Sys.argv < 2 then begin
    prerr_endline "usage: test_dep_closure <seed.sv>";
    exit 1
  end;
  let seed = Sys.argv.(1) in
  let dir = Filename.dirname seed in
  Printf.printf "[scan] %s\n%!" dir;
  let map = walk_sv_dir dir in
  Printf.printf "[scan] %d modules indexed\n%!" (Hashtbl.length map);
  Printf.printf "[seed] %s\n%!" seed;
  let files, prog = close_deps ~seed map in
  Printf.printf "[result] %d file(s), %d module(s)\n"
    (List.length files) (List.length prog.modules);
  List.iter (fun f -> Printf.printf "  file:   %s\n" f) files;
  List.iter (fun (m : Behavioral_ir.bmodule) ->
    Printf.printf "  module: %s\n" m.name
  ) prog.modules
