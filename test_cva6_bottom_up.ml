(* Bottom-up cva6 miter.
 *
 * Pairs every module in
 *     test/cva6_ram/cva6_verilate.json.dir/Vcva6.tree.json
 * with the matching entity in
 *     test/cva6_ram/cva6_elab.vhd
 * and runs the Z3 miter on each. Reports a per-module verdict plus a
 * summary table.
 *
 * Usage:
 *   test_cva6_bottom_up <verilator.json> <vivado.vhd> [filter1 ...]
 *
 * Filters are case-sensitive substrings matched against module name;
 * a module passes the filter if it matches *any* given substring. No
 * filters → run every module that exists on both sides.
 *
 * The driver applies the four BIR transformations
 * (unroll → inline → if-lift → mem-infer) on the verilator side
 * before mitering, so genvar bodies and function/task calls don't
 * vacuously block the formal verdict. *)

let usage () =
  Printf.eprintf
    "usage: %s <verilator.json> <vivado.vhd> [filter ...]\n" Sys.argv.(0);
  exit 2

let contains substr s =
  let ls = String.length s and lp = String.length substr in
  if lp = 0 then true
  else if lp > ls then false
  else
    let rec loop i =
      if i + lp > ls then false
      else if String.sub s i lp = substr then true
      else loop (i + 1)
    in loop 0

let apply_passes (p : Behavioral_ir.bprogram) =
  p
  |> Behavioral_unroll.unroll_program
  |> Behavioral_inline.inline_program
  |> Behavioral_iflift.lift_program
  |> Behavioral_meminfer.infer_program

let () =
  if Array.length Sys.argv < 3 then usage ();
  let json_file = Sys.argv.(1) in
  let vhd_file  = Sys.argv.(2) in
  let filters =
    if Array.length Sys.argv > 3
    then Array.to_list (Array.sub Sys.argv 3 (Array.length Sys.argv - 3))
    else []
  in
  let matches_any name =
    filters = [] || List.exists (fun s -> contains s name) filters
  in
  (* Optional: Verible-derived parameter dictionary for the SV
   * sources, picked up via the env var MITER_VERIBLE_FILES (a
   * colon-separated source list, in the same order cva6's verilate
   * uses). When present, gives us each parameterised module's
   * concrete parameter dictionary so we can pair against Vivado's
   * `__parameterized<N>` entities by parameter values rather than
   * port shape. *)
  let verible_specs =
    match Sys.getenv_opt "MITER_VERIBLE_FILES",
          Sys.getenv_opt "MITER_VERIBLE_TOP" with
    | Some flist, Some top ->
        let files = String.split_on_char ':' flist
                    |> List.filter (fun s -> s <> "") in
        let mods = Verible_elaborate.parse_files files in
        Verible_elaborate.specialise_design mods ~top_name:top
    | _ -> []
  in
  if verible_specs <> [] then
    Printf.printf "[verible] %d specialised modules\n%!"
      (List.length verible_specs);

  Printf.printf "═══════════════════════════════════════════════════════\n";
  Printf.printf "  Bottom-up cva6 miter\n";
  Printf.printf "  Verilator: %s\n" json_file;
  Printf.printf "  Vivado:    %s\n" vhd_file;
  if filters <> [] then
    Printf.printf "  Filters:   %s\n" (String.concat " " filters);
  Printf.printf "═══════════════════════════════════════════════════════\n\n";

  Printf.printf "[1/3] Loading verilator JSON → BIR ...\n%!";
  let sv_prog =
    match Verilator_to_behavioral.convert_verilator_json_to_behavioral json_file with
    | Some p -> p
    | None ->
        Printf.eprintf "verilator → BIR failed for %s\n" json_file;
        exit 1
  in
  Printf.printf "  %d modules\n" (List.length sv_prog.modules);

  Printf.printf "[2/3] Loading Vivado VHDL → BIR ...\n%!";
  let viv_prog =
    match Vhdl_to_ver_front.convert_vhd_file vhd_file with
    | Some p -> p
    | None ->
        Printf.eprintf "Vivado VHDL → BIR failed for %s\n" vhd_file;
        exit 1
  in
  Printf.printf "  %d modules\n\n" (List.length viv_prog.modules);

  Printf.printf "[3/3] Applying SV-side passes (unroll → inline → iflift → meminfer)\n%!";
  let sv_prog = apply_passes sv_prog in

  (* Vivado writes parameterized modules as `\<base>__parameterized<N>\`
   * (after our `unescape` step the backslashes are gone, so it's
   * `<base>__parameterized<N>` in the bmodule.name). Verilator names
   * the same module `<base>__<verilator_hash>` (e.g. `popcount__I2`).
   * Pair by the base name when an exact match isn't available. *)
  let base_of n =
    match String.index_opt n '_' with
    | None -> n
    | Some _ ->
        try
          let i = Str.search_forward (Str.regexp "__") n 0 in
          String.sub n 0 i
        with Not_found -> n
  in
  let viv_by_name =
    List.fold_left (fun acc (m : Behavioral_ir.bmodule) ->
      (m.name, m) :: acc) [] viv_prog.modules in
  let viv_by_base =
    List.fold_left (fun acc (m : Behavioral_ir.bmodule) ->
      let b = base_of m.name in
      let bucket = try List.assoc b acc with Not_found -> [] in
      (b, m :: bucket) :: List.remove_assoc b acc
    ) [] viv_prog.modules in

  (* Compare two modules' input/output port shapes — same set of port
   * names with the same widths. Used to pick the "right"
   * parameterization when multiple Vivado entities share a base name. *)
  let port_shape (m : Behavioral_ir.bmodule) =
    List.filter_map (fun (s : Behavioral_ir.bsignal) ->
      match s.direction with
      | `Input | `Output ->
          let w = match s.stype with
            | BInt { width; _ } -> width
            | BArray { size; element = BInt { width; _ }; _ } -> size * width
            | _ -> 0
          in
          Some (s.name, w)
      | _ -> None
    ) m.signals
    |> List.sort compare
  in
  let same_shape sv_m viv_m = port_shape sv_m = port_shape viv_m in

  let pair_for sv_top =
    let nm = sv_top.Behavioral_ir.name in
    match List.assoc_opt nm viv_by_name with
    | Some m -> Some m
    | None ->
        let base = base_of nm in
        (match List.assoc_opt base viv_by_base with
         | Some [] | None -> None
         | Some [m] -> Some m
         | Some candidates ->
             (* Pick the candidate whose port shape matches. Falls back
              * to the first candidate when none match — better to
              * surface the mismatch as a counter-example than skip. *)
             match List.find_opt (same_shape sv_top) candidates with
             | Some m -> Some m
             | None -> Some (List.hd candidates))
  in

  let candidates = List.filter_map (fun (m : Behavioral_ir.bmodule) ->
    if matches_any m.name then
      match pair_for m with
      | Some viv -> Some (m, viv)
      | None -> None
    else None
  ) sv_prog.modules in

  if Sys.getenv_opt "MITER_DEBUG_NAMES" <> None then begin
    Printf.eprintf "[debug] verilator-side module names:\n";
    List.iter (fun (m : Behavioral_ir.bmodule) ->
      Printf.eprintf "  sv:  %s\n" m.name) sv_prog.modules;
    Printf.eprintf "[debug] vivado-side entity names:\n";
    List.iter (fun (m : Behavioral_ir.bmodule) ->
      Printf.eprintf "  viv: %s\n" m.name) viv_prog.modules
  end;
  Printf.printf "\nPaired %d modules%s.\n\n" (List.length candidates)
    (if filters <> [] then " (after filter)" else "");

  let results = ref [] in
  let saved_stdout = Unix.dup Unix.stdout in
  List.iter (fun ((sv_top : Behavioral_ir.bmodule), (viv_top : Behavioral_ir.bmodule)) ->
    let label =
      if sv_top.name = viv_top.name then sv_top.name
      else Printf.sprintf "%s ↔ %s" sv_top.name viv_top.name
    in
    Printf.printf "─── %s ───%!" label;
    (* The Z3 miter prints a verbose per-encoding dump on stdout.
     * Redirect to /dev/null while running it so the per-module loop
     * stays readable, then restore for the verdict line. *)
    let devnull = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0 in
    Unix.dup2 devnull Unix.stdout;
    Unix.close devnull;
    let result =
      try
        let ok = Z3_miter.check_miter_equivalence viv_top sv_top in
        if ok then `Pass else `Fail
      with e ->
        `Error (Printexc.to_string e)
    in
    Unix.dup2 saved_stdout Unix.stdout;
    let mark = match result with
      | `Pass -> "✅"
      | `Fail -> "❌"
      | `Error _ -> "⚠"
    in
    Printf.printf " %s\n%!" mark;
    results := (sv_top.name, result) :: !results
  ) candidates;
  Unix.close saved_stdout;

  let pass = List.length (List.filter (fun (_, r) -> r = `Pass) !results) in
  let fail = List.length (List.filter (fun (_, r) -> r = `Fail) !results) in
  let err  = List.length (List.filter (fun (_, r) ->
    match r with `Error _ -> true | _ -> false) !results) in

  Printf.printf "═══════════════════════════════════════════════════════\n";
  Printf.printf "  Summary: %d ✅  %d ❌  %d ⚠   (total %d)\n"
    pass fail err (List.length !results);
  Printf.printf "═══════════════════════════════════════════════════════\n";
  if fail = 0 && err = 0 then exit 0 else exit 1
