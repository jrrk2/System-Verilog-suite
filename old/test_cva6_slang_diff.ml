(* Per-entity Slang ↔ Verible miter on cva6.
 *
 * Two pre-synth SV elaborators run independently on the same flat
 * cva6 source, then every paired bmodule goes through the Z3 miter.
 * Unlike test_cva6_ff_diff (which mitres against Vivado's
 * synthesised VHDL), this is the pure SV-elaboration agreement
 * check — no Xilinx primitives, no synth-time state pruning, no
 * Yosys lowering. The expected outcome on a Verible bug-free run
 * is 100 % PROVEN EQUIVALENT.
 *
 * Slang is invoked once over the whole flat file (~7s for cva6),
 * its --ast-json output is parsed, and then we sweep every
 * InstanceBody. Verible runs through the standard convert_files +
 * downstream pipeline (unroll → inline → iflift → blocking_subst
 * → meminfer → flatten).
 *
 * Usage:
 *   test_cva6_slang_diff [filter-substr ...]
 *
 * Env:
 *   MITER_FLAT_SV    — path to the flat cva6 SV file
 *                      (default: test/cva6_ram/cva6_flat.sv)
 *   MITER_TOP        — top module name (default: cva6) *)

open Behavioral_ir

let () =
  let flat_sv = match Sys.getenv_opt "MITER_FLAT_SV" with
    | Some p -> p
    | None -> "test/cva6_ram/cva6_flat.sv" in
  let top = match Sys.getenv_opt "MITER_TOP" with
    | Some t -> t | None -> "cva6" in
  let filters =
    Array.sub Sys.argv 1 (Array.length Sys.argv - 1)
    |> Array.to_list in
  let contains substr s =
    let ls = String.length s and lsub = String.length substr in
    if lsub > ls then false
    else
      let rec scan i =
        if i + lsub > ls then false
        else if String.sub s i lsub = substr then true
        else scan (i + 1)
      in scan 0
  in
  let matches name =
    filters = [] || List.exists (fun f -> contains f name) filters
  in

  Printf.printf "═══════════════════════════════════════════════════════\n";
  Printf.printf "  Slang ↔ Verible per-entity miter on cva6\n";
  Printf.printf "  source: %s, top: %s\n" flat_sv top;
  Printf.printf "═══════════════════════════════════════════════════════\n\n";

  Printf.printf "[1/3] Slang → BIR ...\n%!";
  let t0 = Unix.gettimeofday () in
  let slang_prog =
    match Slang_to_behavioral.convert_files ~top [flat_sv] with
    | Some p -> p
    | None ->
        Printf.eprintf "Slang side load failed\n"; exit 1
  in
  Printf.printf "  %d slang modules in %.1fs\n"
    (List.length slang_prog.modules)
    (Unix.gettimeofday () -. t0);

  Printf.printf "[2/3] Verible → BIR ...\n%!";
  let t1 = Unix.gettimeofday () in
  let ver_prog = Verible_to_behavioral.convert_files ~top [flat_sv] in
  let ver_prog = ver_prog
    |> Behavioral_unroll.unroll_program
    |> Behavioral_inline.inline_program
    |> Behavioral_iflift.lift_program
    |> Behavioral_blocking_subst.blocking_subst_program
    |> Behavioral_meminfer.infer_program
    |> Behavioral_flatten.flatten_program
  in
  Printf.printf "  %d verible modules in %.1fs\n"
    (List.length ver_prog.modules)
    (Unix.gettimeofday () -. t1);

  (* Pair Slang and Verible bmodules. Both frontends specialise
   * parameterised modules into separate `<base>__<suffix>` entries
   * but with different naming conventions:
   *   Verible (popcount): `popcount__IW2` (skips localparams)
   *   Slang   (popcount): `popcount__IW2_P2` (also includes
   *                       PopcountWidth, a localparam Slang
   *                       elaborates as an isPort parameter)
   * Falling back to base-name + I/O-port-shape match catches these
   * — the port shapes disambiguate sibling specialisations
   * uniquely when their parameter values produce different I/O
   * widths, which is the common case. *)
  let base_of n =
    try
      let i = Str.search_forward (Str.regexp "__") n 0 in
      String.sub n 0 i
    with Not_found -> n
  in
  let port_shape (m : bmodule) =
    List.filter_map (fun (s : bsignal) ->
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
  let by_base = List.fold_left (fun acc (m : bmodule) ->
    let b = base_of m.name in
    let bucket = try List.assoc b acc with Not_found -> [] in
    (b, m :: bucket) :: List.remove_assoc b acc
  ) [] ver_prog.modules in
  let pair_for slang_m =
    (* exact name first, then base-name + port-shape match *)
    match List.find_opt (fun (m : bmodule) -> m.name = slang_m.name)
            ver_prog.modules with
    | Some m -> Some m
    | None ->
        let b = base_of slang_m.name in
        let s_shape = port_shape slang_m in
        let candidates = try List.assoc b by_base with Not_found -> [] in
        List.find_opt (fun m -> port_shape m = s_shape) candidates
  in

  Printf.printf "\n[3/3] Pairing + miter ...\n";
  let pass = ref 0 and fail = ref 0 and err = ref 0 and skip = ref 0 in
  let fail_names = ref [] in
  List.iter (fun (slang_m : bmodule) ->
    if not (matches slang_m.name) then ()
    else
      match pair_for slang_m with
      | None -> incr skip
      | Some ver_m ->
          Printf.printf "  %-60s ... %!" slang_m.name;
          (try
            if Z3_miter.check_miter_equivalence slang_m ver_m then begin
              incr pass; Printf.printf "✅\n"
            end else begin
              incr fail; fail_names := slang_m.name :: !fail_names;
              Printf.printf "❌\n"
            end
          with e ->
            incr err;
            Printf.printf "⚠ %s\n" (Printexc.to_string e))
  ) slang_prog.modules;

  Printf.printf "\n═══════════════════════════════════════════════════════\n";
  Printf.printf "  Summary: %d slang modules, %d verible modules\n"
    (List.length slang_prog.modules) (List.length ver_prog.modules);
  Printf.printf "  miter:   %d ✅  %d ❌  %d ⚠  (%d unpaired)\n"
    !pass !fail !err !skip;
  Printf.printf "═══════════════════════════════════════════════════════\n";
  if !fail > 0 then begin
    Printf.printf "\nfailing entities (first 20):\n";
    List.iteri (fun i n ->
      if i < 20 then Printf.printf "  %s\n" n
    ) (List.rev !fail_names)
  end
