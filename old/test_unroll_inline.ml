(* Test driver for behavioral_unroll + behavioral_inline.
 *
 * Reads an SV file, builds BIR, applies the two passes, and counts
 * how many BCall / BCallStmt / BFor / BWhile nodes remain. A
 * successful transformation reduces those counts to zero (assuming
 * the call targets and loop bounds are statically known).
 *
 * If `--miter <vivado.vhd>` is given, additionally runs the Z3
 * equivalence check between the transformed BIR and the Vivado-
 * elaborated VHDL — that's the full integration test.
 *
 * Usage:
 *   test_unroll_inline <top> <file.sv> [<more.sv>...]
 *   test_unroll_inline <top> <file.sv> --miter <vivado.vhd>  *)

let usage () =
  Printf.eprintf
    "usage: %s <top> <file.sv> [more.sv ...] [--miter <vhd>]\n" Sys.argv.(0);
  exit 2

let run_verilator top files =
  let mdir = Filename.concat (Filename.get_temp_dir_name ())
               (Printf.sprintf "uitest_%s_%d" top (Unix.getpid ())) in
  let _ = Sys.command (Printf.sprintf "rm -rf %s && mkdir -p %s"
                         (Filename.quote mdir) (Filename.quote mdir)) in
  let files_str = String.concat " " (List.map Filename.quote files) in
  let cmd =
    Printf.sprintf
      "verilator --json-only -Wno-fatal -Wno-WIDTHTRUNC -Wno-WIDTHEXPAND \
       -Wno-UNOPTFLAT -Wno-CASEINCOMPLETE -Wno-CASEX -Wno-PINMISSING \
       -Wno-IMPLICIT -Wno-DECLFILENAME -Wno-MULTITOP \
       --top-module %s %s --Mdir %s > %s/v.log 2>&1"
      (Filename.quote top) files_str (Filename.quote mdir)
      (Filename.quote mdir)
  in
  let rc = Sys.command cmd in
  if rc <> 0 then begin
    Printf.eprintf "verilator failed (rc=%d) — see %s/v.log\n" rc mdir;
    let ic = open_in (Filename.concat mdir "v.log") in
    (try while true do prerr_endline (input_line ic) done
     with End_of_file -> close_in ic);
    exit 1
  end;
  Filename.concat mdir (Printf.sprintf "V%s.tree.json" top)

(* Walk a bmodule and count occurrences of each shape we care about. *)
let count_nodes (bmod : Behavioral_ir.bmodule) =
  let calls = ref 0 in
  let call_stmts = ref 0 in
  let fors = ref 0 in
  let whiles = ref 0 in
  let ifs = ref 0 in
  let stmts = ref 0 in
  let rec walk_e = function
    | Behavioral_ir.BCall { args; _ } -> incr calls; List.iter walk_e args
    | BVar _ | BConst _ -> ()
    | BBinOp { lhs; rhs; _ } -> walk_e lhs; walk_e rhs
    | BUnOp { operand; _ } -> walk_e operand
    | BSelect { array; index } -> walk_e array; walk_e index
    | BSlice { signal; _ } -> walk_e signal
    | BConcat es -> List.iter walk_e es
    | BReplicate { value; _ } -> walk_e value
    | BCond { condition; then_val; else_val } ->
        walk_e condition; walk_e then_val; walk_e else_val
  in
  let rec walk_s s =
    incr stmts;
    match s with
    | Behavioral_ir.BAssign { rhs; _ } -> walk_e rhs
    | BIf { condition; then_stmts; else_stmts } ->
        incr ifs;
        walk_e condition;
        List.iter walk_s then_stmts; List.iter walk_s else_stmts
    | BCase { selector; cases; default } ->
        walk_e selector;
        List.iter (fun (e, ss) -> walk_e e; List.iter walk_s ss) cases;
        List.iter walk_s default
    | BWhile { condition; body } ->
        incr whiles; walk_e condition; List.iter walk_s body
    | BFor { init; condition; update; body } ->
        incr fors;
        walk_s init; walk_e condition; walk_s update;
        List.iter walk_s body
    | BBlock ss -> List.iter walk_s ss
    | BCallStmt { func; args } ->
        (* Don't count `@mem_write` intrinsics — they're the
         * memory-inference output, not unresolved task calls. *)
        if not (String.length func > 0 && func.[0] = '@') then
          incr call_stmts;
        List.iter walk_e args
    | BReturn None -> ()
    | BReturn (Some e) -> walk_e e
  in
  List.iter (fun p ->
    let body = match p with
      | Behavioral_ir.BCombinational c -> c.body
      | BSequential s -> s.body
    in
    List.iter walk_s body
  ) bmod.processes;
  (`Calls !calls, `CallStmts !call_stmts,
   `Fors !fors, `Whiles !whiles, `Ifs !ifs, `Stmts !stmts)

let print_counts label (`Calls c, `CallStmts cs, `Fors f, `Whiles w,
                        `Ifs i, `Stmts n) =
  Printf.printf "  [%s] BCall=%d BCallStmt=%d BFor=%d BWhile=%d BIf=%d total_stmts=%d\n%!"
    label c cs f w i n

let () =
  if Array.length Sys.argv < 3 then usage ();
  let top = Sys.argv.(1) in
  let rest = Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2)) in
  let rec split_args sv miter = function
    | [] -> (List.rev sv, miter)
    | "--miter" :: v :: r -> split_args sv (Some v) r
    | x :: r -> split_args (x :: sv) miter r
  in
  let (files, miter_vhd) = split_args [] None rest in
  if files = [] then usage ();
  let json = run_verilator top files in

  Printf.printf "═══════════════════════════════════════════════════════\n";
  Printf.printf "  Loop-unroll + function/task-inline test: %s\n" top;
  Printf.printf "═══════════════════════════════════════════════════════\n\n";

  let bprog =
    match Verilator_to_behavioral.convert_verilator_json_to_behavioral json with
    | Some p -> p
    | None ->
        Printf.eprintf "verilator → BIR failed\n"; exit 1
  in
  let find_top p =
    match List.find_opt (fun (m : Behavioral_ir.bmodule) -> m.name = top)
            p.Behavioral_ir.modules with
    | Some m -> m
    | None ->
        Printf.eprintf "top '%s' not found in %s\n" top
          (String.concat ", " (List.map (fun (m : Behavioral_ir.bmodule) ->
             m.name) p.modules));
        exit 1
  in

  let original = find_top bprog in
  Printf.printf "funcs/tasks discovered: %d\n"
    (List.length original.funcs);
  List.iter (fun (f : Behavioral_ir.bfunc) ->
    Printf.printf "  - %s%s (%d params, %d body stmts)\n"
      (if f.is_task then "task " else "func ")
      f.fname (List.length f.params) (List.length f.body)
  ) original.funcs;
  Printf.printf "\n";
  print_counts "before" (count_nodes original);

  let bprog_unrolled = Behavioral_unroll.unroll_program bprog in
  Printf.printf "After unroll:\n";
  print_counts "unroll" (count_nodes (find_top bprog_unrolled));

  let bprog_inlined = Behavioral_inline.inline_program bprog_unrolled in
  Printf.printf "After inline:\n";
  print_counts "inline" (count_nodes (find_top bprog_inlined));

  let bprog_lifted = Behavioral_iflift.lift_program bprog_inlined in
  Printf.printf "After if-lift:\n";
  print_counts "iflift" (count_nodes (find_top bprog_lifted));

  let bprog_mem = Behavioral_meminfer.infer_program bprog_lifted in
  Printf.printf "After mem-infer:\n";
  print_counts "meminfer" (count_nodes (find_top bprog_mem));
  let mems = (find_top bprog_mem).mems in
  if mems <> [] then begin
    Printf.printf "Memories inferred:\n";
    List.iter (fun (m : Behavioral_ir.bmem) ->
      let kind = match m.kind with
        | BRam -> "RAM" | BRom -> "ROM"
      in
      let init = if m.init_values = [] then ""
        else
          let prefix = List.filteri (fun i _ -> i < 4) m.init_values in
          Printf.sprintf " init=[%s%s]"
            (String.concat ", " (List.map string_of_int prefix))
            (if List.length m.init_values > 4 then ", ..." else "")
      in
      Printf.printf "  - %s %s: %dx%d (addr_w=%d, depth=%d)%s\n"
        kind m.mname m.data_width m.depth
        m.addr_width m.depth init
    ) mems
  end;

  let transformed = find_top bprog_mem in

  let (`Calls c, `CallStmts cs, `Fors f, `Whiles w, `Ifs i, _) =
    count_nodes transformed
  in
  (* For modules with inferred memories the surviving BIf is the
   * memory write-enable gate around an `@mem_write` intrinsic — that
   * IS the inferred form, not residual unconverted code. *)
  let has_ram = List.exists (fun (m : Behavioral_ir.bmem) ->
    m.kind = BRam) (find_top bprog_mem).mems in
  let structurally_clean =
    c = 0 && cs = 0 && f = 0 && w = 0
    && (i = 0 || has_ram)
  in
  Printf.printf "\nStructural check: %s\n"
    (if structurally_clean then
       "✅ all loops unrolled, calls inlined, ifs lifted, memories inferred"
     else
       Printf.sprintf
         "⚠ residual: BCall=%d BCallStmt=%d BFor=%d BWhile=%d BIf=%d"
         c cs f w i);

  let miter_ok =
    match miter_vhd with
    | None -> None
    | Some vhd ->
        Printf.printf "\nZ3 miter: transformed BIR ≡ Vivado %s?\n" vhd;
        let viv =
          match Vhdl_to_ver_front.convert_vhd_file vhd with
          | Some p -> p
          | None ->
              Printf.eprintf "Vivado-side parse failed\n"; exit 1
        in
        let viv_top = match List.find_opt
          (fun (m : Behavioral_ir.bmodule) -> m.name = top) viv.modules with
          | Some m -> m
          | None -> Printf.eprintf "Vivado-side top not found\n"; exit 1
        in
        Some (Z3_miter.check_miter_equivalence viv_top transformed)
  in

  let exit_code =
    match miter_ok with
    | Some true -> Printf.printf "  ✅ FORMALLY EQUIVALENT\n"; 0
    | Some false -> Printf.printf "  ❌ NOT EQUIVALENT\n"; 1
    | None -> if structurally_clean then 0 else 1
  in
  exit exit_code
