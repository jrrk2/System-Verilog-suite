(* test_floorplan — pre-flight RAM/usage analyser for FPGA builds (task #142).
 *
 * Vivado hangs in RAM inference on the TALOS-V2 VC707 build without
 * diagnostic output.  This tool walks the same SV sources Vivado would
 * consume, runs Verible→BIR + behavioral_meminfer over each file
 * individually, and reports per-module:
 *   - inferred memories (depth × width, ports, sync/async)
 *   - Xilinx 36Kb BRAM tile estimate
 *   - LUTRAM / DSP / FF estimates
 *   - blowup flags for shapes that typically hang Vivado
 *
 * Per-file parsing keeps us productive when one file has a syntactic
 * construct our parser doesn't handle (e.g. `$clog2(X)'(Y)` size cast
 * in TALOS-V2 rope_selftest) — that file is skipped with a note, the
 * rest get analysed.
 *
 * Usage:
 *   test_floorplan <file.sv> [<file.sv>...]
 * Output: TSV on stdout + sorted blowup summary on stderr.
 *)

open Behavioral_ir

let cdiv a b = (a + b - 1) / b

(* Memory-resource categorisation matching Xilinx 7-series inference
 * rules (xc7vx485t has 1030 RAMB36E1 + 2060 RAMB18E1).
 *
 *   - LUTRAM (distributed): depth ≤ 256, async read, no sync output reg.
 *     Cost = ceil(depth/32) × width LUTs (SLICEM uses 32x1 base slice;
 *     wider shapes cascade in parallel for the data dimension).
 *   - RAMB18E1: total bits ≤ 18432, width ≤ 36.  1 tile.
 *   - RAMB36E1: total bits ≤ 36864, width ≤ 72.  1 tile.
 *   - Cascade RAMB36: bigger shapes → ceil(W/72) × ceil(D/512) tiles.
 *   - Replication: ports > 2 multiplies tile count by the read-port
 *     count (Xilinx BRAMs are at most true-dual-port).
 *
 * Returns a tile_kind summarising what Vivado will most likely pick. *)
type tile_kind =
  | T_zero
  | T_lutram of int        (* LUT count *)
  | T_ramb18 of int        (* RAMB18E1 tile count *)
  | T_ramb36 of int        (* RAMB36E1 tile count *)

let pick_tile (mm : bmem) : tile_kind =
  let bits = mm.depth * mm.data_width in
  if bits = 0 then T_zero
  else
    let ports = max 1 (mm.n_read_ports + mm.n_write_ports) in
    let replication = max 1 (cdiv ports 2) in
    if mm.depth <= 256 && not mm.read_is_sync then
      let luts = cdiv mm.depth 32 * mm.data_width * replication in
      T_lutram luts
    else if bits <= 18432 && mm.data_width <= 36 then
      T_ramb18 (1 * replication)
    else if bits <= 36864 && mm.data_width <= 72 then
      T_ramb36 (1 * replication)
    else
      let tiles_w = cdiv mm.data_width 72 in
      let tiles_d = cdiv mm.depth 512 in
      T_ramb36 (tiles_w * tiles_d * replication)

let rec walk_bexpr_muls (e : bexpr) acc =
  match e with
  | BBinOp { op = BMul; lhs; rhs; result_type } ->
      let w a = match a with
        | BConst { width; _ } -> width
        | _ ->
            (match result_type with
             | BInt { width; _ } -> width
             | _ -> 32) in
      let acc' = (w lhs, w rhs) :: acc in
      walk_bexpr_muls rhs (walk_bexpr_muls lhs acc')
  | BBinOp { lhs; rhs; _ } ->
      walk_bexpr_muls rhs (walk_bexpr_muls lhs acc)
  | BUnOp { operand; _ } -> walk_bexpr_muls operand acc
  | BSelect { array; index } ->
      walk_bexpr_muls index (walk_bexpr_muls array acc)
  | BSlice { signal; _ } -> walk_bexpr_muls signal acc
  | BConcat xs -> List.fold_left (fun a e -> walk_bexpr_muls e a) acc xs
  | BReplicate { value; _ } -> walk_bexpr_muls value acc
  | BCond { condition; then_val; else_val } ->
      walk_bexpr_muls else_val
        (walk_bexpr_muls then_val (walk_bexpr_muls condition acc))
  | BCall { args; _ } -> List.fold_left (fun a e -> walk_bexpr_muls e a) acc args
  | _ -> acc

let rec walk_stmt_muls (s : bstmt) acc =
  match s with
  | BAssign { rhs; _ } -> walk_bexpr_muls rhs acc
  | BIf { condition; then_stmts; else_stmts } ->
      let a = walk_bexpr_muls condition acc in
      let a = List.fold_left (fun a s -> walk_stmt_muls s a) a then_stmts in
      List.fold_left (fun a s -> walk_stmt_muls s a) a else_stmts
  | BCase { selector; cases; default } ->
      let a = walk_bexpr_muls selector acc in
      let a = List.fold_left (fun a (e, ss) ->
        let a = walk_bexpr_muls e a in
        List.fold_left (fun a s -> walk_stmt_muls s a) a ss) a cases in
      List.fold_left (fun a s -> walk_stmt_muls s a) a default
  | BBlock ss -> List.fold_left (fun a s -> walk_stmt_muls s a) acc ss
  | BWhile { condition; body } ->
      let a = walk_bexpr_muls condition acc in
      List.fold_left (fun a s -> walk_stmt_muls s a) a body
  | BFor { init; condition; update; body } ->
      let a = walk_stmt_muls init acc in
      let a = walk_bexpr_muls condition a in
      let a = walk_stmt_muls update a in
      List.fold_left (fun a s -> walk_stmt_muls s a) a body
  | BCallStmt { args; _ } ->
      List.fold_left (fun a e -> walk_bexpr_muls e a) acc args
  | BReturn (Some e) -> walk_bexpr_muls e acc
  | BReturn None -> acc

let count_muls (m : bmodule) =
  let muls = ref [] in
  List.iter (function
    | BCombinational { body; _ } ->
        List.iter (fun s -> muls := walk_stmt_muls s !muls) body
    | BSequential { body; _ } ->
        List.iter (fun s -> muls := walk_stmt_muls s !muls) body
  ) m.processes;
  !muls

let estimate_dsps muls =
  List.fold_left (fun acc (lw, rw) ->
    if max lw rw <= 18 then acc + 1
    else
      let tiles = cdiv lw 17 * cdiv rw 17 in
      acc + tiles) 0 muls

let count_ffs (m : bmodule) =
  let n = ref 0 in
  List.iter (function
    | BSequential { body; _ } ->
        let rec go = function
          | BAssign { lhs; _ } ->
              let w =
                match List.find_opt (fun (s : bsignal) -> s.name = lhs)
                        m.signals with
                | Some s ->
                    (match s.stype with
                     | BInt { width; _ } -> width
                     | _ -> 1)
                | None -> 1
              in
              n := !n + w
          | BIf { then_stmts; else_stmts; _ } ->
              List.iter go then_stmts; List.iter go else_stmts
          | BCase { cases; default; _ } ->
              List.iter (fun (_, ss) -> List.iter go ss) cases;
              List.iter go default
          | BBlock ss -> List.iter go ss
          | _ -> ()
        in
        List.iter go body
    | _ -> ()
  ) m.processes;
  !n

type module_report = {
  m_name : string;
  source : string;
  mems : bmem list;
  ramb18 : int;
  ramb36 : int;
  lutram_luts : int;
  dsps : int;
  ffs : int;
  blowups : string list;
}

let analyse ~source (m : bmodule) : module_report =
  let muls = count_muls m in
  let dsps = estimate_dsps muls in
  let ffs = count_ffs m in
  let ramb18, ramb36, lutram_luts =
    List.fold_left (fun (b18, b36, lut) mm ->
      match pick_tile mm with
      | T_zero -> (b18, b36, lut)
      | T_lutram n -> (b18, b36, lut + n)
      | T_ramb18 n -> (b18 + n, b36, lut)
      | T_ramb36 n -> (b18, b36 + n, lut)
    ) (0, 0, 0) m.mems in
  let blowups = ref [] in
  let add msg = blowups := msg :: !blowups in
  List.iter (fun (mm : bmem) ->
    let bits = mm.depth * mm.data_width in
    (match pick_tile mm with
     | T_ramb36 n when n > 1024 ->
         add (Printf.sprintf
           "mem %s: %dx%d (%d bits) → ~%d RAMB36 tiles — likely hangs Vivado"
           mm.mname mm.depth mm.data_width bits n)
     | _ -> ());
    if mm.n_write_ports > 16 then
      add (Printf.sprintf
        "mem %s: %d write ports — bit-blasts to LUT mesh"
        mm.mname mm.n_write_ports);
    if mm.depth > 1_048_576 then
      add (Printf.sprintf
        "mem %s: depth %d > 1M — DDR territory" mm.mname mm.depth)
  ) m.mems;
  let array_bits =
    List.fold_left (fun acc (s : bsignal) ->
      match s.stype with
      | BArray { element = BInt { width; _ }; size } -> acc + size * width
      | _ -> acc) 0 m.signals in
  if array_bits > 100_000 then
    add (Printf.sprintf
      "unpacked-array bits = %d — bit-blast risk at flatten" array_bits);
  if dsps > 4096 then
    add (Printf.sprintf
      "mul count → %d DSPs — exceeds VC707 capacity (2800)" dsps);
  { m_name = m.name; source; mems = m.mems;
    ramb18; ramb36; lutram_luts; dsps; ffs;
    blowups = List.rev !blowups }

let analyse_file ~top path : module_report list * bool =
  Printf.eprintf "[floorplan] %s ... %!" path;
  try
    let p = Verible_to_behavioral.convert_files ~top [path] in
    let p = Behavioral_meminfer.infer_program p in
    let rs = List.map (analyse ~source:path) p.modules in
    Printf.eprintf "%d modules\n" (List.length rs);
    (rs, true)
  with e ->
    Printf.eprintf "FAILED (%s)\n" (Printexc.to_string e);
    ([], false)

let () =
  if Array.length Sys.argv < 3 then begin
    Printf.eprintf "Usage: %s <top> <file.sv> [<file.sv>...]\n" Sys.argv.(0);
    exit 2
  end;
  let top = Sys.argv.(1) in
  let files =
    Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2)) in
  let t0 = Unix.gettimeofday () in
  let reports, ok_files = List.fold_left (fun (rs, ok) f ->
    let (r, ok_one) = analyse_file ~top f in
    (r @ rs, ok + (if ok_one then 1 else 0))
  ) ([], 0) files in
  Printf.eprintf "\n[floorplan] %d/%d files parsed in %.1fs\n"
    ok_files (List.length files) (Unix.gettimeofday () -. t0);

  Printf.printf "module\tsource\tmems\tramb18\tramb36\tlutram_luts\tdsps\tffs\tblowups\n";
  List.iter (fun r ->
    Printf.printf "%s\t%s\t%d\t%d\t%d\t%d\t%d\t%d\t%d\n"
      r.m_name r.source (List.length r.mems)
      r.ramb18 r.ramb36 r.lutram_luts r.dsps r.ffs
      (List.length r.blowups)
  ) reports;

  (* Per-memory detail for any module with at least one memory.  Lets
   * us see exact (depth × width × ports × tile-kind) for sizing
   * decisions and Vivado-inference debugging. *)
  Printf.eprintf "\n────── PER-MEMORY DETAIL ──────\n";
  let mems_modules = List.filter (fun r -> r.mems <> []) reports in
  List.iter (fun r ->
    Printf.eprintf "  %s\n" r.m_name;
    List.iter (fun (mm : bmem) ->
      let kind_str = match pick_tile mm with
        | T_zero -> "zero"
        | T_lutram n -> Printf.sprintf "LUTRAM × %d LUT" n
        | T_ramb18 n -> Printf.sprintf "RAMB18 × %d" n
        | T_ramb36 n -> Printf.sprintf "RAMB36 × %d" n
      in
      Printf.eprintf "    %-32s %5dx%-4d  %dW %dR %s  → %s\n"
        mm.mname mm.depth mm.data_width
        mm.n_write_ports mm.n_read_ports
        (if mm.read_is_sync then "sync " else "async")
        kind_str
    ) r.mems
  ) mems_modules;

  Printf.eprintf "\n────── BLOWUP SUMMARY ──────\n";
  let with_blowups = List.filter (fun r -> r.blowups <> []) reports in
  let sorted =
    List.sort (fun a b ->
      compare (b.ramb18 + 2*b.ramb36) (a.ramb18 + 2*a.ramb36)
    ) with_blowups in
  if sorted = [] then Printf.eprintf "  (none — clean RTL)\n"
  else List.iter (fun r ->
    Printf.eprintf "  %s [B18=%d B36=%d LUTRAM=%d DSP=%d FF=%d]\n"
      r.m_name r.ramb18 r.ramb36 r.lutram_luts r.dsps r.ffs;
    List.iter (fun msg -> Printf.eprintf "    ⚠ %s\n" msg) r.blowups
  ) sorted;

  let total field = List.fold_left (fun a r -> a + field r) 0 reports in
  let t_b18 = total (fun r -> r.ramb18) in
  let t_b36 = total (fun r -> r.ramb36) in
  let t_lut = total (fun r -> r.lutram_luts) in
  Printf.eprintf "\n────── TOTALS  (VC707 xc7vx485t budget) ──────\n";
  Printf.eprintf "  modules:        %d\n" (List.length reports);
  Printf.eprintf "  memories:       %d\n" (total (fun r -> List.length r.mems));
  Printf.eprintf "  RAMB18E1:       %d  (budget = 2060,  %.1f%% used)\n"
    t_b18 (100.0 *. float_of_int t_b18 /. 2060.0);
  Printf.eprintf "  RAMB36E1:       %d  (budget = 1030,  %.1f%% used)\n"
    t_b36 (100.0 *. float_of_int t_b36 /. 1030.0);
  Printf.eprintf "  Combined-as-36: %d  (B18 counts as 0.5 of B36)\n"
    (t_b18 / 2 + t_b36);
  Printf.eprintf "  LUTRAM LUTs:    %d  (budget ~32400 SLICEM LUTs)\n" t_lut;
  Printf.eprintf "  DSPs:           %d  (budget = 2800,  %.1f%% used)\n"
    (total (fun r -> r.dsps))
    (100.0 *. float_of_int (total (fun r -> r.dsps)) /. 2800.0);
  Printf.eprintf "  FFs:            %d  (budget ~607200,  %.1f%% used)\n"
    (total (fun r -> r.ffs))
    (100.0 *. float_of_int (total (fun r -> r.ffs)) /. 607200.0)
