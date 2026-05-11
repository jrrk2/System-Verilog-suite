(* OCaml-driven ORFS staging for our decompiler synth output.

   Replaces the bash-wrapper-around-make pattern: the same OCaml
   pipeline that knows what macros it generated also writes the
   per-stamp [config.mk], the SDC, and the netlist directly into
   ORFS's expected layout.  The user then runs `make` against the
   prepared config — no scripting layer in between.

   What it does:
     1. Pick a date-stamped FLOW_VARIANT (or honour --variant).
     2. Run the full SV → cell-mapped Verilog pipeline (calls
        [Synth_orfs_shim.run]).
     3. Stage the netlist into <orfs_flow>/results/<plat>/<design>/<stamp>/1_2_yosys.v
     4. Stage the SDC at the same path with .sdc.
     5. Write a self-contained <design_cfg_dir>/<stamp>/config.mk:
          - DESIGN_NICKNAME, DESIGN_NAME, PLATFORM
          - VERILOG_FILES = the user's .sv files + every macro wrapper .v
          - SDC_FILE      = the user's existing .sdc (or one we write)
          - ADDITIONAL_LEFS / ADDITIONAL_LIBS for each macro
          - FLOW_VARIANT  = the chosen stamp
     6. Print the make command.

   Why this layout:
     - Each invocation gets its own <stamp>/ — runs don't clobber.
     - The macro paths come from [Mem_macro_resolve.artifacts] which
       the same pipeline already produced; no second discovery step.
     - The user's design files stay where they are; we don't copy
       them, we just reference them from config.mk.

   ORFS's existing $(USE_DECOMP_SYNTH) hook is bypassed entirely:
   1_2_yosys.v already exists when make starts, so the synth rule is
   up-to-date and doesn't re-fire.  ORFS proceeds straight to
   floorplan.

   Usage:
     orfs_prep.exe \
       --top picosoc \
       --orfs-flow /home/jonathan/OpenROAD-flow-scripts/flow \
       --platform nangate45 \
       --design-cfg-dir /home/jonathan/sv_decompiler_orfs/picosoc \
       --sdc /home/jonathan/sv_decompiler_orfs/picosoc/constraint.sdc \
       [--variant 20260510_080000] \
       -- file1.sv file2.sv ...
*)

let usage () =
  prerr_endline
"usage: orfs_prep.exe \\
    --top <top> \\
    --orfs-flow <flow-root> \\
    --platform <platform-name> \\
    --design-cfg-dir <dir> \\
    --sdc <constraint.sdc> \\
    [--variant <stamp>] \\
    [--design-nickname <name>] \\
    -- <file1.sv> [<file2.sv> ...]";
  exit 1

let parse_args () =
  let top = ref "" in
  let orfs_flow = ref "" in
  let platform = ref "" in
  let design_cfg_dir = ref "" in
  let sdc = ref "" in
  let variant = ref None in
  let nickname = ref None in
  let files = ref [] in
  let in_files = ref false in
  let i = ref 1 in
  while !i < Array.length Sys.argv do
    let a = Sys.argv.(!i) in
    if !in_files then begin
      files := a :: !files;
      incr i
    end else
      let next () =
        if !i + 1 >= Array.length Sys.argv then usage ();
        let v = Sys.argv.(!i + 1) in
        i := !i + 2;
        v
      in
      match a with
      | "--top"             -> top := next ()
      | "--orfs-flow"       -> orfs_flow := next ()
      | "--platform"        -> platform := next ()
      | "--design-cfg-dir"  -> design_cfg_dir := next ()
      | "--sdc"             -> sdc := next ()
      | "--variant"         -> variant := Some (next ())
      | "--design-nickname" -> nickname := Some (next ())
      | "--"                -> in_files := true; incr i
      | s when String.length s > 0 && s.[0] <> '-' ->
          (* Bare positional — treat as file. *)
          in_files := true;
          files := s :: !files;
          incr i
      | s ->
          Printf.eprintf "unknown flag: %s\n" s;
          usage ()
  done;
  if !top = "" || !orfs_flow = "" || !platform = ""
     || !design_cfg_dir = "" || !sdc = "" || !files = []
  then usage ();
  (!top, !orfs_flow, !platform, !design_cfg_dir, !sdc,
   !variant, !nickname, List.rev !files)

let mkdir_p path =
  let rec go p =
    if Sys.file_exists p then ()
    else begin
      let parent = Filename.dirname p in
      if parent <> p then go parent;
      try Unix.mkdir p 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    end
  in go path

let copy_file src dst =
  let ic = open_in src in
  let oc = open_out dst in
  let buf = Bytes.create 8192 in
  let rec loop () =
    let n = input ic buf 0 8192 in
    if n > 0 then (output oc buf 0 n; loop ())
  in
  loop ();
  close_in ic;
  close_out oc

let stamp_now () =
  let t = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d%02d%02d_%02d%02d%02d"
    (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday
    t.tm_hour t.tm_min t.tm_sec

let write_text path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

(* ── ORFS config.mk emission ───────────────────────────────────── *)

(* Exports for env vars that affect the shim's behaviour.  They must
   propagate from orfs_prep's invocation through make → shim re-run.
   Only export the ones we know about (others stay user-controlled). *)
let preserved_envs = [
  "MEM_USE_FAKERAM";
  "FAKERAM_PLATFORM_DIR";
  "MEM_MACRO_TECH";
  "MEMLOWER";
  "MEM_REQUIRE_DUAL_PORT";
  "SV_DECOMP_LIBERTY";
  "SV_DECOMP_NO_SIZE";
  "SV_DECOMP_WIRE_CAP_PF";
  "SV_DECOMP_ARCH_SWAP";
  "SV_DECOMP_ARCH_MIN_W";
  "SV_DECOMP_NO_TIE_FAN";
  "SV_DECOMP_TIE_FANOUT_MAX";
  "SV_DECOMP_NO_DCE";
  "SV_DECOMP_MUX_FLATTEN";
  "SV_DECOMP_MUX_CHAIN_MIN";
  "SV_DECOMP_NO_KARY_MERGE";
  "SV_DECOMP_KARY_MAX_PASSES";
  "SV_DECOMP_KARY_DEBUG";
  "SV_DECOMP_TIMING_REF";
]

let emit_config_mk ~top ~nickname ~platform ~variant
                   ~user_files ~sdc_path
                   ~mem_arts =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf
    (Printf.sprintf
       "# Auto-generated by orfs_prep — do not edit, regenerate.\n\
        # Variant %s\n\n" variant);
  Buffer.add_string buf (Printf.sprintf "export DESIGN_NICKNAME = %s\n" nickname);
  Buffer.add_string buf (Printf.sprintf "export DESIGN_NAME     = %s\n" top);
  Buffer.add_string buf (Printf.sprintf "export PLATFORM        = %s\n" platform);
  Buffer.add_string buf (Printf.sprintf "export FLOW_VARIANT    = %s\n" variant);
  (* Re-routes the do-yosys rule to our shim instead of yosys+ABC.
     The staged 1_2_yosys.v we wrote alongside this config will be
     overwritten by an identical (deterministic) shim re-run when
     synth fires — but with USE_DECOMP_SYNTH=1 the path is correct.
     Without this, ORFS invokes yosys, which can't compile picorv32
     and silently differs from our pipeline.                         *)
  Buffer.add_string buf "export USE_DECOMP_SYNTH := 1\n";
  List.iter (fun k ->
    match Sys.getenv_opt k with
    | Some v -> Buffer.add_string buf (Printf.sprintf "export %s := %s\n" k v)
    | None -> ()
  ) preserved_envs;
  Buffer.add_string buf "\n";

  (* Source Verilog: user files + every macro wrapper.  ORFS's
     synth step would normally compile these, but since we already
     produced 1_2_yosys.v in the variant's results dir with newer
     mtime, the synth rule sees its output as up-to-date and skips
     the actual run.  The VERILOG_FILES list still has to be
     accurate for downstream lint/sta checks that re-read source. *)
  let macro_vs =
    List.sort_uniq compare
      (List.map (fun a -> a.Mem_macro_resolve.verilog_path) mem_arts) in
  let all_v = user_files @ macro_vs in
  Buffer.add_string buf "export VERILOG_FILES = \\\n";
  List.iteri (fun i f ->
    Buffer.add_string buf
      (Printf.sprintf "    %s%s\n" f
         (if i = List.length all_v - 1 then "" else " \\"))
  ) all_v;
  Buffer.add_string buf "\n";

  Buffer.add_string buf (Printf.sprintf "export SDC_FILE        = %s\n\n" sdc_path);

  if mem_arts <> [] then begin
    let lefs =
      List.sort_uniq compare
        (List.filter_map (fun a ->
           match a.Mem_macro_resolve.lef_path with
           | Some p when p <> "" -> Some p
           | _ -> None) mem_arts) in
    let libs =
      List.sort_uniq compare
        (List.map (fun a -> a.Mem_macro_resolve.liberty_path) mem_arts) in
    if lefs <> [] then begin
      Buffer.add_string buf "export ADDITIONAL_LEFS = \\\n";
      List.iteri (fun i f ->
        Buffer.add_string buf
          (Printf.sprintf "    %s%s\n" f
             (if i = List.length lefs - 1 then "" else " \\"))
      ) lefs;
      Buffer.add_string buf "\n"
    end;
    if libs <> [] then begin
      Buffer.add_string buf "export ADDITIONAL_LIBS = \\\n";
      List.iteri (fun i f ->
        Buffer.add_string buf
          (Printf.sprintf "    %s%s\n" f
             (if i = List.length libs - 1 then "" else " \\"))
      ) libs;
      Buffer.add_string buf "\n"
    end
  end;

  (* Sensible nangate45 defaults — the user can override by editing
     a sibling config.mk and `include`ing this one, but for a first-
     run zero-config experience these match what picosoc was running
     with manually. *)
  Buffer.add_string buf
"export CORE_UTILIZATION       = 30
export CORE_ASPECT_RATIO      = 1
export CORE_MARGIN            = 2
export PLACE_DENSITY_LB_ADDON = 0.20
export TNS_END_PERCENT        = 100
export SYNTH_MEMORY_MAX_BITS  = 8192
";
  Buffer.contents buf

(* ── 1_2_yosys.sdc emitter ─────────────────────────────────────── *)

(* If the user's SDC file exists, copy it to the variant's
   1_2_yosys.sdc.  ORFS expects 1_2_yosys.sdc to land alongside
   1_2_yosys.v after the synth step. *)
let stage_sdc ~user_sdc ~staged_sdc =
  if Sys.file_exists user_sdc then copy_file user_sdc staged_sdc
  else
    write_text staged_sdc
      "# orfs_prep: no user SDC supplied — using a 10ns clock placeholder.\n\
       create_clock -name clk -period 10.000 [get_ports clk]\n"

let () =
  let (top, orfs_flow, platform, design_cfg_dir, user_sdc,
       variant_opt, nickname_opt, user_files) = parse_args () in
  let variant = match variant_opt with Some v -> v | None -> stamp_now () in
  let nickname = match nickname_opt with Some n -> n | None -> top in

  Printf.eprintf "[orfs_prep] variant=%s, top=%s, %d input files\n"
    variant top (List.length user_files);

  (* ORFS path layout — keep in sync with flow/Makefile.            *)
  let results_dir =
    Filename.concat orfs_flow
      (Printf.sprintf "results/%s/%s/%s" platform nickname variant) in
  let logs_dir =
    Filename.concat orfs_flow
      (Printf.sprintf "logs/%s/%s/%s" platform nickname variant) in
  let cfg_dir = Filename.concat design_cfg_dir variant in
  mkdir_p results_dir;
  mkdir_p logs_dir;
  mkdir_p cfg_dir;

  let staged_v   = Filename.concat results_dir "1_2_yosys.v" in
  let staged_sdc = Filename.concat results_dir "1_2_yosys.sdc" in
  let staged_canon = Filename.concat results_dir "1_1_yosys_canonicalize.rtlil" in
  let cfg_path   = Filename.concat cfg_dir "config.mk" in

  (* Pipeline.                                                       *)
  let _, mem_arts = Synth_pipeline.run ~top ~out_path:staged_v ~files:user_files () in

  (* The ORFS make rule chain is:
       1_synth.odb ← 1_2_yosys.v ← 1_1_yosys_canonicalize.rtlil ← VERILOG_FILES
     If we provide 1_2_yosys.v alone, make will still rebuild because
     the rtlil prerequisite is missing.  Touch a stub rtlil with the
     same mtime as 1_2_yosys.v (older than 1_2_yosys.v would also
     trigger rebuild via -W rules, so keep them equal/older).      *)
  write_text staged_canon
    "# orfs_prep stub — synth was run by orfs_prep, not yosys.\n";
  (* Order: rtlil should appear OLDER than 1_2_yosys.v so make sees
     the chain as satisfied.  We just wrote rtlil after the .v, so
     bump the .v forward to "now" again to invert order.           *)
  Unix.utimes staged_v 0.0 0.0;     (* set then immediately reset to current: *)
  let now = Unix.time () in
  Unix.utimes staged_v now now;

  stage_sdc ~user_sdc ~staged_sdc;
  Unix.utimes staged_sdc now now;

  let cfg = emit_config_mk
    ~top ~nickname ~platform ~variant
    ~user_files ~sdc_path:user_sdc
    ~mem_arts in
  write_text cfg_path cfg;

  Printf.eprintf "[orfs_prep] wrote %s\n" staged_v;
  Printf.eprintf "[orfs_prep] wrote %s\n" staged_sdc;
  Printf.eprintf "[orfs_prep] wrote %s\n" cfg_path;
  Printf.eprintf "[orfs_prep] %d macro(s) staged in config\n"
    (List.length (List.sort_uniq compare
       (List.map (fun a -> a.Mem_macro_resolve.module_name) mem_arts)));
  Printf.printf "\nTo run ORFS:\n  cd %s\n  make DESIGN_CONFIG=%s FLOW_VARIANT=%s\n"
    orfs_flow cfg_path variant
