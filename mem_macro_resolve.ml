(* Memory-macro resolver.
 *
 * Given a request for an SRAM/ROM of a particular shape, return a
 * struct describing where to find its Verilog model, Liberty .lib,
 * LEF, and (when available) GDS — generating the macro on demand
 * via OpenRAM if it isn't already cached.
 *
 * Cache layout:
 *   ~/.cache/sv_decompiler/openram_macros/<tech>/<key>/
 *      <key>.v      Verilog blackbox/behavioural model
 *      <key>.lib    Liberty for OpenROAD blackbox
 *      <key>.lef    LEF for OpenROAD placement
 *      <key>.gds    GDS (optional)
 *      cfg.py       OpenRAM config (RAM only)
 *      manifest     "module_name=<key>" + port shape
 *
 * The cache is content-addressed — if the request matches an existing
 * directory, it's a hit and we skip OpenRAM.  Hand-emitted ROM stubs
 * land in the same cache so consumers always see a uniform interface.
 *)

type tech = Sky130 | Scn4m_subm | Freepdk45 | Gf180mcu

let string_of_tech = function
  | Sky130 -> "sky130"
  | Scn4m_subm -> "scn4m_subm"
  | Freepdk45 -> "freepdk45"
  | Gf180mcu -> "gf180mcu"

let tech_of_string = function
  | "sky130" -> Sky130
  | "scn4m_subm" -> Scn4m_subm
  | "freepdk45" -> Freepdk45
  | "gf180mcu" -> Gf180mcu
  | s -> failwith ("unknown tech: " ^ s)

(* Default tech: scn4m_subm — works without external PDK install,
 * matches the OpenRAM smoke-test path.  Override with TECH env or
 * a per-call argument. *)
let default_tech () =
  match Sys.getenv_opt "MEM_MACRO_TECH" with
  | Some t -> tech_of_string t
  | None -> Scn4m_subm

type macro_kind =
  | Sram of { n_rw: int; n_r: int; n_w: int }
  | Rom  of { init_values: int list }

type request = {
  tech: tech;
  kind: macro_kind;
  word_size: int;   (* bits per word *)
  num_words: int;   (* depth *)
}

type port_shape = {
  module_name: string;
  addr_width: int;
  data_width: int;
  (* Each list is per-port, indexed 0..n_ports-1.  None entries mean
     that port doesn't have the corresponding pin (e.g. read-only ports
     omit din/web/wmask). *)
  clk:   string list;
  csb:   string list;
  web:   string option list;
  wmask: string option list;
  addr:  string list;
  din:   string option list;
  dout:  string list;
}

type artifacts = {
  request: request;
  module_name: string;
  cache_dir: string;
  verilog_path: string;
  liberty_path: string;
  lef_path: string option;
  gds_path: string option;
  port_shape: port_shape;
}

(* Crash on shapes we don't try to handle.  v1 supports SRAM with
 * n_rw + n_r ≤ 2 (1RW, 1RW1R) and ROM. *)
let validate r =
  if r.word_size <= 0 then
    failwith (Printf.sprintf "mem_macro_resolve: word_size=%d invalid" r.word_size);
  if r.num_words <= 1 then
    failwith (Printf.sprintf "mem_macro_resolve: num_words=%d invalid" r.num_words);
  match r.kind with
  | Sram { n_rw; n_r; n_w } ->
      if n_w > 0 then
        failwith "mem_macro_resolve: write-only ports not supported (use 1RW)";
      if n_rw + n_r > 2 then
        failwith
          (Printf.sprintf "mem_macro_resolve: %dRW%dR exceeds 1RW1R limit" n_rw n_r);
      if n_rw = 0 && n_r > 0 then
        failwith "mem_macro_resolve: read-only SRAM — use Rom kind instead"
  | Rom _ -> ()

let bits_needed n =
  if n <= 1 then 1
  else
    let rec loop b m = if m >= n then b else loop (b + 1) (m * 2) in
    loop 1 2

(* ──────────── cache key ──────────── *)

let cache_root () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  Filename.concat home ".cache/sv_decompiler/openram_macros"

let key_of_request r =
  let tech_s = string_of_tech r.tech in
  match r.kind with
  | Sram { n_rw; n_r; n_w } ->
      Printf.sprintf "sram_%drw%dr%dw_%d_%d_%s"
        n_rw n_r n_w r.word_size r.num_words tech_s
  | Rom { init_values } ->
      let h = Hashtbl.hash init_values in
      Printf.sprintf "rom_%d_%d_%s_%08x"
        r.word_size r.num_words tech_s (h land 0xFFFFFFFF)

let cache_dir_for r =
  Filename.concat (Filename.concat (cache_root ()) (string_of_tech r.tech))
    (key_of_request r)

(* ──────────── filesystem helpers ──────────── *)

let mkdir_p p =
  let rec aux p =
    if Sys.file_exists p then ()
    else begin
      aux (Filename.dirname p);
      try Unix.mkdir p 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    end
  in
  aux p

let write_file p contents =
  mkdir_p (Filename.dirname p);
  let oc = open_out p in
  output_string oc contents;
  close_out oc

let copy_file src dst =
  mkdir_p (Filename.dirname dst);
  let ic = open_in_bin src in
  let oc = open_out_bin dst in
  let buf = Bytes.create 8192 in
  let rec loop () =
    let n = input ic buf 0 (Bytes.length buf) in
    if n > 0 then begin output oc buf 0 n; loop () end in
  loop (); close_in ic; close_out oc

(* ──────────── OpenRAM invocation ──────────── *)

let openram_home () =
  match Sys.getenv_opt "OPENRAM_HOME" with
  | Some p -> p
  | None ->
      let home = Sys.getenv "HOME" in
      Filename.concat home "OpenRAM/compiler"

let openram_tech () =
  match Sys.getenv_opt "OPENRAM_TECH" with
  | Some p -> p
  | None ->
      let home = Sys.getenv "HOME" in
      Filename.concat home "OpenRAM/technology"

let openram_root () =
  Filename.concat (openram_home ()) ".."
  |> Filename.dirname |> Filename.dirname  (* ~/OpenRAM *)

let sram_compiler_py () =
  Filename.concat (openram_root ()) "OpenRAM/sram_compiler.py"

(* Heuristic existence check for the SRAM compiler. *)
let openram_available () =
  Sys.file_exists (sram_compiler_py ())
  || Sys.file_exists (Filename.concat (openram_home ()) "../sram_compiler.py")

let resolved_sram_compiler () =
  let candidates = [
    Filename.concat (Filename.dirname (openram_home ())) "sram_compiler.py";
    sram_compiler_py ();
  ] in
  match List.find_opt Sys.file_exists candidates with
  | Some p -> p
  | None ->
      failwith
        ("mem_macro_resolve: cannot find sram_compiler.py — set \
          OPENRAM_HOME or install OpenRAM at ~/OpenRAM")

(* OpenRAM crashes during delay characterization for non-power-of-2
 * num_words (its bitline-net probe lookup assumes the natural
 * row × col split for a clean shape).  Round up to the next pow2
 * for the OpenRAM call: the decoder then addresses 2^k rows, the
 * upper rows are physically present but never accessed by caller
 * RTL (whose addr bus is bits_needed(num_words) wide and is
 * already valid for the rounded depth — bits_needed(576) ==
 * bits_needed(1024) == 10).  Trades up to ~50% area for shape
 * coverage; a future OpenRAM patch could omit the unused rows
 * physically. *)
let next_pow2 n =
  let rec loop p = if p >= n then p else loop (p * 2) in
  loop 1

(* OpenRAM also enforces a minimum of 16 rows total
 * (sram_config.amend_words_per_row asserts
 *  tentative_num_rows * words_per_row >= 16).  Tiny depths like 4
 * are bumped to 16; the upper rows are present but unaddressed. *)
let openram_num_words r = max 16 (next_pow2 r.num_words)

let emit_openram_config r =
  match r.kind with
  | Rom _ -> failwith "emit_openram_config: not for ROM"
  | Sram { n_rw; n_r; n_w } ->
      let tech_s = string_of_tech r.tech in
      let depth = openram_num_words r in
      Printf.sprintf {|word_size = %d
num_words = %d
num_rw_ports = %d
num_r_ports = %d
num_w_ports = %d
tech_name = "%s"
nominal_corner_only = True
process_corners = ["TT"]
%s
%s

route_supplies = "side"
check_lvsdrc = False

output_name = "%s"
output_path = "."
|}
        r.word_size depth n_rw n_r n_w tech_s
        (match r.tech with
         | Scn4m_subm -> "supply_voltages = [5.0]"
         | Sky130 -> "supply_voltages = [1.8]"
         | Freepdk45 -> "supply_voltages = [1.0]"
         | Gf180mcu -> "supply_voltages = [3.3]")
        (match r.tech with
         | Scn4m_subm | Sky130 | Gf180mcu -> "temperatures = [25]"
         | Freepdk45 -> "temperatures = [25]")
        (key_of_request r)

let invoke_openram r =
  validate r;
  let tmp_dir = Filename.concat
    (try Sys.getenv "OPENRAM_TMP" with Not_found -> "/tmp")
    (Printf.sprintf "openram_resolve_%d" (Unix.getpid ())) in
  mkdir_p tmp_dir;
  let cfg_path = Filename.concat tmp_dir "cfg.py" in
  write_file cfg_path (emit_openram_config r);
  let cmd =
    Printf.sprintf "OPENRAM_HOME=%s OPENRAM_TECH=%s python3 %s cfg.py"
      (Filename.quote (openram_home ()))
      (Filename.quote (openram_tech ()))
      (Filename.quote (resolved_sram_compiler ())) in
  Printf.eprintf "[mem_macro_resolve] generating %s …\n%!" (key_of_request r);
  let rc = Sys.command (Printf.sprintf "cd %s && %s > openram.log 2>&1"
                          (Filename.quote tmp_dir) cmd) in
  if rc <> 0 then begin
    Printf.eprintf "OpenRAM failed (rc=%d).  Log:\n" rc;
    let log = Filename.concat tmp_dir "openram.log" in
    if Sys.file_exists log then begin
      let ic = open_in log in
      try while true do prerr_endline (input_line ic) done
      with End_of_file -> close_in ic
    end;
    failwith "mem_macro_resolve: OpenRAM compile failed"
  end;
  tmp_dir

(* ──────────── ROM stub emitter (small ROMs) ──────────── *)

let emit_rom_stub r init_values =
  let aw = bits_needed r.num_words in
  let dw = r.word_size in
  let module_name = key_of_request r in
  let buf = Buffer.create 1024 in
  Buffer.add_string buf
    (Printf.sprintf "// hand-emitted ROM stub (depth=%d, width=%d)\n"
       r.num_words r.word_size);
  Buffer.add_string buf
    (Printf.sprintf "module %s (clk0, csb0, addr0, dout0);\n" module_name);
  Buffer.add_string buf
    (Printf.sprintf "  parameter ADDR_WIDTH = %d;\n" aw);
  Buffer.add_string buf
    (Printf.sprintf "  parameter DATA_WIDTH = %d;\n" dw);
  Buffer.add_string buf
    "  input  clk0;\n  input  csb0;\n";
  Buffer.add_string buf
    "  input  [ADDR_WIDTH-1:0] addr0;\n";
  Buffer.add_string buf
    "  output reg [DATA_WIDTH-1:0] dout0;\n";
  Buffer.add_string buf "  always @(posedge clk0) if (!csb0) case (addr0)\n";
  List.iteri (fun i v ->
    if i < r.num_words then
      Buffer.add_string buf
        (Printf.sprintf "    %d'd%d: dout0 <= %d'h%x;\n" aw i dw v)
  ) init_values;
  Buffer.add_string buf
    (Printf.sprintf "    default: dout0 <= %d'h0;\n" dw);
  Buffer.add_string buf "  endcase\nendmodule\n";
  Buffer.contents buf

(* Minimal Liberty stub for OpenROAD blackbox treatment.  Just the cell
 * declaration with pin types — no characterised timing.  Tools should
 * treat the macro as a black box for now (later we can call OpenSTA's
 * characterise_cell or run OpenRAM with check_lvsdrc on for proper
 * timing). *)
let emit_rom_lib_stub r =
  let module_name = key_of_request r in
  let aw = bits_needed r.num_words in
  Printf.sprintf {|library (%s) {
  cell (%s) {
    pin (clk0)  { direction : input; clock : true; }
    pin (csb0)  { direction : input; }
    bus (addr0) { bus_type : addr_bus%d; direction : input; }
    bus (dout0) { bus_type : data_bus%d; direction : output; }
  }
  type (addr_bus%d) { base_type : array; data_type : bit; bit_width : %d; bit_from : %d; bit_to : 0; }
  type (data_bus%d) { base_type : array; data_type : bit; bit_width : %d; bit_from : %d; bit_to : 0; }
}
|} module_name module_name
   aw r.word_size aw aw (aw - 1)
   r.word_size r.word_size (r.word_size - 1)

(* ──────────── port_shape for SRAM / ROM ──────────── *)

let sram_port_shape r n_rw n_r =
  let aw = bits_needed r.num_words in
  let dw = r.word_size in
  let key = key_of_request r in
  let rw_ports = List.init n_rw (fun i -> i) in
  let r_ports  = List.init n_r  (fun i -> i + n_rw) in
  let port_id i = string_of_int i in
  let clk   = List.map (fun i -> "clk" ^ port_id i) (rw_ports @ r_ports) in
  let csb   = List.map (fun i -> "csb" ^ port_id i) (rw_ports @ r_ports) in
  let web   =
    List.map (fun i -> Some ("web" ^ port_id i)) rw_ports
    @ List.map (fun _ -> None) r_ports in
  let wmask =
    if dw >= 8 then
      List.map (fun i -> Some ("wmask" ^ port_id i)) rw_ports
      @ List.map (fun _ -> None) r_ports
    else
      List.map (fun _ -> None) (rw_ports @ r_ports) in
  let addr  = List.map (fun i -> "addr" ^ port_id i) (rw_ports @ r_ports) in
  let din   =
    List.map (fun i -> Some ("din" ^ port_id i)) rw_ports
    @ List.map (fun _ -> None) r_ports in
  let dout  = List.map (fun i -> "dout" ^ port_id i) (rw_ports @ r_ports) in
  { module_name = key;
    addr_width = aw; data_width = dw;
    clk; csb; web; wmask; addr; din; dout }

let rom_port_shape r =
  let aw = bits_needed r.num_words in
  { module_name = key_of_request r;
    addr_width = aw; data_width = r.word_size;
    clk = ["clk0"]; csb = ["csb0"];
    web = [None]; wmask = [None];
    addr = ["addr0"]; din = [None]; dout = ["dout0"] }

(* ──────────── lookup / generate ──────────── *)

let cache_hit cache_dir =
  Sys.file_exists cache_dir
  && Sys.file_exists (Filename.concat cache_dir "manifest")

let load_from_cache r cache_dir =
  let key = key_of_request r in
  let port_shape = match r.kind with
    | Sram { n_rw; n_r; _ } -> sram_port_shape r n_rw n_r
    | Rom _ -> rom_port_shape r in
  let p ext = Filename.concat cache_dir (key ^ ext) in
  let exists_or_none p = if Sys.file_exists p then Some p else None in
  { request = r;
    module_name = key;
    cache_dir;
    verilog_path = p ".v";
    liberty_path = p ".lib";
    lef_path = exists_or_none (p ".lef");
    gds_path = exists_or_none (p ".gds");
    port_shape }

let generate_rom_into_cache r init_values cache_dir =
  let key = key_of_request r in
  mkdir_p cache_dir;
  write_file (Filename.concat cache_dir (key ^ ".v"))
    (emit_rom_stub r init_values);
  write_file (Filename.concat cache_dir (key ^ ".lib"))
    (emit_rom_lib_stub r);
  write_file (Filename.concat cache_dir "manifest")
    (Printf.sprintf "module_name=%s\nkind=rom_stub\n" key)

let generate_sram_into_cache r cache_dir =
  let tmp_dir = invoke_openram r in
  let key = key_of_request r in
  mkdir_p cache_dir;
  (* OpenRAM drops files directly in tmp_dir, not tmp_dir/<key>/.
     Plain extensions land as <key><ext>; .lib gets a corner suffix
     (e.g. <key>_TT_5p0V_25C.lib).  Find them by prefix-match. *)
  let entries =
    try Array.to_list (Sys.readdir tmp_dir) with Sys_error _ -> [] in
  let prefix_match prefix ext =
    List.find_opt (fun n ->
      let l = String.length n in
      let lp = String.length prefix and le = String.length ext in
      l >= lp + le
      && String.sub n 0 lp = prefix
      && String.sub n (l - le) le = ext) entries
  in
  List.iter (fun ext ->
    match prefix_match key ext with
    | Some name ->
        let src = Filename.concat tmp_dir name in
        copy_file src (Filename.concat cache_dir (key ^ ext))
    | None -> ()
  ) [".v"; ".lib"; ".lef"; ".gds"; ".sp"];
  let cfg = Filename.concat tmp_dir "cfg.py" in
  if Sys.file_exists cfg then
    copy_file cfg (Filename.concat cache_dir "cfg.py");
  write_file (Filename.concat cache_dir "manifest")
    (Printf.sprintf "module_name=%s\nkind=openram_sram\n" key)

(* ──────────── FakeRAM (bsg_fakeram-style pre-built macros) ──────────── *)

(* Find $ORFS_DIR/flow/platforms/$PLATFORM/lef and .../lib by reading
   FAKERAM_PLATFORM_DIR set by the GUI's wizard.  Each platform ships
   pre-baked fakeram45_<N>x<W>.lef/.lib pairs (originally generated by
   bsg_fakeram); when our request shape exactly matches one, we emit a
   thin wrapper that bridges our (clkN, csbN, webN, addrN, dinN, doutN,
   wmaskN) interface onto FakeRAM's (clk, ce_in, we_in, addr_in, wd_in,
   rd_out, w_mask_in).                                                *)

let fakeram_platform_dir () = Sys.getenv_opt "FAKERAM_PLATFORM_DIR"

let fakeram_use_enabled () =
  match Sys.getenv_opt "MEM_USE_FAKERAM" with
  | Some "1" | Some "true" | Some "yes" -> true
  | _ -> false

let fakeram_shape_re =
  Str.regexp "fakeram[0-9]+_\\([0-9]+\\)x\\([0-9]+\\)\\.lef$"

let discover_fakeram_shapes () =
  match fakeram_platform_dir () with
  | None -> []
  | Some dir ->
      let lef_dir = Filename.concat dir "lef" in
      if not (Sys.file_exists lef_dir) then []
      else
        let entries =
          try Array.to_list (Sys.readdir lef_dir) with _ -> [] in
        List.filter_map (fun f ->
          if Str.string_match fakeram_shape_re f 0 then
            try
              let n = int_of_string (Str.matched_group 1 f) in
              let w = int_of_string (Str.matched_group 2 f) in
              let stem = Filename.chop_extension f in    (* fakeram45_NxW *)
              Some (n, w, stem)
            with _ -> None
          else None
        ) entries

(* Returns Some (module, lef, lib) if the request shape matches a
   pre-built single-port FakeRAM exactly; None otherwise.            *)
let fakeram_match r =
  match r.kind with
  | Rom _ -> None
  | Sram { n_rw; n_r; n_w } when n_rw = 1 && n_r = 0 && n_w = 0 ->
      let want = (r.num_words, r.word_size) in
      let shapes = discover_fakeram_shapes () in
      let m = List.find_opt (fun (n, w, _) -> (n, w) = want) shapes in
      (match m, fakeram_platform_dir () with
       | Some (_, _, stem), Some dir ->
           let lef = Filename.concat (Filename.concat dir "lef") (stem ^ ".lef") in
           let lib = Filename.concat (Filename.concat dir "lib") (stem ^ ".lib") in
           if Sys.file_exists lef && Sys.file_exists lib
           then Some (stem, lef, lib)
           else None
       | _ -> None)
  | Sram _ -> None    (* dual-port not supported by FakeRAM *)

let emit_fakeram_wrapper r fakeram_mod =
  let key = key_of_request r in
  let aw = bits_needed r.num_words in
  let dw = r.word_size in
  let mask_bits = (dw + 7) / 8 in
  let buf = Buffer.create 512 in
  Buffer.add_string buf
    (Printf.sprintf "// Auto-generated FakeRAM wrapper: %s → %s\n"
       key fakeram_mod);
  Buffer.add_string buf
    (Printf.sprintf "module %s (\n" key);
  Buffer.add_string buf "  input  clk0,\n";
  Buffer.add_string buf "  input  csb0,\n";
  Buffer.add_string buf "  input  web0,\n";
  if dw >= 8 then
    Buffer.add_string buf
      (Printf.sprintf "  input  [%d:0] wmask0,\n" (mask_bits - 1));
  Buffer.add_string buf
    (Printf.sprintf "  input  [%d:0] addr0,\n" (aw - 1));
  Buffer.add_string buf
    (Printf.sprintf "  input  [%d:0] din0,\n" (dw - 1));
  Buffer.add_string buf
    (Printf.sprintf "  output [%d:0] dout0\n" (dw - 1));
  Buffer.add_string buf ");\n";
  (* OpenROAD's STA Verilog reader (synth_odb.tcl) is structural-only
     and rejects inline bitwise expressions in port connections.
     Pre-compute ce_in / we_in via continuous assigns so the macro
     port list contains plain wire references only.  yosys is fine
     with either form, but we have to clear OpenROAD's bar.        *)
  Buffer.add_string buf "  wire ce_in;\n";
  Buffer.add_string buf "  wire we_in;\n";
  Buffer.add_string buf "  assign ce_in = ~csb0;\n";
  Buffer.add_string buf "  assign we_in = ~web0 & ~csb0;\n";
  Buffer.add_string buf
    (Printf.sprintf "  %s ram (\n" fakeram_mod);
  Buffer.add_string buf "    .clk      (clk0),\n";
  Buffer.add_string buf "    .ce_in    (ce_in),\n";
  Buffer.add_string buf "    .we_in    (we_in),\n";
  Buffer.add_string buf "    .addr_in  (addr0),\n";
  Buffer.add_string buf "    .wd_in    (din0),\n";
  if dw >= 8 then
    Buffer.add_string buf "    .w_mask_in(wmask0),\n";
  Buffer.add_string buf "    .rd_out   (dout0)\n";
  Buffer.add_string buf "  );\nendmodule\n";
  Buffer.contents buf

let generate_fakeram_into_cache r cache_dir fakeram_mod fakeram_lef fakeram_lib =
  let key = key_of_request r in
  mkdir_p cache_dir;
  write_file (Filename.concat cache_dir (key ^ ".v"))
    (emit_fakeram_wrapper r fakeram_mod);
  (* The platform's .lef/.lib files are already on ORFS's path, but
     we copy them into our cache_dir under the request's key prefix
     so synth_orfs_shim's manifest emit can point at concrete paths
     that won't move.                                              *)
  copy_file fakeram_lef (Filename.concat cache_dir (key ^ ".lef"));
  copy_file fakeram_lib (Filename.concat cache_dir (key ^ ".lib"));
  write_file (Filename.concat cache_dir "manifest")
    (Printf.sprintf "module_name=%s\nkind=fakeram_wrapper\nbacking=%s\n"
       key fakeram_mod)

let resolve r =
  validate r;
  let cache_dir = cache_dir_for r in
  if not (cache_hit cache_dir) then begin
    match r.kind with
    | Rom { init_values } ->
        if r.num_words <= 256 then
          generate_rom_into_cache r init_values cache_dir
        else
          (* Larger ROMs: would call rom_compiler.py; that path has a
             known upstream regression (lef_rom_interconnect) and isn't
             exercised yet.  Until it lands, fall back to the stub. *)
          generate_rom_into_cache r init_values cache_dir
    | Sram _ ->
        let used_fakeram =
          if fakeram_use_enabled () then
            match fakeram_match r with
            | Some (m, lef, lib) ->
                Printf.eprintf
                  "[mem_macro_resolve] FakeRAM exact match: %dx%d → %s\n%!"
                  r.num_words r.word_size m;
                generate_fakeram_into_cache r cache_dir m lef lib;
                true
            | None ->
                Printf.eprintf
                  "[mem_macro_resolve] FakeRAM: no exact match for \
                   %dx%d (1RW), falling through to OpenRAM\n%!"
                  r.num_words r.word_size;
                false
          else false
        in
        if not used_fakeram then generate_sram_into_cache r cache_dir
  end;
  load_from_cache r cache_dir
