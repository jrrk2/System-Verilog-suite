(* tool_scan.ml — find the external tools each frontend needs, before anything
 * offers them.
 *
 * A menu that lists `slang` on a machine with no slang is not a menu, it is a
 * trap: the user picks it, waits for a parse, and gets "[slang] driver not
 * found" from somewhere three layers down.  So the workbench SCANS first and
 * only offers what it can actually run, and where a tool exists in a place
 * nobody guessed, the user SELECTS it and that choice is persisted.
 *
 * Selection only works because every external frontend now takes an env
 * override (YOSYS_BIN / SLANG_BIN / SV2V_BIN / SYNLIG_BIN / VERILATOR_BIN /
 * SV_PARSER_BIN) and this module exports the resolved path into it.  The scan
 * and the frontend therefore agree by construction: whatever the scan resolved
 * is exactly what the frontend will exec.
 *)

type kind =
  | Builtin       (* pure OCaml — nothing to find *)
  | External      (* needs a binary on disk *)
  | Prepared      (* needs no binary here, but its INPUT is made by one *)

type spec = {
  fe          : string;        (* frontend name, as load_frontend spells it *)
  kind        : kind;
  exe         : string;        (* binary base name ("" for Builtin) *)
  env         : string;        (* env var this module exports the path into *)
  candidates  : string list;   (* absolute paths to try, in the frontend's own order *)
  version_arg : string;
  note        : string;
}

let home () = try Sys.getenv "HOME" with Not_found -> ""

(* The candidate lists MIRROR the frontends' own search order (sv_lua's
   find_yosys / find_sv2v / find_synlig / find_verilator, slang_to_behavioral's
   find_slang, sv_parser_dump's default bin).  Keep them in step: a scan that
   looks somewhere the frontend does not is a scan that lies. *)
let specs () : spec list = [
  { fe = "verible"; kind = Builtin; exe = ""; env = ""; candidates = [];
    version_arg = "";
    note = "built-in parse-tree frontend (no external tool)" };
  { fe = "verible-ext"; kind = Builtin; exe = ""; env = ""; candidates = [];
    version_arg = "";
    note = "built-in, resolves externals across the file set" };
  { fe = "vhdl"; kind = Builtin; exe = ""; env = ""; candidates = [];
    version_arg = "";
    note = "built-in VHDL frontend (vhd_front)" };
  { fe = "slang"; kind = External; exe = "slang"; env = "SLANG_BIN";
    candidates = [ home () ^ "/sv-tests/third_party/tools/slang/build/bin/slang";
                   home () ^ "/slang/build/bin/slang";
                   "/usr/local/bin/slang"; "/usr/bin/slang" ];
    version_arg = "--version";
    note = "independent SV elaborator, --ast-json" };
  { fe = "yosys"; kind = External; exe = "yosys"; env = "YOSYS_BIN";
    candidates = [ home () ^ "/oss-cad-suite/bin/yosys";
                   "/usr/local/bin/yosys"; "/usr/bin/yosys" ];
    version_arg = "--version";
    note = "synthesis: RTLIL via the slang plugin (YOSYS_READ_VERILOG=1 for the built-in reader)" };
  { fe = "sv2v"; kind = External; exe = "sv2v"; env = "SV2V_BIN";
    candidates = [ home () ^ "/sv2v/bin/sv2v"; "/usr/local/bin/sv2v" ];
    version_arg = "--version";
    note = "SV → Verilog, then yosys (needs yosys too)" };
  { fe = "synlig"; kind = External; exe = "synlig"; env = "SYNLIG_BIN";
    candidates = [ home () ^ "/synlig/build/release/synlig/synlig";
                   "/usr/local/bin/synlig"; "/usr/bin/synlig" ];
    version_arg = "--version";
    note = "yosys fork reading SV through Surelog" };
  (* NOT "VERILATOR_BIN": that variable is verilator's own (it names
     `verilator_bin` for the perl driver), so exporting the wrapper's path into
     it makes verilator re-exec itself without end.  SVS reads VERILATOR_BIN
     for compatibility but writes only SVS_VERILATOR_BIN. *)
  { fe = "verilator"; kind = External; exe = "verilator"; env = "SVS_VERILATOR_BIN";
    candidates = [ "/usr/local/bin/verilator"; "/usr/bin/verilator" ];
    version_arg = "--version";
    note = "elaborated AST JSON (a bare .json argument is used as-is)" };
  { fe = "sv-parser"; kind = External; exe = "parse_sv"; env = "SV_PARSER_BIN";
    candidates = [ home () ^ "/sv-parser/target/release/examples/parse_sv" ];
    version_arg = "";
    note = "dalance/sv-parser CST oracle (`parse_sv -t`)" };
  { fe = "surelog"; kind = Prepared; exe = "surelog"; env = "SURELOG_BIN";
    candidates = [ "/usr/local/bin/surelog"; "/usr/bin/surelog" ];
    version_arg = "--version";
    note = "takes a PRE-CAPTURED uhdm-dump .dump; surelog itself is not invoked" };
]

type status = {
  st_spec    : spec;
  st_path    : string option;    (* resolved binary *)
  st_source  : string;           (* built-in | selected | env | candidate | PATH *)
  st_version : string option;
  st_extra   : string option;    (* capability notes found by probing *)
}

let is_exec p =
  try (Unix.stat p).Unix.st_kind = Unix.S_REG
      && (try Unix.access p [ Unix.X_OK ]; true with _ -> false)
  with _ -> false

(* First line of a command's output, with a wall-clock bound: a tool that
   blocks on stdin (or waits for a licence server) must not hang the scan. *)
let first_line cmd =
  let cmd =
    if Sys.file_exists "/usr/bin/timeout" then "timeout 5 " ^ cmd else cmd in
  try
    let ic = Unix.open_process_in (cmd ^ " 2>/dev/null </dev/null") in
    let l = try Some (String.trim (input_line ic)) with End_of_file -> None in
    ignore (Unix.close_process_in ic);
    (match l with Some "" -> None | x -> x)
  with _ -> None

let which exe =
  match first_line (Printf.sprintf "command -v %s" (Filename.quote exe)) with
  | Some p when p <> "" && Sys.file_exists p -> Some p
  | _ -> None

(* ── persisted selections ──────────────────────────────────────────── *)

let config_path () =
  let xdg =
    try Sys.getenv "XDG_CONFIG_HOME"
    with Not_found ->
      (try Sys.getenv "HOME" ^ "/.config" with Not_found -> ".") in
  Filename.concat xdg "sv_suite/tools.json"

let selections : (string, string) Hashtbl.t = Hashtbl.create 8

let load_config () =
  Hashtbl.reset selections;
  let p = config_path () in
  if Sys.file_exists p then
    try
      match Yojson.Safe.from_file p with
      | `Assoc l ->
          (match List.assoc_opt "tools" l with
           | Some (`Assoc tl) ->
               List.iter (function
                 | (fe, `String path) -> Hashtbl.replace selections fe path
                 | _ -> ()) tl
           | _ -> ())
      | _ -> ()
    with _ -> ()

(* mkdir -p without a shell.  `Sys.command "mkdir -p …"` needs mkdir on PATH,
   and the one situation where a tool picker matters most is a session with a
   broken PATH — where that silently failed and the selection was never
   written. *)
let rec mkdir_p dir =
  if dir <> "" && dir <> "/" && dir <> "." && not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let save_config () =
  let p = config_path () in
  mkdir_p (Filename.dirname p);
  let tools = Hashtbl.fold (fun k v acc -> (k, `String v) :: acc) selections [] in
  let j = `Assoc [ "version", `Int 1;
                   "tools", `Assoc (List.sort compare tools) ] in
  let oc = open_out p in
  output_string oc (Yojson.Safe.pretty_to_string j);
  output_char oc '\n';
  close_out oc

(* ── the scan ──────────────────────────────────────────────────────── *)

(* Probe what a found binary can actually do, where the answer changes whether
   the frontend works.  yosys is the case that matters: the reader is the slang
   PLUGIN, so a yosys without it fails on the first SV file even though the
   binary is right there. *)
let probe_extra (sp : spec) (path : string) : string option =
  if sp.fe <> "yosys" then None
  else
    let cmd = Printf.sprintf "%s -q -p 'plugin -i slang' " (Filename.quote path) in
    let rc = Sys.command (cmd ^ " >/dev/null 2>&1 </dev/null") in
    if rc = 0 then Some "yosys-slang plugin present"
    else Some "NO yosys-slang plugin — set YOSYS_READ_VERILOG=1 for the \
               built-in reader (weaker SV coverage)"

let scan_one (sp : spec) : status =
  match sp.kind with
  | Builtin ->
      { st_spec = sp; st_path = None; st_source = "built-in";
        st_version = None; st_extra = None }
  | External | Prepared ->
      let selected = Hashtbl.find_opt selections sp.fe in
      let env_val =
        if sp.env = "" then None
        else match Sys.getenv_opt sp.env with
          | Some s when s <> "" -> Some s | _ -> None in
      let resolved, source =
        match selected with
        | Some p when is_exec p -> (Some p, "selected")
        | _ ->
          match env_val with
          | Some p when is_exec p -> (Some p, "env " ^ sp.env)
          | _ ->
            match List.find_opt is_exec sp.candidates with
            | Some p -> (Some p, "candidate")
            | None ->
              match which sp.exe with
              | Some p -> (Some p, "PATH")
              | None -> (None, "not found") in
      let version = match resolved with
        | Some p when sp.version_arg <> "" ->
            first_line (Printf.sprintf "%s %s" (Filename.quote p) sp.version_arg)
        | _ -> None in
      let extra = match resolved with
        | Some p -> probe_extra sp p
        | None -> None in
      { st_spec = sp; st_path = resolved; st_source = source;
        st_version = version; st_extra = extra }

(* Export every resolved path into the frontend's env override, so the tool the
   scan reported is exactly the tool that runs. *)
let export (sts : status list) =
  List.iter (fun st ->
    match st.st_path, st.st_spec.env with
    | Some p, env when env <> "" -> Unix.putenv env p
    | _ -> ()) sts

let scan () : status list =
  let sts = List.map scan_one (specs ()) in
  export sts;
  sts

let available (st : status) =
  match st.st_spec.kind with
  | Builtin -> true
  | Prepared -> true          (* usable from a pre-captured dump either way *)
  | External -> st.st_path <> None

(* sv2v is a two-stage frontend: it converts, then hands off to yosys.  Offering
   it when yosys is missing is the same trap one level down. *)
let blocked_by (sts : status list) (st : status) : string option =
  if st.st_spec.fe <> "sv2v" then None
  else
    match List.find_opt (fun s -> s.st_spec.fe = "yosys") sts with
    | Some y when available y -> None
    | _ -> Some "needs yosys as well"

let usable sts st = available st && blocked_by sts st = None

let cached : status list option ref = ref None

let get ?(rescan = false) () : status list =
  match !cached with
  | Some s when not rescan -> s
  | _ -> load_config (); let s = scan () in cached := Some s; s

let find ?(rescan = false) fe : status option =
  List.find_opt (fun st -> st.st_spec.fe = fe) (get ~rescan ())

let available_frontends ?(rescan = false) () : string list =
  let sts = get ~rescan () in
  List.filter_map (fun st -> if usable sts st then Some st.st_spec.fe else None) sts

let all_frontends () = List.map (fun sp -> sp.fe) (specs ())

(* One-line label for a menu: the name plus WHERE it came from, because
   "yosys" and "yosys from the checkout you forgot about" are different tools
   and the difference shows up as a mysterious verdict. *)
let label (st : status) : string =
  match st.st_spec.kind, st.st_path with
  | Builtin, _ -> st.st_spec.fe ^ "  (built-in)"
  | _, Some p -> Printf.sprintf "%s  (%s)" st.st_spec.fe p
  | _, None -> Printf.sprintf "%s  (NOT FOUND)" st.st_spec.fe

(* Actionable failure, used before a frontend runs.  The point is that the
   message names the fix — the env var and the picker — rather than reporting
   an absence. *)
let ensure (fe : string) : (unit, string) result =
  let sts = get () in
  match List.find_opt (fun st -> st.st_spec.fe = fe) sts with
  | None ->
      Error (Printf.sprintf "unknown frontend '%s' (have: %s)" fe
               (String.concat ", " (all_frontends ())))
  | Some st ->
      (match blocked_by sts st with
       | Some why ->
           Error (Printf.sprintf "frontend '%s' %s" fe why)
       | None ->
         if available st then begin
           (* re-export in case something cleared the environment since *)
           export [ st ]; Ok ()
         end else
           Error (Printf.sprintf
             "frontend '%s' needs '%s', which is not installed anywhere I \
              looked (%s). Point it at the binary: set %s=/path/to/%s, or use \
              the workbench's Tools… picker (`sv_suite equiv --tool %s=/path`)."
             fe st.st_spec.exe
             (String.concat ", " (st.st_spec.candidates @ [ "PATH" ]))
             st.st_spec.env st.st_spec.exe fe))

(* Record a user's choice and make it take effect now. *)
let select (fe : string) (path : string) : (unit, string) result =
  match List.find_opt (fun sp -> sp.fe = fe) (specs ()) with
  | None -> Error ("unknown frontend '" ^ fe ^ "'")
  | Some sp when sp.kind = Builtin ->
      Error (Printf.sprintf "'%s' is built in — there is no binary to select" fe)
  | Some sp ->
      if not (Sys.file_exists path) then Error ("no such file: " ^ path)
      else if not (is_exec path) then Error ("not an executable file: " ^ path)
      else begin
        Hashtbl.replace selections sp.fe path;
        if sp.env <> "" then Unix.putenv sp.env path;
        cached := None;
        (* The selection is live either way; say so when it could not be
           REMEMBERED, rather than swallowing the write error and letting the
           user find out next session. *)
        (try save_config (); Ok () with e ->
           Error (Printf.sprintf
             "%s → %s applied for this session, but %s could not be written: %s"
             fe path (config_path ()) (Printexc.to_string e)))
      end

let clear_selection (fe : string) =
  Hashtbl.remove selections fe;
  (try save_config () with _ -> ());
  cached := None

let () = load_config ()

let report (sts : status list) : string =
  let b = Buffer.create 2048 in
  let p fmt = Printf.ksprintf (Buffer.add_string b) fmt in
  p "External tools (config: %s)\n" (config_path ());
  p "  %-12s %-10s %-46s %s\n" "frontend" "status" "path" "found via";
  List.iter (fun st ->
    let status =
      if not (available st) then "MISSING"
      else match blocked_by sts st with
        | Some _ -> "BLOCKED"
        | None -> (match st.st_spec.kind with
                   | Builtin -> "built-in" | Prepared -> "dump-only" | External -> "ok") in
    p "  %-12s %-10s %-46s %s\n" st.st_spec.fe status
      (match st.st_path with Some x -> x | None -> "—") st.st_source;
    (match st.st_version with Some v -> p "  %-12s %s\n" "" v | None -> ());
    (match st.st_extra with Some e -> p "  %-12s %s\n" "" e | None -> ());
    (match blocked_by sts st with
     | Some why -> p "  %-12s %s\n" "" ("unusable: " ^ why)
     | None -> ())) sts;
  p "\n  %d of %d frontends usable: %s\n"
    (List.length (List.filter (usable sts) sts)) (List.length sts)
    (String.concat ", "
       (List.filter_map (fun st -> if usable sts st then Some st.st_spec.fe else None) sts));
  Buffer.contents b
