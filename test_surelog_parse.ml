(* Smoke-test for the Surelog UHDM dump parser.
 *
 * Pipeline:
 *   surelog -parse -sverilog FILE.sv  →  slpp_all/surelog.uhdm
 *   uhdm-dump --elab slpp_all/surelog.uhdm  →  text on stdout
 *   surelog_uhdm.parse_uhdm_dump_string  →  cache + token list
 *
 * Surelog/uhdm-dump live in synlig's build tree; libtcmalloc.so.4
 * isn't on the system so we borrow it from the Vivado tree via
 * LD_LIBRARY_PATH. Edit the constants below if the paths move.
 *
 * Currently this is just a "did the parser accept the input" check —
 * it counts top-level token-tree entries. The actual converter to
 * Behavioral_ir is the next deliverable. *)

let surelog =
  "/home/jonathan/synlig/build/release/surelog/install/bin/surelog"
let uhdm_dump =
  "/home/jonathan/synlig/build/release/surelog/install/bin/uhdm-dump"
let ld_lib_path = "/home/Xilinx/Vivado/2022.2/lib/lnx64.o"

let run_cmd cmd =
  let rc = Sys.command cmd in
  if rc <> 0 then
    Printf.eprintf "(warning) command exited %d: %s\n%!" rc cmd

let parse_file sv_path =
  (* If the input ends in ".dump", treat it as a pre-captured uhdm-dump
   * text file and parse it directly — useful for debugging the grammar
   * against a known-good sample. *)
  let ic =
    if Filename.check_suffix sv_path ".dump" then
      open_in sv_path
    else begin
      let dir = Filename.concat (Filename.get_temp_dir_name ())
                                (Printf.sprintf "surelog_%d" (Unix.getpid ())) in
      run_cmd (Printf.sprintf "rm -rf %s && mkdir -p %s" dir dir);
      let surelog_cmd =
        Printf.sprintf "cd %s && LD_LIBRARY_PATH=%s %s -parse -sverilog %s \
                        > surelog.stdout 2>&1"
          dir ld_lib_path surelog sv_path in
      run_cmd surelog_cmd;
      let uhdm_path = Filename.concat dir "slpp_all/surelog.uhdm" in
      if not (Sys.file_exists uhdm_path) then begin
        Printf.eprintf "no .uhdm produced — see %s/surelog.stdout\n" dir;
        exit 1
      end;
      (* Match the user's documented pipeline: no --elab flag. *)
      let pipe_cmd =
        Printf.sprintf "LD_LIBRARY_PATH=%s %s %s 2>/dev/null"
          ld_lib_path uhdm_dump uhdm_path in
      Unix.open_process_in pipe_cmd
    end
  in
  let lb = Lexing.from_channel ic in
  (* Build a token-recording lexer wrapper so we can dump the last N
   * tokens when the parser dies. *)
  let lex = Surelog_uhdm_lex.deflate Surelog_uhdm_lex.token in
  let history = Queue.create () in
  let history_cap = 500 in
  let record_lex lb =
    let t = lex lb in
    if Queue.length history >= history_cap then ignore (Queue.pop history);
    Queue.add t history;
    t
  in
  let tok_name : Surelog_uhdm.token -> string = function
    | STRING s -> Printf.sprintf "STRING %S" s
    | Int i -> Printf.sprintf "Int %d" i
    | COLON -> "COLON" | DOT -> "DOT" | SLASH -> "SLASH"
    | LPAREN -> "LPAREN" | RPAREN -> "RPAREN"
    | Indent -> "Indent" | Unindent -> "Unindent"
    | EOF_TOKEN -> "EOF_TOKEN" | AT -> "AT" | COMMA -> "COMMA"
    | HYPHEN -> "HYPHEN" | Builtin -> "Builtin" | Package -> "Package"
    | Class_defn -> "Class_defn" | Design -> "Design"
    | Vpiparent -> "Vpiparent" | Vpiname -> "Vpiname"
    | Vpifullname -> "Vpifullname" | Vpiclassdefn -> "Vpiclassdefn"
    | Vpidefname -> "Vpidefname" | Vpitop -> "Vpitop"
    | Restored -> "Restored" | Pre_Elab -> "Pre_Elab" | Post_Elab -> "Post_Elab"
    | Work -> "Work" | DESIGN -> "DESIGN"
    | Uhdmallpackages -> "Uhdmallpackages"
    | Uhdmtoppackages -> "Uhdmtoppackages"
    | Uhdmallclasses -> "Uhdmallclasses"
    | Uhdmtopmodules -> "Uhdmtopmodules"
    | Uhdmallmodules -> "Uhdmallmodules"
    | Mailbox -> "Mailbox" | Process -> "Process" | Semaphore -> "Semaphore"
    | File -> "File" | Line -> "Line" | Endln -> "Endln"
    | New -> "New" | Num -> "Num" | Get -> "Get"
    | Bound -> "Bound" | Message -> "Message"
    | Array -> "Array" | Queue -> "Queue" | String -> "String"
    | System -> "System" | Any_sverilog_class -> "Any_sverilog_class"
    | _ -> "<other>"
  in
  let result =
    try Surelog_uhdm.ml_start record_lex lb
    with e ->
      Printf.eprintf "parse error: %s\n" (Printexc.to_string e);
      Printf.eprintf "last %d tokens (oldest first):\n" history_cap;
      Queue.iter (fun t -> Printf.eprintf "  %s\n" (tok_name t)) history;
      let buf = Bytes.to_string (Bytes.sub lb.lex_buffer 0
                                   (min 300 (Bytes.length lb.lex_buffer))) in
      Printf.eprintf "buffer head: %S\n" buf;
      raise e
  in
  (try ignore (Unix.close_process_in ic) with _ -> close_in ic);
  result

let rec count_nodes = function
  | Surelog_uhdm.TLIST xs ->
      1 + List.fold_left (fun acc x -> acc + count_nodes x) 0 xs
  | TUPLE2 (a, b) -> 1 + count_nodes a + count_nodes b
  | TUPLE3 (a, b, c) ->
      1 + count_nodes a + count_nodes b + count_nodes c
  | TUPLE4 (a, b, c, d) ->
      1 + count_nodes a + count_nodes b + count_nodes c + count_nodes d
  | TUPLE5 (a, b, c, d, e) ->
      1 + count_nodes a + count_nodes b + count_nodes c
        + count_nodes d + count_nodes e
  | _ -> 1

let () =
  let sv =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else "/home/jonathan/System-Verilog-suite/test/regressions/single_port_bram.sv"
  in
  Printf.printf "Parsing UHDM dump for %s ...\n%!" sv;
  let (_cache, tokens) = parse_file sv in
  Printf.printf "  %d top-level entries\n" (List.length tokens);
  let total = List.fold_left (fun acc t -> acc + count_nodes t) 0 tokens in
  Printf.printf "  %d total token-tree nodes\n" total;
  Printf.printf "  ✅ parser accepted the dump\n";
  Printf.printf "\nConverting to Behavioral_ir ...\n";
  let prog = Surelog_to_behavioral.convert_tokens tokens in
  Printf.printf "  %d modules\n" (List.length prog.modules);
  List.iter (fun (m : Behavioral_ir.bmodule) ->
    let ins  = List.length (List.filter (fun (s : Behavioral_ir.bsignal) ->
                              s.direction = `Input) m.signals) in
    let outs = List.length (List.filter (fun (s : Behavioral_ir.bsignal) ->
                              s.direction = `Output) m.signals) in
    let ints = List.length (List.filter (fun (s : Behavioral_ir.bsignal) ->
                              s.direction = `Internal) m.signals) in
    Printf.printf "  %-25s  %d in / %d out / %d int  (total %d signals)\n"
      m.name ins outs ints (List.length m.signals)
  ) prog.modules
