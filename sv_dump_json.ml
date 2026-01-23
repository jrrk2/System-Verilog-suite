(* sv_dump_json.ml - Dump SystemVerilog token patterns to JSON for debugging *)

open Source_text_verible_json

(* Compact description extractor for logging *)
let rec get_description ?(depth=0) token =
  if depth > 2 then "..." else
  match token with
  | EMPTY_TOKEN -> "EMPTY"
  | STRING s -> Printf.sprintf "\"%s\"" (String.sub s 0 (min (String.length s) 20))
  | SymbolIdentifier id -> Printf.sprintf "ID:%s" id
  | TK_UnBasedNumber n -> Printf.sprintf "NUM:%s" n
  | TK_DecNumber n -> Printf.sprintf "DEC:%s" n
  | TUPLE2 (a, _) -> Printf.sprintf "TUPLE2(%s,...)" (get_description ~depth:(depth+1) a)
  | TUPLE3 (a, _, _) -> Printf.sprintf "TUPLE3(%s,...)" (get_description ~depth:(depth+1) a)
  | TUPLE4 (a, _, _, _) -> Printf.sprintf "TUPLE4(%s,...)" (get_description ~depth:(depth+1) a)
  | TUPLE5 (a, _, _, _, _) -> Printf.sprintf "TUPLE5(%s,...)" (get_description ~depth:(depth+1) a)
  | TUPLE6 (a, _, _, _, _, _) -> Printf.sprintf "TUPLE6(%s,...)" (get_description ~depth:(depth+1) a)
  | TUPLE7 (a, _, _, _, _, _, _) -> Printf.sprintf "TUPLE7(%s,...)" (get_description ~depth:(depth+1) a)
  | TUPLE8 (a, _, _, _, _, _, _, _) -> Printf.sprintf "TUPLE8(%s,...)" (get_description ~depth:(depth+1) a)
  | TUPLE9 (a, _, _, _, _, _, _, _, _) -> Printf.sprintf "TUPLE9(%s,...)" (get_description ~depth:(depth+1) a)
  | TUPLE10 (a, _, _, _, _, _, _, _, _, _) -> Printf.sprintf "TUPLE10(%s,...)" (get_description ~depth:(depth+1) a)
  | TUPLE11 (a, _, _, _, _, _, _, _, _, _, _) -> Printf.sprintf "TUPLE11(%s,...)" (get_description ~depth:(depth+1) a)
  | TUPLE12 (a, _, _, _, _, _, _, _, _, _, _, _) -> Printf.sprintf "TUPLE12(%s,...)" (get_description ~depth:(depth+1) a)
  | TUPLE13 (a, _, _, _, _, _, _, _, _, _, _, _, _) -> Printf.sprintf "TUPLE13(%s,...)" (get_description ~depth:(depth+1) a)
  | TUPLE14 (a, _, _, _, _, _, _, _, _, _, _, _, _, _) -> Printf.sprintf "TUPLE14(%s,...)" (get_description ~depth:(depth+1) a)
  | TUPLE15 (a, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> Printf.sprintf "TUPLE15(%s,...)" (get_description ~depth:(depth+1) a)
  | TLIST items -> Printf.sprintf "TLIST[%d]" (List.length items)
  | ELIST items -> Printf.sprintf "ELIST[%d]" (List.length items)
  | SLIST items -> Printf.sprintf "SLIST[%d]" (List.length items)
  | _ -> "TOKEN"

(* Dump unhandled pattern to JSON file using ppx_deriving_yojson *)
let dump_unhandled context pattern_type token =
  let timestamp = Unix.time () |> int_of_float in
  let filename = Printf.sprintf "unhandled_sv/%s_%s_%d.json" context pattern_type timestamp in

  (* Ensure directory exists *)
  (try Unix.mkdir "unhandled_sv" 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());

  (* Use the auto-generated token_to_yojson function *)
  let token_json = token_to_yojson token in

  let json = `Assoc [
    ("context", `String context);
    ("pattern_type", `String pattern_type);
    ("timestamp", `Int timestamp);
    ("pattern", token_json);
  ] in

  let oc = open_out filename in
  Yojson.Safe.pretty_to_channel oc json;
  close_out oc;

  Printf.printf "Dumped unhandled %s pattern to %s\n" pattern_type filename;
  Printf.printf "  Description: %s\n" (get_description token)

(* Create a summary of all unhandled patterns *)
let create_summary () =
  if Sys.file_exists "unhandled_sv" && Sys.is_directory "unhandled_sv" then
    let files = Sys.readdir "unhandled_sv" |> Array.to_list in
    let json_files = List.filter (fun f -> Filename.check_suffix f ".json") files in
    Printf.printf "\n=== Unhandled SystemVerilog Patterns Summary ===\n";
    Printf.printf "Total unhandled patterns: %d\n" (List.length json_files);

    (* Group by pattern type *)
    let pattern_types = Hashtbl.create 16 in
    List.iter (fun filename ->
      let parts = String.split_on_char '_' filename in
      if List.length parts >= 2 then
        let ptype = List.nth parts 1 in
        let count = try Hashtbl.find pattern_types ptype with Not_found -> 0 in
        Hashtbl.replace pattern_types ptype (count + 1)
    ) json_files;

    Printf.printf "\nBy pattern type:\n";
    Hashtbl.iter (fun ptype count ->
      Printf.printf "  %s: %d\n" ptype count
    ) pattern_types
  else
    Printf.printf "No unhandled patterns directory found.\n"
