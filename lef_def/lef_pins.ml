(* Token-stream extractor for cell/pin direction info from LEF.

   Walks the LEF token stream and pulls out
       (cell_name, pin_name, direction)
   for every PIN inside every MACRO.  Used by the placement
   timing pipeline to identify drivers (DIRECTION OUTPUT) when
   the DEF doesn't follow the "first-pin-is-driver" convention.

   Like the DEF placement extractor, this is heuristic
   pattern-matching over tokens — robust to LEF version drift
   and not tripped up by the bison grammar's gaps. *)

open Lef_file

type direction = Input | Output | Inout | Other_dir

let direction_of_token = function
  | K_INPUT  -> Input
  | K_OUTPUT -> Output
  | K_INOUT  -> Inout
  | _        -> Other_dir

let string_of_direction = function
  | Input -> "INPUT"
  | Output -> "OUTPUT"
  | Inout -> "INOUT"
  | Other_dir -> "?"

type pin_entry = { cell : string; pin : string; dir : direction }

let tokens_of_channel ch =
  let lb = Lexing.from_channel ch in
  let acc = ref [] in
  (try
     while true do
       let t = Lef_file_lex.token lb in
       acc := t :: !acc;
       if t = EOF_TOKEN then raise Exit
     done
   with Exit -> ());
  Array.of_list (List.rev !acc)

(* Look for [K_DIRECTION (K_INPUT|K_OUTPUT|K_INOUT) SEMICOLON]
   ahead of the current PIN; first such hit wins. *)
let find_dir toks i hi =
  let j = ref i in
  let found = ref Other_dir in
  let stop = ref false in
  while not !stop && !j < hi - 2 do
    (match toks.(!j) with
     | K_DIRECTION ->
         (match toks.(!j+1), toks.(!j+2) with
          | (K_INPUT | K_OUTPUT | K_INOUT) as d, SEMICOLON ->
              found := direction_of_token d;
              stop := true
          | _ -> incr j)
     | K_PIN | K_END -> stop := true   (* moved into next PIN/MACRO *)
     | _ -> incr j);
  done;
  !found

let parse_channel ch =
  let toks = tokens_of_channel ch in
  let n = Array.length toks in
  let out = ref [] in
  let cur_macro = ref "" in
  let i = ref 0 in
  while !i + 1 < n do
    (match toks.(!i), toks.(!i+1) with
     | K_MACRO, QSTRING s ->
         cur_macro := s; i := !i + 2
     | K_END, QSTRING s
       when !cur_macro <> "" && s = !cur_macro ->
         cur_macro := ""; i := !i + 2
     | K_PIN, QSTRING pin ->
         let dir = find_dir toks (!i + 2) n in
         if !cur_macro <> "" then
           out := { cell = !cur_macro; pin; dir } :: !out;
         i := !i + 2
     | _ -> incr i);
  done;
  List.rev !out

let parse path =
  let ch = open_in path in
  let r = parse_channel ch in
  close_in ch;
  r

(* Convenience: build a hashtable keyed by (cell, pin) for fast
   lookup during net analysis. *)
let table_of_entries entries =
  let h = Hashtbl.create (List.length entries) in
  List.iter (fun e -> Hashtbl.replace h (e.cell, e.pin) e.dir) entries;
  h
