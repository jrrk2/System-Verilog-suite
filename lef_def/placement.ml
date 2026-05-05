(* Placement extractor over the DEF token stream.
   Real DEF puts attribute clauses ([+ SOURCE DIST] etc.) between
   "- inst cell" and "+ PLACED ( x y ) orient", so we scan forward
   for [K_PLACED LPAREN NUMBER NUMBER RPAREN orient] and walk back
   to the nearest preceding HYPHEN to recover (inst, cell). *)

open Def_file

type orient =
  | N | S | E | W
  | FN | FS | FE | FW
  | Other of string

type placement = {
  inst   : string;
  cell   : string;
  x      : int;
  y      : int;
  orient : orient;
}

let orient_of_token = function
  | K_N -> N | K_S -> S | K_E -> E | K_W -> W
  | K_FN -> FN | K_FS -> FS | K_FE -> FE | K_FW -> FW
  | _   -> Other "?"

let string_of_orient = function
  | N -> "N" | S -> "S" | E -> "E" | W -> "W"
  | FN -> "FN" | FS -> "FS" | FE -> "FE" | FW -> "FW"
  | Other s -> s

let tokens_of_channel ch =
  let lb = Lexing.from_channel ch in
  let acc = ref [] in
  (try
     while true do
       let t = Def_file_lex.token lb in
       acc := t :: !acc;
       if t = EOF_TOKEN then raise Exit
     done
   with Exit -> ());
  Array.of_list (List.rev !acc)

(* Search backwards from index [hi] (inclusive) for the most recent
   HYPHEN. Return Some (inst, cell) if the next two tokens after the
   HYPHEN are STRING/STRING; None otherwise. *)
let recover_inst_cell toks hi =
  let rec back j =
    if j < 0 then None
    else match toks.(j) with
      | HYPHEN ->
          if j + 2 < Array.length toks then
            (match toks.(j+1), toks.(j+2) with
             | STRING inst, STRING cell -> Some (inst, cell)
             | _ -> None)
          else None
      | SEMICOLON when j <> hi -> None  (* stop at previous record *)
      | _ -> back (j-1)
  in
  back hi

let debug = ref false

let parse_channel ch =
  let toks = tokens_of_channel ch in
  let n = Array.length toks in
  let out = ref [] in
  let placed_seen = ref 0 and recovered = ref 0 in
  let i = ref 0 in
  while !i + 5 < n do
    (match toks.(!i),    toks.(!i+1), toks.(!i+2),
           toks.(!i+3),  toks.(!i+4), toks.(!i+5) with
    | K_PLACED, LPAREN, NUMBER x, NUMBER y, RPAREN, orient_t ->
        incr placed_seen;
        (match recover_inst_cell toks (!i - 1) with
         | Some (inst, cell) ->
             incr recovered;
             out := { inst; cell; x; y;
                      orient = orient_of_token orient_t } :: !out
         | None ->
             if !debug then begin
               Printf.eprintf "no inst/cell before K_PLACED at tok %d; ctx:" !i;
               for k = max 0 (!i - 30) to !i do
                 Printf.eprintf " [%d]%s" k (Def_file_tokens.getstr toks.(k))
               done;
               prerr_newline ()
             end);
        i := !i + 6
    | _ -> incr i);
  done;
  if !debug then
    Printf.eprintf "K_PLACED seen: %d, recovered: %d\n" !placed_seen !recovered;
  List.rev !out

let parse path =
  let ch = open_in path in
  let r = parse_channel ch in
  close_in ch;
  r
