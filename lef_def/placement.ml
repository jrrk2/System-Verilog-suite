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
    (* PLACED is not the only placement status: a DEF that REPLAYS a vendor
       layout marks components FIXED, and a hard macro may be COVER.  The
       token shape is identical, so all three share this arm.  (In NETS,
       "+ FIXED SITE ( ..." is followed by a STRING, not LPAREN, so it
       cannot reach here.) *)
    | (K_PLACED | K_FIXED | K_COVER), LPAREN, NUMBER x, NUMBER y, RPAREN, orient_t ->
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

(* ── Routing, from the same token stream ──────────────────────────────────
   A DEF routing statement is  <layer> <point> (<point> | <via>)*  where a
   point is "( x y )" and STAR repeats that coordinate from the previous
   point.  Two points in succession are a wire segment; a via name attaches
   at the point before it.  Statements open with "+ ROUTED"/"+ FIXED"/
   "+ COVER" and continue with NEW, each NEW restarting layer and point.

   This walks the tokens rather than matching text: the lexer already knows
   about STAR, comments, quoted names and the fact that a net's header line
   ("- name ( SITE PIN ) ...") holds parenthesised NON-coordinates, which is
   exactly what trips a regex tokeniser up. *)

type routing = {
  segs : (int * int * int * int * string) list;   (* x1 y1 x2 y2 layer *)
  vias : (int * int) list;
  die  : int * int * int * int;
  units : int;
  design : string;
}

(* Text of a token used as a name.  Plain identifiers arrive as STRING; one
   that happens to spell a DEF keyword arrives as that keyword, so fall back
   to the token's own spelling with the K_ prefix removed. *)
let layer_name t =
  match t with
  | STRING s -> Some s
  | LPAREN | RPAREN | SEMICOLON | HYPHEN | PLUS | STAR | NUMBER _ -> None
  | _ ->
    let s = Def_file_tokens.getstr t in
    if String.length s > 2 && String.sub s 0 2 = "K_"
    then Some (String.sub s 2 (String.length s - 2))
    else None

let routing_of_tokens toks =
  let n = Array.length toks in
  let segs = ref [] and vias = ref [] in
  let die = ref (0, 0, 0, 0) and units = ref 2000 and design = ref "" in
  let in_nets = ref false in
  let layer = ref "" and last = ref None in
  (* a point at [i]: LPAREN (NUMBER|STAR) (NUMBER|STAR) RPAREN *)
  let point_at i =
    if i + 3 < n && toks.(i) = LPAREN && toks.(i+3) = RPAREN then
      let cv t prev = match t with
        | NUMBER v -> Some v
        | STAR     -> Some prev
        | _        -> None in
      let px, py = match !last with Some (a, b) -> a, b | None -> 0, 0 in
      (match cv toks.(i+1) px, cv toks.(i+2) py with
       | Some x, Some y -> Some (x, y)
       | _ -> None)
    else None
  in
  let i = ref 0 in
  while !i < n do
    (match toks.(!i) with
     | K_DESIGN ->
       (match toks.(!i + 1) with STRING s -> design := s | _ -> ())
     | K_UNITS ->
       (* UNITS DISTANCE MICRONS <n> *)
       let rec num j =
         if j >= n || j > !i + 4 then ()
         else match toks.(j) with NUMBER v -> units := v | _ -> num (j + 1) in
       num !i
     | K_DIEAREA ->
       (match point_at (!i + 1) with
        | Some (x1, y1) ->
          (match point_at (!i + 5) with
           | Some (x2, y2) -> die := (x1, y1, x2, y2)
           | None -> ())
        | None -> ())
     | K_NETS    -> in_nets := true
     | K_END     -> if !i + 1 < n && toks.(!i + 1) = K_NETS then in_nets := false
     | HYPHEN    -> if !in_nets then (layer := ""; last := None)
     | K_NEW | K_ROUTED ->
       if !in_nets then (last := None;
                         (* The layer follows immediately -- but a layer name
                            can collide with a DEF KEYWORD, and SITE does:
                            "NEW SITE (...)" lexes as K_SITE, not STRING, so
                            taking only STRING silently dropped every
                            SITE-layer statement (6022 segments, 10428 pips
                            on ethmin).  Recover the text from getstr. *)
                         match layer_name toks.(!i + 1) with
                         | Some s -> layer := s; incr i
                         | None -> ())
     | _ -> ());
    (* Points and vias only inside a routing statement, i.e. once a layer is
       open.  Before that (the net header) they are site/pin pairs. *)
    if !in_nets && !layer <> "" then begin
      match point_at !i with
      | Some (x, y) ->
        (match !last with
         | Some (a, b) when not (a = x && b = y) ->
           segs := (a, b, x, y, !layer) :: !segs
         | _ -> ());
        last := Some (x, y);
        i := !i + 3
      | None ->
        (match toks.(!i), !last with
         | STRING _, Some (x, y) -> vias := (x, y) :: !vias
         | _ -> ())
    end;
    incr i
  done;
  { segs = List.rev !segs; vias = List.rev !vias;
    die = !die; units = !units; design = !design }

let routing path =
  let ch = open_in path in
  let toks = tokens_of_channel ch in
  close_in ch;
  routing_of_tokens toks
