(* Token-stream connectivity extractor for the DEF NETS section.

   Each net in DEF has the form
       - <net_name> ( inst pin ) ( inst pin ) ... + USE SIGNAL
         + ROUTED ... ;
   so after the leading "- name" we collect every parenthesised
   (STRING STRING) pair up to the first "+" — those are the pin
   connections.  The routing geometry that follows the first "+"
   is ignored. *)

open Def_file

type pin_ref = { inst : string; pin : string }
type net     = { name : string; pins : pin_ref list }

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

(* Find K_NETS (start) and K_END K_NETS (end), return (start, stop)
   indices delimiting the section body. *)
let find_section toks tag_opener tag_closer =
  let n = Array.length toks in
  let i = ref 0 in
  let start = ref None in
  let stop  = ref None in
  while !i < n - 1 do
    (match toks.(!i), toks.(!i+1) with
     | K_END, t when t = tag_closer && !stop = None ->
        stop := Some !i
     | t, _ when t = tag_opener && !start = None
                && (!i = 0 || toks.(!i - 1) <> K_END) ->
        let j = ref (!i + 1) in
        while !j < n && toks.(!j) <> SEMICOLON do incr j done;
        start := Some (!j + 1)
     | _ -> ());
    incr i
  done;
  match !start, !stop with
  | Some a, Some b when a < b -> Some (a, b)
  | _ -> None

let parse_nets_in_range toks lo hi =
  let nets = ref [] in
  let i = ref lo in
  while !i < hi do
    if toks.(!i) = HYPHEN && !i + 1 < hi then begin
      match toks.(!i + 1) with
      | STRING net_name ->
          let j = ref (!i + 2) in
          let pins = ref [] in
          let stop = ref false in
          while not !stop && !j < hi do
            (match toks.(!j) with
             | LPAREN when !j + 3 < hi ->
                 (match toks.(!j+1), toks.(!j+2), toks.(!j+3) with
                  | STRING inst, STRING pin, RPAREN ->
                      pins := { inst; pin } :: !pins;
                      j := !j + 4
                  | _ -> incr j)
             | PLUS | SEMICOLON -> stop := true
             | _ -> incr j)
          done;
          nets := { name = net_name; pins = List.rev !pins } :: !nets;
          i := !j
      | _ -> incr i
    end else incr i
  done;
  List.rev !nets

let parse_channel ch =
  let toks = tokens_of_channel ch in
  match find_section toks K_NETS K_NETS with
  | None -> []
  | Some (lo, hi) -> parse_nets_in_range toks lo hi

let parse path =
  let ch = open_in path in
  let r = parse_channel ch in
  close_in ch;
  r
