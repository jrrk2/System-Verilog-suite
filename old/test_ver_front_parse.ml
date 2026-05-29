(* Smoke + structure dump for ver_front on Vivado-emitted Verilog. *)
open Ver_front
open Vparser

(* Map a Vparser.token constructor to a short tag name, for tree dumps. *)
let tag = function
  | EMPTY -> "EMPTY" | ID id -> Printf.sprintf "ID(%s)" id.Idhash.id
  | INPUT -> "INPUT" | OUTPUT -> "OUTPUT" | INOUT -> "INOUT"
  | WIRE -> "WIRE" | REG -> "REG" | TRI0 -> "TRI0" | TRI1 -> "TRI1"
  | ASSIGN -> "ASSIGN" | ALWAYS -> "ALWAYS"
  | INT n -> Printf.sprintf "INT(%d)" n
  | INTNUM s -> Printf.sprintf "INTNUM(%s)" s
  | DECNUM s -> Printf.sprintf "DECNUM(%s)" s
  | HEXNUM s -> Printf.sprintf "HEXNUM(%s)" s
  | BINNUM s -> Printf.sprintf "BINNUM(%s)" s
  | DOT -> "DOT"
  | TLIST _ -> "TLIST" | THASH _ -> "THASH"
  | DOUBLE _ -> "DOUBLE" | TRIPLE _ -> "TRIPLE"
  | QUADRUPLE _ -> "QUADRUPLE" | QUINTUPLE _ -> "QUINTUPLE"
  | SEXTUPLE _ -> "SEXTUPLE" | SEPTUPLE _ -> "SEPTUPLE"
  | RANGE _ -> "RANGE"
  | PARTSEL -> "PARTSEL" | BITSEL -> "BITSEL"
  | other -> Ord.getstr other

let rec dump_token indent t =
  let pad = String.make indent ' ' in
  let ind = indent + 2 in
  match t with
  | TLIST lst ->
      Printf.printf "%sTLIST[%d]\n" pad (List.length lst);
      List.iter (dump_token ind) lst
  | THASH (h1, h2) ->
      Printf.printf "%sTHASH(decls=%d, body=%d)\n" pad
        (Hashtbl.length h1) (Hashtbl.length h2);
      Hashtbl.iter (fun k _ -> dump_token ind k) h1;
      Hashtbl.iter (fun k _ -> dump_token ind k) h2
  | DOUBLE (a, b) ->
      Printf.printf "%sDOUBLE(%s)\n" pad (tag a);
      dump_token ind b
  | TRIPLE (a, b, c) ->
      Printf.printf "%sTRIPLE(%s)\n" pad (tag a);
      dump_token ind b; dump_token ind c
  | QUADRUPLE (a, b, c, d) ->
      Printf.printf "%sQUADRUPLE(%s)\n" pad (tag a);
      dump_token ind b; dump_token ind c; dump_token ind d
  | QUINTUPLE (a, b, c, d, e) ->
      Printf.printf "%sQUINTUPLE(%s)\n" pad (tag a);
      dump_token ind b; dump_token ind c; dump_token ind d; dump_token ind e
  | RANGE (a, b) ->
      Printf.printf "%sRANGE\n" pad; dump_token ind a; dump_token ind b
  | other -> Printf.printf "%s%s\n" pad (tag other)

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "usage: %s <file.v>\n" Sys.argv.(0); exit 2
  end;
  let f = Sys.argv.(1) in
  Printf.printf "Parsing %s with ver_front...\n%!" f;
  let ok = Vparse.parse f in
  Printf.printf "Vparse.parse → %b\n%!" ok;
  Hashtbl.iter (fun name (mt : Globals.modtree) ->
    Printf.printf "\n=== Module %s (top=%b netlist=%b behav=%b seq=%b) ===\n"
      name mt.is_top mt.is_netlist mt.is_behav mt.is_seq;
    dump_token 0 mt.tree
  ) Globals.modprims
