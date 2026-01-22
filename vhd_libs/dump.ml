(*
    <vscr - Verilog converter to abc format.>
    Copyright (C) <2011,2012>  <Jonathan Richard Robert Kimmitt>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

open Printf
open Vparser
open List

type uptr = UPTR of (out_channel -> int -> Vparser.token -> unit) | UNIL;;

let rec dump (ch:Buffer.t) exp indent = match exp with
| TLIST lst -> bprintf ch "TLIST["; List.iter (fun x -> dump ch x (indent+2); bprintf ch "; ") lst; bprintf ch "]"
| THASH thash -> Hashtbl.iter (fun x _ -> dump ch x (indent+2)) (fst thash);  Hashtbl.iter (fun x _ -> dump ch x (indent+2)) (snd thash)
| DOUBLE(POSEDGE,arg) -> bprintf ch "@(posedge "; dump ch arg (indent+2) ; bprintf ch ")"
| DOUBLE(tok,arg) -> bprintf ch "DOUBLE("; dump ch tok indent; dump ch arg (indent+2) ; bprintf ch ")"
| TRIPLE(EQUALS, arg1, arg2) -> dump ch arg1 (indent+2); bprintf ch "= "; dump ch arg2 (indent+2)
| TRIPLE(IF, arg1, arg2) -> bprintf ch "if ( "; dump ch arg1 (indent+2); bprintf ch ") "; dump ch arg2 (indent+2)
| TRIPLE(PLUS, arg1, arg2) -> dump ch arg1 (indent+2); bprintf ch "+ "; dump ch arg2 (indent+2)
| TRIPLE(LESS, arg1, arg2) -> dump ch arg1 (indent+2); bprintf ch "< "; dump ch arg2 (indent+2)
| TRIPLE(ASSIGNMENT, arg1, arg2) -> dump ch arg1 (indent+2); bprintf ch "= "; dump ch arg2 (indent+2)
| TRIPLE(tok, arg1, arg2) ->  bprintf ch "TRIPLE("; dump ch tok indent; dump ch arg1 (indent+2); dump ch arg2 (indent+2); bprintf ch ")"
| QUADRUPLE(QUERY, arg1, arg2, arg3) ->  bprintf ch "( "; dump ch arg1 (indent+2); bprintf ch "? ";
  dump ch arg2 (indent+2); bprintf ch ": "; dump ch arg3 (indent+2); bprintf ch ") "
| QUADRUPLE(PARTSEL, ID id, hi, lo) ->
    bprintf ch "%s[" id.Idhash.id; dump ch hi (indent+2); bprintf ch ":"; dump ch lo (indent+2); bprintf ch "] "
| QUADRUPLE(EQUALS, arg1, arg2, arg3) -> dump ch arg1 (indent+2); bprintf ch "= "; dump ch arg2 (indent+2); dump ch arg3 (indent+2)
| QUADRUPLE(IF, arg1, arg2, arg3) -> bprintf ch "if ( "; dump ch arg1 (indent+2); bprintf ch ") "; dump ch arg2 (indent+2); bprintf ch "else "; dump ch arg3 (indent+2)
| QUADRUPLE(tok, arg1, arg2, arg3) -> dump ch tok indent; dump ch arg1 (indent+2); dump ch arg2 (indent+2); dump ch arg3 (indent+2); bprintf ch ")"
| QUINTUPLE(MODULE, arg1, arg2, arg3, arg4) -> bprintf ch "module "; dump ch arg1 indent; bprintf ch " "; dump ch arg2 indent; dump ch arg3 (indent+2); dump ch arg4 (indent+2); bprintf ch " endmodule"
| QUINTUPLE(tok, arg1, arg2, arg3, arg4) -> bprintf ch "QUADRUPLE("; dump ch tok indent; dump ch arg1 (indent+2); dump ch arg2 (indent+2); dump ch arg3 (indent+2); dump ch arg4 (indent+2)
| SEXTUPLE(tok, arg1, arg2, arg3, arg4, arg5) -> dump ch tok indent; dump ch arg1 (indent+2); dump ch arg2 (indent+2); dump ch arg3 (indent+2); dump ch arg4 (indent+2); dump ch arg5 (indent+2)
| SEPTUPLE(tok, arg1, arg2, arg3, arg4, arg5, arg6) -> dump ch tok indent; dump ch arg1 (indent+2); dump ch arg2 (indent+2); dump ch arg3 (indent+2); dump ch arg4 (indent+2); dump ch arg5 (indent+2); dump ch arg6 (indent+2)
| RANGE(arg1,arg2) -> bprintf ch "[ "; dump ch arg1 indent; bprintf ch ": "; dump ch arg2 indent; bprintf ch "] "
| ALWAYS -> bprintf ch "always "
| ASCNUM c -> bprintf ch "%s " c
| ASSIGN -> bprintf ch "assign "
| AT -> bprintf ch "@ "
| BINNUM c -> bprintf ch "BINNUM(%s) " c
| BITSEL -> bprintf ch "BitSelect "
| BUFIF lev -> bprintf ch "%s " lev
| D_ATTRIBUTE -> bprintf ch "$attribute "
| DECNUM c -> bprintf ch "DECNUM(%s) " c
| DOT -> bprintf ch " ."
| EMPTY -> bprintf ch " "
| FLOATNUM flt -> bprintf ch "(%f) " flt
| HASH -> bprintf ch "# "
| HEXNUM c -> bprintf ch "'H(%s) " c
| ID str -> bprintf ch "%s " str.Idhash.id
| IDSTR str -> bprintf ch "%s " str
| ILLEGAL c -> bprintf ch "ILLEGAL character %c " c
| INOUT -> bprintf ch "inout "
| INPUT -> bprintf ch "input "
| INTNUM c -> bprintf ch "%s " c
| NEGEDGE -> bprintf ch "negedge "
| OUTPUT -> bprintf ch "output "
| PARTSEL -> bprintf ch "PartSelect "
| PREPROC str -> bprintf ch "`%s" str
| REG -> bprintf ch "reg "
| WEAK strength -> bprintf ch "weak%s" strength
| WIDTHNUM(radix,sz,num) -> bprintf ch "%d'%c%d " sz ((function
  | 2 -> 'b'
  | 8 -> 'o'
  | 10 -> 'd'
  | 16 -> 'h'
  | _ -> '?') radix) num
| INT n -> bprintf ch "%d " n
| _ -> bprintf ch "%s " (Ord.getstr exp)

let dumpstr tok = let buf = Buffer.create 64 in dump buf tok 0; Buffer.contents buf

let moditer oc k (x:Globals.modtree) = bprintf oc "Module %s : " k; dump oc x.Globals.tree 0;;
let dump_module oc m = dump oc (Hashtbl.find Globals.modprims m).Globals.tree 0;;

let (unhand_list) = ref [];;

let unhandled_dflt out_chan ln argt =
  let arg = (ln,argt) in
  if (List.mem arg !unhand_list == false) then
    begin
      unhand_list := arg :: !unhand_list;
      let buf = Buffer.create 64 in
      Printf.bprintf buf "\n**** Unhandled %d at line %d ****\n" (List.length !unhand_list) ln;
      dump buf argt 0;
      Printf.bprintf buf "\n";
      if false then
        Buffer.output_buffer out_chan buf
      else
        failwith (Buffer.contents buf)
    end

let unhandled_ptr = ref (UPTR unhandled_dflt);;

let unhandled out_chan ln arg = match !unhandled_ptr with UPTR fn -> fn out_chan ln arg | UNIL -> ();;
