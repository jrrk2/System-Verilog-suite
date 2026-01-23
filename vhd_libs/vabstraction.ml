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

open Buffer
open Printf
open Vparser
open VhdlTree

type patrec = {
     buf: Buffer.t;
     uniqs: int ref;
     uniql: int ref;
     uniqe: int ref;
     uniqn: int ref;
     uniqr: int ref;
     uniqc: int ref;
     tab: int ref;
     verbose: bool;
     blst: string list ref;
     name: string;
     }

(* Changed from immutable binding to mutable reference for isolation between parses *)
let vhdlhash = ref (Hashtbl.create 256 : (vhdintf*string, vhdintf) Hashtbl.t);;

let dumplen = function
  | 0 -> "Str(\"ZeroTuple\""
  | 1 -> ""
  | 2 -> "Double"
  | 3 -> "Triple"
  | 4 -> "Quadruple"
  | 5 -> "Quintuple"
  | 6 -> "Sextuple"
  | 7 -> "Septuple"
  | 8 -> "Octuple"
  | 9 -> "Nonuple"
  | 10 -> "Decuple"
  | 11 -> "Undecuple"
  | 12 -> "Duodecuple"
  | 13 -> "Tredecuple"
  | 14 -> "Quattuordecuple"
  | 15 -> "Quindecuple"
  | 16 -> "Sexdecuple"
  | 17 -> "Septendecuple"
  | 18 -> "Octodecuple"
  | 19 -> "Novemdecuple"
  | 20 -> "Vigenuple"
  | 21 -> "Unvigenuple"
  | 22 -> "Duovigenuple"
  | 23 -> "Trevigenuple"
  | 24 -> "Quattuorvigenuple"
  | 25 -> "Quinvigenuple"
  | err -> "N-"^string_of_int err^"-tuple"

let rec log2 = function
  | 0 -> 0
  | 1 -> 0
  | n -> 1 + log2(n/2)

let rec abstraction' patrec pat = 
abstraction patrec pat;
bprintf patrec.buf " -> %s(Str \"%%\"" (dumplen (!(patrec.uniqs)+(!(patrec.uniql))+(!(patrec.uniqe))+(!(patrec.uniqn))+(!(patrec.uniqr))+(!(patrec.uniqc))+1));
let delim = ref ',' in for i = 0 to !(patrec.uniqs)-1 do bprintf patrec.buf "%cstr%d" !delim i; delim := ','; done;
for i = 0 to !(patrec.uniqn)-1 do bprintf patrec.buf "%cnum%d" !delim i; delim := ','; done;
for i = 0 to !(patrec.uniqr)-1 do bprintf patrec.buf "%creal%d" !delim i; delim := ','; done;
for i = 0 to !(patrec.uniqc)-1 do bprintf patrec.buf "%cchr%d" !delim i; delim := ','; done;
for i = 0 to !(patrec.uniql)-1 do
bprintf patrec.buf "%cmatch_%s lst%d" !delim patrec.name i; delim := ','; done;
for i = 0 to !(patrec.uniqe)-1 do bprintf patrec.buf "%cmatch_%s exp%d" !delim patrec.name i; delim := ','; done;bprintf patrec.buf ")"

and abstraction patrec = function
  | Octuple(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) -> bprintf patrec.buf "Octuple("; abstraction patrec arg1;bprintf patrec.buf ",";
                                                                abstraction patrec arg2;bprintf patrec.buf ",";
                                                                abstraction patrec arg3;bprintf patrec.buf ",";
                                                                abstraction patrec arg4;bprintf patrec.buf ",";
                                                                abstraction patrec arg5;bprintf patrec.buf ",";
                                                                abstraction patrec arg6;bprintf patrec.buf ",";
                                                                abstraction patrec arg7;bprintf patrec.buf ",";
                                                                abstraction patrec arg8;bprintf patrec.buf ")";
  | Septuple(arg1,arg2,arg3,arg4,arg5,arg6,arg7) ->  bprintf patrec.buf "Septuple("; abstraction patrec arg1;bprintf patrec.buf ",";
                                                             abstraction patrec arg2;bprintf patrec.buf ",";
                                                             abstraction patrec arg3;bprintf patrec.buf ",";
                                                             abstraction patrec arg4;bprintf patrec.buf ",";
                                                             abstraction patrec arg5;bprintf patrec.buf ",";
                                                             abstraction patrec arg6;bprintf patrec.buf ",";
                                                             abstraction patrec arg7;bprintf patrec.buf ")";
  | Sextuple(arg1,arg2,arg3,arg4,arg5,arg6) ->  bprintf patrec.buf "Sextuple(";abstraction patrec arg1;bprintf patrec.buf ",";
                                                        abstraction patrec arg2;bprintf patrec.buf ",";
                                                        abstraction patrec arg3;bprintf patrec.buf ",";
                                                        abstraction patrec arg4;bprintf patrec.buf ",";
                                                        abstraction patrec arg5;bprintf patrec.buf ",";
                                                        abstraction patrec arg6;bprintf patrec.buf ")";
  | Quintuple(arg1,arg2,arg3,arg4,arg5) ->  bprintf patrec.buf "Quintuple(";abstraction patrec arg1;bprintf patrec.buf ",";
                                                     abstraction patrec arg2;bprintf patrec.buf ",";
                                                     abstraction patrec arg3;bprintf patrec.buf ",";
                                                     abstraction patrec arg4;bprintf patrec.buf ",";
                                                     abstraction patrec arg5;bprintf patrec.buf ")";
  | Quadruple(arg1,arg2,arg3,arg4) -> bprintf patrec.buf "Quadruple(";abstraction patrec arg1;bprintf patrec.buf ",";
                abstraction patrec arg2;bprintf patrec.buf ",";
                abstraction patrec arg3;bprintf patrec.buf ",";
                abstraction patrec arg4;bprintf patrec.buf ")";
  | Triple(arg1,arg2,arg3) -> bprintf patrec.buf "Triple(";
                abstraction patrec arg1;bprintf patrec.buf ",";
                abstraction patrec arg2;bprintf patrec.buf ",";
               abstraction patrec arg3;bprintf patrec.buf ")";
  | List lst ->
    if (patrec.verbose) then ( bprintf patrec.buf "List ";
      let delim = ref '[' in
      List.iter (fun arg -> 
                bprintf patrec.buf "%c" !delim;
                delim := ';';
                abstraction patrec arg) lst;
      add_string patrec.buf (if !delim<>'[' then "]" else "[]"))
      else (
        bprintf patrec.buf "lst%d" !(patrec.uniql); incr patrec.uniql;
        List.iter (fun itm ->
                let patrec2 = {buf=Buffer.create 256;uniqs=ref 0;uniql=ref 0;uniqe=ref 0;uniqn=ref 0;uniqr=ref 0;uniqc=ref 0;tab=ref 0;verbose=false;blst=ref [];name=patrec.name} in
                abstraction' patrec2 itm;
                patrec.blst := ("| "^Buffer.contents patrec2.buf) :: !(patrec.blst) @ !(patrec2.blst)
                ) lst
        )
  | Double((VhdAttributeName|VhdElsif|VhdCondition|VhdParenthesedPrimary) as kind,exp) ->
        bprintf patrec.buf "Double(%s,exp%d)" (Asctoken.asctoken kind) !(patrec.uniqe); incr patrec.uniqe;
        let patrec2 = {buf=Buffer.create 256;uniqs=ref 0;uniql=ref 0;uniqe=ref 0;uniqn=ref 0;uniqr=ref 0;uniqc=ref 0;tab=ref 0;verbose=false;blst=ref [];name=patrec.name} in
                abstraction' patrec2 exp;
                patrec.blst := ("| "^Buffer.contents patrec2.buf) :: !(patrec.blst) @ !(patrec2.blst)
  | Double(arg1,arg2) -> bprintf patrec.buf "Double(";abstraction
    patrec arg1;bprintf patrec.buf ","; abstraction patrec arg2;bprintf patrec.buf ")";
  | VhdNone -> bprintf patrec.buf "VhdNone"
  | Str "" -> bprintf patrec.buf "Str \"\""
  | Str s -> bprintf patrec.buf "str%d" !(patrec.uniqs); incr patrec.uniqs
  | Num s -> bprintf patrec.buf "num%d" !(patrec.uniqn); incr patrec.uniqn
  | Real s -> bprintf patrec.buf "real%d" !(patrec.uniqr); incr patrec.uniqr
  | Char s -> bprintf patrec.buf "chr%d" !(patrec.uniqc); incr patrec.uniqc
  | others -> add_string patrec.buf (Asctoken.asctoken others)

let patabstract nam pat =
let patrec = {buf=Buffer.create 256;uniqs=ref 0;uniql=ref 0;uniqe=ref 0;uniqn=ref 0;uniqr=ref 0;uniqc=ref 0;tab=ref 0;verbose=false;blst=ref [];name=nam} in abstraction' patrec pat;
let abslst = ref ["| "^Buffer.contents patrec.buf] in
let last = ref "" in List.iter (fun itm -> if itm <> !last then abslst := itm :: !abslst; last := itm) (List.sort compare !(patrec.blst));
!abslst

let rec unescape str = if str <> "" then (match str.[0] with
| '\\' -> (match str.[1] with
    | 't' -> (String.make 1 '\t')^unescape (String.sub str 2 (String.length str -2))
    | 'n' -> (String.make 1 '\n')^unescape (String.sub str 2 (String.length str -2))
    | '\"' -> (String.make 1 '\"')^unescape (String.sub str 2 (String.length str -2))
    | _ -> unescape (String.sub str 1 (String.length str - 1)))
| _ -> (String.make 1 str.[0])^unescape (String.sub str 1 (String.length str - 1)))
else ""

let patdump nam =
let hashtab = Hashtbl.create 256 in
let ifd = open_in (nam^".digest") in
let fd3 = open_out (nam^".digest.chk") in
try 
    while true do let lin = input_line ifd in
          let idx1 = String.index lin ' ' in
          let ix = int_of_string (String.sub lin 0 idx1) in
          let idx2 = String.index_from lin (idx1+1) ' ' in
          let hash = String.sub lin (idx1+1) (idx2-idx1-1) in
          let stmt = String.sub lin (idx2+2) ((String.length lin)-idx2-3) in
          let ustmt = unescape stmt in
          let pstmt = String.escaped ustmt in
          assert(String.length hash = 32);
          assert(stmt = pstmt);
          Hashtbl.replace hashtab ustmt ix;
          fprintf fd3 "%.3d %s \"%s\"\n" ix hash stmt;
          done;
    with End_of_file -> ();
close_in ifd;
close_out fd3;
let abslst = ref [] in
Hashtbl.iter (fun str itm -> abslst := str :: !abslst) hashtab;
Hashtbl.iter (fun (c,k) pat -> abslst := (patabstract nam pat) @ !abslst) !vhdlhash;

let fd = open_out (nam^".ml") in
Printf.fprintf fd "\nopen VhdlTree\n\nlet rec match_%s = function\n" nam;
let last = ref "" in List.iter (fun itm -> if (itm <> !last) && (String.sub itm 0 9 <> "| str0 ->") && (String.sub itm 0 9 <> "| lst0 ->") then (
    let pc = String.index itm '%' in
    if not (Hashtbl.mem hashtab itm) then Hashtbl.replace hashtab itm (Hashtbl.length hashtab);
    let outstr = String.sub itm 0 pc^(string_of_int (Hashtbl.find hashtab itm))^String.sub itm (pc+1) ((String.length itm)-(pc+1)) in
    let tab = ref 0 in for i = 0 to (String.length outstr)-1 do 
      output_char fd outstr.[i];
      if (outstr.[i]=',') && (i > !tab+80) then (output_string fd "\n\t"; tab := i)
      done;
      output_char fd '\n');
  last := itm) (List.sort compare !abslst);
Printf.fprintf fd "| List [itm] -> match_%s itm\n" nam;
Printf.fprintf fd "| List lst -> List (List.map match_%s lst)\n" nam;
Printf.fprintf fd "| others -> others\n";
close_out fd;
let max = ref 0 in Hashtbl.iter (fun str itm -> if !max < itm then max := itm) hashtab;
let fd2 = open_out (nam^".digest.new") in
let arr = Array.make (!max+1) "" in
Hashtbl.iter (fun str itm -> arr.(itm) <- str) hashtab;
Array.iteri (fun ix str -> fprintf fd2 "%.3d %s \"%s\"\n" ix (Digest.to_hex (Digest.string str)) (String.escaped str)) arr;
close_out fd2
