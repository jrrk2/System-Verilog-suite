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

open Elexer
open Edif2

let rec str_token (e:token) = match e with
| ID id -> "ID \""^id^"\""
| STRING str -> "STRING \""^str^"\""
| INT arg -> "INT "^(string_of_int arg)
| ILLEGAL arg -> "ILLEGAL "^(String.make 1 arg)
| TLIST lst -> "TLIST ["^String.concat ";" (List.map str_token lst)^"]"
| ITEM(kind, itm) -> "ITEM("^str_token kind^", "^str_token itm^")"
| ITEM2(kind, itm1, itm2) -> "ITEM2("^str_token kind^", "^str_token itm1^", "^str_token itm2^")"
| _ -> (Eord.getstr e)

exception End_of_File

let eparse arg =
  let inchan = open_in arg in
  let lexbuf = Lexing.from_channel inchan in
  try 
    Edif2.start Elexer.token lexbuf;
  with e ->
    for i = 1 to hsiz do
      let idx = (hsiz-i+(!histcnt))mod hsiz in
      let item = !(history.(idx)) in
      match item.tok with
        | EMPTYEDIF -> ()
        | STRING str -> Printf.printf "Backtrace %d : \"%s\" (%d-%d)\n"  i str item.strt item.stop
        | ID id -> Printf.printf "Backtrace %d : ID %s (%d-%d)\n"  i id item.strt item.stop
        | _ -> Printf.printf "Backtrace %d : %s (%d-%d)\n"  i (str_token (item.tok)) item.strt item.stop
    done;
    Edif2.EMPTYEDIF

let eparsestr arg =
  let lexbuf = Lexing.from_string arg in
  try 
    Edif2.start Elexer.token lexbuf;
  with e ->
    for i = 1 to hsiz do
      let idx = (hsiz-i+(!histcnt))mod hsiz in
      let item = !(history.(idx)) in
      match item.tok with
        | EMPTYEDIF -> ()
        | STRING str -> Printf.printf "Backtrace %d : \"%s\" (%d-%d)\n"  i str item.strt item.stop
        | ID id -> Printf.printf "Backtrace %d : ID %s (%d-%d)\n"  i id item.strt item.stop
        | _ -> Printf.printf "Backtrace %d : %s (%d-%d)\n"  i (str_token (item.tok)) item.strt item.stop
    done;
    failwith arg
