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

open Vparser;;
open Ord;;

type logt = Closed | Open of out_channel

module TokSet = Set.Make (OrdTok)

  let hsiz = 64;;
  type hist = {tok:token;strt:int;stop:int;key:bool;psl:bool};;
  let hist_init () = {tok=EMPTY;strt=0;stop=0;key=false;psl=false};;
  let histcnt = ref 0;;
  let history = Array.init hsiz (fun i -> hist_init())

let one_elm = TokSet.add EMPTY TokSet.empty;;

let rec str_token (e:token) = match e with
| ID id -> "ID "^id.Idhash.id
| RANGE (INT left,INT right) -> "[" ^ (string_of_int left) ^ ":" ^ (string_of_int right) ^ "] "
| RANGE (left, right) -> "[" ^ (str_token left) ^ ":" ^ (str_token right) ^ "] "
| TRIPLE (DOT, inner, tok) -> "."^(str_token inner)^"("^(str_token tok)^")"
| WEAK arg | PWEAK arg -> arg
| PREPROC arg -> arg
| INTNUM arg -> "INTNUM "^arg
| ILLEGAL arg -> "ILLEGAL "^(String.make 1 arg)
| HEXNUM arg -> "HEXNUM "^arg
| FLOATNUM arg -> "FLOATNUM "^(string_of_float arg)
| DECNUM arg -> "DECNUM "^arg
| BUFIF arg -> arg
| BINNUM arg -> "BINNUM "^arg
| ASCNUM arg -> "ASCNUM "^arg
| TEDGE arg -> Printf.sprintf "EDGE %c%c" (fst arg) (snd arg)
| SCALAR -> "scalar"
| WIRE -> "wire"
| REG -> "reg"
| _ -> String.uppercase_ascii (Ord.getstr e)

type maincmd_t = (string, ((bool ref -> int -> int ref -> string array -> unit) * string * int)) Hashtbl.t

let (main_cmds:maincmd_t) = Hashtbl.create 256

let failwith_ptr = ref (fun (msg:string) -> ())

let myfailwith (msg:string) = print_endline msg; !failwith_ptr msg
