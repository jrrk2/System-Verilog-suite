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

type logt = Closed | Open of out_channel

val hsiz : int
val histcnt : int ref
type hist = { tok : Vparser.token; strt : int; stop : int; key : bool; psl : bool; }
val hist_init : unit -> hist
val history : hist array
val str_token : Vparser.token -> string

type maincmd_t = (string, ((bool ref -> int -> int ref -> string array -> unit) * string * int)) Hashtbl.t

val main_cmds : maincmd_t

val failwith_ptr: (string -> unit) ref

val myfailwith: string -> unit
