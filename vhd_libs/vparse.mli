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

val parse: string -> bool
val parse': bool ref -> (out_channel -> string -> Vparser.token -> string -> unit) -> string -> unit
val myflush : Setup.logt ref -> unit
val my_openin : string -> in_channel
val includes : (string*in_channel) Stack.t
val from_func : out_channel -> bytes -> int -> int
val from_special1 : out_channel -> bytes -> string
val from_special2 : out_channel -> bytes -> string
val from_blit : out_channel -> string -> bytes -> int -> int
val paste : out_channel -> string -> bytes -> int -> int
val ifdef_stk : bool Stack.t
