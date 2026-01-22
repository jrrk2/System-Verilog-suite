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

type uptr = UPTR of (out_channel -> int -> Vparser.token -> unit) | UNIL

val moditer: Buffer.t -> string -> Globals.modtree -> unit
val dump: Buffer.t -> Vparser.token -> int -> unit
val dumpstr: Vparser.token -> string
val dump_module : Buffer.t -> string -> unit
val unhand_list : (int * Vparser.token) list ref
val unhandled_dflt : out_channel -> int -> Vparser.token -> unit
val unhandled_ptr : uptr ref
val unhandled : out_channel -> int -> Vparser.token -> unit
