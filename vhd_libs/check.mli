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

val dotted : string -> bool
val erc_chk_sig : out_channel -> Vparser.tset -> Vparser.TokSet.t -> string -> string
val erc_chk : out_channel -> (string, string list) Hashtbl.t -> (unit->unit) -> Vparser.sentries -> Idhash.idhash -> Vparser.symtab -> unit
val shash_create' : string -> string -> Vparser.shash -> Vparser.sentries -> Vparser.shash 
