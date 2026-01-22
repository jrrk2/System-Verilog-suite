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

val show_token : Vparser.token -> unit
val sym_detail : Vparser.shash -> Vparser.symtab -> unit
val show_sym : string -> Vparser.symtab -> unit
val maxwidth : out_channel -> Vparser.shash -> Vparser.token -> int
val exprBoolean :
  out_channel ->
  Vparser.shash ->
  (Vparser.token -> Vparser.token -> bool) -> Vparser.token -> Vparser.token -> bool
val exprConst : out_channel -> Vparser.shash -> Vparser.token -> Vparser.token
val exprConstStr : out_channel -> Vparser.shash -> Vparser.token -> string
val iwidth :
  out_channel -> Vparser.shash -> Vparser.token -> int * int * int
val exactwidth : out_channel -> Vparser.shash -> Vparser.token -> int
val widthnum' : out_channel -> int -> string -> int*int*string
val widthnum : out_channel -> int -> string -> int*int
val dump_sym : string -> Idhash.idhash -> unit
val dump_syms : string -> unit
val my_syms : string -> unit
val shash_chain_mem : Vparser.shash -> Idhash.idhash -> bool
val shash_chain_find : Vparser.shash -> Idhash.idhash -> Vparser.symtab
val shash_chain_replace : Vparser.shash -> Idhash.idhash -> Vparser.symtab -> unit
val idirection : int -> int -> int
