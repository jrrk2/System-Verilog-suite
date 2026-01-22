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

type exprtree = {
  entry : Vparser.token;
  symbol : Vparser.symtab;
}

type exprt =
    DYADIC of (Vparser.token * exprtree * exprtree)
  | ASSIGNS of (exprtree * exprt)
  | UNHANDLED of Vparser.token

type declmode = Create | SizeOnly | AttrOnly

val enter_a_sym : out_channel -> Vparser.shash -> Idhash.idhash -> Vparser.token -> Vparser.token -> declmode -> unit
val find_ident : out_channel -> Vparser.shash -> Vparser.token -> Vparser.symtab
val not_found : out_channel -> Vparser.shash -> Idhash.idhash -> unit
val connect : out_channel -> Vparser.shash -> Idhash.idhash -> Idhash.idhash -> Vparser.token -> Vparser.token -> unit
val fiter : out_channel -> Vparser.shash -> Idhash.idhash -> Idhash.idhash -> Vparser.token -> Vparser.token -> unit
val enter_sym_attrs : out_channel -> Vparser.shash -> Vparser.token -> Vparser.TokSet.elt list -> Vparser.token -> declmode -> unit
val check_syms : out_channel -> string -> Vparser.shash -> unit
val endscan : unit -> string
val subexp : out_channel -> Vparser.TokSet.elt -> Vparser.shash -> Vparser.token -> unit
val enter_parameter : out_channel -> Vparser.shash -> Idhash.idhash -> Vparser.token -> Vparser.token -> unit
val create_attr : out_channel -> Vparser.shash -> Vparser.token -> Vparser.tsigattr
val stmtBlock : out_channel -> Vparser.shash -> Vparser.TokSet.elt -> unit
val for_stmt : out_channel -> Vparser.shash -> Idhash.idhash -> Vparser.token -> Vparser.token -> Vparser.token -> Vparser.TokSet.elt -> unit
val shash_add : Vparser.shash -> Idhash.idhash -> Vparser.symtab -> unit
val shash_create : string -> string -> Vparser.shash -> int -> Vparser.shash 
val shash_iter : (Idhash.idhash -> Vparser.symtab -> unit) -> Vparser.shash -> unit 
val shash_remove : Vparser.shash -> Idhash.idhash -> unit 
val enter_a_sig_attr : out_channel -> Vparser.shash -> Vparser.token -> Vparser.token -> Vparser.token -> Vparser.shash -> Vparser.symtab -> unit
val sig_attr_extract : out_channel -> Vparser.shash -> Vparser.symtab -> int * int * int * Vparser.tset array
val moditemlist : out_channel -> Globals.modtree -> unit
val scan : out_channel -> string -> Globals.modtree -> unit
val scan' : out_channel -> string -> Globals.modtree -> unit
val init_tree : string list -> string -> Vparser.token -> Vparser.shash -> string -> Globals.modtree
val remove_from_pending : out_channel -> string -> unit
val prescan : out_channel -> string -> Vparser.token -> string -> unit
val chk_inner_attr : out_channel -> Vparser.symtab -> Vparser.TokSet.t array -> Vparser.token -> int -> bool
val enter_range :
  out_channel ->
  Vparser.shash ->
  Idhash.idhash ->
  Vparser.symtab ->
  Vparser.TokSet.elt ->
  Vparser.token ->
  Vparser.symtab ->
  int -> int -> Vparser.TokSet.t array -> Vparser.TokSet.t array -> unit
val senitem : out_channel -> Vparser.shash -> Vparser.TokSet.elt -> unit
val misc_syntax : out_channel -> Vparser.shash -> Vparser.token -> unit
val decls : out_channel -> Globals.modtree -> declmode -> unit
val specifyitems : out_channel -> Vparser.shash -> Vparser.token -> unit
val toplevelitems : out_channel -> Globals.modtree -> unit
val dispatch : out_channel -> Globals.modtree -> bool -> unit
val moditemlist : out_channel -> Globals.modtree -> unit
val has_behav : Vparser.token -> bool
val genthash : Vparser.token list -> (Vparser.token, unit) Hashtbl.t * (Vparser.token, unit) Hashtbl.t
val prev_pending : (string*Globals.modtree) list ref
val unresolved_dir : string ref
val unresolved_ext : string ref
val unresolved_to_parse : (string, unit) Hashtbl.t
val cnt_child' : Vparser.token -> Vparser.token list
val verbose : bool ref
val psl_clk : string ref
(*
val psl_lst : (string*Smv.smv*Vparser.token) list ref
*)
val dbg_left : (Vparser.shash * Vparser.token * int) option ref
val dbg_rght : (Vparser.shash * Vparser.token * int) option ref
