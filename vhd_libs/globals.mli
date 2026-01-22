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

type modtree = {
  tree : Vparser.token;
  symbols : Vparser.shash;
  mutable unresolved : string list;
  mutable is_netlist: bool;
  mutable is_behav: bool;
  mutable is_seq: bool;
  mutable is_hier: bool;
  mutable is_top: bool;
  arch: string;
  comment: string;
  datestamp: float;
}

type prop_t = 
  | Pvar of Vparser.token
  | Pnot of prop_t
  | Pand of prop_t * prop_t 
  | Por of  prop_t * prop_t 
  | Pimp of prop_t * prop_t 
  | Piff of prop_t * prop_t
  | Prime of prop_t
  | Ptrue
  | Pfalse
  | Punknown

type iopin = 
    {
    idpin: Idhash.idhash;
    rngpin: Vparser.token;
  }

type param_t =
    {
    pid: Idhash.idhash;
    prng: Vparser.token;
    dflt: Vparser.token;
  }
      
type mybuft =
    {
    nlen: int;
    nam: Idhash.idhash;
    mutable decl: Vparser.token;
    mutable len: int;
    mutable seq: Vparser.token;
    mutable func: Vparser.token;
    mutable prop: prop_t;
    mutable iolst: Vparser.token list;
    mutable ipinlst: iopin list;
    mutable opinlst: iopin list;
    mutable reglst: iopin list;
    mutable wirlst: iopin list;
    mutable bnam: Idhash.idhash;
    mutable clk: Idhash.idhash;
    mutable clr: Idhash.idhash;
    mutable dat: Idhash.idhash;
    mutable en: Idhash.idhash;
    mutable qout: Idhash.idhash;
    mutable instcnt: int;
    mutable tlst: Vparser.token list;
    mutable instid: Idhash.idhash;
    mutable paramlst: param_t list;
  }

val implicit_params : Idhash.idhash list ref
val implicit_wires : Idhash.idhash list ref
val tmpnam : string

val modprims: (string, modtree) Hashtbl.t
val pending: (string, modtree) Hashtbl.t
val black_box: (string, Idhash.idhash) Hashtbl.t
val libhash : (string, mybuft) Hashtbl.t
val tsymbols : (bytes, Vparser.token) Hashtbl.t

val get_table : Idhash.idhash -> modtree

val unresolved_list : string list ref

val stk : (int * Vparser.token) Stack.t
val logfile : Setup.logt ref
val trace_file : Setup.logt ref
val mygetenv : string -> string
val mygetenv_int : string -> int
val verbose : int
val fatal : bool ref
val archenv : string ref
val modsuffix : string ref
val list_mapi: int -> (int -> 'a -> 'b) -> 'a list -> 'b list
val list_iteri : int -> (int -> 'a -> unit) -> 'a list -> unit
val exists : string -> bool
val unique_open : string -> out_channel
(*
val unresolved_check : Vparser.token -> unit
*)
val log_open : unit -> unit
