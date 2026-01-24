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

open Idhash
open Vparser

type modtree = {
     tree: token;
     symbols: Vparser.shash;
     mutable unresolved: string list;
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
    rngpin: token;
  }

type param_t =
    {
    pid: Idhash.idhash;
    prng: token;
    dflt: token;
  }
      
type mybuft =
    {
    nlen: int;
    nam: Idhash.idhash;
    mutable decl: token;
    mutable len: int;
    mutable seq: token;
    mutable func: token;
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
    mutable tlst: token list;
    mutable instid: Idhash.idhash;
    mutable paramlst: param_t list;
  }

let (modprims: (string, modtree) Hashtbl.t) = Hashtbl.create 256
let (pending: (string, modtree) Hashtbl.t) = Hashtbl.create 256
let (black_box: (string, Idhash.idhash) Hashtbl.t) = Hashtbl.create 256
let libhash = Hashtbl.create 256

let env_cache = Hashtbl.create 256

let tmpnam = "report."^(string_of_int(Unix.getpid()))^"."^Unix.gethostname()^".report"
let (unresolved_list: string list ref) = ref []
let (stk: (int * Vparser.token) Stack.t) = Stack.create()
let logfile = ref Setup.Closed
let trace_file = ref Setup.Closed

let (implicit_params:Idhash.idhash list ref) = ref []
let (implicit_wires:Idhash.idhash list ref) = ref []

let get_table (m:Idhash.idhash) = Hashtbl.find modprims m.id
let get_syms (r:modtree) = r.symbols;;

let (tsymbols : (bytes, Vparser.token) Hashtbl.t) = Hashtbl.create 256

let mygetenv str = if Hashtbl.mem env_cache str then Hashtbl.find env_cache str else
        try let env = Sys.getenv str in Hashtbl.add env_cache str env; env; with Not_found -> (); ""

let mygetenv_int str = try int_of_string (mygetenv str) with Failure("int_of_string") -> 0

let verbose = mygetenv_int "VCHK_VERBOSE"

let fatal = ref false
let archenv = ref "verilog"
let modsuffix = ref ""

let rec list_mapi i f = function
    [] -> []
  | a::l -> let r = f i a in r :: list_mapi (i + 1) f l

let rec list_iteri i f = function
    [] -> ()
  | a::l -> f i a; list_iteri (i + 1) f l
				   
let exists nam = try ignore(Unix.stat nam); true with Unix.Unix_error _ -> false
let unique_open fullnam' =
  let fullnam = ref fullnam' and changed = ref false in
  while (exists !fullnam) do fullnam := !fullnam^"X"; changed := true done;
  if !changed then
    begin
      Printf.printf "Previous file version changed from %s to %s due to pending over-write\n" fullnam' !fullnam;
      Sys.rename fullnam' !fullnam;
    end;
  open_out fullnam'

(*
let unresolved_check = function
  | ID {Idhash.id=kind} -> if not (Hashtbl.mem modprims kind or Hashtbl.mem libhash kind) then
  begin
  if (List.mem kind !unresolved_list == false) then begin
    unresolved_list := kind :: !unresolved_list
    end
  end
  | _ -> failwith "unexpected argument to unresolved_check"
*)

let log_open () = 
  if (!logfile == Setup.Closed) then
      let fd = open_out tmpnam in
          logfile := Setup.Open (fd) (*,Format.formatter_of_out_channel fd*)
