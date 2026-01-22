(*
    <vscr - Verilog converter to abc format.>
    Copyright (C) <2011,2012>  <Jonathan Richard Robert Kimmitt>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY || FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

open Const
open Printf
open Idhash
open Vparser
open Globals
open Setup
open Dump

type exprtree = { entry: token; symbol: symtab };;

type exprt =
| DYADIC of (token * exprtree * exprtree)
| ASSIGNS of (exprtree * exprt)
| UNHANDLED of token
;;

type declmode = Create | SizeOnly | AttrOnly

let showmode mode = match mode with
| Create -> "Create"
| SizeOnly -> "SizeOnly"
| AttrOnly -> "AttrOnly"

let verbose = ref false
let mod_empty = ref true
let psl_lst = ref []
let psl_clk = ref ""
let anon = {Vparser.symattr = (TokSet.empty); width=UNKNOWN; sigattr=Sigundef; localsyms=EndShash; path={id=""}};;

(* these functions are for debugging symbol hash related issues *)

let shash_add (syms:shash) key (sym:symtab) = match syms with
| Shash symr -> Hashtbl.add symr.syms key sym
| EndShash -> myfailwith "No symbol table passed to shash_add"

let shash_create ar nm prev (siz:int) = Check.shash_create' ar nm prev (Hashtbl.create siz)

let shash_iter f syms = match syms with
| Shash symr -> Hashtbl.iter f symr.syms
| EndShash -> myfailwith "No symbol table passed to shash_iter"

let shash_remove (syms:shash) key = match syms with
| Shash symr -> Hashtbl.remove symr.syms key
| EndShash -> myfailwith "No symbol table passed to shash_remove"

let find_param out_chan syms id = begin
if Const.shash_chain_mem syms id == false then begin
  ( if List.mem id !implicit_params == false then implicit_params := id :: !implicit_params );
  shash_add syms id {Vparser.symattr = TokSet.singleton PARAMETER;
                       width = SCALAR;
		       sigattr = Sigparam (INT 1);
		       localsyms = syms;
		       path=id}
  end;
Const.shash_chain_find syms id
end
;;

let create_attr out_chan syms neww = 
 Sigarray (Array.make (maxwidth out_chan syms neww) TokSet.empty)

let enter_a_sym out_chan (symbols:shash) id attr w mode =
  (match w with
    | EMPTY -> ()
    | RANGE(INT lft, INT rght) -> if lft < 0 || rght < 0 then
        myfailwith (Printf.sprintf "vector [%d:%d]\n" lft rght)
    | _ -> ());
  match attr with
(IOPORT|INPUT|OUTPUT|INOUT|REG|WIRE|TRI0|TRI1|SUPPLY0|SUPPLY1|INTEGER|REAL|MEMORY|EVENT|TIME
 |MODULE|PRIMITIVE|SUBMODULE|SUBCCT|SPECIFY|SPECIAL|PARAMUSED|FUNCASSIGNED|TASKUSED
 |PARAMETER|TASK|FUNCTION|GENVAR|(DOUBLE(TRI, TLIST _))) ->
if Const.shash_chain_mem symbols id then let found = Const.shash_chain_find symbols id in begin
if (Globals.verbose > 0) then ( Printf.fprintf out_chan "Update %s %s: %s %s\n" id.Idhash.id (Dump.dumpstr w) (Ord.getstr attr) (showmode mode));
  let newset = found.symattr
  and oldw = found.width
  and oldsattr = found.sigattr in
  let notparam = (match oldsattr with Sigparam _ -> false | _ -> true) in
  if (oldw<>UNKNOWN)&&(oldw<>w)&&(w<>UNKNOWN)&&notparam then
    (if (maxwidth out_chan symbols oldw > maxwidth out_chan symbols w) then
	Printf.fprintf out_chan "Addition of attribute %s to signal %s changed width from %s to %s\n"
      (Dump.dumpstr attr) id.Idhash.id (Dump.dumpstr oldw) (Dump.dumpstr w));
  if (w<>UNKNOWN) && (mode<>AttrOnly) && notparam then
  shash_chain_replace symbols id
    {Vparser.symattr = (TokSet.add attr newset);
    width = w;
    sigattr = create_attr out_chan symbols w;
    localsyms = symbols;
    path=id}
  else if notparam then shash_chain_replace symbols id
    {Vparser.symattr = (TokSet.add attr newset);
    width = oldw;
    sigattr = oldsattr;
    localsyms = symbols;
    path=id}
  else shash_chain_replace symbols id
    {Vparser.symattr = newset;
    width = w;
    sigattr = oldsattr;
    localsyms = symbols;
    path=id}
    end
else begin
if (Globals.verbose > 0) then (Printf.fprintf out_chan "Enter %s %s: %s %s\n" id.Idhash.id (Dump.dumpstr w) (Ord.getstr attr) (showmode mode));
  shash_add symbols id {Vparser.symattr = (TokSet.singleton attr);
     width = w;
     sigattr = create_attr out_chan symbols w;
     localsyms = symbols;
     path=id}
  end
| _ -> unhandled out_chan 86 attr
;;

let init_tree un ar x syms com = {Globals.unresolved=un; tree=x; symbols=syms; is_top=true; is_netlist=false; is_hier=false; is_behav=false; is_seq=false; arch=ar; comment=com; datestamp=Unix.gettimeofday()}

let iter_ semantics out_chan syms list =
  List.iter (fun x -> semantics out_chan (init_tree [] "" x syms "")) list
;;

let find_ident out_chan syms tok = match tok with ID id -> begin
if Const.shash_chain_mem syms id then Const.shash_chain_find syms id else (
  ( if List.mem id !implicit_wires == false then implicit_wires := id :: !implicit_wires );
  shash_add syms id {Vparser.symattr = TokSet.singleton IMPLICIT;
                       width = SCALAR;
		       sigattr = create_attr out_chan syms SCALAR;
		       localsyms = syms;
		       path=id};
   Const.shash_chain_find syms id
      )
end
| _ -> unhandled out_chan 118 tok; ({Vparser.symattr = TokSet.singleton tok; width = EMPTY; sigattr = create_attr out_chan syms SCALAR; localsyms = EndShash; path={id=""}})
;;

let not_found out_chan syms w = myfailwith ("wire/port "^w.Idhash.id^" not found")

let enter_sym_attrs out_chan syms (tok:token) list width mode = match tok with
| ID id -> if (Const.shash_chain_mem syms id == false)&&(mode <> Create) then (
          Printf.fprintf out_chan "Signal %s cannot be declared here\n" id.Idhash.id )
  else begin
     List.iter (fun x -> enter_a_sym out_chan syms id x width mode) list;
     let newset = (find_ident out_chan syms tok).symattr in
     if (TokSet.mem INPUT newset) && (TokSet.mem REG newset) && !verbose then 
       Printf.fprintf out_chan "Error: signal %s cannot be input and reg\n" id.Idhash.id
     else if (TokSet.mem INPUT newset) && (TokSet.mem WIRE newset) && !verbose then 
       Printf.fprintf out_chan "Note: input signal %s redundantly declared as wire\n" id.Idhash.id;
    end
| _ -> unhandled out_chan 128 tok;
;;

let enter_parameter out_chan syms id arg6 w =
  if (Globals.verbose > 0) then (Printf.fprintf out_chan "Enter Parameter %s %s: %s\n" id.Idhash.id (Dump.dumpstr w) (exprConstStr out_chan syms arg6));
  shash_add syms id {Vparser.symattr = (TokSet.singleton PARAMETER);
     width = w;
     sigattr = Sigparam arg6;
		       localsyms = syms;
     path=id}

let sig_attr_extract out_chan isyms inner = let rslt0 = (0,0,0,Array.make 1 TokSet.empty) in
let rslt = ref rslt0 in ( match inner.sigattr with
| Sigarray attrs -> (
match inner.width with
| RANGE range -> let (left,right,inc) = iwidth out_chan isyms inner.width in rslt := (left,right,inc,attrs)
| SCALAR ->
    rslt := (0,0,0,attrs);
| UNKNOWN -> (*TBD*)
    rslt := rslt0;
| EMPTY -> (*TBD*)
    rslt := rslt0;
| _ -> unhandled out_chan 98 inner.width)
| Sigparam x -> rslt := rslt0
| Sigundef -> rslt := rslt0
| Sigtask x -> rslt := rslt0
| Sigfunc x -> rslt := rslt0
| Signamed x -> rslt := rslt0); !rslt

let chk_inner_attr out_chan inner inner_attr attr idx = let len = Array.length(inner_attr) in
let retval = (inner.sigattr == Sigundef) || (idx >= len) || (idx < 0) || (TokSet.mem SPECIAL inner.symattr) || (match attr with
      | DRIVER -> TokSet.mem DRIVER (inner_attr.(idx))
      | RECEIVER -> TokSet.mem RECEIVER (inner_attr.(idx))
      | BIDIR -> TokSet.mem BIDIR (inner_attr.(idx))
      | _ -> false) in
if (inner.sigattr <> Sigundef) && (Globals.verbose >= 2) then
    Printf.fprintf out_chan "Accessing %s with index [%d] -> result %s\n" inner.path.Idhash.id idx (string_of_bool retval);
retval
;;

let enter_range out_chan syms id sym attr wid inner high inci inner_attr attrs = let (left,right,inc) = iwidth out_chan syms wid in
  if not ((TokSet.mem IMPLICIT sym.symattr)||(TokSet.mem MEMORY sym.symattr)||(left < 0)||(right < 0)) then
    let i = ref left and j = ref high in while (if inc > 0 then !i <= right else !i >= right) do
    if chk_inner_attr out_chan inner inner_attr attr !j then
        begin
	if (!i < Array.length attrs) then attrs.(!i) <- TokSet.add attr attrs.(!i)
        else Printf.fprintf out_chan "Trying to access %s with index [%d]\n" id.Idhash.id !i
	end;
    i := !i + inc;
    j := !j + inci
    done

let enter_a_sig_attr out_chan syms (tok:token) attr w isyms isym = ( match tok with 
| ID id -> let sym = find_ident out_chan syms tok and (left,right,inc,inner_attr) = sig_attr_extract out_chan isyms isym
 in (match sym.sigattr with
| Sigarray attrs -> enter_range out_chan syms id sym attr w isym left inc inner_attr attrs
| Sigparam x -> enter_sym_attrs out_chan syms tok [PARAMUSED] UNKNOWN AttrOnly
| Sigundef -> Printf.fprintf out_chan "Internal error - Signal %s has no width\n" id.Idhash.id
| Sigtask x -> Printf.fprintf out_chan "Entity %s is already declared as a task\n" id.Idhash.id
| Sigfunc x -> enter_sym_attrs out_chan syms tok [FUNCASSIGNED] UNKNOWN AttrOnly
| Signamed x -> Printf.fprintf out_chan "Entity %s is already declared as a named block\n" id.Idhash.id)
| _ -> unhandled out_chan 175 tok);
 if (Globals.verbose >= 2) then Printf.fprintf out_chan "enter_a_sig_attr out_chan syms tok:%s attr:%s width:%s\n"
  (Dump.dumpstr tok) (Dump.dumpstr attr) (Dump.dumpstr w)

let dbg_left = ref None
let dbg_rght = ref None

let notcompat isyms width1 syms width2 =
  let left = exactwidth stderr isyms width1 in
  let rght = exactwidth stderr syms width2 in
  let compat=ref (left = rght) in
    begin
    match width2 with
    | RANGE(x, y) -> if ((x=y) && ((width1 == EMPTY) || (width1 == SCALAR))) then compat := true;
    | _ -> ();
    match width1 with
    | RANGE(x, y) -> if ((x=y) && ((width2 == EMPTY) || (width2 == SCALAR))) then compat := true;
    | _ -> ();
    end;
    dbg_left := Some (isyms,width1,left);
    dbg_rght := Some (syms,width2,rght);
    not !compat

let inner_chk out_chan syms isyms isym subcct outer wid = begin
 if (Globals.verbose >= 2) then Printf.fprintf out_chan "inner_chk out_chan syms isym {path:%s width:%s} subcct:%s outer:%s width:%s\n"
  isym.path.Idhash.id (Dump.dumpstr isym.width) subcct.Idhash.id outer.Idhash.id (Dump.dumpstr wid);
  let hier = ID (outer) in 
  begin
  if notcompat isyms isym.width syms wid then
      myfailwith (Printf.sprintf "Width mismatch subcct=%s inner=%s %s outer=%s %s\n"
          subcct.Idhash.id isym.path.Idhash.id (Dump.dumpstr isym.width) outer.Idhash.id (Dump.dumpstr(wid)));
  if (TokSet.mem IOPORT isym.symattr == false) then Printf.fprintf out_chan "Instance port %s not an ioport\n" isym.path.Idhash.id
  else if (TokSet.mem INPUT isym.symattr) then ( enter_a_sig_attr out_chan syms hier DRIVER wid isyms isym)
  else if (TokSet.mem OUTPUT isym.symattr) then ( enter_a_sig_attr out_chan syms hier RECEIVER wid isyms isym)
  else if (TokSet.mem INOUT isym.symattr) then ( enter_a_sig_attr out_chan syms hier BIDIR wid isyms isym)
  end
end

let inner_chk_const out_chan syms isyms isym subcct (tok:token) wid = begin
  begin
  if notcompat isyms isym.width syms wid then
      myfailwith (Printf.sprintf "Width mismatch subcct=%s inner=%s %s const=%s %s\n"
          subcct.Idhash.id isym.path.Idhash.id (Dump.dumpstr isym.width) (Dump.dumpstr tok) (Dump.dumpstr(wid)));
  if (TokSet.mem IOPORT isym.symattr == false) then Printf.fprintf out_chan "Instance port %s not an ioport\n" isym.path.Idhash.id
  else if (TokSet.mem INPUT isym.symattr) then ()
  else if (TokSet.mem OUTPUT isym.symattr) then Printf.fprintf out_chan "Output port %s cannot connect to constant\n" isym.path.Idhash.id
  else if (TokSet.mem INOUT isym.symattr) then Printf.fprintf out_chan "Output port %s cannot connect to constant\n" isym.path.Idhash.id
  end
end

let rec inner_chk_expr out_chan syms isyms isym subcct (tok:token) = begin
 let wid = exprGeneric out_chan syms tok in
  begin
  if notcompat isyms isym.width syms wid then
      myfailwith (Printf.sprintf "Width mismatch subcct=%s inner=%s %s expr=%s %s\n"
          subcct.Idhash.id isym.path.Idhash.id (Dump.dumpstr isym.width) (Dump.dumpstr tok) (Dump.dumpstr(wid)));
  if (TokSet.mem IOPORT isym.symattr == false) then Printf.fprintf out_chan "Instance port %s not an ioport\n" isym.path.Idhash.id
  else if (TokSet.mem INPUT isym.symattr) then ()
(*
  else if (TokSet.mem OUTPUT isym.symattr) then Printf.fprintf out_chan "Output port %s cannot connect to expression\n" isym.path
  else if (TokSet.mem INOUT isym.symattr) then Printf.fprintf out_chan "Output port %s cannot connect to expression\n" isym.path
*)
  end
end

and connect_subcct out_chan isyms syms kind (subcct:Idhash.idhash) (innert:token) tok = 
  ( Stack.push (255, innert) stk; match innert with ID innerid -> begin
    if (Const.shash_chain_mem isyms innerid) then
      let isym=Const.shash_chain_find isyms innerid in
      let (fst_irange,snd_irange,inc_irange) = iwidth out_chan isyms isym.width in match tok with
        | ID outer -> inner_chk out_chan syms isyms isym subcct outer (find_ident out_chan syms tok).width
        | TRIPLE(BITSEL, ID outer, sel) -> if (Const.shash_chain_mem syms outer) then inner_chk out_chan syms isyms isym subcct outer (RANGE (sel, sel)) else not_found out_chan syms outer
        | QUADRUPLE(PARTSEL, ID outer, INT hi, INT lo) -> if (Const.shash_chain_mem syms outer) then inner_chk out_chan syms isyms isym subcct outer (RANGE(INT hi, INT lo)) else not_found out_chan syms outer
        | INT lev -> inner_chk_const out_chan syms isyms isym subcct tok (RANGE(INT 31, INT 0))
        | BINNUM lev -> inner_chk_const out_chan syms isyms isym subcct tok (RANGE(INT (fst(widthnum out_chan 2 lev)-1), INT 0))
        | DOUBLE(CONCAT, TLIST concat) ->
          let idx = ref fst_irange in List.iter (fun (item:token) -> 
            ( if (Globals.verbose >= 3) then Printf.fprintf out_chan "Concat idx %d\n" !idx; match item with
              | ID id -> let wid = (find_ident out_chan syms item).width in
                         let (left,right,inc) = iwidth out_chan syms wid in
                         let last = !idx + (right-left)*inc*inc_irange in
                         begin
                           inner_chk out_chan syms isyms {symattr=isym.symattr; width=RANGE(INT !idx, INT last); sigattr = isym.sigattr; localsyms = EndShash; path=isym.path} subcct id wid; idx := last+inc_irange;
                         end
              | TRIPLE(BITSEL, ID id, INT sel) ->
                inner_chk out_chan syms isyms {symattr=isym.symattr; width=RANGE(INT !idx, INT !idx); sigattr = isym.sigattr; localsyms = EndShash; path=isym.path} subcct id (RANGE(INT sel, INT sel)); idx := !idx+inc_irange
              | QUADRUPLE(PARTSEL, ID id, INT left, INT right) ->
                let inc = idirection left right in
                let last = !idx + (right-left)*inc*inc_irange in
                inner_chk out_chan syms isyms {symattr=isym.symattr; width=RANGE(INT !idx, INT last); sigattr = isym.sigattr; localsyms = EndShash; path=isym.path} subcct id (RANGE(INT left, INT right)); idx := last + inc_irange
              | BINNUM lev -> 
                let w = fst(widthnum out_chan 2 lev) in
                inner_chk_const out_chan syms isyms {symattr=isym.symattr; width=RANGE(INT !idx, INT (!idx+1-w)); sigattr = isym.sigattr; localsyms = EndShash; path=isym.path} subcct tok (RANGE(INT (w-1), INT 0)); idx := !idx+inc_irange*w
              | WIDTHNUM(radix,w,num) ->
                inner_chk_const out_chan syms isyms {symattr=isym.symattr; width=RANGE(INT !idx, INT (!idx+1-w)); sigattr = isym.sigattr; localsyms = EndShash; path=isym.path} subcct tok (RANGE(INT (w-1), INT 0)); idx := !idx+inc_irange*w
	      | DOUBLE(CONCAT, TLIST concat') -> Printf.fprintf out_chan "nested concat skipped in %s\n" subcct.Idhash.id
              | _ -> unhandled out_chan 224 item)
          ) concat;
          if (!idx <> snd_irange+inc_irange) then
            Printf.fprintf out_chan "Concatenation width %d does not match port width %s[%d:%d] in %s\n"
              ((!idx - fst_irange)*inc_irange) innerid.Idhash.id fst_irange snd_irange subcct.Idhash.id;
        | _ -> inner_chk_expr out_chan syms isyms isym subcct tok
    else Printf.fprintf out_chan "Instance port %s of %s (type %s) not found\n" innerid.Idhash.id subcct.Idhash.id kind.Idhash.id
  end
    | _ -> unhandled out_chan 229 innert);
  ignore(Stack.pop stk)

and connect_libcell out_chan libcell syms kind subcct (innert:token) tok = 
  match innert with ID innerid -> begin
    if (List.mem innert libcell.iolst) then ()
    else Printf.fprintf out_chan "Instance port %s of %s (type %s) not found\n" innerid.Idhash.id subcct.Idhash.id kind.Idhash.id
  end
    | _ -> unhandled out_chan 229 innert

and connect out_chan syms kind subcct (innert:token) tok =
  if Hashtbl.mem Globals.modprims kind.id then
    let isyms = (Hashtbl.find Globals.modprims kind.id).symbols in
    connect_subcct out_chan isyms syms kind subcct (innert:token) tok
  else if Hashtbl.mem Globals.libhash kind.id then
    let libcell = Hashtbl.find Globals.libhash kind.id in
    connect_libcell out_chan libcell syms kind subcct (innert:token) tok
  else myfailwith ("sub-cell "^kind.Idhash.id^" is not found")

and fiter out_chan syms kind subcct (inner:token) (term:token) = match term with
          | DOUBLE(CELLPIN, myinner) -> ()
          | TRIPLE(CELLPIN, myinner, tok) -> connect out_chan syms kind subcct myinner tok
          (* connect by position syntax - deprecated *)
          | ID id -> connect out_chan syms kind subcct inner term
	  | DOUBLE(CONCAT, TLIST concat) -> connect out_chan syms kind subcct inner term
          | QUADRUPLE(PARTSEL, ID net, INT hi, INT lo) -> connect out_chan syms kind subcct inner term
	  | _ -> connect out_chan syms kind subcct inner (exprGeneric out_chan syms term)

and exprGeneric out_chan syms expr = Stack.push (288, expr) stk; let retval = ref SCALAR in ( match expr with
| TRIPLE( P_OROR, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right;
| TRIPLE( P_ANDAND, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right
| TRIPLE( AND, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right
| TRIPLE( OR, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right
| TRIPLE( P_NAND, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right
| TRIPLE( P_NOR, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right
| TRIPLE( XOR, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right
| TRIPLE( P_XNOR, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right
| TRIPLE( P_EQUAL, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right;
| TRIPLE( P_NOTEQUAL, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right
| TRIPLE( P_CASEEQUAL, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right
| TRIPLE( P_CASENOTEQUAL, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right
| TRIPLE( P_WILDEQUAL, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right
| TRIPLE( P_WILDNOTEQUAL, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right
| TRIPLE( GREATER, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right
| TRIPLE( LESS, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right
| TRIPLE( P_GTE, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right
| TRIPLE( P_LTE, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right
| TRIPLE( P_SLEFT, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right
| TRIPLE( P_SRIGHT, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right
| TRIPLE( P_SSRIGHT, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right
| TRIPLE( PLUS, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right
| TRIPLE( MINUS, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right
| TRIPLE( TIMES, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right
| TRIPLE( DIVIDE, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right
| TRIPLE( MODULO, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right
| TRIPLE( P_POW, left, right ) -> retval := exprGeneric out_chan syms left; retval := exprGeneric out_chan syms right
| TRIPLE(CONCAT, arg2, TLIST arg4) -> List.iter (fun arg -> retval := exprGeneric out_chan syms arg) (arg2::arg4)
| DOUBLE(LPAREN, right ) -> retval := exprGeneric out_chan syms right
| DOUBLE(CONCAT, TLIST concat) -> List.iter (fun arg -> retval := exprGeneric out_chan syms arg) concat
| DOUBLE(MINUS, arg2) -> retval := exprGeneric out_chan syms arg2
| DOUBLE(PLUS, arg2) -> retval := exprGeneric out_chan syms arg2
| DOUBLE(AND, arg2) -> retval := exprGeneric out_chan syms arg2
| DOUBLE(OR, arg2) -> retval := exprGeneric out_chan syms arg2
| DOUBLE(XOR, arg2) -> retval := exprGeneric out_chan syms arg2
| DOUBLE(P_XNOR, arg2) -> retval := exprGeneric out_chan syms arg2
| DOUBLE(P_NAND, arg2) -> retval := exprGeneric out_chan syms arg2
| DOUBLE(P_NOR, arg2) -> retval := exprGeneric out_chan syms arg2
| DOUBLE(PLING, arg2) -> retval := exprGeneric out_chan syms arg2
| DOUBLE(NOT, arg2) -> retval := exprGeneric out_chan syms arg2
| QUADRUPLE(QUERY, expr, then_clause, else_clause ) ->
    ( ignore(exprGeneric out_chan syms expr);
    let retval1 = exprGeneric out_chan syms then_clause in
    let retval2 = exprGeneric out_chan syms else_clause in
    let (left1,right1,inc1) = iwidth out_chan syms retval1 and (left2,right2,inc2) = iwidth out_chan syms retval2 in
      retval := if ((right1-left1)*inc1 > (right2-left2)*inc2) then retval1 else retval2; )
| DOUBLE(D_BITS, right ) -> retval := exprGeneric out_chan syms right
| DOUBLE(D_C, TLIST right ) -> List.iter(fun arg -> retval := exprGeneric out_chan syms arg) right
| DOUBLE(D_CLOG2, right ) -> retval := exprGeneric out_chan syms right
| DOUBLE(D_COUNTDRIVERS, right ) -> retval := exprGeneric out_chan syms right
| DOUBLE(D_COUNTONES, right ) -> retval := exprGeneric out_chan syms right
| DOUBLE(D_FEOF, right ) -> retval := exprGeneric out_chan syms right
| DOUBLE(D_FGETC, right ) -> retval := exprGeneric out_chan syms right
| TRIPLE(D_FGETS, right, arg5) -> retval := exprGeneric out_chan syms right
| TRIPLE(D_FSCANF, right, arg6) -> retval := exprGeneric out_chan syms right
| TRIPLE(D_SSCANF, right, arg6) -> retval := exprGeneric out_chan syms right
| DOUBLE(D_ISUNKNOWN, right ) -> retval := exprGeneric out_chan syms right
| DOUBLE(D_ONEHOT, right ) -> retval := exprGeneric out_chan syms right
| DOUBLE(D_ONEHOT0, right ) -> retval := exprGeneric out_chan syms right
| DOUBLE(D_RANDOM, right ) -> retval := exprGeneric out_chan syms right
| D_RANDOM -> retval := RANGE(INT 31, INT 0)
| DOUBLE(D_SIGNED, right ) -> retval := exprGeneric out_chan syms right
| D_STIME -> retval := RANGE(INT 31, INT 0)
| D_TIME -> retval := RANGE(INT 31, INT 0)
| DOUBLE(D_TEST_PLUSARGS, right ) -> retval := exprGeneric out_chan syms right
| DOUBLE(D_UNSIGNED, right ) -> retval := exprGeneric out_chan syms right
| TRIPLE(FUNCREF, func, TLIST arg3) -> ( match func with
  | ID funcname ->
  begin
    if (Const.shash_chain_mem syms funcname) then let h = Const.shash_chain_find syms funcname in match h.sigattr with
      | Sigfunc fnc -> dispatch out_chan (init_tree [] "" fnc h.localsyms "") true
      | _ -> Printf.fprintf out_chan "Trying to evaluate non function %s\n" funcname.Idhash.id
    else Printf.fprintf out_chan "Function %s not found\n" funcname.Idhash.id;
  end
  | DOTTED path -> List.iter (fun name -> match name with ID id -> Printf.fprintf out_chan "%s." id.Idhash.id | _ -> ()) path;
    Printf.fprintf out_chan " - hierarchical function etc. not (yet) supported\n"
  | _ -> ()
 )
| INT left -> ()
| BINNUM left -> ()
| DECNUM left -> ()
| HEXNUM left -> ()
| ID arg1 -> retval := (find_ident out_chan syms expr).width; enter_a_sig_attr out_chan syms expr DRIVER !retval syms anon
| TRIPLE(BITSEL, ID arg1, arg3) -> retval := RANGE(INT 0, INT 0);
    enter_a_sig_attr out_chan syms (ID arg1) DRIVER UNKNOWN syms anon
| TRIPLE (BITSEL, TRIPLE (BITSEL, ID arg1, sel1), sel2) -> enter_a_sig_attr out_chan syms (ID arg1) DRIVER UNKNOWN syms anon (* This looks like an array bit select *)
| QUADRUPLE(PARTSEL, ID arg1 , arg3 , arg5 ) -> retval := RANGE(arg3,arg5); enter_a_sig_attr out_chan syms (ID arg1) DRIVER !retval syms anon
| QUADRUPLE(PARTSEL, TRIPLE (BITSEL, ID arg1, sel1), left, right) -> enter_a_sig_attr out_chan syms (ID arg1) DRIVER UNKNOWN syms anon (* This looks like an array part select *)
| QUADRUPLE(P_PLUSCOLON, arg1 , arg3 , arg5 ) -> ()
| QUADRUPLE(P_MINUSCOLON, arg1, arg3, arg5 ) -> ()
| ASCNUM arg1 -> ()
| FLOATNUM arg1 -> ()
| DOTTED path -> (*TBD*) retval := UNKNOWN
| SCALAR -> (*TBD*) retval := SCALAR
| WIDTHNUM(radix,w,num) -> retval := RANGE(INT (w-1), INT 0)
| _ -> unhandled out_chan 321 expr; retval := UNKNOWN );
ignore(Stack.pop stk);
!retval

and caseitems out_chan syms expr = Stack.push (369, expr) stk; ( match expr with
(* Parse case statement *)
| TRIPLE(CASECOND, TLIST thecases, stmt) ->
List.iter (fun pattern -> ignore(exprGeneric out_chan syms pattern)) thecases;
stmtBlock out_chan syms stmt
| DOUBLE(DEFAULT, stmt) ->
stmtBlock out_chan syms stmt
| QUADRUPLE(P_LTE, dest, dly, exp) ->
ignore(exprGeneric out_chan syms exp);
ignore(subexp out_chan RECEIVER syms dest)
| ID id -> enter_a_sym out_chan syms id EMPTY EMPTY AttrOnly
| PREPROC txt -> ()
| EMPTY -> ()
| _ -> unhandled out_chan 417 expr );
ignore(Stack.pop stk)

and for_stmt out_chan syms id start test inc clause = let wid = (find_ident out_chan syms (ID id)).width and crnt = ref (exprConst out_chan syms start) in begin
Printf.fprintf out_chan "Begin for %s statement\n" id.Idhash.id;
  shash_add syms id {Vparser.symattr = (TokSet.singleton PARAMETER);
     width = wid;
     sigattr = Sigparam !crnt;
		       localsyms = syms;
     path=id};
let loops = ref 0 and unrolling = ref true in while (!unrolling) && (exprBoolean out_chan syms (<>) test (INT 0)) do
    stmtBlock out_chan syms clause;
    crnt := exprConst out_chan syms inc;
    shash_chain_replace syms id
      {Vparser.symattr = (TokSet.singleton PARAMETER);
      width = wid;
      sigattr = Sigparam !crnt;
		       localsyms = syms;
      path=id};
    loops := 1 + !loops;
    if (!loops > 1000) then
      (unrolling := false; Printf.fprintf out_chan "Loop %s unrolling stopped after 1000 iterations\n" id.Idhash.id)
done;
shash_remove syms id;
Printf.fprintf out_chan "End for %s statement\n" id.Idhash.id

end 

and hash_dly out_chan syms dly = match dly with
  | EMPTY -> ()
  | DOUBLE(HASH, ID dlytok) -> enter_sym_attrs out_chan syms (ID dlytok) [PARAMUSED] UNKNOWN AttrOnly
  | DOUBLE(HASH, TLIST dlylist) -> List.iter (fun item -> match item with 
        | ID _ -> enter_sym_attrs out_chan syms item [PARAMUSED] UNKNOWN AttrOnly
        | INT dly -> ()
        | _ -> unhandled out_chan 408 item) dlylist
  | DOUBLE(HASH, FLOATNUM num) -> ()
  | TLIST [WEAK weak0; WEAK weak1] -> ()
  | _ -> unhandled out_chan 493 dly

and stmtBlock out_chan syms block = Stack.push (465, block) stk; ( match block with
| EMPTY -> ()
| DOUBLE(DISABLE, nam) -> ()
| DOUBLE(P_MINUSGT, ev) -> ()
| DOUBLE(DOUBLE(HASH, _) as dly, stmt) -> hash_dly out_chan syms dly; stmtBlock out_chan syms stmt
| QUINTUPLE(NAMED, ID blk_named, TLIST loc_decls, TLIST stmts, EMPTY) ->
  let syms2 = shash_create "named" blk_named.Idhash.id syms 256 in
  shash_add syms blk_named {Vparser.symattr = TokSet.singleton NAMED;
                       width = VOID;
		       sigattr = Signamed block;
		       localsyms = syms2;
		       path=blk_named};
  List.iter (fun item -> decls out_chan (init_tree [] "" item syms2 "") Create) loc_decls;
  List.iter (fun item -> stmtBlock out_chan syms2 item) stmts
| QUADRUPLE(GENITEM, ID blk_named, TLIST stmts, EMPTY) ->
  let syms2 = shash_create "genitem" blk_named.Idhash.id syms 256 in
  shash_add syms blk_named {Vparser.symattr = TokSet.singleton GENITEM;
                       width = VOID;
		       sigattr = Signamed block;
		       localsyms = syms2;
		       path=blk_named};
  List.iter (fun item -> toplevelitems out_chan (init_tree [] "" item syms2 "")) stmts
| TLIST stmtList -> List.iter (fun item ->
    stmtBlock out_chan syms item) stmtList
| TRIPLE(BEGIN, TLIST stmtList, endLabelE) -> List.iter (fun item -> stmtBlock out_chan syms item) stmtList
| DOUBLE(AT, TLIST stmtList) -> List.iter (fun item -> stmtBlock out_chan syms item) stmtList
| DOUBLE(AT, DOUBLE(DOUBLE(HASH, _) as dly, stmt)) -> hash_dly out_chan syms dly; stmtBlock out_chan syms stmt
| DOUBLE(DOUBLE(AT, TLIST sens_list), stmt)
-> List.iter (fun item -> senitem out_chan syms item) sens_list; ( match stmt with
  | TRIPLE(BEGIN, TLIST stmts, EMPTY)
    -> stmtBlock out_chan syms stmt
  | TLIST stmts
    -> stmtBlock out_chan syms stmt
  | _ -> stmtBlock out_chan syms stmt )
| QUADRUPLE((ASSIGNMENT|DLYASSIGNMENT), var1, dly, expr)
-> ignore(subexp out_chan RECEIVER syms var1);
   ignore(exprGeneric out_chan syms expr);
   hash_dly out_chan syms dly
| QUADRUPLE(ASSIGN, ID id, EMPTY, expr) ->
   ignore(subexp out_chan RECEIVER syms (ID id));
   ignore(exprGeneric out_chan syms expr)
| DOUBLE(DEASSIGN, ID id) ->
   ignore(subexp out_chan RECEIVER syms (ID id))
| DOUBLE(FOREVER, stmt) ->
   stmtBlock out_chan syms stmt
| TRIPLE(REPEAT, iter, stmt) ->
   stmtBlock out_chan syms stmt
| TRIPLE(WHILE, expr, stmt) ->
   ignore(exprGeneric out_chan syms expr);
   stmtBlock out_chan syms stmt
| TRIPLE(IF, expr, then_clause) ->
ignore(exprGeneric out_chan syms expr);
stmtBlock out_chan syms then_clause
| QUADRUPLE(IF, expr, then_clause, else_clause) ->
ignore(exprGeneric out_chan syms expr);
stmtBlock out_chan syms then_clause;
stmtBlock out_chan syms else_clause
| QUINTUPLE(FOR, TRIPLE(ASSIGNMENT,ID idstart, start), test, TRIPLE(ASSIGNMENT,ID idinc,inc), clause) ->
if idstart <> idinc then Printf.fprintf out_chan "For variable not consistent %s vs. %s\n" idstart.Idhash.id idinc.Idhash.id
else for_stmt out_chan syms idstart start test inc clause;
| QUADRUPLE((CASE|CASEX|CASEZ), expr, caseAttr, TLIST caseList) ->
ignore(exprGeneric out_chan syms expr);
List.iter (fun caseitem -> caseitems out_chan syms caseitem) caseList
(* Parse task reference *)
| TRIPLE(TASKREF, task, args) -> ( match task with
  | ID taskname ->
  begin
    if (Const.shash_chain_mem syms taskname) then let h = Const.shash_chain_find syms taskname in match h.sigattr with
      | Sigtask tsk -> dispatch out_chan (init_tree [] "" tsk h.localsyms "") true; (* scan the task *)
        enter_a_sym out_chan syms taskname TASKUSED UNKNOWN AttrOnly
      | _ -> Printf.fprintf out_chan "Trying to call non task %s\n" taskname.Idhash.id
    else Printf.fprintf out_chan "Task %s not found\n" taskname.Idhash.id;
end
  | DOTTED path -> List.iter (fun name -> match name with ID id -> Printf.fprintf out_chan "%s." id.Idhash.id | _ -> ()) path;
    Printf.fprintf out_chan " - hierarchical task etc. not (yet) supported\n"
  | _ -> ()
 )
(*
| TRIPLE((D_READMEMB|D_READMEMH), (ASCNUM file|ID file), args) -> ()
*)
| TRIPLE(D_FOPEN, dest, nam ) -> ignore(subexp out_chan RECEIVER syms dest);
    ignore(exprGeneric out_chan syms nam)
| TRIPLE(D_WRITE, ASCNUM msg, args) -> ()
| TRIPLE((D_FDISPLAY|D_FWRITE|D_FWRITEH), fd, TLIST args) -> List.iter (fun arg -> ignore(exprGeneric out_chan syms arg)) args
| DOUBLE(D_FCLOSE, fd) -> ignore(exprGeneric out_chan syms fd)
| QUADRUPLE(D_FDISPLAY, fd, ASCNUM msg, args) -> ()
| TRIPLE(D_FDISPLAY, fd, EMPTY) -> ()
| TRIPLE(D_DISPLAY, ASCNUM msg, args) -> ()
| DOUBLE(D_DISPLAY, EMPTY) -> ()
| DOUBLE(D_MONITOR, TLIST args) -> ()
| DOUBLE(D_STOP, EMPTY) -> ()
| DOUBLE(D_FINISH, EMPTY) -> ()
| _ -> unhandled out_chan 468 block );
ignore(Stack.pop stk)

and subexp out_chan dir syms exp = Stack.push (475, exp) stk; match exp with
| ID id -> enter_a_sig_attr out_chan syms exp dir (find_ident out_chan syms exp).width syms anon
| TRIPLE(BITSEL, ID id, sel) -> enter_a_sig_attr out_chan syms (ID id) dir (RANGE (sel, sel)) syms anon
| _ -> ignore(exprGeneric out_chan syms exp);
ignore(Stack.pop stk)
(*
and expr_dyadic out_chan dir syms op left right = DYADIC(op, subexp2 out_chan dir syms left, subexp2 out_chan dir syms right)
*)
and vtran out_chan syms x y =
List.iter(fun x -> ignore(subexp out_chan RECEIVER syms x)) [x;y]

and vtranif out_chan syms x y z =
List.iter(fun x -> ignore(subexp out_chan RECEIVER syms x)) [x;y;z]

and vnmos out_chan syms x y z =
ignore(subexp out_chan RECEIVER syms x);
List.iter(fun x -> ignore(subexp out_chan DRIVER syms x)) [y;z]

and vpmos out_chan syms x y z =
ignore(subexp out_chan RECEIVER syms x);
List.iter(fun x -> ignore(subexp out_chan DRIVER syms x)) [y;z]

and vpullup out_chan syms (x:token) =
ignore(subexp out_chan RECEIVER syms x)

and vbuf out_chan syms (x:token) a =
ignore(subexp out_chan RECEIVER syms x);
ignore(subexp out_chan DRIVER syms a)

and vbufif out_chan syms (x:token) (inlist:token list) =
ignore(subexp out_chan RECEIVER syms x);
List.iter (fun t -> ignore(subexp out_chan DRIVER syms t)) inlist

and vnotif out_chan syms (x:token) (inlist:token list) =
ignore(subexp out_chan RECEIVER syms x);
List.iter (fun t -> ignore(subexp out_chan DRIVER syms t)) inlist

and vnot out_chan syms (x:token) a =
ignore(subexp out_chan RECEIVER syms x);
List.iter (fun w -> ignore(subexp out_chan DRIVER syms w)) [x;a]

and vand out_chan syms (x:token) (inlist:token list) =
ignore(subexp out_chan RECEIVER syms x);
List.iter (fun t -> ignore(subexp out_chan DRIVER syms t)) inlist

and vor out_chan syms (x:token) (inlist:token list) =
ignore(subexp out_chan RECEIVER syms x);
List.iter (fun t -> ignore(subexp out_chan DRIVER syms t)) inlist

and vxor out_chan syms (x:token) (inlist:token list) =
ignore(subexp out_chan RECEIVER syms x);
List.iter (fun t -> ignore(subexp out_chan DRIVER syms t)) inlist

and vnand out_chan syms (x:token) (inlist:token list) =
ignore(subexp out_chan RECEIVER syms x);
List.iter (fun t -> ignore(subexp out_chan DRIVER syms t)) inlist

and vnor out_chan syms (x:token) (inlist:token list) =
ignore(subexp out_chan RECEIVER syms x);
List.iter (fun t -> ignore(subexp out_chan DRIVER syms t)) inlist

and vxnor out_chan syms (x:token) (inlist:token list) =
ignore(subexp out_chan RECEIVER syms x);
List.iter (fun t -> ignore(subexp out_chan DRIVER syms t)) inlist

and senitem out_chan syms item = match item with
| DOUBLE(POSEDGE, clk) -> ignore(subexp out_chan DRIVER syms clk)
| DOUBLE(NEGEDGE, clk) -> ignore(subexp out_chan DRIVER syms clk)
| ID signal -> ignore(subexp out_chan SENSUSED syms item)
| TRIPLE (BITSEL, ID memory, sel) -> ignore(subexp out_chan DRIVER syms sel)
| _ -> unhandled out_chan 490 item

and misc_syntax out_chan syms = function
| TLIST lst -> List.iter (function
  | SEXTUPLE (PARAMETER, signing, range, ID id, attr, arg6) -> enter_parameter out_chan syms id arg6 EMPTY
  | oth -> unhandled out_chan 682 oth) lst
| EMPTY -> ()
| expr -> unhandled out_chan 684 expr

and decls out_chan tree mode =
   let expr = tree.Globals.tree and syms = tree.Globals.symbols in Stack.push (539, expr) stk; ( match expr with
(* Parse parameter declarations *)
| SEXTUPLE((PARAMETER|LOCALPARAM), signing, range, ID id, arg5, arg6) ->
    let width = ref EMPTY in
    (match range with
      | RANGE(left,right) as rangehilo -> width := rangehilo
      | TLIST arg9 ->  List.iter (fun arg -> unhandled out_chan 659 arg) arg9
      | EMPTY -> ()
      | _ -> unhandled out_chan 661 range);
    enter_parameter out_chan syms id arg6 !width
| DOUBLE(DEFPARAM, TLIST lst) ->
  List.iter (function
    | TRIPLE(ID id1, ID id2, INT num) -> fprintf stderr "Ignoring defparam %s.%s = %d\n" id1.id id2.id num
    | oth -> unhandled out_chan 696 oth) lst
(* Parse IO declarations *)
| QUINTUPLE((INPUT|OUTPUT|INOUT) as dir, arg1, arg2, arg3, arg4) ->
    let width = ref SCALAR and attr = ref [IOPORT;dir] in begin
    (match arg1 with
      | EMPTY -> ()
      | REG -> attr := REG::!attr
      | SUPPLY0 -> attr := SUPPLY0::!attr
      | SUPPLY1 -> attr := SUPPLY1::!attr
      | _ -> unhandled out_chan 558 arg1);
    misc_syntax out_chan syms arg2;
    (match arg3 with
      | RANGE(left,right) as rangehilo -> width := rangehilo
      | TLIST arg9 ->  List.iter (fun arg -> unhandled out_chan 508 arg) arg9
      | EMPTY -> ()
      | _ -> unhandled out_chan 510 arg3);
    ( match arg4 with
      | DOUBLE(id, arg5) -> enter_sym_attrs out_chan syms id !attr !width mode
      | TRIPLE(id, TLIST arg5, TLIST arg6) -> enter_sym_attrs out_chan syms id !attr !width mode
      | TLIST arg9 ->  List.iter (fun x -> match x with
        | TRIPLE(id, arg5, arg6) -> enter_sym_attrs out_chan syms id !attr !width mode
        | _ -> unhandled out_chan 514 x) arg9
      | EMPTY -> ()
      | _ -> unhandled out_chan 516 arg4); end
(* Parse wire/reg declarations *)
| QUADRUPLE((WIRE|REG|TRI0|TRI1|SUPPLY0|SUPPLY1|DOUBLE(TRI,_)) as kind, arg1, arg2, TLIST arg3) ->
    let width = ref SCALAR in begin
    misc_syntax out_chan syms arg1;
    (match arg2 with
      | RANGE(left, right) as rangehilo -> width := rangehilo
      | TRIPLE(EMPTY,(RANGE(left, right) as rangehilo),EMPTY) -> width := rangehilo
      | TLIST arg7 ->  List.iter (fun arg -> unhandled out_chan 524 arg) arg7
      | EMPTY -> ()
      | TRIPLE(EMPTY,EMPTY,EMPTY) -> ()
      | _ ->  unhandled out_chan 527 arg2);
    ( List.iter (fun x -> match x with
      | TRIPLE(ID id, arg5, arg6) -> (match arg5 with
          | EMPTY ->
              enter_sym_attrs out_chan syms (ID id) [kind] !width Create;
              enter_a_sig_attr out_chan syms (ID id) RECEIVER !width syms anon
          | TLIST [RANGE (expr1, expr2)] ->
              enter_sym_attrs out_chan syms (ID id) [MEMORY] !width Create;
          | _ -> unhandled out_chan 582 arg5);
          if (arg6 <> EMPTY) then ignore(exprGeneric out_chan syms arg6);
      | DOUBLE(id, anno) -> enter_sym_attrs out_chan syms id [kind] !width Create
      | QUADRUPLE(id, EMPTY, init, EMPTY) -> enter_sym_attrs out_chan syms id [kind] !width Create;
         Printf.fprintf out_chan "Initialisation %s of register %s discarded\n" (dumpstr init) (dumpstr id)
      | _ -> unhandled out_chan 534 (DOUBLE(expr,x))) arg3); end
(* Parse real/integer/event decls *)
| QUADRUPLE((REAL|INTEGER|EVENT|TIME) as kind, arg1, arg2, TLIST arg3) ->
    misc_syntax out_chan syms arg1;
    (match arg2 with
      | EMPTY -> ()
      | TRIPLE(EMPTY,EMPTY,EMPTY) -> ()
      | _ ->  unhandled out_chan 744 arg2);
    ( List.iter (fun x -> match x with
      | TRIPLE(id, arg5, arg6) -> enter_sym_attrs out_chan syms id [kind] SCALAR Create
      | _ -> unhandled out_chan 747 x) arg3)
(* Parse genvar decls *)
| TRIPLE((GENVAR) as kind, arg1, TLIST arg3) ->
    misc_syntax out_chan syms arg1;
    ( List.iter (fun x -> match x with
      | TRIPLE(id, arg5, arg6) -> enter_sym_attrs out_chan syms id [kind] SCALAR Create
      | _ -> unhandled out_chan 753 x) arg3)
(*
| QUADRUPLE(TIME, EMPTY, EMPTY, TLIST loclst) ->
    List.iter (function
      | TRIPLE (ID time, EMPTY, EMPTY) -> ()
      | oth -> unhandled out_chan 757 oth ) loclst
*)
| _ -> unhandled out_chan 759 expr );
ignore(Stack.pop stk)

and specifyitems out_chan syms = function
  | ID id -> enter_a_sym out_chan syms id SPECIFY UNKNOWN AttrOnly
  | _ -> ()

and conmodinst out_chan syms kind params instances tree is_input = 
      List.iter (fun inst -> match inst with
        | TRIPLE(ID subcct, SCALAR, TLIST termlist) -> (* semantics out_chan (stem^subcct^".") kindhash; *)
          enter_a_sym out_chan syms subcct SUBCCT EMPTY Create;
          ( match tree with QUINTUPLE((MODULE|PRIMITIVE),ID arg1, params, TLIST primargs, THASH targ4) ->
            ( match params with
              | EMPTY -> ()
	      | TLIST plst -> List.iter
                (fun item -> decls out_chan (init_tree [] "" item syms "") Create) plst
              | _ -> unhandled out_chan 904 params);
            (try
               List.iter2 (fun (inner:token) (term:token) -> fiter out_chan syms kind subcct inner term) primargs termlist;
             with Invalid_argument "List.iter2" -> 
               let ids = ref [] and partlist = ref ([],[]) and byposn = ref false in
               begin
                 List.iter (fun (inner:token) -> (match inner with
                   | ID id -> ids := (!ids @ [id])
                   | QUINTUPLE ((INPUT|OUTPUT|INOUT), (EMPTY|REG), EMPTY, range, DOUBLE(ID id, EMPTY)) -> ids := (!ids @ [id])
                   | _ -> unhandled out_chan 648 inner)) primargs;
                 let primstr = !ids in try List.iter2 (fun innert (term:token) -> (match term with
                   | DOUBLE(CELLPIN, ID innern) -> () (*Explicitly unconnected pin*)
                   | TRIPLE(CELLPIN, ID innern, tok) -> ids := List.filter (fun item -> item<>innern) !ids
                   | _ -> byposn := true; ids := List.filter (fun item -> item<>innert) !ids)) primstr termlist;
                   with Invalid_argument "List.iter2" -> ();
                     if (!byposn) then begin
                       Printf.fprintf out_chan "sub-module %s of kind %s deprecated connect by position - %d unconnected pins(s) - might be " subcct.Idhash.id kind.Idhash.id (List.length (!ids));
                       List.iter (fun id -> Printf.fprintf out_chan "%s " id.Idhash.id) (!ids);
                       output_char out_chan '\n';
                     end;
(* Find which of the unconnected pins are inputs *)
                     partlist := List.partition is_input !ids;
                     if (List.length (fst(!partlist)) > 0) then begin
                       Printf.fprintf out_chan "sub-module %s of kind %s insufficient args - %d unconnected inputs(s): " subcct.Idhash.id kind.Idhash.id (List.length (fst(!partlist)));
                       List.iter (fun id -> Printf.fprintf out_chan "%s " id.Idhash.id) (fst(!partlist));
                       output_char out_chan '\n';
                     end
               end)
            | EMPTY -> Printf.fprintf out_chan "Tree %s is empty\n" kind.id
            | _ -> unhandled out_chan 762 tree)
        | _ -> unhandled out_chan 763 inst) instances;
      match params with
        | EMPTY -> ()
        | TLIST parmlist -> List.iter (fun param -> match param with ID id -> () | _ -> unhandled out_chan 717 param) parmlist
        | DOUBLE (HASH, ID parm) -> ()
        | DOUBLE (HASH, TLIST dlylist) -> List.iter (fun param -> match param with
            | ID id -> ()
            | INT n -> ()
            | TRIPLE (CELLPIN, ID id1, ID id2) -> ()
            | TRIPLE (CELLPIN, ID id1, BINNUM num2) -> ()
	    | TRIPLE (CELLPIN, ID id1, ASCNUM ascii_param) -> ()
	    | TRIPLE (CELLPIN, ID id1, HEXNUM hex_param) -> ()
            | _ -> ignore(exprConst out_chan syms param)) dlylist
        | _ -> unhandled out_chan 719 params

and toplevelitems out_chan tree =
   let expr = tree.Globals.tree and syms = tree.Globals.symbols in Stack.push (595, expr) stk; ( match expr with
| DOUBLE((INITIAL|FINAL|ALWAYS|GENERATE), stmt) -> stmtBlock out_chan syms stmt
| TRIPLE(ASSIGN, dly, TLIST assignlist) -> hash_dly out_chan syms dly;
    List.iter (function
      | TRIPLE(ASSIGNMENT, var1, expr) ->
        ignore(subexp out_chan RECEIVER syms var1);
        ignore(exprGeneric out_chan syms expr)
      | err -> unhandled out_chan 560 err) assignlist
| TRIPLE(BUF, dly, TLIST instances) -> hash_dly out_chan syms dly;
    List.iter (fun inst -> match inst with
      | QUADRUPLE(nam, SCALAR, x, a) -> vbuf out_chan syms x a
      | QUADRUPLE(nam, RANGE(INT hi, INT lo), x, a) -> vbuf out_chan syms x a
      | _ -> unhandled out_chan 564 inst) instances
| TRIPLE(NOT,dly, TLIST instances) -> hash_dly out_chan syms dly;
    List.iter (fun inst -> match inst with
      | QUADRUPLE(nam, SCALAR, x, a) -> vnot out_chan syms x a
      | _ -> unhandled out_chan 568 inst) instances
| TRIPLE(AND, dly, TLIST instances) -> hash_dly out_chan syms dly;
    List.iter (fun inst -> match inst with
      | QUADRUPLE(nam, SCALAR, x, TLIST inlist) -> vand out_chan syms x inlist
      | _ -> unhandled out_chan 572 inst) instances
| TRIPLE(OR, dly, TLIST instances) -> hash_dly out_chan syms dly;
    List.iter (fun inst -> match inst with
      | QUADRUPLE(nam, SCALAR, x, TLIST inlist) -> vor out_chan syms x inlist
      | _ -> unhandled out_chan 576 inst) instances
| TRIPLE(XOR, dly, TLIST instances) -> hash_dly out_chan syms dly;
    List.iter (fun inst -> match inst with
      | QUADRUPLE(nam, SCALAR, x, TLIST inlist) -> vxor out_chan syms x inlist
      | _ -> unhandled out_chan 580 inst) instances
| TRIPLE(NAND, dly, TLIST instances) -> hash_dly out_chan syms dly;
    List.iter (fun inst -> match inst with
      | QUADRUPLE(nam, SCALAR, x, TLIST inlist) -> vand out_chan syms x inlist
      | _ -> unhandled out_chan 584 inst) instances
| TRIPLE(NOR, dly, TLIST instances) -> hash_dly out_chan syms dly;
    List.iter (fun inst -> match inst with
      | QUADRUPLE(nam, SCALAR, x, TLIST inlist) -> vor out_chan syms x inlist
      | _ -> unhandled out_chan 588 inst) instances
| TRIPLE(XNOR, dly, TLIST instances) -> hash_dly out_chan syms dly;
    List.iter (fun inst -> match inst with
      | QUADRUPLE(nam, SCALAR, x, TLIST inlist) -> vxor out_chan syms x inlist
      | _ -> unhandled out_chan 592 inst) instances
| TRIPLE(BUFIF lev, dly, TLIST instances) -> hash_dly out_chan syms dly;
    List.iter (fun inst -> match inst with
      | QUADRUPLE(nam, SCALAR, x, TLIST inlist) -> vbufif out_chan syms x inlist
      | _ -> unhandled out_chan 596 inst) instances
| TRIPLE(NOTIF lev, dly, TLIST instances) -> hash_dly out_chan syms dly;
    List.iter (fun inst -> match inst with
      | QUADRUPLE(nam, SCALAR, x, TLIST inlist) -> vnotif out_chan syms x inlist
      | _ -> unhandled out_chan 596 inst) instances
| TRIPLE(PULLUP, dly, TLIST instances) -> hash_dly out_chan syms dly;
    List.iter (fun inst -> match inst with
      | QUADRUPLE(nam, EMPTY, EMPTY, x) -> vpullup out_chan syms x
      | _ -> unhandled out_chan 600 inst) instances
| TRIPLE(NMOS, dly, TLIST instances) -> hash_dly out_chan syms dly;
    List.iter (fun inst -> match inst with
      | QUINTUPLE(nam, SCALAR, pin1, pin2, pin3) -> vnmos out_chan syms pin1 pin2 pin3
      | _ -> unhandled out_chan 604 inst) instances
| TRIPLE(PMOS, dly, TLIST instances) -> hash_dly out_chan syms dly;
    List.iter (fun inst -> match inst with
      | QUINTUPLE(nam, SCALAR, pin1, pin2, pin3) -> vpmos out_chan syms pin1 pin2 pin3
      | _ -> unhandled out_chan 608 inst) instances
| TRIPLE(TRANIF lev, dly, TLIST instances) -> hash_dly out_chan syms dly;
    List.iter (fun inst -> match inst with
      | TRIPLE(pin1, pin2, pin3) -> vtranif out_chan syms pin1 pin2 pin3
      | _ -> unhandled out_chan 612 inst) instances
| TRIPLE(TRAN, dly, TLIST instances) -> hash_dly out_chan syms dly;
    List.iter (fun inst -> match inst with
      | QUADRUPLE(nam, SCALAR, pin1, pin2) -> vtran out_chan syms pin1 pin2
      | _ -> unhandled out_chan 616 inst) instances
(* Parse table declarations *)
| DOUBLE(TABLE, TLIST trows) -> List.iter (fun row -> match row with
  | DOUBLE(TLIST tin,TLIST tout) -> ()
  | TRIPLE(TLIST tin,TLIST treg,TLIST tout) -> ()
  | _ -> unhandled out_chan 621 row) trows
(* Parse specify blocks *)
| DOUBLE(SPECIFY, TLIST lst) -> List.iter (fun expr -> specifyitems out_chan syms expr) lst
(* Parse primitive instance *)
| QUADRUPLE(PRIMINST, ID prim, params, TLIST inlist) ->
(*
    if (Const.shash_chain_mem Globals.modprims prim) then
      moditemlist out_chan (stem^prim^".") (Const.shash_chain_find Globals.modprims prim) (* scan the inner primitive *)
    else Printf.fprintf out_chan "Primitive %s not found\n" prim;
*)
    enter_a_sym out_chan syms prim PRIMITIVE EMPTY Create;
    let fc inner t = connect out_chan syms prim prim inner t in 
    ( match (Hashtbl.find Globals.modprims prim.id).Globals.tree with QUINTUPLE(PRIMITIVE,ID arg1, EMPTY, TLIST primargs, THASH targ4) ->
List.iter2 fc primargs inlist | _ -> ())
(* Parse module instance *)
| QUADRUPLE(MODINST, ID kind,params, TLIST instances) -> if Hashtbl.mem Globals.modprims kind.id then
    begin
      let kindhash = Hashtbl.find Globals.modprims kind.id in
      let is_input inner = 
        (Const.shash_chain_mem kindhash.symbols inner) && (TokSet.mem INPUT (Const.shash_chain_find kindhash.symbols inner).symattr) in
      enter_a_sym out_chan syms kind SUBMODULE EMPTY Create;
      kindhash.is_top <- false;
      conmodinst out_chan syms kind params instances kindhash.Globals.tree is_input
    end
  else if Hashtbl.mem libhash kind.id then
    begin
      enter_a_sym out_chan syms kind SUBMODULE EMPTY Create;
      let kindhash = Hashtbl.find Globals.libhash kind.id in
      let is_input itm = let rslt = ref false in
			 List.iter (fun {idpin=pin} -> if itm = pin then rslt := true) kindhash.ipinlst;
			 !rslt in
      conmodinst out_chan syms kind params instances kindhash.Globals.decl is_input
    end
  else
    Printf.fprintf out_chan "Sub-module %s is not found\n" kind.Idhash.id
| _ -> unhandled out_chan 721 expr );
ignore(Stack.pop stk);
mod_empty := false

and dispatch out_chan tree pass2 =
   let expr = tree.Globals.tree and syms = tree.Globals.symbols in Stack.push (726, expr) stk; ( match expr with
(* handled by decls *)
| SEXTUPLE(PARAMETER, _, range, id, attr, expr) -> if (pass2==false) then decls out_chan tree Create
| QUINTUPLE((INPUT|OUTPUT|INOUT), arg1, arg2, arg3, targ4) -> if (pass2==true) then decls out_chan tree SizeOnly
| QUADRUPLE((WIRE|REG|TRI0|TRI1|SUPPLY0|SUPPLY1|REAL|INTEGER|EVENT|DOUBLE (TRI, TLIST _)), arg1, arg2, TLIST arg3) ->
    if (pass2==true) then decls out_chan tree Create
| TRIPLE(GENVAR, arg1, TLIST arg3) ->
    if (pass2==true) then decls out_chan tree Create
(* handled by toplevelitems *)
| DOUBLE((INITIAL|FINAL|ALWAYS|TABLE|SPECIFY|GENERATE), items) -> if (pass2) then toplevelitems out_chan tree
| TRIPLE(ASSIGN, dly, TLIST assignlist) ->  if (pass2) then toplevelitems out_chan tree
| TRIPLE((BUF|NOT|AND|OR|XOR|NAND|NOR|XNOR|PULLUP|NMOS|PMOS|TRAN), dly, TLIST instances) ->  if (pass2) then toplevelitems out_chan tree
| TRIPLE((BUFIF lev|NOTIF lev|TRANIF lev), weaklist, TLIST instances) ->
    if (pass2) then toplevelitems out_chan tree
| QUADRUPLE((MODINST|PRIMINST), ID prim, params, TLIST inlist) -> if (pass2) then toplevelitems out_chan tree
(* Parse function declarations *)
| OCTUPLE(FUNCTION, EMPTY, range, ID funcname, EMPTY, TLIST args, stmts, EMPTY) ->
let syms2 = shash_create "function" funcname.Idhash.id syms 256 in (
shash_add syms funcname {Vparser.symattr = TokSet.singleton FUNCTION;
                       width = range;
		       sigattr = Sigfunc expr;
		       localsyms = syms2;
		       path=funcname};
List.iter (fun arg -> decls out_chan (init_tree [] "" arg syms2 "") Create) args;
if (pass2==false) then stmtBlock out_chan syms2 stmts)
(* Parse task declarations *)
| SEPTUPLE(TASK, EMPTY, ID taskname, EMPTY, TLIST args, stmts, EMPTY) ->
let syms2 = shash_create "task" taskname.Idhash.id syms 256 in (
shash_add syms taskname {Vparser.symattr = TokSet.singleton TASK;
                       width = VOID;
		       sigattr = Sigtask expr;
		       localsyms = syms2;
		       path=taskname};
if (Globals.verbose > 0) then (Printf.fprintf out_chan "Enter Task %s\n" taskname.Idhash.id);
List.iter (fun arg -> decls out_chan (init_tree [] "" arg syms2 "") Create) args;
if (pass2==true) then stmtBlock out_chan syms2 stmts;
if (Globals.verbose > 0) then (Printf.fprintf out_chan "End Task %s\n" taskname.Idhash.id))
| THASH _ -> myfailwith "THASH"
| DOUBLE(DEFPARAM, TLIST deflst) -> if (pass2==false) then decls out_chan tree Create
| QUADRUPLE(LOCALPARAM, EMPTY, EMPTY, TLIST loclst) -> if (pass2==false) then decls out_chan tree Create
| QUADRUPLE(TIME, EMPTY, EMPTY, TLIST timlst) -> if (pass2==false) then decls out_chan tree Create
| DOUBLE(PSL, _) -> if (pass2) then toplevelitems out_chan tree
| ASCNUM str -> () (* unhandled internal syntax *)
| _ -> unhandled out_chan 702 expr );
ignore(Stack.pop stk)

and moditemlist out_chan tree =
  let expr = tree.Globals.tree and syms = tree.Globals.symbols in Stack.push (752, expr) stk; ( match expr with
    (* Parse module declarations *)
    | QUINTUPLE(MODULE,ID arg1, TLIST arg2, TLIST arg3, THASH targ4) ->
      enter_a_sym out_chan syms arg1 MODULE SCALAR Create;
	List.iter (function
	  | SEXTUPLE (PARAMETER, signing, range, ID id, attr, expr) -> enter_parameter out_chan syms id expr EMPTY
	  | oth -> unhandled out_chan 995 oth) arg2;
	let lastarg = ref EMPTY in
	Hashtbl.iter (fun item _ -> dispatch out_chan (init_tree [] "" item syms "") false) (fst targ4);
	Hashtbl.iter (fun item _ -> dispatch out_chan (init_tree [] "" item syms "") false) (snd targ4);
	List.iter (fun arg -> match arg with
        | ID id -> enter_a_sym out_chan syms id IOPORT UNKNOWN Create
        | QUINTUPLE((INPUT|OUTPUT|INOUT), arg1, arg2, arg3, arg4) ->
            decls out_chan (init_tree [] "" arg syms "") Create;
            lastarg := arg
        | DOUBLE (ID id, EMPTY) -> (match !lastarg with
          | QUINTUPLE((INPUT|OUTPUT|INOUT) as dir, arg1, arg2, arg3, arg4) ->
              let newdecl = QUINTUPLE(dir, arg1, arg2, arg3, arg) in
              decls out_chan (init_tree [] "" newdecl syms "") Create
          | EMPTY -> enter_a_sym out_chan syms id IOPORT UNKNOWN Create
          | oth -> unhandled out_chan 977 oth)
        | _ -> unhandled out_chan 971 arg) arg3;
	Hashtbl.iter (fun item _ -> dispatch out_chan (init_tree [] "" item syms "") true) (fst targ4);
	Hashtbl.iter (fun item _ -> dispatch out_chan (init_tree [] "" item syms "") true) (snd targ4);
    (* Parse primitive declarations *)
    | QUINTUPLE(PRIMITIVE,ID arg1, EMPTY, TLIST primargs, THASH targ4) ->
	enter_a_sym out_chan syms arg1 PRIMITIVE EMPTY Create;
	List.iter (fun arg -> match arg with
        | ID id -> List.iter (fun x -> enter_a_sym out_chan syms id x UNKNOWN Create) [IOPORT;SPECIAL]; 
        | _ -> misc_syntax out_chan syms arg) primargs;
      Hashtbl.iter (fun item _ -> dispatch out_chan (init_tree [] "" item syms "") true) (fst targ4);
      Hashtbl.iter (fun item _ -> dispatch out_chan (init_tree [] "" item syms "") true) (snd targ4);
    | _ -> unhandled out_chan 723 expr );
  ignore(Stack.pop stk)

let rec cnt_ios' = function
| QUINTUPLE((INPUT|OUTPUT|INOUT) as dir, arg1, arg2, arg3, arg4) -> Vparser.TokSet.add dir (Vparser.TokSet.empty)
| QUINTUPLE(MODULE,ID arg1, arg2, TLIST arg3, THASH thash) ->
  let myset = ref Vparser.TokSet.empty in
  Hashtbl.iter (fun itm _ -> myset := Vparser.TokSet.union (cnt_ios' itm) !myset) (fst thash);
  Hashtbl.iter (fun itm _ -> myset := Vparser.TokSet.union (cnt_ios' itm) !myset) (snd thash);
  !myset
| _ -> Vparser.TokSet.empty

let rec cnt_child' = function
| DOUBLE((INITIAL|FINAL|ALWAYS), items) -> []
| DOUBLE((TABLE|SPECIFY|GENERATE), items) -> []
| DOUBLE(DEFPARAM, items) -> []
| TRIPLE(GENVAR, arg1, TLIST arg3) -> []
| TRIPLE(ASSIGN, dly, TLIST assignlist) -> []
| TRIPLE((BUF|NOT|AND|OR|XOR|NAND|NOR|XNOR|PULLUP|NMOS|PMOS|TRAN), dly, TLIST instances) -> instances
| TRIPLE((BUFIF lev|NOTIF lev|TRANIF lev), weaklist, TLIST instances) -> []
| SEXTUPLE(PARAMETER, signing, range, id, attr, expr) -> []
| QUINTUPLE((INPUT|OUTPUT|INOUT), arg1, arg2, arg3, arg4) -> []
| QUADRUPLE((WIRE|REG|TRI0|TRI1|SUPPLY0|SUPPLY1|REAL|INTEGER|EVENT), arg1, arg2, TLIST arg3) -> []
| QUADRUPLE((MODINST|PRIMINST), ID prim, params, TLIST inlist) -> if Hashtbl.mem libhash prim.id then [] else inlist
| QUINTUPLE(MODULE,ID arg1, arg2, TLIST arg3, THASH thash) ->
  let lst = ref [] in
  Hashtbl.iter (fun x _ -> lst := cnt_child' x @ !lst) (fst thash);
  Hashtbl.iter (fun x _ -> lst := cnt_child' x @ !lst) (snd thash);
  !lst
| QUINTUPLE(PRIMITIVE,ID arg1, EMPTY, TLIST primargs, THASH targ4) -> []
| SEPTUPLE(TASK, EMPTY, ID taskname, EMPTY, TLIST args, stmts, EMPTY) -> []
| OCTUPLE(FUNCTION, EMPTY, range, ID funcname, EMPTY, TLIST args, stmts, EMPTY) -> []
| _ -> []

let undecided = ref []

let rec has_behav = function
| DOUBLE(LPAREN, exp) -> has_behav exp
| DOUBLE((INITIAL|FINAL|ALWAYS), items) -> false
| DOUBLE((TABLE|SPECIFY|GENERATE), items) -> false
| DOUBLE(DEFPARAM, items) -> true
| TRIPLE(GENVAR, arg1, TLIST arg3) -> false
| TRIPLE(ASSIGN, dly, TLIST assignlist) ->
  let orall = ref false in List.iter (fun itm -> orall := !orall || has_behav itm) assignlist; !orall
| TRIPLE((XOR|PLUS|MINUS|P_ANDAND|P_OROR|P_EQUAL|BUF|NOT|AND|OR|NAND|NOR|XNOR|PULLUP|NMOS|PMOS|TRAN), _, _) -> false
| TRIPLE(ASSIGNMENT, ID op, BINNUM "1'b1") -> false
| TRIPLE(ASSIGNMENT, ID op, BINNUM "1'b0") -> false
| TRIPLE(ASSIGNMENT, ID op, exp) -> has_behav exp
| TRIPLE(ASSIGNMENT, _, DOTTED _) -> true
| TRIPLE(ASSIGNMENT, DOTTED _, _) -> true
| TRIPLE((ASSIGNMENT|BITSEL), _, _) -> false
| TRIPLE((BUFIF lev|NOTIF lev|TRANIF lev), weaklist, TLIST instances) -> false
| QUADRUPLE((QUERY|PARTSEL), _, _, _) -> false
| QUADRUPLE(LOCALPARAM, _, range, params) -> false
| QUINTUPLE((INPUT|OUTPUT|INOUT), arg1, arg2, arg3, arg4) -> false
| QUADRUPLE((REAL|INTEGER|EVENT|TIME), arg1, arg2, TLIST arg3) -> true
| QUADRUPLE(REG, EMPTY, RANGE (wlft, wrght), TLIST [TRIPLE(ID data, TLIST [RANGE (mstrt, mend)], EMPTY)]) -> true
| QUADRUPLE((REG|WIRE|TRI0|TRI1|SUPPLY0|SUPPLY1), arg1, arg2, TLIST arg3) -> false
| QUADRUPLE((MODINST|PRIMINST), ID prim, params, TLIST inlist) -> false
| QUINTUPLE(MODULE,ID arg1, arg2, TLIST arg3, THASH thash) ->
  let orall = ref false in
  Hashtbl.iter (fun itm _ -> orall := !orall || has_behav itm) (fst thash);
  Hashtbl.iter (fun itm _ -> orall := !orall || has_behav itm) (snd thash);
  !orall
| QUINTUPLE(PRIMITIVE,ID arg1, EMPTY, TLIST primargs, TLIST arg4) -> true
| SEPTUPLE(TASK, EMPTY, ID taskname, EMPTY, TLIST args, stmts, EMPTY) -> false
| SEXTUPLE(PARAMETER, signing, range, id, attr, expr) -> false
| OCTUPLE(FUNCTION, EMPTY, range, ID funcname, EMPTY, TLIST args, stmts, EMPTY) -> false
| QUADRUPLE(DOUBLE (TRI, TLIST strlst), EMPTY, _, TLIST _) -> false
| (ID _|INT _|BINNUM _|OCTNUM _|DECNUM _|HEXNUM _|WIDTHNUM _) -> false
| oth -> undecided := oth :: !undecided; false

let rec has_seq = function
| DOUBLE(LPAREN, exp) -> has_seq exp
| DOUBLE
    (ALWAYS,
     TLIST
      [DOUBLE
        (DOUBLE
          (AT,
           TLIST
            [DOUBLE
              ((POSEDGE|NEGEDGE), clk)]),
         lst)]) -> true
| QUINTUPLE(MODULE,ID arg1, arg2, TLIST arg3, THASH thash) ->
  let orall = ref false in
  Hashtbl.iter (fun itm _ -> orall := !orall || has_seq itm) (fst thash);
  Hashtbl.iter (fun itm _ -> orall := !orall || has_seq itm) (snd thash);
  !orall
| SEPTUPLE(TASK, EMPTY, ID taskname, EMPTY, TLIST args, stmts, EMPTY) -> false
| oth -> false

exception Error
let scan_current = ref ""
let prev_pending = ref []
let unresolved_dir = ref (Globals.mygetenv "UNRESOLVED_DIR")
let unresolved_ext = ref (Globals.mygetenv "UNRESOLVED_EXT")
let unresolved_to_parse = Hashtbl.create 256

let check_syms out_chan (key:string) (gsyms:shash) = let h = ref false and msg_cache = Hashtbl.create 256 in match gsyms with
| Shash symr -> 
    let erch () = begin if not !h then if !verbose then Printf.fprintf out_chan "In %s:\n" key; h := true; end in
    if (List.length(!implicit_params) > 0) then
      ( erch();
        if !verbose then
          begin
            Printf.fprintf out_chan "Implicit params:";
            List.iter (fun s -> Printf.fprintf out_chan " %s" s.Idhash.id) !implicit_params;
            Printf.fprintf out_chan "\n"
          end);
    if (List.length(!implicit_wires) > 0) then
      ( erch();
        if !verbose then
          begin
            Printf.fprintf out_chan "Implicit wires:";
            List.iter (fun s -> Printf.fprintf out_chan " %s" s.Idhash.id) !implicit_wires;
            Printf.fprintf out_chan "\n"
          end);
    shash_iter (fun nam s -> Check.erc_chk out_chan msg_cache erch symr.syms nam s) gsyms;
    let oc = out_chan in Hashtbl.iter (fun key contents -> 
        erch();
      if !verbose then
        begin
          if List.length contents > 1 then Printf.fprintf oc "%ss: " key else Printf.fprintf oc "%s: " key;
          let tab = ref (1 + String.length key) in List.iter (fun item ->
            Printf.fprintf oc "%s " item; tab := !tab+1+String.length item;
            if !tab > 72 then (output_char oc '\n'; tab := 0)) (List.sort compare contents);
          Printf.fprintf oc "\n"
        end) msg_cache
| EndShash -> myfailwith "No symbol table passed to check_syms"

let is_blackbox = function
| QUINTUPLE(MODULE,ID arg1, arg2, TLIST arg3, THASH thash) -> Hashtbl.length (snd thash) = 0
| _ -> false

let scan' out_chan key contents = begin
mod_empty := true;
implicit_wires := [];
implicit_params := [];
moditemlist out_chan contents;
contents.is_hier <- cnt_child' contents.tree <> [];
contents.is_behav <- has_behav contents.tree;
contents.is_seq <- has_seq contents.tree;
contents.is_netlist <- not (Hashtbl.mem libhash key) && not contents.is_behav && not (is_blackbox contents.tree);
end

let scan out_chan key contents =
Hashtbl.add Globals.modprims key contents;
if (Globals.verbose > 0) then Printf.fprintf out_chan "scanning ..\n";
scan' out_chan key contents;
if !mod_empty then
    Printf.fprintf out_chan "%s check skipped due to black boxing\n" key
else
    check_syms out_chan key contents.Globals.symbols;
scan_current := (!scan_current)^"."^key

let rec remove_from_pending out_chan (mykey:string) =  let reslist = ref [] in begin
		Hashtbl.iter (fun key contents ->
contents.Globals.unresolved <- List.filter(fun item -> item <> mykey) contents.Globals.unresolved;
if contents.Globals.unresolved == [] then match contents.Globals.tree with
| QUINTUPLE(kind, ID mykey, _, _, _) -> (
			if (Globals.verbose > 0) then Printf.fprintf out_chan "%s %s: resumed " (Dump.dumpstr kind) key;
			scan out_chan key contents; reslist := key :: !reslist)
| _ -> unhandled out_chan 899 contents.Globals.tree) pending;
			List.iter (fun key -> Hashtbl.remove pending key; remove_from_pending out_chan key) !reslist;
			end

let genthash lst =
  let thash = (Hashtbl.create 256,Hashtbl.create 256) in List.iter (function
  | QUINTUPLE((INPUT|OUTPUT|INOUT), _, _, _, _) as quint -> Hashtbl.add (fst thash) quint ()
  | QUADRUPLE((REG|WIRE|TRI0|TRI1), _, _, _) as quad -> Hashtbl.add (fst thash) quad ()
  | other -> Hashtbl.add (snd thash) other ()) lst; thash

let prescan out_chan var decl comment = 
  let prescan' kind (mykey:Idhash.idhash) arg3 arg4 thash =
    if (Globals.verbose > 0) then Printf.fprintf out_chan "%s %s: parsed " (Dump.dumpstr kind) mykey.Idhash.id;
    let expt = init_tree (!unresolved_list) var
      (QUINTUPLE(kind, ID mykey, arg3, arg4, THASH thash))
      (shash_create var mykey.Idhash.id EndShash 256) comment in
    if (List.length(!unresolved_list)==0) then begin
      scan out_chan mykey.id expt;
      remove_from_pending out_chan mykey.id;
    end
    else begin
      let direntries = (try Sys.readdir (!unresolved_dir) with Sys_error _ -> [|""|]) in
      if Globals.verbose >= 0 then Printf.printf "pending: (not yet encountered):\n";
      List.iter (fun (key:string) ->
        if Globals.verbose >= 0 then Printf.printf "%s " key;
        let expected = key^(!unresolved_ext) in
        if key = mykey.id then myfailwith ("Module "^key^" contains itself");
        Array.iter (fun itm -> 
          if itm = expected then
            begin
              let concat = ((!unresolved_dir)^"/"^itm) in
              if Globals.verbose >= 0 then print_endline concat;
              Hashtbl.replace unresolved_to_parse concat ()
            end) direntries
      ) !unresolved_list;
      Hashtbl.add pending mykey.id expt;
      unresolved_list := [];
    end;
    flush out_chan in
  match decl with
| QUINTUPLE(kind, ID mykey, arg3, arg4, THASH thash) ->
  prescan' kind mykey arg3 arg4 thash
| QUINTUPLE(kind, ID mykey, arg3, arg4, TLIST insts) ->
    let hash = genthash insts in
    prescan' kind mykey arg3 arg4 hash
| PREPROC str -> Printf.fprintf out_chan "Encountered %s\n" str
| other -> unhandled out_chan 919 other
;;

let rec endscan2 indent (mykey:string) =
	match !logfile with Open out_chan -> begin
        if (Globals.verbose > 0) then ( for i = 1 to indent do output_char out_chan ' '; done;
	Printf.fprintf out_chan "Checking %s: " mykey );
	if (Hashtbl.mem pending mykey) then
          begin
	  if (Globals.verbose >= 0) then Printf.fprintf stderr "Module %s still postponed\n" mykey;
	  List.iter (fun key -> if (Hashtbl.mem pending key) then endscan2 (indent+2) key
          else if (Globals.verbose >= 0)
          then Printf.fprintf stderr "%s " key) ((Hashtbl.find pending mykey).Globals.unresolved);
 	  output_char stderr '\n';
          end
	end
	| Closed -> raise Error
;;

let endscan () =
Globals.log_open();
Hashtbl.iter (fun key item -> endscan2 0 key) pending;
let repfile = if String.length !scan_current >= 32 then "report.multi" else "report"^(!scan_current) in 
scan_current := "";
if (Hashtbl.length pending > 0) then
  begin
    prev_pending := [];
    Hashtbl.iter (fun k x -> prev_pending := (k,x) :: !prev_pending) pending;
  end;
Hashtbl.clear pending;
match !logfile with
| Open out_chan -> close_out out_chan; logfile := Closed;
Sys.rename tmpnam repfile;
repfile
| Closed -> ""

(*
let nullsym = {Vparser.symattr = TokSet.empty; width = EMPTY; localsyms = EndShash; path = ""};;

let moditer k (x:Globals.modtree) = semantics out_chan k x

let find_glob s = Vparser.show_sym s ( Const.shash_chain_find s);;

let find_glob_substr s = let reg = Str.regexp s in shash_iter (fun k x -> try Printf.printf "%s posn %d\n" k (Str.search_forward reg k 0); with not_found out_chan -> ()) gsyms;;

let find_referrer s = Vparser.show_sym s (match (Const.shash_chain_find s).referrer with Referrer lk -> lk | Nil -> nullsym);;
*)
