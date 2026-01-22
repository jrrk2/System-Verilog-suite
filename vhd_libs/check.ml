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

open List
open Const
open Printf
open Vparser
open Globals
open Setup
open Dump

let dotted s = try String.index s '.' > 0 ; with Not_found -> false;;

let erc_chk_sig out_chan (syma:tset) siga id =
  let skip="UNCONNECTED" in
  let len = String.length skip in
  if ((TokSet.mem INPUT syma) && (TokSet.mem SENSUSED siga)) && not (TokSet.mem DRIVER siga) then
    "Input mentioned in sensitivity list but not referenced"
  else if (TokSet.mem INPUT syma) && not ((TokSet.mem DRIVER siga) || (TokSet.mem SPECIAL syma)) then
    "Unloaded input"
  else if (TokSet.mem OUTPUT syma) && not ((TokSet.mem RECEIVER siga) || (TokSet.mem SPECIAL syma)) then
    "Undriven output"
  else if (TokSet.mem INOUT syma) then
    "Note: Inout"
  else if ((String.length id < len) || (skip <> String.sub id (String.length id - len) len)) && (TokSet.mem WIRE syma) && (TokSet.mem DRIVER siga) && not ((TokSet.mem RECEIVER siga) || (TokSet.mem SPECIFY syma)) then
    "Undriven but driving wire"
  else if ((String.length id < len) || (skip <> String.sub id (String.length id - len) len)) && (TokSet.mem WIRE syma) && not ((TokSet.mem RECEIVER siga) || (TokSet.mem SPECIFY syma)) then
    "Unused wire"
  else ""

let cache_msgs msg_cache msg id = if (String.length msg > 0) then begin
if (Hashtbl.mem msg_cache msg) then begin
 let entry = Hashtbl.find msg_cache msg in
  Hashtbl.replace msg_cache msg (id :: entry)
 end
else
 Hashtbl.add msg_cache msg [id]
end;;

let shash_create' ar nm prev table = Shash {nxt=prev; syms=table; stabarch=ar; stabnam=nm}

let erc_chk out_chan msg_cache erch (gsyms:sentries) (id:Idhash.idhash) s = match s.sigattr with
| Sigarray attrs -> (
match s.width with
| RANGE range -> let (left, right, inc) = iwidth out_chan (shash_create' "erc" "chk" EndShash gsyms) s.width in
  if not ((TokSet.mem IMPLICIT s.symattr)||(TokSet.mem MEMORY s.symattr)) then
  ( let msg0 = ref "" and i0 = ref left and i1 = ref left and i = ref left in try while (if inc > 0 then !i <= right else !i >= right) do
    let msg = erc_chk_sig out_chan s.symattr attrs.(!i) id.Idhash.id in
(*    if (String.length msg > 0) then
        Printf.fprintf (fst out_chan) "DBG: %s %s\n" (id^"["^(string_of_int i)^"]") msg;  *)
    if (msg <> !msg0) then (
      cache_msgs msg_cache !msg0 (id.Idhash.id^"["^(string_of_int !i0)^":"^(string_of_int !i1)^"]");
      i0 := !i;
      i1 := !i;
      msg0 := msg; )
    else if (!i == right) then (
      if (String.length msg > 0) then
      cache_msgs msg_cache !msg0 (id.Idhash.id^"["^(string_of_int !i0)^":"^(string_of_int !i)^"]"))
    else
      i1 := !i;
    i := !i + inc
    done
  with Invalid_argument("index out of bounds") ->
    (erch(); Printf.fprintf out_chan "Trying to access %s with index [%d:%d]\n" id.Idhash.id left right))
| SCALAR | EMPTY | UNKNOWN->
    let msg = erc_chk_sig out_chan s.symattr attrs.(0) id.Idhash.id in
    cache_msgs msg_cache msg id.Idhash.id
| _ -> unhandled out_chan 791 s.width)
| Sigparam x ->
  if not (TokSet.mem PARAMUSED s.symattr) then cache_msgs msg_cache "Unused Parameter" id.Idhash.id
| Sigtask x ->
  if not (TokSet.mem TASKUSED s.symattr) then cache_msgs msg_cache "Unused Task" id.Idhash.id
| Sigfunc x ->
  if not (TokSet.mem FUNCUSED s.symattr) then cache_msgs msg_cache "Unused Function" id.Idhash.id
| Signamed x -> ()
| Sigundef -> ()
;;
