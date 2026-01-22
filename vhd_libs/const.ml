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

open Vparser
open Globals
open Setup
open Ord
open Dump

let rec exprBoolean out_chan (syms:shash) op expr1 expr2 =
op (exprConst out_chan syms expr1) (exprConst out_chan syms expr2)

and exprInteger out_chan (syms:shash) (op:int->int->int) (expr1:token) (expr2:token) =
let const1 = exprConst out_chan syms expr1
and const2 = exprConst out_chan syms expr2
in match (const1,const2) with
| (INT num1, INT num2) -> INT (op num1 num2)
| _ -> INT 1

and widthnum' out_chan expbase (str:string) =
let base = ref 10
and width = ref 0
and value = ref 0
and basing = ref 0
and converting = ref true in
for idx = 0 to String.length(str)-1 do let ch = Char.lowercase_ascii(str.[idx]) in begin
if (Globals.verbose > 2) then Printf.fprintf out_chan "%d %c %d %d %d\n" idx ch !base !value !width;
    match ch with
| '_' -> ()
| '?' -> value := !value * !base
| '\'' -> converting := false; basing := idx+1;
| '0'..'9' -> if (!converting) then
    width := (!width * !base) + int_of_char(ch) - int_of_char('0')
else
    value := (!value * !base) + int_of_char(ch) - int_of_char('0')
| 'a'..'z' ->  if (!converting) then
    width := (!width * !base) + int_of_char(ch) - int_of_char('a') + 10
else if (!basing==idx) then begin match ch with
  | 'b' -> base := 2
  | 'd' -> base := 10
  | 'h' -> base := 16
  | _ -> value := int_of_char(ch) - int_of_char('a') + 10
end else
    value := (!value * !base) + int_of_char(ch) - int_of_char('a') + 10;
| _ -> converting := false; width := 0
end
done;
if (!basing == 0) then begin
  value := !width;
  width := 32;
end;
if (!base <> expbase) then Printf.printf "Expected base %d, actual base %d\n" expbase !base;
(!width, !value, (String.sub str (1 + !basing) (String.length str - 1 - !basing)))

and widthnum out_chan expbase (str:string) = let (a,b,c) = widthnum' out_chan expbase (str:string) in (a,b)

and shash_chain_mem (syms:shash) nam = match syms with
| Shash symr -> if Hashtbl.mem symr.syms nam then true else shash_chain_mem symr.nxt nam
| EndShash -> false

and shash_chain_find (syms:shash) nam = match syms with
| Shash symr -> if Hashtbl.mem symr.syms nam then Hashtbl.find symr.syms nam else shash_chain_find symr.nxt nam
| EndShash -> failwith "Not found"

and shash_chain_replace (syms:shash) nam (sym:symtab) = match syms with
| Shash symr -> if Hashtbl.mem symr.syms nam then Hashtbl.replace symr.syms nam sym else shash_chain_replace symr.nxt nam sym
| EndShash -> failwith "Not found"

and exprConst out_chan (syms:shash) expr = Stack.push (67, expr) stk; let rslt = ( match expr with
| INT n -> expr
| WIDTHNUM(radix,sz,num) -> INT num
| HEXNUM str -> let radix = 16 in let (sz,num) = widthnum out_chan radix str in WIDTHNUM(radix,sz,num)
| DECNUM str -> let radix = 10 in let (sz,num) = widthnum out_chan radix str in WIDTHNUM(radix,sz,num)
| OCTNUM str -> let radix = 8 in let (sz,num) = widthnum out_chan radix str in WIDTHNUM(radix,sz,num)
| BINNUM str -> let radix = 2 in let (sz,num) = widthnum out_chan radix str in WIDTHNUM(radix,sz,num)
| TRIPLE(TIMES, expr1, expr2) -> exprInteger out_chan syms ( * ) expr1 expr2
| TRIPLE(DIVIDE, expr1, expr2) -> exprInteger out_chan syms ( / ) expr1 expr2
| TRIPLE(PLUS, expr1, expr2) -> exprInteger out_chan syms ( + ) expr1 expr2
| TRIPLE(MINUS, expr1, expr2) -> exprInteger out_chan syms ( - ) expr1 expr2
| TRIPLE(P_EQUAL, expr1, expr2) -> if (exprBoolean out_chan syms (=)) expr1 expr2 then INT 1 else INT 0
| TRIPLE(P_NOTEQUAL, expr1, expr2) -> if (exprBoolean out_chan syms (<>)) expr1 expr2 then INT 1 else INT 0
| TRIPLE(LESS, expr1, expr2) -> if (exprBoolean out_chan syms (<)) expr1 expr2 then INT 1 else INT 0
| TRIPLE(GREATER, expr1, expr2) -> if (exprBoolean out_chan syms (>)) expr1 expr2 then INT 1 else INT 0
| TRIPLE(P_LTE, expr1, expr2) -> if (exprBoolean out_chan syms (<=)) expr1 expr2 then INT 1 else INT 0
| TRIPLE(P_GTE, expr1, expr2) -> if (exprBoolean out_chan syms (>=)) expr1 expr2 then INT 1 else INT 0
| DOUBLE(LPAREN, expr) -> exprConst out_chan syms expr
| DOUBLE(CONCAT, TLIST [left; right]) -> Printf.fprintf out_chan "Concat expr not yet implemented\n"; CONCAT
| ID id -> exprConstID out_chan syms id
| TRIPLE(FUNCREF, ID id, TLIST args) -> Printf.fprintf out_chan "%s is a function\n" id.Idhash.id; FUNCREF
| TRIPLE(BITSEL, arg, sel) -> exprInteger out_chan syms ( mod ) (exprInteger out_chan syms ( lsr ) arg sel) (INT 2)
| QUADRUPLE(PARTSEL, arg, INT hi, INT lo) -> exprInteger out_chan syms (lsr) arg (INT lo)
| QUADRUPLE(QUERY, expr, arg1, arg2) -> if (exprBoolean out_chan syms (<>)) expr (INT 0) then
    (exprConst out_chan syms arg1) else (exprConst out_chan syms arg2)
| TRIPLE(P_SLEFT, INT n, INT m) -> INT (n lsl m)
| TRIPLE(P_SLEFT, INT 1, ID id) -> Printf.fprintf out_chan "Const expression 1<<%s is too complicated\n" id.Idhash.id; P_SLEFT
| _ -> unhandled out_chan 97 expr; INT 1 ) in
ignore(Stack.pop stk);
rslt

and exprConstID out_chan syms id = begin
if shash_chain_mem syms id == false then begin
    Printf.fprintf out_chan "constant %s not declared in this scope\n" id.Idhash.id;
    if (!Globals.fatal) then failwith ("constant "^id.Idhash.id^" not declared in this scope\n");
    UNKNOWN
    end
else
    let found = shash_chain_find syms id in match found.sigattr with
    | Sigparam pexpr -> shash_chain_replace syms id
    {Vparser.symattr = (TokSet.add PARAMUSED found.symattr); width = found.width; sigattr = found.sigattr; localsyms = EndShash; path=id};
    exprConst out_chan syms pexpr
    | Sigarray x ->
        (match syms with 
          | Shash symr -> if shash_chain_mem symr.nxt id then exprConstID out_chan symr.nxt id
            else (Printf.fprintf out_chan "%s not a constant or for variable\n" id.Idhash.id; NOTCONST)
          | EndShash ->  Printf.fprintf out_chan "%s not a constant or for variable\n" id.Idhash.id; NOTCONST)
    | Signamed _ -> Printf.fprintf out_chan "Named block %s cannot be used here\n" id.Idhash.id; NOTCONST
    | Sigfunc _ -> Printf.fprintf out_chan "Function %s cannot be used here\n" id.Idhash.id; NOTCONST
    | Sigtask _ -> Printf.fprintf out_chan "Task %s cannot be used here\n" id.Idhash.id; NOTCONST
    | Sigundef -> Printf.fprintf out_chan "Unknown %s cannot be used here\n" id.Idhash.id; NOTCONST
  end

let exprConstStr out_chan syms expr = match exprConst out_chan syms expr with
    | INT n -> string_of_int n
    | WIDTHNUM(radix,sz,num) -> string_of_int num
    | _ -> str_token(expr)

let idirection fst snd = let inc = snd - fst in (if inc<0 then -1 else 1)

let iwidth out_chan syms wid =  let r = match wid with 
| RANGE(expr1, expr2) -> begin match (exprConst out_chan syms expr1,exprConst out_chan syms expr2) with
    | INT left, INT right -> (left, right)
    | WIDTHNUM(lradix,lsz,lnum), WIDTHNUM(rradix,rsz,rnum) -> (lnum,rnum)
    | (NOTCONST,NOTCONST) -> (0,0)
    | oth -> unhandled out_chan 144 (DOUBLE oth); (-1,-1) end
| UNKNOWN -> (0,0)
| SCALAR -> (0,0)
| EMPTY -> (0,0)
| _ -> unhandled out_chan 148 wid; (-1,-1) in (fst r,snd r,idirection (fst r) (snd r))

let maxwidth out_chan syms neww = let (left,right,inc) = iwidth out_chan syms neww in
1 + (max left right)

let exactwidth out_chan syms neww = let (left,right,inc) = iwidth out_chan syms neww in
1 + (max left right) - (min left right)

let show_set s = TokSet.iter (fun e -> Printf.printf "%s " (str_token e)) s;;

let show_chk_sig nam syma siga =
  begin
  Printf.printf "%s: " nam;
  show_set siga;
  Printf.printf "\n"
  end

let show_sig_attr syms sym = let id = sym.path in match sym.sigattr with
| Sigarray attrs -> (
match sym.width with
| RANGE range -> let (left, right, inc) = iwidth stderr syms sym.width in
  if not ((TokSet.mem IMPLICIT sym.symattr)||(TokSet.mem MEMORY sym.symattr)) then
  ( let i = ref left in try while (if inc > 0 then !i <= right else !i >= right) do
    show_chk_sig (id.Idhash.id^"["^(string_of_int !i)^"]") sym.symattr attrs.(!i);
    i := !i + inc
    done
  with Invalid_argument("index out of bounds") -> Printf.printf "Trying to access %s with index [%d:%d]\n" id.Idhash.id left right)
| SCALAR | EMPTY | UNKNOWN->
    show_chk_sig id.Idhash.id sym.symattr attrs.(0)
| _ -> unhandled stderr 791 sym.width)
| Sigparam x ->
  if not (TokSet.mem PARAMUSED sym.symattr) then Printf.printf "Parameter %s is not used\n" id.Idhash.id
| Sigtask x ->
  if not (TokSet.mem TASKUSED sym.symattr) then Printf.printf "Task %s is not used\n" id.Idhash.id
| Sigfunc x ->
  if not (TokSet.mem FUNCUSED sym.symattr) then Printf.printf "Function %s is not used\n" id.Idhash.id
| _ -> unhandled stderr 804 sym.width

let show_token (e:token) = Printf.printf "%s " (str_token e)

let show_sym _ (x:symtab) = Printf.printf "%s: " x.path.Idhash.id; TokSet.iter show_token x.symattr; print_char '\n';;

let sym_detail gsyms sym =
Vparser.TokSet.iter show_token sym.Vparser.symattr;
(match sym.Vparser.width with
| SCALAR -> Printf.printf " scalar\n"
| RANGE(INT lft, INT rght) -> Printf.printf "vector [%d:%d]\n" lft rght
| _ -> let buf = Buffer.create 64 in Dump.dump buf sym.Vparser.width 0; Buffer.output_buffer stdout buf);
show_sig_attr gsyms sym

let dump_sym m s = let gsyms = (Hashtbl.find Globals.modprims m).Globals.symbols in match gsyms with
| Shash symr -> sym_detail gsyms (Hashtbl.find symr.syms s)
| EndShash -> ()

let my_syms m = match (Hashtbl.find Globals.modprims m).Globals.symbols with
| Shash symr -> Hashtbl.iter show_sym symr.syms
| EndShash -> ()

let dump_syms m = match (Hashtbl.find Globals.modprims m).Globals.symbols with
| Shash symr -> Hashtbl.iter (fun _ x -> Printf.printf "%s: " x.path.Idhash.id; sym_detail (Shash symr) x) symr.syms
| EndShash -> ()

