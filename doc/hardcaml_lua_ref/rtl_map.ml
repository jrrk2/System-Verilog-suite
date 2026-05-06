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

open Input
open Input_types
open Input_dump
open Input_cnv
open Dump_types
open File_rewrite
open Rtl_parser

let form = "!(((C1 | C2) & A) & (B1 | B2))";;
let rslt = Formula_rewrite.rewrite form;;
let othf = ref ("", "");;
let othmap = ref None
let othmaplib = ref (XML [],([],[]))
let othuniq = ref None
let othport = ref None
let othfilt = ref ENDOFFILE
let othchk = ref (XML [], XML [])
let othargs = ref (XML [], XML [], XML [])
let othchkarith = ref ENDOFFILE
let othsel = ref (XML [])
let othalw = ref ENDOFFILE
let othmapfail = ref UNKNOWN
let oth_expr_fail = ref UNKNOWN
let sigcnt = ref 9999
let lastpat = ref UNKNOWN
let othlst = ref ("",[])
let othbody = ref []

let form' form = try Formula_rewrite.rewrite form with _ -> failwith ("Formula_rewrite failed: "^form)

let read_lib stem =
  let rw, cellhash = File_rewrite.rewrite (stem^".lib") in
  print_endline (string_of_int (Hashtbl.length cellhash));
  let newcells = ref [] in
  Hashtbl.iter (fun k (iolst, formlst, purplst) ->
	    newcells := (k,(iolst,List.map (fun (y,form) -> othf := k, form;
		     y, form' form, purplst) formlst)) :: !newcells) cellhash;
  stem, List.sort compare !newcells

let dflt_liberty = function
| None ->
  (* https://raw.githubusercontent.com/ieee-ceda-datc/RDF-2019/master/techlibs/nangate45/NangateOpenCellLibrary_typical.lib *)   
  let stem = "liberty/NangateOpenCellLibrary_typical" in
  (* from environment *)
  let stem = try Sys.getenv ("LIBERTY_LIB") with _ -> stem in
  stem
| Some stem -> stem

let filt' prop = function (_,
  (_,
    [(_, TRIPLE (IDSTR a, p, IDSTR b), String _)
    ])) -> p=prop | _ -> false

let filtbuf = function (_,
  ((_,"output")::(_,"input")::[],
    [(_, IDSTR a, String _)
    ])) -> true | _ -> false

let filtnot = function (_,
  ((_,"output")::(_,"input")::[],
    [(_, DOUBLE (NOT, IDSTR a), String _)
    ])) -> true | _ -> false

let filtplus = function (_,
  (_,
     [(_, TRIPLE (IDSTR ci, XOR, TRIPLE (IDSTR a, XOR, IDSTR b)),
       String _);
      (_, TRIPLE (TRIPLE (IDSTR a', P_AMPERSAND, IDSTR b'), P_VBAR,
         TRIPLE (IDSTR ci', P_AMPERSAND, TRIPLE (IDSTR a'', P_VBAR, IDSTR b''))),
       String _)
    ])) -> a=a' && b=b' && b'=b'' &&ci=ci' | _ -> false

let filtxor = function
    | (_, (_, [(_, TRIPLE (TRIPLE (IDSTR a, P_AMPERSAND, DOUBLE (NOT, IDSTR b)), P_VBAR,
	      TRIPLE (DOUBLE (NOT, IDSTR a'), P_AMPERSAND, IDSTR b')), _)])) when a=a' && b=b' -> true
    | (_, (_, [(_, TRIPLE (IDSTR a, XOR, IDSTR b), String _)])) -> true
    | _ -> false

let filtmux = function
    | (_, (_, [(_, TRIPLE (TRIPLE (IDSTR s, P_AMPERSAND, IDSTR b), P_VBAR,
	      TRIPLE (IDSTR a, P_AMPERSAND, DOUBLE (NOT, IDSTR s'))), String _)])) when s=s' -> true
    | (_, (_, [(_, TRIPLE (TRIPLE (IDSTR a0, P_AMPERSAND, DOUBLE (NOT, IDSTR s)), P_VBAR,
	      TRIPLE (IDSTR a1, P_AMPERSAND, IDSTR s')), String _)])) when s=s' -> true
    | _ -> false

let filtedge = function
    | (_, (ports, (_, IDSTR _, FlipFlop (_,Related ("clocked_on", clk) :: Related ("next_state", d) :: [])) :: _)) -> clk.[0]<>'!'
    | (_, (ports, (_, IDSTR _, FlipFlop (_,Related ("next_state", d) :: Related ("clocked_on", clk) :: [])) :: _)) -> clk.[0]<>'!'
    | _ -> false

let filtmap = function
| AND -> filt' P_AMPERSAND
| OR -> filt' P_VBAR
| XOR -> filtxor
| NOT -> filtnot
| oth -> othfilt := oth; failwith "filtmap"
    
let filtcells' = function
| (AND|OR|XOR) as op -> filtmap op
| NOT as op -> filtmap op
| BUF -> filtbuf
| PLUS -> filtplus
| QUERY -> filtmux
| POSEDGE -> filtedge
| oth -> othfilt := oth; failwith "filtcells"

let filtcells cells func =
  let nlst = ref [] in
  let uniq = List.sort compare !nlst, List.sort_uniq compare (List.map (fun (nam,p) -> nlst := nam :: !nlst; p) (List.filter (filtcells' func) cells)) in
  match uniq with
    | (_,[]) -> othuniq := Some (func, uniq); failwith ("filtcells returned an empty list")
    | _ -> othuniq := None; uniq

let opmap = function
| P_AMPERSAND -> "&"
| P_VBAR -> "|"
| XOR -> "^"
| NOT -> "!"
| oth -> othmapfail := oth; failwith "opmap"

let rec expr = function
| IDSTR id -> id
| INT n -> string_of_int n
| INTNUM n -> n
| DOUBLE((NOT as op),arg) -> "("^opmap op^expr arg^")"
| TRIPLE(lft, (P_AMPERSAND|P_VBAR|XOR as op), rght) -> "("^expr lft^opmap op^expr rght^")"
| DOUBLE(CONCAT, TLIST lst) -> String.concat ", " (List.map expr lst)
| QUADRUPLE (PARTSEL, id, INT hi, INT lo) -> expr id^"["^string_of_int hi^":"^string_of_int lo^"]"
| QUADRUPLE (QUERY, cond, lft, rght) -> "("^expr cond^"?"^expr  lft^":"^expr  rght^")"
| oth -> oth_expr_fail := oth; failwith "rtl_map_expr"

let othdump' = ref None

let clocked q iq = function
| Related ("clocked_on", clk) -> "reg "^iq^"; always @(posedge "^clk^") "^iq^" <= ~D; assign "^q^" = "^iq^";"
| _ -> ""

let enabled = function
| Related ("enable", en) -> "reg IQ; always @("^en^") if ("^en^") IQ <= D; assign Q = IQ;"
| _ -> ""

let concat' = function
| q, IDSTR iq, FlipFlop (oplst, relst) as oth -> othdump' := Some oth; String.concat "" (List.map (clocked q iq) relst)
| _, _, Latch (oplst, relst) -> String.concat "" (List.map (enabled) relst)
| (lhs, exp', (String _|Related _|IsolationCell)) -> "assign "^lhs^" = "^expr exp'^";"
| oth -> othdump' := Some oth; failwith "dumpv''"

let dumpv cells stem = let fd = open_out (stem^".v") in
List.iter (function
| (nam, (portlst, funclst)) ->
output_string fd ("module "^nam^" ("^String.concat ", " (List.map (fun (port,dir) -> dir^" "^port) portlst)^");\n"^
String.concat "\n" (List.map (concat') funclst)^"\nendmodule\n\n")
) cells;
close_out fd

let othrtl = ref ENDOFFILE
let othmapfail = ref ENDOFFILE
let uitms = ref []

let dir = function
| INPUT -> Dinput
| OUTPUT -> Doutput
| oth -> othmapfail := oth; failwith "dir"

let arithop = function
| TIMES -> Amul "fastest"
| PLUS -> Aadd "fastest"
| MINUS -> Asub "fastest"
| oth -> othmapfail := oth; failwith "arithop"

let logicop = function
| AND -> Land
| OR -> Lor
| XOR -> Lxor
| oth -> othmapfail := oth; failwith "logicop"

let cmpop = function
| P_EQUAL -> Ceq
| LESS -> Clt
| oth -> othmapfail := oth; failwith "cmpop"

let width_reg itms reg =
  let wid' rng _ = function
    | Width(hi,lo,signed) -> rng := TYPRNG (HEX hi, HEX lo)
    | _ -> failwith "width_reg" in
  let rng = ref TYPNONE in tran_search itms (wid' rng) (wid' rng) reg; !rng

let chk_reg itms reg fn =
  let wid' _ = function
    | Width(hi,lo,signed) -> for i = hi downto lo do fn hi lo i; done
    | _ -> failwith "chk_reg" in
  tran_search itms wid' wid' reg

let othff = ref (String "")

let conndir = function
| "input" -> Dinput
| "output" -> Doutput
| oth -> failwith ("conndir: "^oth)

let rec prefix pref = function
| IDSTR s -> IDSTR (if (* s.[0] = '_' && *) not (List.mem s pref) then (String.concat "_" pref)^s else s)
| TLIST slst -> TLIST (List.map (prefix pref) slst)
| DOUBLE(a,b) -> DOUBLE((prefix pref) a, (prefix pref) b)
| TRIPLE(a,b,c) -> TRIPLE((prefix pref) a, (prefix pref) b, (prefix pref) c)
| QUADRUPLE(a,b,c,d) -> QUADRUPLE((prefix pref) a, (prefix pref) b, (prefix pref) c, (prefix pref) d)
| QUINTUPLE(a,b,c,d,e) -> QUINTUPLE((prefix pref) a, (prefix pref) b, (prefix pref) c, (prefix pref) d, (prefix pref) e)
| SEXTUPLE(a,b,c,d,e,f) -> SEXTUPLE((prefix pref) a, (prefix pref) b, (prefix pref) c, (prefix pref) d, (prefix pref) e, (prefix pref) f)
| SEPTUPLE(a,b,c,d,e,f,g) -> SEPTUPLE((prefix pref) a, (prefix pref) b, (prefix pref) c, (prefix pref) d, (prefix pref) e, (prefix pref) f, (prefix pref) g)
| oth -> oth

let nthexp porth pin ix = let lst = Hashtbl.find porth pin in othlst := (pin,lst);
if lst <> [] then (if ix >= List.length lst then (List.hd lst :: []) else List.nth lst ix :: []) else []

let connlst' porth hi lo i p = let pin = fst p in PORT ("", pin, conndir (snd p), nthexp porth pin (i-lo))

let rec explode' itms lst = function
| VRF(nam, _, []) as p -> chk_reg itms nam (fun hi lo i ->
  lst := (if hi > lo then SEL("", p :: CNST(32, HEX i) :: CNST (32, SHEX 1) :: []) else p) :: !lst)
| CNST (wid, HEX n) -> for i = wid-1 downto 0 do lst := (CNST (1, HEX (if ((1 lsl i) land n) > 0 then 1 else 0))) :: !lst done
| SEL ("", (VRF (_, _, []) as net) :: CNST (32, HEX base) :: CNST (32, HEX wid) :: []) ->
	   for i = base+wid-1 downto base do lst := (SEL("", net :: CNST(32, HEX (i)) :: CNST (32, SHEX 1) :: [])) :: !lst done
| SEL ("", _) as p -> othsel := p;
| CAT ("", catlst) -> List.iter (explode' itms lst) catlst
| oth -> othmap := Some oth; failwith "explode'"

let explode itms x = let lst = ref [] in explode' itms lst x; List.rev !lst

let annot itms porth inports outports op = function
 | FlipFlop (ipins, lst),rhs::clk::[] -> List.iter (function
	| Related ("next_state", pin) -> Hashtbl.replace porth pin (explode itms rhs)
	| Related ("clocked_on", pin) -> Hashtbl.replace porth pin (clk :: [])
	| String ipin when ipin.[String.length ipin-1] <> 'N' -> let pin = String.sub ipin 1 (String.length ipin - 1) in Hashtbl.replace porth pin (explode itms op)
	| oth -> othff := oth) (ipins@lst)
 | Latch (_, _), _ -> ()
 | String _, lst ->
 let pnam ix = fst (try List.nth inports ix with _ -> othport := Some (inports, ix); failwith "othport") in
 List.iteri (fun ix itm -> Hashtbl.replace porth (pnam ix) (explode itms itm)) lst;
 Hashtbl.replace porth (fst (List.hd outports)) (explode itms op)
 | oth,_ -> othff := oth; failwith "maplib'_othff"

let maplib' mapcnt itms lst = function
| (VRF (nam, _, _) as op), (cellnam::_,(portlst,(funy,eqn,(liberty:liberty))::_)::_) ->
let porth = Hashtbl.create 127 in
List.iter (fun (nam, _) -> Hashtbl.add porth nam []) portlst;
let inports, outports = List.partition (function (nam, "input") -> true | _ -> false) portlst in
annot itms porth inports outports op (liberty,lst);
incr mapcnt;
chk_reg itms nam (fun hi lo i ->
if false then print_endline ("Iteration: "^string_of_int i);
let cellinst = cellnam^"_"^string_of_int !mapcnt^(if hi > lo then "_"^string_of_int i else "") in
itms.inst :=
     (cellinst,
      ("", cellnam, List.map (connlst' porth hi lo i) inports @ List.map (connlst' porth hi lo i) outports)) :: !(itms.inst)
)
| oth -> othmaplib := oth;failwith "othmaplib"

(*
let maplib itms lst cell =
decr sigcnt;
let nam = "_"^string_of_int !sigcnt in
let outp = VRF (nam, (BASDTYP, "wire", TYPNONE, []), []) in
_Identyp itms nam Vpinet;
maplib' mapcnt itms lst (outp,cell);
outp
*)

let rec expr itms = function
| IDSTR id -> _Ident itms id
| BINNUM c -> CNST (cexp c)
| INT n -> CNST (32, HEX n)
| QUADRUPLE (PARTSEL, id, INT hi, INT lo) -> SEL ("", [expr itms id; CNST (32, HEX lo); CNST (32, HEX (hi-lo+1))])
| DOUBLE(CONCAT, TLIST lst) -> _Concat (List.map (expr itms) lst)
(*
| DOUBLE(NOT,arg) -> UNRY(Unot, expr itms arg :: [])
| TRIPLE((AND|OR|XOR), lft, rght) as func -> maplib itms [expr itms lft;expr itms rght] (filtcells itms.cells func)
| TRIPLE((PLUS|MINUS|TIMES as op), lft, rght) -> ARITH(arithop op, [expr itms lft;expr itms rght])
| TRIPLE((P_EQUAL|LESS as op), lft, rght) -> CMP(cmpop op, [expr itms lft;expr itms rght])
| QUADRUPLE (PARTSEL, id, hi, lo) -> _Selection itms (expr itms id, expr itms hi, expr itms lo, 0, 0)
| QUADRUPLE (QUERY, cond, lft, rght) -> maplib itms [expr itms cond; expr itms lft; expr itms rght] (filtcells itms.cells QUERY)
*)
| oth -> othmapfail := oth; failwith "expr"

let expr itms expression = othmapfail := expression; expr itms expression
	 
let ports = List.iter (function IDSTR id -> print_endline id | _ -> failwith "ports")

let body itms = function
| QUADRUPLE(DLYASSIGNMENT, src, EMPTY, dest) -> _Asgn itms (expr itms src, expr itms dest)
| oth -> othmapfail := oth; failwith "body"

let _chk_assign mapcnt itms (lhs, rhs) = maplib' mapcnt itms [lhs] (rhs, filtcells itms.cells BUF)

let rec _map mapcnt itms = function
| TLIST lst -> List.iter (map mapcnt itms) lst
| DOUBLE(POSEDGE,arg) as pat -> othmapfail := pat; failwith "DOUBLE(POSEDGE,arg)"
| DOUBLE(ALWAYS, TLIST [DOUBLE (DOUBLE (AT, TLIST [DOUBLE (POSEDGE, clk)]), TLIST
         [TLIST [QUADRUPLE (DLYASSIGNMENT, lhs, EMPTY, rhs)]])]) ->
maplib' mapcnt itms (List.map (expr itms) [rhs;clk]) (expr itms lhs, filtcells itms.cells POSEDGE)
| DOUBLE(ALWAYS, TLIST [DOUBLE (DOUBLE (AT, TLIST [DOUBLE (POSEDGE, IDSTR clk)]), TLIST [TLIST lst])]) as alw -> othalw := alw;
  _Always itms (POSEDGE clk, List.map (body itms) lst)
| DOUBLE(tok,arg) as pat -> othmapfail := pat; failwith "DOUBLE(tok,arg)"
| TRIPLE(EQUALS, arg1, arg2) as pat -> othmapfail := pat; failwith "TRIPLE(EQUALS,"
| TRIPLE(IF, arg1, arg2) as pat -> othmapfail := pat; failwith "TRIPLE(IF,"
| TRIPLE(PLUS, arg1, arg2) as pat -> othmapfail := pat; failwith "TRIPLE(PLUS,"
| TRIPLE(LESS, arg1, arg2) as pat -> othmapfail := pat; failwith "TRIPLE(LESS,"
| TRIPLE(ASSIGNMENT, arg1, arg2) as pat -> othmapfail := pat; failwith "TRIPLE(ASSIGNMENT,"
| TRIPLE(ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, lhs, (TRIPLE((AND|OR|XOR as func), lft, rght)))]) ->
  maplib' mapcnt itms ([expr itms lft;expr itms rght]) (expr itms lhs, filtcells itms.cells func)
| TRIPLE(ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, lhs, (DOUBLE((NOT as func), rght)))]) ->
  maplib' mapcnt itms ([expr itms rght]) (expr itms lhs, filtcells itms.cells func)
| TRIPLE(ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, lhs, QUADRUPLE (QUERY, cond, lft, rght))]) ->
  maplib' mapcnt itms [expr itms cond; expr itms lft; expr itms rght] (expr itms lhs, filtcells itms.cells QUERY)
| TRIPLE(ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, lhs, (TRIPLE((PLUS|MINUS|P_EQUAL|LESS as op), lft, rght)))]) ->
  let body = match _chk_arith itms op (expr itms lft, expr itms rght, expr itms lhs) with
    | QUINTUPLE (MODULE, IDSTR _, TLIST [], TLIST io, TLIST lst) ->
      let body' = List.filter (function QUINTUPLE ((INPUT|OUTPUT), _, _, _, _) -> false | _ -> true) lst in
      let map' = function IDSTR id -> id | _ -> failwith "prefix" in
      List.map (prefix (List.map map' io)) body'
    | oth -> othchkarith := oth; failwith "_chk_arith" in
    othbody := body; List.iter (map mapcnt itms) body
| TRIPLE(ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, lhs, rhs)]) -> _chk_assign mapcnt itms (expr itms rhs, expr itms lhs)
| TRIPLE(tok, arg1, arg2) as pat ->  othmapfail := pat; failwith "TRIPLE(tok,"
| QUADRUPLE(QUERY, arg1, arg2, arg3) as pat ->  othmapfail := pat; failwith "QUADRUPLE(QUERY,"
| QUADRUPLE(PARTSEL, IDSTR id, hi, lo) as pat -> othmapfail := pat; failwith "QUADRUPLE(PARTSEL,"
| QUADRUPLE(EQUALS, arg1, arg2, arg3) as pat -> othmapfail := pat; failwith "QUADRUPLE(EQUALS,"
| QUADRUPLE(IF, arg1, arg2, arg3) as pat -> othmapfail := pat; failwith "QUADRUPLE(IF,"
| QUADRUPLE(REG, EMPTY, EMPTY,
    TLIST [TRIPLE (IDSTR nam, EMPTY, EMPTY)]) -> _Identyp itms nam Vpinet
| QUADRUPLE(WIRE, EMPTY, TRIPLE (EMPTY, EMPTY, EMPTY),
    TLIST [DOUBLE (IDSTR nam, EMPTY)]) -> _Identyp itms nam Vpinet
| QUADRUPLE((WIRE|REG), EMPTY, TRIPLE (EMPTY, RANGE (INT hi, INT lo), EMPTY),
    TLIST [DOUBLE (IDSTR nam, EMPTY)]) -> _Identyprng itms nam (TYPRNG(HEX hi, HEX lo)) Vpinet
| QUADRUPLE((WIRE|REG), EMPTY, RANGE (INT hi, INT lo),
    TLIST [TRIPLE (IDSTR nam, EMPTY, EMPTY)]) -> _Identyprng itms nam (TYPRNG(HEX hi, HEX lo)) Vpinet
| QUADRUPLE((WIRE|REG), EMPTY, TRIPLE (EMPTY, EMPTY, EMPTY),
	  TLIST [TRIPLE (IDSTR nam, EMPTY, init)]) ->
		  _Identyp itms nam Vpinet; _chk_assign mapcnt itms (expr itms init, _Ident itms nam)
| QUADRUPLE((WIRE|REG), EMPTY, TRIPLE (EMPTY, RANGE (INT hi, INT lo), EMPTY),
	  TLIST [TRIPLE (IDSTR nam, EMPTY, init)]) ->
		  _Identyprng itms nam (TYPRNG(HEX hi, HEX lo)) Vpinet; _chk_assign mapcnt itms (expr itms init, _Ident itms nam)
| QUADRUPLE(tok, arg1, arg2, arg3) as pat -> othmapfail := pat; failwith "QUADRUPLE(tok,"
| QUINTUPLE(MODULE, arg1, arg2, TLIST arg3, arg4) -> let u = empty_itms itms.cells in uitms := u :: !uitms; map mapcnt u arg2; ports arg3; map mapcnt u arg4
| QUINTUPLE((INPUT|OUTPUT as dir'), EMPTY, EMPTY, EMPTY,
        TLIST [TRIPLE (IDSTR nam, EMPTY, EMPTY)]) -> _Port itms (dir dir') nam
| QUINTUPLE((INPUT|OUTPUT as dir'), EMPTY, EMPTY, RANGE (INT hi, INT lo),
        TLIST [TRIPLE (IDSTR nam, EMPTY, EMPTY)]) -> _Portrng itms (dir dir') nam (TYPRNG(HEX hi, HEX lo))
| QUINTUPLE(tok, arg1, arg2, arg3, arg4) as pat -> othmapfail := pat; failwith "QUINTUPLE(tok,"
| SEXTUPLE(tok, arg1, arg2, arg3, arg4, arg5) as pat -> othmapfail := pat; failwith "SEXTUPLE(tok,"
| SEPTUPLE(tok, arg1, arg2, arg3, arg4, arg5, arg6) as pat -> othmapfail := pat; failwith "SEPTUPLE(tok,"
| RANGE(arg1,arg2) as pat -> othmapfail := pat; failwith "RANGE(arg1,arg2)"
| ALWAYS as pat -> othmapfail := pat; failwith "ALWAYS"
| ASCNUM c as pat -> othmapfail := pat; failwith "ASCNUM"
| ASSIGN as pat -> othmapfail := pat; failwith "ASSIGN"
| AT as pat -> othmapfail := pat; failwith "AT"
| BINNUM c as pat -> othmapfail := pat; failwith "BINNUM"
| BITSEL as pat -> othmapfail := pat; failwith "BITSEL"
| BUFIF lev as pat -> othmapfail := pat; failwith "BUFIF"
| D_ATTRIBUTE as pat -> othmapfail := pat; failwith "D_ATTRIBUTE"
| DECNUM c as pat -> othmapfail := pat; failwith "DECNUM"
| DOT as pat -> othmapfail := pat; failwith "DOT"
| EMPTY as pat -> othmapfail := pat; failwith "EMPTY"
| FLOATNUM flt as pat -> othmapfail := pat; failwith "FLOATNUM"
| HASH as pat -> othmapfail := pat; failwith "HASH"
| HEXNUM c as pat -> othmapfail := pat; failwith "HEXNUM"
| IDSTR str as pat -> othmapfail := pat; failwith "IDSTR"
| ILLEGAL c as pat -> othmapfail := pat; failwith "ILLEGAL"
| INOUT as pat -> othmapfail := pat; failwith "INOUT"
| INPUT as pat -> othmapfail := pat; failwith "INPUT"
| INTNUM c as pat -> othmapfail := pat; failwith "INTNUM"
| NEGEDGE as pat -> othmapfail := pat; failwith "NEGEDGE"
| OUTPUT as pat -> othmapfail := pat; failwith "OUTPUT"
| PARTSEL as pat -> othmapfail := pat; failwith "PARTSEL"
| PREPROC str as pat -> othmapfail := pat; failwith "PREPROC"
| REG as pat -> othmapfail := pat; failwith "REG"
| WEAK strength as pat -> othmapfail := pat; failwith "WEAK"
| WIDTHNUM(radix,sz,num) as pat -> othmapfail := pat; failwith "WIDTHNUM(radix,sz,num)"
| INT n as pat -> othmapfail := pat; failwith "INT"
| oth -> othmapfail := oth; failwith "dump"

and map mapcnt itms pat = lastpat := pat; _map mapcnt itms pat

and _chk_arith itms op = function
| VRF (a, _, _), VRF (b, _, _), VRF (y, _, _) ->
  let typrnga = width_reg itms a in
  let typrngb = width_reg itms b in
  let typrngy = width_reg itms y in
  let arithop lhs rhs = function
   | LESS -> CMP(Clt, lhs :: rhs :: [])
   | P_EQUAL -> CMP(Ceq, lhs :: rhs :: [])
   | PLUS -> ARITH(Aadd "fastest", lhs :: rhs :: [])
   | MINUS -> ARITH(Asub "fastest", lhs :: rhs :: [])
   | oth -> othmapfail := oth; failwith "arithop'" in
  let op' = arithop
               (VRF (a, (BASDTYP, "wire", typrnga, []), [])) 
               (VRF (b, (BASDTYP, "wire", typrngb, []), [])) op in
  map' (Input_hardcaml.cnv ("arithop",
     {io =
       {contents =
         [(a, ("", (BASDTYP, "wire", typrnga, []), Dinput, "wire", []));
          (b, ("", (BASDTYP, "wire", typrngb, []), Dinput, "wire", []));
          (y, ("", (BASDTYP, "wire", typrngy, []), Doutput, "wire", []))]};
      v = {contents = []}; iv = {contents = []}; ir = {contents = []};
      ca = {contents = []};
      alwys =
       {contents =
         [("", COMB,
           [SNTRE [];
            ASGN (false, "", op' :: VRF (y, (BASDTYP, "wire", typrngy, []), []) :: [])])]};
      init = {contents = []}; func = {contents = []}; task = {contents = []};
      gen = {contents = []}; imp = {contents = []}; inst = {contents = []};
      cnst = {contents = []}; needed = {contents = []};
      remove_interfaces = false; mode = ""; names'' = []; cells=[]; topmods=ref []; allmods=ref [] } ))
| oth -> othargs := oth; failwith "_chk_arith othargs"

and map' rtl =
let verbose = try int_of_string (Sys.getenv "RTL_MAP_VERBOSE") > 0 with _ -> false in
if verbose then (let fd = open_out (Filename.temp_file ~temp_dir:"." "arith_" ".v") in output_string fd rtl; close_out fd);
let lexbuf = Lexing.from_string rtl in
let rslt = Rtl_parser.start Rtl_lexer.token lexbuf in
rslt

let map cells modnam rtl =
let chk = modnam^"_hardcaml.v" in
print_endline chk;
let fd = open_out chk in
output_string fd rtl;
close_out fd;
let rslt = map' rtl in
othrtl := rslt;
let u = empty_itms cells in
uitms := u :: [];
let _ = map (ref 0) u rslt in
let u = List.hd !uitms in
uitms := u :: [];
u
