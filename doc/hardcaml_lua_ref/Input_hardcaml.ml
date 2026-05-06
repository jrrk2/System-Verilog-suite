(*
 MIT License

Copyright (c) 2024 Jonathan Kimmitt

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)

open Input
open Input_types
open Dump_types
open Hardcaml
open Always
open Signal

type othalw = (string * Input_types.rw * Input_types.rw list)

let othrepl = ref (REPL("",0,[]))
let othsel' = ref (SEL ("", []))
let othcse = ref []
let othdflt = ref []
let othfunc = ref ""
let othradix = ref (Void 33)
let otharith = ref UNKNOWN
let othlogop = ref Lunknown
let othc = ref UNKNOWN
let othcs = ref None
let othcmp = ref Cunknown
let othirng = ref UNKNOWN
let othtokens = ref Rtl_parser.ENDOFFILE
let othcnst = ref (ERR "othcnst")
let (othalwystran:othalw ref) = ref ("", COMB, [])
let othcatran = ref None
let othasgn = ref Work
let othr = ref Invalid
let othdecl' = ref ("", Work)
let othrw = ref Work
let othlhs = ref (Void 47)
let othrhs = ref (Void 48)
let othrmlhs = ref Invalid
let othrmrhs = ref Invalid
let othcond' = ref (Void 51)
let othlhs' = ref (Void 52)
let othrhs' = ref (Void 53)
let othrmcond' = ref Invalid
let othrmlhs' = ref Invalid
let othrmrhs' = ref Invalid
let othc' = ref (Void 57)
let othr' = ref (Void 58)
let otht = ref None
let othremapp' = ref []
let othremapp'' = ref []
let othremapp''' = ref []
let othdecl = ref []
let othckedg = ref Work
let othrstedg = ref Work
let othop = ref (Void 65, Invalid_, Invalid_)

let rec log2 n = if n <= 1 then 0 else 1 + log2 (n/2)

let exact_log2 n = 1 lsl (log2 n) = n

let sys_func x = function
| "$signed" -> Unary (Signed, x)
| "$unsigned" -> Unary (Unsigned, x)
| oth -> othfunc := oth; failwith "sys_func"

let signed_zero n = Signed.of_signal (Signal.zero n)
let arithnegate rhs = Signal.zero (width rhs) -: rhs
let signedwidth rhs = width (Signed.to_signal rhs)
let signednegate rhs = let open Signed in signed_zero (signedwidth rhs) -: rhs

let adder_config a b = let wa, wb = width a, width b in function
   | "Serial" | "" -> Hardcaml_circuits.Prefix_sum.Config.Serial
   | "Sklansky" | "cla" -> Sklansky
   | "Kogge-Stone" -> Kogge_stone
   | "Brent-Kung" -> Brent_kung
   | "fastest" -> if exact_log2 wa && exact_log2 wb then Kogge_stone else if wa > 2 || wb > 2 then Sklansky else Serial
   | oth -> failwith ("Adder model not supported: "^oth)

let add_fast model a_sig b_sig =
          (Hardcaml_circuits.Prefix_sum.create
           ~config:(if model <> "" then if false then print_endline ("add_fast model: "^model); adder_config a_sig b_sig model)
             (module Signal)
             ~input1:(a_sig)
             ~input2:(b_sig)
             ~carry_in:(Signal.zero 1))

let sub_fast model carry_in a_sig b_sig =
          (Hardcaml_circuits.Prefix_sum.create
             ~config:(if model <> "" then if false then print_endline ("add_fast model: "^model); adder_config a_sig b_sig model)
             (module Signal)
             ~input1:(a_sig)
             ~input2:(~: b_sig)
             ~carry_in)

let fold' fn rhs = let expl = List.init (width rhs) (bit rhs) in List.fold_left fn (List.hd expl) (List.tl expl)

let notequal a b = fold' (|:) (a ^: b)

let equal a b = (~:) (notequal a b)

let less_fast a b =
    let diff = sub_fast "" Signal.vdd (uresize a (width a + 1)) (uresize b (width b + 1)) in
    ~: (bit diff (width diff - 1))

let greater_fast a b =
    let diff = sub_fast "" Signal.vdd (uresize b (width b + 1)) (uresize a (width a + 1)) in
    ~: (bit diff (width diff - 1))

let greater_equal_fast a b =
    let diff = sub_fast "" Signal.gnd (uresize b (width b + 1)) (uresize a (width a + 1)) in
    ~: (bit diff (width diff - 1))

let less_equal_fast a b =
    let diff = sub_fast "" Signal.gnd (uresize a (width a + 1)) (uresize b (width b + 1)) in
    ~: (bit diff (width diff - 1))

let less_signed lhs rhs = Signed.of_signal (less_fast (Signed.to_signal lhs) (Signed.to_signal rhs))

let greater_signed lhs rhs = Signed.of_signal (greater_fast (Signed.to_signal lhs) (Signed.to_signal rhs))

let greater_equal_signed lhs rhs = Signed.of_signal (greater_equal_fast (Signed.to_signal lhs) (Signed.to_signal rhs))

let less_equal_signed lhs rhs = Signed.of_signal (less_equal_fast (Signed.to_signal lhs) (Signed.to_signal rhs))

let mult_config = function
  | "Wallace" | "" -> Hardcaml_circuits.Mul.Config.Wallace
  | "Dadda" | "fastest" -> Dadda
  | oth -> failwith ("Multiplier model not supported: "^oth)

let mult_fast model a_sig b_sig =
          (Hardcaml_circuits.Mul.create
             ~config:(if model <> "" then if false then print_endline ("mult_fast model: "^model); mult_config model)
             (module Signal)
             (a_sig)
             (b_sig))

let spec width signedness architecture =
  (module struct
    let width = width
    let signedness = signedness
    let architecture = architecture
  end : Hardcaml_circuits.Divider.Spec)

let divmod_instance signed numerator denominator =
  let open Hardcaml_circuits.Divider.Make (val spec (width numerator) signed Combinational) in
  let clock = Signal.vdd in
  let clear = Signal.vdd in
  let start = Signal.vdd in
  let (inp : Hardcaml__.Signal.t I.t) = { clock; clear; numerator; denominator; start } in
  let ({ quotient ; remainder ; valid } : Hardcaml__.Signal.t O.t) =
      create (Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()) inp in
  quotient, remainder

let div_unsigned numerator denominator = let (quo,rem) = divmod_instance Unsigned numerator denominator in quo
let mod_unsigned numerator denominator = let (quo,rem) = divmod_instance Unsigned numerator denominator in rem

let div_signed numerator denominator = let (quo,rem) = divmod_instance Signed numerator denominator in quo
let mod_signed numerator denominator = let (quo,rem) = divmod_instance Signed numerator denominator in rem

let mux2' cond' lhs rhs =
let cond = fold' (|:) cond' in
let wlhs = width lhs in
let wrhs = width rhs in
if wlhs = wrhs then
mux2 cond lhs rhs
else if wlhs < wrhs then
mux2 cond (uresize lhs wrhs) rhs
else if wlhs > wrhs then
mux2 cond lhs (uresize rhs wlhs)
else failwith "mux2'"

let mult_fast_signed model a_sig b_sig =
          let open Signed in
          let nega = to_signal (less_signed a_sig (signed_zero (signedwidth a_sig))) in
          let negb = to_signal (less_signed b_sig (signed_zero (signedwidth b_sig))) in
          let mult' = Hardcaml_circuits.Mul.create
             ~config:(if model <> "" then if false then print_endline ("mult_fast model: "^model); mult_config model)
             (module Signal)
             (mux2' (nega) (to_signal (signednegate a_sig)) (to_signal a_sig))
             (mux2' (negb) (to_signal (signednegate b_sig)) (to_signal b_sig))
          in (of_signal (mux2' (nega ^: negb) (arithnegate mult') mult'))

let sim_mult n_bits =
  let circuit =
    Circuit.create_exn
      ~name:"lfsr"
      [ output "o" (mult_fast "" (input "a" n_bits) (input "b" n_bits))]
  in
  let sim = Cyclesim.create circuit in
  let a = Cyclesim.in_port sim "a" in
  let b = Cyclesim.in_port sim "b" in
  fun () ->
    for i = 0 to (1 lsl n_bits) - 1 do
      for j = 0 to (1 lsl n_bits) - 1 do
        a := Bits.of_int ~width:n_bits i;
        b := Bits.of_int ~width:n_bits j;
        Cyclesim.cycle sim
      done
    done

let relational relation lhs rhs =
let wlhs = width lhs in
let wrhs = width rhs in
if wlhs = wrhs then
Sig (relation lhs rhs)
else if wlhs < wrhs then
Sig (relation (uresize lhs wrhs) rhs)
else if wlhs > wrhs then
Sig (relation lhs (uresize rhs wlhs))
else failwith "relational"

let relationalc relation lhs rhs = print_endline ("sll by: "^string_of_int rhs); Sig (relation lhs rhs)
let relationalc' relation lhs rhs = Sigs (relation lhs rhs)

let relational' relation lhs rhs =
let wlhs = signedwidth lhs in
let wrhs = signedwidth rhs in
if wlhs = wrhs then
Sigs (relation lhs rhs)
else if wlhs < wrhs then
Sigs (relation (Signed.resize lhs wrhs) rhs)
else if wlhs > wrhs then
Sigs (relation lhs (Signed.resize rhs wlhs))
else failwith "relational'"

let othsel = ref None

let mux' sel inputs =
let wid = width inputs in
othsel := Some sel;
if false then print_endline ("mux width: "^string_of_int wid);
mux sel (List.init wid (bit inputs))

let assign' (lhs:Hardcaml.Always.Variable.t) rhs =
let wlhs = width lhs.value in
let wrhs = width rhs in
if wlhs = wrhs then
lhs <-- rhs
else if wlhs < wrhs then
lhs <-- (uresize rhs wlhs)
else if wlhs > wrhs then
lhs <-- (uresize rhs wlhs)
else failwith "assign'"

let summary = function
      | Con x -> Con_
      | Var _ -> Var_
      | Sig _ -> Sig_
      | Sigs _ -> Sigs_
      | Itm _ -> Itm_
      | Alw _ -> Alw_
      | Invalid -> Invalid_

let othtok = ref INVALID

let arithopvpi = function
| Aadd model -> Add model
| Asub model -> Sub model
| Amul model -> Mult model
| Amuls model -> Mults model
| Adiv -> Div
| Adivs -> Divs
| Amod -> Mod
| Amods -> Mods
| Apow -> Pow
| Apows -> Pows
| Astring -> failwith "Astring"
| Aenum -> failwith "Aenum"
| Asyscall -> failwith "Asyscall"

let logopvpi = function
| Lunknown -> failwith "Lunknown"
| Land -> And
| Lredand -> LogAnd
| Lrednand -> LogNand
| Lor -> Or
| Lredor -> LogOr
| Lrednor -> LogNor
| Lxor -> Xor
| Lxnor -> Xor
| Lredxor -> LogXor
| Lredxnor -> LogXnor
| Lshiftl -> LshiftL
| Lshiftr -> LshiftR
| Lshiftrs -> AshiftR

let unaryvpi = function
| Unknown -> failwith "Unknown"
| Unot -> Not
| Ulognot -> LogNot
| Usigned -> Signed
| Unegate -> Negate
| Uunsigned -> Unsigned
| Uextend(int1, int2) -> Unsigned
| Uextends(string, int1, int2) -> Signed

let cnv_op' oplst k = function
        | Var v -> oplst := output k v.value :: !oplst; if false then print_string "*"
        | Itm v -> ()
        | Con v -> ()
        | Alw v -> ()
        | Sig v -> ()
        | Sigs v -> ()
        | Invalid -> ()

let cnv_op oplst k x = let rslt = cnv_op' oplst k x in if false then print_endline ("\tcnv_op: "^k); rslt

let cnv (modnam, modul) =
begin
let declare_lst = ref [] in
let exists k = List.mem_assoc k !declare_lst in
let add_decl k x = function
| Width(hi,lo,signed) as wid ->
  if exists k then print_endline (k^": redeclared") else
  begin
    declare_lst := (k, x) :: !declare_lst;
    othdecl := (k, (wid, summary x)) :: !othdecl
  end
| oth -> otht := Some oth; failwith "add_decl" in

let declare_input port = function
| Width(hi,lo,signed) as w ->
  if false then print_endline ("Input: "^port^"["^string_of_int hi^":"^string_of_int lo^"]");
  let s = Signal.input port (hi-lo+1) in
  add_decl port (if signed then Sigs (Signed.of_signal s) else Sig s) w
| oth -> otht := Some oth; failwith "declare_input" in

let declare_wire wire = function
| Width(hi,lo,signed) as wid -> let wid' = hi-lo+1 in
  if false then print_endline ("Wire: "^wire^"["^string_of_int hi^":"^string_of_int lo^"]");
  add_decl wire (Var (Always.Variable.wire ~default:(Signal.zero wid'))) wid
| oth -> otht := Some oth; failwith "declare_wire" in

let find_decl k = if not (List.mem_assoc k !declare_lst) then Input_dump.tran_search modul declare_input declare_wire k; List.assoc k !declare_lst in
(* let iter_decl fn = List.iter (fun (k,x) -> fn k x) !declare_lst in *)

let unsigned x = Signed.to_signal x in

let sig' = function
| Con x -> Signal.of_constant x
| Sig x -> x
| Sigs x -> unsigned x
| Var x -> x.value
| oth -> othr := oth; failwith "sig'" in

let alw' = function
| Alw x -> x
| oth -> othr := oth; failwith "alw'" in

let var' = function
| Var x -> x
| oth -> othr := oth; failwith "var'" in
(*
let declare_orphan = function
| TUPLE2 (Vpitypespec, TUPLE3 (Ref_typespec, TLIST pth,
					     TUPLE2 (Vpiactual, TUPLE2 (Logic_typespec, (Width(hi, lo, signed) as wid))))) ->
  let wire = List.hd (List.rev (List.map (function Input.STRING s -> s | oth -> failwith "declare_orphan") pth)) in
  let wid' = hi-lo+1 in
 let s = Signal.input wire (hi-lo+1) in
  add_decl wire (if signed then Sigs (Signed.of_signal s) else Sig s) wid
| oth -> othrw := oth; failwith "declare_orphan" in
*)
let declare_option enable width = function
    | Some r_sync ->
      Always.Variable.reg ~enable r_sync ~width
    | None -> Always.Variable.wire ~default:(Signal.zero width) in

let declare_reg attr reg =
  if exists reg then () else
  begin
  let wid' _ = function
    | Width(hi,lo,signed) as wid ->
      add_decl reg (Var (declare_option attr.enable (hi-lo+1) attr.r_sync)) wid
    | _ -> failwith "declare_reg" in
  Input_dump.tran_search modul wid' wid' reg;
  end in

let compare lft rght = function
| Ceq -> Dyadic(Eq, lft, rght)
| Cneq -> Dyadic(Ne, lft, rght)
| Cgt -> Dyadic(Gt, lft, rght)
| Cgts -> Dyadic(Gts, lft, rght)
| Cgte -> Dyadic(Ge, lft, rght)
| Cgtes -> Dyadic(Ges, lft, rght)
| Ceqwild -> Dyadic(Gewild, lft, rght)
| Cneqwild -> Dyadic(Newild, lft, rght)
| Cltes -> Dyadic(Les, lft, rght)
| Clte -> Dyadic(Le, lft, rght)
| Clt -> Dyadic(Lt, lft, rght)
| Clts -> Dyadic(Lts, lft, rght)
| Cunknown as oth -> othcmp := oth; failwith "compare" in

let signed_id id =
  let wid' sgn _ = function
    | Width(hi, lo, false) -> if false then print_endline ("VRF unsigned "^id); sgn := Ident id
    | Width(hi, lo, true) -> if false then print_endline ("VRF signed "^id); sgn := Unary (Signed, Ident id)
    | _ -> failwith "signed_id" in
  let sgn = ref (Void 399) in Input_dump.tran_search modul (wid' sgn) (wid' sgn) id;
  (match !sgn with Void _ -> failwith id | oth -> oth) in

let rec tranitm attr = function
| UNKNOWN -> Void 402
| VRF (id, (_, _, _, [TYPSIGNED]), []) -> if attr.dest then declare_reg attr id; Unary (Signed, Ident id)
| VRF (id, _, []) -> if attr.dest then declare_reg attr id; signed_id id
| ASGN (bool, str2, src::dst::[]) -> Asgn((tranitm {attr with dest=true}) dst, (tranitm attr) src)
| ASGN (bool, str2, rw_lst) -> failwith "ASGN"
| ARITH (arithop, lft::rght::[]) as a -> otharith := a; Dyadic(arithopvpi arithop, (tranitm attr) lft, (tranitm attr) rght)
| ARITH (arithop, _) as a -> otharith := a; failwith "ARITH"
| LOGIC (logop, lft::rght::[]) -> Dyadic(logopvpi logop, (tranitm attr) lft, (tranitm attr) rght)
| LOGIC (logop, rght::[]) -> Unary(logopvpi logop, (tranitm attr) rght)
| LOGIC (logop, _) -> failwith "LOGIC"
| UNRY (unaryop, rght::[]) -> Unary(unaryvpi unaryop, (tranitm attr) rght)
| IF (str1, cond::then_::else_::[]) -> If_ ((tranitm attr) cond, (tranitm attr) then_::[], (tranitm attr) else_::[])
| IF (str1, cond::then_::[]) -> If_ ((tranitm attr) cond, (tranitm attr) then_::[], [])
| IF (str1, rw_lst) -> failwith "IF"
| CND (str1, cond::lft::rght::[]) -> Mux2 (tranitm attr cond, tranitm attr lft, tranitm attr rght)
| CND (str1, rw_lst) -> failwith "CND"
| CAT (str1, rw_lst) -> Concat (List.map (tranitm attr) rw_lst)
| SEL (str1, nam::CNST (_, HEX lo)::CNST (w', HEX wid)::[]) -> if wid == 1 then Bitsel(tranitm attr nam, Hex(string_of_int lo,w')) else
    Selection (tranitm attr nam, lo+wid-1, lo, 0, 0)
| SEL (str1, rw_lst) as sel -> othsel' := sel; failwith "SEL"
| CMP (cmpop, lft::rght::[]) -> compare (tranitm attr lft) (tranitm attr rght) cmpop
| CMP (cmpop, rw_lst) -> failwith "CMP"
| IRNG (str1, rw_lst) as irng -> othirng := irng; failwith "IRNG"
| CNST (w, HEX n) -> Hex (string_of_int n, w)
| CNST (w, SHEX n) -> Unary(Signed, Hex (string_of_int n, w)) (* TBC *)
| BGN (_, rw::[]) -> tranitm attr rw
| BGN (_, rw_lst) -> Seq (List.map (tranitm attr) rw_lst)
(*
| BGN (_, rw_lst) -> failwith "BGN"
*)
| CS (str1, expr :: cslst) as cs -> othcs := Some cs; let expr' = tranitm attr expr in
Case (expr', List.map (function
  | CSITM ("", (CNST _ as cexp) :: stmt :: []) -> Item (tranitm attr cexp, tranitm attr stmt)
  | CSITM ("", stmt :: []) -> Default (tranitm attr stmt)
  | oth -> othcs := Some oth; failwith "Case") cslst)
| CS (str1, _) as cs -> othcs := Some cs; failwith "CS"
(*
| CNST (_, BIN n) -> (String.make 1 n)
| CNST (_, SHEX n) -> (string_of_int n)
| CNST (w, BIGINT n) -> hex_of_bigint w n
| CNST (_, STRING s) -> String.map (function '0'..'9' | 'a'..'f' | 'A'..'F' as ch -> ch | oth -> '_') s
| CNST (_, FLT f) -> (string_of_float f)
| CNST (_, ERR err) -> failwith "ERR"
*)
| CNST (_, err) as c -> othc := c; failwith "CNST"
| XML (rw_lst) -> failwith "XML"
| EITM (str1, str2, str3, int2, rw_lst) -> failwith "EITM"
| IO (str1, str2lst, typ2, dirop, str3, rw_lst) -> failwith "IO"
| VAR (str1, str2lst, typ2, str3) -> failwith "VAR"
| IVAR (str1, str2, typ2, rw_lst, int3) -> failwith "IVAR"
| VRF (str1, typ', rw_lst) -> failwith "VRF"
| TYP (idx, (typenc, str1, typmap, typ_lst)) -> failwith "TYP"
| FNC (str1, str2, typ2, rw_lst) -> failwith "FNC"
| TASKDEF (str1, str2, rw_lst) -> failwith "TASKDEF"
| TASKRF (str1, str2, rw_lst) -> failwith "TASKRF"
| INST (str1, kind, str2lst, (str3, rw_lst)) -> failwith "INST"
| SFMT (str1, rw_lst) -> failwith "SFMT"
| SYS (str1, str2, rw_lst) -> failwith "SYS"
| TPLSRGS (str1, str2, int2, rw_lst) -> failwith "TPLSRGS"
| VPLSRGS (str1, int2, rw_lst) -> failwith "VPLSRGS"
| PORT (str1, str2, dirop, rw_lst) -> failwith "PORT"
| CA (str1, rw_lst) -> failwith "CA"
| UNRY (unaryop, rw_lst) -> failwith "UNRY"
| ASEL (rw_lst) -> failwith "ASEL"
| SNITM (str1, rw_lst) -> failwith "SNITM"
| FRF (str1, str2, rw_lst) -> failwith "FRF"
| XRF (str1, str2, str3, str4, dirop) -> failwith "XRF"
| PKG (str1, str2, rw_lst) -> failwith "PKG"
| CPS (str1, rw_lst) -> failwith "CPS"
| REPL (str1, int2, rw_lst) as r -> othrepl := r; failwith "REPL"
| MODUL (str1, str2, rw_lst, tmp) -> failwith "MODUL"
| RNG (rw_lst) -> failwith "RNG"
| ALWYS (str1, rw_lst) -> failwith "ALWYS"
| SNTRE (rw_lst) -> failwith "SNTRE"
| INIT (str1, str2, rw_lst) -> failwith "INIT"
| IFC (str1, str2, rw_lst) -> failwith "IFC"
| IMP (str1, str2, rw_lst) -> failwith "IMP"
| IMRF (str1, str2, dir, rw_lst) -> failwith "IMRF"
| JMPL (str1, rw_lst) -> failwith "JMPL"
| JMPG (str1, rw_lst) -> failwith "JMPG"
| JMPBLK (str1, rw_lst) -> failwith "JMPBLK"
| CSITM (str1, rw_lst) -> failwith "CSITM"
| WHL (rw_lst) -> failwith "WHL"
| FORSTMT _ -> failwith "FORSTMT"
| ARG (rw_lst) -> failwith "ARG"
| DSPLY (str1, str2, rw_lst) -> failwith "DSPLY"
| FILS (str1, rw_lst) -> failwith "FILS"
| FIL (str1, str2) -> failwith "FIL"
| NTL (rw_lst) -> failwith "NTL"
| CELLS (rw_lst, attr) -> failwith "CELLS"
| CELL (str1, str2, str3, str4, rw_lst) -> failwith "CELL"
| POSPOS (str1, str2) -> failwith "POSPOS"
| POSNEG (str1, str2) -> failwith "POSNEG"
| NEGNEG (str1, str2) -> failwith "NEGNEG"
| POSEDGE (str1) -> failwith "POSEDGE"
| NEGEDGE (str1) -> failwith "NEGEDGE"
| COMB -> failwith "COMB"
| MODPORTFTR (str1, str2) -> failwith "MODPORTFTR"
| TYPETABLE arr -> failwith "TYPETABLE"
| TIM _ -> failwith "TIM"
| SCOPE tid -> failwith "SCOPE"
| ITM _ -> failwith "ITM"
| CONTAINER (itms, top) -> failwith "CONTAINER"
| CONSPACK (_, _) -> failwith "CONSPACK"
| CONSPACKMEMB (_, _) -> failwith "CONSPACKMEMB"
in

let _ = List.iter (fun (io,_ as args) -> if not (exists io) then Input_dump.iofunc modul declare_input (fun _ _ -> ()) args) (List.rev !(modul.io)) in

let alwystran = List.flatten (List.map (function
  | ("", COMB, (SNTRE [] :: lst)) -> let attr = {enable=Signal.vdd; r_sync=None; dest=false} in List.map (tranitm attr) lst
  | ("", COMB, ((ASGN _|BGN _) as stmt :: [])) -> let attr = {enable=Signal.vdd; r_sync=None; dest=false} in [tranitm attr stmt]
  | ("", POSEDGE clk, lst) -> let attr = {enable=Signal.vdd; r_sync=Some (Reg_spec.create ~clock:(sig' (find_decl clk)) ()); dest=false} in List.map (tranitm attr) lst
  | ("", POSPOS (clk, rst), lst) -> let attr = {enable=Signal.vdd; r_sync=Some (Reg_spec.create ~clock:(sig' (find_decl clk)) ~reset:(sig' (find_decl rst)) ()); dest=false} in List.map (tranitm attr) lst
  
  | oth -> othalwystran := oth; (* Verible_typ.fail_json othalw_to_yojson oth; *) failwith "alwystran") !(modul.alwys)) in

let catran = List.map (function
  | ("", dst, rhs) -> let attr = {enable=Signal.vdd; r_sync=None; dest=false} in Asgn((tranitm {attr with dest=true}) dst, tranitm attr rhs)
  | oth -> othcatran := Some oth; failwith "catran") !(modul.ca) in

let _ = List.iter (fun (io,_ as args) -> if not (exists io) then Input_dump.iofunc modul (fun _ _ -> ()) declare_wire args) !(modul.io) in

let _ = List.iter (fun (v,_ as args) -> if not (exists v) then Input_dump.vfunc modul declare_input declare_wire args) !(modul.v) in

let remapp' = List.filter (function Void _ -> false |_ -> true) (alwystran@catran) in

let strength_reduce = function
| Asgn (Ident c, 
  If_ (cond, then_, else_)) ->
  If_ (cond, List.map (fun itm -> Asgn (Ident c, itm)) then_, List.map (fun itm -> Asgn (Ident c, itm)) else_)
| oth -> oth in

let rec combiner = function
| [] -> []
| If_ (Dyadic (Eq, a, b), c, d) ::
  If_ (Dyadic (Eq, a', b'), c', d') :: [] when a=a' && b=b' -> If_ (Dyadic (Eq, a, b), c@c', d@d') :: []
| If_ (Dyadic _ as a, b, c) :: tl -> If_ ( a, combiner b, combiner c) :: combiner tl
| Asgn (_, Void 540) :: tl -> combiner tl
| hd :: tl -> hd :: combiner tl in

let remapp'' = List.map strength_reduce remapp' in
let remapp''' = combiner remapp'' in

let unimp = let seen = ref [] in fun lbl lhs rhs -> 
if not (List.mem lbl !seen) then print_endline ("unimp: "^lbl); seen := lbl :: !seen; lhs ^: rhs in

let unimps = let seen = ref [] in fun lbl lhs rhs -> let open Signed in
if not (List.mem lbl !seen) then print_endline ("unimps: "^lbl); seen := lbl :: !seen; of_signal (to_signal lhs ^: to_signal rhs) in

let lognegate fn = fun lhs rhs -> fn lhs (~: rhs) in

let signed_relational x = let open Signed in match x with
|Eq -> (fun lhs rhs -> Signed.of_signal (equal (Signed.to_signal rhs) (Signed.to_signal lhs)))
|Ne -> (fun lhs rhs -> Signed.of_signal (notequal (Signed.to_signal rhs) (Signed.to_signal lhs)))
|Lt -> less_signed
|Le -> less_equal_signed
|Ge -> greater_equal_signed
|Gt -> greater_signed
|LshiftL -> (fun lhs rhs -> Signed.of_signal (Signal.log_shift sll (Signed.to_signal lhs) (Signed.to_signal rhs)))
|LshiftR -> (fun lhs rhs -> Signed.of_signal (Signal.log_shift srl (Signed.to_signal lhs) (Signed.to_signal rhs)))
|AshiftR -> (fun lhs rhs -> Signed.of_signal (Signal.log_shift sra (Signed.to_signal lhs) (Signed.to_signal rhs)))
|Add model -> (fun lhs rhs -> Signed.of_signal (add_fast model (Signed.to_signal rhs) (Signed.to_signal lhs)))
|Sub model -> (fun lhs rhs -> Signed.of_signal (sub_fast model Signal.vdd (Signed.to_signal rhs) (Signed.to_signal lhs)))
|Mult model -> mult_fast_signed model
|Mults model -> mult_fast_signed model
|Div -> (fun lhs rhs -> Signed.of_signal (div_signed (Signed.to_signal rhs) (Signed.to_signal lhs)))
|Mod -> (fun lhs rhs -> Signed.of_signal (mod_signed (Signed.to_signal rhs) (Signed.to_signal lhs)))
|Pow -> unimps "power"
|LogAnd -> (fun lhs rhs -> of_signal (to_signal lhs &&: to_signal rhs))
|LogOr -> (fun lhs rhs -> of_signal (to_signal lhs ||: to_signal rhs))
|And -> (fun lhs rhs -> of_signal (to_signal lhs &: to_signal rhs))
|Or -> (fun lhs rhs -> of_signal (to_signal lhs |: to_signal rhs))
|Xor -> (fun lhs rhs -> of_signal (to_signal lhs ^: to_signal rhs))
|Xnor -> (fun lhs rhs -> of_signal ( ~: (to_signal lhs ^: to_signal rhs)))
|oth -> othr' := oth; failwith "signed_relational" in

let unsigned_relational = function
|Eq -> equal 
|Ne -> notequal
|Lt -> less_fast 
|Le -> less_equal_fast
|Ge -> greater_equal_fast
|Gt -> greater_fast
|LshiftL -> Signal.log_shift sll
|LshiftR -> Signal.log_shift srl
|AshiftR -> Signal.log_shift sra
|Add model  -> add_fast model
|Sub model -> sub_fast model Signal.vdd
|Mult model -> mult_fast model
|Div -> div_unsigned
|Mod -> unimp "mod"
|Pow -> unimp "power"
|LogAnd -> (&&:) 
|LogOr -> (||:) 
|And -> (&:) 
|Or -> (|:) 
|Xor -> (^:) 
|Xnor -> lognegate (^:)
|oth -> othr' := oth; failwith "unsigned_relational" in

let unsigned_relationalc = function
|LshiftL -> Signal.sll
|LshiftR -> Signal.srl
|AshiftR -> Signal.sra
|oth -> othr' := oth; failwith "unsigned_relationalc" in

let signed_relationalc x = match x with
|LshiftL -> (fun lhs rhs -> Signed.of_signal (Signal.sll (Signed.to_signal lhs) rhs))
|LshiftR -> (fun lhs rhs -> Signed.of_signal (Signal.srl (Signed.to_signal lhs) rhs))
|AshiftR -> (fun lhs rhs -> Signed.of_signal (Signal.sra (Signed.to_signal lhs) rhs))
|oth -> othr' := oth; failwith "unsigned_relationalc" in

let _detect_dyadic = function
(*
 | (Vpirhs as op),Sig lhs, Sig rhs -> othop := (op, summary (Sig lhs), summary (Sig rhs)); failwith "detect_dyadic_rhs"
*)
| (Mults _|Divs|Mods as op), Sig lhs, Sig rhs -> relational' (signed_relational op) (Signed.of_signal lhs) (Signed.of_signal rhs)
| op,Sigs lhs, Sigs rhs -> relational' (signed_relational op) lhs rhs
| op,Sig lhs, Sig rhs -> relational (unsigned_relational op) lhs rhs
| op,Sigs lhs, Sig rhs -> relational (unsigned_relational op) (Signed.to_signal lhs) rhs
| op,Sig lhs, Sigs rhs -> relational (unsigned_relational op) lhs (Signed.to_signal rhs)
| (Sub _ as op), Sig lhs, Con rhs -> relational (unsigned_relational op) lhs (Signal.of_constant rhs)
| op, Con lhs, Var rhs -> relational (unsigned_relational op) (Signal.of_constant lhs) rhs.value
| op, Con lhs, Sig rhs -> relational (unsigned_relational op) (Signal.of_constant lhs) rhs
| op, Con lhs, Sigs rhs -> relational' (signed_relational op) (Signed.of_signal (Signal.of_constant lhs)) rhs
| (LshiftL|LshiftR|AshiftR as op), Sig lhs, Con rhs -> relationalc (unsigned_relationalc op) lhs (Constant.to_int rhs)
| op,Sig lhs, Con rhs -> relational (unsigned_relational op) lhs (Signal.of_constant rhs)
| op,Sigs lhs, Con rhs -> relationalc' (signed_relationalc op) lhs (Constant.to_int rhs)
| (LshiftL|LshiftR|AshiftR as op), Var lhs, Con rhs -> relationalc (unsigned_relationalc op) lhs.value (Constant.to_int rhs)
| op, Var lhs, Con rhs -> relational (unsigned_relational op) lhs.value (Signal.of_constant rhs)
| (LshiftL|LshiftR|AshiftR as op), Con lhs, Con rhs -> relationalc (unsigned_relationalc op) (Signal.of_constant lhs) (Constant.to_int rhs)
| op, Var lhs, Sig rhs -> relational (unsigned_relational op) lhs.value rhs
| op, Var lhs, Var rhs -> relational (unsigned_relational op) lhs.value rhs.value
| op,lhs,rhs -> othop := (op, summary lhs, summary rhs); failwith "detect_dyadic" in

let detect_dyadic (op, lhs,rhs) = othop := (op, summary lhs, summary rhs); _detect_dyadic (op, lhs, rhs) in

(*
let unimplu = let seen = ref [] in fun lbl rhs ->
if not (List.mem lbl !seen) then print_endline ("unimplu: "^lbl); seen := lbl :: !seen; ~: rhs in
*)

let remapunary' = function
|Add _ -> (fun rhs -> rhs)
|Sub _ -> arithnegate
|And -> fold' (&:)
|Nand -> fold' (lognegate (&:))
|Or -> fold' (|:)
|Nor -> fold' (lognegate (|:))
|Xor -> fold' (^:)
|Xnor -> fold' (lognegate (^:))
|Lneg -> (~:)
|Not -> (~:)
|LogAnd -> fold' (&:)
|LogNand -> fold' (lognegate (&:))
|LogOr -> fold' (|:)
|LogNor -> fold' (lognegate (|:))
|LogXor -> fold' (^:)
|LogXnor -> fold' (lognegate (^:))
|LogNot -> (~:)
|Negate -> fold' (-:)
|oth -> othr' := oth; failwith "remapop'" in

let rec radix = function
| Dec (n, w) -> int_of_string n
| Hex (s, w) -> Scanf.sscanf s "%x" (fun h -> h)
| Unary (Signed, r) -> radix r
| oth -> othradix := oth; failwith "radix" in

let rec (remap:remapp->remap) = function
| Ident wire -> if exists wire then find_decl wire else Invalid
| Unary (Signed, x) -> (match remap x with Sig x -> Sigs (Signed.of_signal x) | Sigs _ as x -> x | _ -> failwith "Signed")
| Unary (Unsigned, x) -> (match remap x with Sigs x -> Sig (Signed.to_signal x) | Sig _ as x -> x | _ -> failwith "Unsigned")
| Unary (op, rhs) -> Sig (remapunary' op (sig' (remap rhs)))
| Dyadic (op, lhs, rhs) -> let lhs = remap lhs and rhs = remap rhs in detect_dyadic (op,lhs, rhs)
| Mux2 (cond, lhs, rhs) ->
  othcond' := cond;
  othlhs' := lhs;
  othrhs' := rhs;
  othrmcond' := remap cond;
  othrmlhs' := remap lhs;
  othrmrhs' := remap rhs;
  let cond_ = sig' (remap cond) in
  let then_ = sig' (remap lhs) in
  let else_ = sig' (remap rhs) in
  Sig (mux2' cond_ then_ else_)
| If_ (cond, lhs, rhs) ->
  othcond' := cond;
  let cond_ = sig' (remap cond) in
  let then_ = List.map (fun itm -> alw' (remap itm)) lhs in
  let else_ = List.map (fun itm -> alw' (remap itm)) rhs in
  Alw (if_ cond_ then_ else_)
| Asgn (Update(lhs, lft, rght, hi, lo), rhs) ->
   othlhs' := lhs;
   othrhs' := rhs;
   othrmlhs' := remap lhs;
   othrmrhs' := remap rhs;
   let lhs = var' (remap lhs) in
   let rhs = sig' (remap rhs) in
   let rhs' = rhs @: select lhs.value lft rght in (* placeholder, not working yet *)
   Alw (assign' lhs rhs')
| Asgn (lhs, rhs) ->
   othlhs' := lhs;
   othrhs' := rhs;
   othrmlhs' := remap lhs;
   othrmrhs' := remap rhs;
   let lhs = var' (remap lhs) in
   let rhs = sig' (remap rhs) in
   Alw (assign' lhs rhs)
| Hex (s,width) -> Con (Constant.of_hex_string ~width ~signedness:Unsigned s)
| Dec (s,width) -> Con (Constant.of_z ~width (Z.of_string s))
| Oct (s,width) -> Con (Constant.of_octal_string ~width ~signedness:Unsigned s)
| Bin (s,width) -> Con (Constant.of_binary_string_hum s)
| Concat lst -> Sig (Signal.concat_msb (List.map (fun itm -> sig' (remap itm)) lst))
| Selection(nam, lft, rght, hi, lo) -> Sig (select (sig' (remap nam)) (lft) (rght))
| Bitsel(nam, Dec (n, _)) -> Sig (bit (sig' (remap nam)) (int_of_string n))
| Bitsel(nam, sel) as b -> othr' := b; Sig (mux' (sig' (remap sel)) (sig' (remap nam)))
| Item (r, stmt) as itm -> othr' := itm; failwith "remap Item othr'"
| Default (stmt) as itm -> othr' := itm; failwith "remap Default othr'"
| Case (cond, lst) as itm -> othc' := itm; let cond' = sig' (remap cond) in let wid = width cond' in
  let cases, dflt = List.partition (function Default _ -> false | _ -> true) lst in
  let dflt' = List.map (function Default (stmt) -> [ alw' (remap stmt) ] | _ -> failwith "dflt") dflt in
  othcse := cases;
  othdflt := dflt;
  Alw (Always.switch cond' (match dflt' with
    | default::_ ->
        let bitmap = Array.init (1 lsl wid) (fun _ -> default) in
        List.iter (function
		   | Item (Hex (s,wid), stmt) -> bitmap.(int_of_string s) <- [ alw' (remap stmt) ]
                   | oth -> othr' := oth; failwith "itm") cases;
        List.mapi (fun ix itm -> of_int ~width:wid ix, itm) (Array.to_list bitmap);
    | [] -> List.map (function
		      | Item (r, stmt) -> of_int ~width:wid (radix r), [ alw' (remap stmt) ]
                      | oth -> othr' := oth; failwith "itm") cases))
| Seq lst -> Alw (Always.proc (List.map (fun itm -> alw' (remap itm)) lst))
| oth -> othr' := oth; failwith "remap othr'" in

if false then print_endline ("remapp' size = "^string_of_int (List.length remapp'));
othremapp' := remapp';
othremapp'' := remapp'';
othremapp''' := remapp''';
let remap' = List.filter (function Invalid -> false | Sig _ -> false |_ -> true) (List.map remap remapp''') in
let remap'' = List.map alw' remap' in
let _ = Always.compile remap'' in
let oplst = ref [] in
List.iter (Input_dump.iofunc modul (fun _ _ -> ()) (fun io _ -> cnv_op oplst io (find_decl io))) !(modul.io);
let rtl = Buffer.create 65535 in
Hardcaml.Rtl.output ~output_mode:(Hardcaml.Rtl.Output_mode.To_buffer rtl) Verilog (Hardcaml.Circuit.create_exn ~name:modnam !oplst);
Buffer.contents rtl;
end
