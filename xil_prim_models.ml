(* xil_prim_models.ml — self-contained formal models of the Xilinx 7-series
 * primitives that appear in the open-flow JSON netlist, synthesised directly
 * as Behavioral IR.  No Vivado, no unisim VHDL: this is the no-Vivado
 * replacement for Vhdl_to_behavioral.lookup_xil_primitive_impl.
 *
 * Ported from the validated reference models in
 *   nextpnr-xilinx/xilinx/examples/counter25/formal/prims.py
 * (truth-table self-tested there: LUT2 0110=XOR, LUT5 1000..=AND5,
 *  LUT6 0..01=NOR6, FDRE sync-R, FDSE sync-S).
 *
 * Why a whole pass and not one body per cell type: flatten_for_z3 inlines a
 * primitive body WITHOUT parameter substitution and caches it by module_name.
 * A generic `LUT6` body would therefore be shared across instances with
 * different INITs — wrong.  So augment_program SPECIALISES each instance:
 * it bakes the instance's INIT into a uniquely-named body (`LUT6__<init>`),
 * rewrites the binstance to point at it, and appends the body.  Instances
 * with identical INIT share one body (dedup by name → cache stays warm). *)

open Behavioral_ir

(* 1-bit unsigned — every primitive pin in these models is scalar. *)
let b1 = BInt { width = 1; signed = Unsigned }

let v n          = BVar n
let c0           = BConst { value = Z.zero; width = 1 }
let c1           = BConst { value = Z.one; width = 1 }
let notb e       = BUnOp  { op = BNot; operand = e; result_type = b1 }
let andb a b     = BBinOp { op = BAnd; lhs = a; rhs = b; result_type = b1 }
let orb  a b     = BBinOp { op = BOr;  lhs = a; rhs = b; result_type = b1 }

let sig_ name dir ?init () =
  { name; stype = b1; direction = dir; initial_value = init; attrs = [] }

let empty_mod name signals processes =
  { name; params = []; signals; processes;
    instances = []; funcs = []; mems = []; attrs = [] }

(* ---------------------------------------------------------------------- *)
(* INIT parameter normalisation                                           *)
(*                                                                        *)
(* Returns a binary string of length 2^k, MSB-first (so character index   *)
(* (len-1-m) is the output for minterm m, m = sum_i I_i*2^i, I0 the LSB).  *)
(* Accepts: a plain binary string ("0110"), a Verilog sized literal       *)
(* ("64'h...."/"6'b..."), a bare hex ("0x.."/"h.."), or a decimal.        *)
(* ---------------------------------------------------------------------- *)
let pad_bin n s =
  let len = String.length s in
  if len = n then s
  else if len < n then String.make (n - len) '0' ^ s
  else String.sub s (len - n) n        (* take the low n bits, MSB-first *)

let hex_to_bin h =
  let buf = Buffer.create (String.length h * 4) in
  String.iter (fun ch ->
    let d = match ch with
      | '0'..'9' -> Char.code ch - Char.code '0'
      | 'a'..'f' -> Char.code ch - Char.code 'a' + 10
      | 'A'..'F' -> Char.code ch - Char.code 'A' + 10
      | _ -> -1 in
    if d >= 0 then
      for b = 3 downto 0 do
        Buffer.add_char buf (if (d lsr b) land 1 = 1 then '1' else '0')
      done) h;
  Buffer.contents buf

let is_all c s = s <> "" && String.for_all (fun ch -> ch = c || ch = '0' || ch = '1') s
let only_binary s = s <> "" && String.for_all (fun ch -> ch = '0' || ch = '1') s
let only_hex s = s <> "" && String.for_all (fun ch ->
  (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F')) s

let normalize_init ~k raw =
  ignore is_all;
  let n = 1 lsl k in
  let s = String.trim raw in
  (* strip a Verilog size/base prefix  <width>'<base><digits>  *)
  let body, base =
    match String.index_opt s '\'' with
    | Some i when i + 1 < String.length s ->
        let base = Char.lowercase_ascii s.[i+1] in
        (String.sub s (i+2) (String.length s - i - 2), base)
    | _ ->
        if String.length s > 1 && s.[0] = '0'
           && (s.[1] = 'x' || s.[1] = 'X')
        then (String.sub s 2 (String.length s - 2), 'h')
        else (s, '?')
  in
  let bin =
    match base with
    | 'b' -> body
    | 'h' -> hex_to_bin body
    | 'd' -> ""                                   (* fall through to int *)
    | _ ->
        if only_binary body && String.length body >= 2 then body
        else if only_hex body then hex_to_bin body
        else ""
  in
  let bin =
    if bin <> "" then bin
    else
      (* last resort: parse as an integer and render n bits *)
      (try
         let i = int_of_string (String.trim raw) in
         String.init n (fun p -> if (i lsr (n - 1 - p)) land 1 = 1 then '1' else '0')
       with _ -> String.make n '0')
  in
  pad_bin n bin

(* fetch a parameter from either the int or the string table *)
let param_lookup (i : binstance) key =
  match List.assoc_opt key i.param_strs with
  | Some s -> Some s
  | None ->
      (match List.assoc_opt key i.param_values with
       | Some v -> Some (string_of_int v)
       | None -> None)

let inverted (i : binstance) key =
  match param_lookup i key with
  | Some s -> let s = String.trim s in s = "1" || s = "1'b1" || s = "true" || s = "TRUE"
  | None -> false

(* sanitise an INIT string into a name fragment (identifier-safe) *)
let name_frag s =
  String.map (fun c -> if (c='0'||c='1') then c else '_') s

(* ---------------------------------------------------------------------- *)
(* LUTk : O = OR over minterms m (INIT[m]=1) of AND of input literals.     *)
(* ---------------------------------------------------------------------- *)
let lut_expr ~k ~init ins =
  let n = 1 lsl k in
  let terms = ref [] in
  for m = 0 to n - 1 do
    if init.[n - 1 - m] = '1' then begin
      let lits =
        List.mapi (fun idx e -> if (m lsr idx) land 1 = 1 then e else notb e) ins in
      let term = match lits with
        | [] -> c1
        | x :: xs -> List.fold_left andb x xs in
      terms := term :: !terms
    end
  done;
  match !terms with
  | [] -> c0
  | x :: xs -> List.fold_left orb x xs

let lut_body ~k ~init =
  let ins = List.init k (fun idx -> "I" ^ string_of_int idx) in
  let signals =
    List.map (fun n -> sig_ n `Input ()) ins @ [ sig_ "O" `Output () ] in
  let rhs = lut_expr ~k ~init (List.map v ins) in
  let proc = BCombinational
    { name = "comb"; sensitivity = [BAny];
      body = [ BAssign { lhs = "O"; rhs } ] } in
  let name = Printf.sprintf "LUT%d__%s" k (name_frag init) in
  empty_mod name signals [proc]

(* ---------------------------------------------------------------------- *)
(* Flip-flops : next-state function, modelled with an explicit hold so we  *)
(* do not rely on incomplete-assignment FF inference.                      *)
(*   FDRE: R sync-reset dominant, then CE.   FDSE: S sync-set dominant.     *)
(* ---------------------------------------------------------------------- *)
let ff_body ~ty ~set_val ~ctrl_pin ~init ~inv =
  let pin name = if List.mem name inv then notb (v name) else v name in
  let hold = BAssign { lhs = "Q"; rhs = v "Q" } in
  let load = BIf { condition = pin "CE";
                    then_stmts = [ BAssign { lhs = "Q"; rhs = pin "D" } ];
                    else_stmts = [ hold ] } in
  let body = [ BIf { condition = pin ctrl_pin;
                     then_stmts = [ BAssign { lhs = "Q";
                                              rhs = if set_val then c1 else c0 } ];
                     else_stmts = [ load ] } ] in
  let q_init = if String.trim init = "1" then Some c1 else Some c0 in
  let signals =
    [ sig_ "C" `Input (); sig_ "CE" `Input (); sig_ "D" `Input ();
      sig_ ctrl_pin `Input (); sig_ "Q" `Output ?init:q_init () ] in
  let proc = BSequential
    { name = "seq"; clock = "C"; clock_edge = `Pos;
      reset = None; reset_edge = None; reset_async = false;
      body; blocking_vars = [] } in
  let name = Printf.sprintf "%s__%s%s%s" ty
      (if set_val then "S1" else "R0")
      (if String.trim init = "1" then "_i1" else "_i0")
      (if inv = [] then "" else "_inv" ^ String.concat "" inv) in
  empty_mod name signals [proc]

(* Bare D flip-flops with NO clock-enable: FD (Q/C/D), FDP (+async PRE),
   FDC (+async CLR).  Q' = ctrl ? set_val : D.  The async/sync distinction
   is irrelevant to a cycle-based next-state miter (modelled the same on
   both flows, it cancels).  Used by the PCS sync_block / reset_sync. *)
let bare_ff_body ~ty ~ctrl ~set_val ~init ~inv =
  let pin name = if List.mem name inv then notb (v name) else v name in
  let load = BAssign { lhs = "Q"; rhs = pin "D" } in
  let body = match ctrl with
    | None -> [ load ]
    | Some cpin -> [ BIf { condition = pin cpin;
                           then_stmts = [ BAssign { lhs = "Q";
                                            rhs = if set_val then c1 else c0 } ];
                           else_stmts = [ load ] } ] in
  let q_init = if String.trim init = "1" then Some c1 else Some c0 in
  let signals =
    [ sig_ "C" `Input (); sig_ "D" `Input (); sig_ "Q" `Output ?init:q_init () ]
    @ (match ctrl with Some cpin -> [ sig_ cpin `Input () ] | None -> []) in
  let proc = BSequential
    { name = "seq"; clock = "C"; clock_edge = `Pos;
      reset = None; reset_edge = None; reset_async = false;
      body; blocking_vars = [] } in
  let name = Printf.sprintf "%s__%s%s" ty
      (if String.trim init = "1" then "i1" else "i0")
      (if inv = [] then "" else "_inv" ^ String.concat "" inv) in
  empty_mod name signals [proc]

(* ---------------------------------------------------------------------- *)
(* Buffers / constants : pure wire-through (or a tied value).              *)
(* ---------------------------------------------------------------------- *)
let buf_body ~name ~ins ~out ~rhs =
  let signals =
    List.map (fun n -> sig_ n `Input ()) ins @ [ sig_ out `Output () ] in
  let proc = BCombinational
    { name = "comb"; sensitivity = [BAny];
      body = [ BAssign { lhs = out; rhs } ] } in
  empty_mod name signals [proc]

(* ---------------------------------------------------------------------- *)
(* CARRY4: 4-bit carry chain.  unisim model: c0 = CI|CYINIT ;              *)
(*   CO[i] = S[i] ? carry_in[i] : DI[i] ;  O[i] = S[i] ^ carry_in[i]       *)
(* where carry_in = {CO[2],CO[1],CO[0],c0}.  Pure combinational.           *)
(* ---------------------------------------------------------------------- *)
let carry4_body () =
  let sig_w name w dir =
    { name; stype = BInt { width = w; signed = Unsigned };
      direction = dir; initial_value = None; attrs = [] } in
  let seli name i = BSlice { signal = v name; msb = i; lsb = i } in
  let xorb a b = BBinOp { op = BXor; lhs = a; rhs = b; result_type = b1 } in
  let mux se c d = BCond { condition = se; then_val = c; else_val = d } in
  let c0 = orb (v "CI") (v "CYINIT") in
  let co i = v (Printf.sprintf "co%d" i) in
  let carry_in = [| c0; co 0; co 1; co 2 |] in
  let assigns =
    List.init 4 (fun i ->
      BAssign { lhs = Printf.sprintf "co%d" i;
                rhs = mux (seli "S" i) carry_in.(i) (seli "DI" i) })
    @ [ BAssign { lhs = "O";
                  rhs = BConcat (List.rev (List.init 4 (fun i ->
                          xorb (seli "S" i) carry_in.(i)))) };
        BAssign { lhs = "CO"; rhs = BConcat [ co 3; co 2; co 1; co 0 ] } ] in
  let signals =
    [ sig_w "CI" 1 `Input; sig_w "CYINIT" 1 `Input;
      sig_w "DI" 4 `Input; sig_w "S" 4 `Input;
      sig_w "co0" 1 `Internal; sig_w "co1" 1 `Internal;
      sig_w "co2" 1 `Internal; sig_w "co3" 1 `Internal;
      sig_w "CO" 4 `Output; sig_w "O" 4 `Output ] in
  let proc = BCombinational { name = "comb"; sensitivity = [BAny]; body = assigns } in
  empty_mod "CARRY4" signals [proc]

(* ---------------------------------------------------------------------- *)
(* SRL16E / SRLC32E: static-length shift register in one LUT.              *)
(*   posedge CLK: if CE then sr <= {sr[W-2:0], D}  (D enters bit 0).        *)
(*   Q  = sr[addr]  (addr from the A pins; SRL16E has A0..A3, SRLC32E a     *)
(*        5-bit bus A);  SRLC32E also exposes Q31 = sr[31] for cascading.   *)
(* This is the exact next-state / read semantics srl_infer targets, so a    *)
(* depth-1 miter of {this model} vs {an explicit reg-chain} proves the pass *)
(* correct.  The internal register is named `sr` so a same-named RTL        *)
(* reference aligns under Behavioral_ffrip's by-name state matching.        *)
(* ---------------------------------------------------------------------- *)
let sig_wide name w dir ?init () =
  { name; stype = BInt { width = w; signed = Unsigned };
    direction = dir; initial_value = init; attrs = [] }

(* addr specification: individual 1-bit pins (LSB first) or a single bus. *)
type srl_addr = APins of string list | ABus of string * int

(* mux: select sr[addr] where addr = sum_i addr_bits[i]*2^i (LSB first). *)
let srl_select addr_bits =
  let k = List.length addr_bits in
  let bit = Array.of_list addr_bits in
  let rec go level base =
    if level < 0 then BSlice { signal = v "sr"; msb = base; lsb = base }
    else BCond { condition = bit.(level);
                 then_val = go (level - 1) (base + (1 lsl level));
                 else_val = go (level - 1) base } in
  go (k - 1) 0

let srl_body ~ty ~w ~addr ~q31 ~init =
  let init_z =
    if only_binary init && init <> "" then
      (try Z.of_string ("0b" ^ init) with _ -> Z.zero)
    else Z.zero in
  let sr_init = BConst { value = init_z; width = w } in
  (* address bit expressions (LSB first) + the address port signals *)
  let addr_bits, addr_sigs = match addr with
    | APins pins ->
        List.map v pins, List.map (fun p -> sig_ p `Input ()) pins
    | ABus (name, aw) ->
        List.init aw (fun i -> BSlice { signal = v name; msb = i; lsb = i }),
        [ sig_wide name aw `Input () ] in
  (* sequential shift: if CE then sr <= {sr[W-2:0], D} else sr <= sr *)
  let shift_rhs = BConcat [ BSlice { signal = v "sr"; msb = w - 2; lsb = 0 }; v "D" ] in
  let seq = BSequential
    { name = "seq"; clock = "CLK"; clock_edge = `Pos;
      reset = None; reset_edge = None; reset_async = false;
      body = [ BIf { condition = v "CE";
                     then_stmts = [ BAssign { lhs = "sr"; rhs = shift_rhs } ];
                     else_stmts = [ BAssign { lhs = "sr"; rhs = v "sr" } ] } ];
      blocking_vars = [] } in
  (* combinational read ports *)
  let read_assigns =
    BAssign { lhs = "Q"; rhs = srl_select addr_bits }
    :: (if q31 then [ BAssign { lhs = "Q31";
                                rhs = BSlice { signal = v "sr"; msb = w - 1; lsb = w - 1 } } ]
        else []) in
  let comb = BCombinational { name = "comb"; sensitivity = [BAny]; body = read_assigns } in
  let signals =
    [ sig_ "CLK" `Input (); sig_ "CE" `Input (); sig_ "D" `Input () ]
    @ addr_sigs
    @ [ sig_wide "sr" w `Internal ~init:sr_init ();
        sig_ "Q" `Output () ]
    @ (if q31 then [ sig_ "Q31" `Output () ] else []) in
  let name = Printf.sprintf "%s__%s" ty (name_frag init) in
  empty_mod name signals [seq; comb]

(* ---------------------------------------------------------------------- *)
(* RAM64M: four independent 64x1 memories (mem_a/b/c/d), a common write     *)
(* address (ADDRD, posedge WCLK when WE), and independent async reads:      *)
(*   posedge WCLK, WE:  mem_x[ADDRD] <= DIx   (x = a,b,c,d)                  *)
(*   DOA=mem_a[ADDRA]  DOB=mem_b[ADDRB]  DOC=mem_c[ADDRC]  DOD=mem_d[ADDRD]  *)
(* (Vivado unisim RAM64M.v).  Each memory is a 64-bit register: the read is  *)
(* a dynamic bit-select and the write a shift/mask update, so the model is a *)
(* plain FF+logic body the miter handles (ffrip lifts mem_x as state).       *)
(* ---------------------------------------------------------------------- *)
let ram64m_body ~inits =
  let w64 = BInt { width = 64; signed = Unsigned } in
  let bnot e = BUnOp  { op = BNot; operand = e; result_type = w64 } in
  let band a b = BBinOp { op = BAnd; lhs = a; rhs = b; result_type = w64 } in
  let bor  a b = BBinOp { op = BOr;  lhs = a; rhs = b; result_type = w64 } in
  let bshl a b = BBinOp { op = BShl; lhs = a; rhs = b; result_type = w64 } in
  let k64 z = BConst { value = z; width = 64 } in
  let zext1 e = BConcat [ BConst { value = Z.zero; width = 63 }; e ] in
  (* mem with bit [idx] set to [di] : (mem & ~(1<<idx)) | (di<<idx) *)
  let set_bit mem idx di =
    bor (band mem (bnot (bshl (k64 Z.one) idx))) (bshl (zext1 di) idx) in
  let ports = [ ("a", "ADDRA", "DIA", "DOA", "INIT_A")
              ; ("b", "ADDRB", "DIB", "DOB", "INIT_B")
              ; ("c", "ADDRC", "DIC", "DOC", "INIT_C")
              ; ("d", "ADDRD", "DID", "DOD", "INIT_D") ] in
  let init_of key = try List.assoc key inits with Not_found -> String.make 64 '0' in
  let mem_sig x init_s =
    let z = if only_binary init_s then (try Z.of_string ("0b" ^ init_s) with _ -> Z.zero)
            else Z.zero in
    { name = "mem_" ^ x; stype = w64; direction = `Internal;
      initial_value = Some (BConst { value = z; width = 64 }); attrs = [] } in
  let write_body = List.map (fun (x, _, di, _, _) ->
    BAssign { lhs = "mem_" ^ x;
              rhs = BCond { condition = v "WE";
                            then_val = set_bit (v ("mem_" ^ x)) (v "ADDRD") (v di);
                            else_val = v ("mem_" ^ x) } }) ports in
  let seq = BSequential { name = "seq"; clock = "WCLK"; clock_edge = `Pos;
                          reset = None; reset_edge = None; reset_async = false;
                          body = write_body; blocking_vars = [] } in
  let read_body = List.map (fun (x, addr, _, dox, _) ->
    BAssign { lhs = dox;
              rhs = BSelect { array = v ("mem_" ^ x); index = v addr } }) ports in
  let comb = BCombinational { name = "comb"; sensitivity = [BAny]; body = read_body } in
  let signals =
    [ sig_ "WCLK" `Input (); sig_ "WE" `Input ();
      sig_wide "ADDRA" 6 `Input (); sig_wide "ADDRB" 6 `Input ();
      sig_wide "ADDRC" 6 `Input (); sig_wide "ADDRD" 6 `Input ();
      sig_ "DIA" `Input (); sig_ "DIB" `Input (); sig_ "DIC" `Input (); sig_ "DID" `Input ();
      sig_ "DOA" `Output (); sig_ "DOB" `Output (); sig_ "DOC" `Output (); sig_ "DOD" `Output () ]
    @ List.map (fun (x, _, _, _, k) -> mem_sig x (init_of k)) ports in
  let name = "RAM64M__" ^ Digest.to_hex (Digest.string
      (String.concat "_" (List.map (fun (_, _, _, _, k) -> init_of k) ports))) in
  empty_mod name signals [seq; comb]

(* ---------------------------------------------------------------------- *)
(* RAM32X1D: 32x1 dual-port distributed RAM (Vivado unisim RAM32X1D.v).     *)
(*   posedge WCLK, WE:  mem[{A4..A0}] <= D                                  *)
(*   SPO = mem[{A4..A0}]      DPO = mem[{DPRA4..DPRA0}]     (async reads)    *)
(* Same shape as ram64m_body -- mem is one 32-bit register, the read a       *)
(* dynamic bit-select and the write a mask/shift update -- but the address   *)
(* arrives as five SCALAR pins rather than a bus, so it is concatenated      *)
(* MSB-first here.  Without this model every module built on RAM32X1D (the   *)
(* open flow's async FIFOs and elastic buffers) was UNPROVABLE: augment_xil_ *)
(* models left it uninterpreted, its internal state read as zero, and        *)
(* miter_hier reported a spurious DIFFER that looks exactly like a real bug. *)
(* ---------------------------------------------------------------------- *)
let ram32x1d_body ~init =
  let w32 = BInt { width = 32; signed = Unsigned } in
  let bnot e = BUnOp  { op = BNot; operand = e; result_type = w32 } in
  let band a b = BBinOp { op = BAnd; lhs = a; rhs = b; result_type = w32 } in
  let bor  a b = BBinOp { op = BOr;  lhs = a; rhs = b; result_type = w32 } in
  let bshl a b = BBinOp { op = BShl; lhs = a; rhs = b; result_type = w32 } in
  let k32 z = BConst { value = z; width = 32 } in
  let zext1 e = BConcat [ BConst { value = Z.zero; width = 31 }; e ] in
  (* MSB-first, so bit i of the index is pin i -- {A4,A3,A2,A1,A0}. *)
  let addr_of pfx =
    BConcat (List.map (fun i -> v (Printf.sprintf "%s%d" pfx i)) [4; 3; 2; 1; 0]) in
  let wa = addr_of "A" and ra = addr_of "DPRA" in
  let set_bit mem idx di =
    bor (band mem (bnot (bshl (k32 Z.one) idx))) (bshl (zext1 di) idx) in
  let mem_init =
    if only_binary init then (try Z.of_string ("0b" ^ init) with _ -> Z.zero)
    else Z.zero in
  let seq = BSequential
    { name = "seq"; clock = "WCLK"; clock_edge = `Pos;
      reset = None; reset_edge = None; reset_async = false;
      body = [ BAssign { lhs = "mem";
                         rhs = BCond { condition = v "WE";
                                       then_val = set_bit (v "mem") wa (v "D");
                                       else_val = v "mem" } } ];
      blocking_vars = [] } in
  let comb = BCombinational
    { name = "comb"; sensitivity = [BAny];
      body = [ BAssign { lhs = "SPO"; rhs = BSelect { array = v "mem"; index = wa } };
               BAssign { lhs = "DPO"; rhs = BSelect { array = v "mem"; index = ra } } ] } in
  let addr_pins pfx = List.init 5 (fun i -> sig_ (Printf.sprintf "%s%d" pfx i) `Input ()) in
  let signals =
    [ sig_ "WCLK" `Input (); sig_ "WE" `Input (); sig_ "D" `Input ();
      sig_ "SPO" `Output (); sig_ "DPO" `Output ();
      { name = "mem"; stype = w32; direction = `Internal;
        initial_value = Some (k32 mem_init); attrs = [] } ]
    @ addr_pins "A" @ addr_pins "DPRA" in
  empty_mod ("RAM32X1D__" ^ name_frag init) signals [seq; comb]

(* ---------------------------------------------------------------------- *)
(* Dispatch: binstance -> specialised body bmodule (or None if unknown).   *)
(* ---------------------------------------------------------------------- *)
let lut_arity = function
  | "LUT1" -> Some 1 | "LUT2" -> Some 2 | "LUT3" -> Some 3
  | "LUT4" -> Some 4 | "LUT5" -> Some 5 | "LUT6" -> Some 6 | _ -> None

let synth (i : binstance) : bmodule option =
  match lut_arity i.module_name with
  | Some k ->
      let raw = match param_lookup i "INIT" with Some s -> s | None -> "" in
      let init = normalize_init ~k raw in
      Some (lut_body ~k ~init)
  | None ->
    match i.module_name with
    | "FDRE" ->
        let init = match param_lookup i "INIT" with Some s -> s | None -> "0" in
        let inv = List.filter_map (fun (p, k) -> if inverted i p then Some k else None)
            [("IS_D_INVERTED","D"); ("IS_CE_INVERTED","CE"); ("IS_R_INVERTED","R")] in
        Some (ff_body ~ty:"FDRE" ~set_val:false ~ctrl_pin:"R" ~init ~inv)
    | "FDSE" ->
        let init = match param_lookup i "INIT" with Some s -> s | None -> "1" in
        let inv = List.filter_map (fun (p, k) -> if inverted i p then Some k else None)
            [("IS_D_INVERTED","D"); ("IS_CE_INVERTED","CE"); ("IS_S_INVERTED","S")] in
        Some (ff_body ~ty:"FDSE" ~set_val:true ~ctrl_pin:"S" ~init ~inv)
    | "FDCE" ->
        (* async CLR -> 0. For a cycle-based next-state miter the async/sync
           distinction is irrelevant (both give Q'=CLR?0:CE?D:Q); modelling it
           the SAME on both flows makes it cancel. *)
        let init = match param_lookup i "INIT" with Some s -> s | None -> "0" in
        let inv = List.filter_map (fun (p, k) -> if inverted i p then Some k else None)
            [("IS_D_INVERTED","D"); ("IS_CE_INVERTED","CE"); ("IS_CLR_INVERTED","CLR")] in
        Some (ff_body ~ty:"FDCE" ~set_val:false ~ctrl_pin:"CLR" ~init ~inv)
    | "FDPE" ->
        (* async PRE -> 1. *)
        let init = match param_lookup i "INIT" with Some s -> s | None -> "1" in
        let inv = List.filter_map (fun (p, k) -> if inverted i p then Some k else None)
            [("IS_D_INVERTED","D"); ("IS_CE_INVERTED","CE"); ("IS_PRE_INVERTED","PRE")] in
        Some (ff_body ~ty:"FDPE" ~set_val:true ~ctrl_pin:"PRE" ~init ~inv)
    | "FD" | "FD_1" ->
        let init = match param_lookup i "INIT" with Some s -> s | None -> "0" in
        let inv = List.filter_map (fun (p, k) -> if inverted i p then Some k else None)
            [("IS_D_INVERTED","D")] in
        Some (bare_ff_body ~ty:"FD" ~ctrl:None ~set_val:false ~init ~inv)
    | "FDP" | "FDP_1" ->
        let init = match param_lookup i "INIT" with Some s -> s | None -> "1" in
        let inv = List.filter_map (fun (p, k) -> if inverted i p then Some k else None)
            [("IS_D_INVERTED","D"); ("IS_PRE_INVERTED","PRE")] in
        Some (bare_ff_body ~ty:"FDP" ~ctrl:(Some "PRE") ~set_val:true ~init ~inv)
    | "FDC" | "FDC_1" ->
        let init = match param_lookup i "INIT" with Some s -> s | None -> "0" in
        let inv = List.filter_map (fun (p, k) -> if inverted i p then Some k else None)
            [("IS_D_INVERTED","D"); ("IS_CLR_INVERTED","CLR")] in
        Some (bare_ff_body ~ty:"FDC" ~ctrl:(Some "CLR") ~set_val:false ~init ~inv)
    | "CARRY4" -> Some (carry4_body ())
    | "RAM64M" ->
        let ini key = match param_lookup i key with
          | Some s -> normalize_init ~k:6 s | None -> String.make 64 '0' in
        Some (ram64m_body ~inits:[ ("INIT_A", ini "INIT_A"); ("INIT_B", ini "INIT_B")
                                 ; ("INIT_C", ini "INIT_C"); ("INIT_D", ini "INIT_D") ])
    | "RAM32X1D" ->
        (* INIT is 32-bit; normalize_init ~k:5 renders 2^5 = 32 bits MSB-first,
           so INIT[0] (address 0) is the last character -- same convention as
           RAM64M/SRLC32E above. *)
        let raw = match param_lookup i "INIT" with Some s -> s | None -> "0" in
        Some (ram32x1d_body ~init:(normalize_init ~k:5 raw))
    | "SRL16E" | "SRL16" ->
        (* INIT is 16-bit; normalize_init ~k:4 renders 2^4 = 16 bits MSB-first *)
        let raw = match param_lookup i "INIT" with Some s -> s | None -> "0" in
        let init = normalize_init ~k:4 raw in
        Some (srl_body ~ty:i.module_name ~w:16
                ~addr:(APins ["A0"; "A1"; "A2"; "A3"]) ~q31:false ~init)
    | "SRLC32E" | "SRLC32" ->
        (* INIT is 32-bit; normalize_init ~k:5 renders 2^5 = 32 bits MSB-first *)
        let raw = match param_lookup i "INIT" with Some s -> s | None -> "0" in
        let init = normalize_init ~k:5 raw in
        Some (srl_body ~ty:i.module_name ~w:32
                ~addr:(ABus ("A", 5)) ~q31:true ~init)
    | "INV"   -> Some (buf_body ~name:"INV"  ~ins:["I"] ~out:"O" ~rhs:(notb (v "I")))
    | "BUF"   -> Some (buf_body ~name:"BUF"  ~ins:["I"] ~out:"O" ~rhs:(v "I"))
    | "IBUF"  -> Some (buf_body ~name:"IBUF"  ~ins:["I"] ~out:"O" ~rhs:(v "I"))
    | "OBUF"  -> Some (buf_body ~name:"OBUF"  ~ins:["I"] ~out:"O" ~rhs:(v "I"))
    | "BUFG"  -> Some (buf_body ~name:"BUFG"  ~ins:["I"] ~out:"O" ~rhs:(v "I"))
    | "BUFGCTRL_THRU" -> None
    | "IBUFDS" -> Some (buf_body ~name:"IBUFDS" ~ins:["I";"IB"] ~out:"O" ~rhs:(v "I"))
    | "VCC"   -> Some (buf_body ~name:"VCC" ~ins:[] ~out:"P" ~rhs:c1)
    | "GND"   -> Some (buf_body ~name:"GND" ~ins:[] ~out:"G" ~rhs:c0)
    (* nextpnr post-pack constant drivers (output pin Y) *)
    | "PSEUDO_VCC" -> Some (buf_body ~name:"PSEUDO_VCC" ~ins:[] ~out:"Y" ~rhs:c1)
    | "PSEUDO_GND" -> Some (buf_body ~name:"PSEUDO_GND" ~ins:[] ~out:"Y" ~rhs:c0)
    | _ -> None

(* ---------------------------------------------------------------------- *)
(* Whole-program augmentation.  Rewrites every primitive binstance to its  *)
(* specialised body name and appends the (deduped) bodies.                 *)
(* ---------------------------------------------------------------------- *)
let augment_program (p : bprogram) : bprogram =
  let bodies : (string, bmodule) Hashtbl.t = Hashtbl.create 64 in
  let rewrite_inst (i : binstance) : binstance =
    match synth i with
    | None -> i
    | Some body ->
        if not (Hashtbl.mem bodies body.name) then
          Hashtbl.replace bodies body.name body;
        { i with module_name = body.name }
  in
  let modules' =
    List.map (fun (m : bmodule) ->
      { m with instances = List.map rewrite_inst m.instances }) p.modules in
  let existing = List.map (fun (m : bmodule) -> m.name) modules' in
  let extra =
    Hashtbl.fold (fun name body acc ->
      if List.mem name existing then acc else body :: acc) bodies [] in
  { p with modules = modules' @ extra }

(* Count, per primitive type, how many instances would be modelled — for a
 * recipe to report coverage before/after. *)
let coverage (p : bprogram) : (string * int) list =
  let tbl : (string, int) Hashtbl.t = Hashtbl.create 32 in
  List.iter (fun (m : bmodule) ->
    List.iter (fun (i : binstance) ->
      if synth i <> None then
        Hashtbl.replace tbl i.module_name
          (1 + (try Hashtbl.find tbl i.module_name with Not_found -> 0)))
      m.instances) p.modules;
  Hashtbl.fold (fun k n acc -> (k, n) :: acc) tbl []
