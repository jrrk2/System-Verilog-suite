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
let c0           = BConst { value = 0; width = 1 }
let c1           = BConst { value = 1; width = 1 }
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
