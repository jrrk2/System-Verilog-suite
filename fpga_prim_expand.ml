(* fpga_prim_expand.ml — expand FPGA primitive INSTANCES in a gate-mapped BIR
 * module into behavioural BIR so that:
 *   - Behavioral_ffrip can rip registers (FDRE/FDCE/FDPE/FDSE) by name, and
 *   - Z3_miter can reason about the LUT / CARRY4 / buffer logic,
 * while irreducible primitives (GTXE2, MMCME2, RAMB, RAM64M, PLL, SRL)
 * become uninterpreted functions: each output bit = BCall "module__port_bit"
 * of ALL the box inputs.  Z3_miter shares the same FuncDecl across both miter
 * sides (bcall_decl_cache), so a matched box behaves identically on both sides
 * and any INPUT mis-wire propagates to a miter DIFFER.
 *
 * Enable end-to-end via the FPGA_LEC_NAMES-named netlist + svd.expand_fpga. *)

open Behavioral_ir

(* ---- Verilog INIT literal -> LSB-first bit array ---------------------- *)
let parse_init_bits (s : string) : bool array =
  let s = String.trim s in
  match String.index_opt s '\'' with
  | None ->
      (* bare decimal *)
      (try
         let v = int_of_string s in
         Array.init 64 (fun i -> (v lsr i) land 1 = 1)
       with _ -> [| false |])
  | Some q ->
      let base = if q + 1 < String.length s then Char.lowercase_ascii s.[q+1] else 'b' in
      let digs =
        if q + 2 <= String.length s
        then String.sub s (q+2) (String.length s - (q+2)) else "" in
      let digs = String.concat "" (String.split_on_char '_' digs) in
      let bits_per = match base with 'h' -> 4 | 'o' -> 3 | 'b' -> 1 | _ -> 0 in
      if bits_per = 0 then
        (try let v = int_of_string digs in Array.init 64 (fun i -> (v lsr i) land 1 = 1)
         with _ -> [| false |])
      else begin
        let n = String.length digs in
        let arr = Array.make (max 1 (n * bits_per)) false in
        String.iteri (fun idx ch ->
          let dv =
            if ch >= '0' && ch <= '9' then Char.code ch - Char.code '0'
            else Char.code (Char.lowercase_ascii ch) - Char.code 'a' + 10 in
          let base_bit = (n - 1 - idx) * bits_per in
          for b = 0 to bits_per - 1 do
            if base_bit + b < Array.length arr then
              arr.(base_bit + b) <- (dv lsr b) land 1 = 1
          done) digs;
        arr
      end

(* ---- port helpers ---------------------------------------------------- *)
let one = BConst { value = Z.one; width = 1 }
let zero = BConst { value = Z.zero; width = 1 }

let port_expr (i : binstance) name = List.assoc_opt name i.port_connections

(* Expand a bexpr into a LSB-first list of 1-bit bexprs. Gate-mapped port
 * connections are already scalar BVars or MSB-first BConcat of scalars. *)
let rec bits_of (e : bexpr) : bexpr list =
  match e with
  | BConcat es -> List.rev (List.concat_map (fun x -> List.rev (bits_of x)) es)
  | BConst { value; width } ->
      List.init width (fun b -> BConst { value = (if Z.testbit value b then Z.one else Z.zero); width = 1 })
  | BSlice { signal = BVar n; msb; lsb } ->
      List.init (msb - lsb + 1) (fun b -> BVar (Printf.sprintf "%s[%d]" n (lsb + b)))
  | other -> [ other ]

(* Normalise the const-tie sentinels to real constants — the gate-map ties
   pins to BVar "GND"/"VCC" which the Z3 miter would otherwise leave as free
   inputs (e.g. a CARRY4 CYINIT=GND read as free -> wrong carry). *)
let norm = function BVar "GND" -> zero | BVar "VCC" -> one | e -> e

(* one input-port bit (default const-0 if unconnected) *)
let in_bit i name idx =
  match port_expr i name with
  | None -> zero
  | Some e -> (match List.nth_opt (bits_of e) idx with Some b -> norm b | None -> zero)

let in_bit0 i name = in_bit i name 0

(* output-port net names, LSB-first (only BVar / BConcat of BVar supported) *)
let out_nets i name : string list =
  match port_expr i name with
  | None -> []
  | Some e ->
      List.filter_map (function BVar n -> Some n
                              | BSlice { signal = BVar s; msb; lsb } when msb = lsb ->
                                  Some (Printf.sprintf "%s[%d]" s lsb)
                              | _ -> None) (bits_of e)

let param_str i name =
  match List.assoc_opt name i.param_strs with Some s -> Some s | None -> None

(* ---- LUT mux-tree ---------------------------------------------------- *)
let lut_expr (inputs : bexpr list) (bit : int -> bool) : bexpr =
  let k = List.length inputs in
  let rec build offset j =
    if j = 0 then (if bit offset then one else zero)
    else
      let sel = List.nth inputs (j - 1) in
      let e0 = build offset (j - 1) in
      let e1 = build (offset lor (1 lsl (j - 1))) (j - 1) in
      BCond { condition = sel; then_val = e1; else_val = e0 }
  in
  build 0 k

let comb name body =
  BCombinational { name; sensitivity = [ BAny ]; body }

(* ---- flip-flop ------------------------------------------------------- *)
(* Emit a bare-BAssign FF body so Behavioral_ffpack can re-pack bit FFs into a
   bus.  Clock-enable folds into the data (d_ce = CE ? D : Q); a SYNC reset
   also folds into the data (matches how ffrip represents the behavioural
   reg's reset in its D-cone); an ASYNC reset stays a register-port property. *)
let is_vcc = function BVar "VCC" -> true | BConst { value = zv; _ } when Z.equal zv Z.one -> true | _ -> false
let is_gnd = function BVar "GND" -> true | BConst { value = zv; _ } when Z.equal zv Z.zero -> true | _ -> false

let ff_process ~inst ~qn ~clk ~ce ~d ~rst ~rst_async ~rst_val =
  (* Name the process `<inst>__seq` (same convention as the inline path's
     xil-model FF processes) — canonicalize_ff_names keys the cross-flow
     state rename on the INSTANCE name (`acc_reg[0]__seq` → state acc__b0),
     because Vivado's Q NET name is often unrelated (`acc_reg_n_0_[0]`,
     `\^a`).  The Q-net-based name hid the instance and the canon never
     fired under miter_hier's expand path. *)
  let pname = (if inst = "" then "exp_" ^ qn else inst ^ "__seq") in
  (* Drop the CE-hold mux when CE is tied high (the common case) so the
     packed D-cone doesn't carry dead references to the pre-pack bit nets. *)
  let d_ce = if is_vcc ce then d
             else BCond { condition = ce; then_val = d; else_val = BVar qn } in
  (* A reset tied to GND (sync) never fires — treat as no reset. *)
  let rst = match rst with Some (rsig, _) when is_gnd rsig && not rst_async -> None | r -> r in
  match clk with
  | BVar cn ->
      let mk ?reset ?reset_edge ~reset_async rhs =
        Some (BSequential { name = pname; clock = cn; clock_edge = `Pos;
                            reset; reset_edge; reset_async;
                            body = [ BAssign { lhs = qn; rhs } ];
                            blocking_vars = [] }) in
      let rv = if rst_val = 1 then one else zero in
      (match rst, rst_async with
       | None, _ -> mk ~reset_async:false d_ce
       | Some (rsig, _), false ->
           mk ~reset_async:false
             (BCond { condition = rsig; then_val = rv; else_val = d_ce })
       | Some (_, rname), true ->
           mk ~reset:rname ~reset_edge:`Pos ~reset_async:true d_ce)
  | _ -> None

let reset_arg i name =
  match port_expr i name with
  | Some (BVar n) -> Some (BVar n, n)
  | Some e -> (match bits_of e with BVar n :: _ -> Some (BVar n, n) | _ -> None)
  | None -> None

(* ---- classification -------------------------------------------------- *)
let is_lut m = String.length m = 4 && String.sub m 0 3 = "LUT"
let lut_k m = Char.code m.[3] - Char.code '0'
let passthrough = [ "BUFG"; "BUFH"; "BUFR"; "BUFGCE"; "IBUF"; "OBUF"; "BUFGCTRL" ]

(* USER-submodule port directions, for HIERARCHICAL UF: an instance of a known
 * user module becomes an uninterpreted function (same FuncDecl both miter
 * sides) instead of being flattened — so each module is verified with its
 * children abstracted, keeping Z3 capacity bounded. Populated by
 * [set_user_ports] from the reference (parse) program's module interfaces. *)
let rec btw = function
  | BInt { width; _ } -> width | BBool -> 1
  | BArray { element; size } -> size * btw element | BStruct _ -> 32
let user_ports : (string, (string * [ `Input | `Output ] * int) list) Hashtbl.t =
  Hashtbl.create 64
let set_user_ports (mods : bmodule list) =
  Hashtbl.reset user_ports;
  List.iter (fun (m : bmodule) ->
    let ps = List.filter_map (fun (s : bsignal) ->
      match s.direction with
      | `Input  -> Some (s.name, `Input,  btw s.stype)
      | `Output -> Some (s.name, `Output, btw s.stype)
      | `Internal -> None) m.signals in
    Hashtbl.replace user_ports m.name ps) mods

(* irreducible-primitive output ports, from the authoritative JSON via
 * bir_to_edif (reused).  Cell -> [(port, `In|`Out, width)].  User submodules
 * (set_user_ports) take priority so they UF-abstract hierarchically. *)
let box_ports ty : (string * [ `Input | `Output ] * int) list =
  match Hashtbl.find_opt user_ports ty with
  | Some l -> l
  | None ->
      match Hashtbl.find_opt (Lazy.force Bir_to_edif.xil_json_ports) ty with
      | Some l -> l
      | None -> []

(* Expand a single instance -> (processes, keep?) where keep=true leaves the
 * instance in place (unhandled).  [canon] is the CANONICAL child key
 * (module name + sibling ordinal) for USER-module instances — identical on
 * both miter sides regardless of the flows' own instance names (gate_map
 * suffixes i_pcs_pma -> i_pcs_pma_257).  A user child is CUT, not
 * UF-abstracted: its outputs become same-named FREE nets (`ufo__<canon>__…`,
 * paired across sides by the miter's undriven-internal matching — this also
 * breaks feedback cycles through the child, where a UF fixed-point lets each
 * side settle differently) and its inputs become compared nets
 * (`ufi__<canon>__…`, picked up as extra output pairs by the miter) so the
 * parent's wiring INTO the child stays verified (assume-guarantee). *)
(* CHILD-KNOWN output constants: bits of a child OUTPUT port the child module
   itself drives with a constant (`assign status_vector[15] = <const0>` /
   merged concat with const elements).  Each miter side scans its OWN child
   body and DRIVES the corresponding ufo bit — one side may have hoisted the
   constant above the boundary (Vivado narrows the port), the other keeps it
   inside; either side's knowledge propagates through the cross-side ufo tie. *)
let child_out_consts : (string, (string * int, int) Hashtbl.t) Hashtbl.t =
  Hashtbl.create 8
let mod_tbl : (string, bmodule) Hashtbl.t = Hashtbl.create 16

let compute_out_consts (mm : bmodule) : (string * int, int) Hashtbl.t =
  let tbl = Hashtbl.create 16 in
  let outs = List.filter_map (fun (s : bsignal) ->
    if s.direction = `Output then Some s.name else None) mm.signals in
  let rec ewidth = function
    | BConst { width; _ } -> Some width
    | BSlice { msb; lsb; _ } -> Some (abs (msb - lsb) + 1)
    | BVar ("GND" | "VCC" | "<const0>" | "<const1>") -> Some 1
    | BReplicate { count; value } ->
        (match ewidth value with Some w -> Some (count * w) | None -> None)
    | BConcat es ->
        List.fold_left (fun a e -> match a, ewidth e with
          | Some s, Some w -> Some (s + w) | _ -> None) (Some 0) es
    | _ -> None in
  let rec bitc e j = match e with
    | BConst { value; _ } -> Some (if Z.testbit value j then 1 else 0)
    | BVar ("GND" | "<const0>") -> Some 0
    | BVar ("VCC" | "<const1>") -> Some 1
    | BSlice { signal = BVar ("GND" | "<const0>"); _ } -> Some 0
    | BSlice { signal = BVar ("VCC" | "<const1>"); _ } -> Some 1
    | BSlice { signal = BConst { value; _ }; msb; lsb } ->
        let lo = min msb lsb in
        Some (if Z.testbit value (lo + j) then 1 else 0)
    | BConcat es ->                        (* MSB-first *)
        let rec go j = function
          | [] -> None
          | el :: rest ->
              (match ewidth el with
               | Some w -> if j < w then bitc el j else go (j - w) rest
               | None -> None) in
        go j (List.rev es)
    | _ -> None in
  let record port rhs =
    if List.mem port outs then
      match ewidth rhs with
      | Some w ->
          for j = 0 to w - 1 do
            match bitc rhs j with
            | Some c -> Hashtbl.replace tbl (port, j) c
            | None -> ()
          done
      | None -> () in
  let bracket_re = Str.regexp "^\\(.+\\)\\[\\([0-9]+\\)\\]$" in
  List.iter (function
    | BCombinational { body; _ } ->
        List.iter (function
          | BAssign { lhs; rhs } ->
              record lhs rhs;
              if Str.string_match bracket_re lhs 0 then begin
                let base = Str.matched_group 1 lhs in
                let bit = int_of_string (Str.matched_group 2 lhs) in
                if List.mem base outs then
                  match bitc rhs 0 with
                  | Some c -> Hashtbl.replace tbl (base, bit) c
                  | None -> ()
              end
          | _ -> ()) body
    | _ -> ()) mm.processes;
  tbl

(* Does the child module READ input [pin]?  A side that optimised the pin
   away (Vivado unifies tx_clk≡rx_clk inside the MAC and ties the pin off)
   declares it but never references it — comparing the parent's wiring of a
   pin that side ignores only produces noise, so its ufi is not emitted on
   that side (no cross-side intersection ⇒ no compare). *)
let child_reads_tbl : (string, (string, unit) Hashtbl.t) Hashtbl.t =
  Hashtbl.create 8
let child_reads child_mod pin : bool =
  match Hashtbl.find_opt mod_tbl child_mod with
  | None -> true
  | Some mm ->
      let tbl =
        match Hashtbl.find_opt child_reads_tbl child_mod with
        | Some t -> t
        | None ->
            let t = Hashtbl.create 64 in
            let rec ee e = match e with
              | BVar n ->
                  Hashtbl.replace t n ();
                  (* bracket-string reads `pin[3]` count as reads of pin *)
                  (match String.index_opt n '[' with
                   | Some k -> Hashtbl.replace t (String.sub n 0 k) ()
                   | None -> ())
              | BConst _ -> ()
              | BBinOp r -> ee r.lhs; ee r.rhs
              | BUnOp r -> ee r.operand
              | BSelect r -> ee r.array; ee r.index
              | BSlice r -> ee r.signal
              | BConcat es -> List.iter ee es
              | BReplicate r -> ee r.value
              | BCond r -> ee r.condition; ee r.then_val; ee r.else_val
              | BCall r -> List.iter ee r.args in
            let rec es_ s = match s with
              | BAssign r -> ee r.rhs
              | BIf r -> ee r.condition;
                  List.iter es_ r.then_stmts; List.iter es_ r.else_stmts
              | BCase r -> ee r.selector;
                  List.iter (fun (g, b) -> ee g; List.iter es_ b) r.cases;
                  List.iter es_ r.default
              | BWhile r -> ee r.condition; List.iter es_ r.body
              | BFor r -> es_ r.init; ee r.condition; es_ r.update;
                  List.iter es_ r.body
              | BBlock b -> List.iter es_ b
              | BCallStmt r -> List.iter ee r.args
              | BReturn (Some e) -> ee e
              | BReturn None -> () in
            List.iter (function
              | BCombinational { body; _ } -> List.iter es_ body
              | BSequential { clock; reset; body; _ } ->
                  Hashtbl.replace t clock ();
                  (match reset with Some r -> Hashtbl.replace t r () | None -> ());
                  List.iter es_ body) mm.processes;
            List.iter (fun (i : binstance) ->
              List.iter (fun (_, e) -> ee e) i.port_connections) mm.instances;
            Hashtbl.replace child_reads_tbl child_mod t; t in
      Hashtbl.mem tbl pin

(* CHILD-KNOWN output ALIASES: two outputs the child drives from the SAME
   internal net (`assign rx_clk = userclk2; assign eth_clk = userclk2;`).
   Vivado rewires the parent's consumers onto ONE of them and dead-ties the
   other, while the other flow wires them as written — tying the aliased
   ufos propagates the equivalence so the parent wiring compares close. *)
let child_out_alias_tbl : (string, (string, string) Hashtbl.t) Hashtbl.t =
  Hashtbl.create 8
(* output ≡ INPUT passthrough (`assign rd_data = rd_data_mem`): Vivado
   dissolves it and rewires the parent's consumers straight to the source —
   drive the output's ufo from the parent's connection to that input. *)
let child_out_passthru_tbl : (string, (string, string) Hashtbl.t) Hashtbl.t =
  Hashtbl.create 8
let compute_out_aliases (mm : bmodule) : (string, string) Hashtbl.t =
  let outs = List.filter_map (fun (s : bsignal) ->
    if s.direction = `Output then Some s.name else None) mm.signals in
  let inputs = List.filter_map (fun (s : bsignal) ->
    if s.direction = `Input then Some s.name else None) mm.signals in
  let ptbl = Hashtbl.create 8 in
  Hashtbl.replace child_out_passthru_tbl mm.name ptbl;
  (* identity detection sees through the gate-map's obuf LUT trees: rhs whose
     support is a SINGLE variable v and which evaluates to 0 under v=0 and 1
     under v=1 is ≡ v *)
  let support e =
    let acc = ref [] in
    let rec go = function
      | BVar n -> if not (List.mem n !acc) then acc := n :: !acc
      | BConst _ -> ()
      | BBinOp r -> go r.lhs; go r.rhs
      | BUnOp r -> go r.operand
      | BSelect r -> go r.array; go r.index
      | BSlice r -> go r.signal
      | BConcat es -> List.iter go es
      | BReplicate r -> go r.value
      | BCond r -> go r.condition; go r.then_val; go r.else_val
      | BCall r -> List.iter go r.args in
    go e; !acc in
  let rec ev env = function
    | BConst { value; _ } -> Some (if Z.equal value Z.zero then 0 else 1)
    | BVar n ->
        (match List.assoc_opt n env with
         | Some v -> Some v
         | None -> (match n with
                    | "GND" | "<const0>" -> Some 0
                    | "VCC" | "<const1>" -> Some 1
                    | _ -> None))
    | BCond r ->
        (match ev env r.condition with
         | Some 0 -> ev env r.else_val
         | Some _ -> ev env r.then_val
         | None -> None)
    | BUnOp { op = BNot; operand; _ } ->
        (match ev env operand with Some v -> Some (1 - v) | None -> None)
    | BSlice { signal; _ } -> ev env signal
    | _ -> None in
  let ident_of rhs =
    match rhs with
    | BVar x -> Some x
    | _ ->
        (match support rhs with
         | [ v ] when ev [ (v, 0) ] rhs = Some 0 && ev [ (v, 1) ] rhs = Some 1 ->
             Some v
         | _ -> None) in
  let drv : (string, string list ref) Hashtbl.t = Hashtbl.create 16 in
  let note out x =
    match Hashtbl.find_opt drv x with
    | Some l -> l := out :: !l
    | None -> Hashtbl.add drv x (ref [ out ]) in
  List.iter (function
    | BCombinational { body; _ } ->
        List.iter (function
          | BAssign { lhs; rhs } when List.mem lhs outs ->
              (match ident_of rhs with
               | Some x ->
                   note lhs x;
                   if List.mem x inputs then Hashtbl.replace ptbl lhs x
               | None -> ())
          | _ -> ()) body
    | _ -> ()) mm.processes;
  (* gate-mapped children drive output ports through identity obuf LUT
     INSTANCES (all inputs the same net, INIT an identity table) *)
  List.iter (fun (i : binstance) ->
    if is_lut i.module_name then begin
      let k = lut_k i.module_name in
      let o = match List.assoc_opt "O" i.port_connections with
        | Some (BVar n) when List.mem n outs -> Some n
        | Some (BSlice { signal = BVar n; msb = 0; lsb = 0 })
          when List.mem n outs -> Some n
        | _ -> None in
      let ins = List.init k (fun j ->
        List.assoc_opt (Printf.sprintf "I%d" j) i.port_connections) in
      let same_var = match ins with
        | Some (BVar v) :: rest
          when List.for_all (function Some (BVar v') -> String.equal v v'
                                    | _ -> false) rest -> Some v
        | _ -> None in
      match o, same_var with
      | Some out, Some v ->
          let init = match param_str i "INIT" with
            | Some s -> parse_init_bits s | None -> [| false |] in
          let bit b = b < Array.length init && init.(b) in
          if (not (bit 0)) && bit ((1 lsl k) - 1) then note out v
      | _ -> ()
    end) mm.instances;
  let tbl = Hashtbl.create 8 in
  Hashtbl.iter (fun _ l ->
    match List.sort compare !l with
    | rep :: (_ :: _ as rest) ->
        List.iter (fun o -> Hashtbl.replace tbl o rep) rest
    | _ -> ()) drv;
  tbl
let out_alias_of child_mod port : string option =
  let tbl =
    match Hashtbl.find_opt child_out_alias_tbl child_mod with
    | Some t -> t
    | None ->
        let t = match Hashtbl.find_opt mod_tbl child_mod with
          | Some mm -> compute_out_aliases mm
          | None -> Hashtbl.create 1 in
        Hashtbl.replace child_out_alias_tbl child_mod t; t in
  Hashtbl.find_opt tbl port

let out_passthru_of child_mod port : string option =
  ignore (out_alias_of child_mod port);   (* forces the scan *)
  match Hashtbl.find_opt child_out_passthru_tbl child_mod with
  | Some t -> Hashtbl.find_opt t port
  | None -> None

let out_const_of child_mod port bit : int option =
  let tbl =
    match Hashtbl.find_opt child_out_consts child_mod with
    | Some t -> t
    | None ->
        let t = match Hashtbl.find_opt mod_tbl child_mod with
          | Some mm -> compute_out_consts mm
          | None -> Hashtbl.create 1 in
        Hashtbl.replace child_out_consts child_mod t; t in
  Hashtbl.find_opt tbl (port, bit)

(* WIDTH-AWARE bit extraction for cut-boundary wiring: `bits_of` cannot split
   a multi-bit BVar ELEMENT inside a concat (no width info), so
   `.addra({nextbuf, rx_word_addr})` yielded bit0 = the whole 12-bit bus and
   bits2+ = zero — garbling every ufi compare of the connection.  [sigw] is
   the PARENT module's declared signal widths. *)
let rec bits_of_w sigw (e : bexpr) : bexpr list =
  match e with
  | BConcat es -> List.rev (List.concat_map (fun x -> List.rev (bits_of_w sigw x)) es)
  | BConst { value; width } ->
      List.init width (fun b ->
        BConst { value = (if Z.testbit value b then Z.one else Z.zero); width = 1 })
  | BSlice { signal = BVar n; msb; lsb } ->
      List.init (msb - lsb + 1) (fun b ->
        BSlice { signal = BVar n; msb = lsb + b; lsb = lsb + b })
  | BVar n ->
      (match Hashtbl.find_opt sigw n with
       | Some w when w > 1 ->
           List.init w (fun b -> BSlice { signal = BVar n; msb = b; lsb = b })
       | _ -> [ e ])
  | other -> [ other ]

let in_bit_w sigw i name idx =
  match port_expr i name with
  | None -> zero
  | Some e ->
      (match List.nth_opt (bits_of_w sigw e) idx with
       | Some b -> norm b | None -> zero)

let expand_instance ?canon ?(sigw : (string, int) Hashtbl.t = Hashtbl.create 0)
    (i : binstance) : bprocess list * bool =
  let m = i.module_name in
  if is_lut m then begin
    let k = lut_k m in
    let inputs = List.init k (fun idx -> in_bit0 i (Printf.sprintf "I%d" idx)) in
    let init = match param_str i "INIT" with Some s -> parse_init_bits s | None -> [| false |] in
    let get b = if b < Array.length init then init.(b) else false in
    (match out_nets i "O" with
     | [ o ] -> [ comb ("exp_" ^ o) [ BAssign { lhs = o; rhs = lut_expr inputs get } ] ], false
     | _ -> [], true)
  end
  else if List.mem m passthrough then begin
    let src = match port_expr i "I" with Some e -> e | None -> in_bit0 i "I0" in
    (match out_nets i "O" with
     | [ o ] -> [ comb ("exp_" ^ o) [ BAssign { lhs = o; rhs = List.hd (bits_of src @ [zero]) } ] ], false
     | _ -> [], true)
  end
  else if m = "INV" then begin
    let src = match port_expr i "I" with Some e -> e | None -> in_bit0 i "I0" in
    (match out_nets i "O" with
     | [ o ] -> [ comb ("exp_" ^ o)
         [ BAssign { lhs = o; rhs = BUnOp { op = BNot;
             operand = List.hd (bits_of src @ [zero]);
             result_type = BInt { width = 1; signed = Unsigned } } } ] ], false
     | _ -> [], true)
  end
  else match m with
  | "FDRE" | "FDCE" | "FDPE" | "FDSE" ->
      let clk = (match port_expr i "C" with Some e -> e | None -> zero) in
      let ce  = in_bit0 i "CE" in
      let d   = in_bit0 i "D" in
      let qn  = (match out_nets i "Q" with [ q ] -> Some q | _ -> None) in
      let rst, rst_async, rst_val = match m with
        | "FDRE" -> reset_arg i "R",   false, 0
        | "FDCE" -> reset_arg i "CLR", true,  0
        | "FDPE" -> reset_arg i "PRE", true,  1
        | _      -> reset_arg i "S",   false, 1 in
      (match qn with
       | Some qn ->
           (match ff_process ~inst:i.inst_name ~qn ~clk ~ce ~d ~rst ~rst_async ~rst_val with
            | Some p -> [ p ], false | None -> [], true)
       | None -> [], true)
  | "CARRY4" ->
      let s   = List.init 4 (fun b -> in_bit i "S" b) in
      let di  = List.init 4 (fun b -> in_bit i "DI" b) in
      let ci  = in_bit0 i "CI" in
      let cyi = in_bit0 i "CYINIT" in
      let cin0 = BCond { condition = cyi; then_val = one; else_val = ci } in
      let co = Array.make 4 zero and o = Array.make 4 zero in
      let cur = ref cin0 in
      for b = 0 to 3 do
        let sb = List.nth s b and dib = List.nth di b in
        o.(b) <- BBinOp { op = BXor; lhs = sb; rhs = !cur;
                          result_type = BInt { width = 1; signed = Unsigned } };
        co.(b) <- BCond { condition = sb; then_val = !cur; else_val = dib };
        cur := co.(b)
      done;
      let asg port arr =
        List.mapi (fun b n -> BAssign { lhs = n; rhs = arr.(b) }) (out_nets i port) in
      let body = asg "O" o @ asg "CO" co in
      if body = [] then [], true else [ comb ("exp_carry_" ^ i.inst_name) body ], false
  | "GND" ->
      (match out_nets i "G" with
       | [ o ] -> [ comb ("exp_gnd_" ^ o) [ BAssign { lhs = o; rhs = zero } ] ], false
       | _ -> [], true)
  | "VCC" ->
      (match out_nets i "P" with
       | [ o ] -> [ comb ("exp_vcc_" ^ o) [ BAssign { lhs = o; rhs = one } ] ], false
       | _ -> [], true)
  | _ ->
      (* irreducible primitive OR user submodule -> uninterpreted function per
         output bit.  ARGS = one full-width expr per INPUT port (NOT bit-
         flattened): the behavioural side wires a bus as one BVar while the
         gate-mapped side wires a BConcat of bits — both encode to the port
         width, so per-port args keep the UF's FuncDecl arg-sorts identical on
         both miter sides.  GND/VCC ties are normalised so a const-tied input
         isn't a free variable. *)
      (* CANONICAL port order (by name): the two miter sides learn the module's
         ports from different sources (netlist decl order vs RTL decl order);
         the UF argument list must line up positionally or the same function
         symbol is applied to permuted arguments and the sides never agree. *)
      let ports = List.sort (fun (a, _, _) (b, _, _) -> compare a b)
                    (box_ports m) in
      match canon with
      | Some ck when ports <> [] ->
          (* Boxed child → CUT at the canonical boundary (see doc above).
             Applies to USER modules AND irreducible PRIMITIVES: an MMCM has
             a CLKFBOUT→CLKFBIN feedback loop, so the UF encoding admits
             multiple fixpoints and the two miter sides settle differently —
             every clock derived from it (cpu_clk!) then spuriously differs. *)
          let asgs = List.concat_map (fun (p, dir, w) ->
            match dir with
            | `Output ->
                (* child output bit ← free canonical net.  A CONST element in
                   the connection (Vivado hoists child-internal constants
                   ABOVE the boundary: `.status_vector({const0,const0,bits…})`)
                   instead DRIVES the ufo net with the constant — the
                   cross-side ufo tie then propagates the hoisted knowledge to
                   the flow that kept it inside the child. *)
                let ufon bit = Printf.sprintf "ufo__%s__%s_%d" ck p bit in
                let free bit = BVar (ufon bit) in
                (* child-known constant output bit → DRIVE the ufo;
                   child-known output ALIAS → tie this port's ufo to the
                   representative port's ufo *)
                let known bit = match out_const_of m p bit with
                  | Some 0 -> [ BAssign { lhs = ufon bit; rhs = zero } ]
                  | Some _ -> [ BAssign { lhs = ufon bit; rhs = one } ]
                  | None ->
                      (match out_passthru_of m p with
                       | Some inp ->
                           (* output ≡ child INPUT: drive the ufo from the
                              parent's connection to that input *)
                           [ BAssign { lhs = ufon bit; rhs = in_bit_w sigw i inp bit } ]
                       | None ->
                      match out_alias_of m p with
                       | Some rep when rep <> p ->
                           [ BAssign { lhs = ufon bit;
                                       rhs = BVar (Printf.sprintf
                                               "ufo__%s__%s_%d" ck rep bit) } ]
                       | _ -> []) in
                (match port_expr i p with
                 | Some (BVar single) when w > 1 && not (String.contains single ']') ->
                     List.concat (List.init w (fun bit ->
                       BAssign { lhs = Printf.sprintf "%s[%d]" single bit;
                                 rhs = free bit } :: known bit))
                 | Some e ->
                     List.concat (List.mapi (fun bit b ->
                       match b with
                       | BVar ("GND" | "<const0>") ->
                           [ BAssign { lhs = ufon bit; rhs = zero } ]
                       | BVar ("VCC" | "<const1>") ->
                           [ BAssign { lhs = ufon bit; rhs = one } ]
                       | BConst _ ->
                           [ BAssign { lhs = ufon bit; rhs = b } ]
                       | BVar n ->
                           BAssign { lhs = n; rhs = free bit } :: known bit
                       | BSlice { signal = BVar s; msb; lsb } when msb = lsb ->
                           BAssign { lhs = Printf.sprintf "%s[%d]" s msb;
                                     rhs = free bit } :: known bit
                       | _ -> known bit) (bits_of_w sigw e))
                 | None -> List.concat (List.init (max 1 w) known))
            | `Input when not (child_reads m p) -> []
            | `Input ->
                (* child input → compared canonical net (per bit, so widths
                   agree across sides regardless of connection shape).  A
                   whole-bus BVar connection must be SLICED by the port width
                   (bits_of can't split it — no width info), else bit 0 gets
                   the whole bus and bits 1..w-1 get zero. *)
                let conn = port_expr i p in
                List.init (max 1 w) (fun bit ->
                  let rhs = match conn with
                    | Some (BVar _ as v) when w > 1 ->
                        BSlice { signal = v; msb = bit; lsb = bit }
                    | _ -> in_bit_w sigw i p bit in
                  BAssign { lhs = Printf.sprintf "ufi__%s__%s_%d" ck p bit;
                            rhs })) ports in
          if asgs = [] then [], true
          else [ comb ("exp_cut_" ^ i.inst_name) asgs ], false
      | _ ->
      if ports = [] then [], true
      else begin
        let rec norm_e = function
          | BVar "GND" -> zero | BVar "VCC" -> one
          | BConcat es -> BConcat (List.map norm_e es)
          | BSlice { signal; msb; lsb } -> BSlice { signal = norm_e signal; msb; lsb }
          | e -> e in
        let args = List.filter_map (fun (p, dir, _w) ->
          match dir with
          | `Input -> Some (match port_expr i p with Some e -> norm_e e | None -> zero)
          | `Output -> None) ports in
        let args = if args = [] then [ zero ] else args in
        let asgs = List.concat_map (fun (p, dir, w) ->
          match dir with
          | `Output ->
              (match out_nets i p with
               | [ single ] when w > 1 && not (String.contains single ']') ->
                   (* WHOLE-BUS output connection (`.status_vector(^st)`): the
                      gate-mapped miter side bit-blasts the same port into w
                      scalar nets and calls `<m>__<p>_<bit>` PER BIT — one
                      w-bit call here would give the SAME UF symbol a
                      different result sort (a different Z3 FuncDecl), so the
                      sides can never agree.  Emit per-bit calls onto
                      `single[bit]`; resolve_input_bitbus canonicalises the
                      bracket-string writes onto the obuf convention and
                      reconstructs the bus. *)
                   List.init w (fun bit ->
                     BAssign { lhs = Printf.sprintf "%s[%d]" single bit;
                               rhs = BCall { func = Printf.sprintf "%s__%s_%d" m p bit;
                                             args } })
               | nets ->
                   List.mapi (fun bit n ->
                     BAssign { lhs = n;
                               rhs = BCall { func = Printf.sprintf "%s__%s_%d" m p bit;
                                             args } })
                     nets)
          | `Input -> []) ports in
        if asgs = [] then [], true
        else [ comb ("exp_uf_" ^ i.inst_name) asgs ], false
      end

let clone_re1 = Str.regexp "^\\(.+\\)_[0-9]+$"

(* Normalise per-instance module VARIANT names back to a canonical base so
   child keys pair across the two flows:
   - Vivado write_verilog clones:  RAMB16_S36_S36_xc7__1, async_fifo_rd__1,
     dualmem64__parameterized0, rgmii_lfsr_0
   - SVS elaboration specialisations:  async_fifo_wr__DW72_AW5,
     gig_ethernet_pcs_pma_0_support__CGL0
   Rule: cut at the FIRST "__" (both conventions suffix with double
   underscore; applied unconditionally so it is SYMMETRIC even when one
   side's base module no longer exists).  The single-underscore digit form
   (`rgmii_lfsr_0`) is stripped only when the base is a known module —
   protects names that merely end in digits (eth_mac_1g, ..._pcs_pma_0). *)
let norm_clone mn =
  (* SYMMETRIC variant-suffix strip: cut at the FIRST "__" (Vivado clones
     `__1`/`__parameterized0` AND SVS specialisations `__DW72_AW5`/`__CGL0`
     both use double underscore) — unconditional, so both sides normalise
     identically even when a side no longer carries the base module.  The
     single-underscore digit form (`rgmii_lfsr_0`) stays gated on the base
     being a known module (protects ..._pcs_pma_0 / eth_mac_1g). *)
  let rec first_dd i =
    if i + 1 >= String.length mn then None
    else if mn.[i] = '_' && mn.[i+1] = '_' then Some i
    else first_dd (i + 1) in
  match first_dd 1 with
  | Some i -> String.sub mn 0 i
  | None ->
      if Str.string_match clone_re1 mn 0 then begin
        let base = Str.matched_group 1 mn in
        if Hashtbl.mem user_ports base then base else mn
      end else mn

let expand_module (mo : bmodule) : bmodule =
  (* Canonical child key = NORMALISED module name + ORDINAL among same-module
     siblings (sorted by inst_name): identical across the two miter flows even
     though each flow names instances (and clones modules) differently. *)
  let canon_of : (string, string) Hashtbl.t = Hashtbl.create 8 in
  let by_mod : (string, string list ref) Hashtbl.t = Hashtbl.create 8 in
  List.iter (fun (i : binstance) ->
    if Hashtbl.mem user_ports i.module_name
       || Hashtbl.mem (Lazy.force Bir_to_edif.xil_json_ports) i.module_name
    then begin
      let key = norm_clone i.module_name in
      match Hashtbl.find_opt by_mod key with
      | Some l -> l := i.inst_name :: !l
      | None -> Hashtbl.add by_mod key (ref [ i.inst_name ])
    end) mo.instances;
  Hashtbl.iter (fun mn l ->
    List.iteri (fun k inst ->
      Hashtbl.replace canon_of inst (Printf.sprintf "%s_%d" mn k))
      (List.sort compare !l)) by_mod;
  let sigw : (string, int) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (s : bsignal) ->
    match s.stype with
    | BInt { width; _ } -> Hashtbl.replace sigw s.name width
    | BBool -> Hashtbl.replace sigw s.name 1
    | _ -> ()) mo.signals;
  let new_procs = ref [] and kept = ref [] in
  List.iter (fun i ->
    let procs, keep =
      expand_instance ?canon:(Hashtbl.find_opt canon_of i.inst_name) ~sigw i in
    new_procs := procs @ !new_procs;
    if keep then kept := i :: !kept) mo.instances;
  (* DECLARE the ufo__/ufi__ child-boundary nets the cut emission created —
     undeclared, the miter's undriven-internal matching never sees the ufo
     frees, so they stay UNPAIRED across sides and every child-fed cone
     spuriously differs. *)
  let bnd_sigs = ref [] and seen = Hashtbl.create 32 in
  let note n =
    if (String.length n > 5
        && (String.sub n 0 5 = "ufo__" || String.sub n 0 5 = "ufi__"))
       && not (Hashtbl.mem seen n) then begin
      Hashtbl.replace seen n ();
      bnd_sigs := { name = n; stype = BInt { width = 1; signed = Unsigned };
                    direction = `Internal; initial_value = None; attrs = [] }
                  :: !bnd_sigs
    end in
  List.iter (function
    | BCombinational { body; _ } ->
        List.iter (function
          | BAssign { lhs; rhs } ->
              note lhs;
              (match rhs with BVar n -> note n | _ -> ())
          | _ -> ()) body
    | _ -> ()) !new_procs;
  { mo with instances = List.rev !kept;
            signals = !bnd_sigs @ mo.signals;
            processes = mo.processes @ List.rev !new_procs }

(* After ffpack collapses `<bus>__b<idx>` bit-FFs into one bus FF `<bus>`, the
   LUT D-cones still read the pre-pack bit names.  Alias each back to the packed
   bus bit (`<bus>__b<idx> := <bus>[idx]`) so the register feedback closes. *)
let alias_packed_bits (m : bmodule) : bmodule =
  let extra_procs = ref [] and extra_sigs = ref [] in
  let has n =
    List.exists (fun (s : bsignal) -> s.name = n) m.signals
    || List.exists (fun (s : bsignal) -> s.name = n) !extra_sigs in
  let decl n w =
    if not (has n) then
      extra_sigs := { name = n; stype = BInt { width = w; signed = Unsigned };
                      direction = `Internal; initial_value = None; attrs = [] } :: !extra_sigs in
  let alias bitname bus i =
    decl bitname 1;
    extra_procs := BCombinational
      { name = "alias_" ^ bitname; sensitivity = [ BAny ];
        body = [ BAssign { lhs = bitname;
                           rhs = BSlice { signal = BVar bus; msb = i; lsb = i } } ] }
      :: !extra_procs in
  (* packed register buses (ffpack): alias <bus>__b<i> := <bus>[i] *)
  List.iter (function
    | BSequential { body = [ BAssign { lhs = bus; rhs = BConcat es } ]; _ } ->
        let w = List.length es in
        if w > 1 then begin
          decl bus w;
          for i = 0 to w - 1 do alias (Printf.sprintf "%s__b%d" bus i) bus i done
        end
    | _ -> ()) m.processes;
  (* input buses: the gate-map reads bit `x__i` (bir_to_aig port_bit_names)
     but the behavioural miter side has the bus `x` — alias `x__i := x[i]`
     so the two correspond (constrained equal on the shared bus input). *)
  List.iter (fun (s : bsignal) ->
    if s.direction = `Input then
      match s.stype with
      | BInt { width; _ } when width > 1 ->
          for i = 0 to width - 1 do alias (Printf.sprintf "%s__%d" s.name i) s.name i done
      | _ -> ()) m.signals;
  (* output buses: the gate-map's OBUFs drive bit-select nets `y[i]`, but the
     bus `y` itself is never assigned, so the miter sees it as 0.  Reconstruct
     `y := {y[w-1],…,y[0]}` (MSB-first) so the output bus matches. *)
  List.iter (fun (s : bsignal) ->
    if s.direction = `Output then
      match s.stype with
      | BInt { width; _ } when width > 1 ->
          let bits = List.init width (fun i -> BVar (Printf.sprintf "%s[%d]" s.name (width - 1 - i))) in
          extra_procs := BCombinational
            { name = "recon_" ^ s.name; sensitivity = [ BAny ];
              body = [ BAssign { lhs = s.name; rhs = BConcat bits } ] } :: !extra_procs
      | _ -> ()) m.signals;
  { m with signals = m.signals @ List.rev !extra_sigs;
           processes = m.processes @ List.rev !extra_procs }

let expand_program (p : bprogram) : bprogram =
  Hashtbl.reset mod_tbl;
  Hashtbl.reset child_out_consts;
  Hashtbl.reset child_reads_tbl;
  Hashtbl.reset child_out_alias_tbl;
  Hashtbl.reset child_out_passthru_tbl;
  List.iter (fun (m : bmodule) -> Hashtbl.replace mod_tbl m.name m) p.modules;
  let p = { p with modules = List.map expand_module p.modules } in
  (* Re-pack bit-blasted `<bus>__b<idx>` register FFs into a single bus-level
     BSequential so the miter's ffrip lines up with the behavioural bus reg. *)
  let p = Behavioral_ffpack.pack_program p in
  { p with modules = List.map alias_packed_bits p.modules }
