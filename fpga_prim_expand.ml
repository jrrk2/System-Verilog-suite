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
let one = BConst { value = 1; width = 1 }
let zero = BConst { value = 0; width = 1 }

let port_expr (i : binstance) name = List.assoc_opt name i.port_connections

(* Expand a bexpr into a LSB-first list of 1-bit bexprs. Gate-mapped port
 * connections are already scalar BVars or MSB-first BConcat of scalars. *)
let rec bits_of (e : bexpr) : bexpr list =
  match e with
  | BConcat es -> List.rev (List.concat_map (fun x -> List.rev (bits_of x)) es)
  | BConst { value; width } ->
      List.init width (fun b -> BConst { value = (value lsr b) land 1; width = 1 })
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
let is_vcc = function BVar "VCC" -> true | BConst { value = 1; _ } -> true | _ -> false
let is_gnd = function BVar "GND" -> true | BConst { value = 0; _ } -> true | _ -> false

let ff_process ~qn ~clk ~ce ~d ~rst ~rst_async ~rst_val =
  (* Drop the CE-hold mux when CE is tied high (the common case) so the
     packed D-cone doesn't carry dead references to the pre-pack bit nets. *)
  let d_ce = if is_vcc ce then d
             else BCond { condition = ce; then_val = d; else_val = BVar qn } in
  (* A reset tied to GND (sync) never fires — treat as no reset. *)
  let rst = match rst with Some (rsig, _) when is_gnd rsig && not rst_async -> None | r -> r in
  match clk with
  | BVar cn ->
      let mk ?reset ?reset_edge ~reset_async rhs =
        Some (BSequential { name = "exp_" ^ qn; clock = cn; clock_edge = `Pos;
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
 * instance in place (unhandled). *)
let expand_instance (i : binstance) : bprocess list * bool =
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
           (match ff_process ~qn ~clk ~ce ~d ~rst ~rst_async ~rst_val with
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
      let ports = box_ports m in
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
        let asgs = List.concat_map (fun (p, dir, _w) ->
          match dir with
          | `Output ->
              List.mapi (fun bit n ->
                BAssign { lhs = n;
                          rhs = BCall { func = Printf.sprintf "%s__%s_%d" m p bit; args } })
                (out_nets i p)
          | `Input -> []) ports in
        if asgs = [] then [], true
        else [ comb ("exp_uf_" ^ i.inst_name) asgs ], false
      end

let expand_module (mo : bmodule) : bmodule =
  let new_procs = ref [] and kept = ref [] in
  List.iter (fun i ->
    let procs, keep = expand_instance i in
    new_procs := procs @ !new_procs;
    if keep then kept := i :: !kept) mo.instances;
  { mo with instances = List.rev !kept;
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
  let p = { p with modules = List.map expand_module p.modules } in
  (* Re-pack bit-blasted `<bus>__b<idx>` register FFs into a single bus-level
     BSequential so the miter's ffrip lines up with the behavioural bus reg. *)
  let p = Behavioral_ffpack.pack_program p in
  { p with modules = List.map alias_packed_bits p.modules }
