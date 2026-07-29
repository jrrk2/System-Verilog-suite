(* BIR -> AIG adapter (the combinational front of the FPGA flow).
 *
 * Pipeline: Behavioral_ir --(behavioral_to_hardcaml)--> Hardcaml.Circuit
 * --(here)--> Lut_cover AIG.  We reuse Hardcaml as the gate-level IR:
 * behavioral_to_hardcaml already resolves processes / if / case /
 * registers into a Signal.t dataflow graph, so this stage only has to
 * bit-blast that graph into 2-input AIG nodes (the same Signal.Type
 * walk lib_map does for ASIC cells, but lowering arithmetic to gates
 * instead of emitting raw operators).
 *
 * Sequential elements are boundaries: a register's Q becomes an AIG
 * primary INPUT (named r<uid>_q<bit>) and its D-cone an AIG primary
 * OUTPUT (r<uid>_d<bit>).  The returned [regs] list lets a later pass
 * stitch FDRE/FDCE between the mapped D outputs and Q inputs.  Memories
 * and submodule instances are not yet lowered (DSP/BRAM territory). *)

open! Base

module S = Hardcaml.Signal
module T = Hardcaml.Signal.Type

(* ---- signed literals + hash-consing AIG builder ------------------ *)

(* A literal packs (node id, inversion): bit 0 = invert, rest = id. *)
type lit = int

let lit_node (l : lit) = l lsr 1
let lit_inv (l : lit) = l land 1 = 1
let mk_lit node inv = (node lsl 1) lor (if inv then 1 else 0)
let lit_not (l : lit) : lit = l lxor 1

(* node 0 is the canonical Const false; const_true is its complement. *)
let const_false : lit = 0
let const_true : lit = 1

type builder =
  { mutable gates : Lut_cover.gate list (* reversed; head = highest id *)
  ; mutable count : int
  ; and_hash : (int * int, lit) Hashtbl.t
  ; input_hash : (string, lit) Hashtbl.t
  }

let new_node b (g : Lut_cover.gate) : int =
  let id = b.count in
  b.gates <- g :: b.gates;
  b.count <- id + 1;
  id

let create_builder () : builder =
  let b =
    { gates = []
    ; count = 0
    ; and_hash = Hashtbl.Poly.create ()
    ; input_hash = Hashtbl.create (module String)
    }
  in
  let n0 = new_node b (Lut_cover.Const false) in
  assert (n0 = 0);
  b

let aig_input b name : lit =
  match Hashtbl.find b.input_hash name with
  | Some l -> l
  | None ->
    let id = new_node b (Lut_cover.Input name) in
    let l = mk_lit id false in
    Hashtbl.set b.input_hash ~key:name ~data:l;
    l

let aig_and b (x : lit) (y : lit) : lit =
  if x = const_false || y = const_false then const_false
  else if x = const_true then y
  else if y = const_true then x
  else if x = y then x
  else if x = lit_not y then const_false
  else begin
    let a, bb = if x <= y then x, y else y, x in
    match Hashtbl.find b.and_hash (a, bb) with
    | Some l -> l
    | None ->
      let id =
        new_node b
          (Lut_cover.And2
             { a = lit_node a; b = lit_node bb; a_inv = lit_inv a; b_inv = lit_inv bb })
      in
      let l = mk_lit id false in
      Hashtbl.set b.and_hash ~key:(a, bb) ~data:l;
      l
  end

let aig_or b x y = lit_not (aig_and b (lit_not x) (lit_not y))
let aig_xor b x y = aig_or b (aig_and b x (lit_not y)) (aig_and b (lit_not x) y)
let aig_xnor b x y = lit_not (aig_xor b x y)

(* s ? t : f *)
let aig_mux b s t f = aig_or b (aig_and b s t) (aig_and b (lit_not s) f)

(* ---- bit-vector helpers (lit arrays, index 0 = LSB) -------------- *)

let vbit (v : lit array) i = if i < Array.length v then v.(i) else const_false

let fit (v : lit array) n = Array.init n ~f:(fun i -> vbit v i)

(* ripple-carry add of [a]+[b]+carry_in, low [n] bits (carry-out dropped).
   Pure-AIG fallback — slow on FPGA because the carry threads through
   general LUT routing.  [v_add_carry4] below emits dedicated CARRY4
   cells for [Signal_add]/[Signal_sub] in [lower_circuit] instead. *)
let v_add b a bb ~carry_in ~n =
  let cy = ref carry_in in
  Array.init n ~f:(fun i ->
    let ai = vbit a i and bi = vbit bb i in
    let axb = aig_xor b ai bi in
    let s = aig_xor b axb !cy in
    cy := aig_or b (aig_and b ai bi) (aig_and b !cy axb);
    s)

(* unsigned a < b via the borrow out of a-b over [n] bits. *)
let v_lt b a bb ~n =
  let borrow = ref const_false in
  for i = 0 to n - 1 do
    let ai = vbit a i and bi = vbit bb i in
    borrow :=
      aig_or b
        (aig_and b (lit_not ai) bi)
        (aig_and b (lit_not (aig_xor b ai bi)) !borrow)
  done;
  [| !borrow |]

let v_eq b a bb ~n =
  let acc = ref const_true in
  for i = 0 to n - 1 do
    acc := aig_and b !acc (aig_xnor b (vbit a i) (vbit bb i))
  done;
  [| !acc |]

(* unsigned shift-add multiplier; result width = wa+wb. *)
let v_mul b a bb =
  let wa = Array.length a and wb = Array.length bb in
  let acc = ref (Array.create ~len:(wa + wb) const_false) in
  for j = 0 to wb - 1 do
    let pj =
      Array.init (wa + wb) ~f:(fun i ->
        let k = i - j in
        if k >= 0 && k < wa then aig_and b a.(k) bb.(j) else const_false)
    in
    acc := v_add b !acc pj ~carry_in:const_false ~n:(wa + wb)
  done;
  !acc

(* split a list into (even-index, odd-index) elements, order preserved. *)
let split_parity lst =
  let rec go i = function
    | [] -> [], []
    | x :: xs ->
      let e, o = go (i + 1) xs in
      if i land 1 = 0 then x :: e, o else e, x :: o
  in
  go 0 lst

let pad_to n lst =
  let len = List.length lst in
  if len >= n then List.take lst n
  else lst @ List.init (n - len) ~f:(fun _ -> List.last_exn lst)

(* N:1 mux of single bits, selector bits LSB-first, leaves indexed by
   selector value (already padded to 2^|selbits|). *)
let rec select_mux b selbits leaves =
  match selbits with
  | [] -> List.hd_exn leaves
  | s :: rest ->
    let evens, odds = split_parity leaves in
    let e = select_mux b rest evens and o = select_mux b rest odds in
    aig_mux b s o e

let v_mux b ~sel ~cases ~w =
  let sel_w = Array.length sel in
  let cases = pad_to (1 lsl sel_w) cases in
  let selbits = Array.to_list sel in
  Array.init w ~f:(fun bit ->
    let leaves = List.map cases ~f:(fun c -> vbit c bit) in
    select_mux b selbits leaves)

(* ---- register boundary + result ---------------------------------- *)

type reg_boundary =
  { rb_q_names : string list (* AIG inputs for Q bits, LSB-first *)
  ; rb_d_names : string list (* AIG outputs for D bits, LSB-first *)
  ; rb_clock : string
  ; rb_reset : string option (* async reset net, if any *)
  ; rb_reset_neg : bool
      (* ACTIVE-LOW async reset (`negedge rst_n` / Hardcaml Edge.Falling).
         fpga_map must invert the net before FDCE.CLR / FDPE.PRE (which are
         active-HIGH pins) — wiring it raw held the FF in reset whenever the
         reset was DEASSERTED (gmii_rst_sync PRE = mmcm_locked uninverted:
         MAC permanently reset while the MMCM is locked). *)
  ; rb_enable : string option
      (* clock-enable net, if the register's D is an enable feedback mux
         [mux en dnext Q].  Lifting [en] onto the FDRE/FDCE CE pin (instead
         of folding the mux + Q-feedback into the D LUT cone) is exactly what
         the hardware CE pin is for: it saves a LUT input + the fabric-routed
         feedback loop, and keeps the shared enable off the per-FF D-LUT
         fanout that otherwise saturates the control network. *)
  ; rb_width : int
  ; rb_init  : int option
      (* BIR-level initial_value, threaded through Hardcaml's
         Reg_spec.override ~reset_to.  FPGA mappers emit this as an
         FDRE INIT parameter (per-bit slice).  ASIC mappers should
         REJECT a register with rb_init <> None and rb_reset = None
         since ASIC FFs have no config-time init — the source code
         must be rewritten with an explicit reset block. *)
  ; rb_sync_reset : string option
      (* SYNCHRONOUS reset/set net, lifted from a constant-armed top-level
         D-mux [mux srst K inner] that behavioral_to_hardcaml folds into the
         D cone (it does NOT use Hardcaml's async reg_reset for sync resets).
         Only lifted when rb_reset = None (a FF cannot have both async CLR/PRE
         and sync R/S).  Per bit: rb_srval bit 1 -> FDSE (sync set to 1),
         bit 0 -> FDRE (sync reset to 0).  R/S has priority over CE on the
         primitive, matching the folded [mux srst K (mux en Q dnext)]. *)
  ; rb_srval : int option
      (* per-bit synchronous reset VALUE K (LSB-first), the constant arm of
         the peeled reset mux; selects FDRE vs FDSE and the FF INIT. *)
  }

(* A black-box instance, lowered as a generalized boundary: its OUTPUT
   ports' bits are AIG primary inputs (like a register Q), and each INPUT
   port's cone bits are AIG primary outputs (like a register D).  fpga_map
   re-instantiates the box and wires these buses. *)
type inst_boundary =
  { ib_name : string (* module / cell type, e.g. "RAMB18E1" *)
  ; ib_instance : string
  ; ib_generics : Hardcaml.Parameter.t list
  ; ib_in_ports : (string * string list) list
      (* input port -> per-bit AIG-output names (cone driving the box), LSB-first *)
  ; ib_out_ports : (string * string list) list
      (* output port -> per-bit AIG-input names (box output), LSB-first *)
  }

type lowered =
  { graph : Lut_cover.graph
  ; regs : reg_boundary list
  ; insts : inst_boundary list
  ; inputs : (string * int) list (* primary input ports (name, width) *)
  }

let finalize b ~outputs : Lut_cover.graph =
  let gates = Array.of_list (List.rev b.gates) in
  let nodes = Array.mapi gates ~f:(fun id g -> { Lut_cover.id; gate = g }) in
  let outs = List.map outputs ~f:(fun (nm, l) -> nm, lit_node l, lit_inv l) in
  { Lut_cover.nodes; outputs = outs }

(* ---- AIG balancing -------------------------------------------------------
   Rebuild AND-supergates as balanced (minimum-depth) trees.  AND is
   associative, so collecting the leaves reachable through fanout-1,
   non-inverted And2 children and recombining them lowest-level-first
   shortens logic depth WITHOUT changing the boolean function.  OR chains are
   handled transparently: an OR is a complemented AND of complemented inputs,
   so balancing the inner AND balances the OR too.  Primary input/output names
   are preserved exactly (the reg/inst boundary contract with fpga_map).      *)
let balance_once (g : Lut_cover.graph) : Lut_cover.graph =
  let n = Array.length g.nodes in
  let is_and i = match g.nodes.(i).Lut_cover.gate with
    | Lut_cover.And2 _ -> true | _ -> false in
  let fanout = Array.create ~len:n 0 in
  Array.iter g.nodes ~f:(fun nd -> match nd.Lut_cover.gate with
    | Lut_cover.And2 { a; b; _ } ->
        fanout.(a) <- fanout.(a) + 1; fanout.(b) <- fanout.(b) + 1
    | _ -> ());
  List.iter g.outputs ~f:(fun (_, id, _) -> fanout.(id) <- fanout.(id) + 1);
  let b = create_builder () in
  let newlit = Array.create ~len:n const_false in
  let level  = Array.create ~len:n 0 in
  let factors : (lit * int) list array = Array.create ~len:n [] in
  let rec insert x = function           (* keep level-ascending *)
    | [] -> [ x ]
    | h :: t -> if snd x <= snd h then x :: h :: t else h :: insert x t in
  let rec reduce = function              (* combine two lowest-level first *)
    | [] -> (const_true, 0)
    | [ x ] -> x
    | (l1, v1) :: (l2, v2) :: rest ->
        reduce (insert (aig_and b l1 l2, 1 + Int.max v1 v2) rest) in
  Array.iter g.nodes ~f:(fun nd ->
    let i = nd.Lut_cover.id in
    match nd.Lut_cover.gate with
    | Lut_cover.Const c ->
        newlit.(i) <- (if c then const_true else const_false); level.(i) <- 0
    | Lut_cover.Input name -> newlit.(i) <- aig_input b name; level.(i) <- 0
    | Lut_cover.And2 { a; b = bb; a_inv; b_inv } ->
        let edge child inv =
          let l = newlit.(child) in if inv then lit_not l else l in
        let expand child inv =
          if (not inv) && fanout.(child) = 1 && is_and child
          then factors.(child)
          else [ (edge child inv, level.(child)) ] in
        let leaves =
          List.fold (expand a a_inv @ expand bb b_inv) ~init:[]
            ~f:(fun acc x -> insert x acc) in
        let (res, lv) = reduce leaves in
        newlit.(i) <- res; level.(i) <- lv; factors.(i) <- leaves);
  let outs = List.map g.outputs ~f:(fun (nm, id, inv) ->
    let l = newlit.(id) in nm, (if inv then lit_not l else l)) in
  finalize b ~outputs:outs

let balance ?(passes = 2) (g : Lut_cover.graph) : Lut_cover.graph =
  let rec go k g = if k <= 0 then g else go (k - 1) (balance_once g) in
  go (Int.max 0 passes) g

(* ---- the walk ---------------------------------------------------- *)

let uid_int s = T.Uid.to_int (T.uid s)

let signal_name s =
  match S.names s with
  | n :: _ -> n
  | [] -> Printf.sprintf "_n%d" (uid_int s)

(* per-bit AIG input names for a w-bit port. *)
let port_bit_names nm w =
  if w = 1 then [ nm ] else List.init w ~f:(fun i -> Printf.sprintf "%s__%d" nm i)

let lower_circuit (circ : Hardcaml.Circuit.t) : lowered =
  let b = create_builder () in
  let memo = Hashtbl.create (module Int) in
  let reg_queue = Queue.create () in
  let regs = ref [] in
  let insts = ref [] in
  let inst_outs = ref [] in
  (* Reverse map: a black-box OUTPUT lit -> its generated boundary net name
     (`i<key>_<port>_o<bit>`).  Those generated names are the canonical
     boundary I/O (unique per box, so manually-instanced primitives survive
     AIG conversion).  When an FF's async-reset (or any consumer) net IS such a
     box output, we rename that consumer to the boundary name so fpga_map
     bridges it to the on-chip driver (q_wire) instead of minting a fresh input
     pad — the mmcm_locked boundary break. *)
  let boundary_of_lit : (int, string) Hashtbl.t = Hashtbl.create (module Int) in
  let carry4_counter = ref 0 in
  (* Minimum operand width to map an add / sub / compare onto a dedicated
     CARRY4 chain; narrower ops lower to plain AIG (ripple) so the LUT cover
     packs them into ordinary LUTs — exactly what Vivado does.  Unconditional
     CARRY4 (the old behaviour) produced ~2822 carries for ibex-mini vs Vivado's
     76: a carry per tiny compare/increment, each burning a whole slice + S/DI
     LUTs and fighting the placer/router (and Vivado's opt_design can't un-map a
     carry).  Threshold via CARRY4_MIN (default 16).  16 (was 8) keeps only the
     wide adders on carries and lets the LUT cover — abc especially — pack the
     short ones: on ibex-mini + abc this drops CARRY4 842->318 AND LUTs
     16421->16058 (plus ~1.7k fewer carry_stamp routethru LUT1 downstream), with
     conformance preserved.  Long adders still benefit from CARRY4 (all-AIG is
     worse), so a threshold — not "abc everything" — is the right policy. *)
  let carry4_min =
    match Sys.getenv "CARRY4_MIN" with
    | Some s -> (try Int.of_string s with _ -> 16)
    | None -> 16 in
  (* Decompose a + b + carry_in (low w bits) into ceil(w/4) chained
     CARRY4 instances.  S = a XOR b is still a 1-LUT per bit (LUT2),
     but the carry propagates through Xilinx's dedicated carry chain
     instead of general LUT routing — picosoc carry-add chains run
     much faster (target: Fmax > 25 MHz, vs 18.6 MHz with all-AIG).

     CARRY4 truth (UG953):
       O[i]  = S[i] XOR cin_chain[i]      (sum)
       CO[i] = S[i] ? cin_chain[i] : DI[i]  (carry)
       cin_chain[0] = (CYINIT or CI; exactly one driven, other 0)
       cin_chain[i>0] = CO[i-1]
     For a+b: S[i] = a[i] XOR b[i], DI[i] = a[i].
     Returns ([sum bits LSB-first], top carry-out lit).  The CO is
     the full carry chain output at bit (w-1), used by comparator
     lowerings (a<b = !(a + ~b + 1).CO_top, etc.). *)
  let emit_carry4_chain ~carry_in s_lits di_lits ~w =
    let n_blocks = (w + 3) / 4 in
    let out_lits = Array.create ~len:w const_false in
    let prev_co3_name = ref None in
    let top_co_lit = ref const_false in
    for k = 0 to n_blocks - 1 do
      let block_id = !carry4_counter in
      Int.incr carry4_counter;
      let base = Printf.sprintf "c4_%d" block_id in
      let mk_bit_names port =
        List.init 4 ~f:(fun j -> Printf.sprintf "%s_%s_%d" base port j)
      in
      (* Pad the unused upper lanes of the top CARRY4 (bit_idx >= w) with the
         last real S/DI lit rather than a constant 0.  Those lanes' sum and
         carry-out feed nothing (the chain's used carry-out is taken at lane
         (w-1) mod 4), so the value is a don't-care — but tying them to GND
         makes them $PACKER_GND_NET sinks on the CARRY4.S/DI pins, which are
         driven by the slice's own LUTs and so are NOT reachable by general
         constant routing (they'd be left unrouted).  Reusing a real local
         signal keeps every lane routable. *)
      let s_names = mk_bit_names "S" in
      let di_names = mk_bit_names "DI" in
      let pad_s  = s_lits.(w - 1) in
      let pad_di = di_lits.(w - 1) in
      List.iteri s_names ~f:(fun j nm ->
        let bit_idx = (4 * k) + j in
        let lit = if bit_idx < w then s_lits.(bit_idx) else pad_s in
        inst_outs := (nm, lit) :: !inst_outs);
      List.iteri di_names ~f:(fun j nm ->
        let bit_idx = (4 * k) + j in
        let lit = if bit_idx < w then di_lits.(bit_idx) else pad_di in
        inst_outs := (nm, lit) :: !inst_outs);
      (* Carry-in source: the chain head uses CYINIT (driven by carry_in);
         upper blocks cascade via CI from the block below.  Each block must
         drive EXACTLY ONE of the two (see in_ports): CI is a dedicated
         cascade pin that can't take a routed constant, and driving CYINIT
         and CI on the same block makes the fasm backend emit conflicting
         PRECYINIT.AX and PRECYINIT.CIN bits for that slice. *)
      let o_names = List.init 4 ~f:(fun j -> Printf.sprintf "%s_O_%d" base j) in
      let co_names = List.init 4 ~f:(fun j -> Printf.sprintf "%s_CO_%d" base j) in
      let o_lits = List.map o_names ~f:(aig_input b) in
      let co_lits = List.map co_names ~f:(aig_input b) in
      List.iteri o_lits ~f:(fun j l ->
        let bit_idx = (4 * k) + j in
        if bit_idx < w then out_lits.(bit_idx) <- l);
      (* The carry-out at bit w-1 lives in block k = (w-1)/4 at position
         (w-1) mod 4.  Record it as we walk the chain. *)
      let last_block = n_blocks - 1 in
      if k = last_block
      then begin
        let last_bit = (w - 1) - (4 * k) in
        top_co_lit := List.nth_exn co_lits last_bit
      end;
      let in_ports =
        let common = [ "DI", di_names; "S", s_names ] in
        match !prev_co3_name with
        | Some prev -> ("CI", [ prev ]) :: common      (* upper block: CI cascade, no CYINIT *)
        | None ->                                       (* head block: CYINIT only, no CI *)
            let cyinit_name = Printf.sprintf "%s_CYINIT" base in
            inst_outs := (cyinit_name, carry_in) :: !inst_outs;
            ("CYINIT", [ cyinit_name ]) :: common
      in
      let out_ports = [ "O", o_names; "CO", co_names ] in
      insts
      := { ib_name = "CARRY4"
         ; ib_instance = base
         ; ib_generics = []
         ; ib_in_ports = in_ports
         ; ib_out_ports = out_ports
         }
         :: !insts;
      prev_co3_name := List.nth co_names 3
    done;
    out_lits, !top_co_lit
  in
  let v_add_carry4 ~carry_in a bb ~w =
    let s_lits = Array.init w ~f:(fun i -> aig_xor b (vbit a i) (vbit bb i)) in
    let di_lits = Array.init w ~f:(fun i -> vbit a i) in
    let sum, _co = emit_carry4_chain ~carry_in s_lits di_lits ~w in
    sum
  in
  (* a < b (unsigned), via CARRY4 of a + ~b + 1.  Top CO = 0 iff a < b
     (the subtract borrowed).  Returns a 1-bit lit. *)
  let v_lt_carry4 a bb ~w =
    let s_lits = Array.init w ~f:(fun i -> aig_xnor b (vbit a i) (vbit bb i)) in
    let di_lits = Array.init w ~f:(fun i -> vbit a i) in
    let _sum, co = emit_carry4_chain ~carry_in:const_true s_lits di_lits ~w in
    [| lit_not co |]
  in
  let rec walk sig_ : lit array =
    if T.is_empty sig_ then [||]
    else begin
      let key = uid_int sig_ in
      match Hashtbl.find memo key with
      | Some v -> v
      | None ->
        let w = S.width sig_ in
        let v =
          let open T in
          match sig_ with
          | Empty -> [||]
          | Const { constant; _ } ->
            let s = Hardcaml.Bits.to_string constant in
            Array.init w ~f:(fun i ->
              if Char.equal s.[w - 1 - i] '1' then const_true else const_false)
          | Op2 { op; arg_a; arg_b; _ } ->
            let a = walk arg_a and bb = walk arg_b in
            let n = Int.max (Array.length a) (Array.length bb) in
            (match op with
             | Signal_and -> Array.init w ~f:(fun i -> aig_and b (vbit a i) (vbit bb i))
             | Signal_or -> Array.init w ~f:(fun i -> aig_or b (vbit a i) (vbit bb i))
             | Signal_xor -> Array.init w ~f:(fun i -> aig_xor b (vbit a i) (vbit bb i))
             | Signal_eq -> v_eq b a bb ~n
             | Signal_lt ->
               if n >= carry4_min then v_lt_carry4 a bb ~w:n
               else v_lt b a bb ~n
             | Signal_add ->
               if w >= carry4_min then v_add_carry4 ~carry_in:const_false a bb ~w
               else v_add b a bb ~carry_in:const_false ~n:w
             | Signal_sub ->
               let nb = Array.map bb ~f:lit_not in
               if w >= carry4_min then v_add_carry4 ~carry_in:const_true a nb ~w
               else v_add b a nb ~carry_in:const_true ~n:w
             | Signal_mulu -> fit (v_mul b a bb) w
             | Signal_muls ->
               Stdlib.prerr_endline
                 "[bir_to_aig] WARN: signed * lowered as unsigned (sign ignored)";
               fit (v_mul b a bb) w
             | _ -> failwith "bir_to_aig: unhandled Op2")
          | Not { arg; _ } -> Array.map (walk arg) ~f:lit_not
          | Mux { select; cases; _ } ->
            let sel = walk select in
            let cases = List.map cases ~f:walk in
            v_mux b ~sel ~cases ~w
          | Wire { driver; _ } ->
            if is_empty !driver then begin
              let nm = signal_name sig_ in
              Array.of_list (List.map (port_bit_names nm w) ~f:(aig_input b))
            end
            else walk !driver
          | Cat { args; _ } -> Array.concat (List.rev_map args ~f:walk)
          | Select { arg; high; low; _ } ->
            let a = walk arg in
            Array.init (high - low + 1) ~f:(fun i -> vbit a (low + i))
          | Reg { register; d; _ } ->
            (* Preserve the source register net name (like the ASIC path's
               lib_map.net_for_signal / bit_net) so the gate-mapped netlist's
               register Q nets stay name-correspondent with the behavioural
               source.  Z3_miter's ffrip matches state BY NAME across the two
               designs; the old `r<uid>` naming made every FPGA-mapped
               register anonymous and broke that correspondence (ASIC LEC
               worked, FPGA LEC didn't).  Fall back to `r<uid>` only for a
               truly-unnamed reg. *)
            let raw = signal_name sig_ in
            let base =
              if String.length raw >= 2 && Char.equal raw.[0] '_'
                 && Char.equal raw.[1] 'n'
              then Printf.sprintf "r%d" key else raw in
            let q_names =
              if w = 1 then [ base ]
              (* `<bus>__b<idx>` — the convention Behavioral_ffpack re-packs
                 into a single bus FF so ffrip lines up with a behavioural
                 bus reg (`<bus>__Q`). *)
              else List.init w ~f:(fun i -> Printf.sprintf "%s__b%d" base i) in
            let d_names = List.init w ~f:(fun i -> Printf.sprintf "%s__d%d" base i) in
            let q_lits = Array.of_list (List.map q_names ~f:(aig_input b)) in
            let clk = signal_name register.reg_clock in
            let rst =
              if is_empty register.reg_reset then None
              else
                (* If the async-reset net is a black-box output, use its
                   generated boundary name so fpga_map resolves it to the box
                   driver (q_wire) rather than a fresh input pad (which splits
                   the net — the mmcm_locked break).  Walking it also forces the
                   box to be lowered.  Otherwise it's a real pad/net: use its
                   signal name. *)
                let rlits = walk register.reg_reset in
                let boundary =
                  if Array.length rlits = 1 then
                    (match Hashtbl.find boundary_of_lit rlits.(0) with
                     | Some nm -> Some nm
                     | None -> Hashtbl.find boundary_of_lit (rlits.(0) lxor 1))
                  else None in
                (match boundary with
                 | Some nm -> Some nm
                 | None -> Some (signal_name register.reg_reset))
            in
            (* Extract init value from Hardcaml's [reg_reset_value] when
               it's a constant.  behavioral_to_hardcaml threads the BIR
               [initial_value] there via Reg_spec.override ~reset_to. *)
            let init =
              match register.reg_reset_value with
              | Const { constant; _ } ->
                let bits = Hardcaml.Bits.to_string constant in
                (* bits is MSB-first; parse to int.  Width matches w. *)
                let n = String.length bits in
                let v = ref 0 in
                for i = 0 to n - 1 do
                  if Char.equal bits.[i] '1' then
                    v := !v lor (1 lsl (n - 1 - i))
                done;
                if !v = 0 then None else Some !v
              | _ -> None
            in
            (* Enable extraction: if D is [mux en dnext Q] (Q = this reg's own
               output fed back), lift [en] onto CE and register only [dnext]
               as the D cone.  Conservative: only the case-0-is-self form
               ([mux sel dnext self] = en active-high, hold in else); anything
               else keeps the full D (correctness over coverage). *)
            let rec resolve s =
              match s with
              | T.Wire { driver; _ } when not (is_empty !driver) -> resolve !driver
              | _ -> s
            in
            (if (match Stdlib.Sys.getenv_opt "CE_LIFT_STATS" with
                 | Some "1" -> true | _ -> false) then begin
               let is_self s = uid_int (resolve s) = uid_int sig_ in
               let is_const s = match resolve s with T.Const _ -> true | _ -> false in
               let self_mux s = match resolve s with
                 | T.Mux { cases = [ a; b ]; select; _ }
                   when S.width select = 1 && (is_self a || is_self b) -> true
                 | _ -> false in
               let cls = match resolve d with
                 | T.Mux { cases = [ a; b ]; select; _ } when S.width select = 1 ->
                   if is_self a || is_self b then "EN_TOP"
                   else if (is_const a && self_mux b) || (is_const b && self_mux a)
                   then "RST_over_EN"
                   else if is_const a || is_const b then "RST_only"
                   else "MUX_other"
                 | T.Mux _ -> "MUX_wide"
                 | _ -> "PLAIN" in
               let has_async = match rst with Some _ -> 1 | None -> 0 in
               Stdlib.Printf.eprintf "CE_LIFT_STAT %s async=%d w=%d\n" cls has_async w
             end);
            let is_const s = match resolve s with T.Const _ -> true | _ -> false in
            let const_int s =
              match resolve s with
              | T.Const { constant; _ } ->
                let bits = Hardcaml.Bits.to_string constant in
                let n = String.length bits in
                let v = ref 0 in
                for i = 0 to n - 1 do
                  if Char.equal bits.[i] '1' then v := !v lor (1 lsl (n - 1 - i))
                done;
                !v
              | _ -> 0
            in
            (* Synchronous reset/set lift: peel a constant-armed top-level
               D-mux [mux srst K inner] onto the FF's dedicated R/S pin.
               behavioral_to_hardcaml folds sync resets into the D cone (it
               reserves Hardcaml's async reg_reset for true async resets), so
               without this the reset value + its mux burn a LUT layer that
               Vivado spends on the FDRE.R / FDSE.S pin instead.  Only lifted
               when rb_reset = None — a 7-series FF has EITHER async CLR/PRE OR
               sync R/S, never both.  R/S has priority over CE on the
               primitive, exactly matching [mux srst K (mux en Q dnext)]. *)
            let sync_lift_on =
              match Stdlib.Sys.getenv_opt "CE_SYNC_LIFT" with
              | Some "0" -> false | _ -> true in
            let rb_sync_reset, rb_srval, d1 =
              if Option.is_some rst || not sync_lift_on then (None, None, d)
              else match resolve d with
                | T.Mux { select; cases = [ c0; c1 ]; _ }
                  when S.width select = 1 && is_const c1 && not (is_const c0) ->
                  (* select=1 -> K (reset active-high); select=0 -> inner. *)
                  let sr = Printf.sprintf "%s__sr" base in
                  Queue.enqueue reg_queue (select, [ sr ]);
                  (Some sr, Some (const_int c1), c0)
                | T.Mux { select; cases = [ c0; c1 ]; _ }
                  when S.width select = 1 && is_const c0 && not (is_const c1) ->
                  (* select=0 -> K (reset active-low); invert so R/S stays
                     active-high on the primitive. *)
                  let sr = Printf.sprintf "%s__sr" base in
                  Queue.enqueue reg_queue (S.( ~: ) select, [ sr ]);
                  (Some sr, Some (const_int c0), c1)
                | _ -> (None, None, d)
            in
            let rb_enable, eff_d =
              match resolve d1 with
              | T.Mux { select; cases = [ c0; c1 ]; _ }
                when S.width select = 1 && uid_int (resolve c0) = uid_int sig_ ->
                let en_name = Printf.sprintf "%s__ce" base in
                Queue.enqueue reg_queue (select, [ en_name ]);
                (Some en_name, c1)
              | _ -> (None, d1)
            in
            regs :=
              { rb_q_names = q_names
              ; rb_d_names = d_names
              ; rb_clock = clk
              ; rb_reset = rst
              ; rb_reset_neg =
                  (match register.reg_reset_edge with
                   | Falling -> true
                   | Rising -> false)
              ; rb_enable
              ; rb_width = w
              ; rb_init = init
              ; rb_sync_reset
              ; rb_srval
              }
              :: !regs;
            Queue.enqueue reg_queue (eff_d, d_names);
            q_lits
          | Inst { instantiation = inst; _ } ->
            let base = Printf.sprintf "i%d" key in
            (* output ports -> AIG primary inputs (the box's output bus). *)
            let out_lits = Array.create ~len:w const_false in
            let ob_out =
              List.map inst.inst_outputs ~f:(fun (pname, (pw, off)) ->
                let names =
                  List.init pw ~f:(fun bit -> Printf.sprintf "%s_%s_o%d" base pname bit)
                in
                List.iteri names ~f:(fun bit nm ->
                  let l = aig_input b nm in
                  out_lits.(off + bit) <- l;
                  Hashtbl.set boundary_of_lit ~key:l ~data:nm);
                pname, names)
            in
            Hashtbl.set memo ~key ~data:out_lits;
            (* input ports -> AIG primary outputs (cones into the box). *)
            let ob_in =
              List.map inst.inst_inputs ~f:(fun (pname, s) ->
                let lits = walk s in
                let names =
                  List.mapi (Array.to_list lits) ~f:(fun bit lit ->
                    let nm = Printf.sprintf "%s_%s_i%d" base pname bit in
                    inst_outs := (nm, lit) :: !inst_outs;
                    nm)
                in
                pname, names)
            in
            insts :=
              { ib_name = inst.inst_name
              ; ib_instance = inst.inst_instance
              ; ib_generics = inst.inst_generics
              ; ib_in_ports = ob_in
              ; ib_out_ports = ob_out
              }
              :: !insts;
            out_lits
          | Multiport_mem _ | Mem_read_port _ ->
            failwith
              "bir_to_aig: raw hardcaml memory not lowered — route memories \
               through behavioral_memlower to fixed-shape BRAM binstances"
        in
        Hashtbl.set memo ~key ~data:v;
        v
    end
  in
  let out_of_signal s =
    let nm = signal_name s in
    let w = S.width s in
    let lits = walk s in
    List.mapi (port_bit_names nm w) ~f:(fun i name -> name, lits.(i))
  in
  let main_outs = List.concat_map (Hardcaml.Circuit.outputs circ) ~f:out_of_signal in
  let reg_outs = ref [] in
  let rec drain () =
    match Queue.dequeue reg_queue with
    | None -> ()
    | Some (d, d_names) ->
      let lits = walk d in
      List.iteri d_names ~f:(fun i nm -> reg_outs := (nm, lits.(i)) :: !reg_outs);
      drain ()
  in
  drain ();
  let graph =
    finalize b ~outputs:(main_outs @ List.rev !reg_outs @ List.rev !inst_outs)
  in
  let inputs =
    List.map (Hardcaml.Circuit.inputs circ) ~f:(fun s -> signal_name s, S.width s)
  in
  { graph; regs = !regs; insts = !insts; inputs }
