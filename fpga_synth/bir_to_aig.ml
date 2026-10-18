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
     carry).  Threshold via CARRY4_MIN (default 8).  Raising it to 16 lets the
     LUT cover (abc especially) pack the short adders — on ibex-mini + abc it
     drops CARRY4 842->318 and LUTs 16421->16058 (plus ~1.7k fewer carry_stamp
     routethru LUT1) with SIM conformance preserved (22/22) — BUT the extra
     LUT-ripple adders BROKE THE CORE ON SILICON (no LEDs / conformance gp=0):
     a timing/realization Fmax regression the functional sim can't see.  So 16
     is OPT-IN only, pending an STA/Fmax check; the default stays 8. *)
  let carry4_min =
    match Sys.getenv "CARRY4_MIN" with
    | Some s -> (try Int.of_string s with _ -> 8)
    | None -> 8 in
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
  (* Block-level CSE: a CARRY4 whose carry source (the chain's carry_in lit,
     or the block below it) and four S / DI lits are all identical to an
     already-emitted block computes the same O / CO, so the later chain
     reuses it instead of emitting a duplicate.  Chains with a common prefix
     then share it: the unsigned and signed compares of the same operands
     differ only in the sign lane, and `a < b` is the same chain as `a - b`
     (S = a XNOR b, DI = a, carry-in 1).  CARRY4_CSE=0 disables. *)
  let carry4_cse =
    match Sys.getenv "CARRY4_CSE" with Some "0" -> false | _ -> true in
  let carry4_blocks : (string * int list * int list, string * lit list * lit list) Hashtbl.t =
    Hashtbl.Poly.create () in
  let emit_carry4_chain ~carry_in s_lits di_lits ~w =
    let n_blocks = (w + 3) / 4 in
    let out_lits = Array.create ~len:w const_false in
    let prev_base = ref None in
    let top_co_lit = ref const_false in
    for k = 0 to n_blocks - 1 do
      (* Pad the unused upper lanes of the top CARRY4 (bit_idx >= w) with the
         last real S/DI lit rather than a constant 0.  Those lanes' sum and
         carry-out feed nothing (the chain's used carry-out is taken at lane
         (w-1) mod 4), so the value is a don't-care — but tying them to GND
         makes them $PACKER_GND_NET sinks on the CARRY4.S/DI pins, which are
         driven by the slice's own LUTs and so are NOT reachable by general
         constant routing (they'd be left unrouted).  Reusing a real local
         signal keeps every lane routable. *)
      let lane arr pad j =
        let bit_idx = (4 * k) + j in
        if bit_idx < w then arr.(bit_idx) else pad in
      let s4 = List.init 4 ~f:(lane s_lits s_lits.(w - 1)) in
      let di4 = List.init 4 ~f:(lane di_lits di_lits.(w - 1)) in
      let cin_key =
        match !prev_base with
        | Some pb -> "ci:" ^ pb
        | None -> "cyinit:" ^ Int.to_string carry_in in
      let key = (cin_key, s4, di4) in
      let base, o_lits, co_lits =
        match (if carry4_cse then Hashtbl.find carry4_blocks key else None) with
        | Some hit -> hit
        | None ->
          let block_id = !carry4_counter in
          Int.incr carry4_counter;
          let base = Printf.sprintf "c4_%d" block_id in
          let mk_bit_names port =
            List.init 4 ~f:(fun j -> Printf.sprintf "%s_%s_%d" base port j)
          in
          let s_names = mk_bit_names "S" in
          let di_names = mk_bit_names "DI" in
          List.iter2_exn s_names s4 ~f:(fun nm lit -> inst_outs := (nm, lit) :: !inst_outs);
          List.iter2_exn di_names di4 ~f:(fun nm lit -> inst_outs := (nm, lit) :: !inst_outs);
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
          let in_ports =
            let common = [ "DI", di_names; "S", s_names ] in
            match !prev_base with
            | Some prev -> ("CI", [ prev ^ "_CO_3" ]) :: common   (* upper block: CI cascade, no CYINIT *)
            | None ->                                             (* head block: CYINIT only, no CI *)
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
          let entry = (base, o_lits, co_lits) in
          if carry4_cse then Hashtbl.set carry4_blocks ~key ~data:entry;
          entry in
      List.iteri o_lits ~f:(fun j l ->
        let bit_idx = (4 * k) + j in
        if bit_idx < w then out_lits.(bit_idx) <- l);
      (* The carry-out at bit w-1 lives in block k = (w-1)/4 at position
         (w-1) mod 4.  Record it as we walk the chain. *)
      if k = n_blocks - 1 then begin
        let last_bit = (w - 1) - (4 * k) in
        top_co_lit := List.nth_exn co_lits last_bit
      end;
      prev_base := Some base
    done;
    out_lits, !top_co_lit
  in
  (* a + bb + carry_in over w bits, with the CARRY4 chain trimmed to the
     span whose carry is actually unknown.  Below that span the carry is a
     known constant c and each bit has a constant operand that keeps it so
     (c = 0 and a or b = 0, or c = 1 and a or b = 1): the sum bit is a plain
     XOR and no carry cell is needed.  Above it both operands are constant,
     so the sum bits are functions of the span's carry-out alone (folded by
     the AIG constructors, no gates).  Zero-padded partial products and
     shifted operands (yosys gets the same from opt_expr + wreduce) were
     costing a full-width chain per adder.  CARRY4_TRIM=0 restores the
     full-width chain. *)
  let carry4_trim =
    match Sys.getenv "CARRY4_TRIM" with Some "0" -> false | _ -> true in
  (* residual span below which the trimmed adder ripples through LUTs *)
  let carry4_trim_min =
    match Sys.getenv "CARRY4_TRIM_MIN" with
    | Some s -> (try Int.of_string s with _ -> carry4_min)
    | None -> carry4_min in
  let v_add_carry4 ~carry_in a bb ~w =
    let is_c l = l = const_false || l = const_true in
    let lo = ref 0 in
    if carry4_trim && is_c carry_in then
      while !lo < w && (vbit a !lo = carry_in || vbit bb !lo = carry_in) do
        Int.incr lo
      done;
    let hi = ref w in
    if carry4_trim then
      while !hi > !lo && is_c (vbit a (!hi - 1)) && is_c (vbit bb (!hi - 1)) do
        Int.decr hi
      done;
    let lo = !lo and hi = !hi in
    if (lo > 0 || hi < w) && Option.is_some (Sys.getenv "CARRY4_TRIM_DEBUG") then
      Stdlib.Printf.eprintf "[carry4_trim] w=%d span=[%d,%d) cin=%s aconst=%b bconst=%b\n%!" w lo hi
        (if carry_in = const_true then "1" else if carry_in = const_false then "0" else "x")
        (List.for_all (List.range lo hi) ~f:(fun i -> is_c (vbit a i)))
        (List.for_all (List.range lo hi) ~f:(fun i -> is_c (vbit bb i)));
    let out = Array.create ~len:w const_false in
    for i = 0 to lo - 1 do
      out.(i) <- aig_xor b (aig_xor b (vbit a i) (vbit bb i)) carry_in
    done;
    let mid = hi - lo in
    let co =
      if mid = 0 then carry_in
      else if carry4_trim && mid < carry4_trim_min
              && (List.for_all (List.range lo hi) ~f:(fun i -> is_c (vbit a i))
                  || List.for_all (List.range lo hi) ~f:(fun i -> is_c (vbit bb i)))
      then begin
        (* short residual span against a constant (an increment / add-const
           left after trimming): ripple in LUTs, keeping the carry-out -- a
           LUT6 packs several such bits, where the chain costs a LUT per bit
           plus the CARRY4s (uart_receiver 64 -> 47 LUTs, 11 -> 0 CARRY4).
           Two variable operands keep the chain: rippling those measured
           +26 LUTs on picorv32. *)
        let cy = ref carry_in in
        for i = lo to hi - 1 do
          let ai = vbit a i and bi = vbit bb i in
          let axb = aig_xor b ai bi in
          out.(i) <- aig_xor b axb !cy;
          cy := aig_or b (aig_and b ai bi) (aig_and b !cy axb)
        done;
        !cy
      end else begin
        let s_lits = Array.init mid ~f:(fun j -> aig_xor b (vbit a (lo + j)) (vbit bb (lo + j))) in
        let di_lits = Array.init mid ~f:(fun j -> vbit a (lo + j)) in
        let sum, co = emit_carry4_chain ~carry_in s_lits di_lits ~w:mid in
        Array.iteri sum ~f:(fun j l -> out.(lo + j) <- l);
        co
      end in
    let cy = ref co in
    for i = hi to w - 1 do
      let ai = vbit a i and bi = vbit bb i in
      let axb = aig_xor b ai bi in
      out.(i) <- aig_xor b axb !cy;
      cy := aig_or b (aig_and b ai bi) (aig_and b !cy axb)
    done;
    out
  in
  (* a < b (unsigned), via CARRY4 of a + ~b + 1.  Top CO = 0 iff a < b
     (the subtract borrowed).  Returns a 1-bit lit. *)
  let v_lt_carry4 a bb ~w =
    (* S = a XOR ~b, built exactly as Signal_sub builds it, so the carry-block
       CSE recognises `a < b` and `a - b` as the same chain *)
    let s_lits = Array.init w ~f:(fun i -> aig_xor b (vbit a i) (lit_not (vbit bb i))) in
    let di_lits = Array.init w ~f:(fun i -> vbit a i) in
    let _sum, co = emit_carry4_chain ~carry_in:const_true s_lits di_lits ~w in
    [| lit_not co |]
  in
  (* carry-in fusion: Some (x, y, c) when one operand of a w-bit add is
     itself a w-bit add x + y and the other is a single bit c (zero-extended).
     CARRY4_FUSE=0 disables. *)
  let carry4_fuse =
    match Sys.getenv "CARRY4_FUSE" with Some "0" -> false | _ -> true in
  let rec strip_wire (s : T.t) =
    match s with
    | T.Wire { driver; _ } when not (T.is_empty !driver) -> strip_wire !driver
    | s -> s in
  let walk_ref = ref (fun (_ : T.t) -> ([||] : lit array)) in
  let fuse_cin ~w p q =
    let try_one inner bit =
      match strip_wire inner with
      | T.Op2 { op = Signal_add; arg_a = x; arg_b = y; _ } when S.width inner = w ->
          let cb = !walk_ref bit in
          if Array.length cb >= 1
             && Array.for_alli cb ~f:(fun i l -> i = 0 || l = const_false)
             && not (cb.(0) = const_false)
          then Some (x, y, cb.(0)) else None
      | _ -> None in
    match try_one p q with
    | Some r -> Some r
    | None -> try_one q p in
  (* Mux trees as AND-OR (MUX_ANDOR=0 disables; MUX_ANDOR_MIN leaves,
     default 6).  A case/if chain arrives as nested 2:1 muxes -- one mux per
     arm per bit, packed about two to a LUT.  Its root-to-leaf path
     conditions are mutually exclusive and exhaustive, which the cover cannot
     see; using that, out = OR_j (sel_j & V_j) with sel_j the OR of the paths
     reaching distinct leaf value V_j.  The path literals are built once and
     shared by every bit, a constant leaf bit reduces its term to sel_j or
     nothing, and the value reached by most paths (usually the register's
     hold) is the default: selected exactly when no other sel_j is, so it
     needs no OR of its own.  Exact for any inputs, so the miter checks it
     combinationally. *)
  let mux_andor = match Sys.getenv "MUX_ANDOR" with Some "0" -> false | _ -> true in
  let mux_andor_min =
    match Sys.getenv "MUX_ANDOR_MIN" with
    | Some s -> (try Int.of_string s with _ -> 6)
    | None -> 6 in
  let mux_andor_debug = Option.is_some (Sys.getenv "MUX_ANDOR_DEBUG") in
  let is_mux2 (s : T.t) =
    match s with
    | T.Mux { select; cases = [ _; _ ]; _ } -> S.width select = 1
    | _ -> false in
  (* leaves of the 2:1-mux tree under s, counting stops at [budget] *)
  let rec mux_leaves (s : T.t) budget =
    if budget <= 0 then 0
    else match s with
      | T.Mux { cases = [ c0; c1 ]; _ } when is_mux2 s ->
        let a = mux_leaves c0 budget in
        a + mux_leaves c1 (budget - a)
      | _ -> 1 in
  let rec bal_or = function
    | [] -> const_false
    | [ x ] -> x
    | l ->
      let rec pairs = function
        | x :: y :: r -> aig_or b x y :: pairs r
        | r -> r in
      bal_or (pairs l) in
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
          | Op2 { op = Signal_add; arg_a; arg_b; _ }
            when (walk_ref := walk; carry4_fuse) && w >= carry4_min
                 && Option.is_some (fuse_cin ~w arg_a arg_b) ->
            (* (x + y) + c with c a single bit: one chain, c on CYINIT --
               the add/sub fusion `c ? x - y : x + y` -> `(x + (c ? ~y : y)) + c`
               (Behavioral_share_accum.fuse_addsub) lands here *)
            let x, y, c = Option.value_exn (fuse_cin ~w arg_a arg_b) in
            v_add_carry4 ~carry_in:c (walk x) (walk y) ~w
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
               (* two's-complement product mod 2^w = unsigned product of the
                  operands sign-extended to w bits (a no-op when the frontend
                  already widened them to the context width) *)
               let sext v =
                 let n = Array.length v in
                 if n = 0 || n >= w then v
                 else Array.init w ~f:(fun i -> if i < n then v.(i) else v.(n - 1)) in
               fit (v_mul b (sext a) (sext bb)) w
             | _ -> failwith "bir_to_aig: unhandled Op2")
          | Not { arg; _ } -> Array.map (walk arg) ~f:lit_not
          | Mux _ when mux_andor && is_mux2 sig_
                       && mux_leaves sig_ mux_andor_min >= mux_andor_min ->
            let groups = Hashtbl.Poly.create () and order = ref [] in
            let rec go (s : T.t) path =
              match s with
              | Mux { select; cases = [ c0; c1 ]; _ } when is_mux2 s ->
                let c = (walk select).(0) in
                if c = const_true then go c1 path
                else if c = const_false then go c0 path
                else begin
                  go c1 (aig_and b path c);
                  go c0 (aig_and b path (lit_not c))
                end
              | leaf ->
                if not (path = const_false) then begin
                  let v = fit (walk leaf) w in
                  match Hashtbl.find groups v with
                  | Some l -> Hashtbl.set groups ~key:v ~data:(path :: l)
                  | None ->
                    order := v :: !order;
                    Hashtbl.set groups ~key:v ~data:[ path ]
                end in
            go sig_ const_true;
            let vals = List.rev !order in
            let npaths v = List.length (Hashtbl.find_exn groups v) in
            (match vals with
             | [] -> Array.create ~len:w const_false
             | [ v ] -> v
             | v0 :: _ ->
               let dflt =
                 List.fold vals ~init:v0 ~f:(fun d v -> if npaths v > npaths d then v else d) in
               let sels =
                 List.filter_map vals ~f:(fun v ->
                   if phys_equal v dflt then None
                   else Some (v, bal_or (Hashtbl.find_exn groups v))) in
               let none = lit_not (bal_or (List.map sels ~f:snd)) in
               if mux_andor_debug then
                 Stdlib.Printf.eprintf "[mux_andor] %s: w=%d leaves=%d values=%d default=%d paths\n%!"
                   (signal_name sig_) w
                   (List.fold vals ~init:0 ~f:(fun a v -> a + npaths v))
                   (List.length vals) (npaths dflt);
               let term s l =
                 if l = const_false then None
                 else if l = const_true then Some s
                 else Some (aig_and b s l) in
               Array.init w ~f:(fun i ->
                 bal_or
                   (List.filter_map ((dflt, none) :: sels) ~f:(fun (v, s) -> term s (vbit v i)))))
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
               Stdlib.Printf.eprintf "CE_LIFT_STAT %s async=%d w=%d %s\n" cls has_async w base
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
            (* The hold leaves of the whole D mux tree, not only a top-level
               [mux en dnext Q]: a threaded next-state tree keeps the
               register (`: accu`) in every arm that does not write it, and
               those leaves are what the CE pin is for.  CE = OR of the path
               conditions reaching a non-hold leaf, and D is the tree with
               each hold leaf replaced by the non-hold value reached by most
               paths (any value does: CE is low there), which the AND-OR
               lowering then takes as its default.  yosys gets the same
               through opt_dff; without it the VM's accu had CE tied high and
               2400 LUTs in its D cone.  CE_LIFT_TREE=0 keeps the one-level
               rule. *)
            let tree_lift =
              match Stdlib.Sys.getenv_opt "CE_LIFT_TREE" with Some "0" -> false | _ -> true in
            (* the OR of the write paths is paid once per register, the saved
               hold term once per bit: a 1-bit register keeps its hold mux
               (CE_LIFT_MIN_W, default 2 -- measured best on uart_rx/tx,
               picorv32 and the VM; 1 and 4 were worse on all of them) *)
            let tree_lift =
              tree_lift
              && w >= (match Stdlib.Sys.getenv_opt "CE_LIFT_MIN_W" with
                       | Some s -> (try Int.of_string s with _ -> 2) | None -> 2) in
            let is_self s = uid_int (resolve s) = uid_int sig_ in
            let tree_result =
              if not tree_lift then None
              else begin
                let budget = ref 65536 in
                let rec leaves s path acc =
                  match resolve s with
                  | T.Mux { select; cases = [ c0; c1 ]; _ }
                    when S.width select = 1 && !budget > 0 ->
                    Int.decr budget;
                    let sel = select in
                    let with_ c = match path with None -> Some c | Some q -> S.(Some (q &: c)) in
                    leaves c0 (with_ (S.( ~: ) sel)) (leaves c1 (with_ sel) acc)
                  | _ -> (path, s) :: acc in
                let ls = leaves d1 None [] in
                let selves = List.filter ls ~f:(fun (_, l) -> is_self l) in
                let others = List.filter ls ~f:(fun (_, l) -> not (is_self l)) in
                if !budget <= 0 || List.is_empty selves || List.is_empty others
                   || List.exists others ~f:(fun (p, _) -> Option.is_none p)
                then None
                else begin
                  (* the non-hold value reached by most paths *)
                  let counts = Hashtbl.create (module Int) in
                  List.iter others ~f:(fun (_, l) ->
                    Hashtbl.update counts (uid_int (resolve l)) ~f:(function None -> 1 | Some n -> n + 1));
                  let best =
                    List.fold others ~init:(snd (List.hd_exn others)) ~f:(fun acc (_, l) ->
                      let c x = Option.value (Hashtbl.find counts (uid_int (resolve x))) ~default:0 in
                      if c l > c acc then l else acc) in
                  let rec rebuild s =
                    match resolve s with
                    | T.Mux { select; cases = [ c0; c1 ]; _ } when S.width select = 1 ->
                      S.mux2 select (rebuild c1) (rebuild c0)
                    | _ -> if is_self s then best else s in
                  let rec bal = function
                    | [] -> S.gnd
                    | [ x ] -> x
                    | l ->
                      let rec pairs = function
                        | x :: y :: r -> S.(x |: y) :: pairs r
                        | r -> r in
                      bal (pairs l) in
                  let ce = bal (List.map others ~f:(fun (p, _) -> Option.value_exn p)) in
                  Some (ce, rebuild d1)
                end
              end in
            let rb_enable, eff_d =
              match tree_result with
              | Some (ce, d') ->
                let en_name = Printf.sprintf "%s__ce" base in
                Queue.enqueue reg_queue (ce, [ en_name ]);
                (Some en_name, d')
              | None ->
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
