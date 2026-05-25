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

(* ripple-carry add of [a]+[b]+carry_in, low [n] bits (carry-out dropped). *)
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
  ; rb_width : int
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
             | Signal_lt -> v_lt b a bb ~n
             | Signal_add -> v_add b a bb ~carry_in:const_false ~n:w
             | Signal_sub ->
               v_add b a (Array.map bb ~f:lit_not) ~carry_in:const_true ~n:w
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
            let base = Printf.sprintf "r%d" key in
            let q_names = List.init w ~f:(fun i -> Printf.sprintf "%s_q%d" base i) in
            let d_names = List.init w ~f:(fun i -> Printf.sprintf "%s_d%d" base i) in
            let q_lits = Array.of_list (List.map q_names ~f:(aig_input b)) in
            let clk = signal_name register.reg_clock in
            let rst =
              if is_empty register.reg_reset then None
              else Some (signal_name register.reg_reset)
            in
            regs :=
              { rb_q_names = q_names
              ; rb_d_names = d_names
              ; rb_clock = clk
              ; rb_reset = rst
              ; rb_width = w
              }
              :: !regs;
            Queue.enqueue reg_queue (d, d_names);
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
                List.iteri names ~f:(fun bit nm -> out_lits.(off + bit) <- aig_input b nm);
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
