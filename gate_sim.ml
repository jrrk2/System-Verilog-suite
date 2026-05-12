(* Bit-parallel combinational gate-level simulator.

   Packs 64 test patterns into one [int] per net, evaluates each cell
   once across all patterns simultaneously.  Operates on a scan-flattened
   view of [Lib_map.netlist]: every FF's Q becomes an extra input and
   every FF's D an extra output, so the remaining graph is purely
   combinational.

   Cell semantics are hardcoded for the Liberty cells our pipeline
   emits (AND/OR/XOR/INV/BUF/NAND/NOR up to 4-way, MUX2, LOGIC0/1,
   tie cells).  Unknown cells abort with a clear message — extend the
   match table to cover new ones.

   Used by [Fault_sim] for ATPG random-pattern coverage; usable
   standalone for golden-vector regression testing.                  *)

open Lib_map

(* ── Bit-parallel pattern words ─────────────────────────────────── *)

(* Each net carries a 64-bit word holding the value of that net across
   64 test patterns.  [vals.(net_idx)] is the live value. *)

(* ── Cell semantics — boolean function over 64-bit pattern words ─── *)

let starts_with pfx s =
  let lp = String.length pfx and ls = String.length s in
  ls >= lp && String.sub s 0 lp = pfx

(* Drop the _X<drive> suffix so AND2_X1 / AND2_X2 / AND2_X4 share a
   semantics-table entry.  Same family, same boolean function. *)
let strip_drive cell =
  try
    let i = String.rindex cell '_' in
    if i + 1 < String.length cell && cell.[i+1] = 'X' then
      String.sub cell 0 i
    else cell
  with Not_found -> cell

(* Evaluate one cell given its input *pattern words*.  Returns the
   output pattern word.  All ops are bitwise on the 64-bit int so we
   process 64 patterns in one shot. *)
let eval_cell ?(mask = -1) cell inputs =
  let band a b = a land b in
  let bor  a b = a lor b in
  let bxor a b = a lxor b in
  let bnot a = (lnot a) land mask in
  match strip_drive cell, inputs with
  | "AND2",  [a; b]       -> band a b
  | "AND3",  [a; b; c]    -> band (band a b) c
  | "AND4",  [a; b; c; d] -> band (band a b) (band c d)
  | "OR2",   [a; b]       -> bor  a b
  | "OR3",   [a; b; c]    -> bor  (bor  a b) c
  | "OR4",   [a; b; c; d] -> bor  (bor  a b) (bor  c d)
  | "NAND2", [a; b]       -> bnot (band a b)
  | "NAND3", [a; b; c]    -> bnot (band (band a b) c)
  | "NAND4", [a; b; c; d] -> bnot (band (band a b) (band c d))
  | "NOR2",  [a; b]       -> bnot (bor  a b)
  | "NOR3",  [a; b; c]    -> bnot (bor  (bor  a b) c)
  | "NOR4",  [a; b; c; d] -> bnot (bor  (bor  a b) (bor  c d))
  | "XOR2",  [a; b]       -> bxor a b
  | "XNOR2", [a; b]       -> bnot (bxor a b)
  | "INV",   [a]          -> bnot a
  | "BUF",   [a]          -> a
  | "MUX2",  [a; b; s]    -> bor (band a (bnot s)) (band b s)
  | "AOI21", [a; b; c]    -> bnot (bor  (band a b) c)
  | "AOI22", [a; b; c; d] -> bnot (bor  (band a b) (band c d))
  | "OAI21", [a; b; c]    -> bnot (band (bor  a b) c)
  | "OAI22", [a; b; c; d] -> bnot (band (bor  a b) (bor  c d))
  | "LOGIC0", []          -> 0
  | "LOGIC1", []          -> mask
  | c, ins ->
      failwith
        (Printf.sprintf
           "gate_sim: unhandled cell %s with %d inputs (input cell list \
            wants extension)"
           c (List.length ins))

(* ── Topological order ──────────────────────────────────────────── *)

(* Build the eval order: every cell must come after the cells driving
   its inputs.  FFs are treated as primary-input drivers (their Q
   pattern word is supplied externally).  Cycles indicate a true comb
   loop and are reported but not fatal — we evaluate them by holding
   the previous-iter value. *)

let is_ff cell_name =
  starts_with "DFF" cell_name
  || starts_with "DFFR" cell_name
  || starts_with "DFFS" cell_name
  || starts_with "DFFRS" cell_name
  || starts_with "SDFF" cell_name
  || starts_with "SDFFR" cell_name
  || starts_with "SDFFS" cell_name
  || starts_with "SDFFRS" cell_name

(* The FF's combinational input — just the D pin.  SI is the previous
   FF's Q (covered by the chain), SE/CK are global controls, RN/SN are
   reset/set strobes driven by the test bench, not internal cone tips. *)
let ff_observable_in_pins = ["D"]

(* ── Simulator state ────────────────────────────────────────────── *)

type net_id = int

type compiled = {
  c_n_nets       : int;
  c_net_of_name  : (string, net_id) Hashtbl.t;
  c_name_of_net  : string array;
  (* Cell evaluation order (topological).  Each entry holds the cell
     name, the list of input net_ids, and the output net_id. *)
  c_evals        : (string * net_id list * net_id) array;
  (* All FFs in the netlist — their Q net (driven by the scan-flattened
     "external" input set) and D net (the captured "external" output). *)
  c_ff_q_nets    : net_id list;
  c_ff_d_nets    : net_id list;
  (* PIs / POs of the parent module — used to know which nets the
     external test bench drives / observes. *)
  c_pi_nets      : net_id list;
  c_po_nets      : net_id list;
  (* Combinational driver lookup — for fault simulation we need the
     reverse map from a faulted output net back to the cell. *)
  c_driver_of    : (net_id, int) Hashtbl.t;
}

let compile (nl : netlist) : compiled =
  (* Resolve simple [assign lhs = rhs] bus-level aliases — both sides
     are single bare identifiers — into a name-rewriting table.  When
     a cell pin reads "_n_1_[0]" but the netlist contains an
     `assign _n_1_ = _n_2_`, route the lookup to "_n_2_[0]" instead.
     Handles only the pure-alias case; concat-rhs assigns
     (`assign x = {a, b, …}`) and arithmetic-rhs assigns are left
     unresolved (their lhs reads as 0 in the simulator — a known
     Stage-1 limitation).                                            *)
  let is_simple_id s =
    let n = String.length s in
    n > 0
    && (let c = s.[0] in
        c = '_' || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))
    && (let ok = ref true in
        for i = 0 to n - 1 do
          let c = s.[i] in
          if not (c = '_'
                  || ('a' <= c && c <= 'z')
                  || ('A' <= c && c <= 'Z')
                  || ('0' <= c && c <= '9')) then ok := false
        done; !ok) in
  let alias_of : (string, string) Hashtbl.t = Hashtbl.create 64 in
  (* Width table from nl.wires + nl.inputs + nl.outputs — every named
     bus/scalar carries a declared width.  Used by the concat-assign
     processor to split lhs into bit ranges that align with each
     concat piece's width.                                            *)
  let width_of : (string, int) Hashtbl.t = Hashtbl.create 64 in
  let add_w (n, w) = Hashtbl.replace width_of n w in
  List.iter add_w nl.wires;
  List.iter add_w nl.inputs;
  List.iter add_w nl.outputs;
  let lookup_w n =
    try Hashtbl.find width_of n with Not_found -> 1 in
  (* Trim leading/trailing whitespace.  String.trim exists since 4.00 *)
  let split_concat s =
    let s = String.trim s in
    let n = String.length s in
    if n < 2 || s.[0] <> '{' || s.[n-1] <> '}' then None
    else
      let inner = String.sub s 1 (n - 2) in
      let parts =
        String.split_on_char ',' inner
        |> List.map String.trim
        |> List.filter (fun p -> p <> "") in
      Some parts in

  List.iter (fun (l, r) ->
    if is_simple_id l && is_simple_id r then
      (* Bus-level alias: every bit-level lookup will route through. *)
      Hashtbl.replace alias_of l r
    else
      match split_concat r with
      | None -> ()  (* arith / slice / unrecognised — skip *)
      | Some pieces when not (is_simple_id l) -> ignore pieces  (* skip slice-LHS for now *)
      | Some pieces ->
          (* lhs is a simple bus name; pieces are in MSB-to-LSB order.
             Assign each piece-bit to the corresponding lhs[bit]. *)
          let lhs_w = lookup_w l in
          let bit_pos = ref (lhs_w - 1) in
          List.iter (fun piece ->
            if !bit_pos < 0 then ()
            else begin
              let pw = lookup_w piece in
              (* piece occupies pw bits, MSB at the current bit_pos. *)
              for k = pw - 1 downto 0 do
                if !bit_pos < 0 then ()
                else begin
                  let lhs_bit =
                    if lhs_w = 1 then l
                    else Printf.sprintf "%s[%d]" l !bit_pos in
                  let piece_bit =
                    if pw = 1 then piece
                    else Printf.sprintf "%s[%d]" piece k in
                  Hashtbl.replace alias_of lhs_bit piece_bit;
                  decr bit_pos
                end
              done
            end
          ) pieces
  ) nl.assigns;
  (* Find canonical name with path compression. *)
  let rec canon n =
    match Hashtbl.find_opt alias_of n with
    | Some n' when n' <> n -> canon n'
    | _ -> n in
  (* For a name like "_n_1_[3]" or "_n_1_": first try a full-name
     bit-level alias (concat-rhs assigns added these — `_n_51_[3] →
     _T__…_sum__57_`).  If none, fall back to bus-level canon on the
     part before `[` so a bus-rename like `_n_1_ = _n_2_` reaches the
     bit too.  Iterate to fixed point so chains of aliases (`_n_1_[3]
     → _n_2_[3] → _n_3_[3]`) collapse.                                *)
  let rec rewrite name =
    match Hashtbl.find_opt alias_of name with
    | Some name' when name' <> name -> rewrite name'
    | _ ->
        if String.contains name '[' then begin
          let i = String.index name '[' in
          let bus = String.sub name 0 i in
          let rest = String.sub name i (String.length name - i) in
          let bus' = canon bus in
          if bus' = bus then name
          else rewrite (bus' ^ rest)
        end else canon name in

  let name_to_id : (string, net_id) Hashtbl.t = Hashtbl.create 1024 in
  let id_to_name = ref [] in
  let alloc raw_name =
    let name = rewrite raw_name in
    match Hashtbl.find_opt name_to_id name with
    | Some i -> i
    | None ->
        let i = Hashtbl.length name_to_id in
        Hashtbl.add name_to_id name i;
        id_to_name := name :: !id_to_name;
        i in

  (* Pre-allocate net ids for everything we expect to see. *)
  List.iter (fun (n, _) -> ignore (alloc n)) nl.inputs;
  List.iter (fun (n, _) -> ignore (alloc n)) nl.outputs;
  List.iter (fun (n, _) -> ignore (alloc n)) nl.wires;

  let evals = ref [] in
  let ff_qs = ref [] in
  let ff_ds = ref [] in
  let driver_of : (net_id, int) Hashtbl.t = Hashtbl.create 1024 in

  List.iter (fun (i : instance) ->
    if is_ff i.cell.cell_name then begin
      List.iter (fun c ->
        let n = alloc c.net in
        if c.pin = "Q" then ff_qs := n :: !ff_qs
        else if List.mem c.pin ff_observable_in_pins then ff_ds := n :: !ff_ds
      ) i.conns
    end else begin
      let in_nets =
        List.filter_map (fun c ->
          if c.pin = i.cell.out_pin then None
          else Some (alloc c.net)) i.conns in
      let out_net =
        match List.find_opt (fun c -> c.pin = i.cell.out_pin) i.conns with
        | Some c -> alloc c.net
        | None -> -1 in
      let eval_idx = List.length !evals in
      if out_net >= 0 then Hashtbl.replace driver_of out_net eval_idx;
      evals := (i.cell.cell_name, in_nets, out_net) :: !evals
    end
  ) nl.insts;

  (* Topological sort: a cell's output net must be valid before any
     cell that reads it.  We build a producer→consumers edge list and
     run Kahn's algorithm.  Cells with input nets that have no
     producer (PI / FF.Q / undriven) are eligible immediately.       *)
  let raw_evals = List.rev !evals in
  let n_evals = List.length raw_evals in
  let eval_arr = Array.of_list raw_evals in
  let producer_of : (net_id, int) Hashtbl.t = Hashtbl.create 1024 in
  Array.iteri (fun i (_, _, out) ->
    if out >= 0 then Hashtbl.replace producer_of out i) eval_arr;
  let in_deg = Array.make n_evals 0 in
  let consumers = Array.make n_evals [] in
  Array.iteri (fun i (_, ins, _) ->
    List.iter (fun in_net ->
      match Hashtbl.find_opt producer_of in_net with
      | Some j when j <> i ->
          consumers.(j) <- i :: consumers.(j);
          in_deg.(i) <- in_deg.(i) + 1
      | _ -> ()
    ) ins
  ) eval_arr;
  let ready = Queue.create () in
  Array.iteri (fun i d -> if d = 0 then Queue.push i ready) in_deg;
  let sorted = ref [] in
  while not (Queue.is_empty ready) do
    let i = Queue.pop ready in
    sorted := eval_arr.(i) :: !sorted;
    List.iter (fun j ->
      in_deg.(j) <- in_deg.(j) - 1;
      if in_deg.(j) = 0 then Queue.push j ready
    ) consumers.(i)
  done;
  if List.length !sorted < n_evals then
    Printf.eprintf
      "[gate_sim] WARN: combinational loop detected (%d / %d cells \
       in topo order — the rest are part of a cycle and evaluate \
       once at their current values)\n%!"
      (List.length !sorted) n_evals;
  (* Loop-only cells fall through here and are appended in their
     original order — they evaluate once per call to [run], so a
     true comb loop oscillates but doesn't infinite-loop the sim. *)
  let in_topo = Array.make n_evals false in
  List.iter (fun e ->
    let _, _, out = e in
    match Hashtbl.find_opt producer_of out with
    | Some i -> in_topo.(i) <- true
    | None -> ()) !sorted;
  Array.iteri (fun i e ->
    if not in_topo.(i) then sorted := e :: !sorted) eval_arr;
  let evals_arr = Array.of_list (List.rev !sorted) in
  let names_arr = Array.of_list (List.rev !id_to_name) in
  (* Rebuild [driver_of] keyed on indices INTO [evals_arr] — the
     pre-sort indices stored during the initial walk are now stale
     because the topo sort permuted [evals].  Without this, every
     consumer of [c_driver_of] (Atpg_directed, Atpg_podem, the
     PO/observable filter below) was looking up the WRONG cell. *)
  let driver_of_sorted : (net_id, int) Hashtbl.t = Hashtbl.create 1024 in
  Array.iteri (fun i (_, _, out) ->
    if out >= 0 then Hashtbl.replace driver_of_sorted out i
  ) evals_arr;
  (* PI nets = declared module inputs + any other undriven net that
     isn't a FF.Q (those get seeded separately).  Bit-blasted bus
     inputs like `op[0]` exist as undriven nets but aren't always
     present in [nl.inputs]; without them, [run]'s input_pat seeding
     skips them and they read as 0, which silently invalidates ATPG
     PODEM patterns that backtrace to bit-level PIs.                  *)
  let ff_q_set = List.fold_left (fun s n -> n :: s) [] !ff_qs in
  let declared_pis = List.map (fun (n, _) -> alloc n) nl.inputs in
  let pi_set = ref declared_pis in
  let n_nets = Hashtbl.length name_to_id in
  for nid = 0 to n_nets - 1 do
    if not (Hashtbl.mem driver_of_sorted nid)
       && not (List.mem nid ff_q_set)
       && not (List.mem nid declared_pis)
    then pi_set := nid :: !pi_set
  done;
  { c_n_nets      = n_nets;
    c_net_of_name = name_to_id;
    c_name_of_net = names_arr;
    c_evals       = evals_arr;
    c_ff_q_nets   = !ff_qs;
    c_ff_d_nets   = !ff_ds;
    c_pi_nets     = !pi_set;
    c_po_nets     =
      List.filter_map (fun (n, _) ->
        let id = alloc n in
        if Hashtbl.mem driver_of_sorted id then Some id
        else if List.mem id !ff_qs then Some id
        else None) nl.outputs;
    c_driver_of   = driver_of_sorted }

(* ── Pattern-parallel simulation ─────────────────────────────────── *)

(* [run] returns a freshly-allocated values-per-net array.  Caller
   supplies a function that returns the pattern word for each primary
   input + FF.Q ("scan-flattened input").  Useful both for golden
   simulation (random patterns) and for fault-injected reruns. *)
let run ?(fault = fun _ x -> x) (c : compiled) ~input_pat : int array =
  let v = Array.make c.c_n_nets 0 in
  List.iter (fun nid -> v.(nid) <- input_pat (c.c_name_of_net.(nid))) c.c_pi_nets;
  List.iter (fun nid -> v.(nid) <- input_pat (c.c_name_of_net.(nid))) c.c_ff_q_nets;
  Array.iter (fun (cell, in_nets, out_net) ->
    if out_net >= 0 then begin
      let ins = List.map (fun n -> v.(n)) in_nets in
      let raw = eval_cell cell ins in
      v.(out_net) <- fault out_net raw
    end
  ) c.c_evals;
  v
