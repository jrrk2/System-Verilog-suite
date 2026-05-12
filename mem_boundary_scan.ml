(* Memory boundary scan — wrap black-box memory macro ports with single-bit
   DFFs so ATPG can observe driver-side faults and control consumer-side
   logic, even though the macro itself is unmodelled at gate-sim level.

   For each child instance whose module name is registered as a macro,
   for every non-clock port:

     INPUT  port [W-1:0] connected to parent net  N:
       allocate a fresh private net  M[W-1:0]
       for i in 0..W-1, emit:  DFF_X1 ( D=N[i], CK=clk_net, Q=M[i] )
       rewrite the child's port conn from N to M.

     OUTPUT port [W-1:0] connected to parent net  N:
       allocate a fresh private net  M[W-1:0]  (macro now drives M)
       for i in 0..W-1, emit:  DFF_X1 ( D=M[i], CK=clk_net, Q=N[i] )
       rewrite the child's port conn from N to M.

   This makes the original driver/consumer side of every port a DFF.D
   or DFF.Q net, which Gate_sim treats as observable / controllable.
   Scan_insert (which runs after this pass) sees ordinary DFFs and
   stitches them into the chain alongside the design's own FFs.

   Functional latency note: this transformation adds one cycle of
   delay through each macro port.  For ATPG analysis that's fine —
   the patterns drive scan-mode, not functional clocking.  In silicon
   you'd use scan-bypass MUXes to keep the functional path transparent;
   we'll add that variant once the basic shape is proven.

   Env-gated: SV_DECOMP_MEM_BSR=1 enables.                              *)

open Lib_map

let enabled () = Sys.getenv_opt "SV_DECOMP_MEM_BSR" = Some "1"

(* When true, also wraps non-macro hierarchical child instances (CPU,
   peripherals, …).  Drives chip-level top coverage up because every
   child is a black box at its parent's gate_sim view; without the
   boundary FFs, faults in cones that *only* observe via a child get
   stuck at "undetected" even when the child is fully tested at its
   own scan-flatten level.                                            *)
let hier_enabled () = Sys.getenv_opt "SV_DECOMP_HIER_BSR" = Some "1"

(* Heuristic clock-pin detection.  Macros from mem_macro_resolve use
   clk0, clk1, ... ; OpenRAM uses clk0 ; Sky130 fakeram uses clk.    *)
let is_clock_port p =
  let p = String.lowercase_ascii p in
  p = "clk" || p = "ck"
  || (String.length p >= 3 && String.sub p 0 3 = "clk")
  || (String.length p >= 2 && String.sub p 0 2 = "ck"
      && (let rest = String.sub p 2 (String.length p - 2) in
          String.for_all (fun c -> c >= '0' && c <= '9') rest))

(* Width of net [n] from nl.wires, or 1 if not declared (single-bit). *)
let width_of_net (nl : netlist) n =
  match List.assoc_opt n nl.wires with
  | Some w -> w
  | None ->
      (* Could also be a module-level input/output port name. *)
      (match List.assoc_opt n nl.inputs with
       | Some w -> w
       | None ->
           (match List.assoc_opt n nl.outputs with
            | Some w -> w
            | None -> 1))

let bit_name net i width =
  if width = 1 then net else Printf.sprintf "%s[%d]" net i

let next_seq = ref 0
let fresh_net base =
  incr next_seq;
  Printf.sprintf "_bs_%s_%d_" base !next_seq

(* Insert N bit-level DFFs between two W-wide bus nets, returning the
   list of DFF instances.  [d_base] feeds the FF D-pins, [q_base] is
   driven by the FF Q-pins.  Both buses must have the same width.   *)
let dffs_between ~clk_net ~d_base ~q_base ~width ~inst_tag =
  let insts = ref [] in
  for i = 0 to width - 1 do
    let d_bit = bit_name d_base i width in
    let q_bit = bit_name q_base i width in
    let inst_name = Printf.sprintf "_bsr_%s_%d_" inst_tag i in
    insts := {
      cell = cell_dff;
      inst_name;
      conns = [
        { pin = "D";  net = d_bit };
        { pin = "CK"; net = clk_net };
        { pin = "Q";  net = q_bit };
      ];
    } :: !insts
  done;
  List.rev !insts

(* Lookup table for child-module port directions, built from the full
   netlists list.  [port_dir module_name port] → `In | `Out | `Unknown.   *)
type port_dir_table = (string * string, [`In | `Out]) Hashtbl.t

let build_port_dir_table (netlists : Hier_synth.module_netlist list) : port_dir_table =
  let h = Hashtbl.create 64 in
  List.iter (fun (mn : Hier_synth.module_netlist) ->
    List.iter (fun (p, _) -> Hashtbl.replace h (mn.mn_name, p) `In)
      mn.mn_real_inputs;
    List.iter (fun (p, _) -> Hashtbl.replace h (mn.mn_name, p) `Out)
      mn.mn_real_outputs;
  ) netlists;
  h

(* Wrap a single child instance's ports.  Returns the updated
   ci_conns and the list of inserted DFFs and any new wire decls.
   For ports whose direction is unknown (no table entry), falls back
   to a name heuristic (dout/out/q → output).                        *)
let wrap_one
    ~(nl : netlist)
    ~(port_dirs : port_dir_table)
    ~clk_net
    (ci_module : string)
    (ci_inst : string)
    (ci_conns : (string * string) list)
  : (string * string) list * instance list * (string * int) list =
  let new_dffs = ref [] in
  let new_wires = ref [] in
  let new_conns = List.map (fun (port, parent_net) ->
    if is_clock_port port then (port, parent_net)
    else begin
      let w = width_of_net nl parent_net in
      let priv = fresh_net (Printf.sprintf "%s_%s" ci_inst port) in
      new_wires := (priv, w) :: !new_wires;
      let tag = Printf.sprintf "%s_%s" ci_inst port in
      let is_output_port =
        match Hashtbl.find_opt port_dirs (ci_module, port) with
        | Some `Out -> true
        | Some `In -> false
        | None ->
            (* Fallback: name-based heuristic for unknown child modules
               (e.g. opaque macros not present in [netlists]).         *)
            let p = String.lowercase_ascii port in
            (String.length p >= 4 && String.sub p 0 4 = "dout")
            || (String.length p >= 3 && String.sub p 0 3 = "out")
            || p = "q" || p = "q0" || p = "q1"
      in
      let dffs =
        if is_output_port then
          dffs_between ~clk_net ~d_base:priv ~q_base:parent_net
            ~width:w ~inst_tag:tag
        else
          dffs_between ~clk_net ~d_base:parent_net ~q_base:priv
            ~width:w ~inst_tag:tag
      in
      new_dffs := dffs @ !new_dffs;
      (port, priv)
    end
  ) ci_conns in
  let _ = ci_module in
  (new_conns, !new_dffs, !new_wires)

(* Find the clock net used by this child instance.  Returns None if
   no clock port is present (in which case we can't wrap — skip). *)
let pick_clk_net (ci_conns : (string * string) list) =
  match List.find_opt (fun (p, _) -> is_clock_port p) ci_conns with
  | Some (_, n) -> Some n
  | None -> None

(* The public entry. [macro_names] is the set of child-instance module
   names to wrap as macros.  [wrap_all_children] = true also wraps any
   other hierarchical child instance — useful at the chip top, where
   every child is a black box at gate_sim level.                       *)
let wrap_module
    ?(wrap_all_children=false)
    ~(port_dirs : port_dir_table)
    ~(macro_names : string list)
    (mn : Hier_synth.module_netlist) : Hier_synth.module_netlist * int =
  let total = ref 0 in
  let nl = ref mn.mn_netlist in
  let new_child_insts = List.map (fun (ci : Hier_synth.child_inst_emit) ->
    let is_macro = List.mem ci.ci_module macro_names in
    if not (is_macro || wrap_all_children) then ci
    else
      match pick_clk_net ci.ci_conns with
      | None -> ci
      | Some clk_net ->
          let new_conns, new_dffs, new_wires =
            wrap_one ~nl:!nl ~port_dirs ~clk_net
              ci.ci_module ci.ci_inst ci.ci_conns in
          total := !total + List.length new_dffs;
          nl := { !nl with
                  insts = !nl.insts @ new_dffs;
                  wires = !nl.wires @ new_wires };
          { ci with ci_conns = new_conns }
  ) mn.mn_child_insts in
  ({ mn with
     mn_netlist = !nl;
     mn_child_insts = new_child_insts },
   !total)
