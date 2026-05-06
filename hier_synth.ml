(* Hierarchical synth driver (#103a).

   Synth each [bmodule] to gate level *separately*, in dependency
   order — leaves before parents.  Hardcaml never sees a parent's
   instances; instead, for each child instance encountered in a
   parent module, we add phantom IOs to the parent's hardcaml view:

     - child's output port  → phantom *input* on the parent
     - child's input  port  → phantom *output* on the parent

   At cell-Verilog emit time we then splice the child instance line
   back in alongside the parent's cells.  This preserves the dual
   hier/flat representation called for in
   project_hier_synth_architecture.md.

   Limitations of the first version:
     - port_connections must be [BVar name] (single signal per port).
       Concats / slices / constants on a child port → fail loudly.
     - param overrides on instances ignored (we synth the named
       child module directly; param specialisation is a follow-up).
     - bus widths on phantom signals come from the child module's
       declared port width, looked up by name; no implicit padding.

   When a leaf fails to lower (e.g. width-inference miss in
   Behavioral_to_hardcaml or unsupported BIR construct), we report
   which module + raise — the synth as a whole then fails loudly,
   per the no-silent-fallback rule.  *)

open Behavioral_ir

(* ── Output structure ──────────────────────────────────────────── *)

type child_inst_emit = {
  ci_module : string;
  ci_inst   : string;
  ci_conns  : (string * string) list;  (* port name, parent net *)
}

type module_netlist = {
  mn_name        : string;
  mn_real_inputs : (string * int) list;  (* declared input ports *)
  mn_real_outputs: (string * int) list;  (* declared output ports *)
  mn_netlist     : Lib_map.netlist;
  mn_child_insts : child_inst_emit list;
}

(* ── Helpers ───────────────────────────────────────────────────── *)

let signal_width (s : bsignal) =
  match s.stype with
  | BInt { width; _ } -> width
  | BBool -> 1
  | _ -> 1  (* fallback; complex types should be elaborated by now *)

let lookup_module name modules =
  List.find_opt (fun (m : bmodule) -> m.name = name) modules

(* Topological sort, children before parents. *)
let topo_sort (modules : bmodule list) : bmodule list =
  let visited = Hashtbl.create 16 in
  let temp = Hashtbl.create 16 in
  let order = ref [] in
  let rec visit (m : bmodule) =
    if Hashtbl.mem visited m.name then ()
    else if Hashtbl.mem temp m.name then
      failwith (Printf.sprintf "module dependency cycle at %s" m.name)
    else begin
      Hashtbl.add temp m.name ();
      List.iter (fun (i : binstance) ->
        match lookup_module i.module_name modules with
        | Some child -> visit child
        | None -> ()  (* unknown — treat as black box *)
      ) m.instances;
      Hashtbl.remove temp m.name;
      Hashtbl.add visited m.name ();
      order := m :: !order
    end in
  List.iter visit modules;
  List.rev !order

(* Extract the parent-net name from a child port_connection's bexpr.
   For now only [BVar n] is allowed; everything else fails loudly. *)
let net_of_conn (parent_name : string) (port : string) (e : bexpr) =
  match e with
  | BVar n -> n
  | _ ->
      failwith (Printf.sprintf
        "hier_synth: parent %s's child connection on port %s is not a \
         simple signal — concat/slice/const on instance pins not yet supported"
        parent_name port)

(* For each child instance, build the splice record + the list of
   (parent-side-signal, width, direction) phantom IO it adds. *)
let child_inst_phantoms
    ~(parent : bmodule) ~(modules : bmodule list)
    (i : binstance)
  : child_inst_emit
    * (string * int * [ `In | `Out ]) list  (* phantom IO additions *)
  =
  let child = match lookup_module i.module_name modules with
    | Some c -> c
    | None ->
        failwith (Printf.sprintf
          "hier_synth: parent %s instantiates unknown module %s"
          parent.name i.module_name) in
  let ci_conns =
    List.map (fun (port, e) ->
      (port, net_of_conn parent.name port e)) i.port_connections in
  let phantoms =
    List.map (fun (port, parent_net) ->
      let cs = List.find (fun (s : bsignal) -> s.name = port) child.signals in
      let w = signal_width cs in
      let dir =
        match cs.direction with
        | `Output -> `In   (* child OUTPUT → drives parent ⇒ parent INPUT *)
        | `Input  -> `Out  (* child INPUT  → driven by parent ⇒ parent OUTPUT *)
        | `Internal ->
            failwith (Printf.sprintf
              "hier_synth: child %s port %s has direction Internal — invalid"
              i.module_name port) in
      (parent_net, w, dir)) ci_conns in
  { ci_module = i.module_name; ci_inst = i.inst_name; ci_conns }, phantoms

(* Take a parent's bmodule, fold in phantom ports for child instances,
   and return: (synth-able bmodule, child instance splice list,
   set of phantom-IO names).  The synth-able view has m.instances=[]
   so [Behavioral_to_hardcaml.create_circuit] doesn't choke. *)
let prepare_parent (parent : bmodule) (modules : bmodule list) =
  let child_results =
    List.map (child_inst_phantoms ~parent ~modules) parent.instances in
  let child_insts  = List.map fst child_results in
  let phantom_decl = List.concat_map snd child_results in
  (* Promote phantoms in the signal list, but never demote real ports.
     A phantom that names an existing real port is a no-op (the port
     already has the right direction). *)
  let signals' =
    List.map (fun (s : bsignal) ->
      match List.find_opt (fun (n, _, _) -> n = s.name) phantom_decl with
      | None -> s
      | Some (_, _, dir) ->
          (match s.direction, dir with
           | `Input,  _    -> s
           | `Output, _    -> s
           | `Internal, `In  -> { s with direction = `Input }
           | `Internal, `Out -> { s with direction = `Output })
    ) parent.signals in
  (* Add brand-new signals for phantom names not in the original list. *)
  let extra_signals =
    List.filter_map (fun (n, w, dir) ->
      if List.exists (fun (s : bsignal) -> s.name = n) signals' then None
      else Some {
        name = n;
        stype = (if w = 1 then BBool else BInt { width = w; signed = Unsigned });
        direction = (match dir with `In -> `Input | `Out -> `Output);
        initial_value = None;
        attrs = [];
      }) phantom_decl in
  let synth_bmod = {
    parent with
    signals = signals' @ extra_signals;
    instances = [];
  } in
  let phantom_names =
    List.fold_left (fun acc (n, _, _) -> n :: acc) [] phantom_decl
    |> List.sort_uniq compare in
  synth_bmod, child_insts, phantom_names

(* ── Per-module synth ─────────────────────────────────────────── *)

let synth_one ~(modules : bmodule list) (m : bmodule) : module_netlist =
  let real_inputs =
    List.filter_map (fun (s : bsignal) ->
      if s.direction = `Input then Some (s.name, signal_width s) else None
    ) m.signals in
  let real_outputs =
    List.filter_map (fun (s : bsignal) ->
      if s.direction = `Output then Some (s.name, signal_width s) else None
    ) m.signals in
  let synth_bmod, child_insts, _phantom_names =
    if m.instances = []
    then m, [], []
    else prepare_parent m modules
  in
  let circuit =
    try Behavioral_to_hardcaml.create_circuit synth_bmod
    with e ->
      failwith (Printf.sprintf "hier_synth: lowering failed for module %s: %s"
                  m.name (Printexc.to_string e))
  in
  let netlist = Lib_map.map_circuit circuit in
  {
    mn_name = m.name;
    mn_real_inputs = real_inputs;
    mn_real_outputs = real_outputs;
    mn_netlist = netlist;
    mn_child_insts = child_insts;
  }

(* ── Top-level driver ─────────────────────────────────────────── *)

let synth_program (prog : bprogram) : module_netlist list =
  let ordered = topo_sort prog.modules in
  List.map (synth_one ~modules:prog.modules) ordered
