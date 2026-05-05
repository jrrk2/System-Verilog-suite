(* Placement-aware combinational arrival, on a flat instance graph.

   Inputs : DEF placements + DEF nets.
   Output : per-instance arrival (ps), longest-path = critical arrival.

   Each instance has a fixed gate-delay budget (set per cell type
   via [Cell_lib.gate_delay] — currently a flat 50 ps placeholder
   so the wire-delay term dominates and the regression can prove
   distance moves the number).  Each net contributes wire delay
   [Wire_delay.elmore_ps] charged once when crossing from driver
   to load.

   We don't read pin direction (need LEF for that), so we make a
   rough assumption: the FIRST pin listed on a net is the driver,
   the rest are loads.  That matches the convention OpenROAD
   writes out in DEF NETS lists. *)

type net_role = Driver | Load
type endpoint = { inst : string; pin : string; role : net_role }

(* Tag pins as driver/load assuming first-listed = driver. *)
let endpoints_of_net (net : Nets.net) =
  match net.Nets.pins with
  | [] -> []
  | drv :: ld ->
      { inst = drv.Nets.inst; pin = drv.Nets.pin; role = Driver } ::
      List.map (fun p -> { inst = p.Nets.inst; pin = p.Nets.pin; role = Load }) ld

(* Build [inst -> list of (downstream_inst, wire_delay_ps)]
   so we can do a longest-path topological traversal. *)
let fanout_edges ?(wp=Wire_delay.default_params) plc_tbl nets =
  let edges = Hashtbl.create 4096 in
  List.iter (fun (net : Nets.net) ->
    let eps = endpoints_of_net net in
    let driver_opt =
      List.find_opt (fun e -> e.role = Driver) eps in
    match driver_opt with
    | None -> ()
    | Some d ->
        let delay_ps, _ = Wire_delay.net_delay_ps ~p:wp plc_tbl net in
        List.iter (fun e ->
          if e.role = Load && e.inst <> d.inst then begin
            let cur = try Hashtbl.find edges d.inst with Not_found -> [] in
            Hashtbl.replace edges d.inst ((e.inst, delay_ps) :: cur)
          end) eps) nets;
  edges

(* Per-cell intrinsic delay.  Default is a 50-ps placeholder so the
   regression has a deterministic number; pass [~delay_of] to plug
   in real Liberty-derived numbers. *)
let default_cell_delay_ps _cell = 50.0

(* Memoised longest path from each node, with cycle break.
   Real gate-level netlists contain FF feedback loops; without
   LEF we can't classify FFs, so we treat any node already
   being computed as a 0-arrival sequential boundary. *)
let arrival_table ?(delay_of=default_cell_delay_ps) edges placements =
  let memo = Hashtbl.create 4096 in
  let in_progress = Hashtbl.create 64 in
  let cell_of = Hashtbl.create 4096 in
  List.iter (fun (p : Placement.placement) ->
    Hashtbl.replace cell_of p.Placement.inst p.Placement.cell) placements;
  let rec arr inst =
    match Hashtbl.find_opt memo inst with
    | Some v -> v
    | None when Hashtbl.mem in_progress inst -> 0.   (* cycle break *)
    | None ->
        Hashtbl.add in_progress inst ();
        let cell = try Hashtbl.find cell_of inst with Not_found -> "" in
        let outs = try Hashtbl.find edges inst with Not_found -> [] in
        let downstream = match outs with
          | [] -> 0.
          | _ -> List.fold_left
                   (fun acc (next, w) -> max acc (w +. arr next)) 0. outs
        in
        let total = delay_of cell +. downstream in
        Hashtbl.remove in_progress inst;
        Hashtbl.replace memo inst total;
        total
  in
  List.iter (fun (p : Placement.placement) -> ignore (arr p.Placement.inst)) placements;
  memo

type report = {
  worst_inst    : string;
  worst_arr_ps  : float;
  worst_cell    : string;
  total_wire_ps : float;
}

let report ?(wp=Wire_delay.default_params)
           ?(delay_of=default_cell_delay_ps) placements nets =
  let plc_tbl = Hpwl.placement_table placements in
  let edges = fanout_edges ~wp plc_tbl nets in
  let arr   = arrival_table ~delay_of edges placements in
  let worst_inst = ref "" and worst_v = ref 0. and worst_cell = ref "" in
  let cell_of = Hashtbl.create 4096 in
  List.iter (fun (p : Placement.placement) ->
    Hashtbl.replace cell_of p.Placement.inst p.Placement.cell) placements;
  Hashtbl.iter (fun inst v ->
    if v > !worst_v then begin
      worst_v := v; worst_inst := inst;
      worst_cell := (try Hashtbl.find cell_of inst with Not_found -> "")
    end) arr;
  let total_wire = List.fold_left
    (fun acc (n : Nets.net) ->
       let d, _ = Wire_delay.net_delay_ps ~p:wp plc_tbl n in
       acc +. d) 0. nets in
  {
    worst_inst    = !worst_inst;
    worst_arr_ps  = !worst_v;
    worst_cell    = !worst_cell;
    total_wire_ps = total_wire;
  }
