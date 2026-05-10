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

(* Tag pins as driver/load.  When [cell_of] and [pin_dir] are
   supplied (typically from LEF + DEF placements) we use the
   real pin direction; otherwise we fall back to the
   "first-pin-is-driver" convention. *)
let endpoints_of_net ?cell_of ?pin_dir (net : Nets.net) =
  let by_dir =
    match cell_of, pin_dir with
    | Some c_of, Some p_dir ->
        let drivers, loads =
          List.partition (fun (pr : Nets.pin_ref) ->
            match Hashtbl.find_opt c_of pr.Nets.inst with
            | None -> false
            | Some cell ->
                Hashtbl.find_opt p_dir (cell, pr.Nets.pin)
                  = Some Lef_pins.Output) net.Nets.pins in
        if drivers = [] then None else Some (drivers, loads)
    | _ -> None
  in
  match by_dir with
  | Some (drivers, loads) ->
      List.map (fun (p : Nets.pin_ref) ->
        { inst = p.Nets.inst; pin = p.Nets.pin; role = Driver }) drivers @
      List.map (fun (p : Nets.pin_ref) ->
        { inst = p.Nets.inst; pin = p.Nets.pin; role = Load }) loads
  | None ->
      match net.Nets.pins with
      | [] -> []
      | drv :: ld ->
          { inst = drv.Nets.inst; pin = drv.Nets.pin; role = Driver } ::
          List.map (fun p -> { inst = p.Nets.inst; pin = p.Nets.pin; role = Load }) ld

(* Build [inst -> list of (downstream_inst, wire_delay_ps)]
   so we can do a longest-path topological traversal.  When
   [pin_dir] is supplied it is used to identify drivers via LEF
   pin direction; otherwise we fall back to first-pin convention. *)
let fanout_edges ?(wp=Wire_delay.default_params) ?cell_of ?pin_dir plc_tbl nets =
  let edges = Hashtbl.create 4096 in
  List.iter (fun (net : Nets.net) ->
    let eps = endpoints_of_net ?cell_of ?pin_dir net in
    let drivers = List.filter (fun e -> e.role = Driver) eps in
    let delay_ps, _ = Wire_delay.net_delay_ps ~p:wp plc_tbl net in
    List.iter (fun (d : endpoint) ->
      List.iter (fun (e : endpoint) ->
        if e.role = Load && e.inst <> d.inst then begin
          let cur = try Hashtbl.find edges d.inst with Not_found -> [] in
          Hashtbl.replace edges d.inst ((e.inst, delay_ps) :: cur)
        end) eps) drivers) nets;
  edges

(* Per-cell intrinsic delay.  Default is a 50-ps placeholder so the
   regression has a deterministic number; pass [~delay_of] to plug
   in real Liberty-derived numbers. *)
let default_cell_delay_ps _cell = 50.0

(* Slew-aware lookup: returns [(delay_ps, out_slew_ns)].  The
   default keeps the single-number delay model when no Liberty
   data is available; out_slew defaults to a steady-state
   ~10 ps so the propagation is well-defined. *)
type delay_slew_fn = string -> slew:float -> load:float -> float * float
let default_delay_slew_fn : delay_slew_fn =
  fun cell ~slew:_ ~load:_ -> (default_cell_delay_ps cell, 0.01)

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

(* Forward-propagation arrival with per-pin slew tracking.  The
   load seen by each driver is approximated as
       fanout_count * pin_cap_fF + wire_cap_per_dbu * hpwl
   with [pin_cap_fF] defaulting to 1.0 (a typical X1-strength
   input-pin cap in Nangate45).  For a strict feed-forward
   netlist (every cycle broken at FF boundaries upstream) a
   topological order suffices; we get one via depth-first
   traversal from the cells that have no fanin in [edges]. *)
let arrival_table_forward
    ?(pin_cap_fF=1.0)
    ?(wp=Wire_delay.default_params)
    ?(default_in_slew=0.05)
    ~edges ~plc_tbl ~delay_slew_fn placements =
  (* Reverse fanin: build [inst -> (driver_inst, wire_delay) list]. *)
  let fanin = Hashtbl.create 4096 in
  let fanout_count = Hashtbl.create 4096 in
  Hashtbl.iter (fun src outs ->
    Hashtbl.replace fanout_count src (List.length outs);
    List.iter (fun (dst, w) ->
      let cur = try Hashtbl.find fanin dst with Not_found -> [] in
      Hashtbl.replace fanin dst ((src, w) :: cur)) outs) edges;
  let arrival = Hashtbl.create 4096 in
  let out_slew = Hashtbl.create 4096 in
  let in_progress = Hashtbl.create 64 in
  let cell_of = Hashtbl.create 4096 in
  List.iter (fun (p : Placement.placement) ->
    Hashtbl.replace cell_of p.Placement.inst p.Placement.cell) placements;
  let _ = plc_tbl and _ = wp in
  let rec eval inst =
    match Hashtbl.find_opt arrival inst with
    | Some _ -> ()
    | None when Hashtbl.mem in_progress inst -> ()  (* cycle break *)
    | None ->
        Hashtbl.add in_progress inst ();
        let cell = try Hashtbl.find cell_of inst with Not_found -> "" in
        let is_port = (cell = "") in
        let fanins = try Hashtbl.find fanin inst with Not_found -> [] in
        List.iter (fun (src, _) -> eval src) fanins;
        let in_slew, fanin_arr =
          match fanins with
          | [] -> (default_in_slew, 0.0)
          | _  ->
              List.fold_left (fun (sw, a) (src, w) ->
                let src_arr = try Hashtbl.find arrival src with Not_found -> 0.0 in
                let src_slew =
                  try Hashtbl.find out_slew src
                  with Not_found -> default_in_slew in
                (max sw src_slew, max a (src_arr +. w))) (0.0, 0.0) fanins in
        let n_out = try Hashtbl.find fanout_count inst with Not_found -> 1 in
        let load = pin_cap_fF *. float_of_int (max 1 n_out) in
        let delay, os =
          if is_port then (0.0, in_slew)
          else delay_slew_fn cell ~slew:in_slew ~load in
        Hashtbl.replace arrival inst (fanin_arr +. delay);
        Hashtbl.replace out_slew inst os;
        Hashtbl.remove in_progress inst
  in
  List.iter (fun (p : Placement.placement) -> eval p.Placement.inst) placements;
  arrival

type report = {
  worst_inst    : string;
  worst_arr_ps  : float;
  worst_cell    : string;
  total_wire_ps : float;
  (* Path back from [worst_inst] to a starting endpoint, ordered
     start → end.  Each tuple is (inst, cell, arrival_ps).  Used by
     the GUI when 6_finish.rpt is unavailable so an intermediate
     stage can still be opened with a critical-path overlay. *)
  path_hops     : (string * string * float) list;
}

let report ?(wp=Wire_delay.default_params)
           ?(delay_of=default_cell_delay_ps)
           ?delay_slew_fn
           ?pin_dir placements nets =
  let plc_tbl = Hpwl.placement_table placements in
  let cell_of = Hashtbl.create (List.length placements) in
  List.iter (fun (p : Placement.placement) ->
    Hashtbl.replace cell_of p.Placement.inst p.Placement.cell) placements;
  let edges = fanout_edges ~wp ~cell_of ?pin_dir plc_tbl nets in
  let arr =
    match delay_slew_fn with
    | Some dsf ->
        arrival_table_forward ~edges ~plc_tbl ~delay_slew_fn:dsf placements
    | None ->
        arrival_table ~delay_of edges placements in
  let worst_inst = ref "" and worst_v = ref 0. and worst_cell = ref "" in
  let cell_of = Hashtbl.create 4096 in
  List.iter (fun (p : Placement.placement) ->
    Hashtbl.replace cell_of p.Placement.inst p.Placement.cell) placements;
  (* Clock-tree cells (CLKBUF / CLKGATE / CLKINV …) carry the largest
     accumulated arrival in any post-CTS netlist because they form a
     deliberate buffer chain.  They aren't data paths — they're the
     clock distribution itself.  Skip them when picking the worst
     data-arrival endpoint.  Match on cell_name prefix (works across
     Nangate45 / sky130 / gf180mcu naming).  Also skip cells whose
     instance name starts with `clkbuf_`/`clknet_` — those are the
     names ORFS gives to CTS-inserted buffer instances regardless
     of the underlying cell type.                                  *)
  let is_clock_cell ~cell ~inst =
    let starts pfx s =
      let pl = String.length pfx and sl = String.length s in
      sl >= pl && String.sub s 0 pl = pfx in
    starts "CLKBUF" cell || starts "CLKGATE" cell
    || starts "CLKINV" cell || starts "CLKAND" cell
    || starts "clkbuf_" inst || starts "clknet_" inst
    || starts "delaybuf_" inst || starts "clkload" inst
  in
  Hashtbl.iter (fun inst v ->
    let cell = try Hashtbl.find cell_of inst with Not_found -> "" in
    if v > !worst_v && not (is_clock_cell ~cell ~inst) then begin
      worst_v := v; worst_inst := inst;
      worst_cell := cell
    end) arr;
  let total_wire = List.fold_left
    (fun acc (n : Nets.net) ->
       let d, _ = Wire_delay.net_delay_ps ~p:wp plc_tbl n in
       acc +. d) 0. nets in
  (* Reconstruct a path from worst_inst back to an endpoint by
     walking fanin edges, picking the predecessor with max arrival
     at each hop.  Stops when a node has no fanin (port / startpoint)
     or arrival drops to 0.  The forward edges have wire weights;
     "max-arrival fanin" is approximated as: among (src, w) pairs in
     the inverted edge list, pick the src whose [arr.(src) + w] is
     largest.  Cap at 256 hops to avoid pathological loops.        *)
  let fanin = Hashtbl.create 4096 in
  Hashtbl.iter (fun src outs ->
    List.iter (fun (dst, w) ->
      let cur = try Hashtbl.find fanin dst with Not_found -> [] in
      Hashtbl.replace fanin dst ((src, w) :: cur)) outs) edges;
  let cell_of_or_blank inst =
    try Hashtbl.find cell_of inst with Not_found -> "" in
  let arr_of inst =
    try Hashtbl.find arr inst with Not_found -> 0. in
  (* Each step prepends the current hop, so after the walk the head
     of [hops] is the chain head (oldest, lowest arrival) and the
     last element is the end (worst).  Returning hops *without*
     reversing gives natural start→end ordering for the report. *)
  let is_placed inst = Hashtbl.mem cell_of inst in
  let rec walk hops cur depth =
    if depth >= 256 then hops
    else
      let cell = cell_of_or_blank cur in
      (* Only emit a hop for instances that actually have a placement
         — otherwise the path overlay can't render them.  Pseudo-
         names like sub-block instance prefixes ("cpu") that appear
         in net pin refs but have no own COMPONENT entry would
         otherwise pollute the hop list and tank the
         hops-matched / hops-total ratio in the GUI overlay.       *)
      let hops =
        if is_placed cur then (cur, cell, arr_of cur) :: hops
        else hops in
      match Hashtbl.find_opt fanin cur with
      | None | Some [] -> hops
      | Some preds ->
          (* Skip clock-tree predecessors AND non-placed pseudo-
             names when walking back. *)
          let preds =
            List.filter (fun (src, _) ->
              let c = cell_of_or_blank src in
              is_placed src
              && not (is_clock_cell ~cell:c ~inst:src)) preds in
          let best =
            List.fold_left (fun acc (src, w) ->
              let a = arr_of src +. w in
              match acc with
              | None -> Some (src, a)
              | Some (_, b) when a > b -> Some (src, a)
              | other -> other
            ) None preds in
          (match best with
           | None -> hops
           | Some (src, _) -> walk hops src (depth + 1)) in
  let path_hops =
    if !worst_inst = "" then [] else walk [] !worst_inst 0 in
  {
    worst_inst    = !worst_inst;
    worst_arr_ps  = !worst_v;
    worst_cell    = !worst_cell;
    total_wire_ps = total_wire;
    path_hops;
  }
