(* schematic_layout.ml — given a BIR module and a symbol library, place
   every instance on a 2-D canvas using a Sugiyama-flavoured layered
   layout and emit polylines for every net.

   The placement is intentionally coarse: it produces something that
   reads as a schematic rather than as a tangle, but does not try to be
   competitive with a commercial router.

   Coordinate convention: canvas y grows downwards (Cairo native); the
   .slib y-up coordinates from Symbol_lib are flipped here once, when
   pin positions are absolutised. *)

open Symbol_lib

(* ---------------- output types ---------------- *)

type placed_pin = {
  pp_inst : string;          (* instance name; "" for module-port pads *)
  pp_pin  : string;          (* pin name *)
  pp_pos  : float * float;   (* absolute canvas coords *)
  pp_dir  : pin_dir;
}

type placed_inst = {
  pi_inst : string;
  pi_type : string;
  pi_sym  : symbol;
  pi_xy   : float * float;
  pi_w    : float;
  pi_h    : float;
  pi_pins : placed_pin list;
}

type port_pad = {
  pad_name : string;
  pad_dir  : [`Input | `Output];
  pad_pos  : float * float;
}

type net = {
  net_key       : string;
  net_endpoints : placed_pin list;
  net_polyline  : (float * float) list list;  (* trunk + spine + stubs *)
  net_tie_const : string option;  (* "0" / "1" when driven by a tie cell *)
}

type schematic = {
  sc_module : string;
  sc_insts  : placed_inst list;
  sc_ports  : port_pad list;
  sc_nets   : net list;
  sc_width  : float;
  sc_height : float;
}

(* ---------------- helpers ---------------- *)

(* Identify Liberty tie cells so they don't show up as visible boxes.
   The driven net is rendered instead as a small "0"/"1" label at each
   consumer pin. *)
let tie_value cell_name : string option =
  let n = String.uppercase_ascii cell_name in
  let starts pfx =
    let lp = String.length pfx in
    String.length n >= lp && String.sub n 0 lp = pfx in
  if starts "LOGIC0" || starts "TIELO" || starts "TIE0"
  then Some "0"
  else if starts "LOGIC1" || starts "TIEHI" || starts "TIE1"
  then Some "1"
  else None

let lookup_module (prog : Behavioral_ir.bprogram) name =
  List.find_opt (fun (m : Behavioral_ir.bmodule) -> m.name = name)
    prog.modules

let lookup_library_cell (prog : Behavioral_ir.bprogram) name =
  List.assoc_opt name prog.library_cells

let dir_of_lib (lp : Behavioral_ir.library_port) : pin_dir =
  match lp.port_direction with
  | `Input  -> PinIn
  | `Output -> PinOut

let _dir_of_bsig (s : Behavioral_ir.bsignal) : pin_dir option =
  match s.direction with
  | `Input | `Inout -> Some PinIn
  | `Output -> Some PinOut
  | `Internal -> None

let symbol_for_instance
    ~(slib : library)
    ~(prog : Behavioral_ir.bprogram)
    (inst : Behavioral_ir.binstance) : symbol =
  match find_opt slib inst.module_name with
  | Some s -> s
  | None ->
      (* Try library_cells first. *)
      (match lookup_library_cell prog inst.module_name with
       | Some ports ->
           let pins = List.map (fun (lp : Behavioral_ir.library_port) ->
             (lp.port_name,
              match lp.port_direction with
              | `Input -> "input" | `Output -> "output")) ports in
           auto_generate ~cell_name:inst.module_name ~pins
       | None ->
           (* Fall back to a user-defined module's port list. *)
           let pins = match lookup_module prog inst.module_name with
             | Some m ->
                 List.filter_map (fun (s : Behavioral_ir.bsignal) ->
                   match s.direction with
                   | `Input  -> Some (s.name, "input")
                   | `Inout  -> Some (s.name, "inout")
                   | `Output -> Some (s.name, "output")
                   | `Internal -> None) m.signals
             | None ->
                 (* Last resort: use whatever pin names appear in
                    port_connections, guessing direction from index. *)
                 List.map (fun (p, _) -> (p, "input"))
                   inst.port_connections in
           auto_generate ~cell_name:inst.module_name ~pins)

(* String-canonicalise a bexpr for use as a net key.  Two identical
   sub-expressions in different connections then alias onto the same
   net. *)
let net_key_of (e : Behavioral_ir.bexpr) : string =
  Behavioral_ir.string_of_bexpr e

(* Strip whitespace + parens so simple `BVar x` and `(x)` collide. *)
let canon s =
  let b = Buffer.create (String.length s) in
  String.iter (fun c ->
    if c <> ' ' && c <> '\t' then Buffer.add_char b c) s;
  Buffer.contents b

(* ---------------- ranking ---------------- *)

module SMap = Map.Make (String)
module SSet = Set.Make (String)

(* Compute a longest-path rank for each instance.  Nodes with no
   predecessors get rank 0; rank propagates +1 along every edge.
   Cycles (registers) are handled by capping the iteration count and
   leaving feedback nodes in the rank reached during the cap.         *)
let rank_instances
    ~(driver_of_net : (string, string) Hashtbl.t)
    ~(consumers_of_net : (string, string list) Hashtbl.t)
    ~(inst_names : string list)
  : int SMap.t =
  let rank = ref (List.fold_left (fun m n -> SMap.add n 0 m) SMap.empty inst_names) in
  let n_iters = max 8 (List.length inst_names) in
  for _ = 1 to n_iters do
    Hashtbl.iter (fun net driver ->
      let r_drv = try SMap.find driver !rank with Not_found -> 0 in
      let cs = try Hashtbl.find consumers_of_net net with Not_found -> [] in
      List.iter (fun c ->
        let r_c = try SMap.find c !rank with Not_found -> 0 in
        if r_drv + 1 > r_c && r_drv + 1 < 256 then
          rank := SMap.add c (r_drv + 1) !rank
      ) cs
    ) driver_of_net
  done;
  !rank

(* Barycenter ordering: for several rounds, sort each rank by the mean
   x-position of its predecessors (and successors). *)
let barycenter
    ~(by_rank : string list array)
    ~(rank_of : string -> int)
    ~(predecessors : string -> string list)
    ~(successors : string -> string list)
  : string list array =
  let by_rank = Array.copy by_rank in
  for _ = 1 to 6 do
    Array.iteri (fun r lst ->
      let prev =
        if r = 0 then Array.of_list lst
        else by_rank.(r - 1) |> Array.of_list in
      let next =
        if r = Array.length by_rank - 1 then Array.of_list lst
        else by_rank.(r + 1) |> Array.of_list in
      let idx_of arr n =
        let found = ref (-1) in
        Array.iteri (fun i x -> if x = n && !found < 0 then found := i) arr;
        if !found < 0 then 0.0 else float_of_int !found in
      let score n =
        let preds = predecessors n in
        let succs = successors n in
        let sum_p = List.fold_left (fun a p ->
          if rank_of p = r - 1 then a +. idx_of prev p else a) 0.0 preds in
        let n_p = List.length (List.filter (fun p -> rank_of p = r - 1) preds) in
        let sum_s = List.fold_left (fun a s ->
          if rank_of s = r + 1 then a +. idx_of next s else a) 0.0 succs in
        let n_s = List.length (List.filter (fun s -> rank_of s = r + 1) succs) in
        let avg_p = if n_p > 0 then sum_p /. float_of_int n_p else float_of_int (List.length lst) /. 2.0 in
        let avg_s = if n_s > 0 then sum_s /. float_of_int n_s else avg_p in
        (avg_p +. avg_s) /. 2.0 in
      let scored = List.map (fun n -> (score n, n)) lst in
      let scored = List.sort (fun (a, _) (b, _) -> compare a b) scored in
      by_rank.(r) <- List.map snd scored
    ) by_rank
  done;
  by_rank

(* ---------------- main entry point ---------------- *)

let _x_gap_unused = 140.0
let _y_gap_unused = 30.0
let port_pad_w = 18.0
let module_margin = 60.0

(* Logical segments used by track allocator. *)
type seg_h = {
  sh_net : int;
  sh_ch  : int;
  sh_g0  : int;
  sh_g1  : int;
  mutable sh_track : int;
}
type seg_v = {
  sv_net : int;
  sv_g   : int;
  sv_r0  : int;
  sv_r1  : int;
  mutable sv_track : int;
}
type net_logical = {
  nl_key : string;
  nl_idx : int;
  nl_eps : (string * string * pin_dir) list;
  nl_drv_inst : string;
  nl_drv_port : string;
  nl_consumers : (string * string) list;
  nl_trunk_gap : int;
  nl_tie : string option;
}

let build
    ~(slib : library)
    ~(prog : Behavioral_ir.bprogram)
    (m : Behavioral_ir.bmodule)
  : schematic =
  (* 1. Symbol resolution for every instance.  *)
  let inst_sym : (string, symbol) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (i : Behavioral_ir.binstance) ->
    Hashtbl.replace inst_sym i.inst_name (symbol_for_instance ~slib ~prog i)
  ) m.instances;

  (* 2. Net wiring: for every (inst, port -> bexpr), record the
        endpoint.  Tie cells are intercepted here: their output nets
        are tagged with the driven constant so the renderer can put a
        "0"/"1" label at each consumer instead of routing a wire.   *)
  let net_endpoints : (string, (string * string * pin_dir) list) Hashtbl.t =
    Hashtbl.create 128 in
  let tie_consts : (string, string) Hashtbl.t = Hashtbl.create 16 in
  let add_endpoint key ep =
    let k = canon key in
    let prev = try Hashtbl.find net_endpoints k with Not_found -> [] in
    Hashtbl.replace net_endpoints k (ep :: prev) in

  List.iter (fun (i : Behavioral_ir.binstance) ->
    match tie_value i.module_name with
    | Some v ->
        (* Don't emit endpoints for tie cells — just remember which
           nets they drive. *)
        let sym = Hashtbl.find inst_sym i.inst_name in
        List.iter (fun (port, value) ->
          match List.find_opt (fun p -> p.pin_name = port) sym.sym_pins with
          | Some p when p.pin_dir = PinOut ->
              Hashtbl.replace tie_consts (canon (net_key_of value)) v
          | _ -> ()
        ) i.port_connections
    | None ->
        let sym = Hashtbl.find inst_sym i.inst_name in
        List.iter (fun (port, value) ->
          let dir = match List.find_opt (fun p -> p.pin_name = port) sym.sym_pins with
            | Some p -> p.pin_dir
            | None -> PinInOut in
          add_endpoint (net_key_of value) (i.inst_name, port, dir)
        ) i.port_connections
  ) m.instances;

  (* Module-level ports: every BVar that names a module port also acts
     as a pad on the canvas border.  Inputs are sources (drive into the
     module body); outputs are sinks. *)
  let module_ports =
    List.filter_map (fun (s : Behavioral_ir.bsignal) ->
      match s.direction with
      | `Input | `Inout -> Some (s.name, `Input)
      | `Output -> Some (s.name, `Output)
      | `Internal -> None) m.signals in
  List.iter (fun (name, dir) ->
    let dir' = match dir with
      | `Input -> PinOut   (* the pad drives into the module *)
      | `Output -> PinIn in
    add_endpoint (canon name) ("", name, dir')
  ) module_ports;

  (* 3. Build driver/consumer maps for ranking.  We use the FIRST
        endpoint of direction PinOut as the canonical driver.  All
        other endpoints become consumers. *)
  let driver_of_net : (string, string) Hashtbl.t = Hashtbl.create 128 in
  let consumers_of_net : (string, string list) Hashtbl.t = Hashtbl.create 128 in
  Hashtbl.iter (fun net eps ->
    let drv = List.find_opt (fun (_, _, d) -> d = PinOut) eps in
    (match drv with
     | Some (inst, _, _) when inst <> "" ->
         Hashtbl.replace driver_of_net net inst;
         let cs = List.filter_map (fun (i, _, d) ->
           if d <> PinOut && i <> "" then Some i else None) eps in
         Hashtbl.replace consumers_of_net net cs
     | _ ->
         (* Net is purely consumed by instances (e.g. module input);
            treat as if rank-0 pad drives it. *)
         let cs = List.filter_map (fun (i, _, _) ->
           if i <> "" then Some i else None) eps in
         if cs <> [] then begin
           Hashtbl.replace driver_of_net net ("__pad__:" ^ net);
           Hashtbl.replace consumers_of_net net cs
         end)
  ) net_endpoints;

  (* 4. Cone-based ranking.  Every primary output of the module defines
        a logic cone: the set of cells reachable by walking backward
        through driver chains until a primary input or unresolved net.
        Each cell is greedy-assigned to the first cone that claims it.
        Within each cone we rank from the output cell (rank 0 at the
        cone's right edge) so the deepest-delay cell sits rightmost
        and no wire goes backwards in the cone's local DAG. *)
  let visible_insts = List.filter (fun (i : Behavioral_ir.binstance) ->
    tie_value i.module_name = None) m.instances in
  ignore consumers_of_net;

  let cell_input_nets : (string, string list) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (i : Behavioral_ir.binstance) ->
    let sym = Hashtbl.find inst_sym i.inst_name in
    let nets = List.filter_map (fun (port, value) ->
      match List.find_opt (fun (p : pin) -> p.pin_name = port) sym.sym_pins with
      | Some p when p.pin_dir = PinIn -> Some (canon (net_key_of value))
      | _ -> None) i.port_connections in
    Hashtbl.replace cell_input_nets i.inst_name nets
  ) visible_insts;

  let primary_outputs =
    List.filter_map (fun (s : Behavioral_ir.bsignal) ->
      match s.direction with `Output -> Some s.name | _ -> None) m.signals in

  let is_pad_driver d =
    String.length d >= 8 && String.sub d 0 8 = "__pad__:" in

  ignore (fun (output_name : string) ->
    (* unused now; cell→cone assignment derives directly from depths *)
    let _ = output_name in ());

  (* Collect cone roots: every cell output net that is not consumed by
     any other cell.  This covers both module-driven primary outputs
     and netlists where the top-level output isn't wired up (e.g.
     dangling sums in a multiplier whose register linkage hasn't been
     emitted).  We also include the named module primary outputs so a
     well-formed netlist gets cones keyed by the user's port names. *)
  let cell_output_nets : (string, string list) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (i : Behavioral_ir.binstance) ->
    let sym = Hashtbl.find inst_sym i.inst_name in
    let outs = List.filter_map (fun (port, value) ->
      match List.find_opt (fun (p : pin) -> p.pin_name = port) sym.sym_pins with
      | Some p when p.pin_dir = PinOut -> Some (canon (net_key_of value))
      | _ -> None) i.port_connections in
    Hashtbl.replace cell_output_nets i.inst_name outs
  ) visible_insts;

  let consumed_nets : (string, unit) Hashtbl.t = Hashtbl.create 128 in
  List.iter (fun (i : Behavioral_ir.binstance) ->
    let nets = try Hashtbl.find cell_input_nets i.inst_name with Not_found -> [] in
    List.iter (fun n -> Hashtbl.replace consumed_nets n ()) nets
  ) visible_insts;

  let terminal_nets = ref [] in
  let seen_terminal = Hashtbl.create 32 in
  List.iter (fun (i : Behavioral_ir.binstance) ->
    let outs = try Hashtbl.find cell_output_nets i.inst_name with Not_found -> [] in
    List.iter (fun n ->
      if not (Hashtbl.mem consumed_nets n)
         && not (Hashtbl.mem seen_terminal n)
      then begin
        Hashtbl.add seen_terminal n ();
        terminal_nets := n :: !terminal_nets
      end
    ) outs
  ) visible_insts;
  let terminal_nets = List.rev !terminal_nets in

  let cone_keys =
    let by_named = List.map (fun n -> (n, n)) primary_outputs in
    let by_term  = List.filter_map (fun n ->
      if List.mem n primary_outputs then None else Some (n, n)) terminal_nets in
    by_named @ by_term in

  let n_real_cones = List.length cone_keys in
  let misc_cone_idx = n_real_cones in
  let n_cones = n_real_cones + 1 in

  (* For each (cell, cone) pair, the shortest-path distance from the
     cone's output cell to this cell (counted in stages of combinational
     logic, going backwards).  Cells unreachable from a cone's output
     don't have an entry. *)
  let cell_depth : (string * int, int) Hashtbl.t = Hashtbl.create 1024 in
  List.iteri (fun ci (_, net) ->
    match Hashtbl.find_opt driver_of_net (canon net) with
    | None -> ()
    | Some d when is_pad_driver d -> ()
    | Some drv ->
        let q = Queue.create () in
        Queue.add (drv, 0) q;
        while not (Queue.is_empty q) do
          let (cell, d) = Queue.pop q in
          let key = (cell, ci) in
          let should = match Hashtbl.find_opt cell_depth key with
            | None -> true
            | Some r -> d < r in
          if should then begin
            Hashtbl.replace cell_depth key d;
            let nets = try Hashtbl.find cell_input_nets cell with Not_found -> [] in
            List.iter (fun n ->
              match Hashtbl.find_opt driver_of_net n with
              | None -> ()
              | Some next when is_pad_driver next -> ()
              | Some next -> Queue.add (next, d + 1) q
            ) nets
          end
        done
  ) cone_keys;

  (* Assign each cell to the cone where it sits closest to the output.
     Per-cone rank = its depth from that cone's output. *)
  let cell_cone : (string, int) Hashtbl.t = Hashtbl.create 64 in
  let cell_per_cone_rank : (string, int) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (i : Behavioral_ir.binstance) ->
    let best_ci = ref misc_cone_idx in
    let best_d  = ref max_int in
    for ci = 0 to n_real_cones - 1 do
      match Hashtbl.find_opt cell_depth (i.inst_name, ci) with
      | None -> ()
      | Some d -> if d < !best_d then begin best_d := d; best_ci := ci end
    done;
    Hashtbl.add cell_cone i.inst_name !best_ci;
    Hashtbl.add cell_per_cone_rank i.inst_name
      (if !best_d = max_int then 0 else !best_d)
  ) visible_insts;

  let cone_max_rank = Array.make n_cones 0 in
  Hashtbl.iter (fun c r ->
    let ci = try Hashtbl.find cell_cone c with Not_found -> misc_cone_idx in
    if r > cone_max_rank.(ci) then cone_max_rank.(ci) <- r
  ) cell_per_cone_rank;

  (* Diagnostic — cells per cone, useful when sanity-checking cone
     identification on real designs. *)
  if (try Sys.getenv "SV_DECOMP_SCHEMATIC_DEBUG" = "1" with _ -> false) then begin
    let cone_size = Array.make n_cones 0 in
    Hashtbl.iter (fun _ ci ->
      if ci >= 0 && ci < n_cones then
        cone_size.(ci) <- cone_size.(ci) + 1
    ) cell_cone;
    Printf.eprintf "[schematic] %d cone(s) total (%d named outputs + %d terminal-net + 1 misc)\n"
      n_cones (List.length primary_outputs)
      (List.length cone_keys - List.length primary_outputs);
    List.iteri (fun ci (label, _) ->
      Printf.eprintf "  cone %d (%s): %d cells, max-rank %d\n"
        ci label cone_size.(ci) cone_max_rank.(ci)
    ) cone_keys;
    Printf.eprintf "  misc cone: %d cells, max-rank %d\n%!"
      cone_size.(misc_cone_idx) cone_max_rank.(misc_cone_idx)
  end;

  (* Cone offsets along the global rank axis.  Each cone takes
     (cone_max_rank + 1) rank slots; +1 spacer between cones gives a
     visible gap separating them. *)
  let cone_spacer = 1 in
  let cone_offset = Array.make (n_cones + 1) 0 in
  for i = 1 to n_cones do
    cone_offset.(i) <- cone_offset.(i-1) + cone_max_rank.(i-1) + 1 + cone_spacer
  done;

  let n_ranks = if n_cones = 0 then 1
                else cone_offset.(n_cones) - cone_spacer in

  let global_rank_of_cell cell =
    let ci = try Hashtbl.find cell_cone cell with Not_found -> misc_cone_idx in
    let pcr = try Hashtbl.find cell_per_cone_rank cell with Not_found -> 0 in
    cone_offset.(ci) + (cone_max_rank.(ci) - pcr) in

  let rank_map = List.fold_left (fun acc (i : Behavioral_ir.binstance) ->
    SMap.add i.inst_name (global_rank_of_cell i.inst_name) acc
  ) SMap.empty visible_insts in

  (* Predecessors / successors for barycenter. *)
  let preds : (string, string list) Hashtbl.t = Hashtbl.create 64 in
  let succs : (string, string list) Hashtbl.t = Hashtbl.create 64 in
  Hashtbl.iter (fun net drv ->
    let cs = try Hashtbl.find consumers_of_net net with Not_found -> [] in
    List.iter (fun c ->
      let p = try Hashtbl.find preds c with Not_found -> [] in
      Hashtbl.replace preds c (drv :: p);
      let s = try Hashtbl.find succs drv with Not_found -> [] in
      Hashtbl.replace succs drv (c :: s)
    ) cs
  ) driver_of_net;

  let by_rank = Array.make n_ranks [] in
  SMap.iter (fun n r ->
    if r >= 0 && r < n_ranks then by_rank.(r) <- n :: by_rank.(r)
  ) rank_map;
  for i = 0 to n_ranks - 1 do by_rank.(i) <- List.rev by_rank.(i) done;
  let by_rank = barycenter
    ~by_rank
    ~rank_of:(fun n -> try SMap.find n rank_map with Not_found -> 0)
    ~predecessors:(fun n -> try Hashtbl.find preds n with Not_found -> [])
    ~successors:(fun n -> try Hashtbl.find succs n with Not_found -> [])
  in

  (* 5. Logical placement: every cell lives at (rank, row_idx).  Pads
        get virtual ranks -1 (inputs) and n_ranks (outputs).            *)
  let n_rows = Array.fold_left
    (fun a lst -> max a (List.length lst)) 0 by_rank in
  let n_rows = max 1 n_rows in

  let cell_sym : (string, symbol) Hashtbl.t = Hashtbl.create 64 in
  let cell_rc  : (string, int * int) Hashtbl.t = Hashtbl.create 64 in
  Array.iteri (fun r names ->
    List.iteri (fun row_idx n ->
      Hashtbl.replace cell_rc n (r, row_idx);
      let sym = match List.find_opt
                        (fun (i : Behavioral_ir.binstance) -> i.inst_name = n)
                        m.instances with
        | Some i -> Hashtbl.find inst_sym i.inst_name
        | None -> auto_generate ~cell_name:n ~pins:[] in
      Hashtbl.replace cell_sym n sym
    ) names
  ) by_rank;

  (* Uniform cell footprint = bbox of widest/tallest in the module. *)
  let row_h, col_w =
    let mh = ref 40.0 and mw = ref 60.0 in
    Hashtbl.iter (fun _ (s : symbol) ->
      let (sx1, sy1, sx2, sy2) = s.sym_bbox in
      mh := max !mh (sy2 -. sy1);
      mw := max !mw (sx2 -. sx1)
    ) cell_sym;
    !mh, !mw in

  (* Pads as virtual cells.  Pad rows are distributed across the n_rows
     range so an input pad shows up near the rows that consume it.  *)
  let input_ports  = List.filter (fun (_, d) -> d = `Input)  module_ports in
  let output_ports = List.filter (fun (_, d) -> d = `Output) module_ports in
  let pad_row total i =
    if total <= 0 then 0
    else (i * n_rows) / total in
  let pad_rc : (string, int * int) Hashtbl.t = Hashtbl.create 16 in
  List.iteri (fun i (name, _) ->
    Hashtbl.replace pad_rc name (-1, pad_row (List.length input_ports) i)
  ) input_ports;
  List.iteri (fun i (name, _) ->
    Hashtbl.replace pad_rc name (n_ranks, pad_row (List.length output_ports) i)
  ) output_ports;

  (* For each (inst, port) pin: (rank, row, pin_off_x, pin_off_y, dir)
     where offsets are in cell-local (top-left) coords after centring
     the symbol in its row_h slot. *)
  let pin_info (inst, port) : (int * int * float * float * pin_dir) option =
    if inst = "" then
      match Hashtbl.find_opt pad_rc port with
      | Some (r, row) ->
          (* Pad pin: stub points inward from the pad position.        *)
          let dir = if r = -1 then PinOut else PinIn in
          let off_x = if r = -1 then col_w else 0.0 in
          Some (r, row, off_x, row_h /. 2.0, dir)
      | None -> None
    else
      match Hashtbl.find_opt cell_rc inst with
      | None -> None
      | Some (r, row) ->
          let sym = Hashtbl.find cell_sym inst in
          let (sx1, sy1, sx2, sy2) = sym.sym_bbox in
          (match List.find_opt (fun p -> p.pin_name = port) sym.sym_pins with
           | None -> None
           | Some p ->
               let (px, py) = p.pin_connect in
               let py' = sy2 -. (py -. sy1) in
               let off_x = px -. sx1
                           +. (col_w -. (sx2 -. sx1)) /. 2.0 in
               let off_y = py' +. (row_h -. (sy2 -. sy1)) /. 2.0 in
               Some (r, row, off_x, off_y, p.pin_dir))
  in

  (* 6. Per-net structural route in logical coordinates.

     Channels (horizontal): channel_idx i lies between row i and row i+1.
     i ranges from -1 (above row 0) to n_rows-1 (below last row).

     Column gaps (vertical): col_gap_idx g lies between rank g-1 and
     rank g.  g ranges from 0 (before rank 0) to n_ranks (after last).  *)

  let h_segs : seg_h list ref = ref [] in
  let v_segs : seg_v list ref = ref [] in
  let net_logicals : net_logical list ref = ref [] in
  let net_counter = ref 0 in

  let logical_for_pin inst port =
    pin_info (inst, port) in

  Hashtbl.iter (fun key eps ->
    let tie = Hashtbl.find_opt tie_consts (canon key) in
    if tie <> None then begin
      let nl = {
        nl_key = key; nl_idx = !net_counter; nl_eps = eps;
        nl_drv_inst = ""; nl_drv_port = "";
        nl_consumers = [];
        nl_trunk_gap = 0;
        nl_tie = tie } in
      incr net_counter;
      net_logicals := nl :: !net_logicals
    end else begin
      (* Find driver (PinOut) and consumers. *)
      let drv = List.find_opt (fun (inst, port, _) ->
        match logical_for_pin inst port with
        | Some (_, _, _, _, PinOut) -> true
        | _ -> false) eps in
      let driver = match drv with
        | Some d -> d
        | None ->
            (* No driver — treat the first endpoint as a notional
               source (e.g. a module input pad). *)
            (match eps with [] -> ("", "", PinInOut) | e :: _ -> e)
      in
      let (drv_inst, drv_port, _) = driver in
      let consumers = List.filter_map (fun (i, p, _) ->
        if i = drv_inst && p = drv_port then None else Some (i, p)) eps in
      if consumers = [] then ()  (* skip — nothing to route *)
      else begin
        (* Pick trunk column gap based on driver and consumers' ranks. *)
        let drv_rank = match logical_for_pin drv_inst drv_port with
          | Some (r, _, _, _, _) -> r
          | None -> 0 in
        let cons_ranks = List.filter_map (fun (i, p) ->
          match logical_for_pin i p with
          | Some (r, _, _, _, _) -> Some r
          | None -> None) consumers in
        let any_forward = List.exists (fun r -> r > drv_rank) cons_ranks in
        let trunk_gap =
          if any_forward then drv_rank + 1
          else drv_rank in
        let trunk_gap = max 0 (min n_ranks trunk_gap) in
        let nl = {
          nl_key = key; nl_idx = !net_counter; nl_eps = eps;
          nl_drv_inst = drv_inst; nl_drv_port = drv_port;
          nl_consumers = consumers; nl_trunk_gap = trunk_gap;
          nl_tie = None } in
        incr net_counter;
        net_logicals := nl :: !net_logicals;

        (* Emit logical segments for this net. *)
        let drv_log = logical_for_pin drv_inst drv_port in
        let cons_logs = List.map (fun (i, p) ->
          (i, p, logical_for_pin i p)) consumers in
        match drv_log with
        | None -> ()
        | Some (drv_r, drv_row, _drv_off_x, _drv_off_y, _) ->
            (* Driver's channel: the one below its row (row drv_row, ch
               drv_row).  Use ch=drv_row, which is between row drv_row
               and drv_row+1.  Edge case: if drv_row = n_rows-1, use
               that channel anyway (extends into bottom margin). *)
            let ch_d = drv_row in
            let drv_gap_right = drv_r + 1 in  (* col gap right of driver *)
            let drv_gap_left  = drv_r in
            let drv_exit_gap =
              if trunk_gap > drv_r then drv_gap_right
              else drv_gap_left in
            (* Driver horizontal in ch_d, from drv_exit_gap to trunk_gap. *)
            let g_lo = min drv_exit_gap trunk_gap in
            let g_hi = max drv_exit_gap trunk_gap in
            if g_lo <> g_hi then
              h_segs := {
                sh_net = nl.nl_idx; sh_ch = ch_d;
                sh_g0 = g_lo; sh_g1 = g_hi;
                sh_track = -1 } :: !h_segs;
            let cons_chs = List.filter_map (fun (_, _, lg) ->
              match lg with
              | Some (_, row, _, _, _) -> Some row
              | None -> None) cons_logs in
            let chs = ch_d :: cons_chs in
            let ch_min = List.fold_left min max_int chs in
            let ch_max = List.fold_left max min_int chs in
            if ch_min <> ch_max then
              v_segs := {
                sv_net = nl.nl_idx; sv_g = trunk_gap;
                sv_r0 = ch_min; sv_r1 = ch_max;
                sv_track = -1 } :: !v_segs;
            (* Per consumer: horizontal in ch_c from trunk_gap to consumer's
               adjacent column gap. *)
            List.iter (fun (_, _, lg) ->
              match lg with
              | None -> ()
              | Some (c_r, c_row, _, _, _) ->
                  let ch_c = c_row in
                  let cons_gap_left  = c_r in
                  let cons_gap_right = c_r + 1 in
                  let cons_entry_gap =
                    if trunk_gap < c_r then cons_gap_left
                    else cons_gap_right in
                  let g_lo = min trunk_gap cons_entry_gap in
                  let g_hi = max trunk_gap cons_entry_gap in
                  if g_lo <> g_hi then
                    h_segs := {
                      sh_net = nl.nl_idx; sh_ch = ch_c;
                      sh_g0 = g_lo; sh_g1 = g_hi;
                      sh_track = -1 } :: !h_segs
            ) cons_logs
      end
    end
  ) net_endpoints;

  (* 7. Track allocation per channel and per col-gap via greedy
        interval coloring.                                              *)
  let alloc_h_tracks () =
    (* Group segments by channel. *)
    let by_ch : (int, seg_h list) Hashtbl.t = Hashtbl.create 32 in
    List.iter (fun s ->
      let prev = try Hashtbl.find by_ch s.sh_ch with Not_found -> [] in
      Hashtbl.replace by_ch s.sh_ch (s :: prev)
    ) !h_segs;
    (* For each channel, sort segments by left edge then assign tracks. *)
    let max_tracks = Hashtbl.create 32 in
    Hashtbl.iter (fun ch lst ->
      let sorted = List.sort (fun a b -> compare a.sh_g0 b.sh_g0) lst in
      (* track_end.(i) = right gap of last segment on track i.
         A new segment with sh_g0 = X can reuse track i only when
         track_end.(i) < X — two segments sharing column gap X both
         occupy that gap, so a strict less-than is required. *)
      let track_end = ref [||] in
      List.iter (fun s ->
        let n = Array.length !track_end in
        let found = ref (-1) in
        let i = ref 0 in
        while !i < n && !found < 0 do
          if !track_end.(!i) < s.sh_g0 then found := !i;
          incr i
        done;
        let t =
          if !found >= 0 then !found
          else begin
            track_end := Array.append !track_end [| min_int |];
            n
          end in
        !track_end.(t) <- s.sh_g1;
        s.sh_track <- t
      ) sorted;
      Hashtbl.replace max_tracks ch (Array.length !track_end)
    ) by_ch;
    max_tracks in
  let alloc_v_tracks () =
    let by_g : (int, seg_v list) Hashtbl.t = Hashtbl.create 32 in
    List.iter (fun s ->
      let prev = try Hashtbl.find by_g s.sv_g with Not_found -> [] in
      Hashtbl.replace by_g s.sv_g (s :: prev)
    ) !v_segs;
    let max_tracks = Hashtbl.create 32 in
    Hashtbl.iter (fun g lst ->
      let sorted = List.sort (fun a b -> compare a.sv_r0 b.sv_r0) lst in
      let track_end = ref [||] in
      List.iter (fun s ->
        let n = Array.length !track_end in
        let found = ref (-1) in
        let i = ref 0 in
        while !i < n && !found < 0 do
          if !track_end.(!i) < s.sv_r0 then found := !i;
          incr i
        done;
        let t =
          if !found >= 0 then !found
          else begin
            track_end := Array.append !track_end [| min_int |];
            n
          end in
        !track_end.(t) <- s.sv_r1;
        s.sv_track <- t
      ) sorted;
      Hashtbl.replace max_tracks g (Array.length !track_end)
    ) by_g;
    max_tracks in
  let h_max_tracks = alloc_h_tracks () in
  let v_max_tracks = alloc_v_tracks () in

  (* 8. Physical sizing.  Channel and column-gap widths grow with track
        counts; the cell-content rows/columns remain fixed. *)
  let track_pitch  = 6.0 in
  let min_row_gap  = 18.0 in
  let gap_margin   = 6.0 in
  (* The leftmost ~10 px of every column gap is occupied by output pin
     stubs of the cells in the previous rank; the rightmost ~26 px (for
     up to 4 staggered input pins) is occupied by input pin stubs of
     the cells in the next rank.  Trunk tracks must stay clear of both
     zones, otherwise a vertical spine could end up at the same x as a
     pin stub and look like an overlap. *)
  let max_n_in =
    let m = ref 1 in
    Hashtbl.iter (fun _ (s : symbol) ->
      let n = List.length
                (List.filter (fun (p : pin) -> p.pin_dir = PinIn) s.sym_pins) in
      if n > !m then m := n
    ) cell_sym;
    !m in
  let output_reserve = _pin_stub_len +. 4.0 in
  let input_reserve  = _pin_stub_len
                       +. float_of_int (max 0 (max_n_in - 1)) *. _pin_stub_step
                       +. 4.0 in
  let min_col_gap = output_reserve +. input_reserve
                    +. 2.0 *. gap_margin +. track_pitch *. 2.0 in

  let row_gap_h ch_idx =
    let t = try Hashtbl.find h_max_tracks ch_idx with Not_found -> 0 in
    max min_row_gap (float_of_int t *. track_pitch +. 2.0 *. gap_margin) in
  let col_gap_w g =
    let t = try Hashtbl.find v_max_tracks g with Not_found -> 0 in
    max min_col_gap
      (output_reserve +. gap_margin
       +. float_of_int t *. track_pitch
       +. gap_margin +. input_reserve) in

  (* Row top y for row i.  Channels are between rows: channel i sits
     between row i and row i+1.  Channel -1 is above row 0. *)
  let row_top = Array.make (n_rows + 1) 0.0 in
  row_top.(0) <- module_margin +. row_gap_h (-1);
  for i = 1 to n_rows do
    row_top.(i) <- row_top.(i - 1) +. row_h +. row_gap_h (i - 1)
  done;
  let channel_y_top ch =
    if ch < 0 then module_margin
    else row_top.(ch) +. row_h in
  let channel_y ch track =
    channel_y_top ch +. gap_margin +. float_of_int track *. track_pitch in
  let canvas_h = row_top.(n_rows - 1) +. row_h +. row_gap_h (n_rows - 1) in

  (* Column-center x for rank r.  Column gaps between ranks indexed
     0..n_ranks (gap 0 is before rank 0, gap n_ranks after last). *)
  let col_left = Array.make (n_ranks + 1) 0.0 in
  col_left.(0) <- module_margin +. col_gap_w 0;
  for r = 1 to n_ranks do
    col_left.(r) <- col_left.(r - 1) +. col_w +. col_gap_w r
  done;
  let col_center r = col_left.(r) +. col_w /. 2.0 in
  let col_gap_x_left g =
    if g = 0 then module_margin
    else col_left.(g - 1) +. col_w in
  let col_gap_x_right g =
    col_gap_x_left g +. col_gap_w g in
  let col_gap_track_x g track =
    col_gap_x_left g +. output_reserve +. gap_margin
    +. float_of_int track *. track_pitch in
  let canvas_w =
    if n_ranks > 0
    then col_left.(n_ranks - 1) +. col_w +. col_gap_w n_ranks
    else module_margin *. 2.0 in

  (* 9. Place cells. *)
  let pi_tbl : (string, placed_inst) Hashtbl.t = Hashtbl.create 64 in
  Hashtbl.iter (fun n (r, row_idx) ->
    let sym = Hashtbl.find cell_sym n in
    let (sx1, sy1, sx2, sy2) = sym.sym_bbox in
    let sw = sx2 -. sx1 and sh = sy2 -. sy1 in
    let x = col_center r -. sw /. 2.0 in
    let y = row_top.(row_idx) +. (row_h -. sh) /. 2.0 in
    let pins = List.map (fun pin ->
      let (px, py) = pin.pin_connect in
      let py' = sy2 -. (py -. sy1) in
      let abs_x = x +. (px -. sx1) in
      let abs_y = y +. py' in
      { pp_inst = n; pp_pin = pin.pin_name;
        pp_pos = (abs_x, abs_y); pp_dir = pin.pin_dir }) sym.sym_pins in
    Hashtbl.replace pi_tbl n {
      pi_inst = n;
      pi_type = (match List.find_opt
                        (fun (i : Behavioral_ir.binstance) -> i.inst_name = n)
                        m.instances with
                 | Some i -> i.module_name
                 | None -> "?");
      pi_sym = sym; pi_xy = (x, y); pi_w = sw; pi_h = sh;
      pi_pins = pins }
  ) cell_rc;

  (* Module-port pads on the canvas edges. *)
  let port_pads = List.map (fun (name, dir) ->
    let (r, row_idx) = Hashtbl.find pad_rc name in
    let y = row_top.(row_idx) +. row_h /. 2.0 in
    let x =
      if r = -1 then module_margin
      else canvas_w -. module_margin in
    { pad_name = name; pad_dir = dir; pad_pos = (x, y) }
  ) module_ports in
  ignore port_pads;
  let port_pos_tbl : (string, float * float * pin_dir) Hashtbl.t =
    Hashtbl.create 32 in
  List.iter (fun pp ->
    let dir = match pp.pad_dir with
      | `Input -> PinOut | `Output -> PinIn in
    Hashtbl.replace port_pos_tbl pp.pad_name (fst pp.pad_pos, snd pp.pad_pos, dir)
  ) port_pads;

  (* 10. Emit physical polylines using assigned tracks. *)
  let pin_abs (inst, port) : (float * float * pin_dir) option =
    if inst = "" then Hashtbl.find_opt port_pos_tbl port
    else
      match Hashtbl.find_opt pi_tbl inst with
      | None -> None
      | Some pi ->
          match List.find_opt (fun pp -> pp.pp_pin = port) pi.pi_pins with
          | Some pp -> Some (fst pp.pp_pos, snd pp.pp_pos, pp.pp_dir)
          | None -> None in

  (* Build a map: net_idx → list of (channel, track) and (col_gap, track) *)
  let net_h_track : (int * int, int) Hashtbl.t = Hashtbl.create 256 in
  let net_v_track : (int * int, int) Hashtbl.t = Hashtbl.create 256 in
  List.iter (fun s ->
    Hashtbl.replace net_h_track (s.sh_net, s.sh_ch) s.sh_track
  ) !h_segs;
  List.iter (fun s ->
    Hashtbl.replace net_v_track (s.sv_net, s.sv_g) s.sv_track
  ) !v_segs;

  let nets = List.rev_map (fun (nl : net_logical) ->
    let placed = List.filter_map (fun (inst, port, _) ->
      match pin_abs (inst, port) with
      | Some (x, y, d) -> Some { pp_inst = inst; pp_pin = port;
                                  pp_pos = (x, y); pp_dir = d }
      | None -> None) nl.nl_eps in
    match nl.nl_tie with
    | Some _ ->
        { net_key = nl.nl_key; net_endpoints = placed;
          net_polyline = []; net_tie_const = nl.nl_tie }
    | None ->
        match pin_abs (nl.nl_drv_inst, nl.nl_drv_port) with
        | None ->
            { net_key = nl.nl_key; net_endpoints = placed;
              net_polyline = []; net_tie_const = None }
        | Some (dx, dy, _) ->
            let trunk_gap = nl.nl_trunk_gap in
            let trunk_track = try Hashtbl.find net_v_track (nl.nl_idx, trunk_gap)
                              with Not_found -> 0 in
            let trunk_x = col_gap_track_x trunk_gap trunk_track in
            let drv_rc = Hashtbl.find_opt cell_rc nl.nl_drv_inst in
            let drv_row = match drv_rc with
              | Some (_, row) -> row
              | None ->
                  (match Hashtbl.find_opt pad_rc nl.nl_drv_inst with
                   | Some (_, row) -> row | None -> 0) in
            let ch_d = drv_row in
            let h_track_drv = try Hashtbl.find net_h_track (nl.nl_idx, ch_d)
                              with Not_found -> 0 in
            let ch_d_y = channel_y ch_d h_track_drv in
            (* Driver jog: (dx, dy) → (dx, ch_d_y). *)
            let driver_jog =
              if abs_float (dy -. ch_d_y) < 0.5 then []
              else [ [ (dx, dy); (dx, ch_d_y) ] ] in
            let driver_horiz =
              if abs_float (dx -. trunk_x) < 0.5 then []
              else [ [ (dx, ch_d_y); (trunk_x, ch_d_y) ] ] in
            (* Spine vertical at trunk_x: from min channel y to max channel y. *)
            let consumer_segs = List.concat_map (fun (cinst, cport) ->
              match pin_abs (cinst, cport) with
              | None -> []
              | Some (cx, cy, _) ->
                  let c_row =
                    match Hashtbl.find_opt cell_rc cinst with
                    | Some (_, row) -> row
                    | None ->
                        (match Hashtbl.find_opt pad_rc cinst with
                         | Some (_, row) -> row | None -> 0) in
                  let ch_c = c_row in
                  let h_track_c = try Hashtbl.find net_h_track (nl.nl_idx, ch_c)
                                  with Not_found -> 0 in
                  let ch_c_y = channel_y ch_c h_track_c in
                  let spine_y0 = min ch_d_y ch_c_y in
                  let spine_y1 = max ch_d_y ch_c_y in
                  let spine_seg =
                    if abs_float (spine_y0 -. spine_y1) < 0.5 then []
                    else [ [ (trunk_x, spine_y0); (trunk_x, spine_y1) ] ] in
                  let horiz =
                    if abs_float (cx -. trunk_x) < 0.5 then []
                    else [ [ (trunk_x, ch_c_y); (cx, ch_c_y) ] ] in
                  let cons_jog =
                    if abs_float (cy -. ch_c_y) < 0.5 then []
                    else [ [ (cx, ch_c_y); (cx, cy) ] ] in
                  spine_seg @ horiz @ cons_jog
            ) nl.nl_consumers in
            { net_key = nl.nl_key; net_endpoints = placed;
              net_polyline = driver_jog @ driver_horiz @ consumer_segs;
              net_tie_const = None }
  ) !net_logicals in

  ignore col_gap_x_right;

  {
    sc_module = m.name;
    sc_insts  = Hashtbl.fold (fun _ v acc -> v :: acc) pi_tbl [];
    sc_ports  = port_pads;
    sc_nets   = nets;
    sc_width  = canvas_w;
    sc_height = canvas_h;
  }
