(* Topographical placer for packed LEF cells (Pack_to_lef output) onto the
   Virtex-7 floorplan (xilinx_lef/gen_floorplan.py).  Route-length aware and
   CARRY-CHAIN aware: a CI/CO-linked run of SLICE_CARRY cells is placed as a
   VERTICAL COLUMN (consecutive rows, one column) so the carry wires stay
   length-1 -- the structure Vivado gets and nextpnr's soup placer loses.

   Four placement engines, selectable by env TOPO_PLACE (default "sa"),
   in increasing order of sophistication -- each builds on the previous:

     greedy   : original one-pass constructive placer, WHOLE-device (no region
                confinement).  Cells drift across all rows -> ~2.5x Vivado area.
     region   : REGION CONFINEMENT.  Restrict the fabric to the K SLICE sites
                nearest the hard-block anchor centroid (K = n_slice / fill), so
                the design clusters into a compact Vivado-sized box instead of
                smearing.  Constructive placement inside that region.
     sa       : region + constructive seed + SIMULATED ANNEALING over the packed
                cells (HPWL cost, carry columns rigid/fixed, Metropolis moves &
                swaps within the region).  This is the quality push.
     analytic : region + QUADRATIC (analytic) placement -- solve the weighted-
                clique wirelength minimum by conjugate gradient with the fixed
                hard blocks / carry columns as anchors -- then legalise onto
                region sites, then SA polish.

   nextpnr legalises + routes afterwards from the per-primitive BEL stamps.
   The lef_def physical compiler (hpwl / placement_timing / predict_swap) can
   drive a later timing-aware refinement; this file keeps the HPWL cost
   self-contained (bbox half-perimeter) for speed. *)

module Y = Yojson.Safe
module U = Yojson.Safe.Util

let getenv_default k d = try Sys.getenv k with Not_found -> d
let getenv_float k d = try float_of_string (Sys.getenv k) with _ -> d
let getenv_int k d = try int_of_string (Sys.getenv k) with _ -> d

let contains s sub =
  let ls = String.length s and lsub = String.length sub in
  let rec go i = i + lsub <= ls && (String.sub s i lsub = sub || go (i + 1)) in
  lsub = 0 || go 0

(* LEF cell -> floorplan site kind. *)
let kind_of_lef = function
  | s when String.length s >= 5 && String.sub s 0 5 = "SLICE" -> "SLICE"
  | "RAMB36" -> "BRAM" | "RAMB18" -> "BRAM18" | "DSP48" -> "DSP"
  | "IOB" -> "IO" | "BUFG" -> "BUFG" | "BUFH" -> "BUFH" | "MMCM" -> "MMCM"
  | "GT" -> "GT"
  | _ -> "SLICE"

type site = { sname : string; mutable used : bool; sx : int; sy : int; sm : bool }

(* TOPO_SITE_PHYSMAP (name<TAB>slice_x<TAB>slice_y): override site coords with
   SLICE-grid-equivalent PHYSICAL positions.  Hard-block site indices are their
   OWN scale: RAMB36_X11Y68 is physically at slice grid ~X176Y353 (top-right
   corner), NOT near SLICE_X11Y68.  Comparing raw indices put the eth-arp
   TX/RX BRAM columns corner-to-corner from their MAC consumers: 7ns routes on
   falling-edge HALF-PERIOD (4ns @125MHz) BRAM paths, Vivado WNS -5.1,
   invisible to nextpnr's STA.  Map generated from the prjxray tilegrid:
   slice_x = SLICE_X of the nearest CLB column, slice_y = yconst - grid_y. *)
let site_physmap : (string, int * int) Hashtbl.t = Hashtbl.create 2048
let () = match Sys.getenv_opt "TOPO_SITE_PHYSMAP" with
  | None -> ()
  | Some f when Sys.file_exists f ->
    let ic = open_in f in
    (try while true do
         match String.split_on_char '\t' (input_line ic) with
         | [n; x; y] ->
           (try Hashtbl.replace site_physmap n (int_of_string x, int_of_string y)
            with _ -> ())
         | _ -> ()
       done with End_of_file -> close_in ic);
    Printf.eprintf "site physmap: %d overrides\n" (Hashtbl.length site_physmap)
  | Some f -> Printf.eprintf "site physmap: %s not found (ignored)\n" f

let load_floorplan path =
  let j = Y.from_file path in
  let by_kind : (string, site list ref) Hashtbl.t = Hashtbl.create 16 in
  List.iter (fun sj ->
      let k = U.member "kind" sj |> U.to_string in
      let sub = try U.member "sub" sj |> U.to_string with _ -> "" in
      let nm = U.member "name" sj |> U.to_string in
      let rx = U.member "x" sj |> U.to_int and ry = U.member "y" sj |> U.to_int in
      let px, py = match Hashtbl.find_opt site_physmap nm with
        | Some (x, y) -> x, y | None -> rx, ry in
      let s = { sname = nm; used = false; sm = (sub = "SLICEM"); sx = px; sy = py } in
      let l = try Hashtbl.find by_kind k with Not_found -> let l = ref [] in Hashtbl.add by_kind k l; l in
      l := s :: !l)
    (j |> U.member "sites" |> U.to_list);
  by_kind

let run_gen floorplan_json ~get_bmod ~get_j =
  let mode = getenv_default "TOPO_PLACE" "sa" in
  let fill = getenv_float "TOPO_REGION_FILL" 0.65 in
  let seed = getenv_int "TOPO_SEED" 1 in
  Random.init seed;
  let fp = load_floorplan floorplan_json in
  (* TOPO_RESERVED_SITES: a placement file (name<TAB>site<TAB>...) whose sites are
     occupied by a frozen macro -- mark them used so our placement avoids them. *)
  (match Sys.getenv_opt "TOPO_RESERVED_SITES" with
   | Some path ->
       let reserved = Hashtbl.create 4096 in
       (try let ic = open_in path in
          (try while true do
             match String.split_on_char '\t' (input_line ic) with
             | _ :: site :: _ -> Hashtbl.replace reserved site ()
             | _ -> ()
           done with End_of_file -> ()); close_in ic
        with Sys_error _ -> ());
       let n = ref 0 in
       Hashtbl.iter (fun _k lref -> List.iter (fun s ->
           if Hashtbl.mem reserved s.sname then (s.used <- true; incr n)) !lref) fp;
       Printf.eprintf "reserved %d sites (%d in floorplan) from %s\n"
         (Hashtbl.length reserved) !n path
   | None -> ());
  (* TOPO_MACRO_SOFT: a frozen macro's own BELs must be excluded -- a core cell
     cannot legally sit on one -- but the tiles AROUND them are a different
     question.  Their interconnect is heavily used by the macro's fixed pips,
     so a core cell placed among them is more likely to fail to route than the
     same cell elsewhere; that is a REASON TO PREFER elsewhere, not a reason to
     forbid.  Forbidding measured worse: reserving the bounding box put 1425
     perfectly good slices out of reach, squeezed the core into a narrow column
     and produced 105 unrouted arcs, against 4 when only the macro's own sites
     were taken.

     So: a cost, not a wall.  Each site within TOPO_MACRO_SOFT_R tiles of a
     macro cell costs TOPO_MACRO_SOFT_W extra, and the annealer spends that
     only where the wirelength it saves is worth more.  W=0 (default) is off. *)
  let macro_pen : (int * int, float) Hashtbl.t = Hashtbl.create 4096 in
  let macro_soft_w = getenv_float "TOPO_MACRO_SOFT_W" 0.0 in
  (match Sys.getenv_opt "TOPO_MACRO_SOFT" with
   | Some path when macro_soft_w > 0.0 ->
       let r = getenv_int "TOPO_MACRO_SOFT_R" 1 in
       let seeds = Hashtbl.create 1024 in
       (try let ic = open_in path in
          (try while true do
             let line = input_line ic in
             if String.length line > 0 && line.[0] <> '#' then
               match String.split_on_char '\t' line with
               | _ :: site :: _ ->
                   let site = match String.index_opt site '/' with
                     | Some i -> String.sub site 0 i | None -> site in
                   Hashtbl.replace seeds site ()
               | _ -> ()
           done with End_of_file -> ()); close_in ic
        with Sys_error _ -> ());
       (* the file names sites; the floorplan knows where they are *)
       let pts = ref [] in
       Hashtbl.iter (fun _k lref -> List.iter (fun s ->
           if Hashtbl.mem seeds s.sname then pts := (s.sx, s.sy) :: !pts) !lref) fp;
       (* dilate by r, cost falling off with distance so the edge is not a cliff *)
       List.iter (fun (x, y) ->
           for dx = -r to r do for dy = -r to r do
             let d = abs dx + abs dy in
             if d <= r then begin
               let k = (x + dx, y + dy) in
               let v = macro_soft_w *. (1.0 -. float d /. float (r + 1)) in
               let cur = try Hashtbl.find macro_pen k with Not_found -> 0.0 in
               if v > cur then Hashtbl.replace macro_pen k v
             end
           done done) !pts;
       Printf.eprintf
         "macro soft penalty: %d seed site(s), radius %d, weight %.2f -> %d penalised tile(s)\n"
         (List.length !pts) r macro_soft_w (Hashtbl.length macro_pen);
       if !pts = [] then
         Printf.eprintf
           "  WARNING: none of %s's sites are in the floorplan -- the penalty covers NOTHING\n"
           path
   | Some _ -> Printf.eprintf "TOPO_MACRO_SOFT set but TOPO_MACRO_SOFT_W is 0 -- penalty OFF\n"
   | None -> ());
  let macro_cost x y =
    if Hashtbl.length macro_pen = 0 then 0.0
    else try Hashtbl.find macro_pen (x, y) with Not_found -> 0.0 in

  (* TOPO_KEEPOUT_FROM: same kind of file, but reserve the whole BOUNDING BOX of
     the macro's sites rather than only the sites it happens to occupy.
     TOPO_RESERVED_SITES leaves the free slices INSIDE a frozen macro available,
     which looks like better density and is a trap: the macro's routing is
     frozen too, so core logic dropped between its cells competes for the very
     interconnect those fixed pips already own, and the router discovers it far
     too late.  A macro is a region, not a set of sites.

     TOPO_KEEPOUT_MARGIN (default 0) widens the box by N tiles if the frozen
     routing needs elbow room beyond the cells themselves. *)
  (match Sys.getenv_opt "TOPO_KEEPOUT_FROM" with
   | Some path ->
       let x0 = ref max_int and x1 = ref min_int in
       let y0 = ref max_int and y1 = ref min_int in
       let sites = Hashtbl.create 4096 in
       (try let ic = open_in path in
          (try while true do
             let line = input_line ic in
             if String.length line > 0 && line.[0] <> '#' then
               match String.split_on_char '\t' line with
               | _ :: site :: _ ->
                   (* accept both `SITE` and `SITE/BEL` *)
                   let site = match String.index_opt site '/' with
                     | Some i -> String.sub site 0 i
                     | None -> site in
                   Hashtbl.replace sites site ()
               | _ -> ()
           done with End_of_file -> ()); close_in ic
        with Sys_error _ -> ());
       (* the floorplan knows where each site is; the file only names them *)
       Hashtbl.iter (fun _k lref -> List.iter (fun s ->
           if Hashtbl.mem sites s.sname then begin
             if s.sx < !x0 then x0 := s.sx;
             if s.sx > !x1 then x1 := s.sx;
             if s.sy < !y0 then y0 := s.sy;
             if s.sy > !y1 then y1 := s.sy
           end) !lref) fp;
       if !x0 > !x1 then
         Printf.eprintf
           "TOPO_KEEPOUT_FROM %s: none of its %d site(s) are in the floorplan -- \
            NOTHING was reserved.  A keep-out that silently covers nothing is \
            worse than none: the build proceeds and the collision appears later \
            as a bind failure.\n"
           path (Hashtbl.length sites)
       else begin
         let m = getenv_int "TOPO_KEEPOUT_MARGIN" 0 in
         let x0 = !x0 - m and x1 = !x1 + m and y0 = !y0 - m and y1 = !y1 + m in
         let n = ref 0 and tot = ref 0 in
         Hashtbl.iter (fun _k lref -> List.iter (fun s ->
             incr tot;
             if s.sx >= x0 && s.sx <= x1 && s.sy >= y0 && s.sy <= y1
                && not s.used then (s.used <- true; incr n)) !lref) fp;
         Printf.eprintf
           "keep-out X%d..%d Y%d..%d (margin %d): %d site(s) of %d reserved for the \
            frozen macro (%d of them beyond the macro's own cells)\n"
           x0 x1 y0 y1 m (!n + Hashtbl.length sites) !tot !n
       end
   | None -> ());
  let bmod = get_bmod () in
  (* TOPO_EXCLUDE_SUBSTR: drop cells whose name contains this substring (used to
     hold a Vivado-frozen hard-IP macro, e.g. eth.eth_macro1., out of our
     topographical placement -- the macro is placed + routed separately). *)
  let bmod = match Sys.getenv_opt "TOPO_EXCLUDE_SUBSTR" with
    | Some sub ->
        let keep = List.filter (fun i -> not (contains i.Behavioral_ir.inst_name sub))
                     bmod.Behavioral_ir.instances in
        Printf.eprintf "TOPO_EXCLUDE_SUBSTR=%s: kept %d of %d instances\n"
          sub (List.length keep) (List.length bmod.Behavioral_ir.instances);
        { bmod with Behavioral_ir.instances = keep }
    | None -> bmod in
  let pr = Pack_to_lef.pack bmod in
  let cells = Array.of_list pr.Pack_to_lef.cells in
  let ncells = Array.length cells in
  let name2id = Hashtbl.create (ncells * 2) in
  Array.iteri (fun i c -> Hashtbl.replace name2id c.Pack_to_lef.pc_name i) cells;

  (* ---- per-cell state ---------------------------------------------------- *)
  let pos_x = Array.make ncells 0 and pos_y = Array.make ncells 0 in
  let cell_site : site option array = Array.make ncells None in
  let movable = Array.make ncells false in           (* SLICE non-carry cells *)
  let need_sm = Array.make ncells false in            (* must land on a SLICEM *)
  Array.iteri (fun i c ->
      let lef = c.Pack_to_lef.pc_lef in
      movable.(i) <- (kind_of_lef lef = "SLICE" && lef <> "SLICE_CARRY");
      need_sm.(i) <- (String.length lef >= 6 && String.sub lef 0 6 = "SLICEM")) cells;
  let skip = Array.make ncells false in
  (* SLICEM reservation for nextpnr's post-load distributed-RAM split-cells.
     RESERVE_SLICEM (legacy): keep general logic OUT of ALL SLICEM columns -- a
       big density/routability hit.
     RESERVE_SLICEM_N=<k> (minimal): keep just k WHOLE SLICEM slices EMPTY and
       let general logic use the rest of the SLICEM columns; the DRAM/SRL
       (need_sm) cells are then NOT placed here -- nextpnr packs its split
       RAMD32/RAMD64 sub-cells into the k reserved slices (+ the arch repair). *)
  let reserve_slicem_all = Sys.getenv_opt "RESERVE_SLICEM" <> None in
  let reserve_slicem_n =
    match Sys.getenv_opt "RESERVE_SLICEM_N" with
    | Some s -> (try int_of_string s with _ -> 0)
    | None -> 0 in
  if reserve_slicem_n > 0 then begin
    let cnt = ref 0 in
    Hashtbl.iter (fun _k lref -> List.iter (fun s ->
        if s.sm && not s.used && !cnt < reserve_slicem_n then (s.used <- true; incr cnt))
      !lref) fp;
    Array.iteri (fun i _ -> if need_sm.(i) then (skip.(i) <- true; movable.(i) <- false)) cells;
    Printf.eprintf "minimal SLICEM reserve: %d slices kept free (DRAM left to nextpnr)\n" !cnt
  end;
  let fits i s =
    if need_sm.(i) then s.sm
    else if reserve_slicem_all then not s.sm
    else true in
  let occ : (string, int) Hashtbl.t = Hashtbl.create (ncells * 2) in
  let bind i s = s.used <- true; cell_site.(i) <- Some s;
                 pos_x.(i) <- s.sx; pos_y.(i) <- s.sy; Hashtbl.replace occ s.sname i in
  let is_placed i = cell_site.(i) <> None in

  (* ---- TOPO_GUIDE: copy placement of corresponding cells from a reference ----
     build (guide file line = "cellname<TAB>BEL", BEL = SLICE_XxYy[/subbel]).
     Matched cells are pinned to the reference SITE and frozen (skipped by the
     constructive/SA engines).  TOPO_GUIDE_STRIP removes a hierarchy segment
     from the guide's names before matching, so a wrapper level that differs
     between designs (e.g. "eth_macro1.") does not block correspondence.
     "assuming the same hierarchy": names must be the SAME synthesis (yosys
     flatten of the identical netlist) -- a guide from a different synthesis of
     the same RTL will NOT match (cell names diverge). *)
  (match Sys.getenv_opt "TOPO_GUIDE" with
   | Some path ->
       let strip = getenv_default "TOPO_GUIDE_STRIP" "" in
       let remove_substr s sub =
         if sub = "" then s else begin
           let ls = String.length s and lb = String.length sub in
           let buf = Buffer.create ls and i = ref 0 in
           while !i < ls do
             if !i + lb <= ls && String.sub s !i lb = sub
             then i := !i + lb
             else (Buffer.add_char buf s.[!i]; incr i)
           done; Buffer.contents buf
         end in
       (* guide: name -> site (sub-bel dropped; nextpnr re-legalises within slice) *)
       let guide = Hashtbl.create 8192 in
       (try
          let ic = open_in path in
          (try while true do
             match String.split_on_char '\t' (input_line ic) with
             | name :: bel :: _ ->
                 let site = match String.index_opt bel '/' with
                   | Some k -> String.sub bel 0 k | None -> bel in
                 Hashtbl.replace guide (remove_substr name strip) site
             | _ -> ()
           done with End_of_file -> ()); close_in ic
        with Sys_error _ -> Printf.eprintf "TOPO_GUIDE: cannot read %s\n" path);
       let site_by_name : (string, site) Hashtbl.t = Hashtbl.create (ncells * 2) in
       Hashtbl.iter (fun _k lref ->
           List.iter (fun s -> Hashtbl.replace site_by_name s.sname s) !lref) fp;
       (* pc_name carries a packing suffix ("$carry"/"$mux"/"$DIgndx"...) that the
          guide (a per-primitive placement) lacks -- strip it before matching. *)
       let base n = match String.index_opt n '$' with
         | Some k -> String.sub n 0 k | None -> n in
       let ng = ref 0 and nbusy = ref 0 and nunfit = ref 0 in
       Array.iteri (fun i c ->
           if not skip.(i) && not (is_placed i) then
             match Hashtbl.find_opt guide (remove_substr (base c.Pack_to_lef.pc_name) strip) with
             | None -> ()
             | Some sn ->
                 (match Hashtbl.find_opt site_by_name sn with
                  | Some s when s.used -> incr nbusy
                  | Some s when not (fits i s) -> incr nunfit
                  | Some s -> bind i s; skip.(i) <- true; movable.(i) <- false; incr ng
                  | None -> ())) cells;
       Printf.eprintf "TOPO_GUIDE: pinned %d cells (%d site-busy, %d unfit) from %s (strip=%S, %d guide entries)\n"
         !ng !nbusy !nunfit path strip (Hashtbl.length guide)
   | None -> ());

  (* ---- nets (exclude clock: nets driven by BUFG/MMCM output) ------------- *)
  let clk_nets = Hashtbl.create 16 in
  Array.iter (fun c -> if c.Pack_to_lef.pc_lef = "BUFG" || c.Pack_to_lef.pc_lef = "MMCM" then
      List.iter (fun (p, nk) ->
          if p = "O" || (String.length p >= 6 && String.sub p 0 6 = "CLKOUT") then
            match nk with Pack_to_lef.Net _ -> Hashtbl.replace clk_nets nk () | _ -> ())
        c.Pack_to_lef.pc_conns) cells;
  let net_of_key : (Pack_to_lef.netkey, int) Hashtbl.t = Hashtbl.create (ncells * 2) in
  let net_lists : (int, int list ref) Hashtbl.t = Hashtbl.create (ncells * 2) in
  let nnets = ref 0 in
  Array.iteri (fun i c ->
      List.iter (fun (_p, nk) -> match nk with
          | Pack_to_lef.Net _ when not (Hashtbl.mem clk_nets nk) ->
              let nid = match Hashtbl.find_opt net_of_key nk with
                | Some n -> n
                | None -> let n = !nnets in incr nnets; Hashtbl.add net_of_key nk n;
                          Hashtbl.add net_lists n (ref []); n in
              let lst = Hashtbl.find net_lists nid in
              if not (List.mem i !lst) then lst := i :: !lst
          | _ -> ()) c.Pack_to_lef.pc_conns) cells;
  let net_cells = Array.make !nnets [||] in
  Hashtbl.iter (fun nid lst -> net_cells.(nid) <- Array.of_list !lst) net_lists;
  (* cell -> net ids *)
  let cell_nets = Array.make ncells [] in
  Array.iteri (fun nid cs -> Array.iter (fun i -> cell_nets.(i) <- nid :: cell_nets.(i)) cs) net_cells;
  (* ---- CLOCK-DOMAIN AFFINITY -------------------------------------------
     Clock nets are excluded from the wirelength cost above (a BUFG-driven net
     is un-shortenable and would swamp HPWL), so the annealer has NO notion of
     clock domains and interleaves them freely.  Measured on eth-arp: userclk2's
     1049 FFs and the CPU's 515 were both smeared over the SAME 76x76 tile box,
     which is why same-domain paths came out 0.9 ns logic / 10.6 ns routing.
     Pull each domain together with a centroid-attraction term.  The centroid is
     held FIXED between refreshes so a move costs O(1) -- adding the clock net to
     net_cells instead would make every move O(domain size) (~1000x). *)
  let dom_of_key : (Pack_to_lef.netkey, int) Hashtbl.t = Hashtbl.create 16 in
  let ndom = ref 0 in
  let cell_dom = Array.make ncells (-1) in
  Array.iteri (fun i c ->
      if c.Pack_to_lef.pc_lef <> "BUFG" && c.Pack_to_lef.pc_lef <> "MMCM" then
        List.iter (fun (_p, nk) -> match nk with
          | Pack_to_lef.Net _ when Hashtbl.mem clk_nets nk && cell_dom.(i) < 0 ->
            let d = match Hashtbl.find_opt dom_of_key nk with
              | Some d -> d
              | None -> let d = !ndom in incr ndom; Hashtbl.add dom_of_key nk d; d in
            cell_dom.(i) <- d
          | _ -> ()) c.Pack_to_lef.pc_conns) cells;
  (* name of each domain's clock net, for TOPO_DOM_PRIO matching *)
  let dom_name = Array.make (max 1 !ndom) "" in
  Hashtbl.iter (fun nk d -> match nk with
    | Pack_to_lef.Net (nm, _) -> if d < Array.length dom_name then dom_name.(d) <- nm
    | _ -> ()) dom_of_key;
  (* Spread the domain tag from the registers to the COMBINATIONAL cells between
     them, by NEAREST-REGISTER breadth-first search from every tagged cell at
     once.  A logic cone belongs to the domain of the registers it sits between.
     The previous majority-vote-over-neighbours rule was a popularity contest,
     not a cone: a LUT between two eth registers but touching several cpu-domain
     cells came out cpu.  It also only spread from already-tagged cells over a
     few passes, so coverage DECAYED with distance from the registers -- worst
     for the domain with the deepest logic.  Measured on eth-arp it captured 61%
     of the cpu cone but only 38% of userclk2's (819 of ~2180 cells), so zone
     sizing undercounted userclk2 2.7x and the partition SPLIT the very domain it
     was meant to compact.  A true backward cone is near-unambiguous here (2695 of
     2713 comb cells lie in exactly one domain's cone, 18 shared), so nearest-
     register BFS is an accurate stand-in and needs no port-direction data.
     High-fanout nets are skipped: a 72-sink clock enable would otherwise bridge
     every domain it touches. *)
  let () =
    let maxfan = getenv_int "TOPO_DOM_MAXFAN" 16 in
    let q = Queue.create () in
    Array.iteri (fun i d -> if d >= 0 then Queue.add i q) cell_dom;
    while not (Queue.is_empty q) do
      let i = Queue.pop q in
      let d = cell_dom.(i) in
      List.iter (fun nid ->
          let cs = net_cells.(nid) in
          if Array.length cs <= maxfan then
            Array.iter (fun j ->
                if cell_dom.(j) < 0 then begin cell_dom.(j) <- d; Queue.add j q end) cs)
        cell_nets.(i)
    done in
  let dom_w = try float_of_string (Sys.getenv "TOPO_DOM_W") with _ -> 0.0 in
  let dom_cx = Array.make (max 1 !ndom) 0.0 and dom_cy = Array.make (max 1 !ndom) 0.0 in
  let refresh_dom () =
    if dom_w > 0.0 && !ndom > 0 then begin
      let sx = Array.make !ndom 0.0 and sy = Array.make !ndom 0.0
      and n = Array.make !ndom 0 in
      Array.iteri (fun i d ->
          if d >= 0 && is_placed i then begin
            sx.(d) <- sx.(d) +. float pos_x.(i);
            sy.(d) <- sy.(d) +. float pos_y.(i);
            n.(d) <- n.(d) + 1
          end) cell_dom;
      for d = 0 to !ndom - 1 do
        if n.(d) > 0 then begin
          dom_cx.(d) <- sx.(d) /. float n.(d); dom_cy.(d) <- sy.(d) /. float n.(d)
        end
      done
    end in
  (* attraction cost of cell i sitting at (x,y), 0 when the term is off *)
  let dom_cost i x y =
    if dom_w <= 0.0 then 0.0 else
    let d = cell_dom.(i) in
    if d < 0 then 0.0
    else dom_w *. (abs_float (float x -. dom_cx.(d)) +. abs_float (float y -. dom_cy.(d))) in
  (* TIMING-DRIVEN PLACEMENT: per-net criticality weight (default 1.0).  Loaded
     from PLACE_CRIT_FILE (nextpnr NEXTPNR_CRIT_EXPORT: "<driver-cell>\t<crit>").
     eval_delta minimises SUM (net_w.(n) * hpwl n) so the SA annealer keeps near-
     critical nets short.  Keyed by DRIVER-CELL name: net names don't survive
     bir_to_nextpnr_json flattening, but cell names do.  A nextpnr cell name lacks
     place_lef's packing suffix ($carry/$mux/...), so match full pc_name then the
     base (strip at '$'); a base may fracture into several packed cells. *)
  let net_w = Array.make !nnets 1.0 in
  (match Sys.getenv_opt "PLACE_CRIT_FILE" with
   | Some cf when Sys.file_exists cf ->
     (* Recover the pre-packing cell name by removing a KNOWN packer suffix from
        the END.  Truncating at the FIRST '$' is wrong for yosys-generated names,
        which BEGIN with one -- `$abc$29188$...$29189$logic` collapsed to the
        empty string, so only the handful of cells whose pc_name happened to be
        unsuffixed ever matched (119 of 4968 on eth-arp).  pack_to_lef appends
        these; keep in sync with it. *)
     let strip n = match String.rindex_opt n '$' with
       | Some i ->
         let suf = String.sub n (i + 1) (String.length n - i - 1) in
         if List.mem suf [ "carry"; "mux"; "site"; "hard"; "m"; "logic"; "ff" ]
         then String.sub n 0 i else n
       | None -> n in
     let base2ids = Hashtbl.create (ncells * 2) in
     let add_base b i =
       Hashtbl.replace base2ids b (i :: (try Hashtbl.find base2ids b with Not_found -> [])) in
     Array.iteri (fun i c ->
       add_base (strip c.Pack_to_lef.pc_name) i;
       (* A packed cell ABSORBS several original primitives; only the base one is
          recoverable from pc_name, so index every original too.  pc_bels stamps
          each at its slot, and its fst IS the pre-packing inst_name -- the exact
          space nextpnr keys criticality by.  Without this, LUTs/FFs folded into
          a SLICE_CARRY or SLICE_LOGIC are invisible to the annealer (matching
          went 1661 -> 4029 of 4968 on eth-arp). *)
       List.iter (fun (orig, _) -> add_base orig i) c.Pack_to_lef.pc_bels) cells;
     let k = try float_of_string (Sys.getenv "PLACE_CRIT_K") with _ -> 8.0 in
     (* CONCENTRATING the weight, not just raising it.  nextpnr exports every
        cell with crit>0.05 -- 8686 of them here -- and their criticality is
        mostly middling: median 0.327, p90 0.697, only 2.0% at >=0.9.  Under the
        linear law w = 1 + K*crit the median weighted net already gets 3.6x at
        K=8 while the worst gets 9.0x, so the genuinely critical 2% are a mere
        2.5x more attractive to the annealer than the median.  Raising K alone
        does not fix that: it scales both ends and the RATIO stays 2.5.

        PLACE_CRIT_P applies w = 1 + K*crit^P, which is what actually separates
        them -- at P=3 the median falls to 1.28x while the worst stays 9.0x, a
        7x spread.  PLACE_CRIT_MIN additionally ignores everything below a
        cutoff, so near-idle nets stop competing for the annealer's attention.
        Defaults P=1.0 / MIN=0.0 reproduce the previous behaviour exactly. *)
     let p = try float_of_string (Sys.getenv "PLACE_CRIT_P") with _ -> 1.0 in
     let cmin = try float_of_string (Sys.getenv "PLACE_CRIT_MIN") with _ -> 0.0 in
     let ic = open_in cf in
     let matched = ref 0 and lines = ref 0 in
     (try while true do
        (match String.split_on_char '\t' (input_line ic) with
         | cname :: v :: _ ->
           incr lines;
           let c = try float_of_string v with _ -> 0.0 in
           if c > 0.0 then begin
             let ids = match Hashtbl.find_opt name2id cname with
               | Some i -> [ i ]
               | None ->
                 (match Hashtbl.find_opt base2ids cname with
                  | Some l -> l
                  | None ->
                    (* nextpnr names some drivers by PIN ("<cell>/DP" on a
                       dist-RAM); fall back to the cell part. *)
                    (match String.rindex_opt cname '/' with
                     | Some i ->
                       (try Hashtbl.find base2ids (String.sub cname 0 i)
                        with Not_found -> [])
                     | None -> [])) in
             if ids <> [] then incr matched;
             let w = if c < cmin then 1.0
                     else 1.0 +. k *. (if p = 1.0 then c else Float.pow c p) in
             List.iter (fun idx ->
               List.iter (fun nid -> if net_w.(nid) < w then net_w.(nid) <- w)
                 cell_nets.(idx)) ids
           end
         | _ -> ())
       done with End_of_file -> ()); close_in ic;
     let wmx = Array.fold_left (fun a w -> Stdlib.max a w) 1.0 net_w in
     let nw = Array.fold_left (fun a w -> if w > 1.0 then a + 1 else a) 0 net_w in
     Printf.eprintf
       "[place_lef] timing-driven: %d/%d crit cells matched (K=%.1f P=%.1f MIN=%.2f)         -> %d net(s) weighted, max %.1fx\n%!"
       !matched !lines k p cmin nw wmx
   | _ -> ());

  (* SR CO-LOCATION: yosys maps async set/reset to native FF PRE/CLR pins fed by
     PER-FF inverters (abc replicates them, fanout-1).  If the placer strands that
     inverter columns from its FF, the SLICE's shared SR input can't route (86/92
     skips on the yosys eth-arp netlist).  Weight each LOW-FANOUT net on an FF SR
     pin (PRE/CLR/S/R) so the annealer keeps the SR driver adjacent to its FF ->
     the SR reaches the slice on local interconnect.  Enable with PLACE_SR_K>0.
     NOTE: OFF by default -- soft HPWL weighting proved INSUFFICIENT on the yosys
     eth-arp netlist (failing SR nets stayed ~11 tiles apart; a fanout-1 SR pair is
     out-voted by the FF's data net + the inverter's reset-source net, and even a
     distance-1 SR arc failed to route).  Kept as an option for hard-colocation
     experiments; the robust fix is to avoid native FF SR at synthesis. *)
  let sr_k = try float_of_string (Sys.getenv "PLACE_SR_K") with _ -> 0.0 in
  if sr_k > 0.0 then begin
    let sr_ports = [ "PRE"; "CLR"; "S"; "R"; "SR" ] in
    let w = 1.0 +. sr_k and srn = ref 0 in
    Array.iter (fun c ->
      List.iter (fun (port, nk) ->
        if List.mem port sr_ports then
          match Hashtbl.find_opt net_of_key nk with
          | Some nid when Array.length net_cells.(nid) <= 5 && net_w.(nid) < w ->
            net_w.(nid) <- w; incr srn
          | _ -> ()) c.Pack_to_lef.pc_conns) cells;
    Printf.eprintf "[place_lef] SR co-location: weighted %d low-fanout set/reset nets (K=%.1f)\n%!"
      !srn sr_k
  end;

  (* HPWL of a net over currently-placed cells (bbox half-perimeter). *)
  let net_hpwl nid =
    let cs = net_cells.(nid) in
    let mnx = ref max_int and mxx = ref min_int and mny = ref max_int and mxy = ref min_int
    and cnt = ref 0 in
    Array.iter (fun i -> if is_placed i then begin
        incr cnt;
        if pos_x.(i) < !mnx then mnx := pos_x.(i);
        if pos_x.(i) > !mxx then mxx := pos_x.(i);
        if pos_y.(i) < !mny then mny := pos_y.(i);
        if pos_y.(i) > !mxy then mxy := pos_y.(i) end) cs;
    if !cnt < 2 then 0 else (!mxx - !mnx) + (!mxy - !mny) in
  let total_hpwl () =
    let t = ref 0 and mx = ref 0 in
    for n = 0 to !nnets - 1 do let h = net_hpwl n in t := !t + h; if h > !mx then mx := h done;
    (!t, !mx) in

  (* ======================================================================= *)
  (* Phase A: dedicated / hard blocks (non-SLICE) -> greedy nearest, become   *)
  (*          the FIXED anchors for region + analytic.                        *)
  (* ======================================================================= *)
  let nearest_free_of kind (tx, ty) =
    match Hashtbl.find_opt fp kind with
    | None -> None
    | Some l ->
        List.fold_left (fun best s ->
            if s.used then best else
            let d = abs (s.sx - tx) + abs (s.sy - ty) in
            match best with Some (bd, _) when bd <= d -> best | _ -> Some (d, s)) None !l
        |> Option.map snd in
  (* Group hard blocks (BRAM/DSP) by parent macro so BRAMs that share high-fanout
     control (en/we/addr) land in CONTIGUOUS sites -- otherwise a scattered BRAM
     group makes those broadcast nets span the fabric and they fail to route.
     Place each group's first member near the anchor, each subsequent member at
     the nearest free site to the PREVIOUS member of the same group. *)
  let group_key name =
    try String.sub name 0 (Str.search_forward (Str.regexp "\\.genblk\\|\\.ram_reg\\|\\[[0-9]") name 0)
    with Not_found -> name in
  (* HARD-BLOCK SITE IMPORT.  site_import (TOPO_PLACE=site) only covers SLICE
     kinds, and it runs far later than this -- by then BRAM/DSP are already
     bound here and count as placed, so an imported placement kept OUR BRAM
     positions (measured: 34 RAMB36E1 at e.g. RAMB36_X7Y19 where Vivado had
     X13Y7).  Wide BRAM buses make that a real routing difference, so honour the
     map here too; anything the map does not name falls through to the existing
     group-contiguity heuristic below. *)
  let site_by_name : (string, site) Hashtbl.t = Hashtbl.create 8192 in
  Hashtbl.iter (fun _k lref ->
      List.iter (fun s -> if not (Hashtbl.mem site_by_name s.sname) then
                    Hashtbl.replace site_by_name s.sname s) !lref) fp;
  let site_want : (string, string) Hashtbl.t = Hashtbl.create 8192 in
  (match Sys.getenv_opt "TOPO_SITE_IN" with
   | Some path when Sys.file_exists path ->
     (try
        let ic = open_in path in
        (try while true do
             match String.split_on_char '\t' (input_line ic) with
             | nm :: sb :: _ ->
               let nm = String.trim nm and sb = String.trim sb in
               let site = match String.index_opt sb '/' with
                 | Some i -> String.sub sb 0 i | None -> sb in
               Hashtbl.replace site_want nm site;
               (match String.rindex_opt nm '/' with
                | Some i ->
                  let p = String.sub nm 0 i in
                  if p <> "" && not (Hashtbl.mem site_want p) then Hashtbl.add site_want p site
                | None -> ())
             | _ -> ()
           done with End_of_file -> ()); close_in ic
      with Sys_error _ -> ())
   | _ -> ());
  let want_site_of (c : Pack_to_lef.packed_cell) =
    List.fold_left (fun acc (prim, _) ->
        match acc with Some _ -> acc | None -> Hashtbl.find_opt site_want prim)
      None c.Pack_to_lef.pc_bels in
  let hard = ref [] in
  Array.iteri (fun i c -> if kind_of_lef c.Pack_to_lef.pc_lef <> "SLICE" then hard := (i, c) :: !hard) cells;
  let hard = List.sort (fun (_, a) (_, b) ->
      let ka = kind_of_lef a.Pack_to_lef.pc_lef and kb = kind_of_lef b.Pack_to_lef.pc_lef in
      compare (ka, group_key a.Pack_to_lef.pc_name) (kb, group_key b.Pack_to_lef.pc_name)) !hard in
  let last_pos = Hashtbl.create 16 in
  let hard_imported = ref 0 in
  List.iter (fun (i, c) ->
      let kind = kind_of_lef c.Pack_to_lef.pc_lef in
      let gk = kind ^ ":" ^ group_key c.Pack_to_lef.pc_name in
      let tgt = match Hashtbl.find_opt last_pos gk with Some p -> p | None -> (110, 100) in
      let imported = match want_site_of c with
        | Some sn -> (match Hashtbl.find_opt site_by_name sn with
            | Some s when not s.used -> incr hard_imported; Some s
            | _ -> None)
        | None -> None in
      match (match imported with Some s -> Some s | None -> nearest_free_of kind tgt) with
      | Some s -> bind i s; Hashtbl.replace last_pos gk (s.sx, s.sy)
      | None -> Printf.eprintf "no free %s site for %s\n" kind c.Pack_to_lef.pc_name) hard;
  if !hard_imported > 0 then
    Printf.eprintf "[place_lef] SITE-IN: %d hard blocks (BRAM/DSP/...) placed from the map\n%!"
      !hard_imported;
  (* anchor centroid = TOPO_ANCHOR_X/Y override (used to cluster the fabric next
     to a frozen macro's user-facing edge), else mean of placed hard blocks. *)
  let anchor_cx, anchor_cy =
    match Sys.getenv_opt "TOPO_ANCHOR_X", Sys.getenv_opt "TOPO_ANCHOR_Y" with
    | Some a, Some b -> (int_of_string a, int_of_string b)
    | _ ->
        let sx = ref 0 and sy = ref 0 and n = ref 0 in
        Array.iteri (fun i c -> if kind_of_lef c.Pack_to_lef.pc_lef <> "SLICE" && is_placed i
                      then (sx := !sx + pos_x.(i); sy := !sy + pos_y.(i); incr n)) cells;
        if !n = 0 then (110, 100) else (!sx / !n, !sy / !n) in

  (* ======================================================================= *)
  (* Region: the K free SLICE sites nearest the anchor centroid.              *)
  (* greedy mode uses the WHOLE device (no confinement).                      *)
  (* ======================================================================= *)
  let n_slice = Array.fold_left (fun a c ->
      if kind_of_lef c.Pack_to_lef.pc_lef = "SLICE" then a + 1 else a) 0 cells in
  let all_slices = match Hashtbl.find_opt fp "SLICE" with Some l -> !l | None -> [] in
  (* DRAM SPREADING: the annealer packs all need_sm (RAM64M/RAM32M) cells into the
     2 SLICEM columns nearest the anchor (shared-address HPWL), and the address
     logic then jams that corner to ~90%+ -> unroutable.  Pre-place the DRAM
     round-robin across several SLICEM columns near the anchor (bounded per
     column) and FIX them, so their address logic spreads with them. *)
  (match Sys.getenv_opt "TOPO_DRAM_SPREAD" with
   | None -> ()
   | Some _ ->
     let percol = getenv_int "TOPO_DRAM_PERCOL" 2 in
     let dram = ref [] in
     Array.iteri (fun i _ -> if need_sm.(i) && not (is_placed i) then dram := i :: !dram) cells;
     let sm_arr = Array.of_list
         (List.sort (fun a b ->
              compare (abs (a.sx - anchor_cx) + abs (a.sy - anchor_cy))
                      (abs (b.sx - anchor_cx) + abs (b.sy - anchor_cy)))
            (List.filter (fun s -> s.sm && not s.used) all_slices)) in
     let colcount = Hashtbl.create 32 in
     let nplaced = ref 0 in
     List.iter (fun i ->
         let rec pick k =
           if k >= Array.length sm_arr then None
           else let s = sm_arr.(k) in
             if s.used then pick (k + 1)
             else let c = try Hashtbl.find colcount s.sx with Not_found -> 0 in
               if c >= percol then pick (k + 1) else Some s in
         match pick 0 with
         | Some s ->
             bind i s; skip.(i) <- true; movable.(i) <- false; incr nplaced;
             Hashtbl.replace colcount s.sx ((try Hashtbl.find colcount s.sx with Not_found -> 0) + 1)
         | None -> ()) (List.rev !dram);
     Printf.eprintf "DRAM spread: fixed %d DRAM across SLICEM cols (<=%d/col)\n" !nplaced percol);
  (* REGION SHAPE.  "K nearest sites to the anchor" -- the METRIC picks the
     shape, and the SA only ever moves/swaps within the region, so this is the
     final outline of the design.  Ranking by L1 (|dx|+|dy|) yields a DIAMOND:
     it minimises the worst-case Manhattan span, but it wastes half its
     bounding box on the four corner triangles (an L1 ball of radius R holds
     2R^2 sites inside a 2Rx2R box), so it reaches ~41% further along each axis
     than the rectangle holding the same K.  The SA cost here IS bbox
     half-perimeter, so the diamond fights its own objective; worse, column x
     of a diamond offers only 2(R-|dx|) rows, tapering to ~1 at the tips, which
     is too few consecutive rows for the CARRY4 columns to legalise into.
     Default to a RECTANGLE (Chebyshev), with L1 as tiebreak so the outermost
     ring -- where the K cut lands mid-ring -- fills evenly instead of raggedly.
     Measured on the eth-arp floorplan (K=7737 of 75900 SLICE sites):
       L1 diamond   bbox 128x133 (45% used), min 1 row/col
       rect asp 1.0 bbox  95x 95 (86% used), min 11 rows/col
       rect asp 2.0 bbox  67x135 (86% used), min 33 rows/col
     TOPO_REGION_ASPECT weights dx, so the region is 2R/aspect wide by 2R tall:
     1.0 is square in SLICE-index units, 0.5 square in CLB-TILE counts (SLICE_X
     advances 2 per CLB column), 2.0 roughly square in microns.  Which one wins
     is a routing-delay question, not a geometric one -- sweep it against Fmax.
     TOPO_REGION_SHAPE=diamond restores the old L1 ball. *)
  let region_shape  = getenv_default "TOPO_REGION_SHAPE" "rect" in
  let region_aspect = getenv_float "TOPO_REGION_ASPECT" 1.0 in
  let region_rank s =
    let dx = float_of_int (abs (s.sx - anchor_cx)) *. region_aspect
    and dy = float_of_int (abs (s.sy - anchor_cy)) in
    if region_shape = "diamond" then (dx +. dy, 0.0)
    else (Stdlib.max dx dy, dx +. dy) in
  let region_sites =
    if mode = "greedy" then all_slices
    else begin
      let k = int_of_float (ceil (float n_slice /. fill)) in
      (* only free sites are eligible (reserved macro sites are already used) *)
      let arr = Array.of_list (List.filter (fun s -> not s.used) all_slices) in
      Array.sort (fun a b -> compare (region_rank a) (region_rank b)) arr;
      let k = min k (Array.length arr) in
      Array.to_list (Array.sub arr 0 k)
    end in
  let region_arr = Array.of_list region_sites in
  (* ---- PRIORITY DOMAIN ZONING (TOPO_DOM_ZONE=1) --------------------------
     Give the CRITICAL clock domain its own contiguous block of sites first and
     let the slack domains take what is left.  region_arr is already ordered
     nearest-anchor-first, so a prefix is the most compact space available.
     Rationale: on eth-arp the 125 MHz userclk2 domain and the 50 MHz cpu domain
     are fully interleaved across the same 76x76 tile area, while cpu closes 56
     against a 50 MHz target -- it can afford to be scattered, userclk2 cannot.
     A SOFT centroid pull (TOPO_DOM_W) failed because it fought the wirelength
     term; a HARD partition instead hands the good contiguous space to the domain
     that needs it, which is the shape Vivado's placement has.
     TOPO_DOM_PRIO="sub1,sub2" ranks domains by clock-net-name substring (first =
     highest); unlisted domains follow, largest first. *)
  let zone_on = getenv_int "TOPO_DOM_ZONE" 0 <> 0 in
  let zone_arr = Array.make (max 1 !ndom) [||] in
  let site_zone : (string, int) Hashtbl.t = Hashtbl.create 4096 in
  let () =
    if zone_on && !ndom > 0 && region_arr <> [||] then begin
      let cnt = Array.make !ndom 0 in
      Array.iteri (fun i d -> if d >= 0 && movable.(i) then cnt.(d) <- cnt.(d) + 1) cell_dom;
      let prio = match Sys.getenv_opt "TOPO_DOM_PRIO" with
        | Some s -> String.split_on_char ',' s | None -> [] in
      let rank d =
        let rec find k = function
          | [] -> 1000
          | sub :: tl ->
            let nm = dom_name.(d) in
            let hit =
              let ls = String.length sub and ln = String.length nm in
              ls > 0 && (let rec go j = j + ls <= ln && (String.sub nm j ls = sub || go (j+1)) in go 0) in
            if hit then k else find (k+1) tl in
        find 0 prio in
      let order = Array.init !ndom (fun d -> d) in
      Array.sort (fun a b ->
          let ra = rank a and rb = rank b in
          if ra <> rb then compare ra rb else compare cnt.(b) cnt.(a)) order;
      let pos = ref 0 and total = Array.length region_arr in
      Array.iter (fun d ->
          if cnt.(d) > 0 && !pos < total then begin
            let want = int_of_float (ceil (float cnt.(d) /. fill)) in
            let take = min want (total - !pos) in
            zone_arr.(d) <- Array.sub region_arr !pos take;
            Array.iter (fun s -> Hashtbl.replace site_zone s.sname d) zone_arr.(d);
            Printf.eprintf "[place_lef] zone: domain '%s' cells=%d sites=%d (rank %d)\n"
              dom_name.(d) cnt.(d) take (rank d);
            pos := !pos + take
          end) order;
      Printf.eprintf "[place_lef] zoning: %d/%d region sites allocated to %d domain(s)\n%!"
        !pos total !ndom
    end in
  (* ---- DEF EXPORT for OpenROAD (TOPO_DEF_OUT) ----------------------------
     nextpnr's HeAP hangs on this design and its SA produces invalid placements,
     so the only untried analytic placer is OpenROAD's RePlAce/OpenDP.  The LEF
     model already exists (xilinx_lef/virtex7.tech.lef + virtex7_cells.lef, one
     MACRO per pc_lef on a 1x1 SITE grid), so all that is missing is the DEF.
     Pins carry no geometry inside a 1x1 cell, so a connection is bound to the
     next free pin of the right macro -- which pin is irrelevant to placement,
     only the connectivity matters.  DATABASE MICRONS 2000 and SITE 1.0 => 2000
     DB units per site. *)
  let emit_def () = match Sys.getenv_opt "TOPO_DEF_OUT" with
    | None -> ()
    | Some defp ->
      let def_timing = getenv_int "TOPO_DEF_TIMING" 0 <> 0 in
      (* TOPO_DEF_ONLY_DOM=<clock-net substring>: emit ONLY that domain's cells
         and the nets AMONG them -- everything else is OMITTED, not fixed.  With
         the rest merely FIXED, OpenROAD optimises the critical domain against a
         backdrop of place_lef SA positions we know are ~2.2x off Vivado, and it
         has almost no freedom (measured: HPWL moved 3%, and the result was worse
         than letting it place everything).  Omitting them lets it place the
         domain on its own merits; place_lef then fits the rest around it. *)
      let only_dom = Sys.getenv_opt "TOPO_DEF_ONLY_DOM" in
      (* TOPO_DEF_ONLY_PREFIX=<cell-name prefix>: isolate a HIERARCHY instead of a
         clock domain -- e.g. "eth." hands OpenROAD the whole Ethernet IP core
         (PCS/PMA + MAC + FIFOs) as one unit.  Same rationale as ONLY_DOM: give
         it the design's internal nets and full freedom, then let place_lef fit
         the remainder around the frozen result. *)
      let only_pre = Sys.getenv_opt "TOPO_DEF_ONLY_PREFIX" in
      (* CONTAINMENT, not prefix: yosys's `flatten` tags a module's internal
         cells as "$flatten\eth.$abc$..." -- they carry the hierarchy without
         starting with it.  Matching on prefix finds only 23% of the eth core;
         matching on containment finds 90%. *)
      let has_prefix s pre =
        let ls = String.length pre and ln = String.length s in
        ls > 0 && (let rec go j = j + ls <= ln
                                  && (String.sub s j ls = pre || go (j + 1)) in go 0) in
      let in_dom i =
        match only_pre with
        | Some pre -> has_prefix cells.(i).Pack_to_lef.pc_name pre
        | None ->
        match only_dom with
        | None -> true
        | Some subs ->
          (* comma-separated list: the whole eth IP core is the UNION of its
             clock domains (userclk2 + rxrecclk + userclk + gtrefclk).  Selecting
             by hierarchy does not work -- yosys flatten+abc strips it, leaving
             only 23% of cells with an "eth." prefix. *)
          let d = cell_dom.(i) in
          d >= 0 &&
          (let nm = dom_name.(d) in
           List.exists (fun sub ->
               let ls = String.length sub and ln = String.length nm in
               ls > 0 && (let rec go j = j + ls <= ln
                                         && (String.sub nm j ls = sub || go (j + 1)) in go 0))
             (String.split_on_char ',' subs)) in
      let lefp = getenv_default "TOPO_LEF_CELLS"
          (Filename.concat (Filename.dirname Sys.executable_name) "virtex7_cells.lef") in
      (* macro -> ordered pin list, straight out of the LEF *)
      let macro_pins : (string, string list ref) Hashtbl.t = Hashtbl.create 32 in
      (* pins split by DIRECTION so a connection can be bound to a pin of the
         RIGHT CLASS.  Binding an output net to an input pin is invisible to a
         wirelength placer but produces a nonsense timing graph, so it matters
         as soon as OpenROAD runs -timing_driven. *)
      let macro_in : (string, string list ref) Hashtbl.t = Hashtbl.create 32 in
      let macro_out : (string, string list ref) Hashtbl.t = Hashtbl.create 32 in
      let macro_clk : (string, string) Hashtbl.t = Hashtbl.create 32 in
      (try
         let ic = open_in lefp in
         let cur = ref "" and lastpin = ref "" in
         (try while true do
            let l = String.trim (input_line ic) in
            if String.length l > 6 && String.sub l 0 6 = "MACRO " then begin
              cur := String.sub l 6 (String.length l - 6);
              Hashtbl.replace macro_pins !cur (ref []);
              Hashtbl.replace macro_in !cur (ref []);
              Hashtbl.replace macro_out !cur (ref [])
            end else if String.length l > 4 && String.sub l 0 4 = "PIN " && !cur <> "" then begin
              lastpin := String.sub l 4 (String.length l - 4);
              (match Hashtbl.find_opt macro_pins !cur with
               | Some r -> r := !lastpin :: !r | None -> ());
              let p = !lastpin in
              let isclk = p = "CLK" || (String.length p >= 4 && String.sub p 0 4 = "CLKA")
                          || (String.length p >= 4 && String.sub p 0 4 = "CLKB") in
              if isclk && not (Hashtbl.mem macro_clk !cur) then Hashtbl.replace macro_clk !cur p
            end else if String.length l > 10 && String.sub l 0 10 = "DIRECTION " && !cur <> "" then begin
              let d = String.trim (String.sub l 10 (String.length l - 10)) in
              let d = if String.length d > 0 && d.[String.length d - 1] = ';'
                      then String.trim (String.sub d 0 (String.length d - 1)) else d in
              let tbl = if d = "OUTPUT" then macro_out else macro_in in
              if not (Hashtbl.mem macro_clk !cur && Hashtbl.find macro_clk !cur = !lastpin) then
                (match Hashtbl.find_opt tbl !cur with
                 | Some r -> r := !lastpin :: !r | None -> ())
            end
          done with End_of_file -> ()); close_in ic
       with _ -> Printf.eprintf "[place_lef] DEF: cannot read %s\n" lefp);
      Hashtbl.iter (fun _ r -> r := List.rev !r) macro_pins;
      Hashtbl.iter (fun _ r -> r := List.rev !r) macro_in;
      Hashtbl.iter (fun _ r -> r := List.rev !r) macro_out;
      (* classify a pc_conns port: pc_conns is a MIX of LEF pin names (explicitly
         packed slices) and original primitive ports (the generic fall-through). *)
      let is_clk_port p = p = "CLK" || p = "C" || p = "WCLK"
                          || (String.length p >= 4 && String.sub p 0 4 = "CLKA")
                          || (String.length p >= 4 && String.sub p 0 4 = "CLKB") in
      let is_out_port p =
        p = "O" || p = "Q" || p = "CO" || p = "O6" || p = "O5" || p = "MC31"
        || (String.length p = 2 && (p.[1] = 'O' || p.[1] = 'Q')
            && p.[0] >= 'A' && p.[0] <= 'D')
        || (String.length p >= 2 && p.[0] = 'Q'
            && p.[1] >= '0' && p.[1] <= '9')
        || (String.length p >= 2 && String.sub p 0 2 = "DO") in
      (* Die = the REGION place_lef actually places into, expanded to contain the
         FIXED macros -- NOT the whole device.  Giving OpenROAD the full 222x350
         die let it spread the logic over 77700 positions at the requested
         density while place_lef confines placement to ~5k sites, so importing
         that placement cost a 16-site average snap and destroyed it. *)
      let sx_min = ref max_int and sy_min = ref max_int
      and sx_max = ref min_int and sy_max = ref min_int in
      Array.iter (fun s ->
          if s.sx < !sx_min then sx_min := s.sx;
          if s.sx > !sx_max then sx_max := s.sx;
          if s.sy < !sy_min then sy_min := s.sy;
          if s.sy > !sy_max then sy_max := s.sy) region_arr;
      Array.iteri (fun i _ ->
          if not movable.(i) && is_placed i then begin
            if pos_x.(i) < !sx_min then sx_min := pos_x.(i);
            if pos_x.(i) > !sx_max then sx_max := pos_x.(i);
            if pos_y.(i) < !sy_min then sy_min := pos_y.(i);
            if pos_y.(i) > !sy_max then sy_max := pos_y.(i)
          end) cells;
      let sx_min = !sx_min and sy_min = !sy_min
      and sx_max = !sx_max and sy_max = !sy_max in
      let u = 2000 in
      let oc = open_out defp in
      Printf.fprintf oc "VERSION 5.8 ;\nDIVIDERCHAR \"/\" ;\nBUSBITCHARS \"[]\" ;\n";
      Printf.fprintf oc "DESIGN top ;\nUNITS DISTANCE MICRONS %d ;\n" u;
      Printf.fprintf oc "DIEAREA ( 0 0 ) ( %d %d ) ;\n"
        ((sx_max - sx_min + 1) * u) ((sy_max - sy_min + 1) * u);
      for y = sy_min to sy_max do
        Printf.fprintf oc "ROW ROW_%d SLICE 0 %d N DO %d BY 1 STEP %d 0 ;\n"
          (y - sy_min) ((y - sy_min) * u) (sx_max - sx_min + 1) u
      done;
      (* ---- PLACEMENT BLOCKAGES over every position the region does NOT own ---
         Without these OpenROAD treats the whole die rectangle as placeable and
         optimises onto sites place_lef cannot legalise onto, so the import has
         to drag cells back (measured 16.0 sites average with the full die, still
         10.5 with the region bbox) -- which is exactly the quality being thrown
         away.  Blocking the non-region / macro-occupied positions makes
         OpenROAD's placement legal BY CONSTRUCTION.  Runs are merged
         horizontally to keep the DEF small. *)
      let avail = Hashtbl.create 16384 in
      Array.iter (fun s -> Hashtbl.replace avail (s.sx, s.sy) true) region_arr;
      (* a site holding a cell we export FIXED is not available to OpenROAD *)
      Array.iteri (fun i c ->
          let is_carry = (let m = c.Pack_to_lef.pc_lef in
                          String.length m >= 11 && String.sub m 0 11 = "SLICE_CARRY") in
          if (not movable.(i) || is_carry) && is_placed i then
            Hashtbl.remove avail (pos_x.(i), pos_y.(i))) cells;
      let blk = Buffer.create (1 lsl 16) in
      let nblk = ref 0 in
      for y = sy_min to sy_max do
        let x = ref sx_min in
        while !x <= sx_max do
          if Hashtbl.mem avail (!x, y) then incr x
          else begin
            let x0 = !x in
            while !x <= sx_max && not (Hashtbl.mem avail (!x, y)) do incr x done;
            Buffer.add_string blk
              (Printf.sprintf "    - PLACEMENT RECT ( %d %d ) ( %d %d ) ;\n"
                 ((x0 - sx_min) * u) ((y - sy_min) * u)
                 ((!x - sx_min) * u) ((y - sy_min + 1) * u));
            incr nblk
          end
        done
      done;
      if !nblk > 0 then begin
        Printf.fprintf oc "BLOCKAGES %d ;\n" !nblk;
        Buffer.output_buffer oc blk;
        Printf.fprintf oc "END BLOCKAGES\n"
      end;
      Printf.eprintf "[place_lef] DEF: %d placement blockage rect(s), %d sites left free\n"
        !nblk (Hashtbl.length avail);
      (* ---- TOP-LEVEL PINS -----------------------------------------------
         Derived from the IOB cells, which carry the port name directly
         ($iopadmap$top.<port>$site) and are already placed.  Without a PINS
         section `get_ports` resolves to nothing, so the real XDC's
         `create_clock ... [get_ports clk_p]` cannot be used at all.
         NOTE this does NOT replace the BUFG-output SDC for the internal
         domains: userclk/userclk2/rxrecclk are generated through the GT and the
         PCS MMCM, and neither has a timing model here, so a clock defined at
         clk_p does not propagate to them. *)
      let pinbuf = Buffer.create 4096 in
      let npin = ref 0 in
      if def_timing then begin
        let strip_port n =
          let pre = "$iopadmap$top." in
          let n = if String.length n > String.length pre
                     && String.sub n 0 (String.length pre) = pre
                  then String.sub n (String.length pre)
                         (String.length n - String.length pre) else n in
          match String.rindex_opt n '$' with
          | Some i -> String.sub n 0 i | None -> n in
        Array.iteri (fun i c ->
            if c.Pack_to_lef.pc_lef = "IOB" && is_placed i then begin
              let port = strip_port c.Pack_to_lef.pc_name in
              (* the NETS section names data nets "n<nid>", so a pin must use the
                 SAME name -- referencing "<netname>_<bit>" creates a dangling
                 net and OpenSTA reports Pi-model NaNs *)
              let net = List.fold_left (fun acc (_, nk) -> match acc with
                  | Some _ -> acc
                  | None -> (match Hashtbl.find_opt net_of_key nk with
                      | Some n -> Some (Printf.sprintf "n%d" n)
                      | None -> None)) None c.Pack_to_lef.pc_conns in
              match net with
              | Some nn2 ->
                incr npin;
                Buffer.add_string pinbuf
                  (Printf.sprintf
                     "- %s + NET %s + DIRECTION INPUT + USE SIGNAL\n    + LAYER metal1 ( -100 -100 ) ( 100 100 ) + FIXED ( %d %d ) N ;\n"
                     port nn2 ((pos_x.(i) - sx_min) * u) ((pos_y.(i) - sy_min) * u))
              | None -> ()
            end) cells;
        if !npin > 0 then begin
          Printf.fprintf oc "PINS %d ;\n" !npin;
          Buffer.output_buffer oc pinbuf;
          Printf.fprintf oc "END PINS\n"
        end;
        Printf.eprintf "[place_lef] DEF: %d top-level pin(s) at their IOB sites\n" !npin
      end;
      (* components: pre-placed hard blocks are FIXED, the rest are free *)
      let n_emit = ref 0 in
      Array.iteri (fun i _ -> if in_dom i then incr n_emit) cells;
      Printf.fprintf oc "COMPONENTS %d ;\n" !n_emit;
      Array.iteri (fun i c ->
          if not (in_dom i) then () else
          let mac = c.Pack_to_lef.pc_lef in
          let mac = if Hashtbl.mem macro_pins mac then mac else "SLICE_LOGIC" in
          (* Carry chains stay where place_lef put them: nextpnr requires a
             CARRY4's S/DI feeders in its OWN slice, so letting OpenROAD move
             them apart produces an unroutable design (measured: cross-slice
             ACY0_OUT / MC31 arcs).  TOPO_DEF_FREE_CARRY=1 to override. *)
          let fix_carry = getenv_int "TOPO_DEF_FREE_CARRY" 0 = 0
                          && (let m = c.Pack_to_lef.pc_lef in
                              String.length m >= 11 && String.sub m 0 11 = "SLICE_CARRY") in
          (* TOPO_DEF_FREE_DOM=<clock-net substring>: free ONLY that clock
             domain's cells and FIX everything else at place_lef's positions, so
             OpenROAD optimises the CRITICAL domain in the context of the rest
             rather than trading it off against the whole design. *)
          let dom_ok =
            match Sys.getenv_opt "TOPO_DEF_FREE_DOM" with
            | None -> true
            | Some sub ->
              let d = cell_dom.(i) in
              d >= 0 &&
              (let nm = dom_name.(d) in
               let ls = String.length sub and ln = String.length nm in
               ls > 0 && (let rec go j = j + ls <= ln
                                         && (String.sub nm j ls = sub || go (j + 1)) in go 0)) in
          if movable.(i) && not fix_carry && dom_ok then
            Printf.fprintf oc "- %s %s ;\n" c.Pack_to_lef.pc_name mac
          else begin
            let x = (pos_x.(i) - sx_min) * u and y = (pos_y.(i) - sy_min) * u in
            Printf.fprintf oc "- %s %s + FIXED ( %d %d ) N ;\n" c.Pack_to_lef.pc_name mac x y
          end) cells;
      Printf.fprintf oc "END COMPONENTS\n";
      (* nets: bind each cell's connection to the next free pin of its macro *)
      (* SEPARATE pin counters per class: one shared counter indexes past the end
         of the (much shorter) output list once a cell has taken many input pins,
         which silently DROPPED that cell's driver connection and left 281 nets
         driverless -> STA-1040 NaNs. *)
      let used = Array.make ncells 0 in
      let used_out = Array.make ncells 0 in
      let buf = Buffer.create (1 lsl 20) in
      let nn = ref 0 and dropped = ref 0 and dup = ref 0 in
      (* CRITICALITY -> NET WEIGHT.  OpenROAD's gpl multiplies a GNet's timing
         weight by a customWeight_, but NOTHING ever calls setCustomWeight and
         gpl never reads dbNet::getWeight, so a DEF "+ WEIGHT" is parsed and then
         ignored.  Emulate weight portably by emitting a critical net as PARALLEL
         DUPLICATES: any placer that sums per-net HPWL then pays k times for it,
         which is exactly what place_lef's net_w does in its own SA.  Weight comes
         from net_w (1 + PLACE_CRIT_K * crit), rescaled to 1..1+TOPO_DEF_WREP
         copies.  Pins are plentiful (2589 cells expose ~90k terminals, only ~14k
         used), and a duplicate that cannot find a free pin is simply skipped. *)
      let wrep = getenv_int "TOPO_DEF_WREP" 2 in
      let wmax = Array.fold_left (fun a w -> Stdlib.max a w) 1.0 net_w in
      let reps nid =
        if wrep <= 0 || wmax <= 1.0 then 1
        else 1 + int_of_float (Float.round
               (float wrep *. (net_w.(nid) -. 1.0) /. (wmax -. 1.0))) in
      Array.iteri (fun nid cs0 ->
          let cs = if only_dom = None && only_pre = None then cs0
                   else Array.of_list (List.filter in_dom (Array.to_list cs0)) in
          (* In timing mode a net must have >=2 pins to carry a parasitic at all;
             OpenSTA aborts with STA-1040 (Pi model NaNs) on degenerate nets. *)
          if Array.length cs >= 2 then begin
            let r = reps nid in
            if r > 1 then dup := !dup + (r - 1);
            for copy = 0 to r - 1 do
              incr nn;
              Buffer.add_string buf
                (if copy = 0 then Printf.sprintf "- n%d" nid
                 else Printf.sprintf "- n%d_w%d" nid copy);
              Array.iter (fun i ->
                  let mac = cells.(i).Pack_to_lef.pc_lef in
                  (* TOPO_DEF_TIMING: bind by DIRECTION -- the driver of this net
                     gets an OUTPUT pin, the sinks get INPUT pins.  Required for
                     STA; irrelevant to a pure wirelength placer, so the old
                     next-free-pin behaviour stays the default. *)
                  let pool =
                    if not def_timing then
                      (match Hashtbl.find_opt macro_pins mac with
                       | Some r -> !r
                       | None -> (match Hashtbl.find_opt macro_pins "SLICE_LOGIC" with
                           | Some r -> !r | None -> []))
                    else begin
                      (* Exactly ONE driver per net.  is_out_port misses output
                         ports on some macro types, which left 515 of 3766 nets
                         driverless -> OpenSTA cannot build a Pi model for a net
                         with no driver ("[ERROR STA-1040] parasitic Pi model has
                         NaNs").  So: prefer a cell that genuinely claims an
                         output port, but if none does, designate the first cell
                         on the net as the driver. *)
                      let claims j =
                        List.exists (fun (p, nk) ->
                            is_out_port p &&
                            (match Hashtbl.find_opt net_of_key nk with
                             | Some n -> n = nid | None -> false))
                          cells.(j).Pack_to_lef.pc_conns in
                      let drv_idx =
                        let found = ref (-1) in
                        Array.iter (fun j -> if !found < 0 && claims j then found := j) cs;
                        if !found >= 0 then !found
                        else if Array.length cs > 0 then cs.(0) else (-1) in
                      let drives = (i = drv_idx) in
                      let tbl = if drives then macro_out else macro_in in
                      match Hashtbl.find_opt tbl mac with
                      | Some r when !r <> [] -> !r
                      | _ -> (match Hashtbl.find_opt tbl "SLICE_LOGIC" with
                          | Some r -> !r | None -> [])
                    end in
                  let out_pool = def_timing &&
                    (match Hashtbl.find_opt macro_out mac with
                     | Some r -> List.length !r > 0 && (try List.nth !r 0 = List.nth pool 0
                                                        with _ -> false)
                     | None -> false) in
                  let k = if out_pool then used_out.(i) else used.(i) in
                  if k < List.length pool then begin
                    (if out_pool then used_out.(i) <- k + 1 else used.(i) <- k + 1);
                    Buffer.add_string buf
                      (Printf.sprintf " ( %s %s )" cells.(i).Pack_to_lef.pc_name (List.nth pool k))
                  end else incr dropped) cs;
              Buffer.add_string buf " ;\n"
            done
          end) net_cells;
      if !dup > 0 then
        Printf.eprintf "[place_lef] DEF: %d weight-duplicate net(s) (max %d copies), %d pin-starved conns\n"
          !dup (1 + wrep) !dropped;
      (* CLOCK NETS.  place_lef excludes BUFG/MMCM-driven nets from its cost model,
         so they never reach net_cells -- but without them OpenSTA has no clock,
         no endpoints and therefore no slack, and -timing_driven is inert.
         Rebuild them here straight from pc_conns' clock ports. *)
      let clkbuf = Buffer.create 4096 in
      let nclk = ref 0 in
      let clk_src : (string, string) Hashtbl.t = Hashtbl.create 8 in
      if def_timing then begin
        let byclk : (Pack_to_lef.netkey, int list ref) Hashtbl.t = Hashtbl.create 16 in
        Array.iteri (fun i c ->
            List.iter (fun (p, nk) ->
                if is_clk_port p then match nk with
                  | Pack_to_lef.Net _ ->
                    let l = (try Hashtbl.find byclk nk with Not_found ->
                        let r = ref [] in Hashtbl.add byclk nk r; r) in
                    if not (List.mem i !l) then l := i :: !l
                  | _ -> ()) c.Pack_to_lef.pc_conns) cells;
        Hashtbl.iter (fun nk l ->
            if List.length !l >= 1 then begin
              incr nclk;
              let nm = match nk with Pack_to_lef.Net (s, b) -> Printf.sprintf "%s_%d" s b
                                   | _ -> Printf.sprintf "clk%d" !nclk in
              (* DEF syntax: the connection list comes FIRST, then "+ USE CLOCK" *)
              Buffer.add_string clkbuf (Printf.sprintf "- %s" nm);
              (* include the DRIVER (BUFG/MMCM) so create_clock has a source pin;
                 without it OpenSTA sees a driverless net and there is no clock *)
              Array.iteri (fun j c ->
                  List.iter (fun (p, nk2) ->
                      if nk2 = nk && is_out_port p then
                        match Hashtbl.find_opt macro_out c.Pack_to_lef.pc_lef with
                        | Some r when !r <> [] ->
                          Buffer.add_string clkbuf
                            (Printf.sprintf " ( %s %s )" c.Pack_to_lef.pc_name (List.hd !r));
                          Hashtbl.replace clk_src nm
                            (Printf.sprintf "%s/%s" c.Pack_to_lef.pc_name (List.hd !r))
                        | _ -> ()) c.Pack_to_lef.pc_conns) cells;
              List.iter (fun i ->
                  let mac = cells.(i).Pack_to_lef.pc_lef in
                  match Hashtbl.find_opt macro_clk mac with
                  | Some cp -> Buffer.add_string clkbuf
                                 (Printf.sprintf " ( %s %s )" cells.(i).Pack_to_lef.pc_name cp)
                  | None -> ()) !l;
              Buffer.add_string clkbuf " + USE CLOCK ;\n"
            end) byclk;
        Printf.eprintf "[place_lef] DEF: %d clock net(s) for STA\n" !nclk;
        (* SDC: constrain each clock at its REAL target.  cpu_clk is 50 MHz
           (200 MHz sysclk / MMCM 5/20); everything else is the 125 MHz eth side. *)
        (match Sys.getenv_opt "TOPO_SDC_OUT" with
         | None -> ()
         | Some sdcp ->
           let so = open_out sdcp in
           let cpu_bit = getenv_int "TOPO_SDC_CPU_BIT" (-1) in
           Hashtbl.iter (fun nm src ->
               let per =
                 if cpu_bit >= 0 &&
                    (let s = Printf.sprintf "n%d_" cpu_bit in
                     String.length nm >= String.length s
                     && String.sub nm 0 (String.length s) = s)
                 then 20.0 else 8.0 in
               Printf.fprintf so "create_clock -name %s -period %.3f [get_pins {%s}]\n"
                 nm per src) clk_src;
           close_out so;
           Printf.eprintf "[place_lef] SDC: %d clock(s) -> %s\n" (Hashtbl.length clk_src) sdcp)
      end;
      Printf.fprintf oc "NETS %d ;\n" (!nn + !nclk);
      Buffer.output_buffer oc buf;
      Buffer.output_buffer oc clkbuf;
      Printf.fprintf oc "END NETS\nEND DESIGN\n";
      close_out oc;
      Printf.eprintf "[place_lef] DEF: %d cells, %d nets, die %dx%d sites -> %s\n%!"
        ncells !nn (sx_max - sx_min + 1) (sy_max - sy_min + 1) defp
  in
  (* region column index for carry-run search *)
  let slice_by_col : (int, site array) Hashtbl.t = Hashtbl.create 256 in
  let cols = Hashtbl.create 256 in
  List.iter (fun s ->
      let c = try Hashtbl.find cols s.sx with Not_found -> let r = ref [] in Hashtbl.add cols s.sx r; r in
      c := s :: !c) region_sites;
  Hashtbl.iter (fun c r ->
      let a = Array.of_list !r in
      Array.sort (fun p q -> compare p.sy q.sy) a;
      Hashtbl.replace slice_by_col c a) cols;
  (* region centroid (analytic spring target) *)
  let region_cx, region_cy =
    if region_arr = [||] then (anchor_cx, anchor_cy)
    else (Array.fold_left (fun a s -> a + s.sx) 0 region_arr / Array.length region_arr,
          Array.fold_left (fun a s -> a + s.sy) 0 region_arr / Array.length region_arr) in

  (* ======================================================================= *)
  (* Phase B: carry chains -> vertical columns within the region (rigid).     *)
  (* ======================================================================= *)
  let co2cell = Hashtbl.create 32 in
  Array.iter (fun c -> if c.Pack_to_lef.pc_lef = "SLICE_CARRY" then
      match List.assoc_opt "CO" c.Pack_to_lef.pc_conns with
      | Some nk -> Hashtbl.replace co2cell nk c.Pack_to_lef.pc_name | None -> ()) cells;
  let carry_cells = List.filter (fun c -> c.Pack_to_lef.pc_lef = "SLICE_CARRY") (Array.to_list cells) in
  let is_root c = match List.assoc_opt "CI" c.Pack_to_lef.pc_conns with
    | Some nk -> not (Hashtbl.mem co2cell nk) | None -> true in
  let next_of c = match List.assoc_opt "CO" c.Pack_to_lef.pc_conns with
    | Some co -> List.find_opt (fun c2 -> List.assoc_opt "CI" c2.Pack_to_lef.pc_conns = Some co) carry_cells
    | None -> None in
  (* With an EXACT imported placement (TOPO_PLACE=site + TOPO_SITE_IN) the carry
     chains are already placed -- legally and vertically -- by the tool we are
     importing from, so this phase must not re-place them.  It otherwise runs
     FIRST and binds every carry cell into its own column, after which
     site_import sees them as already placed and skips: measured 843 of 968
     cells imported exactly while the 125 carry slices (780 primitives) sat
     where phase B had put them.
     Site-guided packing also renames pins to <BEL>_<port>, so the "CI"/"CO"
     lookups above find nothing and each carry cell looks like a singleton
     chain -- the chain recognition is meaningless here in any case. *)
  let site_place =
    Sys.getenv_opt "TOPO_PLACE" = Some "site" && Sys.getenv_opt "TOPO_SITE_IN" <> None in
  let chains = ref [] in
  if not site_place then
    List.iter (fun c -> if is_root c then begin
        let rec walk c acc = match next_of c with Some n -> walk n (n :: acc) | None -> List.rev acc in
        chains := (c :: walk c []) :: !chains end) carry_cells;
  (* find the first free consecutive vertical run of `len` slices in column arr *)
  let run_in_col arr len =
    let n = Array.length arr and res = ref None and i = ref 0 in
    while !res = None && !i + len <= n do
      let ok = ref true and consec = ref true in
      for k = 0 to len - 1 do
        if arr.(!i + k).used then ok := false;
        if k > 0 && arr.(!i + k).sy <> arr.(!i + k - 1).sy + 1 then consec := false
      done;
      if !ok && !consec then res := Some (Array.sub arr !i len);
      incr i
    done;
    !res in
  (* TOPO_CARRY_SPREAD: load-balance carry chains across columns instead of
     first-fit (which fills each column to ~95% before moving on -> all 285
     carries land in 3 columns whose D-output (DMUX) switchbox routing then
     saturates: 128 overused DMUX wires, unroutable).  Pick the column with a
     free run that currently holds the FEWEST used slices, so carries (and the
     carry_stamp LUTs that fill their slices) stay sparse per column.  Gated off
     by default so the silicon-validated pinned placement is unchanged. *)
  let carry_spread =
    match Sys.getenv_opt "TOPO_CARRY_SPREAD" with Some v -> v <> "0" && v <> "" | None -> false in
  (* Density CAP (TOPO_CARRY_MAX_PER_COL): fill columns in X order up to this many
     carry slices, then move to the next.  First-fit in column order keeps carries
     in a CONTIGUOUS band (short register-to-register datapaths -> timing) while
     the cap keeps each column below the D-output (DMUX) switchbox-congestion
     threshold that a ~full column hits.  Maximal spread (least-used column)
     cleared congestion but SCATTERED carries over ~70 columns -> the 125 MHz eth
     datapath fell to ~27 MHz.  Default cap gives a handful of contiguous columns. *)
  let carry_max_per_col =
    match Sys.getenv_opt "TOPO_CARRY_MAX_PER_COL" with
    | Some v -> (try int_of_string v with _ -> 32)
    | None -> 32 in
  let sorted_cols =
    lazy (List.sort compare (Hashtbl.fold (fun c _ acc -> c :: acc) slice_by_col [])) in
  let col_used c =
    Array.fold_left (fun a s -> if s.used then a + 1 else a) 0 (Hashtbl.find slice_by_col c) in
  let find_col_run len =
    if not carry_spread then begin
      let res = ref None in
      Hashtbl.iter (fun _c arr -> if !res = None then res := run_in_col arr len) slice_by_col;
      !res
    end else begin
      (* first column (by X) still under the density cap with a free run *)
      let res = ref None in
      List.iter (fun c ->
          if !res = None && col_used c < carry_max_per_col then
            match run_in_col (Hashtbl.find slice_by_col c) len with
            | Some run -> res := Some run
            | None -> ()) (Lazy.force sorted_cols);
      (* every capped column full -> fall back to the least-used column *)
      if !res = None then begin
        let best = ref None and best_used = ref max_int in
        Hashtbl.iter (fun _c arr ->
            let uc = Array.fold_left (fun a s -> if s.used then a + 1 else a) 0 arr in
            if uc < !best_used then
              match run_in_col arr len with
              | Some run -> best := Some run; best_used := uc
              | None -> ()) slice_by_col;
        res := !best
      end;
      !res
    end in
  List.iter (fun chain ->
      match find_col_run (List.length chain) with
      | Some run -> List.iteri (fun k c -> bind (Hashtbl.find name2id c.Pack_to_lef.pc_name) run.(k)) chain
      | None -> Printf.eprintf "no free carry column for chain len %d\n" (List.length chain))
    !chains;

  (* free region site nearest a target (skips used, honours SLICEM need of i) *)
  let nearest_region_free i (tx, ty) =
    let best = ref None in
    Array.iter (fun s -> if (not s.used) && fits i s then begin
        let d = abs (s.sx - tx) + abs (s.sy - ty) in
        match !best with Some (bd, _) when bd <= d -> () | _ -> best := Some (d, s) end) region_arr;
    Option.map snd !best in

  (* Die-wide nearest free site.  The region is a COMPACT block sized for this
     design, but an imported external placement (TOPO_SITE_IN) follows the
     foreign tool's own floorplan and generally lies outside it -- Vivado spread
     this design over X 46..221 where the region is X 24..106.  Falling back to
     nearest_region_free then drops every unmapped cell into a block 100+ tiles
     away from the neighbours it shares nets with, which is far worse than
     either placement alone.  Sites are the same mutable records region_arr
     holds, so `used` stays consistent. *)
  let all_slices_arr = Array.of_list all_slices in
  let nearest_any_free i (tx, ty) =
    let best = ref None in
    Array.iter (fun s -> if (not s.used) && fits i s then begin
        let d = abs (s.sx - tx) + abs (s.sy - ty) in
        match !best with Some (bd, _) when bd <= d -> () | _ -> best := Some (d, s) end)
      all_slices_arr;
    Option.map snd !best in

  (* ======================================================================= *)
  (* Constructive placement of remaining movable SLICE cells (region-bound).  *)
  (* centroid = mean of already-placed net neighbours.                        *)
  (* ======================================================================= *)
  let centroid i =
    let sx = ref 0 and sy = ref 0 and n = ref 0 in
    List.iter (fun nid -> Array.iter (fun j ->
        if j <> i && is_placed j then (sx := !sx + pos_x.(j); sy := !sy + pos_y.(j); incr n))
        net_cells.(nid)) cell_nets.(i);
    if !n = 0 then (region_cx, region_cy) else (!sx / !n, !sy / !n) in
  let constructive () =
    (* place SLICEM-constrained cells (DRAM/SRL) first so logic doesn't hog the
       scarcer SLICEM sites, then the rest. *)
    let do_pass want_sm =
      Array.iteri (fun i c ->
          if kind_of_lef c.Pack_to_lef.pc_lef = "SLICE" && not (is_placed i)
             && not skip.(i) && need_sm.(i) = want_sm then
            match nearest_region_free i (centroid i) with
            | Some s -> bind i s
            | None -> Printf.eprintf "no free region %s for %s\n"
                        (if want_sm then "SLICEM" else "SLICE") c.Pack_to_lef.pc_name) cells in
    do_pass true; do_pass false in

  (* ---- DEF IMPORT (TOPO_DEF_IN): adopt OpenROAD's logic-cell placement ----
     place_lef placed the MACROS (BRAM/dist-RAM/SRL/DSP/IO/clock buffers and the
     carry chains) and exported them FIXED; OpenROAD's RePlAce+OpenDP placed the
     ~2.4k free SLICE_LOGIC cells around them.  Read those coordinates back and
     snap each cell to the nearest free REAL site, closest-first so the cells
     OpenROAD packed tightest get their preferred site.  DEF is in DB units
     (2000 per 1x1 site) offset to the floorplan origin. *)
  (* ---- SITE IMPORT (TOPO_SITE_IN) -------------------------------------
     Adopt an EXTERNAL placement given as "<primitive-name>\t<SITE>" -- the same
     shape as BELS_OUT -- and then run all the normal downstream machinery
     (feedthrough relays, $cebuf promotion, carry stamping).  That machinery is
     the whole point: nextpnr's packer invents cells no external placer knows
     about ($PACKER_GND_NET$LUT drivers for a CARRY4's tied-off DI/S), and they
     MUST sit in the carry's own slice.  Hand-stamping a foreign placement into
     the netlist leaves them homeless ("cannot bind chain child ... bel not
     available"); letting place_lef do the placement import means carry_stamp
     still emits the const LUTs it always did.
     Keyed by PRIMITIVE because that is what external tools name; the packed
     cell is found through pc_bels. *)
  let site_import () = match Sys.getenv_opt "TOPO_SITE_IN" with
    | None -> false
    | Some path when not (Sys.file_exists path) ->
      Printf.eprintf "[place_lef] SITE-IN: %s not found\n" path; false
    | Some path ->
      let want = Hashtbl.create 8192 in
      (try
         let ic = open_in path in
         (try while true do
            let l = input_line ic in
            match String.split_on_char '\t' l with
            | nm :: s :: _ ->
              (* accept "SITE" or "SITE/BEL" *)
              let s = match String.index_opt s '/' with
                | Some i -> String.sub s 0 i | None -> s in
              let nm = String.trim nm and s = String.trim s in
              Hashtbl.replace want nm s;
              (* Distributed RAM arrives from Vivado as its LEAF sub-bels --
                 "<macro>/RAMA", "/RAMB"... -- but the netlist holds only the
                 PARENT (RAM64M/RAM32X1D), so the leaf name matches nothing.
                 All sub-bels of one macro share a site, so also offer the
                 collapsed parent key; first one wins. *)
              (match String.rindex_opt nm '/' with
               | Some i ->
                 let p = String.sub nm 0 i in
                 if p <> "" && not (Hashtbl.mem want p) then Hashtbl.add want p s
               | None -> ())
            | _ -> ()
          done with End_of_file -> ()); close_in ic
       with _ -> ());
      if Hashtbl.length want = 0 then false else begin
        let by_site = Hashtbl.create 4096 in
        Array.iter (fun s -> Hashtbl.replace by_site s.sname s) region_arr;
        Array.iter (fun s -> if not (Hashtbl.mem by_site s.sname) then
                       Hashtbl.replace by_site s.sname s) (Array.of_list all_slices);
        let ok = ref 0 and nosite = ref 0 and unmapped = ref 0 and moved = ref 0 in
        Array.iteri (fun i c ->
            if kind_of_lef c.Pack_to_lef.pc_lef = "SLICE" && not (is_placed i)
               && not skip.(i) then begin
              (* the packed cell inherits the site of any primitive it absorbed *)
              let tgt = List.fold_left (fun acc (prim, _) ->
                  match acc with Some _ -> acc
                               | None -> Hashtbl.find_opt want prim)
                  None c.Pack_to_lef.pc_bels in
              match tgt with
              | None -> incr unmapped
              | Some sn ->
                (match Hashtbl.find_opt by_site sn with
                 | Some s when not s.used && fits i s -> bind i s; incr ok
                 | Some s ->
                   (* site taken (two primitives of one pack, or a site the
                      packing collapsed): stay NEXT TO where it was asked for *)
                   (match nearest_any_free i (s.sx, s.sy) with
                    | Some s2 -> bind i s2; incr moved | None -> incr nosite)
                 | None -> incr unmapped)
            end) cells;
        (* Whatever the external placement did not cover -- cells it never named,
           and the feedthrough/relay cells place_lef itself will add -- goes
           beside its already-imported net neighbours, searching the WHOLE die.
           Confining these to the region would tear the placement in two. *)
        let late = ref 0 in
        Array.iteri (fun i c ->
            if kind_of_lef c.Pack_to_lef.pc_lef = "SLICE" && not (is_placed i)
               && not skip.(i) then
              match nearest_any_free i (centroid i) with
              | Some s -> bind i s; incr late
              | None -> Printf.eprintf "no free site for %s\n" c.Pack_to_lef.pc_name)
          cells;
        Printf.eprintf "[place_lef] SITE-IN: %d placed beside neighbours\n%!" !late;
        (* EXACT vs RELOCATED must be reported separately.  A packed cell claims
           a WHOLE slice, but Vivado puts 5+ primitives in one -- so most cells
           find their requested site already taken and get relocated.  Counting
           those as successful imports hides the fact that the imported
           placement was never actually reproduced. *)
        Printf.eprintf
          "[place_lef] SITE-IN: %d EXACT, %d RELOCATED (site taken), \
           %d unmapped, %d no free site -- from %s\n%!"
          !ok !moved !unmapped !nosite path;
        if !moved > !ok then
          Printf.eprintf
            "[place_lef] SITE-IN WARNING: most cells moved -- the external \
             placement packs denser than pack_to_lef does; the result is NOT \
             the imported placement\n%!";
        true
      end in

  let def_import () = match Sys.getenv_opt "TOPO_DEF_IN" with
    | None -> false
    | Some defp ->
      (* same origin the export used: region bbox expanded over fixed macros *)
      let sx_min = ref max_int and sy_min = ref max_int in
      Array.iter (fun s ->
          if s.sx < !sx_min then sx_min := s.sx;
          if s.sy < !sy_min then sy_min := s.sy) region_arr;
      Array.iteri (fun i _ ->
          if not movable.(i) && is_placed i then begin
            if pos_x.(i) < !sx_min then sx_min := pos_x.(i);
            if pos_y.(i) < !sy_min then sy_min := pos_y.(i)
          end) cells;
      let sx_min = !sx_min and sy_min = !sy_min in
      let u = 2000 in
      let tgt = Hashtbl.create (ncells * 2) in
      (try
         let ic = open_in defp in
         (try while true do
            let l = String.trim (input_line ic) in
            if String.length l > 2 && String.sub l 0 2 = "- " then begin
              let toks = List.filter (fun s -> s <> "")
                  (String.split_on_char ' ' l) in
              match toks with
              | _ :: nm :: rest ->
                (* find "(" x y ")" *)
                let rec scan = function
                  | "(" :: xs :: ys :: _ ->
                    (try Some (int_of_string xs, int_of_string ys) with _ -> None)
                  | _ :: tl -> scan tl
                  | [] -> None in
                (match scan rest with
                 | Some (x, y) ->
                   Hashtbl.replace tgt nm (x / u + sx_min, y / u + sy_min)
                 | None -> ())
              | _ -> ()
            end
          done with End_of_file -> ()); close_in ic
       with _ -> Printf.eprintf "[place_lef] DEF-IN: cannot read %s\n" defp);
      if Hashtbl.length tgt = 0 then false else begin
        (* order by distance to the DEF target: tight clusters bind first *)
        let cand = ref [] in
        Array.iteri (fun i c ->
            if kind_of_lef c.Pack_to_lef.pc_lef = "SLICE" && not (is_placed i)
               && not skip.(i) then
              match Hashtbl.find_opt tgt c.Pack_to_lef.pc_name with
              | Some p -> cand := (i, p) :: !cand
              | None -> ()) cells;
        let cand = List.sort (fun (_, (ax, ay)) (_, (bx, by)) ->
            compare (ax + ay) (bx + by)) !cand in
        let ok = ref 0 and miss = ref 0 and dsum = ref 0 in
        List.iter (fun (i, (tx, ty)) ->
            match nearest_region_free i (tx, ty) with
            | Some s ->
              dsum := !dsum + abs (s.sx - tx) + abs (s.sy - ty);
              bind i s; incr ok
            | None -> incr miss) cand;
        (* TOPO_DEF_FIX_IMPORTED=1: treat what OpenROAD placed as FROZEN, so the
           subsequent constructive+SA pass places only the remaining cells around
           it.  This is what makes "place the critical domain first, fit the rest
           around it" possible in one flow. *)
        if getenv_int "TOPO_DEF_FIX_IMPORTED" 0 <> 0 then
          List.iter (fun (i, _) -> if is_placed i then movable.(i) <- false) cand;
        (* anything OpenROAD did not place falls back to the normal path *)
        Array.iteri (fun i c ->
            if kind_of_lef c.Pack_to_lef.pc_lef = "SLICE" && not (is_placed i)
               && not skip.(i) then
              match nearest_region_free i (centroid i) with
              | Some s -> bind i s | None -> ()) cells;
        Printf.eprintf
          "[place_lef] DEF-IN: adopted %d cells from %s (mean snap %.2f sites, %d unplaceable)\n%!"
          !ok defp (if !ok = 0 then 0.0 else float !dsum /. float !ok) !miss;
        true
      end in

  (* ======================================================================= *)
  (* ANALYTIC: quadratic (weighted-clique) wirelength min by conjugate        *)
  (* gradient, anchored on fixed cells, weak spring to region centre; then    *)
  (* legalise (snap to nearest free region site in solved order).             *)
  (* ======================================================================= *)
  let analytic () =
    let idx = Array.make ncells (-1) in                 (* movable -> dense id *)
    let mv = ref [] and m = ref 0 in
    Array.iteri (fun i c ->
        if kind_of_lef c.Pack_to_lef.pc_lef = "SLICE" && movable.(i) then
          (idx.(i) <- !m; mv := i :: !mv; incr m)) cells;
    let m = !m in
    let mv = Array.of_list (List.rev !mv) in
    if m = 0 then () else begin
      let diag = Array.make m 0.0 in
      let bx = Array.make m 0.0 and by = Array.make m 0.0 in
      let adj = Array.make m [] in                        (* (dense j, w) list *)
      let lambda = getenv_float "TOPO_ANALYTIC_SPRING" 0.05 in
      for i = 0 to m - 1 do
        diag.(i) <- diag.(i) +. lambda;
        bx.(i) <- bx.(i) +. lambda *. float region_cx;
        by.(i) <- by.(i) +. lambda *. float region_cy
      done;
      let cap = getenv_int "TOPO_ANALYTIC_FANOUT_CAP" 40 in
      Array.iter (fun cs ->
          let k = Array.length cs in
          if k >= 2 && k <= cap then begin
            let w = 1.0 /. float (k - 1) in
            for a = 0 to k - 1 do for b = a + 1 to k - 1 do
              let ia = cs.(a) and ib = cs.(b) in
              let ma = idx.(ia) >= 0 and mb = idx.(ib) >= 0 in
              if ma && mb then begin
                let da = idx.(ia) and db = idx.(ib) in
                diag.(da) <- diag.(da) +. w; diag.(db) <- diag.(db) +. w;
                adj.(da) <- (db, w) :: adj.(da); adj.(db) <- (da, w) :: adj.(db)
              end else if ma then begin
                let da = idx.(ia) in diag.(da) <- diag.(da) +. w;
                bx.(da) <- bx.(da) +. w *. float pos_x.(ib);
                by.(da) <- by.(da) +. w *. float pos_y.(ib)
              end else if mb then begin
                let db = idx.(ib) in diag.(db) <- diag.(db) +. w;
                bx.(db) <- bx.(db) +. w *. float pos_x.(ia);
                by.(db) <- by.(db) +. w *. float pos_y.(ia)
              end
            done done
          end) net_cells;
      (* A x = b via conjugate gradient (A SPD: Laplacian + spring) *)
      let matvec x y =
        for i = 0 to m - 1 do
          let s = ref (diag.(i) *. x.(i)) in
          List.iter (fun (j, w) -> s := !s -. w *. x.(j)) adj.(i);
          y.(i) <- !s
        done in
      let cg b x0 =
        let x = Array.copy x0 in
        let r = Array.make m 0.0 and p = Array.make m 0.0 and ap = Array.make m 0.0 in
        matvec x ap;
        for i = 0 to m - 1 do r.(i) <- b.(i) -. ap.(i); p.(i) <- r.(i) done;
        let dot u v = let s = ref 0.0 in for i = 0 to m - 1 do s := !s +. u.(i) *. v.(i) done; !s in
        let rs = ref (dot r r) in
        let iters = getenv_int "TOPO_CG_ITERS" 250 in
        (try for _ = 1 to iters do
            if !rs < 1e-6 then raise Exit;
            matvec p ap;
            let alpha = !rs /. (dot p ap +. 1e-30) in
            for i = 0 to m - 1 do x.(i) <- x.(i) +. alpha *. p.(i); r.(i) <- r.(i) -. alpha *. ap.(i) done;
            let rs2 = dot r r in
            let beta = rs2 /. (!rs +. 1e-30) in
            for i = 0 to m - 1 do p.(i) <- r.(i) +. beta *. p.(i) done;
            rs := rs2
          done with Exit -> ());
        x in
      let x0 = Array.make m (float region_cx) and y0 = Array.make m (float region_cy) in
      let xs = cg bx x0 and ys = cg by y0 in
      (* legalise: snap movable cells to nearest free region site, in order of
         solved position (row-major) so clustered cells stay clustered. *)
      let order = Array.init m (fun i -> i) in
      Array.sort (fun a b -> compare (ys.(a), xs.(a)) (ys.(b), xs.(b))) order;
      let snap want_sm = Array.iter (fun di ->
          let i = mv.(di) in
          if need_sm.(i) = want_sm then begin
            let tx = int_of_float (Float.round xs.(di)) and ty = int_of_float (Float.round ys.(di)) in
            match nearest_region_free i (tx, ty) with
            | Some s -> bind i s
            | None -> Printf.eprintf "analytic: no free region SLICE for cell %d\n" i
          end) order in
      snap true; snap false      (* SLICEM-constrained cells first *)
    end in

  (* ======================================================================= *)
  (* SIMULATED ANNEALING over movable SLICE cells (HPWL cost, region-bound).  *)
  (* Move = relocate a random movable cell to a random region site; if that   *)
  (* site holds another movable cell, SWAP; fixed occupants are never moved.  *)
  (* ======================================================================= *)
  (* ===== global-routing congestion (RUDY) =====================================
     Bin the placement region; spread each multi-pin net's HPWL uniformly over
     the bins its bounding box covers (a wire-density estimate); overflow = the
     demand above a per-bin routing capacity.  With TOPO_CONG_W>0 the SA folds
     the overflow delta into its accept cost, so the placer actively relieves the
     congested regions that router2 fails to route -- not just total wirelength. *)
  let cong_w = getenv_float "TOPO_CONG_W" 0.0 in
  let cong_on = cong_w > 0.0 in
  let cong_bin = max 1 (getenv_int "TOPO_CONG_BIN" 6) in
  let cong_cap = getenv_float "TOPO_CONG_CAP" 20.0 in
  let cbx0 = Array.fold_left (fun a s -> min a s.sx) max_int region_arr in
  let cby0 = Array.fold_left (fun a s -> min a s.sy) max_int region_arr in
  let cbx1 = Array.fold_left (fun a s -> max a s.sx) min_int region_arr in
  let cby1 = Array.fold_left (fun a s -> max a s.sy) min_int region_arr in
  let nbx = if region_arr = [||] then 1 else (cbx1 - cbx0) / cong_bin + 1 in
  let nby = if region_arr = [||] then 1 else (cby1 - cby0) / cong_bin + 1 in
  let rudy = Array.make (nbx * nby) 0.0 in
  let ov d = if d > cong_cap then d -. cong_cap else 0.0 in
  (* SITE-density term: RUDY (wire density) does NOT stop bins filling to 100%
     site occupancy -- and a fully-packed bin has no room for routing/relays,
     which is what actually makes the datapath corner unroutable.  Penalise
     over-full bins directly.  bin capacity = cong_bin^2 sites; target = fraction. *)
  let site_w = getenv_float "TOPO_SITE_W" 0.0 in
  let site_on = site_w > 0.0 in
  let site_frac = getenv_float "TOPO_SITE_FRAC" 0.80 in
  let occbin = Array.make (nbx * nby) 0 in
  let bin_idx x y =
    let bx = (x - cbx0) / cong_bin and by = (y - cby0) / cong_bin in
    if bx >= 0 && bx < nbx && by >= 0 && by < nby then by * nbx + bx else -1 in
  (* per-bin site capacity = (# real SLICE sites in the bin) * target fill, from
     the floorplan -- the coord grid packs >1 slice per unit so a fixed cap is
     wrong; this is what makes the penalty meaningful. *)
  let sitecap = Array.make (nbx * nby) 0.0 in
  Array.iter (fun s -> let b = bin_idx s.sx s.sy in
      if b >= 0 then sitecap.(b) <- sitecap.(b) +. site_frac) region_arr;
  let sov b o = let cap = sitecap.(b) in let f = float o in if f > cap then f -. cap else 0.0 in
  (* accumulate net nid's per-bin RUDY (HPWL / #bins-covered) into `acc` with the
     given sign; used both to build `rudy` and the per-move overflow delta map. *)
  let net_rudy_acc nid sign acc =
    let cs = net_cells.(nid) in
    let mnx = ref max_int and mxx = ref min_int and mny = ref max_int and mxy = ref min_int
    and cnt = ref 0 in
    Array.iter (fun i -> if is_placed i then begin incr cnt;
        if pos_x.(i) < !mnx then mnx := pos_x.(i);
        if pos_x.(i) > !mxx then mxx := pos_x.(i);
        if pos_y.(i) < !mny then mny := pos_y.(i);
        if pos_y.(i) > !mxy then mxy := pos_y.(i) end) cs;
    if !cnt >= 2 then begin
      let hpwl = float ((!mxx - !mnx) + (!mxy - !mny)) in
      let bx0 = (!mnx - cbx0) / cong_bin and bx1 = (!mxx - cbx0) / cong_bin in
      let by0 = (!mny - cby0) / cong_bin and by1 = (!mxy - cby0) / cong_bin in
      let k = float ((bx1 - bx0 + 1) * (by1 - by0 + 1)) in
      let a = sign *. hpwl /. k in
      for by = by0 to by1 do for bx = bx0 to bx1 do
        if bx >= 0 && bx < nbx && by >= 0 && by < nby then acc (by * nbx + bx) a
      done done
    end in
  let total_overflow () = Array.fold_left (fun a d -> a +. ov d) 0.0 rudy in
  (* LONG-LINE / track resource model.  RUDY treats all wire as fungible, but a
     long net consumes SCARCE long-distance tracks (7-series LH/LV long lines) in
     the corridor it crosses -- which is exactly what nextpnr's router exhausts on
     the long spans we fail on, and what HPWL/RUDY miss.  Cut-based demand: for
     each net, every vertical cut inside its bbox carries 1 unit of HORIZONTAL
     track demand (spread over the rows it may route through); every horizontal
     cut carries 1 unit of VERTICAL demand (spread over columns).  Overflow where
     a cut's per-band demand exceeds the available track capacity. *)
  let ll_w = getenv_float "TOPO_LL_W" 0.0 in
  let ll_on = ll_w > 0.0 in
  let hcap = getenv_float "TOPO_LL_HCAP" (float cong_bin) in
  let vcap = getenv_float "TOPO_LL_VCAP" (float cong_bin) in
  (* MODULE COHESION: pull same-parent-module cells toward their module centroid.
     The site-density penalty spreads global density but also scatters coupled
     logic (e.g. i_arp's 48-bit sender_mac readback smeared over 37 columns ->
     its mux nets can't route).  Cohesion keeps each module compact while density
     only punishes UNRELATED modules sharing a bin -- resolving that tension. *)
  let coh_w = getenv_float "TOPO_COH_W" 0.0 in
  let coh_on = coh_w > 0.0 in
  (* TOPO_COH_DEPTH>0: group by the first K hierarchy components (split on '.'
     or '/'), so a deep vendor IP block (e.g. eth.sgmii_soc1.i_pcs_pma) becomes
     ONE cohesion module and is pulled compact, instead of shattering into
     hundreds of per-register-bank groups.  0 = legacy (strip last component). *)
  let coh_depth = getenv_int "TOPO_COH_DEPTH" 0 in
  let strip_last name =
    let ld = try String.rindex name '.' with Not_found -> -1 in
    let ls = try String.rindex name '/' with Not_found -> -1 in
    let cut = max ld ls in
    if cut > 0 then String.sub name 0 cut else name in
  let mod_key name =
    if coh_depth <= 0 then strip_last name
    else begin
      (* Count hierarchy separators.  Only names DEEPER than coh_depth are
         coarsened (cut at the coh_depth-th separator) so a deep vendor IP is
         one compact module; shallow names keep legacy per-bank grouping so the
         surrounding glue logic is unaffected. *)
      let n = String.length name in
      let nsep = ref 0 in
      for i = 0 to n - 1 do
        if name.[i] = '.' || name.[i] = '/' then incr nsep
      done;
      if !nsep <= coh_depth then strip_last name
      else begin
        let seen = ref 0 and cut = ref n in
        (try
           for i = 0 to n - 1 do
             if name.[i] = '.' || name.[i] = '/' then begin
               incr seen;
               if !seen >= coh_depth then (cut := i; raise Exit)
             end
           done
         with Exit -> ());
        String.sub name 0 !cut
      end
    end in
  let mod_ids = Hashtbl.create 256 and nmods = ref 0 in
  let mod_of = Array.make ncells (-1) in
  Array.iteri (fun i c ->
      let k = mod_key c.Pack_to_lef.pc_name in
      let id = match Hashtbl.find_opt mod_ids k with Some d -> d
               | None -> let d = !nmods in incr nmods; Hashtbl.replace mod_ids k d; d in
      mod_of.(i) <- id) cells;
  let msx = Array.make (max 1 !nmods) 0 and msy = Array.make (max 1 !nmods) 0
  and mcnt = Array.make (max 1 !nmods) 0 in
  let hcut = Array.make (nbx * nby) 0.0 in
  let vcut = Array.make (nbx * nby) 0.0 in
  let hov d = if d > hcap then d -. hcap else 0.0 in
  let vov d = if d > vcap then d -. vcap else 0.0 in
  let net_track_acc nid sign hacc vacc =
    let cs = net_cells.(nid) in
    let mnx = ref max_int and mxx = ref min_int and mny = ref max_int and mxy = ref min_int and cnt = ref 0 in
    Array.iter (fun i -> if is_placed i then begin incr cnt;
        if pos_x.(i) < !mnx then mnx := pos_x.(i);
        if pos_x.(i) > !mxx then mxx := pos_x.(i);
        if pos_y.(i) < !mny then mny := pos_y.(i);
        if pos_y.(i) > !mxy then mxy := pos_y.(i) end) cs;
    if !cnt >= 2 then begin
      let bx0 = (!mnx - cbx0) / cong_bin and bx1 = (!mxx - cbx0) / cong_bin in
      let by0 = (!mny - cby0) / cong_bin and by1 = (!mxy - cby0) / cong_bin in
      let nrows = float (by1 - by0 + 1) and ncols = float (bx1 - bx0 + 1) in
      (* horizontal wire crosses each vertical cut in [bx0,bx1), spread over rows *)
      for bxc = bx0 to bx1 - 1 do
        for by = by0 to by1 do
          if bxc >= 0 && bxc < nbx && by >= 0 && by < nby then hacc (by * nbx + bxc) (sign /. nrows)
        done
      done;
      (* vertical wire crosses each horizontal cut in [by0,by1), spread over cols *)
      for byc = by0 to by1 - 1 do
        for bx = bx0 to bx1 do
          if bx >= 0 && bx < nbx && byc >= 0 && byc < nby then vacc (byc * nbx + bx) (sign /. ncols)
        done
      done
    end in
  let track_overflow () =
    Array.fold_left (fun a d -> a +. hov d) 0.0 hcut +. Array.fold_left (fun a d -> a +. vov d) 0.0 vcut in
  let net_stamp = Array.make !nnets 0 and stamp_ctr = ref 0 in
  let anneal () =
    let mv = ref [] in
    Array.iteri (fun i _ -> if movable.(i) && is_placed i then mv := i :: !mv) cells;
    let mv = Array.of_list !mv in
    let m = Array.length mv in
    if m = 0 || region_arr = [||] then () else begin
      let moves = getenv_int "TOPO_SA_MOVES" (max 200000 (150 * m)) in
      (* Co-scale the SA temperature with cong_w: the acceptance uses
         delta = HPWL_delta + cong_w*dcong, so a large cong_w inflates deltas and
         (at a fixed t0) freezes the anneal -> LESS overflow reduction with MORE
         weight (the observed inversion).  Scaling t0/tend by cong_w keeps the
         accept ratio calibrated so higher weight actually spreads congestion. *)
      let ctemp = if cong_on then max 1.0 cong_w else 1.0 in
      let t0 = getenv_float "TOPO_SA_T0" (8.0 *. ctemp)
      and tend = getenv_float "TOPO_SA_TEND" (0.05 *. ctemp) in
      let alpha = (tend /. t0) ** (1.0 /. float (max 1 moves)) in
      let t = ref t0 in
      if cong_on then begin
        Array.fill rudy 0 (nbx * nby) 0.0;
        for n = 0 to !nnets - 1 do net_rudy_acc n 1.0 (fun b a -> rudy.(b) <- rudy.(b) +. a) done;
        Printf.eprintf "congestion: %dx%d bins (size %d), cap %.0f, initial overflow=%.0f\n"
          nbx nby cong_bin cong_cap (total_overflow ())
      end;
      if site_on then begin
        Array.fill occbin 0 (nbx * nby) 0;
        Array.iteri (fun i _ -> if movable.(i) && is_placed i then
            let b = bin_idx pos_x.(i) pos_y.(i) in if b >= 0 then occbin.(b) <- occbin.(b) + 1) cells;
        let peak = Array.fold_left max 0 occbin in
        let peakcap = Array.fold_left max 0.0 sitecap in
        Printf.eprintf "site-density: target fill=%.2f, peak cap=%.1f/bin, initial peak occ=%d\n"
          site_frac peakcap peak
      end;
      if ll_on then begin
        Array.fill hcut 0 (nbx * nby) 0.0; Array.fill vcut 0 (nbx * nby) 0.0;
        for n = 0 to !nnets - 1 do
          net_track_acc n 1.0 (fun b a -> hcut.(b) <- hcut.(b) +. a) (fun b a -> vcut.(b) <- vcut.(b) +. a)
        done;
        Printf.eprintf "long-line: hcap=%.1f vcap=%.1f, initial track overflow=%.0f\n"
          hcap vcap (track_overflow ())
      end;
      if coh_on then begin
        Array.fill msx 0 !nmods 0; Array.fill msy 0 !nmods 0; Array.fill mcnt 0 !nmods 0;
        Array.iteri (fun i _ -> if is_placed i then begin
            let mo = mod_of.(i) in
            msx.(mo) <- msx.(mo) + pos_x.(i); msy.(mo) <- msy.(mo) + pos_y.(i);
            mcnt.(mo) <- mcnt.(mo) + 1 end) cells;
        Printf.eprintf "cohesion: %d modules, w=%.1f\n" !nmods coh_w
      end;
      (* delta over the union of affected cells' nets; apply new positions,
         return (delta, restore-thunk data). *)
      let eval_delta moved newpos =
        incr stamp_ctr; let st = !stamp_ctr in
        let nets = ref [] in
        List.iter (fun i -> List.iter (fun nid ->
            if net_stamp.(nid) <> st then (net_stamp.(nid) <- st; nets := nid :: !nets))
            cell_nets.(i)) moved;
        let before = List.fold_left (fun a n -> a +. net_w.(n) *. float (net_hpwl n)) 0.0 !nets in
        (* clock-domain attraction, O(1) per moved cell (fixed centroid) *)
        let dom_before =
          List.fold_left (fun a i -> a +. dom_cost i pos_x.(i) pos_y.(i)) 0.0 moved in
        let mac_before =
          List.fold_left (fun a i -> a +. macro_cost pos_x.(i) pos_y.(i)) 0.0 moved in
        (* congestion: accumulate the affected nets' OLD RUDY (negated) *)
        let cmap = Hashtbl.create 32 in
        let acc b a = Hashtbl.replace cmap b ((try Hashtbl.find cmap b with Not_found -> 0.0) +. a) in
        let hmap = Hashtbl.create 32 and vmap = Hashtbl.create 32 in
        let hacc b a = Hashtbl.replace hmap b ((try Hashtbl.find hmap b with Not_found -> 0.0) +. a) in
        let vacc b a = Hashtbl.replace vmap b ((try Hashtbl.find vmap b with Not_found -> 0.0) +. a) in
        if cong_on then List.iter (fun n -> net_rudy_acc n (-1.0) acc) !nets;
        if ll_on then List.iter (fun n -> net_track_acc n (-1.0) hacc vacc) !nets;
        let olds = List.map (fun i -> (pos_x.(i), pos_y.(i))) moved in
        List.iter2 (fun i (nx, ny) -> pos_x.(i) <- nx; pos_y.(i) <- ny) moved newpos;
        let after = List.fold_left (fun a n -> a +. net_w.(n) *. float (net_hpwl n)) 0.0 !nets in
        let dom_after =
          List.fold_left (fun a i -> a +. dom_cost i pos_x.(i) pos_y.(i)) 0.0 moved in
        let ddom = dom_after -. dom_before in
        let dmac =
          (List.fold_left (fun a i -> a +. macro_cost pos_x.(i) pos_y.(i)) 0.0 moved)
          -. mac_before in
        (* add the NEW RUDY -> cmap now holds per-bin demand delta *)
        if cong_on then List.iter (fun n -> net_rudy_acc n (1.0) acc) !nets;
        if ll_on then List.iter (fun n -> net_track_acc n (1.0) hacc vacc) !nets;
        let dcong = if not cong_on then 0.0 else
          Hashtbl.fold (fun b d a -> a +. (ov (rudy.(b) +. d) -. ov rudy.(b))) cmap 0.0 in
        let dtrack = if not ll_on then 0.0 else
          (Hashtbl.fold (fun b d a -> a +. (hov (hcut.(b) +. d) -. hov hcut.(b))) hmap 0.0)
          +. (Hashtbl.fold (fun b d a -> a +. (vov (vcut.(b) +. d) -. vov vcut.(b))) vmap 0.0) in
        (* site-occupancy delta: -1 at each moved cell's old bin, +1 at its new
           bin (swaps cancel); penalise pushing a bin past site_cap. *)
        let omap = Hashtbl.create 8 in
        if site_on then begin
          let oacc b d = if b >= 0 then Hashtbl.replace omap b ((try Hashtbl.find omap b with Not_found -> 0) + d) in
          List.iter2 (fun (ox, oy) (nx, ny) -> oacc (bin_idx ox oy) (-1); oacc (bin_idx nx ny) 1) olds newpos
        end;
        let sdelta = if not site_on then 0.0 else
          Hashtbl.fold (fun b d a -> a +. (sov b (occbin.(b) + d) -. sov b occbin.(b))) omap 0.0 in
        (* module-cohesion delta: change in each moved cell's Manhattan distance to
           its module centroid (centroid held fixed within the move -- standard). *)
        let dcoh =
          if not coh_on then 0.0 else
          let rec go acc ms os = match ms, os with
            | i :: mt, (ox, oy) :: ot ->
              let mo = mod_of.(i) in let c = mcnt.(mo) in
              let d = if c <= 1 then 0 else
                let cx = msx.(mo) / c and cy = msy.(mo) / c in
                (abs (pos_x.(i) - cx) + abs (pos_y.(i) - cy))
                - (abs (ox - cx) + abs (oy - cy)) in
              go (acc +. float d) mt ot
            | _ -> acc in
          go 0.0 moved olds in
        ((after -. before) +. cong_w *. dcong +. site_w *. sdelta +. ll_w *. dtrack
           +. coh_w *. dcoh +. ddom +. dmac,
         olds, cmap, omap, hmap, vmap) in
      let accepted = ref 0 in
      refresh_dom ();
      if dom_w > 0.0 then
        Printf.eprintf "[place_lef] clock-domain affinity: %d domain(s), W=%.1f\n%!" !ndom dom_w;
      (* Periodic progress to stderr (unbuffered) -- a 900k-move anneal is
         minutes of otherwise total silence, which reads as a hang. *)
      let prog_every = max 1 (moves / 20) in
      let t_start = Sys.time () in
      for mvno = 1 to moves do
        if mvno mod prog_every = 0 then begin
          (* re-centre each clock domain on where its cells actually are now;
             held fixed between refreshes to keep eval_delta O(1) *)
          refresh_dom ();
          Printf.eprintf "  SA %3d%%  moves=%d/%d  accepted=%d  temp=%.2f  %.0fs\n%!"
            (100 * mvno / moves) mvno moves !accepted !t
            (Sys.time () -. t_start)
        end;
        let i = mv.(Random.int m) in
        (* zoned: draw the target only from this cell's own domain block *)
        let pool =
          if not zone_on then region_arr
          else let d = cell_dom.(i) in
            if d >= 0 && zone_arr.(d) <> [||] then zone_arr.(d) else region_arr in
        let s = pool.(Random.int (Array.length pool)) in
        let si = match cell_site.(i) with Some s -> s | None -> assert false in
        if s.sname <> si.sname && fits i s then begin
          let j = match Hashtbl.find_opt occ s.sname with Some j -> j | None -> -1 in
          (* a swap must not evict the partner out of ITS OWN zone *)
          let swap_ok j =
            not zone_on || j < 0 ||
            (let dj = cell_dom.(j) in
             dj < 0 || zone_arr.(dj) = [||] ||
             (match Hashtbl.find_opt site_zone si.sname with Some z -> z = dj | None -> false)) in
          (* legal iff empty target, or a swap where BOTH land on a site they fit *)
          if (j = -1 || (movable.(j) && fits j si && swap_ok j)) then begin
            let moved, newpos =
              if j = -1 then [i], [(s.sx, s.sy)]
              else [i; j], [(s.sx, s.sy); (si.sx, si.sy)] in
            let delta, olds, cmap, omap, hmap, vmap = eval_delta moved newpos in
            let accept = delta <= 0.0 || Random.float 1.0 < exp (-. delta /. !t) in
            if accept then begin
              incr accepted;
              if cong_on then Hashtbl.iter (fun b d -> rudy.(b) <- rudy.(b) +. d) cmap;
              if site_on then Hashtbl.iter (fun b d -> occbin.(b) <- occbin.(b) + d) omap;
              if coh_on then List.iter2 (fun i (ox, oy) ->
                  let mo = mod_of.(i) in
                  msx.(mo) <- msx.(mo) - ox + pos_x.(i);
                  msy.(mo) <- msy.(mo) - oy + pos_y.(i)) moved olds;
              if ll_on then begin
                Hashtbl.iter (fun b d -> hcut.(b) <- hcut.(b) +. d) hmap;
                Hashtbl.iter (fun b d -> vcut.(b) <- vcut.(b) +. d) vmap
              end;
              (* commit site bookkeeping *)
              if j = -1 then begin
                si.used <- false; Hashtbl.remove occ si.sname;
                s.used <- true; cell_site.(i) <- Some s; Hashtbl.replace occ s.sname i
              end else begin
                cell_site.(i) <- Some s; Hashtbl.replace occ s.sname i;
                cell_site.(j) <- Some si; Hashtbl.replace occ si.sname j
              end
            end else
              List.iter2 (fun c (ox, oy) -> pos_x.(c) <- ox; pos_y.(c) <- oy) moved olds
          end
        end;
        t := !t *. alpha
      done;
      Printf.eprintf "SA: %d/%d moves accepted (%.1f%%)\n" !accepted moves (100. *. float !accepted /. float moves);
      if cong_on then Printf.eprintf "congestion: final overflow=%.0f\n" (total_overflow ());
      if ll_on then Printf.eprintf "long-line: final track overflow=%.0f\n" (track_overflow ())
    end in

  (* ======================================================================= *)
  (* FIGURE OF MERIT: routability statistics computed natively (no external  *)
  (* post-processing) so the placer can OPTIMISE on them, not just report:   *)
  (*   - per-bin site occupancy vs real capacity (peak %, full bins, >=90%)  *)
  (*   - net span histogram (spans beyond long-line reach)                   *)
  (*   - RUDY wire overflow, long-line track overflow, site overflow        *)
  (* fom_value composes them with the ACTIVE anneal weights, so restart      *)
  (* selection optimises the same objective the anneal accepts moves on.     *)
  (* ======================================================================= *)
  let fom_stats () =
    let hp, worst = total_hpwl () in
    let nb = nbx * nby in
    (* per-bin placed SLICE-cell count vs real SLICE sites in the bin *)
    let nsites = Array.make nb 0 and occf = Array.make nb 0 in
    Array.iter (fun s -> let b = bin_idx s.sx s.sy in if b >= 0 then nsites.(b) <- nsites.(b) + 1)
      region_arr;
    Array.iteri (fun i _ -> if is_placed i then
        match cell_site.(i) with
        | Some s when String.length s.sname >= 6 && String.sub s.sname 0 6 = "SLICE_" ->
            let b = bin_idx pos_x.(i) pos_y.(i) in if b >= 0 then occf.(b) <- occf.(b) + 1
        | _ -> ()) cells;
    let full = ref 0 and near = ref 0 and peak = ref 0.0 in
    Array.iteri (fun b n -> if n > 0 then begin
        let r = float occf.(b) /. float n in
        if r > !peak then peak := r;
        if occf.(b) >= n then incr full else if r >= 0.9 then incr near
      end) nsites;
    (* net span histogram (same coord space the anneal optimises) *)
    let sp12 = ref 0 and sp24 = ref 0 and spmax = ref 0 in
    for n = 0 to !nnets - 1 do
      let mnx = ref max_int and mxx = ref min_int and mny = ref max_int and mxy = ref min_int
      and c = ref 0 in
      Array.iter (fun i -> if is_placed i then begin incr c;
          if pos_x.(i) < !mnx then mnx := pos_x.(i);
          if pos_x.(i) > !mxx then mxx := pos_x.(i);
          if pos_y.(i) < !mny then mny := pos_y.(i);
          if pos_y.(i) > !mxy then mxy := pos_y.(i) end) net_cells.(n);
      if !c >= 2 then begin
        let sp = max (!mxx - !mnx) (!mxy - !mny) in
        if sp > !spmax then spmax := sp;
        if sp > 12 then incr sp12;
        if sp > 24 then incr sp24
      end
    done;
    (* wire / track / site overflows, recomputed fresh from current positions *)
    Array.fill rudy 0 nb 0.0;
    for n = 0 to !nnets - 1 do net_rudy_acc n 1.0 (fun b a -> rudy.(b) <- rudy.(b) +. a) done;
    let rovf = total_overflow () in
    Array.fill hcut 0 nb 0.0; Array.fill vcut 0 nb 0.0;
    for n = 0 to !nnets - 1 do
      net_track_acc n 1.0 (fun b a -> hcut.(b) <- hcut.(b) +. a)
        (fun b a -> vcut.(b) <- vcut.(b) +. a)
    done;
    let tovf = track_overflow () in
    let sovf = ref 0.0 in
    Array.iteri (fun b o -> sovf := !sovf +. sov b o) occf;
    (hp, worst, !peak, !full, !near, !sp12, !sp24, !spmax, rovf, tovf, !sovf) in
  let fom_value () =
    let (hp, _, _, _, _, _, _, _, rovf, tovf, sovf) = fom_stats () in
    float hp +. cong_w *. rovf +. site_w *. sovf +. ll_w *. tovf in
  let fom_report () =
    let (hp, worst, peak, full, near, sp12, sp24, spmax, rovf, tovf, sovf) = fom_stats () in
    Printf.eprintf
      "FOM: hpwl=%d worst=%d | bins: peak=%.0f%% full=%d near90=%d | spans: >12=%d >24=%d max=%d | ovf: rudy=%.0f site=%.0f track=%.0f | composite=%.0f\n"
      hp worst (100. *. peak) full near sp12 sp24 spmax rovf sovf tovf
      (float hp +. cong_w *. rovf +. site_w *. sovf +. ll_w *. tovf) in
  (* multi-restart: run the anneal TOPO_RESTARTS times from the same seed
     placement with different RNG streams, keep the placement with the best
     composite FOM.  This is the first internal consumer of fom_value. *)
  let anneal_multi () =
    let restarts = getenv_int "TOPO_RESTARTS" 1 in
    if restarts <= 1 then anneal ()
    else begin
      let snap () = (Array.copy pos_x, Array.copy pos_y, Array.copy cell_site,
                     Hashtbl.copy occ, Array.map (fun s -> s.used) region_arr) in
      let restore (px, py, cs, oc, us) =
        Array.blit px 0 pos_x 0 (Array.length px);
        Array.blit py 0 pos_y 0 (Array.length py);
        Array.blit cs 0 cell_site 0 (Array.length cs);
        Hashtbl.reset occ; Hashtbl.iter (fun k v -> Hashtbl.replace occ k v) oc;
        Array.iteri (fun k s -> s.used <- us.(k)) region_arr in
      let base = snap () in
      let best = ref None in
      for r = 1 to restarts do
        restore base;
        Random.init (seed + 7919 * (r - 1));
        anneal ();
        let f = fom_value () in
        Printf.eprintf "restart %d/%d: composite FOM=%.0f\n" r restarts f;
        (match !best with
         | Some (bf, _) when bf <= f -> ()
         | _ -> best := Some (f, snap ()))
      done;
      match !best with
      | Some (bf, st) -> restore st; Printf.eprintf "restarts: kept best FOM=%.0f\n" bf
      | None -> ()
    end in

  (* ======================================================================= *)
  (* Drive the selected pipeline.                                             *)
  (* ======================================================================= *)
  Printf.printf "mode=%s  region=%d sites (fill target %.2f, n_slice=%d)\n"
    mode (Array.length region_arr) fill n_slice;
  if region_arr <> [||] then begin
    let rx0 = Array.fold_left (fun a s -> min a s.sx) max_int region_arr
    and rx1 = Array.fold_left (fun a s -> max a s.sx) min_int region_arr
    and ry0 = Array.fold_left (fun a s -> min a s.sy) max_int region_arr
    and ry1 = Array.fold_left (fun a s -> max a s.sy) min_int region_arr in
    let w = rx1 - rx0 + 1 and h = ry1 - ry0 + 1 in
    Printf.printf "region shape=%s aspect=%.2f anchor=(%d,%d) \
                   bbox X[%d..%d] Y[%d..%d] = %dx%d, %.0f%% of bbox used\n"
      region_shape region_aspect anchor_cx anchor_cy rx0 rx1 ry0 ry1 w h
      (100. *. float_of_int (Array.length region_arr) /. float_of_int (w * h))
  end;
  let def_only = getenv_int "TOPO_DEF_ONLY" 0 <> 0 in
  (match mode with
   | "site"     -> if not (site_import ()) then constructive ()
   | "def"      -> if not (def_import ()) then constructive ()
   | "defsa"    -> ignore (def_import ()); constructive ();
                   if not def_only then anneal_multi ()
   | "analytic" -> analytic (); anneal_multi ()
   | "sa"       -> constructive (); emit_def ();
                   if not def_only then begin
                     let h0, _ = total_hpwl () in
                     Printf.eprintf "SA seed HPWL=%d\n" h0; anneal_multi ()
                   end
   | "region"   -> constructive (); emit_def ()
   | "greedy" | _ -> constructive (); emit_def ());

  (* ---- report + emit ---------------------------------------------------- *)
  let placed_n = Array.fold_left (fun a i -> if is_placed i then a + 1 else a) 0
      (Array.init ncells (fun i -> i)) in
  let total, worst = total_hpwl () in
  Printf.printf "placed %d/%d cells\n" placed_n ncells;
  Printf.printf "total HPWL = %d (SLICE-hops), worst net = %d\n" total worst;
  (* bounding box of placed SLICE cells *)
  let mnx = ref max_int and mxx = ref min_int and mny = ref max_int and mxy = ref min_int and nsl = ref 0 in
  Array.iteri (fun i c -> if kind_of_lef c.Pack_to_lef.pc_lef = "SLICE" && is_placed i then begin
      incr nsl;
      if pos_x.(i) < !mnx then mnx := pos_x.(i); if pos_x.(i) > !mxx then mxx := pos_x.(i);
      if pos_y.(i) < !mny then mny := pos_y.(i); if pos_y.(i) > !mxy then mxy := pos_y.(i) end) cells;
  if !nsl > 0 then begin
    let w = !mxx - !mnx + 1 and h = !mxy - !mny + 1 in
    Printf.printf "SLICE bbox X[%d..%d] Y[%d..%d] = %dx%d = %d sites, fill=%.1f%%\n"
      !mnx !mxx !mny !mxy w h (w * h) (100. *. float !nsl /. float (w * h))
  end;
  fom_report ();
  let placed_path = getenv_default "PLACED_OUT" "/tmp/counter_placed.txt" in
  let bels_path = getenv_default "BELS_OUT" "/tmp/counter_bels.txt" in
  let oc = open_out placed_path in
  Array.iter (fun c -> match cell_site.(Hashtbl.find name2id c.Pack_to_lef.pc_name) with
      | Some site -> Printf.fprintf oc "%s\t%s\t%s\n" c.Pack_to_lef.pc_name c.Pack_to_lef.pc_lef site.sname
      | None -> ()) cells;
  close_out oc;
  (* Emit BEL stamps for the placeable fabric (SLICE/BRAM/DSP).  Skip the
     pin-dictated clock/IO infrastructure (GT/MMCM/BUFG/BUFH/IO) unless
     TOPO_STAMP_ALL=1 -- nextpnr places those from the XDC. *)
  let stamp_all = Sys.getenv_opt "TOPO_STAMP_ALL" <> None in
  let skip_kind = function "GT" | "MMCM" | "BUFG" | "BUFH" | "IO" -> true | _ -> false in
  (* TOPO_BELS_SKIP_CARREP=1 -- for consumers that hold the ORIGINAL netlist.
     ------------------------------------------------------------------------
     When a CARRY4's CO[3] feeds more than one downstream CARRY4 CI, the carry
     prepass above REPLICATES the whole chain, one clone per extra user, because
     COUT->CIN is a dedicated point-to-point wire that reaches only the slice
     directly above.  Those clones (<cell>_carrep<i>) exist in the netlist WE
     place and nowhere else.

     Hand this placement to a tool reading the PRE-prepass netlist -- Vivado, in
     ethmin/vivado_route_ethmin.tcl -- and the CO net still fans out to every
     consumer, while the positions we supply were computed as if each consumer
     had its own clone.  At most one of them can then be reached through the
     cascade and the rest are physically unroutable:

       [DRC RTSTAT-2] Partially routed nets: ... alu_lts_CARRY4_CO_CO_1..._n_0
       [DRC RTSTAT-5] Partial antennas:      ... the same net

     A consumer's placement is meaningless in a netlist that has no clone, so
     the consumer's own placer should put those few chains where ITS netlist
     says they belong.

     BUT THE STAMPS MUST STILL BE WRITTEN.  bels.txt is not just "the placement
     we hand Vivado" -- carry_stamp.py reads it to build the JSON that NEXTPNR
     routes.  An earlier version of this omitted the stamps outright and thereby
     deleted 11 cells' placement from the open flow: 3 packed cells (2 replica
     chains + the consumer they feed, whose pc_bels carry 8 cloned S-LUTs).
     nextpnr then had them as free cells, and its analytic placer SPUN AT 100%
     CPU INDEFINITELY on those 7 clusters -- a hang that looked like a router
     problem and was self-inflicted.

     So bels.txt stays COMPLETE and the names go to a side file instead
     (TOPO_CARREP_SKIP_OUT, default <bels>.carrep_skip).  A consumer that holds
     the un-replicated netlist reads that list and declines those names;
     everything else, nextpnr included, is unaffected. *)
  let skip_carrep = getenv_int "TOPO_BELS_SKIP_CARREP" 0 <> 0 in
  let contains s sub =
    let ls = String.length sub and ln = String.length s in
    ls > 0 && (let rec go j = j + ls <= ln
                              && (String.sub s j ls = sub || go (j + 1)) in go 0) in
  (* net driven by each packed cell's CO, so a consumer can be traced to a clone *)
  let co_driver : (Pack_to_lef.netkey, string) Hashtbl.t = Hashtbl.create 256 in
  if skip_carrep then
    Array.iter (fun c ->
        if contains c.Pack_to_lef.pc_lef "SLICE_CARRY" then
          match List.assoc_opt "CO" c.Pack_to_lef.pc_conns with
          | Some nk -> Hashtbl.replace co_driver nk c.Pack_to_lef.pc_name
          | None -> ()) cells;
  let n_repl = ref 0 and n_cons = ref 0 in
  let skip_cell c =
    if not skip_carrep then false
    else if contains c.Pack_to_lef.pc_name "_carrep" then (incr n_repl; true)
    else if contains c.Pack_to_lef.pc_lef "SLICE_CARRY" then
      match List.assoc_opt "CI" c.Pack_to_lef.pc_conns with
      | Some nk ->
        (match Hashtbl.find_opt co_driver nk with
         | Some drv when contains drv "_carrep" -> incr n_cons; true
         | _ -> false)
      | None -> false
    else false in
  let ob = open_out bels_path in
  let skip_path = getenv_default "TOPO_CARREP_SKIP_OUT" (bels_path ^ ".carrep_skip") in
  let os_ = if skip_carrep then Some (open_out skip_path) else None in
  let nb = ref 0 and nskip = ref 0 in
  Array.iter (fun c ->
      if stamp_all || not (skip_kind (kind_of_lef c.Pack_to_lef.pc_lef)) then
      match cell_site.(Hashtbl.find name2id c.Pack_to_lef.pc_name) with
      | Some site ->
        (* every stamp is written -- the open flow needs all of them *)
        List.iter (fun (prim, suffix) ->
            Printf.fprintf ob "%s\t%s/%s\n" prim site.sname suffix; incr nb) c.Pack_to_lef.pc_bels;
        (* ...and the replica-tainted ones are ALSO named in the side file *)
        (match os_ with
         | Some oc when skip_cell c ->
           List.iter (fun (prim, _) -> Printf.fprintf oc "%s\n" prim; incr nskip)
             c.Pack_to_lef.pc_bels
         | _ -> ())
      | None -> ()) cells;
  close_out ob;
  (match os_ with Some oc -> close_out oc | None -> ());
  Printf.printf "placement -> %s ; %d BEL stamps -> %s\n" placed_path !nb bels_path;
  if skip_carrep then
    Printf.printf "  [bels] carry-replica advisory: %d replica cell(s) + %d consumer CARRY4(s) \
                   = %d primitive(s) -> %s (bels.txt itself is COMPLETE)\n"
      !n_repl !n_cons !nskip skip_path;

  (* ===== FEEDTHROUGH INSERTION (OCaml, driven by this placer's data) =========
     For LOGIC nets (skip clock/const) whose driver->sink arc exceeds a
     threshold or that are high-fanout, relay the distant/clustered sinks
     through a LUT1 buffer placed near their centroid in a free, low-congestion
     site.  We surgically edit the input yosys JSON (add relay cells + rewire the
     affected sink bits only, leaving GT/MMCM/params byte-identical) rather than
     round-tripping the lossy bmodule.  Enabled by TOPO_FEEDTHRU=<thresh>. *)
  (match Sys.getenv_opt "TOPO_FEEDTHRU" with
   | None -> ()
   | Some ths ->
     let thresh = (try int_of_string ths with _ -> 18) in
     let clu = getenv_int "TOPO_FEEDTHRU_CLU" 12 in
     let minfo = getenv_int "TOPO_FEEDTHRU_MINFO" 12 in
     let module Y = Yojson.Safe in
     let member k = function `Assoc a -> (try List.assoc k a with Not_found -> `Null) | _ -> `Null in
     (* raw primitive inst -> placed (x,y), from each packed cell's pc_bels *)
     let inst2pos = Hashtbl.create (ncells * 2) in
     Array.iteri (fun i c -> if is_placed i then
         List.iter (fun (raw, _bel) -> Hashtbl.replace inst2pos raw (pos_x.(i), pos_y.(i)))
           c.Pack_to_lef.pc_bels) cells;
     let j = get_j () in
     let mods = (match member "modules" j with `Assoc a -> a | _ -> []) in
     let ncells_of m = (match member "cells" m with `Assoc a -> List.length a | _ -> 0) in
     let topname, _, topm = List.fold_left (fun (bnm, bn, bm) (nm, m) ->
         let c = ncells_of m in if c > bn then (nm, c, m) else (bnm, bn, bm)) ("", -1, `Null) mods in
     let cells_j = (match member "cells" topm with `Assoc a -> a | _ -> []) in
     (* driver-cell JSON lookup, for placement-driven DRIVER REPLICATION: a LUT
        feeding a distant sink cluster is DUPLICATED (same inputs) at the cluster
        rather than relayed through a LUT1 -- removes the relay's own extra hop
        and never strands (the copy IS the driver, placed at its sinks). *)
     let cell_json_of = Hashtbl.create 8192 in
     List.iter (fun (cn, c) -> Hashtbl.replace cell_json_of cn c) cells_j;
     let is_lut_ty t = String.length t >= 3 && String.sub t 0 3 = "LUT" in
     let relay_maxd = getenv_int "TOPO_RELAY_MAXD" 6 in
     (* per bit: driver (cell,type) + sink pins (cell, port, index-in-bus) *)
     let drv = Hashtbl.create 8192 and snk = Hashtbl.create 8192 in
     List.iter (fun (cn, c) ->
         let ty = (match member "type" c with `String s -> s | _ -> "") in
         let pd = (match member "port_directions" c with `Assoc a -> a | _ -> []) in
         (match member "connections" c with `Assoc conns ->
            List.iter (fun (port, bits) ->
                let dir = (match List.assoc_opt port pd with Some (`String d) -> d | _ -> "input") in
                (match bits with `List bl ->
                   List.iteri (fun idx b -> match b with `Int bit ->
                       if dir = "output" then Hashtbl.replace drv bit (cn, ty)
                       else (let l = try Hashtbl.find snk bit with Not_found -> [] in
                             Hashtbl.replace snk bit ((cn, port, idx) :: l))
                     | _ -> ()) bl
                 | _ -> ())) conns
          | _ -> ())) cells_j;
     let maxbit = ref 0 in
     Hashtbl.iter (fun b _ -> if b > !maxbit then maxbit := b) drv;
     Hashtbl.iter (fun b _ -> if b > !maxbit then maxbit := b) snk;
     (* CRITICAL: also consider bits that appear ONLY in netnames (Vivado leaves
        "ghost" placeholder bits, e.g. *_CO_UNCONNECTED, that no cell connects).
        If newbit() ignores them it will REUSE such a bit and silently merge our
        relay/replica net with the ghost net -> unroutable phantom arcs. *)
     (match member "netnames" topm with
      | `Assoc nns ->
        List.iter (fun (_, info) -> match member "bits" info with
          | `List bl -> List.iter (function `Int b -> if b > !maxbit then maxbit := b | _ -> ()) bl
          | _ -> ()) nns
      | _ -> ());
     let newbit () = incr maxbit; !maxbit in
     (* nearest free (low-congestion) SLICE site to a target *)
     let take_free (tx, ty) =
       let best = ref None in
       Array.iter (fun s -> if not s.used then begin
           let d = abs (s.sx - tx) + abs (s.sy - ty) in
           (* prefer near + low overflow bin *)
           let bxb = if cong_on then (s.sx - cbx0) / cong_bin else 0 in
           let byb = if cong_on then (s.sy - cby0) / cong_bin else 0 in
           let pen = if cong_on && bxb>=0 && bxb<nbx && byb>=0 && byb<nby
                     then int_of_float (ov rudy.(byb*nbx+bxb)) else 0 in
           let score = d + pen in
           (match !best with Some (bd,_) when bd <= score -> () | _ -> best := Some (score, s))
         end) region_arr;
       match !best with Some (_, s) -> s.used <- true; Some s | None -> None in
     (* distance-ONLY variant: for a fixnet relay that MUST sit next to its
        specific failing sink, nearness beats low congestion -- otherwise the
        congestion penalty banishes the relay far away and it fails to reach the
        sink (there are free-but-congested sites right next to it). *)
     let take_free_near (tx, ty) =
       let best = ref None in
       Array.iter (fun s -> if not s.used then begin
           let d = abs (s.sx - tx) + abs (s.sy - ty) in
           (match !best with Some (bd,_) when bd <= d -> () | _ -> best := Some (d, s))
         end) region_arr;
       match !best with Some (_, s) -> s.used <- true; Some s | None -> None in
     let ftn = ref 0 and rewire = Hashtbl.create 4096 and newcells = ref [] and ftstamps = ref [] in
     (* replication diagnostics: a cluster silently ABANDONED (no free site within
        relay_maxd of its centroid) leaves the whole net on one overloaded driver,
        which is invisible without this counter. *)
     let ft_toofar = ref 0 and ft_nosite = ref 0 and ft_worst = ref "" and ft_worst_n = ref 0 in
     let ft_bygain = ref 0 in
     (* Positions of the cells driving a LUT's INPUTS.  Replicating a LUT does NOT
        come free: the copy needs every input net routed to its new site.  The
        inputs sit near the ORIGINAL driver (the annealer put them there), so a
        replica parked at a distant sink cluster pays that distance on each input
        -- measured, a replica 72 tiles out turned its input arc into 4.3 ns, the
        largest single term on the path, and eth._1018 went 86.7 -> 80.0.  Any
        sound accept test must price the input side too. *)
     let drv_input_pos dcn =
       match Hashtbl.find_opt cell_json_of dcn with
       | Some (`Assoc a) ->
         (match List.assoc_opt "connections" a, List.assoc_opt "port_directions" a with
          | Some (`Assoc conns), Some (`Assoc pdirs) ->
            List.concat_map (fun (port, bits) ->
                match List.assoc_opt port pdirs with
                | Some (`String "input") ->
                  (match bits with
                   | `List bl ->
                     List.filter_map (function
                       | `Int b ->
                         (match Hashtbl.find_opt drv b with
                          | Some (icn, _) -> Hashtbl.find_opt inst2pos icn
                          | None -> None)
                       | _ -> None) bl
                   | _ -> [])
                | _ -> []) conns
          | _ -> [])
       | _ -> [] in
     (* worst-case Manhattan reach from a set of source positions to (x,y) *)
     let worst_reach srcs (x, y) =
       List.fold_left (fun acc (sx, sy) ->
           Stdlib.max acc (abs (sx - x) + abs (sy - y))) 0 srcs in
     (* how much closer than the ORIGINAL driver a far replica must be to be worth
        placing (tiles).  0 = accept any improvement. *)
     let repl_margin = getenv_int "TOPO_REPL_MARGIN" 4 in
     (* TOPO_REPL_GAIN=1 enables driver replication by distance-gain (below).
        DEFAULT OFF: measured on eth-arp it REGRESSES eth._1018 (86.70 MHz with
        replication off -> 78.95 naive / 80.01 true-replica-only / 81.78 with the
        input-aware model).  The annealer has already clustered each LUT with its
        fanin, so any replica site far enough to help the distant sinks is far
        from that fanin, and the input cost cancels the output saving.  To pay
        off, replication must happen INSIDE the anneal, where the fanin can
        migrate with the copy -- not as a post-pass on a frozen placement. *)
     let repl_gain_on = getenv_int "TOPO_REPL_GAIN" 0 <> 0 in
     (* regional buffering: a high-fanout CONTROL net (many CE/R/S sinks) is put
        on a global BUFG so it uses the dedicated clock/enable network instead of
        saturating per-slice CEUSEDMUX/SRUSEDMUX in the fabric.  Bounded by the
        BUFG budget (TOPO_BUFG_MAX); nextpnr preplaces the BUFG. *)
     let bufg_thresh = getenv_int "TOPO_BUFG_FANOUT" 40 in
     let bufg_max = getenv_int "TOPO_BUFG_MAX" 20 in
     let is_ctrl p = p = "CE" || p = "R" || p = "S" || p = "PRE" || p = "CLR" || p = "SR" in
     let nbufg = ref 0 and bufcells = ref [] in
     (* REGION-AWARE buffer choice: a BUFR only drives its OWN clock region
        (~50 CLB rows x half the die), so a control net whose sinks straddle
        clock regions can NEVER be driven by one BUFR -- those arcs fail by
        construction (seen as the $frontend$/BUFR_x/O residual).  Wide nets go
        on a true global BUFG (budget TOPO_BUFG_GMAX, 32 sites minus the
        design's own clocks); single-region nets keep TOPO_BUF_TYPE (BUFR). *)
     let buf_type = getenv_default "TOPO_BUF_TYPE" "BUFR" in
     let region_rows = getenv_int "TOPO_REGION_ROWS" 50 in
     (* Third architectural ceiling: only 12 global clock spines reach any one
        clock region, and the DESIGN's own clocks (BUFG/BUFGCTRL) need theirs
        first -- 16 inserted BUFGs starved the RX recovered clock of a spine
        (design clock net unroutable).  Budget inserted globals to what's left
        under the ceiling; the overflow is handled by the auto-FT driver
        replication in fabric (no dedicated resources). *)
     let design_bufg =
       let seen = Hashtbl.create 16 in
       Hashtbl.iter (fun _ (cn, dty) ->
           if dty = "BUFG" || dty = "BUFGCTRL" then Hashtbl.replace seen cn ()) drv;
       Hashtbl.length seen in
     let gbuf_max = getenv_int "TOPO_BUFG_GMAX" (max 0 (12 - design_bufg - 1)) in
     Printf.eprintf "global budget: %d design BUFG/BUFGCTRL -> %d insertable BUFG\n"
       design_bufg gbuf_max;
     let xmid = List.fold_left (fun a s -> max a s.sx) 0 all_slices / 2 in
     let ngbuf = ref 0 in
     (* ROUTE-FAILURE FEEDBACK -> GLOBAL BUFFER.  TOPO_BUFG_NETS names nets that
        MUST go on a global buffer whatever their fanout, harvested from
        nextpnr's unroutable arcs (ethsoc/harvest_unroutable_nets.py).  The
        fanout threshold alone is not enough: on ethmin the failing arcs were CE
        nets in axis_gmii_tx_inst with fanout well under TOPO_BUFG_FANOUT, yet
        their sinks were spread far enough by the placer (1.85 cells/slice, 72 x
        123 slices) that no fabric route existed.  Fanout measures how BIG a net
        is; it does not measure how far the placer threw it.  Only the router
        knows that, so let the router's failures name the nets.
        TOPO_FIXNETS is the sibling knob and does something DIFFERENT: it
        relays a net through a LUT1 feedthrough toward one failing sink.  Use
        that for a single stubborn arc, this for a net that needs a spine. *)
     let forced_bufg : (int, unit) Hashtbl.t = Hashtbl.create 16 in
     (match Sys.getenv_opt "TOPO_BUFG_NETS" with
      | Some fl when Sys.file_exists fl ->
        let name2bit = Hashtbl.create 4096 in
        (match member "netnames" topm with `Assoc a ->
           List.iter (fun (nm, info) -> match member "bits" info with
             | `List (`Int b :: _) -> Hashtbl.replace name2bit nm b
             | _ -> ()) a
         | _ -> ());
        let last_comp s = match String.rindex_opt s '.' with
          | Some i -> String.sub s (i+1) (String.length s - i - 1) | None -> s in
        let ic = open_in fl in
        let nfound = ref 0 and nmiss = ref 0 in
        (try while true do
           let line = input_line ic in
           let line = match String.index_opt line '#' with
             | Some i -> String.sub line 0 i | None -> line in
           let nm = String.trim line in
           if nm <> "" then
             match (match Hashtbl.find_opt name2bit nm with
                    | Some b -> Some b
                    | None -> Hashtbl.find_opt name2bit (last_comp nm)) with
             | Some b -> Hashtbl.replace forced_bufg b (); incr nfound
             | None -> incr nmiss;
               Printf.eprintf "[place_lef] TOPO_BUFG_NETS: no net matches %S\n" nm
         done with End_of_file -> ());
        close_in ic;
        Printf.eprintf "[place_lef] TOPO_BUFG_NETS: forcing %d net(s) onto globals (%d unmatched)\n%!"
          !nfound !nmiss
      | _ -> ());
     (* buffer the highest-fanout control nets first (within budget); a FORCED
        net is admitted regardless of fanout and sorts ahead of the rest. *)
     let ctrl_nets = Hashtbl.fold (fun bit sinks acc ->
         match Hashtbl.find_opt drv bit with
         | Some (_, dty) when dty <> "BUFG" && dty <> "BUFGCTRL" && dty <> "GND" && dty <> "VCC" ->
           let c = List.length (List.filter (fun (_,p,_) -> is_ctrl p) sinks) in
           if Hashtbl.mem forced_bufg bit then (max c 1 + 1_000_000, bit) :: acc
           else if c >= bufg_thresh && c * 2 >= List.length sinks then (c, bit) :: acc else acc
         | _ -> acc) snk [] in
     (* Per-region BUFR SITE budget: only ~4 BUFR sites per clock-region side.
        If more single-region nets target one region than it has sites, nextpnr
        spills the extra BUFRs into a region with no loads (seen: BUFR_X0Y12/14 =
        region row 3, sinks in rows 0-2) and every arc fails.  Cap BUFRs per
        sink-region (TOPO_BUFR_PER_REGION) and PROMOTE the overflow to BUFG. *)
     let bufr_per_region = getenv_int "TOPO_BUFR_PER_REGION" 4 in
     let rused = Hashtbl.create 8 in
     let regions_of sinks =
       List.sort_uniq compare (List.filter_map (fun (cn,_,_) ->
           match Hashtbl.find_opt inst2pos cn with
           | Some (x, y) -> Some ((if x > xmid then 1 else 0), y / region_rows)
           | None -> None) sinks) in
     let reg_of sinks = match regions_of sinks with r :: _ -> r | [] -> (0, 0) in
     (* BUFHCE middle tier (12/region-side, 168 total): drives its region's
        12-track HCLK row -- BUFG reach within the region, no global spine.
        FOURTH ceiling: those 12 tracks are SHARED with the globals loading the
        region, so per-region BUFH budget = 12 - clocks-in-region - margin.
        Gated by TOPO_BUFH_MAX (0 = off). *)
     let bufh_max = getenv_int "TOPO_BUFH_MAX" 0 in
     let hclk_margin = getenv_int "TOPO_HCLK_MARGIN" 1 in
     let gload = Hashtbl.create 16 in                    (* region -> #clock tracks used *)
     let load_regions sinks =
       List.iter (fun r ->
           Hashtbl.replace gload r (1 + (try Hashtbl.find gload r with Not_found -> 0)))
         (regions_of sinks) in
     (* design clocks: each BUFG/BUFGCTRL-driven net takes a track in every
        region its loads occupy *)
     Hashtbl.iter (fun bit (_, dty) ->
         if dty = "BUFG" || dty = "BUFGCTRL" then
           match Hashtbl.find_opt snk bit with
           | Some sinks -> load_regions sinks | None -> ()) drv;
     let hused = Hashtbl.create 8 and nbufh = ref 0 in
     let region_ok r n =    (* n more tracks in region r stay under the 12-track row *)
       let g = try Hashtbl.find gload r with Not_found -> 0 in
       let h = try Hashtbl.find hused r with Not_found -> 0 in
       g + h + n + hclk_margin <= 12 in
     let take_bufh r =
       Hashtbl.replace hused r (1 + (try Hashtbl.find hused r with Not_found -> 0));
       incr nbufh in
     let add_buf bit btype sel =
       let nb = newbit () in
       let bname = Printf.sprintf "$cebuf$%d" !nbufg in incr nbufg;
       bufcells := (bname, bit, nb, btype) :: !bufcells;
       List.iter (fun (cn,port,idx) -> Hashtbl.replace rewire (cn,port,idx) nb) sel;
       bname in
     (* nextpnr does NOT know a regional buffer must sit in its loads' clock
        region -- left free, it scattered our BUFHCEs into load-less regions
        (BUFHCE_X0Y49/Y76/X1Y10 for loads in left rows 0-2: 435 dead arcs).
        Stamp the BUFHCE site OURSELVES: BUFH sites are (side = x, row = y/12),
        12 per region-side.  SVS places, nextpnr legalises -- as everywhere. *)
     let bufh_sites = match Hashtbl.find_opt fp "BUFH" with Some l -> !l | None -> [] in
     let stamp_bufh (side, row) bname =
       match List.find_opt (fun s -> not s.used && s.sx = side && s.sy / 12 = row)
               bufh_sites with
       | Some s -> s.used <- true;
           ftstamps := Printf.sprintf "%s\t%s/BUFHCE" bname s.sname :: !ftstamps
       | None ->
           Printf.eprintf "no free BUFH site in region (%d,%d) for %s\n" side row bname in
     List.iter (fun (_, bit) ->
         if !nbufg < bufg_max then begin
           let sinks = Hashtbl.find snk bit in
           let regs = regions_of sinks in
           if List.length regs > 1 then begin
             (* WIDE net: one BUFHCE PER SINK REGION, all fed from the source in
                fabric.  Regional-track cost is IDENTICAL to a BUFG entering each
                region, but no global spine / BUFG site is consumed.  Falls back
                to a single BUFG only when a region's HCLK row is full. *)
             if bufh_max > 0 && !nbufh + List.length regs <= bufh_max
                && List.for_all (fun r -> region_ok r 1) regs then
               List.iter (fun r ->
                   let rsinks = List.filter (fun (cn,_,_) ->
                       match Hashtbl.find_opt inst2pos cn with
                       | Some (x, y) -> ((if x > xmid then 1 else 0), y / region_rows) = r
                       | None -> false) sinks in
                   if rsinks <> [] then begin
                     take_bufh r; stamp_bufh r (add_buf bit "BUFHCE" rsinks)
                   end)
                 regs
             else if !ngbuf < gbuf_max then begin
               incr ngbuf; load_regions sinks; ignore (add_buf bit "BUFG" sinks)
             end
           end else begin
             let reg = reg_of sinks in
             let c = try Hashtbl.find rused reg with Not_found -> 0 in
             if c < bufr_per_region then begin
               Hashtbl.replace rused reg (c + 1);
               let bname = add_buf bit buf_type sinks in
               (* Stamp the BUFR site OURSELVES (same reason as BUFHCE below:
                  nextpnr doesn't know a BUFR must sit in its loads' clock
                  region -- left free it landed $cebuf$3 at BUFR_X0Y11/region 2
                  for region-0 loads, 164 dead arcs).  4 BUFR sites per region
                  side: BUFR_X<side>Y<region_row*4 + idx>. *)
               if buf_type = "BUFR" then begin
                 let (side, row) = reg in
                 ftstamps := Printf.sprintf "%s\tBUFR_X%dY%d/BUFR"
                     bname side (row * 4 + c) :: !ftstamps
               end
             end else if bufh_max > 0 && !nbufh < bufh_max && region_ok reg 1 then begin
               take_bufh reg; stamp_bufh reg (add_buf bit "BUFHCE" sinks)
             end else if !ngbuf < gbuf_max then begin
               incr ngbuf; load_regions sinks; ignore (add_buf bit "BUFG" sinks)
             end                                          (* else: fabric replication *)
           end
         end)
       (List.sort (fun (a,_) (b,_) -> compare b a) ctrl_nets);
     Hashtbl.iter (fun bit sinks ->
         match Hashtbl.find_opt drv bit with
         | None -> ()                                   (* top-port (clk/rst): skip *)
         | Some (dcn, dty) ->
           if dty = "BUFG" || dty = "BUFGCTRL" || dty = "GND" || dty = "VCC" then () else
           (* already regionally buffered? then its sinks are rewired -- skip FT *)
           if List.exists (fun s -> Hashtbl.mem rewire s) sinks then () else
           match Hashtbl.find_opt inst2pos dcn with
           | None -> ()
           | Some (dx, dy) ->
             let placed = List.filter (fun (cn,_,_) -> Hashtbl.mem inst2pos cn) sinks in
             let far = List.filter (fun (cn,_,_) ->
                 let (x,y) = Hashtbl.find inst2pos cn in abs (x-dx) + abs (y-dy) > thresh) placed in
             if far = [] && List.length placed < minfo then () else begin
               let relay = if List.length placed >= minfo then placed else far in
               let clusters = Hashtbl.create 16 in
               List.iter (fun (cn,port,idx) ->
                   let (x,y) = Hashtbl.find inst2pos cn in
                   let key = (x / clu, y / clu) in
                   let l = try Hashtbl.find clusters key with Not_found -> [] in
                   Hashtbl.replace clusters key ((cn,port,idx) :: l)) relay;
               Hashtbl.iter (fun _ grp ->
                   let n = List.length grp in
                   let cx = (List.fold_left (fun a (cn,_,_) -> a + fst (Hashtbl.find inst2pos cn)) 0 grp) / n in
                   let cy = (List.fold_left (fun a (cn,_,_) -> a + snd (Hashtbl.find inst2pos cn)) 0 grp) / n in
                   if abs (cx-dx) + abs (cy-dy) <= thresh && List.length placed < minfo then () else
                   match take_free (cx, cy) with
                   | None -> incr ft_nosite
                   | Some site ->
                     (* Accept a replica if it is near the cluster (relay_maxd
                        fast path) OR simply CLOSER to the cluster than the
                        original driver is, by repl_margin.
                        The old test was `d_site > relay_maxd -> abandon`, which
                        threw away 5652 of 5729 clusters on eth-arp: a DENSE
                        cluster saturates its own neighbourhood, so take_free
                        returns a site tens of tiles away and the hard cutoff
                        rejected it -- leaving the whole net on ONE driver
                        reaching every sink (a 72-CE net stuck at 45 tiles).
                        A replica strictly closer than the driver is always a
                        win, however far it sits in absolute terms. *)
                     let d_site = abs (site.sx - cx) + abs (site.sy - cy) in
                     let d_drv = abs (dx - cx) + abs (dy - cy) in
                     (* The distance-gain path is only sound for a TRUE REPLICA:
                        replica_or_relay clones the driver only when it is a LUT,
                        so the cloned copy adds NO logic level.  For a non-LUT
                        driver it falls back to a LUT1 buffer, which is an extra
                        level IN SERIES -- accepting those on distance alone made
                        things worse (2374 LUT1 buffers, eth._1018 86.7 -> 79.0,
                        logic 0.9 -> 1.5 ns).  Those keep the old relay_maxd rule,
                        which is about routability, not timing. *)
                     let is_true_replica = is_lut_ty dty in
                     (* net gain = output-side saving - input-side cost *)
                     let gain_ok =
                       repl_gain_on && is_true_replica &&
                       (let srcs = drv_input_pos dcn in
                        let d_in = worst_reach srcs (site.sx, site.sy)
                        and d_in0 = worst_reach srcs (dx, dy) in
                        (d_drv - d_site) - (d_in - d_in0) > repl_margin) in
                     if d_site > relay_maxd && not gain_ok then begin
                       incr ft_toofar;
                       if List.length grp > !ft_worst_n then begin
                         ft_worst_n := List.length grp;
                         ft_worst := Printf.sprintf "bit %d grp=%d centroid=(%d,%d) nearest free=(%d,%d) d_site=%d d_drv=%d"
                             bit (List.length grp) cx cy site.sx site.sy d_site d_drv
                       end;
                       site.used <- false
                     end
                     else begin
                       if d_site > relay_maxd then incr ft_bygain;
                       let nb = newbit () in
                       let ftname = Printf.sprintf "$feedthrough$%d" !ftn in incr ftn;
                       newcells := (ftname, bit, nb, Some dcn) :: !newcells;
                       ftstamps := Printf.sprintf "%s\t%s/A6LUT" ftname site.sname :: !ftstamps;
                       List.iter (fun (cn,port,idx) -> Hashtbl.replace rewire (cn,port,idx) nb) grp
                     end)
                 clusters
             end) snk;
     (* ROUTE-FAILURE FEEDBACK: TOPO_FIXNETS lists "netname sinkx sinky" lines
        harvested from nextpnr's "Failed to route arc" errors.  For each, relay
        the net's UNSTAMPED sink(s) (the cells nextpnr's repair/DRAM placed, which
        the position-based auto-FT missed) through a LUT1 placed toward the
        reported failing sink SITE -- shortening that specific unroutable arc.
        Deterministic placement (TOPO_SEED) makes this iterate stably. *)
     (match Sys.getenv_opt "TOPO_FIXNETS" with
      | Some fl when Sys.file_exists fl ->
        let name2bits = Hashtbl.create 4096 in
        (match member "netnames" topm with `Assoc a ->
           List.iter (fun (nm, info) -> match member "bits" info with
             | `List bl -> Hashtbl.replace name2bits nm
                 (Array.of_list (List.filter_map (function `Int b -> Some b | _ -> None) bl))
             | _ -> ()) a
         | _ -> ());
        (* "net[bit]" -> (net, bit); "net" -> (net, 0) *)
        let split_bit nm =
          let n = String.length nm in
          if n > 0 && nm.[n-1] = ']' then
            match String.rindex_opt nm '[' with
            | Some i -> (try (String.sub nm 0 i, int_of_string (String.sub nm (i+1) (n-i-2)))
                         with _ -> (nm, 0))
            | None -> (nm, 0)
          else (nm, 0) in
        let ic = open_in fl in
        (try while true do
           (match String.split_on_char ' ' (String.trim (input_line ic)) with
            | nm :: sxs :: sys :: _ ->
              (try
                let sx = int_of_string sxs and sy = int_of_string sys in
                let base, bidx = split_bit nm in
                (* nextpnr prefixes flat net names with a hierarchy path; the JSON
                   netname is the last '.'-component -- try exact then that. *)
                let last_comp s = match String.rindex_opt s '.' with
                  | Some i -> String.sub s (i+1) (String.length s - i - 1) | None -> s in
                let arr_opt = match Hashtbl.find_opt name2bits base with
                  | Some a -> Some a | None -> Hashtbl.find_opt name2bits (last_comp base) in
                let bit_opt = match arr_opt with
                  | Some arr when bidx < Array.length arr -> Some arr.(bidx)
                  | Some arr when Array.length arr > 0 -> Some arr.(0)
                  | _ -> None in
                (match bit_opt with
                 | None -> ()
                 | Some bit ->
                   let sinks = try Hashtbl.find snk bit with Not_found -> [] in
                   let bad = List.filter (fun (cn,_,_) -> not (Hashtbl.mem inst2pos cn)) sinks in
                   let bad = if bad = [] then sinks else bad in
                   let dpos = match Hashtbl.find_opt drv bit with
                     | Some (dcn,_) -> (match Hashtbl.find_opt inst2pos dcn with Some p -> p | None -> (sx,sy))
                     | None -> (sx,sy) in
                   (* place the relay AT the reported failing sink (nearest free
                      low-congestion site to it), so the relay->sink hop is short;
                      the driver->relay hop stays on the original (targetable) net. *)
                   ignore dpos;
                   let mx = sx and my = sy in
                   (match take_free_near (mx, my) with
                    | None -> ()
                    | Some site when abs (site.sx - mx) + abs (site.sy - my) > relay_maxd ->
                      (* no free site near the failing sink (congested region) --
                         don't strand the relay far away; leave the net for the router *)
                      site.used <- false
                    | Some site ->
                      let nb = newbit () in
                      let ftname = Printf.sprintf "$fixft$%d" !ftn in incr ftn;
                      newcells := (ftname, bit, nb, None) :: !newcells;
                      ftstamps := Printf.sprintf "%s\t%s/A6LUT" ftname site.sname :: !ftstamps;
                      List.iter (fun (cn,port,idx) -> Hashtbl.replace rewire (cn,port,idx) nb) bad))
              with _ -> ())
            | _ -> ())
         done with End_of_file -> ()); close_in ic
      | _ -> ());
     (* apply the surgical edit to the JSON cells *)
     let ft_cell bit nb = `Assoc [
         "type", `String "LUT1"; "parameters", `Assoc ["INIT", `String "10"];
         "attributes", `Assoc ["keep", `String "1"];
         "port_directions", `Assoc ["I0", `String "input"; "O", `String "output"];
         "connections", `Assoc ["I0", `List [`Int bit]; "O", `List [`Int nb]] ] in
     let cells_j' =
       List.map (fun (cn, c) ->
           (cn, (match c with `Assoc a ->
              `Assoc (List.map (fun (k,v) ->
                  if k <> "connections" then (k,v) else
                  match v with `Assoc conns ->
                    (k, `Assoc (List.map (fun (port, bits) ->
                         match bits with `List bl ->
                           (port, `List (List.mapi (fun idx b ->
                                match Hashtbl.find_opt rewire (cn,port,idx) with
                                | Some nb -> `Int nb | None -> b) bl))
                         | _ -> (port, bits)) conns))
                  | _ -> (k,v)) a)
            | _ -> c))) cells_j in
     (* regional/global buffer cell, per-net type (BUFR narrow / BUFG wide).
        BUFR/BUFHCE need CE tied high; BUFR also CLR low + BYPASS (no divide). *)
     let buf_cell bit nb btype =
       (* BUFR needs CE=1/CLR=0 tied explicitly (its pack path doesn't tie).
          BUFHCE must have CE UNCONNECTED: pack_clocking_xc7 tie_port()s it and
          asserts port.net==nullptr -- a pre-tied CE crashes the packer. *)
       let params = if btype = "BUFR" then ["parameters", `Assoc ["BUFR_DIVIDE", `String "BYPASS"]] else [] in
       let pdirs = ["I", `String "input"; "O", `String "output"]
         @ (if btype = "BUFR" then ["CE", `String "input"; "CLR", `String "input"] else []) in
       let conns = ["I", `List [`Int bit]; "O", `List [`Int nb]]
         @ (if btype = "BUFR" then ["CE", `List [`String "1"]; "CLR", `List [`String "0"]] else []) in
       `Assoc (("type", `String btype) :: params
               @ ["port_directions", `Assoc pdirs; "connections", `Assoc conns]) in
     let replica_or_relay bit nb dcn_opt =
       match dcn_opt with
       | Some dcn ->
         (match Hashtbl.find_opt cell_json_of dcn with
          | Some (`Assoc a) when
              is_lut_ty (match List.assoc_opt "type" a with Some (`String s) -> s | _ -> "") ->
            (* driver LUT replica: copy all ports, rewire output O -> nb *)
            `Assoc (List.map (fun (k, v) ->
                if k <> "connections" then (k, v) else
                match v with
                | `Assoc conns ->
                  (k, `Assoc (List.map (fun (port, bits) ->
                       if port = "O" then (port, `List [`Int nb]) else (port, bits)) conns))
                | _ -> (k, v)) a)
          | _ -> ft_cell bit nb)
       | None -> ft_cell bit nb in
     let cells_j'' = cells_j'
       @ List.map (fun (ftname, bit, nb, dcn_opt) -> (ftname, replica_or_relay bit nb dcn_opt)) !newcells
       @ List.map (fun (bname, bit, nb, btype) -> (bname, buf_cell bit nb btype)) !bufcells in
     (* NAME every inserted net: without a netnames entry nextpnr's frontend
        auto-names them $frontend$N, making route failures unattributable.
        With names, a failing relay/buffer output is directly identifiable. *)
     let nn_entry nm nb = (nm, `Assoc ["hide_name", `Int 0; "bits", `List [`Int nb];
                                       "attributes", `Assoc []]) in
     let extra_nn =
       List.map (fun (ftname, _bit, nb, _d) -> nn_entry (ftname ^ "_o") nb) !newcells
       @ List.map (fun (bname, _bit, nb, _t) -> nn_entry (bname ^ "_o") nb) !bufcells in
     let topm' = (match topm with `Assoc a ->
         `Assoc (List.map (fun (k,v) ->
             if k = "cells" then (k, `Assoc cells_j'')
             else if k = "netnames" then
               (match v with `Assoc nn -> (k, `Assoc (nn @ extra_nn)) | _ -> (k,v))
             else (k,v)) a)
       | _ -> topm) in
     let j' = (match j with `Assoc a ->
         `Assoc (List.map (fun (k,v) -> if k <> "modules" then (k,v) else
             match v with `Assoc ms -> (k, `Assoc (List.map (fun (nm,m) ->
                 if nm = topname then (nm, topm') else (nm,m)) ms)) | _ -> (k,v)) a)
       | _ -> j) in
     let outj = getenv_default "TOPO_FT_JSON" "/tmp/arp_ft_ocaml.json" in
     Y.to_file outj j';
     let ob = open_out_gen [Open_append; Open_creat] 0o644 bels_path in
     List.iter (fun s -> Printf.fprintf ob "%s\n" s) !ftstamps; close_out ob;
     Printf.eprintf "feedthroughs: %d LUT1 relays + %d buffers (%d %s + %d BUFHCE + %d BUFG wide) -> %s (+%d stamps)\n"
       !ftn !nbufg (!nbufg - !ngbuf - !nbufh) buf_type !nbufh !ngbuf outj (List.length !ftstamps);
     if !ft_toofar > 0 || !ft_nosite > 0 then begin
       Printf.eprintf "replication: %d cluster(s) ABANDONED (no gain over driver), %d with no free site at all, %d accepted by distance-gain\n"
         !ft_toofar !ft_nosite !ft_bygain;
       if !ft_worst <> "" then Printf.eprintf "replication: worst abandoned %s\n" !ft_worst
     end;

     (* ===== CARRY-SLICE COMPLETION (OCaml port of carry_stamp.py) ==========
        Emits a SECOND json (TOPO_STAMPED_JSON) with all BEL stamps applied
        as cell attributes AND every BEL'd CARRY4's slice laid out explicitly.
        nextpnr-xilinx has no site-level LUT routethru, so:
          S[k] LUT-driven  -> stamp the driver at <site>/<slot>6LUT
          S[k] FF/ext      -> identity LUT1 (INIT=10) at the 6LUT, rewire S
          S[k]=GND         -> const-0 LUT1 (INIT=00) fed by a LOCAL net (a
                              0-INIT LUT needs no global GND route)
          DI[k]=GND        -> const-0 LUT1 at the <slot>5LUT (O5->DI), input
                              shared with the 6LUT occupant (fracture rule)
          DI[k] FF/LUT6    -> identity LUT1 at the 5LUT (DI adoption in
                              pack_carry_xc7 covers LUT1-5 only)
          O[k] sum-FF      -> stamp at <site>/<slot>FF
        Plus TARGETED same-slot feedback relays (CARRY_FB_NETS file): a
        counter's bit-0 inverter reading its own slot's Q loses the marginal
        same-tile bounce under congestion; relay via a neighbour slice.
        Blanket relaying of all such pairs REGRESSED 13->65 skips. *)
     let belmap = Hashtbl.create 8192 in
     Array.iter (fun c ->
         if stamp_all || not (skip_kind (kind_of_lef c.Pack_to_lef.pc_lef)) then
         match cell_site.(Hashtbl.find name2id c.Pack_to_lef.pc_name) with
         | Some site -> List.iter (fun (prim, suffix) ->
             Hashtbl.replace belmap prim (site.sname ^ "/" ^ suffix)) c.Pack_to_lef.pc_bels
         | None -> ()) cells;
     List.iter (fun s -> match String.index_opt s '\t' with
         | Some i -> Hashtbl.replace belmap (String.sub s 0 i)
                       (String.sub s (i+1) (String.length s - i - 1))
         | None -> ()) !ftstamps;
     let slot_l = [| "A"; "B"; "C"; "D" |] in
     (* cell tables from the FT-edited json *)
     let ctype = Hashtbl.create 8192 and cconns = Hashtbl.create 8192 in
     let cdirs = Hashtbl.create 8192 in
     List.iter (fun (cn, c) ->
         (match member "type" c with `String t -> Hashtbl.replace ctype cn t | _ -> ());
         (match member "port_directions" c with
          | `Assoc d -> Hashtbl.replace cdirs cn d | _ -> ());
         (match member "connections" c with
          | `Assoc conns ->
            let tbl = Hashtbl.create 8 in
            List.iter (fun (p, bl) -> match bl with
                | `List l -> Hashtbl.replace tbl p (Array.of_list l)
                | _ -> ()) conns;
            Hashtbl.replace cconns cn tbl
          | _ -> ())) cells_j'';
     let conn_of cn p = match Hashtbl.find_opt cconns cn with
       | Some t -> Hashtbl.find_opt t p | None -> None in
     let int_of = function `Int i -> Some i | _ -> None in
     (* net-bit driver (cellname, type) via declared output directions *)
     let cs_drv = Hashtbl.create 8192 in
     let cs_gnd = Hashtbl.create 64 in
     List.iter (fun (cn, _) ->
         let t = try Hashtbl.find ctype cn with Not_found -> "" in
         let dirs = try Hashtbl.find cdirs cn with Not_found -> [] in
         (match Hashtbl.find_opt cconns cn with
          | Some tbl -> Hashtbl.iter (fun p bits ->
              let isout = (match List.assoc_opt p dirs with
                  | Some (`String "output") -> true | _ -> false)
                  || (dirs = [] && (p = "O" || p = "Q" || p = "CO" || p = "G" || p = "P")) in
              if isout then Array.iter (fun b -> match int_of b with
                  | Some i -> Hashtbl.replace cs_drv i (cn, t);
                    if t = "GND" then Hashtbl.replace cs_gnd i ()
                  | None -> ()) bits) tbl
          | None -> ())) cells_j'';
     let cs_occ = Hashtbl.create 8192 in
     Hashtbl.iter (fun cn bel -> Hashtbl.replace cs_occ bel cn) belmap;
     (* rewrites: (cell,port,idx) -> new bit ; created buffer cells *)
     let cs_rewire = Hashtbl.create 256 and cs_cells = ref [] in
     let mk_lut1 name init inbit outbit bel =
       cs_cells := (name, `Assoc [
           "type", `String "LUT1";
           "parameters", `Assoc ["INIT", `String init];
           "attributes", `Assoc ["BEL", `String bel];
           "port_directions", `Assoc ["I0", `String "input"; "O", `String "output"];
           "connections", `Assoc ["I0", `List [`Int inbit]; "O", `List [`Int outbit]]
         ]) :: !cs_cells;
       Hashtbl.replace cs_occ bel name in
     let n_sbuf = ref 0 and n_slut = ref 0 and n_sff = ref 0 and n_dil = ref 0 in
     (* D-bit -> FF cellname, for sum-FF slotting *)
     let ff_by_d = Hashtbl.create 4096 in
     List.iter (fun (cn, _) ->
         let t = try Hashtbl.find ctype cn with Not_found -> "" in
         if String.length t >= 2 && String.sub t 0 2 = "FD" then
           match conn_of cn "D" with
           | Some a when Array.length a > 0 ->
             (match int_of a.(0) with Some i -> Hashtbl.replace ff_by_d i cn | None -> ())
           | _ -> ()) cells_j'';
     let is_lut15 t = t = "LUT1" || t = "LUT2" || t = "LUT3" || t = "LUT4" || t = "LUT5" in
     (* real SLICE site names, for neighbour-bel searches *)
     let cs_slice_sites = Hashtbl.create 65536 in
     List.iter (fun s -> Hashtbl.replace cs_slice_sites s.sname ()) all_slices;
     let cs_free_neighbour site =
       match String.index_opt site 'X', String.index_opt site 'Y' with
       | Some xi, Some yi when yi > xi ->
         (try
            let x = int_of_string (String.sub site (xi+1) (yi - xi - 1)) in
            let y = int_of_string (String.sub site (yi+1) (String.length site - yi - 1)) in
            let r = ref None in
            List.iter (fun (dx, dy) ->
                if !r = None then begin
                  let ns = Printf.sprintf "SLICE_X%dY%d" (x+dx) (y+dy) in
                  if Hashtbl.mem cs_slice_sites ns then
                    Array.iter (fun sl ->
                        let bel = Printf.sprintf "%s/%s6LUT" ns sl in
                        if !r = None && not (Hashtbl.mem cs_occ bel) then r := Some bel)
                      slot_l
                end)
              [(1,0);(-1,0);(0,1);(0,-1);(1,1);(-1,1);(1,-1);(-1,-1);(2,0);(-2,0)];
            !r
          with _ -> None)
       | _ -> None in
     List.iter (fun (cn, _) ->
         if (try Hashtbl.find ctype cn with Not_found -> "") = "CARRY4" then
         match Hashtbl.find_opt belmap cn with
         | Some bel when Filename.basename bel = "CARRY4" ->
           let site = Filename.dirname bel in
           let getp p = match conn_of cn p with Some a -> a | None -> [||] in
           let sarr = getp "S" and diarr = getp "DI" and oarr = getp "O" in
           let slot_in = Array.make 4 None in
           (* local (non-GND) net for const-0 LUT inputs *)
           let local_net () =
             let r = ref None in
             Array.iter (fun b -> match int_of b with
                 | Some i when !r = None && not (Hashtbl.mem cs_gnd i) ->
                   (match Hashtbl.find_opt cs_drv i with
                    | Some (_, t) when String.length t >= 2 && String.sub t 0 2 = "FD" ->
                      r := Some i
                    | _ -> ())
                 | _ -> ()) sarr;
             if !r = None then
               List.iter (fun p -> match conn_of cn p with
                   | Some a when Array.length a > 0 && !r = None ->
                     (match int_of a.(0) with
                      | Some i when not (Hashtbl.mem cs_gnd i) -> r := Some i
                      | _ -> ())
                   | _ -> ()) ["CYINIT"; "CI"];
             !r in
           Array.iteri (fun k b ->
               if k < 4 then match int_of b with
               | None -> ()
               | Some sb ->
                 let slot6 = Printf.sprintf "%s/%s6LUT" site slot_l.(k) in
                 let d = Hashtbl.find_opt cs_drv sb in
                 let is_gnd = Hashtbl.mem cs_gnd sb in
                 (match d with
                  | Some (dn, dt) when String.length dt >= 3 && String.sub dt 0 3 = "LUT"
                                       && not is_gnd ->
                    (match Hashtbl.find_opt cs_occ slot6 with
                     | Some who when who <> dn ->
                       Printf.eprintf "carry-stamp: slot collision %s (%s vs %s)\n" slot6 who dn
                     | _ ->
                       Hashtbl.replace belmap dn slot6; Hashtbl.replace cs_occ slot6 dn;
                       incr n_slut;
                       (* record the occupant's I0 net (its A1 pin) for 5LUT
                          pin sharing -- MUST be I0-first order, not Hashtbl
                          order, or DIgnd picks a net on a different pin and
                          re-creates the A1 double-booking *)
                       (match Hashtbl.find_opt cconns dn with
                        | Some t ->
                          (try List.iter (fun p -> match Hashtbl.find_opt t p with
                               | Some a when Array.length a > 0 ->
                                 (match int_of a.(0) with
                                  | Some i -> slot_in.(k) <- Some i; raise Exit
                                  | None -> ())
                               | _ -> ()) ["I0"; "I1"; "I2"; "I3"; "I4"; "I5"]
                           with Exit -> ())
                        | None -> ()))
                  | _ ->
                    if not (Hashtbl.mem cs_occ slot6) then begin
                      let src, init = if is_gnd then (local_net (), "00")
                                      else (Some sb, "10") in
                      match src with
                      | None -> ()
                      | Some src ->
                        let nb = newbit () in
                        mk_lut1 (Printf.sprintf "%s$Srt$%d" cn k) init src nb slot6;
                        Hashtbl.replace cs_rewire (cn, "S", k) nb;
                        slot_in.(k) <- Some src; incr n_sbuf
                    end)) sarr;
           let pending_gnd = ref [] in
           Array.iteri (fun k b ->
               if k < 4 then match int_of b with
               | None -> ()
               | Some db ->
                 let is_gnd = Hashtbl.mem cs_gnd db in
                 let d = Hashtbl.find_opt cs_drv db in
                 let adoptable = (match d with
                     | Some (_, dt) -> not is_gnd && is_lut15 dt
                     | None -> false) in
                 let slot5 = Printf.sprintf "%s/%s5LUT" site slot_l.(k) in
                 if not adoptable && not (Hashtbl.mem cs_occ slot5) then begin
                   if is_gnd then begin
                     (* const-0: input = the occupant's first net -> shared A1.
                        ILLEGAL when the occupant uses >=6 inputs (Vivado
                        18-608): the fractured LUT's O6 reads the upper INIT
                        half with A6 tied high, and the 5LUT OVERWRITES the
                        lower half -- silently corrupted the SGMII AN
                        comparators (CONFIG_REG_MATCH etc), link never up.
                        Such DI pins are deferred to a per-carry neighbour
                        const-0 (pending_gnd, resolved after the DI loop). *)
                     let occ_n =
                       match Hashtbl.find_opt cs_occ (Printf.sprintf "%s/%s6LUT" site slot_l.(k)) with
                       | None -> 0
                       | Some occ6 ->
                         (match Hashtbl.find_opt cconns occ6 with
                          | None -> 0
                          | Some t ->
                            let ins = Hashtbl.create 8 in
                            Hashtbl.iter (fun p a ->
                                if p <> "O" then Array.iter (fun x -> match int_of x with
                                    | Some i -> Hashtbl.replace ins i () | None -> ()) a) t;
                            Hashtbl.length ins) in
                     match slot_in.(k) with
                     | Some src when occ_n < 6 ->
                       let nb = newbit () in
                       mk_lut1 (Printf.sprintf "%s$DIgnd$%d" cn k) "00" src nb slot5;
                       Hashtbl.replace cs_rewire (cn, "DI", k) nb; incr n_dil
                     | _ -> pending_gnd := k :: !pending_gnd
                   end else begin
                     (* FF/LUT6-driven DI passthrough.  PIN-ALIGN with the 6LUT
                        occupant: nextpnr pin-maps each fractured LUT I0->A1,
                        I1->A2... per cell, so a lone LUT1 with a different net
                        double-books sitewire A1 (SLICE_X2Y65/A1 overused by 2
                        nets -> the "->A1 unroutable" class).  Mirror the
                        occupant's inputs on I0..In-1, DI net on the next pin,
                        INIT = top-input passthrough (upper half ones). *)
                     let occ_ins = ref [] in
                     (match Hashtbl.find_opt cs_occ (Printf.sprintf "%s/%s6LUT" site slot_l.(k)) with
                      | None -> ()
                      | Some occ6 ->
                        (match Hashtbl.find_opt cconns occ6 with
                         | None -> ()
                         | Some t ->
                           List.iter (fun p -> match Hashtbl.find_opt t p with
                               | Some a when Array.length a > 0 ->
                                 (match int_of a.(0) with
                                  | Some i -> occ_ins := i :: !occ_ins | None -> ())
                               | _ -> ()) ["I0"; "I1"; "I2"; "I3"; "I4"; "I5"]));
                     let occ_ins = List.rev !occ_ins in
                     (* HARD fracture rule (Vivado 18-608): >=6 distinct
                        occupant inputs forbid ANY 5LUT in the slot, even if
                        the DI net is among them (truncation doesn't reduce
                        the occupant's pin usage).  nextpnr's di_via_ax
                        handles the pinned-LUT6 case natively. *)
                     let distinct = List.sort_uniq compare occ_ins in
                     if List.length distinct >= 6 then () else begin
                     (* truncate at the DI net if it is already an occupant pin *)
                     let occ_ins =
                       let rec take acc = function
                         | [] -> List.rev acc
                         | x :: _ when x = db -> List.rev acc
                         | x :: tl -> take (x :: acc) tl in
                       take [] occ_ins in
                     let n_in = List.length occ_ins + 1 in
                     if n_in <= 5 then begin
                       let nb = newbit () in
                       let half = 1 lsl (n_in - 1) in
                       let init = String.make half '1' ^ String.make half '0' in
                       let ins = occ_ins @ [db] in
                       let conns = List.mapi (fun i b2 ->
                           (Printf.sprintf "I%d" i, `List [`Int b2])) ins
                           @ ["O", `List [`Int nb]] in
                       let dirs = List.mapi (fun i _ ->
                           (Printf.sprintf "I%d" i, `String "input")) ins
                           @ ["O", `String "output"] in
                       let name = Printf.sprintf "%s$DIrt$%d" cn k in
                       cs_cells := (name, `Assoc [
                           "type", `String (Printf.sprintf "LUT%d" n_in);
                           "parameters", `Assoc ["INIT", `String init];
                           "attributes", `Assoc ["BEL", `String slot5];
                           "port_directions", `Assoc dirs;
                           "connections", `Assoc conns]) :: !cs_cells;
                       Hashtbl.replace cs_occ slot5 name;
                       Hashtbl.replace cs_rewire (cn, "DI", k) nb; incr n_dil
                     end
                     end
                   end
                 end) diarr;
           (* deferred DI=GND pins (illegal 5LUT fractures): ONE const-0 LUT1
              per carry in a free neighbour slice, DI enters via the AX bypass *)
           if !pending_gnd <> [] then begin
             match local_net (), cs_free_neighbour site with
             | Some src, Some rbel ->
               let nb = newbit () in
               mk_lut1 (Printf.sprintf "%s$DIgndx" cn) "00" src nb rbel;
               List.iter (fun k ->
                   Hashtbl.replace cs_rewire (cn, "DI", k) nb; incr n_dil) !pending_gnd
             | _ -> ()
           end;
           Array.iteri (fun k b ->
               if k < 4 then match int_of b with
               | None -> ()
               | Some ob ->
                 match Hashtbl.find_opt ff_by_d ob with
                 | Some fn when not (Hashtbl.mem belmap fn) ->
                   let slotff = Printf.sprintf "%s/%sFF" site slot_l.(k) in
                   if not (Hashtbl.mem cs_occ slotff) then begin
                     Hashtbl.replace belmap fn slotff; Hashtbl.replace cs_occ slotff fn;
                     incr n_sff
                   end
                 | _ -> ()) oarr
         | _ -> ()) cells_j'';
     (* targeted same-slot feedback relays (CARRY_FB_NETS: one net name/line) *)
     let n_fb = ref 0 in
     (match Sys.getenv_opt "CARRY_FB_NETS" with
      | None -> ()
      | Some f when not (Sys.file_exists f) -> ()
      | Some f ->
        let fbn = Hashtbl.create 16 in
        let ic = open_in f in
        (try while true do
             let l = String.trim (input_line ic) in
             if l <> "" then Hashtbl.replace fbn l ()
           done with End_of_file -> close_in ic);
        (* bit -> netname *)
        let bit2name = Hashtbl.create 8192 in
        (match member "netnames" topm' with
         | `Assoc nns -> List.iter (fun (nm, nn) ->
             match member "bits" nn with
             | `List bl -> List.iter (fun b -> match int_of b with
                 | Some i when not (Hashtbl.mem bit2name i) ->
                   Hashtbl.replace bit2name i nm
                 | _ -> ()) bl
             | _ -> ()) nns
         | _ -> ());
        (* real SLICE site set for neighbour lookup *)
        let slice_sites = Hashtbl.create 65536 in
        List.iter (fun s -> Hashtbl.replace slice_sites s.sname ()) all_slices;
        let free_neighbour site =
          match String.index_opt site 'X', String.index_opt site 'Y' with
          | Some xi, Some yi when yi > xi ->
            (try
               let x = int_of_string (String.sub site (xi+1) (yi - xi - 1)) in
               let y = int_of_string (String.sub site (yi+1) (String.length site - yi - 1)) in
               let r = ref None in
               List.iter (fun (dx, dy) ->
                   if !r = None then begin
                     let ns = Printf.sprintf "SLICE_X%dY%d" (x+dx) (y+dy) in
                     if Hashtbl.mem slice_sites ns then
                       Array.iter (fun sl ->
                           let bel = Printf.sprintf "%s/%s6LUT" ns sl in
                           if !r = None && not (Hashtbl.mem cs_occ bel) then r := Some bel)
                         slot_l
                   end)
                 [(1,0);(-1,0);(0,1);(0,-1);(1,1);(-1,1);(1,-1);(-1,-1)];
               !r
             with _ -> None)
          | _ -> None in
        Hashtbl.iter (fun cn bel ->
            let t = try Hashtbl.find ctype cn with Not_found -> "" in
            if String.length t >= 3 && String.sub t 0 3 = "LUT"
               && Filename.check_suffix bel "6LUT" then begin
              let site = Filename.dirname bel in
              let leaf = Filename.basename bel in
              let slot = String.sub leaf 0 1 in
              match Hashtbl.find_opt cconns cn with
              | None -> ()
              | Some tbl -> Hashtbl.iter (fun p a ->
                  if p <> "O" && Array.length a > 0 then
                    match int_of a.(0) with
                    | None -> ()
                    | Some ib ->
                      match Hashtbl.find_opt cs_drv ib with
                      | Some (dn, dt) when String.length dt >= 2 && String.sub dt 0 2 = "FD"
                          && Hashtbl.find_opt belmap dn = Some (site ^ "/" ^ slot ^ "FF")
                          && (match Hashtbl.find_opt bit2name ib with
                              | Some nm -> Hashtbl.mem fbn nm | None -> false) ->
                        (match free_neighbour site with
                         | Some rbel ->
                           let nb = newbit () in
                           mk_lut1 (Printf.sprintf "%s$fbrelay$%s" cn p) "10" ib nb rbel;
                           Hashtbl.replace cs_rewire (cn, p, 0) nb; incr n_fb
                         | None -> ())
                      | _ -> ()) tbl
            end) (Hashtbl.copy belmap));
     (* rebuild: apply belmap as attributes.BEL + cs_rewire to connections *)
     let stamp_attr cn c = match c with
       | `Assoc a ->
         let bel = Hashtbl.find_opt belmap cn in
         let a = List.map (fun (k, v) ->
             if k = "connections" then match v with
               | `Assoc conns -> (k, `Assoc (List.map (fun (port, bits) ->
                   match bits with
                   | `List bl -> (port, `List (List.mapi (fun idx b ->
                       match Hashtbl.find_opt cs_rewire (cn, port, idx) with
                       | Some nb -> `Int nb | None -> b) bl))
                   | _ -> (port, bits)) conns))
               | _ -> (k, v)
             else if k = "attributes" then match bel, v with
               | Some b, `Assoc attrs ->
                 (k, `Assoc (("BEL", `String b) :: List.remove_assoc "BEL" attrs))
               | _ -> (k, v)
             else (k, v)) a in
         let a = if bel <> None && not (List.mem_assoc "attributes" a) then
             a @ ["attributes", `Assoc ["BEL", `String (Option.get bel)]] else a in
         `Assoc a
       | _ -> c in
     let cells_st = List.map (fun (cn, c) -> (cn, stamp_attr cn c)) cells_j''
                    @ List.rev !cs_cells in
     let topm_st = (match topm' with `Assoc a ->
         `Assoc (List.map (fun (k, v) ->
             if k = "cells" then (k, `Assoc cells_st) else (k, v)) a)
       | _ -> topm') in
     let j_st = (match j' with `Assoc a ->
         `Assoc (List.map (fun (k, v) -> if k <> "modules" then (k, v) else
             match v with `Assoc ms -> (k, `Assoc (List.map (fun (nm, m) ->
                 if nm = topname then (nm, topm_st) else (nm, m)) ms)) | _ -> (k, v)) a)
       | _ -> j') in
     let outst = getenv_default "TOPO_STAMPED_JSON" "/tmp/arp_stamped_ocaml.json" in
     Y.to_file outst j_st;
     Printf.eprintf "carry-stamp: %d S-buffers, %d S-LUTs, %d sum-FFs, %d DI-5LUTs, %d fb-relays -> %s\n"
       !n_sbuf !n_slut !n_sff !n_dil !n_fb outst)

(* ---- inert-cell cleanup -------------------------------------------------------
   The yosys netlist goes STRAIGHT into place_lef, so scrub non-physical yosys
   metadata cells here instead of needing a separate yosys pass.  Rule: a
   $-prefixed cell with NO connections (e.g. $scopeinfo -- 102 in the ibex top)
   has no bel and cannot affect the netlist, so drop it.  Real cells, and
   connected $-cells like $specify in blackbox sim models, are left untouched
   (the DSP must still be removed at synth time via -nodsp; that's not a cleanup). *)
let cleanup_netlist (j : Y.t) : Y.t =
  let module U = Yojson.Safe.Util in
  let inert (_, cj) =
    let t = try cj |> U.member "type" |> U.to_string with _ -> "" in
    let noconn = match cj |> U.member "connections" with `Assoc [] | `Null -> true | _ -> false in
    String.length t > 0 && t.[0] = '$' && noconn
  in
  let ndrop = ref 0 in
  let clean_mod mj =
    match mj |> U.member "cells" with
    | `Assoc cells ->
      let kept = List.filter (fun c -> if inert c then (incr ndrop; false) else true) cells in
      `Assoc (List.map (fun (k, v) -> if k = "cells" then (k, `Assoc kept) else (k, v)) (U.to_assoc mj))
    | _ -> mj
  in
  let r =
    match j |> U.member "modules" with
    | `Assoc mods ->
      `Assoc (List.map (fun (k, v) ->
                  if k = "modules"
                  then (k, `Assoc (List.map (fun (mn, mj) -> (mn, clean_mod mj)) mods))
                  else (k, v))
                (U.to_assoc j))
    | _ -> j
  in
  if !ndrop > 0 then Printf.eprintf "[cleanup] dropped %d inert ($-no-conn) cell(s)\n%!" !ndrop;
  r

(* Pre-instantiated primitives (the PCS core's LUT/FF/SRL instances) carry INIT
   as a Verilog literal string ("64'hABCD", "4'h7") rather than a [01] bit-string.
   nextpnr's Property::extract treats INIT as a bit-string and asserts on the ' /
   hex chars (crash at post-routing legalisation: str[i]==S0||S1||Sx||Sz).  The
   yosys netlist feeds place_lef directly, so normalise LUT/FD/SRL INITs here to a
   plain MSB-first bit-string of the literal's declared width -- keeping yosys
   untouched.  GT/MMCM blackbox params are left as strings (nextpnr passes them). *)
let normalise_init (j : Y.t) : Y.t =
  let module U = Yojson.Safe.Util in
  (* hex digit string -> MSB-first bit-string (x/z -> 0), any length. *)
  let hex_bits digits =
    let buf = Buffer.create (4 * String.length digits) in
    String.iter (fun c ->
      let v = match c with
        | '0'..'9' -> Char.code c - Char.code '0'
        | 'a'..'f' -> Char.code c - Char.code 'a' + 10
        | 'A'..'F' -> Char.code c - Char.code 'A' + 10
        | 'x' | 'X' | 'z' | 'Z' -> 0
        | _ -> raise Exit in
      for k = 3 downto 0 do Buffer.add_char buf (if (v lsr k) land 1 = 1 then '1' else '0') done)
      digits;
    Buffer.contents buf in
  (* Verilog literal "N'hV" / "N'bV" / "N'dV" -> MSB-first bit-string of N bits
     (left-pad / low-truncate), else None.  Numeric literals are always numeric,
     so this is safe for LUT/FF INIT *and* GT/MMCM CFG params (nextpnr reads the
     latter via as_int64/write_int_vector, which assert on a string). *)
  let lit_to_bits s =
    match String.index_opt s '\'' with
    | Some q when q > 0 && q + 1 < String.length s ->
      (try
         let width = int_of_string (String.sub s 0 q) in
         let base  = Char.lowercase_ascii s.[q + 1] in
         let digits = String.concat "" (String.split_on_char '_'
                        (String.sub s (q + 2) (String.length s - q - 2))) in
         if width <= 0 then None
         else
           let raw = match base with
             | 'h' -> hex_bits digits
             | 'b' -> String.map (fun c -> match c with 'x'|'X'|'z'|'Z' -> '0' | c -> c) digits
             | 'd' when width <= 62 ->
                 let v = Int64.of_string digits in
                 String.init width (fun i ->
                   if Int64.logand (Int64.shift_right_logical v (width - 1 - i)) 1L = 1L then '1' else '0')
             | _ -> raise Exit in
           let n = String.length raw in
           Some (if n >= width then String.sub raw (n - width) width
                 else String.make (width - n) '0' ^ raw)
       with _ -> None)
    | _ -> None in
  let nfix = ref 0 in
  (* Normalise EVERY Verilog-numeric-literal param on EVERY cell (a genuine
     string param -- an enum/mode -- carries no apostrophe, so is left alone). *)
  let fix_cell (cn, cj) =
    match cj with
    | `Assoc a ->
      (cn, `Assoc (List.map (fun (k, v) ->
         if k <> "parameters" then (k, v)
         else match v with
           | `Assoc ps ->
             (k, `Assoc (List.map (fun (pk, pv) ->
                match pv with
                | `String s when String.contains s '\'' ->
                  (match lit_to_bits s with
                   | Some bits -> incr nfix; (pk, `String bits)
                   | None -> (pk, pv))
                | _ -> (pk, pv)) ps))
           | _ -> (k, v)) a))
    | _ -> (cn, cj) in
  let fix_mod mj =
    match mj |> U.member "cells" with
    | `Assoc cells ->
      `Assoc (List.map (fun (k, v) ->
                  if k = "cells" then (k, `Assoc (List.map fix_cell cells)) else (k, v))
                (U.to_assoc mj))
    | _ -> mj in
  let r = match j |> U.member "modules" with
    | `Assoc mods ->
      `Assoc (List.map (fun (k, v) ->
                  if k = "modules"
                  then (k, `Assoc (List.map (fun (mn, mj) -> (mn, fix_mod mj)) mods))
                  else (k, v)) (U.to_assoc j))
    | _ -> j in
  if !nfix > 0 then
    Printf.eprintf "[cleanup] normalised %d Verilog-literal INIT param(s) to bit-strings\n%!" !nfix;
  r

(* ---- automatic hold buffering (FPGA_HOLD_LUT1=1) ------------------------------
   Insert an identity LUT1 (INIT="10") on every DIRECT FF->FF net — a register Q
   driving a register D with no cell between.  These zero-logic paths have no
   propagation delay to absorb clock skew, so on a skewed clock tree they violate
   hold, and nextpnr does NO hold fixing (this is the JTAG tck DMI-shift-register
   failure: dr_q/address_q).  The buffer adds a LUT delay -> hold margin.  Runs at
   the START of place_lef, on the netlist tree that feeds BOTH the placer and the
   emitted ft.json, so the buffers are placed + routed and nothing folds them
   (of_circuit's identity-collapse is upstream).  One shared buffer per source Q
   bit (its D fanout all reroute through it); non-FF sinks of that Q stay direct. *)
let insert_hold_buffers (j : Y.t) : Y.t =
  if (try Sys.getenv "FPGA_HOLD_LUT1" with Not_found -> "") <> "1" then j
  else begin
    let module U = Yojson.Safe.Util in
    let is_ff t = List.mem t [ "FDRE"; "FDCE"; "FDPE"; "FDSE" ] in
    let bit1 conn = match conn with `List [ b ] -> Some b | _ -> None in
    (* TARGETED buffering (Phase 2c): if NEXTPNR_HOLD_TARGETS points at nextpnr's
       exported hold-target list (net<TAB>capture_cell.port<TAB>slack), buffer ONLY
       those capture FFs instead of blanket every FF->FF.  None -> blanket (old). *)
    let targets =
      match Sys.getenv_opt "NEXTPNR_HOLD_TARGETS" with
      | Some f when Sys.file_exists f ->
        let h = Hashtbl.create 512 in
        (try let ic = open_in f in
           (try while true do
              let line = input_line ic in
              (match String.split_on_char '\t' line with
               | _ :: pin :: _ ->
                 let cell = match String.rindex_opt pin '.' with
                   | Some i -> String.sub pin 0 i | None -> pin in
                 Hashtbl.replace h cell ()
               | _ -> ())
            done with End_of_file -> ()); close_in ic
         with _ -> ());
        Printf.eprintf "[hold_lut1] targeting %d hold endpoint(s) from %s\n%!"
          (Hashtbl.length h) f;
        Some h
      | _ -> None
    in
    (* blanket mode (no targets): buffer only DIRECT FF->FF (D bit == some Q bit).
       targeted mode: nextpnr already picked the hold-critical FFs (their D may be
       driven by logic, not a raw Q), so buffer each listed FF's D directly. *)
    let want_ff cn is_ff2ff = match targets with None -> is_ff2ff | Some h -> Hashtbl.mem h cn in
    let modules = j |> U.member "modules" |> U.to_assoc in
    let ncells (_, mj) = try List.length (mj |> U.member "cells" |> U.to_assoc) with _ -> 0 in
    let topname, _ =
      List.fold_left (fun best m -> if ncells m > ncells best then m else best)
        (List.hd modules) (List.tl modules) in
    let remap modname mj =
      if modname <> topname then mj else begin
        let cells = mj |> U.member "cells" |> U.to_assoc in
        (* Don't hardcode LUT1's port directions — copy them from an existing
           LUT1 already in the netlist (bir_to_nextpnr_json populated those from
           the central cell-port DB / XIL_PRIM_PORTS_JSON), so a $holdbuf matches
           exactly what the flow emits.  Fall back only if the design has none. *)
        let lut1_pd =
          let found = ref None in
          List.iter (fun (_, cj) ->
            if !found = None
               && (try cj |> U.member "type" |> U.to_string with _ -> "") = "LUT1"
            then match cj |> U.member "port_directions" with
              | `Null -> () | v -> found := Some v)
            cells;
          match !found with
          | Some v -> v
          | None -> `Assoc [ "I0", `String "input"; "O", `String "output" ]
        in
        (* highest int bit id + set of FF-Q output bits *)
        let maxbit = ref 0 and ff_q = Hashtbl.create 4096 in
        List.iter (fun (_, cj) ->
          let t = try cj |> U.member "type" |> U.to_string with _ -> "" in
          List.iter (fun (_, conn) ->
            List.iter (function `Int i -> if i > !maxbit then maxbit := i | _ -> ())
              (match conn with `List l -> l | _ -> []))
            (try cj |> U.member "connections" |> U.to_assoc with _ -> []);
          if is_ff t then
            match bit1 (cj |> U.member "connections" |> U.member "Q") with
            | Some (`Int b) -> Hashtbl.replace ff_q b () | _ -> ())
          cells;
        let buf_bit : (int, int) Hashtbl.t = Hashtbl.create 1024 in
        let new_cells = ref [] and cnt = ref 0 in
        let get_buf src =
          match Hashtbl.find_opt buf_bit src with
          | Some n -> n
          | None ->
            incr maxbit; let n = !maxbit in
            Hashtbl.replace buf_bit src n;
            new_cells := (Printf.sprintf "$holdbuf$%d" src,
              `Assoc [ "type", `String "LUT1";
                       "parameters", `Assoc [ "INIT", `String "10" ];
                       "port_directions", lut1_pd;
                       "connections", `Assoc [ "I0", `List [ `Int src ];
                                               "O",  `List [ `Int n ] ] ]) :: !new_cells;
            incr cnt; n
        in
        let cells' = List.map (fun (cn, cj) ->
          let t = try cj |> U.member "type" |> U.to_string with _ -> "" in
          if not (is_ff t) then (cn, cj)
          else match bit1 (cj |> U.member "connections" |> U.member "D") with
            | Some (`Int b) when want_ff cn (Hashtbl.mem ff_q b) ->
              let n = get_buf b in
              let conns' = List.map (fun (p, v) -> if p = "D" then (p, `List [ `Int n ]) else (p, v))
                             (cj |> U.member "connections" |> U.to_assoc) in
              (cn, `Assoc (List.map (fun (k, v) -> if k = "connections" then (k, `Assoc conns') else (k, v))
                             (U.to_assoc cj)))
            | _ -> (cn, cj)) cells in
        Printf.eprintf "[hold_lut1] inserted %d FF->FF hold buffers in %s\n%!" !cnt modname;
        `Assoc (List.map (fun (k, v) -> if k = "cells" then (k, `Assoc (cells' @ List.rev !new_cells)) else (k, v))
                  (U.to_assoc mj))
      end
    in
    let modules' = List.map (fun (mn, mj) -> (mn, remap mn mj)) modules in
    `Assoc (List.map (fun (k, v) -> if k = "modules" then (k, `Assoc modules') else (k, v)) (U.to_assoc j))
  end

(* CLI / file entry: read floorplan + netlist json from disk. *)
(* Delete the GT pad pseudo-IBUF/OBUF cells, identified STRUCTURALLY.

   Vivado models a transceiver's dedicated pins (GTXRXP/GTXRXN/GTXTXP/GTXTXN and
   the IBUFDS_GTE2 refclk pair) as pseudo IBUF/OBUF cells on IPAD/OPAD sites.
   They are not fabric IO buffers: nextpnr's pack_io would try to place them in
   an IOB and pack_gt wants the raw pad nets, so they have to go.  Left in, the
   placer assigns one to an IPAD site and nextpnr dies with

     ERROR: No Bel named 'IPAD_X2Y8/IOB33/INBUF_EN' located for this chip

   stamp_placement.py finds them by reading a VIVADO placement dump for
   IPAD_/OPAD_ sites, which is fine for the placement-replay path and useless
   for a Vivado-free flow.  So identify them from CONNECTIVITY instead: any
   IBUF/OBUF whose pad-side net is also touched by a GT primitive's dedicated
   pin.  Same merge either way -- rewrite every use of the buffer's fabric-side
   bit to the pad-side bit.

   (Was ethsoc/stitch_gt_pad_buffers.py, a separate pipeline stage feeding
   place_lef a temp json; it is a netlist prepass like the others, so it lives
   here now and there is one less file to keep in step.) *)
let stitch_gt_pad_buffers (j : Y.t) : Y.t =
  let module U = Yojson.Safe.Util in
  let gt_types = [ "GTXE2_CHANNEL"; "GTXE2_COMMON"; "IBUFDS_GTE2";
                   "GTPE2_CHANNEL"; "GTPE2_COMMON"; "GTHE2_CHANNEL"; "GTHE2_COMMON" ] in
  let gt_pad_pins = [ "GTXRXP"; "GTXRXN"; "GTXTXP"; "GTXTXN";
                      "GTPRXP"; "GTPRXN"; "GTPTXP"; "GTPTXN";
                      "GTHRXP"; "GTHRXN"; "GTHTXP"; "GTHTXN";
                      "I"; "IB" ] in
  let buf_types = [ "IBUF"; "OBUF"; "IBUFDS"; "OBUFDS" ] in
  let bits_of e = match e with
    | `List l -> List.filter_map (function `Int i -> Some i | _ -> None) l
    | _ -> [] in
  let nstitch = ref 0 in
  let do_mod mj =
    match mj |> U.member "cells" with
    | `Assoc cells when cells <> [] ->
      let ty cj = try cj |> U.member "type" |> U.to_string with _ -> "" in
      let conns cj = match cj |> U.member "connections" with `Assoc c -> c | _ -> [] in
      let pad_bits = Hashtbl.create 64 in
      List.iter (fun (_, cj) ->
          if List.mem (ty cj) gt_types then
            List.iter (fun (p, e) ->
                if List.mem p gt_pad_pins then
                  List.iter (fun b -> Hashtbl.replace pad_bits b ()) (bits_of e))
              (conns cj)) cells;
      if Hashtbl.length pad_bits = 0 then mj
      else begin
        (* victim -> (pad-side bit kept, fabric-side bit dropped) *)
        let remap = Hashtbl.create 16 in
        let victims = Hashtbl.create 16 in
        List.iter (fun (cn, cj) ->
            if List.mem (ty cj) buf_types then
              let c = conns cj in
              match List.assoc_opt "I" c, List.assoc_opt "O" c with
              | Some ie, Some oe ->
                (match bits_of ie, bits_of oe with
                 | bi :: _, bo :: _ ->
                   (* IBUF: pad on I, fabric on O.  OBUF: the other way round. *)
                   if Hashtbl.mem pad_bits bi then begin
                     Hashtbl.replace victims cn (); Hashtbl.replace remap bo bi; incr nstitch;
                     Printf.eprintf "[gtstitch] %s (kept pad bit %d, dropped %d)\n%!" cn bi bo
                   end else if Hashtbl.mem pad_bits bo then begin
                     Hashtbl.replace victims cn (); Hashtbl.replace remap bi bo; incr nstitch;
                     Printf.eprintf "[gtstitch] %s (kept pad bit %d, dropped %d)\n%!" cn bo bi
                   end
                 | _ -> ())
              | _ -> ()) cells;
        if Hashtbl.length victims = 0 then mj
        else begin
          (* follow drop->keep chains so order of removal cannot matter *)
          let rec resolve b n =
            if n > 16 then b
            else match Hashtbl.find_opt remap b with Some k -> resolve k (n + 1) | None -> b in
          let fix e = match e with
            | `List l -> `List (List.map (function `Int i -> `Int (resolve i 0) | x -> x) l)
            | x -> x in
          let fix_bits kv =
            List.map (fun (k, v) -> if k = "bits" then (k, fix v) else (k, v)) kv in
          let cells' =
            List.filter_map (fun (cn, cj) ->
                if Hashtbl.mem victims cn then None
                else Some (cn, `Assoc (List.map (fun (k, v) ->
                    if k <> "connections" then (k, v)
                    else (k, match v with
                        | `Assoc c -> `Assoc (List.map (fun (p, e) -> (p, fix e)) c)
                        | x -> x)) (U.to_assoc cj)))) cells in
          `Assoc (List.map (fun (k, v) ->
              match k with
              | "cells" -> (k, `Assoc cells')
              | "ports" | "netnames" ->
                (k, match v with
                    | `Assoc entries ->
                      `Assoc (List.map (fun (n, ej) -> (n, `Assoc (fix_bits (U.to_assoc ej)))) entries)
                    | x -> x)
              | _ -> (k, v)) (U.to_assoc mj))
        end
      end
    | _ -> mj in
  let r =
    match j |> U.member "modules" with
    | `Assoc mods ->
      `Assoc (List.map (fun (k, v) ->
          if k = "modules"
          then (k, `Assoc (List.map (fun (mn, mj) -> (mn, do_mod mj)) mods))
          else (k, v)) (U.to_assoc j))
    | _ -> j in
  Printf.eprintf "[gtstitch] stitched %d GT pad buffer(s)\n%!" !nstitch;
  r

(* `opt_merge -share_all` (run after flatten in the open flow) merges ANY two
   identical cells -- including the two data LUTs of one MUXF7 when they compute
   the same function.  The mux is then left with I0 and I1 tied to the same net.

   That is fatal on 7-series.  A MUXF7 data input is not general routing: it is
   hard-wired to a fixed LUT pair -- F7AMUX <- A6LUT+B6LUT, F7BMUX <- C6LUT+D6LUT,
   with F8MUX combining the two F7s.  Pack_to_lef.absorb_mux7 absorbs the shared
   LUT into ONE lane, and its "not already absorbed" guard then skips the other
   lane, which is left with no instance to bind.  Its dedicated sitewire has no
   driver and the router reports the impossible arc

       SITEWIRE/SLICE_XnYm/B6LUT_O6 -> SITEWIRE/SLICE_XnYm/A6LUT_O6

   -- LUT output to LUT output.  That is NOT congestion: no amount of rip-up,
   rerouting or global-buffer promotion can fix it, because the path does not
   exist in the device.  (This is what the harvested "unroutable nets" list was
   really full of; every entry was a .f0/.f1 wide-mux net.)  It cannot be
   repaired inside pack_to_lef either, which binds EXISTING instances to bels
   and cannot conjure the missing LUT.

   So restore the second lane here, before packing: clone the shared driver,
   give the clone a fresh output net, and rewire the mux's I1 to it.  Same
   function, same inputs; it costs one LUT per degenerate mux, which is exactly
   what opt_merge saved.  The SR-inverter sharing that -share_all was added for
   is untouched.

   Collapsing the mux instead is NOT viable: a MUXF8 data input must be driven
   by a MUXF7, so deleting the F7 just moves the illegality up a level. *)
(* REPLICATE A MUXF7 SHARED BY SEVERAL MUXF8s.
 *
 * A MUXF7 output reaches an F8MUX only over the dedicated intra-slice path, so
 * one MUXF7 can feed at most ONE MUXF8 -- they must sit in the same slice, and
 * a slice has one F8MUX.  When synthesis shares a MUXF7 between two MUXF8s the
 * netlist is unbuildable as written: pack_to_lef's mux grouping absorbs the
 * MUXF7 into the FIRST MUXF8's slice (its `not (already absorbed)` guard stops
 * the second claiming it), and the second consumer is then physically
 * unreachable.  Measured: net core.rx_ack_..._MUXF8_O_I0 driven by an F7BMUX
 * in SLICE_X8Y73 with a second sink at SLICE_X9Y74/F8MUX, which the router can
 * only skip:
 *
 *     SKIP_FAILED_ARCS: failed to route arc 1 of net '...MUXF8_O_I0'
 *     (SITEWIRE/SLICE_X8Y73/F7BMUX_OUT -> SITEWIRE/SLICE_X9Y74/F7AMUX_OUT)
 *
 * No placement fixes this -- the two consumers are in different slices by
 * necessity.  Give each its own copy: clone the MUXF7 and its two driving LUTs
 * for every consumer after the first, so each MUXF8 group packs a private
 * subtree.  The clones' LUT inputs stay on the original source nets (extra
 * fanout on ordinary routing, which is routable; the F7->F8 hop is not). *)
let replicate_shared_muxf7 (j : Y.t) : Y.t =
  let module U = Yojson.Safe.Util in
  let ncl = ref 0 in
  let ty_of cj = try cj |> U.member "type" |> U.to_string with _ -> "" in
  let bits_of e = match e with
    | `List l -> List.filter_map (function `Int i -> Some i | _ -> None) l
    | _ -> [] in
  let do_mod mj =
    match mj |> U.member "cells" with
    | `Assoc cells when cells <> [] ->
      let byname = Hashtbl.create 4096 in
      List.iter (fun (cn, cj) -> Hashtbl.replace byname cn cj) cells;
      let maxbit = ref 1 in
      List.iter (fun (_, cj) ->
          match cj |> U.member "connections" with
          | `Assoc cs -> List.iter (fun (_, e) ->
              List.iter (fun b -> if b > !maxbit then maxbit := b) (bits_of e)) cs
          | _ -> ()) cells;
      (* driver of each bit, and the MUXF8 consumers of each bit *)
      let drv = Hashtbl.create 4096 in
      let f8_users : (int, (string * string) list ref) Hashtbl.t = Hashtbl.create 256 in
      List.iter (fun (cn, cj) ->
          let dirs = try cj |> U.member "port_directions" |> U.to_assoc with _ -> [] in
          match cj |> U.member "connections" with
          | `Assoc cs ->
            List.iter (fun (p, e) ->
                let out = (match List.assoc_opt p dirs with
                           | Some (`String "output") -> true | _ -> false) in
                List.iter (fun b ->
                    if out then Hashtbl.replace drv b (cn, p)
                    else if ty_of cj = "MUXF8" && (p = "I0" || p = "I1") then begin
                      let l = try Hashtbl.find f8_users b
                              with Not_found -> let r = ref [] in Hashtbl.replace f8_users b r; r in
                      l := (cn, p) :: !l
                    end) (bits_of e)) cs
          | _ -> ()) cells;
      let extra = ref [] and rewire = ref [] in
      Hashtbl.iter (fun b users ->
          match Hashtbl.find_opt drv b with
          | Some (mn, _) when ty_of (Hashtbl.find byname mn) = "MUXF7"
                              && List.length !users > 1 ->
            (* keep the first consumer on the original; clone for the rest *)
            List.iteri (fun i (ucell, uport) ->
                if i > 0 then begin
                  let clone_cell src suffix =
                    let cj = Hashtbl.find byname src in
                    let nm = Printf.sprintf "%s_rep%d_%s" src i suffix in
                    (nm, cj) in
                  (* new output bit for the cloned mux *)
                  incr maxbit; let nb = !maxbit in
                  let (m7name, m7j) = clone_cell mn "m7" in
                  (* clone the two feeding LUTs, giving each a fresh output net *)
                  let conns = try m7j |> U.member "connections" |> U.to_assoc with _ -> [] in
                  let conns' = List.map (fun (p, e) ->
                      if p = "O" then (p, `List [ `Int nb ])
                      else match p, bits_of e with
                        | ("I0" | "I1"), [ ib ] ->
                          (match Hashtbl.find_opt drv ib with
                           | Some (ln, _) when (let lt = String.uppercase_ascii (ty_of (Hashtbl.find byname ln)) in
                                                String.length lt >= 3 && String.sub lt 0 3 = "LUT") ->
                             incr maxbit; let lb = !maxbit in
                             let lj = Hashtbl.find byname ln in
                             let lconns = try lj |> U.member "connections" |> U.to_assoc with _ -> [] in
                             let lconns' = List.map (fun (lp, le) ->
                                 if String.uppercase_ascii lp = "O" then (lp, `List [ `Int lb ]) else (lp, le)) lconns in
                             let lname = Printf.sprintf "%s_rep%d_%s" ln i p in
                             extra := (lname, `Assoc (List.map (fun (k, v) ->
                                 if k = "connections" then (k, `Assoc lconns') else (k, v))
                                 (U.to_assoc lj))) :: !extra;
                             (p, `List [ `Int lb ])
                           | _ -> (p, e))
                        | _ -> (p, e)) conns in
                  extra := (m7name, `Assoc (List.map (fun (k, v) ->
                      if k = "connections" then (k, `Assoc conns') else (k, v))
                      (U.to_assoc m7j))) :: !extra;
                  rewire := (ucell, uport, nb) :: !rewire;
                  incr ncl
                end) (List.rev !users)
          | _ -> ()) f8_users;
      let cells' = List.map (fun (cn, cj) ->
          let rs = List.filter (fun (u, _, _) -> u = cn) !rewire in
          if rs = [] then (cn, cj)
          else match cj |> U.member "connections" with
            | `Assoc cs ->
              let cs' = List.map (fun (p, e) ->
                  match List.find_opt (fun (_, up, _) -> up = p) rs with
                  | Some (_, _, nb) -> (p, `List [ `Int nb ])
                  | None -> (p, e)) cs in
              (cn, `Assoc (List.map (fun (k, v) ->
                   if k = "connections" then (k, `Assoc cs') else (k, v)) (U.to_assoc cj)))
            | _ -> (cn, cj)) cells in
      `Assoc (List.map (fun (k, v) ->
          if k = "cells" then (k, `Assoc (cells' @ List.rev !extra)) else (k, v))
          (U.to_assoc mj))
    | _ -> mj in
  let out = match j |> U.member "modules" with
    | `Assoc mods ->
      `Assoc (List.map (fun (k, v) ->
          if k = "modules" then (k, `Assoc (List.map (fun (mn, mj) -> (mn, do_mod mj)) mods))
          else (k, v)) (U.to_assoc j))
    | _ -> j in
  if !ncl > 0 then
    Printf.eprintf "[place_lef] replicated %d shared MUXF7 subtree(s) for extra MUXF8 consumers\n%!" !ncl;
  out

(* REPLICATE A CARRY CHAIN SHARED BY TWO SUCCESSORS.
 *
 * Same fault as the shared MUXF7, in the other dedicated-path structure.  A
 * CARRY4's CO[3] reaches CIN only over the cascade to the slice DIRECTLY ABOVE
 * in the same column, so it can feed exactly one successor.  `opt_merge
 * -share_all` merges the near-identical alu_lts/alu_ltu comparator chains and
 * produces a CO[3] with two CI consumers -- unbuildable:
 *
 *     SKIP_FAILED_ARCS: net 'core.soc.cpu.alu_lts_CARRY4_CO_CO[7]'
 *     (SITEWIRE/SLICE_X5Y66/CARRY4_CO3 -> SITEWIRE/SLICE_X6Y66/CIN)
 *
 * opt_merge cannot simply be dropped: without it abc leaves a reset inverter
 * per flop, every SR net has fanout 1, and placement cannot reunite them --
 * measured 72 unroutable SRUSEDMUX arcs versus this ONE.  So keep the merge and
 * undo the illegal sharing here.
 *
 * Cloning only the shared CARRY4 would not help: its own CI is fed by the
 * previous CARRY4's CO[3], another dedicated link, so the sharing would just
 * move one rung up.  Walk back to the chain ROOT (a CARRY4 whose CI is not
 * driven by another CARRY4's CO[3]) and clone the whole run, plus each rung's
 * S/DI driving LUTs, so the second consumer gets a private chain it can place
 * in its own column.  The root's CI/CYINIT stays on the original net -- that
 * one is ordinary routing. *)
let replicate_shared_carry (j : Y.t) : Y.t =
  let module U = Yojson.Safe.Util in
  let nch = ref 0 and nrung = ref 0 in
  let ty_of cj = try cj |> U.member "type" |> U.to_string with _ -> "" in
  let bits_of e = match e with
    | `List l -> List.filter_map (function `Int i -> Some i | _ -> None) l
    | _ -> [] in
  let do_mod mj =
    match mj |> U.member "cells" with
    | `Assoc cells when cells <> [] ->
      let byname = Hashtbl.create 4096 in
      List.iter (fun (cn, cj) -> Hashtbl.replace byname cn cj) cells;
      let maxbit = ref 1 in
      List.iter (fun (_, cj) -> match cj |> U.member "connections" with
          | `Assoc cs -> List.iter (fun (_, e) ->
              List.iter (fun b -> if b > !maxbit then maxbit := b) (bits_of e)) cs
          | _ -> ()) cells;
      let drv = Hashtbl.create 4096 in
      List.iter (fun (cn, cj) ->
          let dirs = try cj |> U.member "port_directions" |> U.to_assoc with _ -> [] in
          match cj |> U.member "connections" with
          | `Assoc cs -> List.iter (fun (p, e) ->
              match List.assoc_opt p dirs with
              | Some (`String "output") ->
                List.iteri (fun i b -> Hashtbl.replace drv b (cn, p, i)) (bits_of e)
              | _ -> ()) cs
          | _ -> ()) cells;
      let co3 cn = match (Hashtbl.find byname cn) |> U.member "connections" with
        | `Assoc cs -> (match List.assoc_opt "CO" cs with
            | Some e -> (match bits_of e with [_;_;_;b] -> Some b | _ -> None)
            | None -> None)
        | _ -> None in
      let ci_of cn = match (Hashtbl.find byname cn) |> U.member "connections" with
        | `Assoc cs -> (match List.assoc_opt "CI" cs with
            | Some e -> (match bits_of e with b :: _ -> Some b | _ -> None)
            | None -> None)
        | _ -> None in
      (* CI consumers per bit *)
      let ci_users = Hashtbl.create 256 in
      List.iter (fun (cn, cj) ->
          if ty_of cj = "CARRY4" then
            match ci_of cn with
            | Some b -> let l = try Hashtbl.find ci_users b with Not_found ->
                          let r = ref [] in Hashtbl.replace ci_users b r; r in
                        l := cn :: !l
            | None -> ()) cells;
      let extra = ref [] and rewire = ref [] in
      List.iter (fun (cn, cj) ->
          if ty_of cj <> "CARRY4" then () else
          match co3 cn with
          | None -> ()
          | Some b ->
            let users = try !(Hashtbl.find ci_users b) with Not_found -> [] in
            if List.length users > 1 then begin
              (* chain from root down to cn *)
              let rec back acc cur =
                match ci_of cur with
                | Some cb ->
                  (match Hashtbl.find_opt drv cb with
                   | Some (pn, "CO", 3) when ty_of (Hashtbl.find byname pn) = "CARRY4" ->
                     back (cur :: acc) pn
                   | _ -> cur :: acc)
                | None -> cur :: acc in
              let chain = back [] cn in
              List.iteri (fun i ucell ->
                  if i > 0 then begin
                    incr nch;
                    let prev_co = ref None in
                    List.iter (fun rung ->
                        incr nrung;
                        let rj = Hashtbl.find byname rung in
                        let rconns = try rj |> U.member "connections" |> U.to_assoc with _ -> [] in
                        (* fresh CO bits for this clone *)
                        let newco = List.map (fun _ -> incr maxbit; !maxbit)
                            (match List.assoc_opt "CO" rconns with
                             | Some e -> bits_of e | None -> []) in
                        let rconns' = List.map (fun (p, e) ->
                            match p with
                            | "CO" when newco <> [] -> (p, `List (List.map (fun b -> `Int b) newco))
                            | "O" -> (* sum outputs: fresh nets, unused by the clone *)
                              (p, `List (List.map (fun _ -> incr maxbit; `Int !maxbit) (bits_of e)))
                            | "CI" -> (match !prev_co with
                                | Some pb -> (p, `List [ `Int pb ])   (* chain to our clone *)
                                | None -> (p, e))                     (* root: original net *)
                            | ("S" | "DI") ->
                              (* clone the driving LUTs so the copy is independent *)
                              (p, `List (List.map (fun ib ->
                                   match Hashtbl.find_opt drv ib with
                                   | Some (ln, _, _) when (let lt = String.uppercase_ascii
                                                             (ty_of (Hashtbl.find byname ln)) in
                                                           String.length lt >= 3 && String.sub lt 0 3 = "LUT")
                                       && Hashtbl.mem byname ln ->
                                     incr maxbit; let nb = !maxbit in
                                     let lj = Hashtbl.find byname ln in
                                     let lc = try lj |> U.member "connections" |> U.to_assoc with _ -> [] in
                                     let lc' = List.map (fun (lp, le) ->
                                         if String.uppercase_ascii lp = "O" then (lp, `List [ `Int nb ]) else (lp, le)) lc in
                                     extra := (Printf.sprintf "%s_carrep%d" ln i,
                                               `Assoc (List.map (fun (k, v) ->
                                                   if k = "connections" then (k, `Assoc lc') else (k, v))
                                                   (U.to_assoc lj))) :: !extra;
                                     `Int nb
                                   | _ -> `Int ib) (bits_of e)))
                            | _ -> (p, e)) rconns in
                        (match List.rev newco with b :: _ -> prev_co := Some b | [] -> ());
                        extra := (Printf.sprintf "%s_carrep%d" rung i,
                                  `Assoc (List.map (fun (k, v) ->
                                      if k = "connections" then (k, `Assoc rconns') else (k, v))
                                      (U.to_assoc rj))) :: !extra) chain;
                    (match !prev_co with
                     | Some pb -> rewire := (ucell, "CI", pb) :: !rewire
                     | None -> ())
                  end) (List.rev users)
            end) cells;
      let cells' = List.map (fun (cn, cj) ->
          let rs = List.filter (fun (u, _, _) -> u = cn) !rewire in
          if rs = [] then (cn, cj)
          else match cj |> U.member "connections" with
            | `Assoc cs ->
              let cs' = List.map (fun (p, e) ->
                  match List.find_opt (fun (_, up, _) -> up = p) rs with
                  | Some (_, _, nb) -> (p, `List [ `Int nb ])
                  | None -> (p, e)) cs in
              (cn, `Assoc (List.map (fun (k, v) ->
                   if k = "connections" then (k, `Assoc cs') else (k, v)) (U.to_assoc cj)))
            | _ -> (cn, cj)) cells in
      `Assoc (List.map (fun (k, v) ->
          if k = "cells" then (k, `Assoc (cells' @ List.rev !extra)) else (k, v))
          (U.to_assoc mj))
    | _ -> mj in
  let out = match j |> U.member "modules" with
    | `Assoc mods ->
      `Assoc (List.map (fun (k, v) ->
          if k = "modules" then (k, `Assoc (List.map (fun (mn, mj) -> (mn, do_mod mj)) mods))
          else (k, v)) (U.to_assoc j))
    | _ -> j in
  if !nch > 0 then
    Printf.eprintf "[place_lef] replicated %d shared carry chain(s), %d rung(s)\n%!" !nch !nrung;
  out

(* MATERIALISE CONSTANT DRIVERS ON WIDE-MUX INPUTS.
 *
 * A MUXF7/MUXF8 whose I0/I1 is tied to a constant has no cell driving that
 * pin.  nextpnr's packer therefore INVENTS one -- `$PACKER_GND_NET$LUT$10`,
 * a SLICE_LUTX chain child of the mux -- and place_lef, which only stamps
 * cells present in the JSON, never placed it.  The analytical placer then
 * ends with it unbound and aborts:
 *
 *     ERROR: Found unbound cell $PACKER_GND_NET$LUT$10
 *
 * The two existing chain-binding hooks in nextpnr do not reach it: they bind
 * children RELATIVE to a placed parent for carry S/DI feed-throughs, and this
 * child needs the specific LUT site that feeds its mux.  Rather than teach
 * nextpnr a third special case, give the packer nothing to invent: put a real
 * LUT1 in the netlist driving that pin, so it is an ordinary cell that
 * place_lef stamps like any other.
 *
 * INIT is 2'h0 for a 0 and 2'h3 for a 1 (LUT1 truth table, output independent
 * of I0).
 *
 * I0 is driven from a FLIP-FLOP Q, not from the constant it replaced and not
 * left dangling.  Tying it to the constant makes the timing engine see the
 * const network arriving at a cell that drives it ("combinatorial loops"), and
 * leaving it unconnected invites the packer to tie it to a constant itself --
 * which recreates the very cell this pass exists to eliminate.  The value is a
 * don't-care (INIT ignores I0), so ANY driven net works; pick the flip-flop
 * whose name shares the longest prefix with the mux so the extra load stays
 * local instead of crossing the die. *)
let materialise_const_drivers (j : Y.t) : Y.t =
  let module U = Yojson.Safe.Util in
  let nadded = ref 0 in
  let is_widemux t = t = "MUXF7" || t = "MUXF8" in
  let do_mod mj =
    match mj |> U.member "cells" with
    | `Assoc cells when cells <> [] ->
      (* highest net id in use, so new nets get fresh ids *)
      let maxbit = ref 1 in
      let scan_bits e = match e with
        | `List l -> List.iter (function `Int i -> if i > !maxbit then maxbit := i | _ -> ()) l
        | _ -> () in
      List.iter (fun (_, cj) ->
          match cj |> U.member "connections" with
          | `Assoc conns -> List.iter (fun (_, e) -> scan_bits e) conns
          | _ -> ()) cells;
      (match mj |> U.member "netnames" with
       | `Assoc nets -> List.iter (fun (_, nj) -> scan_bits (nj |> U.member "bits")) nets
       | _ -> ());
      (* flip-flop Q bits, for a don't-care but DRIVEN LUT input *)
      let ff_qs = ref [] in
      List.iter (fun (cn, cj) ->
          let ty = try cj |> U.member "type" |> U.to_string with _ -> "" in
          if String.length ty >= 2 && String.sub ty 0 2 = "FD" then
            match cj |> U.member "connections" with
            | `Assoc conns ->
              (match List.assoc_opt "Q" conns with
               | Some (`List [ `Int b ]) -> ff_qs := (cn, b) :: !ff_qs
               | _ -> ())
            | _ -> ()) cells;
      let common_len a b =
        let n = min (String.length a) (String.length b) in
        let rec go i = if i < n && a.[i] = b.[i] then go (i + 1) else i in go 0 in
      let pick_ff_for cn =
        List.fold_left (fun best (fn, b) ->
            let l = common_len cn fn in
            match best with
            | Some (bl, _) when bl >= l -> best
            | _ -> Some (l, b)) None !ff_qs
        |> function Some (_, b) -> Some b | None -> None in
      let extra = ref [] in
      let cells' = List.map (fun (cn, cj) ->
          let ty = try cj |> U.member "type" |> U.to_string with _ -> "" in
          if not (is_widemux ty) then (cn, cj)
          else match cj |> U.member "connections" with
            | `Assoc conns ->
              let conns' = List.map (fun (p, e) ->
                  if p <> "I0" && p <> "I1" then (p, e)
                  else match e with
                    | `List [ `String ("0" | "1" as k) ] ->
                      incr maxbit;
                      let nb = !maxbit in
                      let lname = Printf.sprintf "%s_const_%s" cn p in
                      let init = if k = "0" then "2'h0" else "2'h3" in
                      extra := (lname, `Assoc [
                          "hide_name", `Int 0;
                          "type", `String "LUT1";
                          "parameters", `Assoc [ "INIT", `String init ];
                          "port_directions", `Assoc [ "I0", `String "input";
                                                      "O", `String "output" ];
                          "connections", `Assoc [
                              "I0", (match pick_ff_for cn with
                                     | Some q -> `List [ `Int q ]
                                     | None -> `List [ `String k ]);
                              "O", `List [ `Int nb ] ] ]) :: !extra;
                      incr nadded;
                      (p, `List [ `Int nb ])
                    | _ -> (p, e)) conns in
              (cn, `Assoc (List.map (fun (k, v) ->
                   if k = "connections" then (k, `Assoc conns') else (k, v))
                   (U.to_assoc cj)))
            | _ -> (cn, cj)) cells in
      `Assoc (List.map (fun (k, v) ->
          if k = "cells" then (k, `Assoc (cells' @ List.rev !extra)) else (k, v))
          (U.to_assoc mj))
    | _ -> mj in
  let out = match j |> U.member "modules" with
    | `Assoc mods ->
      `Assoc (List.map (fun (k, v) ->
          if k = "modules" then (k, `Assoc (List.map (fun (mn, mj) -> (mn, do_mod mj)) mods))
          else (k, v)) (U.to_assoc j))
    | _ -> j in
  if !nadded > 0 then
    Printf.eprintf "[place_lef] materialised %d constant driver(s) on MUXF7/F8 inputs\n%!" !nadded;
  out

let split_degenerate_muxf (j : Y.t) : Y.t =
  let module U = Yojson.Safe.Util in
  let ncloned = ref 0 and nskipped = ref 0 in
  let bits_of e = match e with
    | `List l -> List.filter_map (function `Int i -> Some i | _ -> None) l
    | _ -> [] in
  let is_lut t = String.length t >= 3 && String.uppercase_ascii (String.sub t 0 3) = "LUT" in
  let do_mod mj =
    match mj |> U.member "cells" with
    | `Assoc cells when cells <> [] ->
      let drv : (int, string * string) Hashtbl.t = Hashtbl.create 4096 in
      let byname : (string, Y.t) Hashtbl.t = Hashtbl.create 4096 in
      let maxbit = ref 1 in
      List.iter (fun (cn, cj) ->
          Hashtbl.replace byname cn cj;
          let dirs = try cj |> U.member "port_directions" |> U.to_assoc with _ -> [] in
          match cj |> U.member "connections" with
          | `Assoc conns ->
            List.iter (fun (p, e) ->
                let bs = bits_of e in
                List.iter (fun b -> if b > !maxbit then maxbit := b) bs;
                match List.assoc_opt p dirs with
                | Some (`String "output") -> List.iter (fun b -> Hashtbl.replace drv b (cn, p)) bs
                | _ -> ())
              conns
          | _ -> ()) cells;
      (match mj |> U.member "netnames" with
       | `Assoc nets ->
         List.iter (fun (_, nj) ->
             List.iter (fun b -> if b > !maxbit then maxbit := b)
               (bits_of (nj |> U.member "bits"))) nets
       | _ -> ());
      let dt_of dn =
        match Hashtbl.find_opt byname dn with
        | Some dcj -> (try dcj |> U.member "type" |> U.to_string with _ -> "")
        | None -> "" in
      let extra = ref [] and extranets = ref [] in
      (* Duplicate the LUT driving [b] and return the clone's fresh output bit. *)
      let clone_driver b =
        match Hashtbl.find_opt drv b with
        | Some (dn, dp) when is_lut (dt_of dn) ->
          incr maxbit;
          let fresh = !maxbit in
          let dcj = Hashtbl.find byname dn in
          let clone =
            `Assoc (List.map (fun (k, v) ->
                if k <> "connections" then (k, v)
                else (k, match v with
                    | `Assoc cc ->
                      `Assoc (List.map (fun (p, e) ->
                          if p = dp then (p, `List [ `Int fresh ]) else (p, e)) cc)
                    | _ -> v))
                (U.to_assoc dcj)) in
          let nm = ref (dn ^ "$muxdup") and k = ref 1 in
          while Hashtbl.mem byname !nm do
            incr k; nm := Printf.sprintf "%s$muxdup%d" dn !k
          done;
          Hashtbl.replace byname !nm clone;
          extra := (!nm, clone) :: !extra;
          extranets := (!nm ^ "." ^ dp,
                        `Assoc [ "hide_name", `Int 1;
                                 "bits", `List [ `Int fresh ];
                                 "attributes", `Assoc [] ]) :: !extranets;
          incr ncloned;
          Some fresh
        (* A MUXF8 data pin is driven by a MUXF7, not a LUT.  Duplicating a
           whole F7 subtree is a different (so far unobserved) problem -- count
           it rather than emit something subtly wrong. *)
        | _ -> incr nskipped; None in
      (* Every wide-mux data pin needs a LUT of its OWN, in its own lane of its
         own slice.  Two pins sharing one driver is unsatisfiable whether they
         are I0/I1 of the SAME mux (opt_merge collapsed an identical pair) or
         pins of two DIFFERENT muxes (opt_merge shared one LUT between them):
         a single instance cannot sit in two lanes.  First claimant keeps the
         original, every later one gets a clone. *)
      let claimed : (int, unit) Hashtbl.t = Hashtbl.create 256 in
      let cells' =
        List.map (fun (cn, cj) ->
            let t = try cj |> U.member "type" |> U.to_string with _ -> "" in
            if t <> "MUXF7" && t <> "MUXF8" then (cn, cj)
            else
              match cj |> U.member "connections" with
              | `Assoc conns ->
                let rewire = ref [] in
                List.iter (fun pin ->
                    match List.assoc_opt pin conns with
                    | Some e ->
                      (match bits_of e with
                       | [ b ] ->
                         if Hashtbl.mem claimed b then
                           (match clone_driver b with
                            | Some fresh ->
                              Hashtbl.replace claimed fresh ();
                              rewire := (pin, fresh) :: !rewire
                            | None -> ())
                         else Hashtbl.replace claimed b ()
                       | _ -> ())
                    | None -> ()) [ "I0"; "I1" ];
                if !rewire = [] then (cn, cj)
                else
                  (cn, `Assoc (List.map (fun (k, v) ->
                       if k <> "connections" then (k, v)
                       else (k, `Assoc (List.map (fun (p, e) ->
                           match List.assoc_opt p !rewire with
                           | Some fresh -> (p, `List [ `Int fresh ])
                           | None -> (p, e)) conns)))
                       (U.to_assoc cj)))
              | _ -> (cn, cj)) cells in
      let kv = U.to_assoc mj in
      let kv =
        List.map (fun (k, v) ->
            if k = "cells" then (k, `Assoc (cells' @ List.rev !extra))
            else if k = "netnames" then
              (k, match v with
                  | `Assoc nn -> `Assoc (nn @ List.rev !extranets)
                  | _ -> v)
            else (k, v)) kv in
      let kv =
        if List.mem_assoc "netnames" kv then kv
        else kv @ [ "netnames", `Assoc (List.rev !extranets) ] in
      `Assoc kv
    | _ -> mj in
  let r =
    match j |> U.member "modules" with
    | `Assoc mods ->
      `Assoc (List.map (fun (k, v) ->
          if k = "modules"
          then (k, `Assoc (List.map (fun (mn, mj) -> (mn, do_mod mj)) mods))
          else (k, v)) (U.to_assoc j))
    | _ -> j in
  if !ncloned > 0 || !nskipped > 0 then
    Printf.eprintf
      "[muxsplit] %d wide-mux data pin(s) given their own LUT back \
       (%d left alone: driver is not a LUT)\n%!" !ncloned !nskipped;
  r

(* Emit create_clock constraints for nextpnr, resolved from BUFG CELL names.

   Without this every clock is timed against nextpnr's --freq default.  The XDC
   nextpnr reads can only constrain PORTS, and the interesting clocks are BUFG
   outputs several derivations downstream of a port (GT refclk -> GTXE2 ->
   TXOUTCLK -> MMCM -> userclk2), which nextpnr cannot derive.  Measured on
   ethmin, that meant:

     Max frequency for clock '_40[0]': 65.35 MHz (PASS at 25.00 MHz)

   -- the 125 MHz SGMII datapath missing its real target by 2x and being
   reported as a PASS.  Every criticality the router exports is then relative to
   the wrong period, so timing-driven placement ranks the SLOW domain (closest
   to the uniform default) above the fast one that is actually failing, and no
   amount of PLACE_CRIT_K/P tuning can fix a wrong ranking.

   Net names are synthesis-generated and unstable (_40, _57), so key off the
   BUFG cell names, which come from the design hierarchy and are stable:

     TOPO_CLOCK_PERIODS="bufg_userclk2=8.0,clk_sys_bufg=40.0,..."

   matches each substring against BUFG/BUFGCTRL cell names and constrains the
   net that cell drives.  nextpnr names a multi-bit net's members "<name>[i]",
   so resolve the driver bit's INDEX too -- "_40" alone matches nothing. *)
let emit_clock_xdc (j : Y.t) =
  match Sys.getenv_opt "TOPO_CLOCKS_XDC" with
  | None -> ()
  | Some out ->
    let module U = Yojson.Safe.Util in
    let spec = getenv_default "TOPO_CLOCK_PERIODS" "" in
    let pairs =
      String.split_on_char ',' spec
      |> List.filter_map (fun kv ->
          match String.index_opt kv '=' with
          | Some i ->
            let k = String.trim (String.sub kv 0 i) in
            let v = String.sub kv (i + 1) (String.length kv - i - 1) in
            (try if k = "" then None else Some (k, float_of_string (String.trim v))
             with _ -> None)
          | None -> None) in
    if pairs = [] then
      Printf.eprintf "[clocks] TOPO_CLOCKS_XDC set but TOPO_CLOCK_PERIODS empty -- no constraints emitted\n%!"
    else begin
      let bits_of e = match e with
        | `List l -> List.filter_map (function `Int i -> Some i | _ -> None) l
        | _ -> [] in
      (* pick the real design module: the one with the most cells *)
      let best = ref (`Null) and bestn = ref (-1) in
      (match j |> U.member "modules" with
       | `Assoc mods ->
         List.iter (fun (_, mj) ->
             let n = match mj |> U.member "cells" with `Assoc c -> List.length c | _ -> 0 in
             if n > !bestn then (bestn := n; best := mj)) mods
       | _ -> ());
      let mj = !best in
      (* driver bit -> "name" or "name[i]" *)
      let bitname = Hashtbl.create 4096 in
      (* Index over the RAW bits array, INCLUDING constant ('0'/'1') entries:
         that is how nextpnr numbers a bus member.  Filtering the constants out
         first shifts every later index -- eth.i_pcs_pma._61 has constants at
         32/38/39, so the userclk BUFG's bit 43 came out as [40] and the
         create_clock silently matched nothing. *)
      (match mj |> U.member "netnames" with
       | `Assoc nets ->
         List.iter (fun (nn, nj) ->
             match nj |> U.member "bits" with
             | `List l ->
               let w = List.length l in
               List.iteri (fun i e ->
                   match e with
                   | `Int b ->
                     if not (Hashtbl.mem bitname b) then
                       Hashtbl.replace bitname b
                         (if w > 1 then Printf.sprintf "%s[%d]" nn i else nn)
                   | _ -> ()) l
             | _ -> ()) nets
       | _ -> ());
      let buf = Buffer.create 1024 in
      Buffer.add_string buf
        "# GENERATED by place_lef (TOPO_CLOCK_PERIODS) -- do not edit.\n# nextpnr can only create_clock on PORTS; these are BUFG outputs\n# it cannot derive, so without them every clock falls back to --freq.\n";
      let n = ref 0 in
      (match mj |> U.member "cells" with
       | `Assoc cells ->
         List.iter (fun (cn, cj) ->
             let t = try cj |> U.member "type" |> U.to_string with _ -> "" in
             if t = "BUFG" || t = "BUFGCTRL" then
               match List.find_opt (fun (k, _) -> contains cn k) pairs with
               | None -> ()
               | Some (k, per) ->
                 (match cj |> U.member "connections" with
                  | `Assoc conns ->
                    (match List.assoc_opt "O" conns with
                     | Some e ->
                       (match bits_of e with
                        | b :: _ ->
                          (match Hashtbl.find_opt bitname b with
                           | Some nname ->
                             incr n;
                             Buffer.add_string buf
                               (Printf.sprintf
                                  "create_clock -period %.3f -name %s [get_nets {%s}]\n" per k nname);
                             Printf.eprintf "[clocks] %-16s %6.3f ns  cell=%s  net=%s\n%!"
                               k per cn nname
                           | None ->
                             Printf.eprintf
                               "[clocks] WARNING %s: driver bit %d of %s has no netname -- NOT constrained\n%!"
                               k b cn)
                        | [] -> ())
                     | None -> ())
                  | _ -> ())) cells
       | _ -> ());
      (* Say so loudly: an unmatched key means that clock silently keeps the
         --freq default, which is exactly the failure this function exists for. *)
      List.iter (fun (k, _) ->
          let seen = Buffer.contents buf in
          if not (contains seen ("-name " ^ k ^ " ")) then
            Printf.eprintf "[clocks] WARNING no BUFG matched '%s' -- that clock keeps the --freq default\n%!" k)
        pairs;
      let oc = open_out out in
      output_string oc (Buffer.contents buf); close_out oc;
      Printf.eprintf "[clocks] wrote %d create_clock line(s) -> %s\n%!" !n out
    end

let run floorplan_json netlist_json =
  let j = insert_hold_buffers (normalise_init (materialise_const_drivers (replicate_shared_carry (replicate_shared_muxf7 (split_degenerate_muxf
            (stitch_gt_pad_buffers (cleanup_netlist (Y.from_file netlist_json)))))))) in
  emit_clock_xdc j;
  run_gen floorplan_json
    ~get_bmod:(fun () -> Pack_to_lef.bmodule_of_yosys_tree j)
    ~get_j:(fun () -> j)

(* In-memory entry: the gate-mapped netlist's nextpnr-json tree already in RAM
   (SVS flow: Bir_to_nextpnr_json.yosys_json), placed with NO file round-trip.
   Packing goes through bmodule_of_yosys_tree so the placer sees EXACTLY the
   same netlist it would from the on-disk json. *)
let run_inmem floorplan_json (j : Y.t) =
  let j = insert_hold_buffers (materialise_const_drivers (replicate_shared_muxf7 (split_degenerate_muxf (stitch_gt_pad_buffers (cleanup_netlist j))))) in
  run_gen floorplan_json
    ~get_bmod:(fun () -> Pack_to_lef.bmodule_of_yosys_tree j)
    ~get_j:(fun () -> j)
