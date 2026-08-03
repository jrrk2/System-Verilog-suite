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
             let w = 1.0 +. k *. c in
             List.iter (fun idx ->
               List.iter (fun nid -> if net_w.(nid) < w then net_w.(nid) <- w)
                 cell_nets.(idx)) ids
           end
         | _ -> ())
       done with End_of_file -> ()); close_in ic;
     Printf.eprintf "[place_lef] timing-driven: %d/%d crit cells matched (K=%.1f)\n%!"
       !matched !lines k
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
  let hard = ref [] in
  Array.iteri (fun i c -> if kind_of_lef c.Pack_to_lef.pc_lef <> "SLICE" then hard := (i, c) :: !hard) cells;
  let hard = List.sort (fun (_, a) (_, b) ->
      let ka = kind_of_lef a.Pack_to_lef.pc_lef and kb = kind_of_lef b.Pack_to_lef.pc_lef in
      compare (ka, group_key a.Pack_to_lef.pc_name) (kb, group_key b.Pack_to_lef.pc_name)) !hard in
  let last_pos = Hashtbl.create 16 in
  List.iter (fun (i, c) ->
      let kind = kind_of_lef c.Pack_to_lef.pc_lef in
      let gk = kind ^ ":" ^ group_key c.Pack_to_lef.pc_name in
      let tgt = match Hashtbl.find_opt last_pos gk with Some p -> p | None -> (110, 100) in
      match nearest_free_of kind tgt with
      | Some s -> bind i s; Hashtbl.replace last_pos gk (s.sx, s.sy)
      | None -> Printf.eprintf "no free %s site for %s\n" kind c.Pack_to_lef.pc_name) hard;
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
  let chains = ref [] in
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
           +. coh_w *. dcoh +. ddom,
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
        let s = region_arr.(Random.int (Array.length region_arr)) in
        let si = match cell_site.(i) with Some s -> s | None -> assert false in
        if s.sname <> si.sname && fits i s then begin
          let j = match Hashtbl.find_opt occ s.sname with Some j -> j | None -> -1 in
          (* legal iff empty target, or a swap where BOTH land on a site they fit *)
          if (j = -1 || (movable.(j) && fits j si)) then begin
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
  (match mode with
   | "analytic" -> analytic (); anneal_multi ()
   | "sa"       -> constructive (); let h0, _ = total_hpwl () in
                   Printf.eprintf "SA seed HPWL=%d\n" h0; anneal_multi ()
   | "region"   -> constructive ()
   | "greedy" | _ -> constructive ());

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
  let ob = open_out bels_path in
  let nb = ref 0 in
  Array.iter (fun c ->
      if stamp_all || not (skip_kind (kind_of_lef c.Pack_to_lef.pc_lef)) then
      match cell_site.(Hashtbl.find name2id c.Pack_to_lef.pc_name) with
      | Some site -> List.iter (fun (prim, suffix) ->
          Printf.fprintf ob "%s\t%s/%s\n" prim site.sname suffix; incr nb) c.Pack_to_lef.pc_bels
      | None -> ()) cells;
  close_out ob;
  Printf.printf "placement -> %s ; %d BEL stamps -> %s\n" placed_path !nb bels_path;

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
     (* buffer the highest-fanout control nets first (within budget) *)
     let ctrl_nets = Hashtbl.fold (fun bit sinks acc ->
         match Hashtbl.find_opt drv bit with
         | Some (_, dty) when dty <> "BUFG" && dty <> "BUFGCTRL" && dty <> "GND" && dty <> "VCC" ->
           let c = List.length (List.filter (fun (_,p,_) -> is_ctrl p) sinks) in
           if c >= bufg_thresh && c * 2 >= List.length sinks then (c, bit) :: acc else acc
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
let run floorplan_json netlist_json =
  let j = insert_hold_buffers (normalise_init (cleanup_netlist (Y.from_file netlist_json))) in
  run_gen floorplan_json
    ~get_bmod:(fun () -> Pack_to_lef.bmodule_of_yosys_tree j)
    ~get_j:(fun () -> j)

(* In-memory entry: the gate-mapped netlist's nextpnr-json tree already in RAM
   (SVS flow: Bir_to_nextpnr_json.yosys_json), placed with NO file round-trip.
   Packing goes through bmodule_of_yosys_tree so the placer sees EXACTLY the
   same netlist it would from the on-disk json. *)
let run_inmem floorplan_json (j : Y.t) =
  let j = insert_hold_buffers (cleanup_netlist j) in
  run_gen floorplan_json
    ~get_bmod:(fun () -> Pack_to_lef.bmodule_of_yosys_tree j)
    ~get_j:(fun () -> j)
