(* Half-perimeter wire-length estimation.

   Given a placement (instance -> coordinate) and a netlist
   (net -> connected pins), compute HPWL per net and total wire
   length.  This is the standard pre-routing wire-length estimate;
   it provides the geometric input to a first-order Elmore RC
   delay model.

   We deliberately ignore pin-offset within each cell (we'd need
   LEF for that) — at this granularity it is dominated by the
   inter-cell distance for any reasonable design. *)

type pos = { x : int; y : int }

let pos_of_placement (p : Placement.placement) = { x = p.Placement.x; y = p.Placement.y }

let placement_table (pls : Placement.placement list) =
  let h = Hashtbl.create (List.length pls) in
  List.iter (fun p -> Hashtbl.replace h p.Placement.inst (pos_of_placement p)) pls;
  h

(* HPWL of a list of points — sum of x-span and y-span. *)
let hpwl_of_points = function
  | [] | [_] -> 0
  | p :: rest ->
      let xmin = List.fold_left (fun a (q:pos) -> min a q.x) p.x rest in
      let xmax = List.fold_left (fun a (q:pos) -> max a q.x) p.x rest in
      let ymin = List.fold_left (fun a (q:pos) -> min a q.y) p.y rest in
      let ymax = List.fold_left (fun a (q:pos) -> max a q.y) p.y rest in
      (xmax - xmin) + (ymax - ymin)

(* HPWL of a single net.  Pins whose driving instance has no
   placement (e.g. top-level ports without PINS data) are skipped. *)
let hpwl_of_net plc_tbl (net : Nets.net) =
  let pts = List.filter_map
    (fun pr -> try Some (Hashtbl.find plc_tbl pr.Nets.inst) with Not_found -> None)
    net.Nets.pins in
  hpwl_of_points pts

let hpwl_total ?(skip_clock=true) plc_tbl nets =
  let is_clk n = skip_clock &&
    (let s = n.Nets.name in
     let has sub =
       let ls = String.length s and lp = String.length sub in
       let rec scan i = i + lp <= ls
         && (String.sub s i lp = sub || scan (i+1)) in
       scan 0
     in
     has "clk" || has "CLK") in
  List.fold_left
    (fun acc n -> if is_clk n then acc else acc + hpwl_of_net plc_tbl n)
    0 nets

type stats = {
  n_nets       : int;
  n_signal     : int;
  total_hpwl   : int;
  max_net_name : string;
  max_net_hpwl : int;
  bbox_x       : int * int;
  bbox_y       : int * int;
}

let stats ?(skip_clock=true) placements nets =
  let tbl = placement_table placements in
  let max_n = ref "" and max_h = ref 0 and total = ref 0 and signal = ref 0 in
  List.iter (fun (n : Nets.net) ->
    let h = hpwl_of_net tbl n in
    if not skip_clock || not (let s = n.Nets.name in
       String.length s >= 3 &&
       (String.sub s 0 3 = "clk" || String.sub s 0 3 = "CLK")) then begin
      total := !total + h;
      incr signal;
      if h > !max_h then begin max_h := h; max_n := n.Nets.name end
    end) nets;
  let xs = List.map (fun (p:Placement.placement) -> p.Placement.x) placements in
  let ys = List.map (fun (p:Placement.placement) -> p.Placement.y) placements in
  let mn = List.fold_left min max_int and mx = List.fold_left max min_int in
  {
    n_nets       = List.length nets;
    n_signal     = !signal;
    total_hpwl   = !total;
    max_net_name = !max_n;
    max_net_hpwl = !max_h;
    bbox_x       = (mn xs, mx xs);
    bbox_y       = (mn ys, mx ys);
  }
