(* cell_census.ml -- count cells per type from any of the four netlist formats
   this project moves a design through, so the counts can be COMPARED.

   The open flow hands the same design between four representations:

     .v    yosys structural Verilog   (what synthesis produced)
     .json nextpnr / yosys JSON       (what place & route consumed and emitted)
     .edf  EDIF                       (what Vivado links)
     .xml  opendcp                    (what a DCP actually contains)

   Every stage boundary has silently lost cells at least once in this campaign
   -- 16 BRAMs collapsing to 1, 6464 cells deleted by a blackbox absorb, an EDIF
   that dropped memory INIT, a DCP whose GT nets arrived with no driver.  Each
   time it was found late, by eyeballing a different ad-hoc script per format.
   A census that speaks all four turns "did this stage lose anything?" into one
   comparison rather than four scripts.

   Counting is by CELL TYPE, because that is the only key all four formats
   agree on; names are punctuated differently at every boundary (see
   Opendcp_xml.canon for why joining on those needs care). *)

type census = (string * int) list      (* cell type -> count, descending *)

let tally (tbl : (string, int) Hashtbl.t) : census =
  let l = Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl [] in
  List.sort (fun (ak, av) (bk, bv) ->
      if av <> bv then compare bv av else compare ak bk) l

let total (c : census) = List.fold_left (fun a (_, n) -> a + n) 0 c

(* ------------------------------------------------------------------- xml *)

let of_opendcp_xml path : census =
  let db = Opendcp_xml.load path in
  let t = Hashtbl.create 64 in
  List.iter
    (fun (c : Opendcp_xml.cell) ->
       Hashtbl.replace t c.c_type (1 + (try Hashtbl.find t c.c_type with Not_found -> 0)))
    db.cells;
  tally t

(* ------------------------------------------------------------------ edif *)

let of_edif path : census =
  let e = Edif_parser.parse_schematic path in
  let t = Hashtbl.create 64 in
  List.iter
    (fun (i : Edif_parser.instance_info) ->
       Hashtbl.replace t i.cell_type
         (1 + (try Hashtbl.find t i.cell_type with Not_found -> 0)))
    e.instances;
  tally t

(* ------------------------------------------------------------------ json *)

(* yosys / nextpnr JSON.  Both put cells under modules.<name>.cells.<name>.type;
   nextpnr's routed output additionally carries repacked types (SLICE_LUTX,
   SLICE_FFX), which is exactly the difference worth seeing in a comparison.
   The module with the most cells is the design -- the rest are blackbox
   primitive stubs with no cells at all. *)
let of_json path : census =
  let j = Yojson.Safe.from_file path in
  let modules =
    match j with
    | `Assoc top ->
        (match List.assoc_opt "modules" top with
         | Some (`Assoc ms) -> ms
         | _ -> [])
    | _ -> [] in
  let cells_of m =
    match m with
    | `Assoc fields ->
        (match List.assoc_opt "cells" fields with
         | Some (`Assoc cs) -> cs
         | _ -> [])
    | _ -> [] in
  let best =
    List.fold_left
      (fun acc (_, m) ->
         let c = cells_of m in
         if List.length c > List.length acc then c else acc)
      [] modules in
  let t = Hashtbl.create 64 in
  List.iter
    (fun (_, c) ->
       match c with
       | `Assoc f ->
           (match List.assoc_opt "type" f with
            | Some (`String ty) ->
                Hashtbl.replace t ty (1 + (try Hashtbl.find t ty with Not_found -> 0))
            | _ -> ())
       | _ -> ())
    best;
  tally t

(* --------------------------------------------------------------- verilog *)

(* Structural Verilog arrives as Behavioral IR from whichever front end the
   caller chose (verible / verilator / yosys / slang ...), so this takes the
   parsed program rather than a path: which reader was used is a decision for
   the recipe, not for the census, and hard-wiring one here would quietly make
   the comparison front-end specific. *)
let of_behavioral (prog : Behavioral_ir.bprogram) : census =
  let t = Hashtbl.create 64 in
  List.iter
    (fun (m : Behavioral_ir.bmodule) ->
       List.iter
         (fun (i : Behavioral_ir.binstance) ->
            Hashtbl.replace t i.module_name
              (1 + (try Hashtbl.find t i.module_name with Not_found -> 0)))
         m.instances)
    prog.modules;
  tally t

(* ---------------------------------------------------------------- report *)

let of_file path : census =
  let ext =
    match String.rindex_opt path '.' with
    | Some i -> String.lowercase_ascii
                  (String.sub path (i + 1) (String.length path - i - 1))
    | None -> "" in
  match ext with
  | "xml" -> of_opendcp_xml path
  | "edf" | "edif" | "edn" -> of_edif path
  | "json" -> of_json path
  | _ -> failwith ("cell_census: cannot infer a reader for " ^ path
                   ^ " (Verilog must go through a front end -- use of_behavioral)")

let to_string ?(label = "") (c : census) =
  let b = Buffer.create 1024 in
  if label <> "" then Buffer.add_string b (label ^ "\n");
  Buffer.add_string b (Printf.sprintf "  %-28s %8d\n" "TOTAL" (total c));
  List.iter (fun (k, n) -> Buffer.add_string b (Printf.sprintf "  %-28s %8d\n" k n)) c;
  Buffer.contents b

(* Side-by-side.  Types present in one side only are the interesting rows, so
   they are kept (as 0) rather than dropped. *)
let compare_to_string ?(a_label = "A") ?(b_label = "B") (a : census) (b : census) =
  let keys =
    List.sort_uniq compare (List.map fst a @ List.map fst b) in
  let get c k = try List.assoc k c with Not_found -> 0 in
  let bf = Buffer.create 2048 in
  Buffer.add_string bf
    (Printf.sprintf "  %-28s %10s %10s %8s\n" "cell type" a_label b_label "delta");
  Buffer.add_string bf
    (Printf.sprintf "  %-28s %10d %10d %+8d\n" "TOTAL" (total a) (total b)
       (total b - total a));
  let rows =
    List.map (fun k -> (k, get a k, get b k)) keys in
  let rows = List.sort (fun (_, a1, b1) (_, a2, b2) ->
      compare (max a2 b2) (max a1 b1)) rows in
  List.iter
    (fun (k, x, y) ->
       Buffer.add_string bf
         (Printf.sprintf "  %-28s %10d %10d %+8d%s\n" k x y (y - x)
            (if x = y then "" else "   <--")))
    rows;
  Buffer.contents bf
