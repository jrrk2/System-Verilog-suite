(* Probe Lib_size catalogue + sizing dry-run on picosoc netlist. *)

let dump_body name body =
  Printf.printf "  CELL %s body:\n" name;
  List.iter (fun b ->
    let tag = match b with
      | Liberty_rewrite.Parameter (p, v) -> Printf.sprintf "Parameter(%s, %f)" p v
      | Liberty_rewrite.CellPin (n, attrs) -> Printf.sprintf "CellPin(%s, %d attrs)" n (List.length attrs)
      | Liberty_rewrite.Timing _ -> "Timing"
      | Liberty_rewrite.IPower _ -> "IPower"
      | Liberty_rewrite.LPower _ -> "LPower"
      | Liberty_rewrite.Power (a, b, _) -> Printf.sprintf "Power(%s,%s)" a b
      | Liberty_rewrite.Other (a, b, _) -> Printf.sprintf "Other(%s,%s)" a b
      | Liberty_rewrite.Related (a, b) -> Printf.sprintf "Related(%s,%s)" a b
      | Liberty_rewrite.FlipFlop _ -> "FlipFlop"
      | Liberty_rewrite.Latch _ -> "Latch"
      | Liberty_rewrite.String s -> Printf.sprintf "String(%s)" s
      | _ -> "?other" in
    Printf.printf "    %s\n" tag) body

let dump_pin name attrs =
  Printf.printf "    PIN %s attrs:\n" name;
  List.iter (fun b ->
    let tag = match b with
      | Liberty_rewrite.Parameter (p, v) -> Printf.sprintf "Parameter(%s, %f)" p v
      | Liberty_rewrite.Direction d -> Printf.sprintf "Direction(%s)" d
      | Liberty_rewrite.Function f -> Printf.sprintf "Function(%s)" f
      | _ -> "?other" in
    Printf.printf "      %s\n" tag) attrs

let () =
  let lib = match Lib_size.liberty_path_or_default () with
    | Some p -> p
    | None -> prerr_endline "no Liberty"; exit 1 in
  Printf.printf "Loading %s ...\n%!" lib;
  let lib_root, _hash = Liberty_rewrite.rewrite lib in
  (match lib_root with
   | Liberty_rewrite.Library (_, items) ->
       Printf.printf "Library has %d items\n" (List.length items);
       let n = ref 0 in
       List.iter (function
         | Liberty_rewrite.LibCell (name, body) when name = "AND2_X1" || name = "INV_X4" ->
             dump_body name body;
             List.iter (function
               | Liberty_rewrite.CellPin (pn, attrs) when !n < 3 ->
                   dump_pin pn attrs; incr n
               | _ -> ()) body
         | _ -> ()) items
   | _ -> prerr_endline "not Library");
  Printf.printf "\n\n";
  let cat = Lib_size.load_catalogue lib in
  Printf.printf "Variant families: %d\n" (Hashtbl.length cat);
  let chosen = ["AND2"; "OR2"; "INV"; "BUF"; "NAND2"; "MUX2"; "DFF"; "DFFR"] in
  List.iter (fun bn ->
    match Hashtbl.find_opt cat bn with
    | None -> Printf.printf "  %-10s NOT FOUND\n" bn
    | Some vs ->
        Printf.printf "  %-10s %d variant(s):" bn (List.length vs);
        List.iter (fun (v : Lib_size.variant) ->
          Printf.printf " %s(max_cap=%.2f, ff=%b)"
            v.name v.max_cap v.is_ff
        ) vs;
        print_newline ()
  ) chosen;
  print_newline ();
  Printf.printf "First 5 of each variant family pin caps:\n";
  let i = ref 0 in
  Hashtbl.iter (fun bn vs ->
    if !i < 5 then begin
      Printf.printf "  %s -> %d variants:\n" bn (List.length vs);
      List.iter (fun (v : Lib_size.variant) ->
        Printf.printf "    %s pin_caps:" v.name;
        List.iter (fun (p, c) -> Printf.printf " %s=%.3f" p c) v.pin_caps;
        print_newline ()
      ) vs;
      incr i
    end
  ) cat
