(* Quick correctness test for [Kary_merge].  Builds a 4-cell chain
   of OR2 cells (a|b|c|d|e) and a 7-cell chain (8-input), runs the
   merge pass, and prints the result + a summary.

   Pass criteria:
     - the 5-input chain collapses to one OR4 + a final OR2 (since
       residual arity 5 doesn't fit one cell, the algorithm caps at 4)
       Actually: the head sees inputs (d_out, e); d_out drives 1 head;
       absorb d -> head has (c_out, d_in0, d_in1, e) = arity 4 -> OR4_X1
       But then the c chain (a, b) hangs unmerged.  Acceptable.
     - the 8-input chain collapses to a 2-level OR4 tree.

   Run via: dune exec ./test_kary_merge.exe                            *)

open Lib_map

let mk_or2 inst_name in1 in2 out =
  { cell = { cell_name = "OR2_X1"; in_pins = ["A1"; "A2"]; out_pin = "ZN" };
    inst_name;
    conns = [{ pin = "ZN"; net = out };
             { pin = "A1"; net = in1 };
             { pin = "A2"; net = in2 }] }

(* (((a|b)|c)|d)|e  — 4 OR2 cells, single-fanout chain. *)
let chain5 () : netlist =
  { inputs  = [("a",1);("b",1);("c",1);("d",1);("e",1)];
    outputs = [("y",1)];
    wires   = [("n1",1);("n2",1);("n3",1)];
    insts =
      [ mk_or2 "or_1" "a"  "b"  "n1";
        mk_or2 "or_2" "n1" "c"  "n2";
        mk_or2 "or_3" "n2" "d"  "n3";
        mk_or2 "or_4" "n3" "e"  "y"; ];
    assigns = [] }

(* Balanced 8-input tree: ((a|b)|(c|d)) | ((e|f)|(g|h)) — 7 OR2 cells. *)
let tree8 () : netlist =
  { inputs  = List.init 8 (fun i -> (Printf.sprintf "i%d" i, 1));
    outputs = [("y",1)];
    wires   = List.init 6 (fun i -> (Printf.sprintf "n%d" (i+1), 1));
    insts =
      [ mk_or2 "or_a" "i0" "i1" "n1";
        mk_or2 "or_b" "i2" "i3" "n2";
        mk_or2 "or_c" "i4" "i5" "n3";
        mk_or2 "or_d" "i6" "i7" "n4";
        mk_or2 "or_e" "n1" "n2" "n5";
        mk_or2 "or_f" "n3" "n4" "n6";
        mk_or2 "or_g" "n5" "n6" "y"; ];
    assigns = [] }

let summarise (nl : netlist) =
  let counts = Hashtbl.create 8 in
  List.iter (fun i ->
    let c = try Hashtbl.find counts i.cell.cell_name with Not_found -> 0 in
    Hashtbl.replace counts i.cell.cell_name (c + 1)
  ) nl.insts;
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) counts []
  |> List.sort compare

let print_summary tag (nl : netlist) =
  let s = summarise nl in
  Printf.printf "[%s] insts=%d cells=" tag (List.length nl.insts);
  List.iter (fun (k, v) -> Printf.printf " %s=%d" k v) s;
  print_newline ()

let () =
  Printf.printf "=== chain5 (((a|b)|c)|d)|e ===\n";
  let nl = chain5 () in
  print_summary "before" nl;
  let nl', n = Kary_merge.merge_module nl in
  print_summary "after " nl';
  Printf.printf "merges = %d\n\n" n;

  Printf.printf "=== tree8 (balanced 8-input OR2 tree) ===\n";
  let nl = tree8 () in
  print_summary "before" nl;
  let nl', n = Kary_merge.merge_module nl in
  print_summary "after " nl';
  Printf.printf "merges = %d\n" n;

  (* Detail dump of the rewritten cells — verify pin wiring. *)
  let dump_cells label (nl : netlist) =
    Printf.printf "  --- %s cells ---\n" label;
    List.iter (fun i ->
      let conns_str = String.concat " "
        (List.map (fun c -> Printf.sprintf ".%s(%s)" c.pin c.net) i.conns) in
      Printf.printf "    %-7s %s %s\n"
        i.cell.cell_name i.inst_name conns_str
    ) nl.insts
  in
  dump_cells "chain5 after"
    (let nl = chain5 () in let nl', _ = Kary_merge.merge_module nl in nl');
  dump_cells "tree8 after"
    (let nl = tree8 () in let nl', _ = Kary_merge.merge_module nl in nl');

  (* Long chain: 20 OR2 in cascade.  Tests fixed-point iteration. *)
  let chain20 () : netlist =
    let insts = ref [] in
    let wires = ref [] in
    let prev = ref "a0" in
    for i = 1 to 19 do
      let new_net = if i = 19 then "y" else Printf.sprintf "n%d" i in
      let in_b = Printf.sprintf "a%d" i in
      insts := mk_or2 (Printf.sprintf "or_%d" i) !prev in_b new_net :: !insts;
      if i < 19 then wires := (new_net, 1) :: !wires;
      prev := new_net
    done;
    { inputs  = List.init 20 (fun i -> (Printf.sprintf "a%d" i, 1));
      outputs = [("y",1)];
      wires   = !wires;
      insts   = List.rev !insts;
      assigns = [] }
  in
  (* Shared driver test: A = AND2(x,y) feeds two AND2 sinks B and C.
     Old (single-fanout) algorithm: nothing absorbable (A.load=2).
     New (duplicating) algorithm: B and C each absorb A's inputs;
     A becomes loadless and DCE drops it.  Net: 3 cells → 2 wider cells. *)
  let shared_driver () : netlist =
    { inputs  = [("x",1);("y",1);("z",1);("w",1)];
      outputs = [("b",1);("c",1)];
      wires   = [("a",1)];
      insts =
        [ { cell = { cell_name = "AND2_X1"; in_pins = ["A1";"A2"];
                     out_pin = "ZN" };
            inst_name = "and_a";
            conns = [{pin="ZN";net="a"}; {pin="A1";net="x"};
                     {pin="A2";net="y"}] };
          { cell = { cell_name = "AND2_X1"; in_pins = ["A1";"A2"];
                     out_pin = "ZN" };
            inst_name = "and_b";
            conns = [{pin="ZN";net="b"}; {pin="A1";net="a"};
                     {pin="A2";net="z"}] };
          { cell = { cell_name = "AND2_X1"; in_pins = ["A1";"A2"];
                     out_pin = "ZN" };
            inst_name = "and_c";
            conns = [{pin="ZN";net="c"}; {pin="A1";net="a"};
                     {pin="A2";net="w"}] }; ];
      assigns = [] } in
  Printf.printf "\n=== shared_driver (A drives B and C, fanout=2) ===\n";
  let nl = shared_driver () in
  print_summary "before" nl;
  let nl', n = Kary_merge.merge_module nl in
  print_summary "after " nl';
  Printf.printf "merges = %d\n" n;
  dump_cells "shared_driver after" nl';

  Printf.printf "\n=== chain20 (20-input OR2 cascade) ===\n";
  let nl = chain20 () in
  print_summary "before" nl;
  let nl', n = Kary_merge.merge_module nl in
  print_summary "after " nl';
  Printf.printf "merges = %d  (depth: %d → %d cells)\n"
    n (List.length nl.insts) (List.length nl'.insts)
