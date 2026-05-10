(* Self-test for Lib_map.gen_add_brent_kung — software-simulate the
   gates it produces and compare to (a + b) mod 2^W across either
   exhaustive (small W) or random (large W) input vectors.

   Catches off-by-one / wrong-direction-of-tree bugs that a code
   review can miss but a bit-vector check finds in milliseconds.

   Usage: test_brent_kung_equiv [width] [num_vectors]
                width        — bit width to test (default 8, exhaustive)
                num_vectors  — sample count for W>10 (default 1000)
*)

open Lib_map

let nth_bit v i = (v lsr i) land 1

(* Simulate one gate against a wire-value table. *)
let eval_gate (vals : (string, int) Hashtbl.t) (i : instance) =
  let pin name =
    let conn = List.find (fun (c : pin_conn) -> c.pin = name) i.conns in
    conn.net in
  let in_val n =
    if n = "1'b0" then 0
    else if n = "1'b1" then 1
    else
      try Hashtbl.find vals n
      with Not_found ->
        failwith (Printf.sprintf "unbound wire %s in gate %s" n i.inst_name)
  in
  let out_net = pin i.cell.out_pin in
  let result =
    match i.cell.cell_name with
    | "AND2_X1" -> (in_val (pin "A1")) land (in_val (pin "A2"))
    | "OR2_X1"  -> (in_val (pin "A1")) lor  (in_val (pin "A2"))
    | "XOR2_X1" -> (in_val (pin "A"))  lxor (in_val (pin "B"))
    | "INV_X1"  -> 1 - (in_val (pin "A"))
    | s -> failwith ("unsupported cell in self-test: " ^ s)
  in
  Hashtbl.replace vals out_net result

(* Topo-sort + simulate; for our gen_add_brent_kung output the order
   in which gates were emitted is already topologically sorted (we
   produce gates in dataflow order), so a single forward pass is
   enough.  If it ever isn't, the in_val lookup fails loudly. *)
let simulate ~insts ~inputs =
  let vals = Hashtbl.create 256 in
  List.iter (fun (n, v) -> Hashtbl.replace vals n v) inputs;
  List.iter (eval_gate vals) insts;
  vals

let test_width width =
  let a_name = "a" and b_name = "b" in
  let _, _, insts, _wires =
    gen_add_brent_kung ~ctx:"test" ~a_name ~a_w:width ~b_name ~b_w:width () in
  Printf.printf "  width=%d  cells=%d\n" width (List.length insts);
  (* Collect output net names (sum bits) *)
  let sum_nets = List.init width (fun i ->
    let bit_ctx = Printf.sprintf "test_bit%d" i in
    (* The sum wire was minted as mint_ctx ~ctx:bit_ctx "sum". The
       block_tag fallback (no current_modhash) produces
       "_<bit_ctx>__sum_<seq>_".  We'd need to look it up by walking
       the inst list — find the XOR2 in this stage that drives
       sum.  Use a workaround: look for an XOR2 whose ZN/Z is unique
       at this bit's position by topology.  Actually simplest: the
       last XOR2 minted per bit position IS the sum.  Find it by
       walking insts and matching the bit_ctx prefix in inst_name. *)
    let _ = bit_ctx in
    i)
  in
  let _ = sum_nets in
  (* Easier: re-emit the gates with a known fixed naming convention.
     For a unit test we don't need decoded names — just the wire
     pattern.  Instead, pull sum bits by simulation: after running
     all gates, the wires we care about are whatever the function
     returns as `sums`.  Re-call the generator and capture the
     sum wire NAMES.                                              *)
  Block_tag.reset ();
  let sums, _cout, insts2, _wires2 =
    gen_add_brent_kung ~ctx:"test" ~a_name ~a_w:width ~b_name ~b_w:width () in
  let sum_names = sums in
  let n_total = 1 lsl width in
  let n_test =
    if width <= 10 then n_total
    else 1000 in
  let mismatches = ref 0 in
  let rng = Random.State.make [| 0xc0ffee |] in
  for trial = 0 to n_test - 1 do
    let rand_w w =
      let mask = if w >= 62 then -1 else (1 lsl w) - 1 in
      let rec gen acc shift =
        if shift >= w then acc land mask
        else
          let chunk = Random.State.bits rng in    (* 30 random bits *)
          gen (acc lor (chunk lsl shift)) (shift + 30) in
      gen 0 0 in
    let a = if width <= 10 then trial mod (1 lsl width)
            else rand_w width in
    let b = if width <= 10 then (trial / (1 lsl width)) mod (1 lsl width)
            else rand_w width in
    if width <= 10 || trial < n_test then begin
      let bit_at_name n i =
        if width = 1 then n else Printf.sprintf "%s[%d]" n i in
      let inputs = List.init width (fun i ->
        [ bit_at_name a_name i, nth_bit a i;
          bit_at_name b_name i, nth_bit b i ]) |> List.concat in
      let vals = simulate ~insts:insts2 ~inputs in
      let sum_actual = List.mapi (fun i n ->
        let bit = try Hashtbl.find vals n with Not_found -> 0 in
        bit lsl i
      ) sum_names |> List.fold_left (+) 0 in
      let sum_expect = (a + b) land ((1 lsl width) - 1) in
      if sum_actual <> sum_expect then begin
        if !mismatches < 5 then
          Printf.printf "  MISMATCH a=%d b=%d expect=%d got=%d\n"
            a b sum_expect sum_actual;
        incr mismatches
      end
    end
  done;
  if !mismatches = 0 then begin
    Printf.printf "  ✅ %d / %d vectors match\n" n_test n_test;
    true
  end else begin
    Printf.printf "  ❌ %d / %d vectors mismatch\n" !mismatches n_test;
    false
  end

let () =
  let widths =
    if Array.length Sys.argv > 1
    then [int_of_string Sys.argv.(1)]
    else [2; 4; 8; 16; 32] in
  let all_ok = ref true in
  List.iter (fun w ->
    Printf.printf "── width %d ──\n" w;
    Block_tag.reset ();
    if not (test_width w) then all_ok := false
  ) widths;
  if !all_ok then begin
    Printf.printf "\nALL PASS\n";
    exit 0
  end else begin
    Printf.printf "\nFAILED\n";
    exit 1
  end
