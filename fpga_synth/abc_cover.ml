(* Optional abc LUT-cover backend for fpga_map.
 *
 * The native Lut_cover maps SVS's AIG competitively with abc on datapath
 * cones but trails abc on register/CSR-heavy logic (e.g. ibex_cs_registers
 * 2528 vs abc 1491 on the same AIG).  This module lets the flow route the
 * subject graph through abc's `if` mapper and import the result, so abc can
 * be selected per the LUTCOVER_BACKEND=abc env.
 *
 * Interface with abc is positional (no name mangling): write the graph as
 * binary AIGER (inputs in node order, outputs in g.outputs order), run
 * `if -K 6`, and read back a BLIF whose PIs are pi<idx> / POs are po<idx> in
 * the SAME order.  Each `.names` becomes a LUTk (its INIT rebuilt from the
 * SOP cover); PIs bind to the caller's input signals, POs feed g.outputs.
 * The AIGER output literal already folds any inversion, so po<j> IS the
 * final output value (no extra ~: needed).
 *
 * Validated functionally equivalent to the native cover (300/300 random
 * vectors on alu/cs_registers/decoder/multdiv/dm_csrs/register_file). *)

open! Base
module Tt = Lut_cover.Tt

(* ---- binary AIGER (.aig) writer -------------------------------------------
 * abc's read_aiger wants BINARY aiger (ascii .aag is rejected).  Same variable
 * numbering as Lut_cover.write_aag: inputs 1..I in node order, ANDs I+1..M in
 * topological node order (so a child's var < its parent's).  Binary form omits
 * the input list and delta-encodes each AND's two fanins (LEB128). *)
let write_aig_binary (g : Lut_cover.graph) (path : string) : unit =
  let n = Array.length g.nodes in
  let var_of = Array.create ~len:n 0 in
  let n_in = ref 0 in
  Array.iter g.nodes ~f:(fun nd ->
    match nd.Lut_cover.gate with
    | Lut_cover.Input _ -> Int.incr n_in; var_of.(nd.Lut_cover.id) <- !n_in
    | _ -> ());
  let m = ref !n_in in
  Array.iter g.nodes ~f:(fun nd ->
    match nd.Lut_cover.gate with
    | Lut_cover.And2 _ -> Int.incr m; var_of.(nd.Lut_cover.id) <- !m
    | _ -> ());
  let lit_of id inv =
    match g.nodes.(id).Lut_cover.gate with
    | Lut_cover.Const b -> (if b then 1 else 0) lxor (if inv then 1 else 0)
    | _ -> (var_of.(id) * 2) lor (if inv then 1 else 0)
  in
  let n_and = !m - !n_in and n_out = List.length g.outputs in
  let buf = Buffer.create 8192 in
  Buffer.add_string buf (Printf.sprintf "aig %d %d 0 %d %d\n" !m !n_in n_out n_and);
  (* outputs: ascii decimal literals, one per line *)
  List.iter g.outputs ~f:(fun (_, id, inv) ->
    Buffer.add_string buf (Printf.sprintf "%d\n" (lit_of id inv)));
  (* AND gates: binary delta encoding, in ascending-lhs (= node-array) order *)
  let put_uint x =
    let x = ref x in
    let continue = ref true in
    while !continue do
      let byte = !x land 0x7f in
      x := !x lsr 7;
      if !x = 0 then (Buffer.add_char buf (Char.of_int_exn byte); continue := false)
      else Buffer.add_char buf (Char.of_int_exn (byte lor 0x80))
    done
  in
  Array.iter g.nodes ~f:(fun nd ->
    match nd.Lut_cover.gate with
    | Lut_cover.And2 { a; b; a_inv; b_inv } ->
      let lhs = var_of.(nd.Lut_cover.id) * 2 in
      let l0 = lit_of a a_inv and l1 = lit_of b b_inv in
      let rhs0 = Int.max l0 l1 and rhs1 = Int.min l0 l1 in
      put_uint (lhs - rhs0);
      put_uint (rhs0 - rhs1)
    | _ -> ());
  Stdio.Out_channel.write_all path ~data:(Buffer.contents buf)

(* ---- BLIF parse ------------------------------------------------------------ *)
(* One combinational LUT: output name, ordered input names, and the SOP cover
 * as (input-cube, output-plane-char) pairs (all share one plane in abc BLIF). *)
type entry = { e_ins : string list; e_cubes : (string * char) list }

let parse_blif (path : string) : string list * string list * (string, entry) Hashtbl.t =
  let raw = Stdio.In_channel.read_all path in
  (* splice backslash-continued lines *)
  let raw = String.substr_replace_all raw ~pattern:"\\\n" ~with_:" " in
  let lines = String.split_lines raw in
  let inputs = ref [] and outputs = ref [] in
  let defn = Hashtbl.create (module String) in
  let arr = Array.of_list lines in
  let n = Array.length arr in
  let i = ref 0 in
  while !i < n do
    let ln = String.strip arr.(!i) in
    if String.is_prefix ln ~prefix:".inputs" then
      inputs := List.tl_exn (String.split ln ~on:' ' |> List.filter ~f:(fun s -> not (String.is_empty s)))
    else if String.is_prefix ln ~prefix:".outputs" then
      outputs := List.tl_exn (String.split ln ~on:' ' |> List.filter ~f:(fun s -> not (String.is_empty s)))
    else if String.is_prefix ln ~prefix:".names" then begin
      let toks = String.split ln ~on:' ' |> List.filter ~f:(fun s -> not (String.is_empty s)) |> List.tl_exn in
      let out = List.last_exn toks in
      let sig_ins = List.drop_last_exn toks in
      let cubes = ref [] in
      let j = ref (!i + 1) in
      let continue = ref true in
      while !continue && !j < n do
        let cl = String.strip arr.(!j) in
        if String.is_empty cl || String.is_prefix cl ~prefix:"." then continue := false
        else begin
          let parts = String.split cl ~on:' ' |> List.filter ~f:(fun s -> not (String.is_empty s)) in
          (match parts with
           | [ pat; ov ] -> cubes := (pat, ov.[0]) :: !cubes
           | [ ov ] -> cubes := ("", ov.[0]) :: !cubes   (* 0-input const *)
           | _ -> ());
          Int.incr j
        end
      done;
      Hashtbl.set defn ~key:out ~data:{ e_ins = sig_ins; e_cubes = List.rev !cubes };
      i := !j - 1
    end;
    Int.incr i
  done;
  !inputs, !outputs, defn

(* Rebuild a LUT's INIT (truth table) from its SOP cover.  Bit m of the table
 * is the output when input p carries bit p of m — matching emit_cut_signal. *)
let tt_of_cover ~(nin : int) (cubes : (string * char) list) : Tt.t =
  let tt = Tt.zero ~k:nin in
  let plane = match cubes with (_, c) :: _ -> c | [] -> '1' in
  for m = 0 to (1 lsl nin) - 1 do
    let matched =
      List.exists cubes ~f:(fun (pat, _) ->
        String.foldi pat ~init:true ~f:(fun p acc ch ->
          acc
          && (match ch with
              | '1' -> (m lsr p) land 1 = 1
              | '0' -> (m lsr p) land 1 = 0
              | _ -> true)))
    in
    let v = if Char.equal plane '1' then matched else not matched in
    if v then Tt.set_bit tt m true
  done;
  tt

(* ---- driver ---------------------------------------------------------------- *)
let abc_bin () =
  match (Fpga_opts.get ()).Fpga_opts.abc_bin with
  | Some s -> s
  | None ->
    (match Fpga_opts.find_abc () with
     | Some s -> s
     | None -> "deps/yosys/yosys-abc")

(* Default flow: explicit resyn2 (the alias needs abc.rc, which yosys-abc
   does not load) then choice-based DELAY mapping with area recovery, then mfs
   -- one ROUND.  Re-strashing the mapped network and running another round
   re-derives structure from the previous cover, and usually wins area, but
   ABC is chaotic here: the same picorv32 netlist came out 1173 / 1215 / 1263
   LUTs at 3 / 2 / 4 rounds, and a 1-gate change upstream can move any one of
   them by +-60.  So one ABC run writes the network after every round and
   [pick_round] keeps the smallest whose LUT depth is no worse than round 1's
   -- the noise then only ever helps.  SVS_ABC_ROUNDS (default 4) sets the
   number of rounds.  SVS_ABC_SCRIPT overrides the whole script (single
   result); GATE_MAP_ABC_AREA=1 selects the old area-only form. *)
let resyn2 = "balance; rewrite; refactor; balance; rewrite; rewrite -z; balance; refactor -z; rewrite -z; balance"
let round = "strash; " ^ resyn2 ^ "; dch -f; if -K 6; mfs -W 4"
let abc_rounds () =
  match Stdlib.Sys.getenv_opt "SVS_ABC_ROUNDS" with
  | Some r -> (try Int.max 1 (Int.of_string r) with _ -> 4)
  | None -> 4

(* Some script = fixed single-result script; None = the multi-round flow *)
let abc_script () =
  match Stdlib.Sys.getenv_opt "SVS_ABC_SCRIPT" with
  | Some s when String.length s > 0 -> Some s
  | _ ->
    (match Stdlib.Sys.getenv_opt "GATE_MAP_ABC_AREA" with
     | Some "1" -> Some ("strash; " ^ resyn2 ^ "; dch -f; if -K 6 -a; mfs -W 4")
     | _ -> None)

(* LUT count and LUT depth of a parsed BLIF (1-input nodes are aliases /
   inverters the emitter folds away, so they cost nothing) *)
let blif_cost (_, pos, defn) =
  let real e = List.length e.e_ins >= 2 in
  let luts = Hashtbl.count defn ~f:real in
  let memo = Hashtbl.create (module String) in
  let rec depth nm =
    match Hashtbl.find memo nm with
    | Some d -> d
    | None ->
      Hashtbl.set memo ~key:nm ~data:0;            (* cycle guard *)
      let d =
        match Hashtbl.find defn nm with
        | None -> 0
        | Some e ->
          let sub = List.fold e.e_ins ~init:0 ~f:(fun acc i -> Int.max acc (depth i)) in
          if real e then sub + 1 else sub in
      Hashtbl.set memo ~key:nm ~data:d;
      d in
  let d = List.fold pos ~init:0 ~f:(fun acc o -> Int.max acc (depth o)) in
  luts, d

(* rounds in order; keep the fewest LUTs among those no deeper than round 1 *)
let pick_round parsed =
  let costs = List.map parsed ~f:(fun p -> p, blif_cost p) in
  let _, (_, d0) = List.hd_exn costs in
  let best, (bl, bd) =
    List.fold costs ~init:(List.hd_exn costs) ~f:(fun ((_, (bl, _)) as acc) ((_, (l, d)) as c) ->
      if d <= d0 && l < bl then c else acc) in
  if Option.is_some (Stdlib.Sys.getenv_opt "SVS_ABC_DEBUG") then
    Stdlib.Printf.eprintf "[abc_cover] rounds (luts/depth): %s -> %d/%d\n%!"
      (String.concat ~sep:" " (List.map costs ~f:(fun (_, (l, d)) -> Printf.sprintf "%d/%d" l d)))
      bl bd;
  best

(* Map [g] with abc and return one Signal.t per g.outputs entry (in order).
 * [input_nodes].(i) is the node id of the i-th Input (i.e. pi<i>); [resolve_input]
 * gives the caller's wire for a given input node id (port pad / register Q). *)
let map_graph
    (g : Lut_cover.graph)
    ~(k : int)
    ~(name : string)
    ~(input_nodes : int array)
    ~(resolve_input : int -> Hardcaml.Signal.t)
  : Hardcaml.Signal.t array
  =
  ignore k;
  (* A module with no logic of its own -- CVA6's axi_xbar_intf only re-packs
     an interface into structs -- gives a graph with no outputs, and abc
     aborts on the empty network (Abc_NtkCheckNames assertion, rc 134). *)
  if List.is_empty g.outputs then [||] else
  let base = Stdlib.Filename.temp_file ("svs_abc_" ^ name ^ "_") "" in
  let aig = base ^ ".aig" and blif = base ^ ".blif" in
  write_aig_binary g aig;
  let script, outs =
    match abc_script () with
    | Some sc -> sc ^ "; write_blif " ^ blif, [ blif ]
    | None ->
      let outs = List.init (abc_rounds ()) ~f:(fun r -> Printf.sprintf "%s.r%d.blif" base r) in
      String.concat ~sep:"; " (List.map outs ~f:(fun o -> round ^ "; write_blif " ^ o)), outs in
  let cmd =
    Printf.sprintf "'%s' -q 'read_aiger %s; %s' >/dev/null 2>&1" (abc_bin ()) aig script
  in
  let rc = Stdlib.Sys.command cmd in
  let parsed =
    List.filter_map outs ~f:(fun o ->
      if Stdlib.Sys.file_exists o then begin
        let r = parse_blif o in
        (try Stdlib.Sys.remove o with _ -> ());
        Some r
      end else None) in
  if rc <> 0 || List.is_empty parsed then
    failwith (Printf.sprintf "abc_cover: abc failed (rc=%d) for %s [cmd: %s]" rc name cmd);
  let pis, pos, defn = pick_round parsed in
  (try Stdlib.Sys.remove aig with _ -> ());
  let pi_arr = Array.of_list pis in
  if Array.length pi_arr <> Array.length input_nodes then
    Stdlib.Printf.eprintf
      "[abc_cover] WARN %s: blif PIs=%d but graph inputs=%d\n%!"
      name (Array.length pi_arr) (Array.length input_nodes);
  let wires = Hashtbl.create (module String) in
  (* seed primary-input wires by position *)
  Array.iteri pi_arr ~f:(fun i nm ->
    if i < Array.length input_nodes then
      Hashtbl.set wires ~key:nm ~data:(resolve_input input_nodes.(i)));
  (* abc materialises every primary output (and some internal nets) as a
     buffer/inverter LUT1.  Emitting those verbatim floods the netlist with
     LUT1s (the native cover has none — it maps the AIG directly).  So fold
     trivial nodes: 0-input -> const, 1-input identity -> alias (NO lut),
     1-input inversion -> ~: (Hardcaml folds NOT into the consuming LUT INIT
     during of_circuit, so it does not cost a standalone LUT1). *)
  let open Hardcaml in
  (* abc's `if` duplicates a node into every cut that covers it, and the copies
     come back as distinct BLIF nets with the same function of the same
     inputs.  Hash-cons them here: one LUT per (truth table, input signals).
     On the VM each heap-tile address pin had its own copy -- 576 identical
     LUTs per address bit, 10k of 19k LUTs.  ABC_NODEDUP=1 skips. *)
  let dedup = Option.is_none (Stdlib.Sys.getenv_opt "ABC_NODEDUP") in
  let lut_cache = Hashtbl.Poly.create () in
  let emit ~complement ~ins tt =
    if not dedup then Lut_cover.emit_cut_signal ~complement ~ins tt
    else begin
      let nin = List.length ins in
      let key = (complement, nin, Tt.to_bool_list ~k:nin tt, List.map ins ~f:Signal.uid) in
      match Hashtbl.find lut_cache key with
      | Some s -> s
      | None ->
        let s = Lut_cover.emit_cut_signal ~complement ~ins tt in
        Hashtbl.set lut_cache ~key ~data:s; s
    end in
  let rec resolve nm =
    match Hashtbl.find wires nm with
    | Some s -> s
    | None ->
      let s =
        match Hashtbl.find defn nm with
        | None -> Signal.gnd   (* dangling net -> 0 *)
        | Some e ->
          let nin = List.length e.e_ins in
          let tt = tt_of_cover ~nin e.e_cubes in
          if nin = 0 then (if Tt.bit tt 0 then Signal.vdd else Signal.gnd)
          else if nin = 1 then begin
            (* 2-bit tt: 0b10 buffer, 0b01 inverter, 0b00 gnd, 0b11 vdd *)
            let b0 = Tt.bit tt 0 and b1 = Tt.bit tt 1 in
            let x = resolve (List.hd_exn e.e_ins) in
            match b0, b1 with
            | false, true -> x                 (* identity: alias *)
            | true, false -> Signal.( ~: ) x   (* inverter *)
            | false, false -> Signal.gnd
            | true, true -> Signal.vdd
          end
          else
            let ins = List.map e.e_ins ~f:resolve in
            emit ~complement:false ~ins tt
      in
      Hashtbl.set wires ~key:nm ~data:s;
      s
  in
  (* A primary output drives a non-LUT pin (FF D, CARRY4 S/DI, port), where an
     inverter cannot fold into a consumer and would become a LUT1.  When the
     output inverts a real LUT, emit that LUT with its truth table complemented
     instead: never more LUTs than the LUT1 (a duplicate at worst, when the
     positive form is used elsewhere) and one level shallower.  On picorv32
     the LUT1s fall 37 -> 21 at an unchanged total -- those inverted outputs
     were all also used positively. *)
  let resolve_po nm =
    match Hashtbl.find defn nm with
    | Some { e_ins = [ inner ]; e_cubes } when not (Hashtbl.mem wires nm) ->
      let tt = tt_of_cover ~nin:1 e_cubes in
      (match Tt.bit tt 0, Tt.bit tt 1, Hashtbl.find defn inner with
       | true, false, Some ie when List.length ie.e_ins >= 2 && not (Hashtbl.mem wires inner) ->
         let s =
           emit ~complement:true
             ~ins:(List.map ie.e_ins ~f:resolve)
             (tt_of_cover ~nin:(List.length ie.e_ins) ie.e_cubes) in
         Hashtbl.set wires ~key:nm ~data:s;
         s
       | _ -> resolve nm)
    | _ -> resolve nm
  in
  Array.of_list (List.map pos ~f:resolve_po)
