(* Hardcaml Cyclesim driver for the GUI's waveform window.

   Takes a [Behavioral_ir.bmodule], lowers it through [Behavioral_to_hardcaml]
   to a [Circuit.t], builds a Cyclesim simulator, and runs a fixed number of
   cycles with simple default stimulus:

     - Reset asserted for the first few cycles (matching what most testbenches
       do at start-up).  Polarity detected by name: [rst*]/[reset*] → active
       high, [rstn]/[resetn]/[rst_n] → active low.
     - Clock managed by Cyclesim itself (one full posedge per [cycle] call).
     - All other inputs driven to zero.

     Per cycle, every input and output port is snapshotted as a [Bits.t].  The
     resulting [sim_result] is rendered by [Gui_waveform].

   Stage 1 — port-visible signals only.  Internal nets would require tap-pin
   insertion at circuit-creation time; deferred.                              *)

open Behavioral_ir
open Hardcaml

type trace_signal = {
  ts_name  : string;
  ts_width : int;
  ts_values : Bits.t array;  (* length = n_cycles *)
}

type sim_result = {
  sr_module_name : string;
  sr_cycles      : int;
  sr_inputs      : trace_signal list;
  sr_outputs     : trace_signal list;
}

(* ── Name-based stimulus heuristics ──────────────────────────────────── *)

let ends_with suf s =
  let ls = String.length s and lf = String.length suf in
  ls >= lf && String.sub s (ls - lf) lf = suf

let starts_with pfx s =
  let lp = String.length pfx and ls = String.length s in
  ls >= lp && String.sub s 0 lp = pfx

let is_clock_name n =
  let n = String.lowercase_ascii n in
  n = "clk" || n = "clock"
  || ends_with "_clk" n || ends_with "_clock" n
  || starts_with "clk_" n

let is_reset_name n =
  let n = String.lowercase_ascii n in
  n = "rst" || n = "reset" || n = "resetn" || n = "rstn" || n = "rst_n"
  || ends_with "_rst" n || ends_with "_reset" n
  || ends_with "_resetn" n || ends_with "_rstn" n

let is_active_low_reset n =
  let n = String.lowercase_ascii n in
  ends_with "n" n
  || ends_with "_n" n
  || (is_reset_name n
      && (ends_with "rstn" n || ends_with "resetn" n || ends_with "rst_n" n))

(* ── Run a Cyclesim for N cycles ─────────────────────────────────────── *)

let run ?(n_cycles=128) ?(reset_cycles=4) (bmod : bmodule) : sim_result =
  let circuit = Behavioral_to_hardcaml.create_circuit bmod in
  let sim = Cyclesim.create circuit in
  let in_ports  = Cyclesim.in_ports  sim in
  let out_ports = Cyclesim.out_ports sim in

  (* Pre-allocate per-port snapshot arrays. *)
  let mk_traces ports =
    List.map (fun (name, b) ->
      { ts_name = name;
        ts_width = Bits.width !b;
        ts_values = Array.make n_cycles (Bits.zero (Bits.width !b)) }
    ) ports in
  let inputs  = mk_traces in_ports  in
  let outputs = mk_traces out_ports in

  let drive_input cyc (name, b) =
    let w = Bits.width !b in
    if is_clock_name name then
      (* Cyclesim drives the clock pin itself; leave it alone. *)
      ()
    else if is_reset_name name then begin
      let asserted = cyc < reset_cycles in
      let lo = is_active_low_reset name in
      b :=
        (if asserted = (not lo)
         then Bits.ones w  (* active high asserted, or active low de-asserted *)
         else Bits.zero w)
    end else
      b := Bits.zero w
  in

  for cyc = 0 to n_cycles - 1 do
    List.iter (drive_input cyc) in_ports;
    Cyclesim.cycle sim;
    List.iter2 (fun t (_, b) -> t.ts_values.(cyc) <- !b) inputs in_ports;
    List.iter2 (fun t (_, b) -> t.ts_values.(cyc) <- !b) outputs out_ports
  done;
  { sr_module_name = bmod.name;
    sr_cycles      = n_cycles;
    sr_inputs      = inputs;
    sr_outputs     = outputs }

(* ── Bounded model check by co-simulation ────────────────────────────
   Drive two circuits (built from PREPPED, non-ffripped bmodules — real
   registers, so Cyclesim carries the state) with the SAME random input
   sequence from reset for [n_cycles], comparing every common output port
   each cycle.  This explores only REACHABLE states, so — unlike the
   combinational Z3 miter — it never fires on an unreachable state; a
   divergence it reports is a genuine reachable counterexample with a
   concrete input trace, and no divergence in N cycles is strong evidence
   the designs are bounded-equivalent (the miter's DIFFER was a spurious
   unreachable-state artifact).  Inputs held at 0 during reset, then
   randomized.  Reset polarity / clock detected by name, as in [run]. *)

type bmc_result =
  | Bmc_equiv of int                                 (* agreed for N cycles *)
  | Bmc_diff  of int * string * string list          (* cycle, port, trace lines *)
  | Bmc_error of string

let bmc_compare ?(n_cycles=64) ?(reset_cycles=4) ?(seed=0xB3C0DE)
    (ma : bmodule) (mb : bmodule) : bmc_result =
  try
    Random.init seed;
    (* Yosys's scattered-concat nets can trip Hardcaml's combinational-loop
       check even when acyclic; retry with the check off before giving up. *)
    let mk_sim m =
      try Cyclesim.create (Behavioral_to_hardcaml.create_circuit m)
      with _ -> Cyclesim.create
                  (Behavioral_to_hardcaml.create_circuit ~detect_loops:false m) in
    let sa = mk_sim ma in
    let sb = mk_sim mb in
    let ain = Cyclesim.in_ports sa and aout = Cyclesim.out_ports sa in
    let bin = Cyclesim.in_ports sb and bout = Cyclesim.out_ports sb in
    (* Union of primary-input (name,width); clk left to Cyclesim. *)
    let width_of ports name = match List.assoc_opt name ports with
      | Some b -> Some (Bits.width !b) | None -> None in
    let in_names =
      List.sort_uniq compare (List.map fst ain @ List.map fst bin) in
    let set ports name v = match List.assoc_opt name ports with
      | Some b -> b := v | None -> () in
    (* Common outputs to compare; flag width mismatches as structural. *)
    let common_outs =
      List.filter_map (fun (n, b) ->
        match List.assoc_opt n bout with
        | Some b2 -> Some (n, Bits.width !b, Bits.width !b2)
        | None -> None) aout in
    let hex b = "0b" ^ Bits.to_bstr b in
    let trace = ref [] in
    let diff = ref None in
    let cyc = ref 0 in
    while !diff = None && !cyc < n_cycles do
      let c = !cyc in
      let line = Buffer.create 64 in
      List.iter (fun name ->
        if is_clock_name name then ()
        else begin
          let w = match width_of ain name with Some w -> w
                  | None -> (match width_of bin name with Some w -> w | None -> 1) in
          let v =
            if is_reset_name name then begin
              let asserted = c < reset_cycles in
              let lo = is_active_low_reset name in
              if asserted = (not lo) then Bits.ones w else Bits.zero w
            end else if c < reset_cycles then Bits.zero w
            else Bits.random ~width:w
          in
          set ain name v; set bin name v;
          if not (is_reset_name name) then
            Buffer.add_string line (Printf.sprintf " %s=%s" name (hex v))
        end) in_names;
      Cyclesim.cycle sa; Cyclesim.cycle sb;
      trace := Printf.sprintf "  c%d:%s" c (Buffer.contents line) :: !trace;
      (* compare common outputs *)
      List.iter (fun (n, wa, wb) ->
        if !diff = None then
          if wa <> wb then
            diff := Some (c, Printf.sprintf "%s (width %d vs %d)" n wa wb)
          else begin
            let va = !(List.assoc n aout) and vb = !(List.assoc n bout) in
            if not (Bits.equal va vb) then
              diff := Some (c, Printf.sprintf "%s: %s vs %s" n (hex va) (hex vb))
          end) common_outs;
      incr cyc
    done;
    (match !diff with
     | None -> Bmc_equiv n_cycles
     | Some (c, port) ->
         let tl = List.rev !trace in
         let shown = if List.length tl > 12
           then ("  …" :: (List.filteri (fun i _ -> i >= List.length tl - 12) tl))
           else tl in
         Bmc_diff (c, port, shown))
  with e -> Bmc_error (Printexc.to_string e)

(* ── Pretty-formatters for the renderer ──────────────────────────────── *)

(* 1-bit signal as 0/1.  Wider signals as hex with the right number of nibbles. *)
let format_value ts cyc =
  let b = ts.ts_values.(cyc) in
  let w = ts.ts_width in
  if w = 1 then
    if Bits.is_vdd b then "1" else "0"
  else begin
    (* Bits.to_string emits binary "1010…"; convert to hex.  Hardcaml has
       Bits.to_int but it raises for widths > 62.  Do hex manually so we
       handle any width. *)
    let s = Bits.to_string b in
    let nbits = String.length s in
    let n_nib = (nbits + 3) / 4 in
    let pad = (4 - nbits mod 4) mod 4 in
    let s = String.make pad '0' ^ s in
    let buf = Buffer.create (n_nib + 2) in
    Buffer.add_string buf "0x";
    for i = 0 to n_nib - 1 do
      let nib =
        let c1 = s.[i*4] |> Char.code |> fun x -> x - Char.code '0' in
        let c2 = s.[i*4+1] |> Char.code |> fun x -> x - Char.code '0' in
        let c3 = s.[i*4+2] |> Char.code |> fun x -> x - Char.code '0' in
        let c4 = s.[i*4+3] |> Char.code |> fun x -> x - Char.code '0' in
        c1 * 8 + c2 * 4 + c3 * 2 + c4 in
      Buffer.add_char buf
        (if nib < 10 then Char.chr (nib + Char.code '0')
         else Char.chr (nib - 10 + Char.code 'a'))
    done;
    Buffer.contents buf
  end

(* ── Scripted single-module Cyclesim (portable; no verilog sim) ──────────
   Build a Cyclesim from [create_circuit bmod] — i.e. EXACTLY what gate_map
   feeds to bir_to_aig — and drive it with an explicit per-port stimulus,
   dumping a per-cycle hex trace.  This is the in-process replacement for
   "emit create_circuit as verilog then run it in xsim/iverilog": it exercises
   the create_circuit lowering directly on any platform (pure Hardcaml), so a
   lowering bug (e.g. the fetch_fifo multi-slot array-write clobber) shows up
   as a wrong output stream with no external simulator.  Leaf modules only
   (a module with sub-instances needs the whole hierarchy in the circuit). *)

let bits_hex (b : Bits.t) =
  let w = Bits.width b in
  if w = 1 then (if Bits.is_vdd b then "1" else "0")
  else begin
    let s = Bits.to_string b in
    let nbits = String.length s in
    let n_nib = (nbits + 3) / 4 in
    let pad = (4 - nbits mod 4) mod 4 in
    let s = String.make pad '0' ^ s in
    let buf = Buffer.create (n_nib + 2) in
    Buffer.add_string buf "0x";
    for i = 0 to n_nib - 1 do
      let d j = Char.code s.[i*4+j] - Char.code '0' in
      let nib = (d 0)*8 + (d 1)*4 + (d 2)*2 + (d 3) in
      Buffer.add_char buf
        (if nib < 10 then Char.chr (nib + Char.code '0')
         else Char.chr (nib - 10 + Char.code 'a'))
    done;
    Buffer.contents buf
  end

(* Per-input drive modes.  Everything is explicit (default [DZero]) so the
   trace is reproducible with no name-based reset guessing. *)
type drive_mode =
  | DZero            (* hold 0 *)
  | DOne             (* hold all-ones *)
  | DConst of int    (* hold a constant *)
  | DHex of string   (* hold a wide constant given as a hex string (any width) *)
  | DCount of int    (* value = base + cycle (a walking counter) *)
  | DPulse of int    (* all-ones for the first N cycles, then 0 *)
  | DPulseLow of int (* 0 for the first N cycles, then all-ones (active-low reset) *)

(* LSB-aligned hex string -> Bits.t of exactly [w] bits (pad/truncate). *)
let hex_to_bits w hexstr =
  let b = Buffer.create (String.length hexstr * 4) in
  String.iter (fun c ->
    let v = match c with
      | '0'..'9' -> Char.code c - Char.code '0'
      | 'a'..'f' -> Char.code c - Char.code 'a' + 10
      | 'A'..'F' -> Char.code c - Char.code 'A' + 10
      | _ -> 0 in
    for k = 3 downto 0 do
      Buffer.add_char b (if (v lsr k) land 1 = 1 then '1' else '0') done) hexstr;
  let s = Buffer.contents b in
  let ls = String.length s in
  let s = if ls >= w then String.sub s (ls - w) w
          else String.make (w - ls) '0' ^ s in
  Bits.of_constant (Constant.of_binary_string s)

let run_spec ?(n_cycles = 48) (modes : (string * drive_mode) list) (bmod : bmodule) : string =
  let circuit =
    try Behavioral_to_hardcaml.create_circuit bmod
    with _ -> Behavioral_to_hardcaml.create_circuit ~detect_loops:false bmod in
  let sim =
    try Cyclesim.create circuit
    with _ -> Cyclesim.create (Behavioral_to_hardcaml.create_circuit ~detect_loops:false bmod) in
  let in_ports = Cyclesim.in_ports sim and out_ports = Cyclesim.out_ports sim in
  let buf = Buffer.create 4096 in
  Buffer.add_string buf
    (Printf.sprintf "# cyclesim %s: %d inputs, %d outputs, %d cycles\n"
       bmod.name (List.length in_ports) (List.length out_ports) n_cycles);
  for cyc = 0 to n_cycles - 1 do
    List.iter (fun (name, b) ->
      let w = Bits.width !b in
      if is_clock_name name then ()   (* Cyclesim drives the clock itself *)
      else
        let mode = try List.assoc name modes with Not_found -> DZero in
        b := (match mode with
          | DZero -> Bits.zero w
          | DOne -> Bits.ones w
          | DConst v -> Bits.of_int ~width:w v
          | DHex h -> hex_to_bits w h
          | DCount base -> Bits.of_int ~width:w (base + cyc)
          | DPulse n -> if cyc < n then Bits.ones w else Bits.zero w
          | DPulseLow n -> if cyc < n then Bits.zero w else Bits.ones w))
      in_ports;
    Cyclesim.cycle sim;
    Buffer.add_string buf (Printf.sprintf "c%-3d |" cyc);
    List.iter (fun (n, b) -> Buffer.add_string buf (Printf.sprintf " %s=%s" n (bits_hex !b)))
      in_ports;
    Buffer.add_string buf "  |";
    List.iter (fun (n, b) -> Buffer.add_string buf (Printf.sprintf " %s=%s" n (bits_hex !b)))
      out_ports;
    Buffer.add_char buf '\n'
  done;
  Buffer.contents buf

(* Same value over two adjacent cycles? *)
let same_at ts c1 c2 =
  Bits.equal ts.ts_values.(c1) ts.ts_values.(c2)
