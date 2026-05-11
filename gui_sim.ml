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

(* Same value over two adjacent cycles? *)
let same_at ts c1 c2 =
  Bits.equal ts.ts_values.(c1) ts.ts_values.(c2)
