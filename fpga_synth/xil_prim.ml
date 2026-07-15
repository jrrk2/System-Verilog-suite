(* Xilinx 7-series primitive instantiation helpers.
 *
 * Each primitive is a black box we drop into the netlist via the
 * hardcaml instantiation idiom validated against hardcaml v0.17.1:
 *   Instantiation.create_with_interface (module I) (module O) ~name i
 * (the user's `Circuit.With_interface.inst` is not a v0.17.1 API).
 *
 * The tech mapper (lut_cover for random logic, register/IO/DSP/BRAM
 * inference elsewhere) calls these to build the final primitive
 * netlist that fpga_emit renders for nextpnr-xilinx. *)

open! Base
open Hardcaml

(* ---- LUTs --------------------------------------------------------- *)

(* Build the INIT parameter for a K-input LUT from its truth table.
 * [truth] is the 2^K function values, index i = f(input-combination i)
 * with input j contributing bit j of i.  Rendered MSB-first as the
 * INIT bit-vector Xilinx expects.
 * TODO: finalise the parameter encoding once we see how nextpnr's
 * JSON consumer wants INIT (Std_logic_vector vs sized literal); using
 * a binary String keeps the scaffold compiling and human-checkable. *)
let init_param (truth : bool list) : Parameter.t =
  let bits =
    List.rev truth
    |> List.map ~f:(fun b -> if b then '1' else '0')
    |> String.of_char_list
  in
  Parameter.create ~name:"INIT"
    ~value:(Parameter.Value.Std_logic_vector (Logic.Std_logic_vector.of_string bits))

(* Generic K-input LUT.  [inputs] feeds I0..I(k-1); returns the 1-bit O. *)
let lutk ~(truth : bool list) (inputs : Signal.t list) : Signal.t =
  let k = List.length inputs in
  let named = List.mapi inputs ~f:(fun i s -> (Printf.sprintf "I%d" i, s)) in
  let outs =
    Instantiation.create
      ~parameters:[ init_param truth ]
      ()
      ~name:(Printf.sprintf "LUT%d" k)
      ~inputs:named
      ~outputs:[ ("O", 1) ]
  in
  Map.find_exn outs "O"

(* MUXF7 / MUXF8: SLICE wide-function muxes that combine two LUT6
 * outputs (MUXF7) or two MUXF7 outputs (MUXF8) on a 1-bit select.
 * Used by lut_cover to implement k=7 / k=8 cuts as 2/4 LUT6 cells
 * stitched through these wide muxes — saves a LUT-cascade level
 * vs synthesising the same function from LUT6 + LUT6→LUT6 fanout. *)
let muxf ~name (i0 : Signal.t) (i1 : Signal.t) (s : Signal.t) : Signal.t =
  let outs =
    Instantiation.create () ~name
      ~inputs:[ ("I0", i0); ("I1", i1); ("S", s) ]
      ~outputs:[ ("O", 1) ]
  in
  Map.find_exn outs "O"

let muxf7 i0 i1 s = muxf ~name:"MUXF7" i0 i1 s
let muxf8 i0 i1 s = muxf ~name:"MUXF8" i0 i1 s

(* ---- Registers ---------------------------------------------------- *)

(* FDRE: D flip-flop, clock-enable + synchronous reset (the common
 * mapping target for a plain enabled register with sync reset). *)
module Fdre = struct
  module I = struct
    type 'a t =
      { c  : 'a [@rtlname "C"]
      ; ce : 'a [@rtlname "CE"]
      ; r  : 'a [@rtlname "R"]
      ; d  : 'a [@rtlname "D"]
      }
    [@@deriving hardcaml]
  end
  module O = struct
    type 'a t = { q : 'a [@rtlname "Q"] } [@@deriving hardcaml]
  end
  (* INIT param: per-FDRE config-time value (0 or 1).  Source's
     [reg ... = <init>] threads through BIR initial_value, Hardcaml
     reg_reset_value, bir_to_aig rb_init, and lands here as one bit. *)
  let create ?(init : bool = false) ?(instance : string = "") (i : Signal.t I.t)
    : Signal.t O.t =
    let parameters =
      [ Parameter.create ~name:"INIT"
          ~value:(Parameter.Value.Bit init) ]
    in
    let inputs = [ "C", i.c; "CE", i.ce; "R", i.r; "D", i.d ] in
    let instance = if String.length instance = 0 then None else Some instance in
    let outs =
      Instantiation.create ~parameters ?instance () ~name:"FDRE" ~inputs
        ~outputs:[ "Q", 1 ]
    in
    { O.q = Map.find_exn outs "Q" }
end

(* FDSE: D flip-flop with clock-enable + SYNCHRONOUS set (to 1).  The S
   counterpart of FDRE, for a sync-reset register bit whose reset VALUE is 1.
   S (like R) has priority over CE. *)
module Fdse = struct
  module I = struct
    type 'a t =
      { c  : 'a [@rtlname "C"]
      ; ce : 'a [@rtlname "CE"]
      ; s  : 'a [@rtlname "S"]
      ; d  : 'a [@rtlname "D"]
      }
    [@@deriving hardcaml]
  end
  module O = struct
    type 'a t = { q : 'a [@rtlname "Q"] } [@@deriving hardcaml]
  end
  let create ?(init : bool = false) ?(instance : string = "") (i : Signal.t I.t)
    : Signal.t O.t =
    let parameters =
      [ Parameter.create ~name:"INIT"
          ~value:(Parameter.Value.Bit init) ]
    in
    let inputs = [ "C", i.c; "CE", i.ce; "S", i.s; "D", i.d ] in
    let instance = if String.length instance = 0 then None else Some instance in
    let outs =
      Instantiation.create ~parameters ?instance () ~name:"FDSE" ~inputs
        ~outputs:[ "Q", 1 ]
    in
    { O.q = Map.find_exn outs "Q" }
end

(* FDCE: D flip-flop with clock-enable + asynchronous clear. *)
module Fdce = struct
  module I = struct
    type 'a t =
      { c   : 'a [@rtlname "C"]
      ; ce  : 'a [@rtlname "CE"]
      ; clr : 'a [@rtlname "CLR"]
      ; d   : 'a [@rtlname "D"]
      }
    [@@deriving hardcaml]
  end
  module O = struct
    type 'a t = { q : 'a [@rtlname "Q"] } [@@deriving hardcaml]
  end
  let create ?(instance : string = "") (i : Signal.t I.t) : Signal.t O.t =
    let instance = if String.length instance = 0 then None else Some instance in
    Instantiation.create_with_interface (module I) (module O) ?instance ~name:"FDCE" i
end

(* FDPE: D flip-flop with clock-enable + asynchronous PRESET (to 1).
   The counterpart of FDCE for registers whose async reset value is 1
   (e.g. a reset synchroniser `if(!lock) x <= '1`).  Mapping such a
   register to FDCE (clear-to-0) loses the reset value and, in practice,
   also drops the async-reset connection. *)
module Fdpe = struct
  module I = struct
    type 'a t =
      { c   : 'a [@rtlname "C"]
      ; ce  : 'a [@rtlname "CE"]
      ; pre : 'a [@rtlname "PRE"]
      ; d   : 'a [@rtlname "D"]
      }
    [@@deriving hardcaml]
  end
  module O = struct
    type 'a t = { q : 'a [@rtlname "Q"] } [@@deriving hardcaml]
  end
  let create (i : Signal.t I.t) : Signal.t O.t =
    Instantiation.create_with_interface (module I) (module O) ~name:"FDPE" i
end

(* ---- IO / clock buffers ------------------------------------------- *)

let unary ~name (i : Signal.t) : Signal.t =
  let outs =
    Instantiation.create () ~name ~inputs:[ ("I", i) ] ~outputs:[ ("O", 1) ]
  in
  Map.find_exn outs "O"

let bufg i = unary ~name:"BUFG" i
let ibuf i = unary ~name:"IBUF" i
let obuf i = unary ~name:"OBUF" i

(* IBUFDS: differential input buffer (LVDS, LVDS_25, etc.).  Combines
 * a P/N pad pair into a single-ended O.  Common case is a differential
 * clock pad (VC707's 200 MHz SYSCLK_P/SYSCLK_N) — pair with [bufg] on
 * the output for global clock distribution.
 *
 * Parameters left at Xilinx defaults (DIFF_TERM=FALSE, IBUF_LOW_PWR=
 * TRUE, IOSTANDARD="DEFAULT").  Override at the integration point if
 * a board needs DIFF_TERM=TRUE or a specific LVDS_25 standard; both
 * nextpnr-xilinx and Vivado read the parameters from the EDIF/JSON
 * we emit. *)
let ibufds ?(diff_term : bool = false) ?(iostandard : string = "DEFAULT")
    ~(i : Signal.t) ~(ib : Signal.t) () : Signal.t =
  let parameters =
    [ Parameter.create ~name:"DIFF_TERM"
        ~value:(Parameter.Value.String (if diff_term then "TRUE" else "FALSE"))
    ; Parameter.create ~name:"IBUF_LOW_PWR"
        ~value:(Parameter.Value.String "TRUE")
    ; Parameter.create ~name:"IOSTANDARD"
        ~value:(Parameter.Value.String iostandard)
    ]
  in
  let outs =
    Instantiation.create () ~parameters ~name:"IBUFDS"
      ~inputs:[ ("I", i); ("IB", ib) ] ~outputs:[ ("O", 1) ]
  in
  Map.find_exn outs "O"
