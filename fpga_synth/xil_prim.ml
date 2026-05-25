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
  let create (i : Signal.t I.t) : Signal.t O.t =
    Instantiation.create_with_interface (module I) (module O) ~name:"FDRE" i
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
  let create (i : Signal.t I.t) : Signal.t O.t =
    Instantiation.create_with_interface (module I) (module O) ~name:"FDCE" i
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
