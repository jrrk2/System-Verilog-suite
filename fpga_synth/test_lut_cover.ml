(* Map a hand-built AIG to a Xilinx-LUT netlist and emit it.
 *
 * Design: f = (a & b) | (c & d), in AIG form (only AND + inversion):
 *   n4 = a & b
 *   n5 = c & d
 *   n6 = ~n4 & ~n5         (= ~((a&b) | (c&d)))
 *   f  = ~n6               (output inversion)
 * With k=6 the whole cone collapses to one 4-input LUT. *)
open! Base
open Fpga_synth
open Lut_cover

let g =
  { nodes =
      [| { id = 0; gate = Input "a" }
       ; { id = 1; gate = Input "b" }
       ; { id = 2; gate = Input "c" }
       ; { id = 3; gate = Input "d" }
       ; { id = 4; gate = And2 { a = 0; b = 1; a_inv = false; b_inv = false } }
       ; { id = 5; gate = And2 { a = 2; b = 3; a_inv = false; b_inv = false } }
       ; { id = 6; gate = And2 { a = 4; b = 5; a_inv = true; b_inv = true } }
      |]
  ; outputs = [ ("f", 6, true) ]
  }

let bits_to_string truth =
  List.rev truth
  |> List.map ~f:(fun b -> if b then '1' else '0')
  |> String.of_char_list

let () =
  let k = 6 in
  let chosen = cover ~k g in
  Stdio.printf "cover (k=%d): %d LUT(s)\n" k (List.length chosen);
  List.iter chosen ~f:(fun c ->
    let truth = truth_table_of_cut g c in
    let k_c = List.length c.leaves in
    Stdio.printf "  LUT%d  root=%d  leaves=[%s]  INIT(msb..lsb)=%s\n"
      k_c c.root
      (String.concat ~sep:";" (List.map c.leaves ~f:Int.to_string))
      (bits_to_string (Tt.to_bool_list ~k:k_c truth)));
  Stdio.print_endline "---- netlist ----";
  Fpga_emit.emit_verilog (map_to_luts ~k ~name:"and_or" g)
