(* Smoke test for Symbol_lib parser using the user-supplied example.   *)

let example = {|
/* Example Synopsys Symbol Library (.slib) Source File */

symbol_library (example_sym_lib) {
    technology ( cmos ) ;
    bus_naming_style ( "%s[%d]" ) ;

    symbol (AND2) {
        bbox (0, 0, 40, 40) ;
        line (0, 0, 0, 40) ;
        line (0, 40, 20, 40) ;
        line (0, 0, 20, 0) ;
        arc  (20, 0, 40, 20, 20, 40) ;
        pin (A) {
            direction (input) ;
            line (-10, 30, 0, 30) ;
            connect_point (-10, 30) ;
        }
        pin (B) {
            direction (input) ;
            line (-10, 10, 0, 10) ;
            connect_point (-10, 10) ;
        }
        pin (Y) {
            direction (output) ;
            line (40, 20, 50, 20) ;
            connect_point (50, 20) ;
        }
    }
    symbol (INV) {
        bbox (0, 0, 30, 30) ;
        line (0, 0, 0, 30) ;
        line (0, 30, 20, 15) ;
        line (0, 0, 20, 15) ;
        circle (22, 15, 2) ;
        pin (A) {
            direction (input) ;
            line (-10, 15, 0, 15) ;
            connect_point (-10, 15) ;
        }
        pin (Y) {
            direction (output) ;
            line (24, 15, 35, 15) ;
            connect_point (35, 15) ;
        }
    }
}
|}

let () =
  let lib = Symbol_lib.parse_lib (Symbol_lib.tokenise example) in
  let n = Hashtbl.fold (fun _ _ a -> a + 1) lib 0 in
  Printf.printf "parsed %d symbols\n" n;
  Hashtbl.iter (fun k (s : Symbol_lib.symbol) ->
    let (x1, y1, x2, y2) = s.sym_bbox in
    Printf.printf "  %s : bbox (%g,%g)-(%g,%g), %d prims, %d pins\n"
      k x1 y1 x2 y2
      (List.length s.sym_prims) (List.length s.sym_pins);
    List.iter (fun (p : Symbol_lib.pin) ->
      let (cx, cy) = p.pin_connect in
      let dir = match p.pin_dir with
        | Symbol_lib.PinIn -> "in"
        | PinOut -> "out"
        | PinInOut -> "inout" in
      Printf.printf "      pin %s [%s] connect=(%g,%g) %d prims\n"
        p.pin_name dir cx cy (List.length p.pin_prims)
    ) s.sym_pins
  ) lib;
  (* Auto-gen sanity check. *)
  let auto = Symbol_lib.auto_generate ~cell_name:"DFF"
              ~pins:[("CLK","input"); ("D","input"); ("Q","output")] in
  let (_, _, w, h) = auto.sym_bbox in
  Printf.printf "auto DFF: %gx%g, %d pins, %d prims\n"
    w h (List.length auto.sym_pins) (List.length auto.sym_prims);
  (* Classifier smoke: vendor + drive-strength variants. *)
  let cases = [
    "AND2_X1",   Symbol_lib.GK_And;
    "NAND4_X2",  Symbol_lib.GK_Nand;
    "OR3_X1",    Symbol_lib.GK_Or;
    "NOR2",      Symbol_lib.GK_Nor;
    "XOR2_X1",   Symbol_lib.GK_Xor;
    "XNOR2",     Symbol_lib.GK_Xnor;
    "INV_X4",    Symbol_lib.GK_Inv;
    "BUF_X1",    Symbol_lib.GK_Buf;
    "MX2",       Symbol_lib.GK_Mux;
    "DFFRS_X1",  Symbol_lib.GK_FF;
    "sky130_fd_sc_hd__nand2_2", Symbol_lib.GK_Nand;
    "weird_cell",               Symbol_lib.GK_Generic;
  ] in
  List.iter (fun (nm, want) ->
    let got = Symbol_lib.classify_cell nm in
    if got <> want then begin
      Printf.printf "classify FAIL: %s\n" nm; assert false
    end) cases;
  (* Auto-gen for an AND2 should emit primitives (D shape) not a bare
     rectangle. *)
  let and2 = Symbol_lib.auto_generate ~cell_name:"AND2_X1"
              ~pins:[("A","input"); ("B","input"); ("Z","output")] in
  Printf.printf "auto AND2: %d prims, %d pins\n"
    (List.length and2.sym_prims) (List.length and2.sym_pins);
  assert (and2.sym_prims <> []);
  assert (List.length and2.sym_pins = 3);
  let inv = Symbol_lib.auto_generate ~cell_name:"INV_X1"
              ~pins:[("A","input"); ("ZN","output")] in
  assert (List.length inv.sym_prims >= 4);  (* triangle + bubble *)
  assert (n = 2);
  let and2 = Hashtbl.find lib "AND2" in
  assert (List.length and2.sym_pins = 3);
  assert (List.length and2.sym_prims = 4); (* 3 lines + 1 arc *)
  let inv = Hashtbl.find lib "INV" in
  assert (List.length inv.sym_prims = 4);   (* 3 lines + 1 circle *)
  print_endline "OK"
