(* Discover Xilinx primitives by FUNCTION/STRUCTURE, not by hardcoded
 * name — the reusable kernel from ~/gnusynthesis/ver_toolbox/read_library.ml
 * (JRRK's 2011-12 tech mapper), retargeted at the Xilinx unisim library.
 *
 * read_library.ml's pattern: a boolean-formula IR + `cnv` that
 * canonicalises a cell's function and matches its SHAPE to a role
 * (Por(Pand(s,b),Pand(a,~s)) -> mux2, Pnot(Pand..) -> nand, ...), then
 * find_buffer'/find_logand'/find_mux2'/find_flipflop_ce'... scan every
 * library cell and select by (role, pin-count, input-count).  Doing the
 * same over unisims means FDRE/CARRY4/RAMB/IBUF/BUFG are discovered
 * with their real port interfaces rather than string-literal'd.
 *
 * Scaffold: formula IR + role type are real; the classify/scan passes
 * are stubs. *)

open! Base

(* Boolean-formula IR (mirrors read_library's Pand/Por/Pnot/Pvar). *)
type formula =
  | Ptrue
  | Pfalse
  | Pvar of string
  | Pnot of formula
  | Pand of formula * formula
  | Por of formula * formula

(* Canonical role a primitive plays in the mapper. *)
type role =
  | Buf
  | Inv
  | And2
  | Or2
  | Xor2
  | Mux2
  | Ff          (* plain DFF *)
  | Ff_ce       (* DFF + clock-enable (FDRE/FDCE family) *)
  | Lut of int  (* k-input LUT *)
  | Carry       (* CARRY4 fast-carry *)
  | Bram        (* RAMB18/36 *)
  | Dsp         (* DSP48 *)
  | Io          (* IBUF/OBUF/BUFG *)
  | Other

(* A discovered primitive: role + module name + ordered port interface. *)
type prim = {
  role : role;
  name : string;                       (* e.g. "FDRE", "LUT6" *)
  inputs : (string * int) list;        (* port name, width *)
  outputs : (string * int) list;
}

(* Canonicalise a function formula and match it to a role (read_library
 * `cnv` analogue). *)
let classify (_f : formula) : role =
  (* TODO: rewrite to canonical form, then match shapes:
     Pvar -> Buf, Pnot(Pvar) -> Inv, Pand(Pvar,Pvar) -> And2,
     Por(Pand(s,b),Pand(a,Pnot s)) -> Mux2, etc. *)
  Other

(* Scan a parsed unisim library directory, returning one chosen prim per
 * role (read_library `restore_lib'` analogue). *)
let scan_library (_dir : string) : prim list =
  (* TODO: parse each unisim sim model (or a curated subset), extract its
     ports + function, classify, and pick a representative per role. *)
  failwith "unisim_scan.scan_library: TODO"
