// Parent design with an attributed adder leaf. Once `verify-arch
// adder brent_kung --width 8` has produced a certificate, the
// substitution pass abstracts u_add to a single BBinOp BAdd in the
// parent miter — Z3 doesn't see the prefix-tree internals.

(* sv_decomp_adder = "brent_kung" *)
module bk_adder8 (
  input  [7:0] x, input [7:0] y, output [7:0] z
);
  // Body could be a real Brent-Kung tree; for the test we just
  // place behavioural `+` here. The substitution rule trusts the
  // attribute + certificate, not the body — body equivalence is
  // proven once by verify-arch.
  assign z = x + y;
endmodule

module top (
  input  [7:0] a, input [7:0] b, output [7:0] s
);
  bk_adder8 u_add (.x(a), .y(b), .z(s));
endmodule
