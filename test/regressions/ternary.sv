// `s ? a : b` — Verible parses as `cond_expr2` which is TUPLE6, not
// TUPLE5 as the original handler assumed. Without the fix the
// ternary fell through to the generic TUPLE6 wrapper and lost the
// then/else slots.
module ternary (
  input  logic       s,
  input  logic [3:0] a,
  input  logic [3:0] b,
  output logic [3:0] y
);
  assign y = s ? a : b;
endmodule
