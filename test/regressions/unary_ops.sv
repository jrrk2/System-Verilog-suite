// Unary operator dispatch — TILDE → BNot, HYPHEN → BNeg,
// AMPERSAND/VBAR/CARET → reductions. The handler must match on the
// operator token (slot 1 of `unary_prefix_expr2`); the generic TUPLE3
// fallback at the same arity used to swallow this case and discard
// the operand.
module unary_ops (
  input  logic [3:0] a,
  output logic [3:0] yn,
  output logic [3:0] yneg,
  output logic       yand,
  output logic       yor,
  output logic       yxor
);
  assign yn   = ~a;
  assign yneg = -a;
  assign yand = &a;
  assign yor  = |a;
  assign yxor = ^a;
endmodule
