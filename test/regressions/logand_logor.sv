// Logical `&&` and `||` (Verible: `logand_expr2` and `logor_expr2`).
// Without the explicit handlers the binary-expression dispatch fell
// through and produced a 1-bit zero.
module logand_logor (
  input  logic a,
  input  logic b,
  output logic y_and,
  output logic y_or
);
  assign y_and = a && b;
  assign y_or  = a || b;
endmodule
