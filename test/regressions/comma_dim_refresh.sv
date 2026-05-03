// Comma-grouped port lists: when a fresh group has an explicit
// direction but no packed dim (e.g. `output logic a, b` after
// `input [3:0] d`), the inheritance for `a`/`b` must reset width to
// 1 — NOT inherit the previous group's width.
//
// Found via random_sv_gen --seed 110. The smallest sort-error
// reproducer: `b = d[0:0]` is 1-bit on the Verilator side, but
// Verible was inferring `b` as 4-bit (carrying over from `[3:0] d`),
// causing a Z3 BitVec sort mismatch.
module comma_dim_refresh (
  input  [15:0] c,
  input  [3:0]  d,
  output        a, b
);
  assign a = c[0];
  assign b = d[0];
endmodule
