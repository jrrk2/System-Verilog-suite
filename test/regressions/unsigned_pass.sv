// `$unsigned(...)` should be a no-op pass-through (it's a sign cast at
// the BV level). Same for `$signed`.
module unsigned_pass (
  input  logic [3:0] a,
  output logic [4:0] y
);
  assign y = $unsigned({1'b0, a});
endmodule
