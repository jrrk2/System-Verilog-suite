// Concat-LHS where each part's declared width matches the RHS's
// available bits — sidesteps the operator-context-width gap (op_concat_lhs
// trips that one), focusing the regression purely on whether each part of
// the LHS receives the right slice of the RHS.  If lowering treats either
// part as a whole-signal write, hi or lo (or both) get scrambled.
module op_concat_lhs_balanced (
    input  [7:0] data,
    output [3:0] hi,
    output [3:0] lo
);
  reg [3:0] hi_r;
  reg [3:0] lo_r;
  always @* {hi_r, lo_r} = data;
  assign hi = hi_r;
  assign lo = lo_r;
endmodule
