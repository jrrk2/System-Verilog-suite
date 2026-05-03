// Combinational `always @(*)` with an unconditional default at the
// top followed by a conditional refinement. The must-assign analysis
// must see the default's coverage and stay combinational (not
// promote to a latch).
module default_with_refinement (
  input  logic       s,
  input  logic [3:0] a,
  output logic [3:0] y
);
  reg [3:0] y_r;
  always @(*) begin
    y_r = 4'b0;       // unconditional default
    if (s) y_r = a;   // refinement
  end
  assign y = y_r;
endmodule
