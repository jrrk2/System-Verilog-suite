// Sync-reset FF — Yosys folds this into `$sdff`. The reset is
// inside a single `always @(posedge clk)` (no posedge-rst in the
// sense list). RTLIL converter must emit BSequential with the
// reset-mux body.
module sdff_basic (
  input  logic       clk,
  input  logic       rst,
  input  logic [3:0] d,
  output logic [3:0] q
);
  always @(posedge clk)
    if (rst) q <= 0;
    else     q <= d;
endmodule
