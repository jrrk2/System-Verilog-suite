// Chained blocking assignments. Demonstrates that substitution composes —
// after t1 = a+b and t2 = t1<<1, reads of t2 in q <= t2 should resolve to
// (a+b)<<1, not the FF Q of t2 (which would add a clock delay).
//
// Both t1 and t2 are pure aliases (read only via substituted reads); their
// FFs should be DCE'd. q gets a single FF whose D is (a+b)<<1.
module blocking_chain (
  input  logic       clk,
  input  logic [3:0] a,
  input  logic [3:0] b,
  output logic [4:0] q
);
  always @(posedge clk) begin
    logic [4:0] t1, t2;
    t1 = a + b;
    t2 = t1 << 1;
    q  <= t2;
  end
endmodule
