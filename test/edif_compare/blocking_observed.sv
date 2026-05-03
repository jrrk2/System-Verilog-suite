// Blocking assignment to an externally-observed variable. The user's rule:
// only blocking destinations that are pure aliases (substituted-away)
// lose their FF. Here `tmp` is a port output, so it MUST be a register
// even though the in-block read of `tmp` after `tmp = a+b` resolves via
// substitution.
//
// Expected:
//   2 RTL_REG (one for q, one for tmp — both 4-bit)  -> 8 per-bit in Vivado
//   1 RTL_ADD
//
// Verifies that DCE doesn't drop FFs for variables observed outside the block.
module blocking_observed (
  input  logic       clk,
  input  logic [3:0] a,
  input  logic [3:0] b,
  output logic [3:0] tmp,
  output logic [3:0] q
);
  always @(posedge clk) begin
    tmp = a + b;
    q  <= tmp;
  end
endmodule
