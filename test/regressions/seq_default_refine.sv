// Sequential always block with an unconditional default in the else
// branch followed by a conditional refinement. Verible's parser
// builds the seq_block's TLIST in reverse source order
// (`TLIST ($2 :: lst)`); without the reverse on conversion the
// refinement runs before the default and the default overwrites it.
module seq_default_refine (
  input  logic       clk,
  input  logic       rst,
  input  logic       en,
  input  logic [3:0] d,
  output logic [3:0] q
);
  always @(posedge clk or posedge rst) begin
    if (rst) q <= 0;
    else begin
      q <= 4'b0;        // unconditional default in else branch
      if (en) q <= d;   // refinement should override
    end
  end
endmodule
