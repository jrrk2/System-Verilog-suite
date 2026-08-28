// Renamed AND wrong: the carry-out is taken from bit 7 instead of bit 8, so
// `ovf` differs while `sum` and `zero` still match.  A per-cone run must
// localise the difference to the ovf cone alone.
module acc(
  input  logic       clk,
  input  logic       rst,
  input  logic       en,
  input  logic [7:0] din,
  output logic [7:0] sum,
  output logic       ovf,
  output logic       zero
);
  logic [7:0] n42_reg;
  logic [8:0] n17;
  logic       n43_reg;

  always_comb n17 = {1'b0, n42_reg} + {1'b0, din};

  always_ff @(posedge clk) begin
    if (rst) begin
      n42_reg <= 8'h00;
      n43_reg <= 1'b0;
    end else if (en) begin
      n42_reg <= n17[7:0];
      n43_reg <= n17[7];
    end
  end

  assign sum  = n42_reg;
  assign ovf  = n43_reg;
  assign zero = (n42_reg == 8'h00);
endmodule
