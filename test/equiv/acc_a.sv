// Reference: a small accumulator + status flags.  Three registers, one of
// them feeding a comparison, so a wrong bit in the adder shows up in one
// cone and not the others.
module acc(
  input  logic       clk,
  input  logic       rst,
  input  logic       en,
  input  logic [7:0] din,
  output logic [7:0] sum,
  output logic       ovf,
  output logic       zero
);
  logic [7:0] acc_q;
  logic [8:0] wide;
  logic       ovf_q;

  always_comb wide = {1'b0, acc_q} + {1'b0, din};

  always_ff @(posedge clk) begin
    if (rst) begin
      acc_q <= 8'h00;
      ovf_q <= 1'b0;
    end else if (en) begin
      acc_q <= wide[7:0];
      ovf_q <= wide[8];
    end
  end

  assign sum  = acc_q;
  assign ovf  = ovf_q;
  assign zero = (acc_q == 8'h00);
endmodule
