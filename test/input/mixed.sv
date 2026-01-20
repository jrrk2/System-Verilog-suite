module mixed (
  input logic clk,
  input logic [7:0] a,
  input logic [7:0] b,
  output logic [7:0] sum,
  output logic [7:0] sum_reg
);
  assign sum = a + b;

  always_ff @(posedge clk) begin
    sum_reg <= a + b;
  end
endmodule
