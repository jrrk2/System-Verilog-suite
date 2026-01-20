module counter (
  input logic clk,
  input logic reset,
  output logic [7:0] count
);
  logic [7:0] count_reg;

  always_ff @(posedge clk) begin
    if (reset)
      count_reg <= 8'h0;
    else
      count_reg <= count_reg + 1;
  end

  assign count = count_reg;
endmodule
