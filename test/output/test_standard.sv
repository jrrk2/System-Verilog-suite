module counter (
  input logic clk,
  input logic reset,
  output logic [7:0] count
);
  logic [7:0] count_reg;

  assign count = count_reg;
  always_ff @(posedge clk) begin
    begin
      count_reg <= (reset ? 8'h0 : (8'h1 + count_reg));
    end
  end
endmodule