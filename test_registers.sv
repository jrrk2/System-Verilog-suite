module test_registers (
  input logic clk,
  input logic reset,
  input logic [31:0] data_in,
  output logic [31:0] data_out,
  output logic [15:0] counter,
  output logic overflow
);

  logic [31:0] pipeline_reg1;
  logic [31:0] pipeline_reg2;
  logic [15:0] count_reg;
  logic overflow_flag;

  always_ff @(posedge clk) begin
    if (reset) begin
      pipeline_reg1 <= 32'h0;
      pipeline_reg2 <= 32'h0;
      count_reg <= 16'h0;
      overflow_flag <= 1'b0;
    end else begin
      pipeline_reg1 <= data_in;
      pipeline_reg2 <= pipeline_reg1;
      count_reg <= count_reg + 1;
      overflow_flag <= (count_reg == 16'hFFFF);
    end
  end

  assign data_out = pipeline_reg2;
  assign counter = count_reg;
  assign overflow = overflow_flag;

endmodule
