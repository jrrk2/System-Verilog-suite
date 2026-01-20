module pipeline (
  input logic clk,
  input logic [31:0] data_in,
  output logic [31:0] data_out
);
  logic [31:0] stage3;
  logic [31:0] stage2;
  logic [31:0] stage1;

  assign data_out = stage3;
  always_ff @(posedge clk) begin
    begin
      stage1 <= data_in;
      stage2 <= stage1;
      stage3 <= stage2;
    end
  end
endmodule