module fourstate (
  input logic clk,
  input logic reset,
  output logic [31:0] data
);
  logic [31:0] reg1;

  always_ff @(posedge clk) begin
    if (reset)
      reg1 <= 32'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx;
    else
      reg1 <= reg1 + 1;
  end

  assign data = reg1;
endmodule
