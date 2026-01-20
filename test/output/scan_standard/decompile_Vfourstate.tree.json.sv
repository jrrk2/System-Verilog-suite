module fourstate (
  input logic clk,
  input logic reset,
  output logic [31:0] data
);
  logic [31:0] reg1;

  assign data = reg1;
  always_ff @(posedge clk) begin
    begin
      reg1 <= (reset ? 32'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx : (32'sh1 + reg1));
    end
  end
endmodule