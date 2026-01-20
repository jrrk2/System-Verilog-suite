module test_4state (
  input logic clk,
  input logic reset,
  output logic [31:0] data_out
);

  logic [31:0] unknown_value;
  logic [31:0] highz_value;
  logic [7:0] mixed_value;
  logic single_x;

  always_ff @(posedge clk) begin
    if (reset) begin
      unknown_value <= 32'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx;  // All unknown
      highz_value <= 32'bzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz;    // All high-Z
      mixed_value <= 8'b10xz01xz;                              // Mixed values
      single_x <= 1'bx;                                        // Single unknown bit
      data_out <= 32'h0000_0000;
    end else begin
      data_out <= unknown_value ^ highz_value;
    end
  end

endmodule
