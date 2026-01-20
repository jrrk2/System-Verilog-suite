// Test case for small array - should use flip-flops
// 4 registers × 8 bits = 32 bits (below flip-flop threshold)

module test_small_array (
  input logic clk,
  input logic rst,
  input logic [1:0] raddr,
  input logic [1:0] waddr,
  input logic [7:0] wdata,
  input logic we,
  output logic [7:0] rdata
);

  // Small array: 4 entries × 8 bits
  logic [7:0] regs [3:0];

  // Write port
  always_ff @(posedge clk) begin
    if (rst) begin
      regs <= '{default: '0};
    end else if (we) begin
      regs[waddr] <= wdata;
    end
  end

  // Read port
  assign rdata = regs[raddr];

endmodule
