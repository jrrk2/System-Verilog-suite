module test_memory_conflict (
  input logic clk,
  input logic [4:0] addr1,
  input logic [4:0] addr2,
  input logic [4:0] addr3,
  input logic [31:0] wdata1,
  input logic [31:0] wdata2,
  input logic [31:0] wdata3,
  output logic [31:0] rdata1,
  output logic [31:0] rdata2,
  output logic [31:0] rdata3
);

  logic [31:0] mem [0:31];  // Memory array

  always_ff @(posedge clk) begin
    // Three writes - should trigger error!
    mem[addr1] <= wdata1;
    mem[addr2] <= wdata2;
    mem[addr3] <= wdata3;

    // Three reads - should trigger error!
    rdata1 <= mem[addr1];
    rdata2 <= mem[addr2];
    rdata3 <= mem[addr3];
  end

endmodule
