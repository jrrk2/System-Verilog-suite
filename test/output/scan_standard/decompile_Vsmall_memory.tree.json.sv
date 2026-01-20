module small_memory (
  input logic clk,
  input logic [1:0] addr,
  input logic [7:0] wdata,
  input logic we,
  output logic [7:0] rdata
);
  logic [7:0] [0:3] mem;

  always_ff @(posedge clk) begin
    begin
      if (we)
      mem[addr] <= wdata;
      rdata <= mem[addr];
    end
  end
endmodule