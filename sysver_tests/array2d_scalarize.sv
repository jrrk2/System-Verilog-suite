// A 2-D unpacked array of a packed struct, written per element and per field
// in one clocked block and read with a dynamic inner index.  The verilator
// front end scalarises the inner dimension (mem__0 / mem__1); flattening it
// into the word made the second non-blocking write drop the first.  Verilator
// simulating this source against the front end's dump (gl_cosim.sh GL_NET_V)
// is the check; verible and yosys-slang both misread the array (open).
module array2d_scalarize(input logic clk, input logic [5:0] idx, input logic j, input logic we, input logic [2:0] d, output logic [2:0] q, output logic [5:0] row);
  typedef struct packed { logic valid; logic [1:0] cnt; } e_t;
  e_t mem [63:0][1:0];
  always_ff @(posedge clk) begin
    if (we) mem[idx][j] <= d;
    if (we) mem[idx][~j].cnt <= d[1:0];
  end
  assign q = mem[idx][j];
  assign row = {mem[idx][1], mem[idx][0]};
endmodule
