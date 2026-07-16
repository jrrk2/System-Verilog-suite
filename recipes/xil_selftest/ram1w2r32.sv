// 1-write / 2-read async-read RAM, depth 32 -> must map to RAM32M distributed
// RAM, NOT bit-blast to FFs.  The two reads are in SEPARATE assigns (BUG1: the
// read-address collector previously saw only the first, so depth-32 fell back to
// a 256-FF bit-blast).  Needs MEMLOWER_FPGA=1.
module ram1w2r32 (
  input clk, input we, input [4:0] wa, input [7:0] wd,
  input [4:0] ra0, input [4:0] ra1, output [7:0] rd0, output [7:0] rd1);
  reg [7:0] mem [0:31];
  always @(posedge clk) if (we) mem[wa] <= wd;
  assign rd0 = mem[ra0];
  assign rd1 = mem[ra1];
endmodule
