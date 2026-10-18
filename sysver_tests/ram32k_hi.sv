// A 32K x 36 memory with one write port and two enable-guarded registered
// reads (the VM's stack): deep enough for the x1-bit RAMB36E1 tiling, with
// only 64 addresses used so random traffic reads what it wrote.
module ram32k_hi (
    input  logic        clk,
    input  logic        we, re_a, re_b,
    input  logic [5:0]  a_lo, b_lo,
    input  logic [35:0] wd,
    output logic [35:0] qa, qb
);
  logic [35:0] mem [0:32767];
  logic [14:0] addr_a, addr_b;
  always_comb begin
    addr_a = {9'h1ff, a_lo};
    addr_b = {9'h1ff, b_lo};
  end
  always_ff @(posedge clk) begin
    if (we && !re_a) mem[addr_a] <= wd;
    if (re_a) qa <= mem[addr_a];
    if (re_b) qb <= mem[addr_b];
  end
endmodule
