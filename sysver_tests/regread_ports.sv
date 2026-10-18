// Enable-guarded registered reads of a 1-write memory, the block RAM port
// idiom memlower maps without a flip-flop after the tile:
//   qa shares port A with the write (same address, never in the same cycle:
//      NO_CHANGE), qb is port B.
module regread_ports (
    input  logic        clk,
    input  logic        we, re_a, re_b,
    input  logic [4:0]  addr_a, addr_b,
    input  logic [15:0] wd,
    output logic [15:0] qa, qb
);
  logic [15:0] mem [0:31];
  always_ff @(posedge clk) begin
    if (we && !re_a) mem[addr_a] <= wd;
    if (re_a) qa <= mem[addr_a];
    if (re_b) qb <= mem[addr_b];
  end
endmodule

// qa is also written by other logic (a byte patch), so it is rebuilt as
// re_d1 ? rdata : qa__reg; and here we and re_a may coincide.
module regread_rebuild (
    input  logic        clk,
    input  logic        we, re_a, re_b, patch,
    input  logic [4:0]  addr_a, addr_b,
    input  logic [15:0] wd,
    output logic [15:0] qa, qb
);
  logic [15:0] mem [0:31];
  always_ff @(posedge clk) begin
    if (we) mem[addr_a] <= wd;
    if (re_a) qa <= mem[addr_a];
    else if (patch) qa[7:0] <= wd[7:0];
    if (re_b) qb <= mem[addr_b];
  end
endmodule

// single read port at an address that is not the write address: the write
// takes port A alone, the read port B.
module regread_split (
    input  logic        clk,
    input  logic        we, re,
    input  logic [4:0]  waddr, raddr,
    input  logic [15:0] wd,
    output logic [15:0] q
);
  logic [15:0] mem [0:31];
  always_ff @(posedge clk) begin
    if (we) mem[waddr] <= wd;
    if (re) q <= mem[raddr];
  end
endmodule
