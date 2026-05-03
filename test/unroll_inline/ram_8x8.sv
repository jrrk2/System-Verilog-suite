// RAM inference test: write port + async read port on a single
// `reg [7:0] mem [0:7]` array. behavioral_meminfer should detect the
// `@mem_write` intrinsic in the always_ff body and emit a `bmem`
// declaration with kind = BRam.

module ram_8x8 (
    input  logic       clk,
    input  logic       we,
    input  logic [2:0] adr,
    input  logic [7:0] di,
    output logic [7:0] do_o
);
    reg [7:0] mem [0:7];

    always_ff @(posedge clk)
        if (we) mem[adr] <= di;

    assign do_o = mem[adr];
endmodule
