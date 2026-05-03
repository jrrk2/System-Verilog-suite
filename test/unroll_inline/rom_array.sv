// Read-only memory: `mem` is read but never written.
// behavioral_meminfer flags it as kind = BRom (with empty init —
// the contents come from elsewhere; we just record the indexed-read
// pattern for downstream encoding).

module rom_array (
    input  logic [2:0] adr,
    output logic [7:0] do_o
);
    reg [7:0] mem [0:7];
    initial begin
        mem[0] = 8'h12;
        mem[1] = 8'h34;
        mem[2] = 8'h56;
        mem[3] = 8'h78;
        mem[4] = 8'h9a;
        mem[5] = 8'hbc;
        mem[6] = 8'hde;
        mem[7] = 8'hf0;
    end
    assign do_o = mem[adr];
endmodule
