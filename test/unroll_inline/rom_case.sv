// ROM-from-case inference: every arm assigns the same lhs with a
// constant value. behavioral_meminfer collects the constants into a
// `bmem` (kind = BRom) and rewrites the case as a chain of BConds.

module rom_case (
    input  logic [2:0] sel,
    output logic [7:0] q
);
    always_comb
        case (sel)
            3'd0: q = 8'h12;
            3'd1: q = 8'h34;
            3'd2: q = 8'h56;
            3'd3: q = 8'h78;
            3'd4: q = 8'h9a;
            3'd5: q = 8'hbc;
            3'd6: q = 8'hde;
            3'd7: q = 8'hf0;
        endcase
endmodule
