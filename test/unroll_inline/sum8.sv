// Loop-unroll test: count the set bits in an 8-bit input via a
// constant-bound `for` loop.
//
// The frontend emits this as `init; while (cond) { body; update }`,
// which behavioral_unroll recognises and unrolls 8 times.

module sum8 (
    input  logic [7:0] mask,
    output logic [3:0] cnt
);
    always_comb begin
        cnt = 0;
        for (int i = 0; i < 8; i++) cnt = cnt + mask[i];
    end
endmodule
