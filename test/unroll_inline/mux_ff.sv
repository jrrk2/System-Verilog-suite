// 2-way ternary FF: if/else where both arms assign the same lhs.
// behavioral_iflift rewrites to  q <= sel ? a : b;

module mux_ff (
    input  logic       clk,
    input  logic       sel,
    input  logic [7:0] a,
    input  logic [7:0] b,
    output logic [7:0] q
);
    always_ff @(posedge clk)
        if (sel) q <= a;
        else    q <= b;
endmodule
