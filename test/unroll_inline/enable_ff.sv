// Enable-FF test: `if` with no else inside a sequential always block.
// behavioral_iflift rewrites this to
//   q <= en ? d : q;
// — the canonical enable-FF dataflow form.

module enable_ff (
    input  logic       clk,
    input  logic       en,
    input  logic [7:0] d,
    output logic [7:0] q
);
    always_ff @(posedge clk)
        if (en) q <= d;
endmodule
