// Three-level nested if/else if/else. After bottom-up if-lift the
// chain collapses to a nested ternary:
//   q <= a ? d1 : (b ? d2 : (c ? d3 : d4));

module nested_if (
    input  logic       clk,
    input  logic       a, b, c,
    input  logic [7:0] d1, d2, d3, d4,
    output logic [7:0] q
);
    always_ff @(posedge clk)
        if (a)        q <= d1;
        else if (b)   q <= d2;
        else if (c)   q <= d3;
        else          q <= d4;
endmodule
