// Function-inlining test: a pure function that returns a + b.

package add_pkg;
    function automatic logic [7:0] add(input logic [7:0] a, b);
        return a + b;
    endfunction
endpackage

module func_add (
    input  logic [7:0] x,
    input  logic [7:0] y,
    output logic [7:0] z
);
    import add_pkg::*;
    always_comb z = add(x, y);
endmodule
