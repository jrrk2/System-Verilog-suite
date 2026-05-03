// Task-inlining test: a task that writes its inverted input to an
// output. Inlining replaces the call with the body, rewriting the
// formal `vbar` to the actual lvalue at the call site.

package inv_pkg;
    task automatic invert(input logic [7:0] v, output logic [7:0] vbar);
        vbar = ~v;
    endtask
endpackage

module task_invert (
    input  logic [7:0] din,
    output logic [7:0] dout
);
    import inv_pkg::*;
    always_comb invert(din, dout);
endmodule
