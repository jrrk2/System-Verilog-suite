module bitselect_test (
    input clock,
    input [2:0] b,
    input [7:0] a,
    output reg y);

    always @(posedge clock)
          y <= a[b];

endmodule
