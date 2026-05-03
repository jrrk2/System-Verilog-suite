// Mixed nested form: outer if has both legs but the inner one is
// an enable (no else). After bottom-up lift:
//   inner:  q <= flush ? 8'h00 : q;
//   outer:  q <= load ? d : (flush ? 8'h00 : q);

module nested_enable (
    input  logic       clk,
    input  logic       load,
    input  logic       flush,
    input  logic [7:0] d,
    output logic [7:0] q
);
    always_ff @(posedge clk)
        if (load)
            q <= d;
        else if (flush)
            q <= 8'h00;
endmodule
