(* Generated via HardCaml circuit construction *)
(* Using Signal/Always API directly *)

module mixed (
    clk,
    b,
    a,
    sum,
    sum_reg
);

    input clk;
    input [7:0] b;
    input [7:0] a;
    output [7:0] sum;
    output [7:0] sum_reg;

    wire vdd;
    wire [7:0] _9;
    wire [7:0] _7;
    wire [7:0] _2;
    reg [7:0] _11;
    wire [7:0] _12;
    assign vdd = 1'b1;
    assign _9 = 8'b00000000;
    assign _7 = a + b;
    assign _2 = _7;
    always @(posedge clk) begin
        _11 <= _2;
    end
    assign _12 = a + b;
    assign sum = _12;
    assign sum_reg = _11;

endmodule
