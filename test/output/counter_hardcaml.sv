(* Generated via HardCaml circuit construction *)
(* Using Signal/Always API directly *)

module counter (
    clk,
    reset,
    count
);

    input clk;
    input reset;
    output [7:0] count;

    wire vdd;
    wire [7:0] _7;
    wire [7:0] _5;
    wire [7:0] _10;
    wire [7:0] _12;
    wire [7:0] _3;
    reg [7:0] _9;
    assign vdd = 1'b1;
    assign _7 = 8'b00000000;
    assign _5 = 8'b00000001;
    assign _10 = _5 + _9;
    assign _12 = reset ? _7 : _10;
    assign _3 = _12;
    always @(posedge clk) begin
        _9 <= _3;
    end
    assign count = _9;

endmodule
