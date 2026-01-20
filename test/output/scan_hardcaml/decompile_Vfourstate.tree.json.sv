(* Generated via HardCaml circuit construction *)
(* Using Signal/Always API directly *)

module fourstate (
    clk,
    reset,
    data
);

    input clk;
    input reset;
    output [31:0] data;

    wire vdd;
    wire [31:0] _7;
    wire [31:0] _5;
    wire [31:0] _10;
    wire [31:0] _12;
    wire [31:0] _3;
    reg [31:0] _9;
    assign vdd = 1'b1;
    assign _7 = 32'b00000000000000000000000000000000;
    assign _5 = 32'b00000000000000000000000000000001;
    assign _10 = _5 + _9;
    assign _12 = reset ? _7 : _10;
    assign _3 = _12;
    always @(posedge clk) begin
        _9 <= _3;
    end
    assign data = _9;

endmodule
