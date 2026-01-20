(* Generated via HardCaml circuit construction *)
(* Using Signal/Always API directly *)

module pipeline (
    clk,
    data_in,
    data_out
);

    input clk;
    input [31:0] data_in;
    output [31:0] data_out;

    wire [31:0] _15;
    wire vdd;
    wire [31:0] _3;
    reg [31:0] _10;
    wire [31:0] _4;
    reg [31:0] _13;
    wire [31:0] _5;
    reg [31:0] _16;
    assign _15 = 32'b00000000000000000000000000000000;
    assign vdd = 1'b1;
    assign _3 = data_in;
    always @(posedge clk) begin
        _10 <= _3;
    end
    assign _4 = _10;
    always @(posedge clk) begin
        _13 <= _4;
    end
    assign _5 = _13;
    always @(posedge clk) begin
        _16 <= _5;
    end
    assign data_out = _16;

endmodule
