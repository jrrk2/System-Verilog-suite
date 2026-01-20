(* Generated via HardCaml circuit construction *)
(* Using Signal/Always API directly *)

module small_memory (
    clk,
    rdata
);

    input clk;
    output [7:0] rdata;

    wire vdd;
    wire [7:0] _6;
    wire [7:0] _2;
    reg [7:0] _8;
    assign vdd = 1'b1;
    assign _6 = 8'b00000000;
    assign _2 = _6;
    always @(posedge clk) begin
        _8 <= _2;
    end
    assign rdata = _8;

endmodule
