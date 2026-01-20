(* Generated via HardCaml circuit construction *)
(* Using Signal/Always API directly *)

module combinational (
    a,
    b,
    sel,
    result
);

    input [7:0] a;
    input [7:0] b;
    input sel;
    output [7:0] result;

    wire [7:0] _5;
    assign _5 = sel ? a : b;
    assign result = _5;

endmodule
