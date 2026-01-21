module test_shl(
    input logic [3:0] a,
    input logic [0:0] b,
    output logic [3:0] c);

    always@*
        begin
        c = (a << b);
        end
    endmodule
