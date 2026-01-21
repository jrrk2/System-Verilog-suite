module test_shl_const(
    input logic [3:0] a  /*  ARNG(3:0) */ ,
    output logic [3:0] c  /*  ARNG(3:0) */ );always@*
        begin
        c = (a << 2);
        end
    endmodule
