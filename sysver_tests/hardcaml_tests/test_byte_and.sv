module test_byte_and(
    input logic [7:0] a  /*  ARNG(3:0) */ ,
    input logic [7:0] b  /*  ARNG(3:0) */ ,
    output logic [7:0] c  /*  ARNG(3:0) */ );always@*
        begin
        c = (a & b);
        end
    endmodule
