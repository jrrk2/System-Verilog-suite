module test_bit_xor(
    input logic [3:0] a  /*  ARNG(3:0) */ ,
    input logic [3:0] b  /*  ARNG(3:0) */ ,
    output logic [3:0] c  /*  ARNG(3:0) */ );always@*
        begin
        c = a;c = (c ^ b);
        end
    endmodule
