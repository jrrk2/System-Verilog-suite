module uncon(
    output logic  q  /*  BIT */ ,
    input logic  data  /*  BIT */ ,
    input logic  clk  /*  BIT */ );

DFF_X1 DFF_X1__8(
    .CK(clk),
    .D(data),
    .QN(),
    .Q(q));
   
endmodule
