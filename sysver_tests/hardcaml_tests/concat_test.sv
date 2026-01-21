module creat (
    input [3:0]		a,
    input [3:0]		b,
    input [3:0]		c,
    input [11:0]	d,
    output logic [11:0]	e_wire);

    always_comb
      e_wire =  { a, b, c } + d;

endmodule
