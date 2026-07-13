module nor6(input a,b,c,d,e,f, output y);
  LUT6 #(.INIT(64'h0000000000000001)) u (.I0(a),.I1(b),.I2(c),.I3(d),.I4(e),.I5(f),.O(y));
endmodule
module and6(input a,b,c,d,e,f, output y);
  LUT6 #(.INIT(64'h8000000000000000)) u (.I0(a),.I1(b),.I2(c),.I3(d),.I4(e),.I5(f),.O(y));
endmodule
