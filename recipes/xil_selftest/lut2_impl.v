module top(input I0, input I1, output O);
  LUT2 #(.INIT(4'h6)) u (.I0(I0), .I1(I1), .O(O));
endmodule
