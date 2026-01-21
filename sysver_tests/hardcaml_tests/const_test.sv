module const_test (
    input clock,
    output reg [7:0] a,
    output reg [3:0] b,
    output reg [7:0] c,
    output reg [8:0] d);

    always @(posedge clock)
      begin
         a <= 8'b00000000;
         b <= 16'hDEAD;
         c <= 8'o177;
         d <= 42;
      end

endmodule
