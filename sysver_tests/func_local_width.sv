// A function local declared with a width and initialised from an int argument:
// the initialiser is cut to the declared width before the inliner substitutes it
// (the VM's Make_header lost its 16-bit size field when it was not).
module func_local_width(input logic clk, input logic [3:0] n, output logic [35:0] y, z);
  localparam int W = 5;
  localparam int T = 252;
  function automatic logic [35:0] mk(input int wosize, input int tag);
    logic [7:0] tag8 = tag;
    logic [15:0] wosize16 = wosize;
    mk = {wosize16, 8'd0, tag8};
  endfunction
  always_ff @(posedge clk) begin
    y <= mk(W, T);
    z <= mk(n + 1, T);
  end
endmodule
