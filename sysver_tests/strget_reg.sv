// GETSTRINGCHAR as the VM does it: the byte index is a 2-bit register set
// from a stack word in one state and used by the indexed part-select in the
// next.
module strget_reg (
    input  logic        clk, go,
    input  logic [35:0] word, st_rd_a,
    output logic [35:0] accu
);
  logic [1:0] str_byte;
  logic       phase;
  function automatic logic [35:0] Val_int(input integer n);
    Val_int = {4'd0, ((n <<< 1) | 1)};
  endfunction
  always_ff @(posedge clk) begin
    if (go) begin
      if (!phase) begin
        str_byte <= st_rd_a[2:1];
        phase <= 1'b1;
      end else begin
        accu  <= Val_int(word[8*str_byte+:8]);
        phase <= 1'b0;
      end
    end
  end
endmodule
