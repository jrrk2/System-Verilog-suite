// STRGET: a byte of a 36-bit word picked by a variable indexed part-select,
// wrapped as a tagged integer.  `hm_rd_a[8*str_byte +: 8]` in the VM.
module strget_byte (
    input  logic        clk, go,
    input  logic [35:0] word,
    input  logic [1:0]  str_byte,
    output logic [35:0] accu
);
  function automatic logic [35:0] Val_int(input integer n);
    Val_int = {4'd0, ((n <<< 1) | 1)};
  endfunction
  always_ff @(posedge clk)
    if (go) accu <= Val_int(word[8*str_byte +: 8]);
endmodule
