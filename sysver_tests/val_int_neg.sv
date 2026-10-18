// The VM's tagged-integer helpers: Val_int wraps a 32-bit result in a wider
// word, Int_val unwraps it.  `-Int_val(accu)` and `Int_val(a) + Int_val(b)`
// are 32-bit (integer) results whatever the word width: a 1-bit typing of
// the negation let `<< 1` spill the sign into bit 32.
module val_int_neg (
    input  logic        clk,
    input  logic [1:0]  op,
    input  logic [35:0] accu_in, tos,
    output logic [35:0] accu
);
  localparam int VALUEW = 36;
  function automatic logic [VALUEW-1:0] Val_int(input integer n);
    Val_int = {{(VALUEW-32){1'b0}}, ((n <<< 1) | 1)};
  endfunction
  function automatic integer Int_val(input logic [VALUEW-1:0] v);
    Int_val = $signed(v[31:0]) >>> 1;
  endfunction
  always_ff @(posedge clk) begin
    case (op)
      2'd0: accu <= accu_in;
      2'd1: accu <= Val_int(-Int_val(accu));
      2'd2: accu <= Val_int(Int_val(accu) + Int_val(tos));
      2'd3: accu <= Val_int(Int_val(accu) - Int_val(tos));
    endcase
  end
endmodule
