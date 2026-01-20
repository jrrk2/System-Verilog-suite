// Test module for OCaml VM ADDINT operation
// Verifies: accu + tos = result (with OCaml tagged integer encoding)
//
// OCaml integer encoding:
//   Val_int(n) = (n << 1) | 1
//   Int_val(v) = v >>> 1 (arithmetic shift right)
//
// ADDINT operation:
//   accu_next = Val_int(Int_val(accu) + Int_val(tos))

module test_ocaml_addint #(
  parameter VALUEW = 32
)(
  input  logic clk,
  input  logic [VALUEW-1:0] accu_in,
  input  logic [VALUEW-1:0] tos_in,
  output logic [VALUEW-1:0] accu_out
);

  // OCaml value encoding functions
  function automatic logic [VALUEW-1:0] Val_int(input integer n);
    Val_int = ((n <<< 1) | 1);
  endfunction

  function automatic integer Int_val(input logic [VALUEW-1:0] v);
    Int_val = $signed(v) >>> 1;
  endfunction

  // ADDINT operation
  always_comb begin
    accu_out = Val_int(Int_val(accu_in) + Int_val(tos_in));
  end

endmodule
