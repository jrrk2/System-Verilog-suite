// generate-for loop with bit-select LHS — `assign y[i] = expr;`
// unrolls (in Verilator's JSON) into N separate AssignWs, each with
// a Sel{lhs=VarRef y, lsb=Const i} LHS. The previous lhs_base
// reduction in verilator_to_behavioral.ml dropped the bit position,
// so each unrolled iteration overwrote the whole `y` and only the
// last one's logic survived.
//
// Fix: AssignW pass now groups assigns by base name. When the group
// covers all W bits with constant indexes, coalesce into a single
// BConcat. Otherwise fall back to the lossy whole-signal form.
//
// Found via random_sv_gen --features=gen --seed 1.
module bit_select_lhs_genfor #(parameter N = 4) (
  input  logic [N-1:0] a,
  input  logic [N-1:0] b,
  output logic [N-1:0] y
);
  genvar i;
  generate
    for (i = 0; i < N; i = i + 1) begin : g_cell
      assign y[i] = a[i] ^ b[i];
    end
  endgenerate
endmodule
