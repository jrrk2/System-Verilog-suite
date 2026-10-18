// `unique case (imm)` with 16-bit keys on a 32-bit selector, arms that call
// tasks assigning a 36-bit register from a 32-bit constant, and a default.
// The VM's C_CALL1 0x103 arm (caml_ml_open_descriptor_in: accu <= 32'hC0010000).
module case_task_const (
    input  logic        clk, go,
    input  logic [31:0] imm,
    input  logic [35:0] accu_in,
    output logic [35:0] accu
);
  task caml_ml_open_descriptor_in;
    begin
      accu <= 32'hC0010000;
    end
  endtask
  task flush;
    begin
      accu <= accu_in;
    end
  endtask
  always_ff @(posedge clk) begin
    if (go) begin
      unique case (imm)
        16'h0fd: flush();
        16'h103: caml_ml_open_descriptor_in();
        16'h05a: accu <= 36'd1;
        default: accu <= 36'd1;
      endcase
    end
  end
endmodule
