// ============================================================================
// blocking_test.sv - Test cases for blocking to non-blocking conversion
// ============================================================================

// Test 1: Simple blocking chain (after loop unroll)
module test_blocking_chain(
  input  logic clk,
  input  logic [7:0] mask,
  output logic [3:0] cnt
);
  always_ff @(posedge clk) begin
    cnt = 0;
    cnt = cnt + mask[0];  // Reads previous value
    cnt = cnt + mask[1];  // Reads previous value
    cnt = cnt + mask[2];
    // ... more after unrolling
  end
endmodule

// Expected after conversion:
// always_ff @(posedge clk) begin
//   temp_1 = 0;
//   temp_2 = temp_1 + mask[0];
//   temp_3 = temp_2 + mask[1];
//   temp_4 = temp_3 + mask[2];
//   cnt <= temp_4;  // Non-blocking at end
// end

// Test 2: Blocking in combinational (should become wires)
module test_comb_blocking(
  input  logic [7:0] a,
  input  logic [7:0] b,
  output logic [7:0] result
);
  always_comb begin
    logic [7:0] temp;
    temp = a + b;
    result = temp * 2;
  end
endmodule

// Expected:
// wire [7:0] temp = a + b;
// assign result = temp * 2;

// Test 3: Multiple assignments to same variable (SSA needed)
module test_multiple_assign(
  input  logic clk,
  input  logic [7:0] x,
  output logic [7:0] y
);
  always_ff @(posedge clk) begin
    y = x;
    y = y + 1;
    y = y * 2;
  end
endmodule

// Expected:
// always_ff @(posedge clk) begin
//   temp_1 = x;
//   temp_2 = temp_1 + 1;
//   temp_3 = temp_2 * 2;
//   y <= temp_3;
// end

// Test 4: Accumulator pattern
module test_accumulator(
  input  logic clk,
  input  logic [7:0] data [0:7],
  output logic [7:0] sum
);
  always_ff @(posedge clk) begin
    sum = 0;
    for (int i = 0; i < 8; i = i + 1) begin
      sum = sum + data[i];
    end
  end
endmodule

// After unroll + blocking conversion:
// always_ff @(posedge clk) begin
//   temp_1 = 0;
//   temp_2 = temp_1 + data[0];
//   temp_3 = temp_2 + data[1];
//   ...
//   temp_9 = temp_8 + data[7];
//   sum <= temp_9;
// end

// Test 5: Conditional blocking assignments
module test_cond_blocking(
  input  logic clk,
  input  logic en,
  input  logic [7:0] data,
  output logic [7:0] result
);
  always_ff @(posedge clk) begin
    result = result;  // Keep old value
    if (en) begin
      result = data;
      result = result + 1;
    end
  end
endmodule

// Expected:
// always_ff @(posedge clk) begin
//   temp_1 = result;
//   if (en) begin
//     temp_2 = data;
//     temp_3 = temp_2 + 1;
//     result <= temp_3;
//   end else begin
//     result <= temp_1;
//   end
// end

// Test 6: Mixed blocking/non-blocking (already correct)
module test_mixed(
  input  logic clk,
  input  logic [7:0] a,
  input  logic [7:0] b,
  output logic [7:0] out1,
  output logic [7:0] out2
);
  always_ff @(posedge clk) begin
    out1 <= a;  // Already non-blocking, keep
    out2 = b;   // Blocking, convert
  end
endmodule

// Expected:
// always_ff @(posedge clk) begin
//   out1 <= a;
//   out2 <= b;  // Converted to non-blocking
// end

// Test 7: Intermediate calculation in combinational
module test_comb_intermediate(
  input  logic [7:0] a,
  input  logic [7:0] b,
  input  logic [7:0] c,
  output logic [7:0] result
);
  always_comb begin
    logic [7:0] sum;
    logic [7:0] prod;
    sum = a + b;
    prod = sum * c;
    result = prod;
  end
endmodule

// Expected:
// assign sum_wire = a + b;
// assign prod_wire = sum_wire * c;
// assign result = prod_wire;

// Test 8: Read-after-write dependency
module test_raw_dependency(
  input  logic clk,
  input  logic [7:0] x,
  output logic [7:0] y,
  output logic [7:0] z
);
  always_ff @(posedge clk) begin
    y = x + 1;
    z = y + 2;  // Reads y assigned above!
  end
endmodule

// Expected:
// always_ff @(posedge clk) begin
//   temp_1 = x + 1;
//   temp_2 = temp_1 + 2;
//   y <= temp_1;
//   z <= temp_2;
// end

// Test 9: No dependency (parallel assignments)
module test_no_dependency(
  input  logic clk,
  input  logic [7:0] a,
  input  logic [7:0] b,
  output logic [7:0] x,
  output logic [7:0] y
);
  always_ff @(posedge clk) begin
    x = a;
    y = b;  // No dependency on x
  end
endmodule

// Expected:
// always_ff @(posedge clk) begin
//   x <= a;  // Can be non-blocking directly
//   y <= b;
// end

// Test 10: Loop with accumulation (complex case)
module test_loop_accumulation(
  input  logic clk,
  input  logic [7:0] data [0:3],
  output logic [7:0] min_val,
  output logic [7:0] max_val
);
  always_ff @(posedge clk) begin
    min_val = data[0];
    max_val = data[0];
    for (int i = 1; i < 4; i = i + 1) begin
      if (data[i] < min_val)
        min_val = data[i];
      if (data[i] > max_val)
        max_val = data[i];
    end
  end
endmodule

// After unroll + blocking conversion:
// always_ff @(posedge clk) begin
//   min_val_v1 = data[0];
//   max_val_v1 = data[0];
//   
//   // Iteration 1
//   min_val_v2 = (data[1] < min_val_v1) ? data[1] : min_val_v1;
//   max_val_v2 = (data[1] > max_val_v1) ? data[1] : max_val_v1;
//   
//   // Iteration 2
//   min_val_v3 = (data[2] < min_val_v2) ? data[2] : min_val_v2;
//   max_val_v3 = (data[2] > max_val_v2) ? data[2] : max_val_v2;
//   
//   // Iteration 3
//   min_val_v4 = (data[3] < min_val_v3) ? data[3] : min_val_v3;
//   max_val_v4 = (data[3] > max_val_v3) ? data[3] : max_val_v3;
//   
//   min_val <= min_val_v4;
//   max_val <= max_val_v4;
// end