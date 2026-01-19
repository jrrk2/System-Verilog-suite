// loop_test.sv - Test cases for loop unrolling

// Test 1: Simple population count (your example)
module loop(
  input  logic [7:0] mask,
  output logic [3:0] cnt
);
  always @(mask) begin
    cnt = 0;
    for (int i = 0; i < 8; i = i + 1) begin
      cnt = cnt + mask[i];
    end
  end
endmodule

// After transformation should become:
// always @(mask) begin
//   cnt = 0;
//   cnt = cnt + mask[0];
//   cnt = cnt + mask[1];
//   cnt = cnt + mask[2];
//   ...
//   cnt = cnt + mask[7];
// end

// Test 2: Constant-bound loop with step
module loop_step(
  input  logic [15:0] data,
  output logic [7:0] result
);
  always_comb begin
    result = 0;
    for (int i = 0; i < 16; i = i + 2) begin
      result = result + data[i];
    end
  end
endmodule

// Test 3: Nested loops (should unroll both)
module loop_nested(
  input  logic [7:0] matrix [0:3],
  output logic [7:0] sum
);
  always_comb begin
    sum = 0;
    for (int i = 0; i < 4; i = i + 1) begin
      for (int j = 0; j < 8; j = j + 1) begin
        sum = sum + matrix[i][j];
      end
    end
  end
endmodule

// Test 4: Loop with conditional inside
module loop_conditional(
  input  logic [7:0] data,
  output logic [3:0] cnt
);
  always_comb begin
    cnt = 0;
    for (int i = 0; i < 8; i = i + 1) begin
      if (data[i])
        cnt = cnt + 1;
    end
  end
endmodule

// Test 5: Down-counting loop
module loop_down(
  input  logic [7:0] data,
  output logic [7:0] reversed
);
  always_comb begin
    for (int i = 7; i >= 0; i = i - 1) begin
      reversed[7-i] = data[i];
    end
  end
endmodule

// Test 6: Non-constant bounds (CANNOT unroll)
module loop_variable(
  input  logic [3:0] n,
  input  logic [7:0] data,
  output logic [7:0] result
);
  always_comb begin
    result = 0;
    for (int i = 0; i < n; i = i + 1) begin // Variable bound!
      result = result + data[i];
    end
  end
endmodule

// Test 7: Array initialization
module loop_array_init(
  output logic [7:0] lookup [0:15]
);
  initial begin
    for (int i = 0; i < 16; i = i + 1) begin
      lookup[i] = i * 2;
    end
  end
endmodule