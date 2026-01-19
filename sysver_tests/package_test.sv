package test_pkg;
    // Package with types, constants, and functions
    
    // Enumerated types
    typedef enum logic [1:0] {
        IDLE   = 2'b00,
        ACTIVE = 2'b01,
        WAIT   = 2'b10,
        DONE   = 2'b11
    } state_t;
    
    // Packed structures
    typedef struct packed {
        logic        valid;
        logic [7:0]  data;
        logic [1:0]  mode;
        logic        error;
    } packet_t;
    
    // Unpacked structure
    typedef struct {
        int unsigned count;
        real         weight;
        string       name;
    } config_t;
    
    // Parameters
    localparam int WIDTH = 8;
    localparam logic [WIDTH-1:0] MASK = {WIDTH{1'b1}};
    
    // Function
    function automatic logic [WIDTH-1:0] reverse_bits(logic [WIDTH-1:0] data);
        for (int i = 0; i < WIDTH; i++) begin
            reverse_bits[i] = data[WIDTH-1-i];
        end
    endfunction
    
endpackage

module package_test import test_pkg::*; #(
    parameter int DATA_WIDTH = 16,
    parameter int ADDR_WIDTH = 8,
    parameter type data_t = logic [DATA_WIDTH-1:0],
    parameter data_t RESET_VALUE = '0
) (input mask[DONE:0], output logic cnt[3:0]);

  always @(mask)
    begin
       cnt = 0;
       for (int i = 0; i <= DONE; i++) cnt = cnt + mask[i];
    end

endmodule
