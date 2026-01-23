# Memory Mapping Implementation Status

## Goal
Identify large memory arrays and map them to behavioral memory modules instead of individual flip-flops. This is essential for:
- Register files (32+ registers)
- Cache memories
- FIFOs with significant depth
- Any array where flip-flop implementation would be inefficient

## Completed ✅

### 1. Memory Detection Infrastructure (sv_memory.ml)
- **Module created**: `sv_memory.ml` with memory detection logic
- **Threshold**: 128 bits (16 bytes) - arrays larger than this use memory primitives
- **Detection algorithm**:
  - Parses ArrayType' and ArrayType nodes from AST
  - Extracts array depth from range strings (e.g., "31:0" → 32 entries)
  - Calculates element width from base type
  - Computes total size: depth × width
  - Identifies arrays above threshold with depth > 1

### 2. Type System Integration
- Handles both unresolved (ArrayType') and resolved (ArrayType) array types
- Parses packed arrays (PackArrayType) for element width
- Supports BasicType with range specifications
- Calculates address width: ceil(log2(depth))

### 3. Memory Metadata Structure
```ocaml
type memory_info = {
  mutable name: string;
  mutable addr_width: int;    (* log2(depth) *)
  mutable data_width: int;    (* element width in bits *)
  mutable depth: int;         (* number of entries *)
  mutable size_bits: int;     (* total size *)
  mutable read_accesses: mem_access list;
  mutable write_accesses: mem_access list;
  mutable is_sequential: bool;
}
```

### 4. Test Cases
- **test_regfile.sv**: 32×8-bit register file (256 bits)
  - 2 read ports, 1 write port
  - Sequential write, combinational read
  - ✅ Detected as memory

- **test_small_array.sv**: 4×8-bit array (32 bits)
  - Below threshold
  - Should use flip-flops

### 5. Integration Points
- Added to dune build configuration
- Integrated into sv_gen_hardcaml.ml (line 549-551)
- Detection runs during circuit building
- Prints statistics: "Found N memories"

## In Progress 🚧

### Access Pattern Detection
Need to scan statements to find:
- **Read operations**: `ArraySel` expressions in RHS
- **Write operations**: `ArraySel` in LHS of assignments
- **Conditions**: Enable signals, if statements around writes
- **Sequential vs combinational**: Based on always block type

### Memory Classification
Determine memory type based on access patterns:
- **Single-port RAM**: 1 read + 1 write
- **Dual-port RAM**: 2 reads + 1 write (register file)
- **True dual-port**: 2 reads + 2 writes
- **ROM**: Only reads
- **FIFO**: Specific read/write pointer patterns

## Remaining Work 📋

### 1. Memory Primitives (structural_primitives.sv)
Add hardware memory modules:

```verilog
// Single-port RAM
module memory_sp #(
  parameter ADDR_WIDTH = 5,
  parameter DATA_WIDTH = 8
) (
  input  logic clk,
  input  logic we,
  input  logic [ADDR_WIDTH-1:0] addr,
  input  logic [DATA_WIDTH-1:0] wdata,
  output logic [DATA_WIDTH-1:0] rdata
);
  logic [DATA_WIDTH-1:0] mem [(1<<ADDR_WIDTH)-1:0];

  always_ff @(posedge clk) begin
    if (we)
      mem[addr] <= wdata;
  end

  assign rdata = mem[addr];
endmodule

// Dual-port RAM (1W2R - typical register file)
module memory_1w2r #(
  parameter ADDR_WIDTH = 5,
  parameter DATA_WIDTH = 64
) (
  input  logic clk,
  input  logic we,
  input  logic [ADDR_WIDTH-1:0] waddr,
  input  logic [DATA_WIDTH-1:0] wdata,
  input  logic [ADDR_WIDTH-1:0] raddr1,
  input  logic [ADDR_WIDTH-1:0] raddr2,
  output logic [DATA_WIDTH-1:0] rdata1,
  output logic [DATA_WIDTH-1:0] rdata2
);
  logic [DATA_WIDTH-1:0] mem [(1<<ADDR_WIDTH)-1:0];

  always_ff @(posedge clk) begin
    if (we)
      mem[waddr] <= wdata;
  end

  assign rdata1 = mem[raddr1];
  assign rdata2 = mem[raddr2];
endmodule
```

### 2. Structural Backend Integration (sv_tran_struct.ml)
Modify ArraySel handling:
- Check if array is in detected memories table
- Instead of generating individual flip-flops, emit memory instance
- Group read accesses by port
- Group write accesses by port
- Generate memory primitive with correct parameters

Example transformation:
```verilog
// Before (flip-flop based):
logic [7:0] mem [31:0];
assign rdata1 = mem[raddr1];
assign rdata2 = mem[raddr2];
always_ff @(posedge clk)
  if (we) mem[waddr] <= wdata;

// After (memory primitive):
memory_1w2r #(.ADDR_WIDTH(5), .DATA_WIDTH(8)) mem_inst (
  .clk(clk),
  .we(we),
  .waddr(waddr),
  .wdata(wdata),
  .raddr1(raddr1),
  .raddr2(raddr2),
  .rdata1(rdata1),
  .rdata2(rdata2)
);
```

### 3. HardCaml Backend Integration (sv_gen_hardcaml.ml)
Use HardCaml RAM primitives:
```ocaml
open Hardcaml

(* Single-port RAM *)
let sp_ram ~clock ~we ~addr ~wdata =
  let spec = Reg_spec.create ~clock () in
  let ram = Ram.create ~collision_mode:Read_before_write
    ~size:(1 lsl (width addr))
    ~write_port:{ write_clock = clock;
                  write_address = addr;
                  write_data = wdata;
                  write_enable = we } in
  Ram.read_port ram ~read_clock:clock ~read_address:addr

(* Multi-port RAM *)
let multiport_ram ~clock ~we ~waddr ~wdata ~raddrs =
  let size = 1 lsl (width waddr) in
  let write_port = { Ram.Write_port.
    write_clock = clock;
    write_address = waddr;
    write_data = wdata;
    write_enable = we;
  } in
  let read_ports = List.map (fun raddr ->
    { Ram.Read_port.
      read_clock = clock;
      read_address = raddr;
      read_enable = vdd; (* Always enabled *)
    }
  ) raddrs in
  Ram.create_multi_port ~collision_mode:Read_before_write
    ~size ~write_ports:[write_port] ~read_ports
```

### 4. Access Pattern Analysis
Complete the access tracking in sv_memory.ml:
- Implement `extract_memory_accesses` recursively
- Handle If/Case conditional writes
- Extract enable conditions from If predicates
- Detect write-enable patterns
- Group accesses by port (multiple reads/writes)

### 5. Memory Port Allocation
Algorithm to assign accesses to ports:
- Count unique read address expressions → read ports needed
- Count unique write address expressions → write ports needed
- Allocate port numbers to each access
- Generate port connections in instance

### 6. Testing & Validation
Test on various memory patterns:
- Simple register file (test_regfile.sv) ✅ Created
- FIFO with read/write pointers
- Cache memory with tag/data arrays
- Ariane register file (32×64-bit = 2048 bits)
- Multi-ported memories

### 7. Verification
Ensure memory mapping preserves semantics:
- Z3 verification with memory models
- Compare behavioral vs structural memory
- Test read-during-write behavior
- Verify initialization values

## Benefits

### Before (Flip-Flop Based)
- **32×64-bit register file**: 2048 flip-flops
- **Routing**: Complex mux trees for read ports
- **Area**: ~2000 gates
- **Generated code**: 100+ lines of assignments

### After (Memory Primitive)
- **Memory instance**: 1 module instantiation
- **Routing**: Address/data buses
- **Area**: Single RAM block
- **Generated code**: 5-10 lines

## Example: Ariane Register File
From Ariane RISC-V processor:
```
Module: ariane_regfile
Size: 32 entries × 64 bits = 2048 bits
Ports: 2 read, 2 write
Current: Would generate 2048 individual flip-flops
Target: Single memory_2w2r primitive
Savings: ~95% code reduction
```

## Implementation Priority

1. ✅ **Detection infrastructure** - Complete
2. 🚧 **Access pattern analysis** - In progress
3. ⏳ **Memory primitives** - Next
4. ⏳ **Structural backend** - After primitives
5. ⏳ **HardCaml backend** - Parallel with structural
6. ⏳ **Testing** - Continuous
7. ⏳ **Verification** - Final validation

## Current Status: ~25% Complete

- Detection: ✅ Done
- Analysis: ⏳ 0%
- Primitives: ⏳ 0%
- Structural: ⏳ 0%
- HardCaml: ⏳ 0%
- Testing: ✅ Partial
- Verification: ⏳ 0%
