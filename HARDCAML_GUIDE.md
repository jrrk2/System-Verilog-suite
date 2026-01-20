# HardCaml Backend Integration Guide

## Overview

The HardCaml backend generates OCaml code that uses HardCaml's Signal and Always APIs directly, following the patterns established in `Input_hardcaml.ml`. This avoids intermediate compilation phases and produces idiomatic HardCaml code.

## Architecture

### Direct API Usage (No Intermediate Compilation)

Unlike some approaches that generate an intermediate AST and then compile it, this backend:

1. **Generates HardCaml API calls directly** in the output OCaml code
2. **Uses Variable.reg/wire** for state management
3. **Uses compile [...]** to finalize always block assignments
4. **Returns Signal values** via output record

This matches the proven approach from `Input_hardcaml.ml`.

## Key Patterns

### 1. Module Structure

```ocaml
(* Input interface with [@bits n] annotations *)
module I = struct
  type 'a t = {
    clock : 'a; [@bits 1]
    data_in : 'a; [@bits 8]
  } [@@deriving sexp_of, hardcaml]
end

(* Output interface *)
module O = struct
  type 'a t = {
    data_out : 'a; [@bits 8]
  } [@@deriving sexp_of, hardcaml]
end

(* Create function *)
let create scope (i : _ I.t) =
  let open Signal in
  let open Always in
  
  (* ... logic ... *)
  
  O.{ data_out = ... }
```

### 2. Variable Declaration

```ocaml
(* Wires - combinational *)
let temp = Variable.wire ~default:(zero 8) in

(* Registers - sequential *)
let counter = Variable.reg ~enable:vdd
  (Reg_spec.create ~clock:i.clock ~reset:i.reset ())
  ~width:8 in
```

### 3. Always Blocks

```ocaml
(* Compile assignments *)
compile [
  counter <== counter.value +:. 1;  (* Non-blocking for registers *)
  temp <-- i.data_in;                (* Blocking for wires *)
];
```

### 4. Combinational Logic

```ocaml
(* Direct Signal expressions *)
let sum = a +:. b in
let product = a *: b in
let comparison = a <: b in
```

### 5. Case/Switch Statements

```ocaml
compile [
  switch state.value [
    of_int ~width:2 0, [
      next_state <-- of_int ~width:2 1;
      output <-- gnd;
    ];
    of_int ~width:2 1, [
      next_state <-- of_int ~width:2 2;
      output <-- vdd;
    ];
  ];
];
```

### 6. Conditional Logic

```ocaml
compile [
  when_ condition [
    output <-- value1;
  ] @@ [  (* else clause *)
    output <-- value2;
  ];
];
```

### 7. Signed Arithmetic

```ocaml
let open Signed in

(* Convert to signed *)
let a_signed = of_signal a in
let b_signed = of_signal b in

(* Signed operations *)
let sum = a_signed +: b_signed in
let diff = a_signed -: b_signed in

(* Convert back *)
let result = to_signal sum in
```

## Comparison with Other Approaches

### Intermediate Compilation Approach (NOT USED)

```ocaml
(* This approach is NOT used by our backend *)

(* Generate intermediate AST *)
type expr = 
  | Add of expr * expr
  | Var of string
  | ...

(* Then compile to HardCaml *)
let rec compile_expr = function
  | Add (a, b) -> Signal.(+:) (compile_expr a) (compile_expr b)
  | ...
```

**Problems:**
- Extra compilation phase
- More complex code generation
- Harder to debug
- Doesn't match HardCaml best practices

### Direct API Approach (USED)

```ocaml
(* Our backend generates code like this *)

let create scope (i : _ I.t) =
  let open Signal in
  
  (* Direct Signal operations in generated code *)
  let sum = i.a +:. i.b in
  let product = sum *: i.c in
  
  O.{ result = product }
```

**Benefits:**
- Matches `Input_hardcaml.ml` proven patterns
- No intermediate compilation
- Idiomatic HardCaml code
- Easy to debug and modify
- Efficient

## Generated Code Quality

### Input SystemVerilog

```verilog
module counter(
  input wire clock,
  input wire reset,
  input wire enable,
  output reg [7:0] count
);

always @(posedge clock or posedge reset) begin
  if (reset)
    count <= 8'd0;
  else if (enable)
    count <= count + 1;
end

endmodule
```

### Generated HardCaml

```ocaml
open Hardcaml
open Signal
open Always

module I = struct
  type 'a t = {
    clock : 'a; [@bits 1]
    reset : 'a; [@bits 1]
    enable : 'a; [@bits 1]
  } [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = {
    count : 'a; [@bits 8]
  } [@@deriving sexp_of, hardcaml]
end

let create scope (i : _ I.t) =
  let open Signal in
  let open Always in

  (* Internal signals and variables *)
  let count = Variable.reg ~enable:i.enable
    (Reg_spec.create ~clock:i.clock ~reset:i.reset ())
    ~width:8 in

  (* Combinational logic *)
  let next_count = count.value +:. 1 in

  (* Sequential always block *)
  compile [
    count <== mux2 i.enable next_count count.value;
  ];

  (* Build output record *)
  O.{
    count = count.value
  }

let circuit ~name =
  let module Circuit = Circuit.With_interface(I)(O) in
  Circuit.create_exn ~name (create (Scope.create ()))
```

## Usage Examples

### 1. Build Circuit

```ocaml
let counter_circuit = circuit ~name:"counter"
```

### 2. Simulation

```ocaml
let sim = Cyclesim.create counter_circuit in
let inputs = Cyclesim.inputs sim in
let outputs = Cyclesim.outputs sim in

(* Initialize *)
inputs.clock := Bits.vdd;
inputs.reset := Bits.vdd;
inputs.enable := Bits.gnd;
Cyclesim.cycle sim;

(* Run *)
inputs.reset := Bits.gnd;
inputs.enable := Bits.vdd;

for i = 0 to 255 do
  Cyclesim.cycle sim;
  Printf.printf "Count: %d\n" (Bits.to_int !(outputs.count))
done
```

### 3. Synthesis

```ocaml
(* Generate Verilog for synthesis *)
let rtl = Rtl.output Verilog counter_circuit in
print_endline rtl
```

### 4. Testing

```ocaml
open Expect_test_helpers_core

let%expect_test "counter" =
  let sim = Cyclesim.create counter_circuit in
  (* ... test logic ... *)
  [%expect {| expected output |}]
```

## Integration with Existing Codebase

### 1. Add to dune File

```lisp
(library
 (name my_hardware)
 (libraries hardcaml)
 (preprocess (pps ppx_jane ppx_hardcaml)))
```

### 2. Import Generated Module

```ocaml
(* In your project *)
open My_hardware

(* Use generated module *)
let my_circuit = Counter.circuit ~name:"my_counter"
```

### 3. Compose with Other Modules

```ocaml
module Top = struct
  module I = struct
    type 'a t = {
      clock : 'a; [@bits 1]
      start : 'a; [@bits 1]
    } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = {
      done_ : 'a; [@bits 1]
    } [@@deriving sexp_of, hardcaml]
  end

  let create scope (i : _ I.t) =
    let open Signal in
    
    (* Instantiate generated counter *)
    let counter_out = Counter.create scope {
      Counter.I.clock = i.clock;
      reset = gnd;
      enable = i.start;
    } in
    
    O.{
      done_ = counter_out.Counter.O.count ==:. 255
    }
end
```

## Advanced Features

### 1. Parameterized Modules

```ocaml
(* Add parameters to create function *)
let create ~width scope (i : _ I.t) =
  let open Signal in
  let counter = Variable.reg 
    (Reg_spec.create ~clock:i.clock ())
    ~width in
  (* ... *)
```

### 2. Multiple Clock Domains

```ocaml
let create scope (i : _ I.t) =
  let spec1 = Reg_spec.create ~clock:i.clock1 () in
  let spec2 = Reg_spec.create ~clock:i.clock2 () in
  
  let reg1 = Variable.reg spec1 ~width:8 in
  let reg2 = Variable.reg spec2 ~width:8 in
  (* ... *)
```

### 3. Memory Inference

```ocaml
let create scope (i : _ I.t) =
  let open Signal in
  
  let mem = multiport_memory
    ~write_ports:[|
      { write_clock = i.clock;
        write_enable = i.we;
        write_address = i.waddr;
        write_data = i.wdata }
    |]
    ~read_addresses:[| i.raddr |]
    8 (* address bits *)
    16 (* data width *)
  in
  (* ... *)
```

## Debugging Generated Code

### 1. Add Tracing

```ocaml
let create scope (i : _ I.t) =
  let open Signal in
  
  let counter = Variable.reg 
    (Reg_spec.create ~clock:i.clock ())
    ~width:8 in
  
  (* Add trace points *)
  let counter_traced = Signal.( -- ) "counter" counter.value in
  
  compile [ counter <== counter.value +:. 1 ];
  
  O.{ count = counter_traced }
```

### 2. Waveform Output

```ocaml
let sim = Cyclesim.create 
  ~config:Cyclesim.Config.trace_all
  counter_circuit in

Waveform.expect 
  ~serialize_to:"waves.vcd"
  ~display_width:80
  ~display_height:25
  ~wave_width:1
  sim
```

## Performance Considerations

### 1. Fast Arithmetic

The backend uses optimized arithmetic from `hardcaml-circuits`:

```ocaml
(* Fast adder with configurable architecture *)
let sum = add_fast "Kogge-Stone" a b in

(* Fast multiplier *)
let product = mult_fast "Wallace" a b in

(* Fast comparator *)
let less = less_fast a b in
```

### 2. Register Inference

Variables automatically infer to registers or wires:

```ocaml
(* This becomes a register *)
let reg_var = Variable.reg spec ~width:8 in

(* This becomes a wire *)
let wire_var = Variable.wire ~default:(zero 8) in
```

## Troubleshooting

### Common Issues

1. **Missing [@bits n] annotation**
   ```ocaml
   (* Wrong *)
   type 'a t = { x : 'a }
   
   (* Right *)
   type 'a t = { x : 'a; [@bits 8] }
   ```

2. **Mixing blocking/non-blocking incorrectly**
   ```ocaml
   (* Registers use <== *)
   reg_var <== value;
   
   (* Wires use <-- *)
   wire_var <-- value;
   ```

3. **Width mismatches**
   ```ocaml
   (* Use uresize/sresize *)
   let padded = uresize narrow_signal 16 in
   let truncated = select wide_signal 0 8 in
   ```

## References

- `Input_hardcaml.ml` - Reference implementation
- `Input_types.ml` - Type definitions
- HardCaml documentation: https://github.com/janestreet/hardcaml
- HardCaml circuits: https://github.com/janestreet/hardcaml_circuits

## Summary

The HardCaml backend:

1. **Generates idiomatic HardCaml code** following proven patterns
2. **Uses direct Signal/Always API** - no intermediate compilation
3. **Produces efficient hardware** via optimized arithmetic
4. **Integrates seamlessly** with existing HardCaml projects
5. **Supports full feature set** - registers, wires, always blocks, case statements
6. **Enables simulation and synthesis** through standard HardCaml flows

This approach ensures generated code is maintainable, efficient, and follows HardCaml best practices.
