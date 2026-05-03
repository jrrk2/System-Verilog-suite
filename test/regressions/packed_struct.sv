// Packed struct typedef + assignment-pattern + member-select.
// Tests three Verible-side handlers that previously dropped these
// constructs (and produced single-bit `p` with bare-`p` access):
//
//  (1) extract_typedefs / extract_struct_defs: a typedef whose body
//      is `struct packed { ... }` must be widened to the SUM of its
//      field widths. extract_range used to dive into the first
//      field's [W:0] and short-circuit; now we look for a
//      struct_union_member list first.
//
//  (2) assignment_pattern2 (`'{f1: x, f2: y}`): emit BConcat with
//      fields in declared order (MSB → LSB per SV semantics).
//      Looks up cur_struct_defs by matching the key set so the
//      ordering comes from the typedef, not the source ordering.
//
//  (3) reference2 + hierarchy_extension1 (`p.field`): emit BSlice
//      using the field's bit range computed from cur_struct_defs.
//
// Found via random_sv_gen --features=struct.
module packed_struct (
  input  logic [3:0] hi_in,
  input  logic [3:0] lo_in,
  output logic [3:0] hi_out,
  output logic [3:0] lo_out
);
  typedef struct packed {
    logic [3:0] hi;
    logic [3:0] lo;
  } pair_t;
  pair_t p;
  assign p = '{hi: hi_in, lo: lo_in};
  assign hi_out = p.hi;
  assign lo_out = p.lo;
endmodule
