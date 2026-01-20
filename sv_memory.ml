(* Memory detection and transformation module *)
open Sv_ast

(* Memory access pattern *)
type mem_access =
  | MemRead of { array_name: string; index: sv_node; }
  | MemWrite of { array_name: string; index: sv_node; data: sv_node; enable: sv_node option; }

(* Memory descriptor *)
type memory_info = {
  mutable name: string;
  mutable addr_width: int;
  mutable data_width: int;
  mutable depth: int;
  mutable size_bits: int;
  mutable read_accesses: mem_access list;
  mutable write_accesses: mem_access list;
  mutable is_sequential: bool;
}

(* Memory size threshold for using memory primitives (in bits) *)
(* 128 bits = 16 bytes or 4x 32-bit registers - reasonable flip-flop threshold *)
let memory_threshold = 128

(* Parse range string like "31:0" or "0:31" to get size *)
let parse_range range_str =
  try
    Scanf.sscanf range_str "%d:%d" (fun hi lo -> abs (hi - lo) + 1)
  with _ -> 1

(* Extract array dimensions and element width from dtype *)
let rec get_array_info dtype_opt =
  match dtype_opt with
  | Some (ArrayType' { base; range }) ->
      (* Unpacked array *)
      let depth = parse_range range in
      (* For now, assume base is resolved elsewhere or use default *)
      (depth, 8) (* Default 8-bit elements, should lookup base *)

  | Some (ArrayType { base; range }) ->
      (* Resolved unpacked array *)
      let depth = parse_range range in
      let (_, elem_width) = get_array_info (Some base) in
      (depth, elem_width)

  | Some (PackArrayType' { base; range }) ->
      (* Packed array - this is part of the element *)
      let width = parse_range range in
      (1, width)

  | Some (PackArrayType { base; range }) ->
      let width = parse_range range in
      let (_, inner_width) = get_array_info (Some base) in
      (1, width * inner_width)

  | Some (BasicType { range = Some r; _ }) ->
      let width = parse_range r in
      (1, width)

  | Some (BasicType { range = None; _ }) ->
      (1, 1)

  | _ -> (1, 1)

(* Calculate total memory size *)
let calculate_memory_size depth width =
  depth * width

(* Detect if variable is a memory array *)
let is_memory_array var =
  match var with
  | Var { dtype_ref; _ } ->
      let (depth, width) = get_array_info dtype_ref in
      let size = calculate_memory_size depth width in
      size > memory_threshold && depth > 1
  | _ -> false

(* Print memory info for debugging *)
let print_memory_info mem =
  Printf.eprintf "  Memory: %s[%d] (depth=%d, width=%d, size=%d bits, reads=%d, writes=%d)\n"
    mem.name mem.depth mem.depth mem.data_width mem.size_bits
    (List.length mem.read_accesses) (List.length mem.write_accesses)

(* Detect all memories in a module *)
let detect_memories stmts =
  let memories = Hashtbl.create 10 in

  (* Find all array variables *)
  List.iter (function
    | Var { name; dtype_ref; _ } as v ->
        let (depth, width) = get_array_info dtype_ref in
        let size = calculate_memory_size depth width in
        Printf.eprintf "        Variable %s: depth=%d, width=%d, size=%d bits\n%!"
          name depth width size;
        if size > memory_threshold && depth > 1 then begin
          let addr_width = int_of_float (ceil (log (float_of_int depth) /. log 2.0)) in
          let mem_info = {
            name;
            addr_width;
            data_width = width;
            depth;
            size_bits = size;
            read_accesses = [];
            write_accesses = [];
            is_sequential = false;
          } in
          Hashtbl.add memories name mem_info;
          print_memory_info mem_info
        end
    | _ -> ()
  ) stmts;

  memories

(* Check if an array should use memory primitive *)
let should_use_memory_primitive mem_info =
  mem_info.size_bits > memory_threshold &&
  mem_info.depth > 1
