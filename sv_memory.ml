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
      (* Standard case: explicit multi-dimensional array *)
      let is_multi_dim_array = size > memory_threshold && depth > 1 in
      (* Special case: Wide packed array that's likely a flattened memory *)
      (* If width > 512 bits and is a reasonable memory size, treat as memory *)
      let is_wide_packed = depth = 1 && width > 512 && size <= 65536 in
      is_multi_dim_array || is_wide_packed
  | _ -> false

(* Print memory info for debugging *)
let print_memory_info mem =
  Printf.eprintf "  Memory: %s[%d] (depth=%d, width=%d, size=%d bits, reads=%d, writes=%d)\n"
    mem.name mem.depth mem.depth mem.data_width mem.size_bits
    (List.length mem.read_accesses) (List.length mem.write_accesses)

(* Infer array dimensions for wide packed arrays *)
let infer_array_dimensions stmts total_width =
  (* Look for DATA_WIDTH or similar parameters *)
  let elem_width_from_params =
    List.find_map (function
      | Var { name; value = Some (Const { name = v; _ }); var_type = "GPARAM"; _ }
        when name = "DATA_WIDTH" || name = "DATAWIDTH" ->
          (try Some (int_of_string v) with _ -> None)
      | _ -> None
    ) stmts
  in

  match elem_width_from_params with
  | Some w when total_width mod w = 0 ->
      let d = total_width / w in
      (d, w)
  | _ ->
      (* Try common element widths: 64, 32, 16, 8 *)
      let common_widths = [64; 32; 16; 8] in
      let found = List.find_opt (fun w ->
        total_width mod w = 0 && total_width / w >= 4
      ) common_widths in
      (match found with
      | Some w -> (total_width / w, w)
      | None -> (1, total_width))  (* Fallback: treat as single element *)

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
        end else if depth = 1 && width > 512 && size <= 65536 then begin
          (* Wide packed array - infer dimensions *)
          Printf.eprintf "        Variable %s is a wide packed array, inferring dimensions...\n%!" name;
          let (inferred_depth, inferred_width) = infer_array_dimensions stmts width in
          Printf.eprintf "        Inferred: depth=%d, width=%d\n%!" inferred_depth inferred_width;
          let addr_width = int_of_float (ceil (log (float_of_int inferred_depth) /. log 2.0)) in
          let mem_info = {
            name;
            addr_width;
            data_width = inferred_width;
            depth = inferred_depth;
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

(* Find all array reads in an expression *)
let rec find_array_reads expr memories =
  match expr with
  | ArraySel { expr = VarRef { name; _ }; index } ->
      if Hashtbl.mem memories name then begin
        let mem = Hashtbl.find memories name in
        let read_acc = MemRead { array_name = name; index } in
        if not (List.exists (fun acc ->
          match acc with
          | MemRead { array_name = n; _ } when n = name ->
              (* Check if same read already recorded - simplified check *)
              false
          | _ -> false
        ) mem.read_accesses) then
          mem.read_accesses <- read_acc :: mem.read_accesses
      end

  | BinaryOp { lhs; rhs; _ } | BinaryOp' { lhs; rhs; _ } ->
      find_array_reads lhs memories;
      find_array_reads rhs memories

  | UnaryOp { operand; _ } | UnaryOp' { operand; _ } ->
      find_array_reads operand memories

  | Cond { condition; then_val; else_val } ->
      find_array_reads condition memories;
      find_array_reads then_val memories;
      find_array_reads else_val memories

  | Concat { parts } ->
      List.iter (fun e -> find_array_reads e memories) parts

  | Sel { expr; _ } ->
      find_array_reads expr memories

  | _ -> ()

(* Find all array writes in statements *)
let rec find_array_writes stmt memories is_clocked =
  match stmt with
  | Assign { lhs = ArraySel { expr = VarRef { name; _ }; index }; rhs; _ }
  | AssignW { lhs = ArraySel { expr = VarRef { name; _ }; index }; rhs; _ } ->
      if Hashtbl.mem memories name then begin
        let mem = Hashtbl.find memories name in
        mem.is_sequential <- is_clocked;
        let write_acc = MemWrite { array_name = name; index; data = rhs; enable = None } in
        mem.write_accesses <- write_acc :: mem.write_accesses;
        (* Also scan RHS for reads *)
        find_array_reads rhs memories
      end

  | Assign { rhs; _ } | AssignW { rhs; _ } ->
      (* Check for array reads in RHS *)
      find_array_reads rhs memories

  | Always { stmts; _ } ->
      (* Always blocks are typically clocked *)
      List.iter (fun s -> find_array_writes s memories true) stmts

  | If { condition; then_stmt; else_stmt; _ } ->
      find_array_reads condition memories;
      find_array_writes then_stmt memories is_clocked;
      Option.iter (fun e -> find_array_writes e memories is_clocked) else_stmt

  | Case { expr; items; _ } ->
      find_array_reads expr memories;
      List.iter (fun item ->
        List.iter (fun s -> find_array_writes s memories is_clocked) item.statements
      ) items

  | Begin { stmts; _ } ->
      List.iter (fun s -> find_array_writes s memories is_clocked) stmts

  | For { stmts; _ } | For' { stmts; _ } ->
      List.iter (fun s -> find_array_writes s memories is_clocked) stmts

  | _ -> ()

(* Check for conflicting memory accesses in the same cycle *)
let check_memory_conflicts memories =
  let has_error = ref false in
  Hashtbl.iter (fun name mem ->
    let num_reads = List.length mem.read_accesses in
    let num_writes = List.length mem.write_accesses in
    let total_accesses = num_reads + num_writes in

    (* Check for multiple accesses to same memory in one cycle *)
    if total_accesses > 2 then begin
      Printf.eprintf "\n";
      Printf.eprintf "Error:  Multiple conflicting accesses to memory '%s' in same cycle\n" name;
      Printf.eprintf "        %d read port(s), %d write port(s) detected\n" num_reads num_writes;
      Printf.eprintf "        Memory can support at most 2 read ports and 2 write ports\n";
      Printf.eprintf "        Consider: using multi-ported memory or time-multiplexing accesses\n";
      has_error := true
    end;

    (* Check for too many write ports *)
    if num_writes > 2 then begin
      Printf.eprintf "\n";
      Printf.eprintf "Error:  Too many write ports to memory '%s'\n" name;
      Printf.eprintf "        %d write port(s) detected, maximum 2 supported\n" num_writes;
      Printf.eprintf "        Consider: arbitration logic or banking the memory\n";
      has_error := true
    end;

    (* Check for too many read ports *)
    if num_reads > 2 then begin
      Printf.eprintf "\n";
      Printf.eprintf "Error:  Too many read ports to memory '%s'\n" name;
      Printf.eprintf "        %d read port(s) detected, maximum 2 supported\n" num_reads;
      Printf.eprintf "        Consider: replicating memory or time-multiplexing reads\n";
      has_error := true
    end
  ) memories;

  if !has_error then begin
    Printf.eprintf "\n";
    Printf.eprintf "Fatal: Memory access conflicts detected. Cannot synthesize.\n";
    Printf.eprintf "       Please resolve conflicts and retry.\n";
    Printf.eprintf "\n%!"
  end;
  !has_error

(* Analyze access patterns after detection *)
let analyze_memory_accesses stmts memories =
  List.iter (fun stmt ->
    find_array_writes stmt memories false
  ) stmts;

  (* Print summary *)
  Hashtbl.iter (fun _ mem ->
    if List.length mem.read_accesses > 0 || List.length mem.write_accesses > 0 then begin
      Printf.eprintf "    Access pattern for %s: %d reads, %d writes, %s\n%!"
        mem.name
        (List.length mem.read_accesses)
        (List.length mem.write_accesses)
        (if mem.is_sequential then "sequential" else "combinational")
    end
  ) memories;

  (* Check for conflicts and report errors *)
  let _ = check_memory_conflicts memories in
  ()

(* Check if an array should use memory primitive *)
let should_use_memory_primitive mem_info =
  mem_info.size_bits > memory_threshold &&
  mem_info.depth > 1 &&
  (List.length mem_info.read_accesses > 0 || List.length mem_info.write_accesses > 0)
