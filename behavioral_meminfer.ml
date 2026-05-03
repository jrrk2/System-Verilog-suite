(* Memory inference pass on Behavioral IR.
 *
 * Three patterns are recognised and rewritten:
 *
 * 1. RAM (sync write, async read)
 *
 *      always_ff @(posedge clk)
 *        if (we) mem[adr] <= di;
 *      // separate combinational read:
 *      assign do = mem[adr];
 *
 *    The @(posedge clk) body is at this point a `BSequential` whose
 *    body (after if-lift) is a single
 *
 *      BCallStmt { func = "@mem_write"; args = [BVar mem; addr; di] }
 *
 *    wrapped in a `BIf { condition = we; ... }`. Detection drops
 *    a `bmem` declaration with kind = BRam onto the module's `mems`
 *    list. The original BIR statements stay in place — downstream
 *    encoders use the metadata to pick an array sort.
 *
 * 2. ROM (constant lookup table from a case statement)
 *
 *      always_comb
 *        case (sel)
 *          0: out = 8'h12;
 *          1: out = 8'h34;
 *          ...
 *        endcase
 *
 *    When every case arm is a single BAssign to the same lhs with a
 *    constant value, lift to a bmem with the constants as
 *    `init_values` and rewrite the case as a chain of BConds:
 *
 *      out = (sel == 0) ? 8'h12
 *          : (sel == 1) ? 8'h34
 *          : ... default;
 *
 * 3. ROM (read-only memory access on the always_comb side)
 *
 *      assign do = mem[adr];
 *
 *    A bmem with `init_values = []` and kind = BRom is dropped on
 *    `mems` if the same mem name has no `@mem_write` anywhere in
 *    the module — i.e. it's truly read-only.
 *)

open Behavioral_ir

(* ─── Helpers ─────────────────────────────────────────────────────────── *)

let is_const_int = function BConst _ -> true | _ -> false
let const_value = function BConst { value; _ } -> value | _ -> 0
let const_width = function BConst { width; _ } -> width | _ -> 32

(* Walk all expressions in a statement tree and collect names of
 * memories that appear on the RHS of an `@mem_write`. *)
let walked_writes stmts =
  let writes = Hashtbl.create 8 in
  let rec walk_s = function
    | BCallStmt { func = "@mem_write"; args = (BVar m :: _) } ->
        Hashtbl.replace writes m ()
    | BIf { then_stmts; else_stmts; _ } ->
        List.iter walk_s then_stmts; List.iter walk_s else_stmts
    | BCase { cases; default; _ } ->
        List.iter (fun (_, ss) -> List.iter walk_s ss) cases;
        List.iter walk_s default
    | BBlock ss -> List.iter walk_s ss
    | BWhile { body; _ } | BFor { body; _ } -> List.iter walk_s body
    | _ -> ()
  in
  List.iter walk_s stmts;
  writes

(* Memory names read via BSelect anywhere in an expression tree. *)
let collect_reads_e e =
  let names = ref [] in
  let rec walk = function
    | BSelect { array = BVar m; index = _ } ->
        if not (List.mem m !names) then names := m :: !names
    | BBinOp { lhs; rhs; _ } -> walk lhs; walk rhs
    | BUnOp { operand; _ } -> walk operand
    | BSlice { signal; _ } -> walk signal
    | BConcat es -> List.iter walk es
    | BReplicate { value; _ } -> walk value
    | BCond { condition; then_val; else_val } ->
        walk condition; walk then_val; walk else_val
    | BCall { args; _ } -> List.iter walk args
    | BSelect { array; index } -> walk array; walk index
    | _ -> ()
  in
  walk e;
  !names

let collect_reads_s stmts =
  let acc = ref [] in
  let add l =
    List.iter (fun n -> if not (List.mem n !acc) then acc := n :: !acc) l
  in
  let rec walk_s = function
    | BAssign { rhs; _ } -> add (collect_reads_e rhs)
    | BIf { condition; then_stmts; else_stmts } ->
        add (collect_reads_e condition);
        List.iter walk_s then_stmts; List.iter walk_s else_stmts
    | BCase { selector; cases; default } ->
        add (collect_reads_e selector);
        List.iter (fun (k, ss) ->
          add (collect_reads_e k); List.iter walk_s ss) cases;
        List.iter walk_s default
    | BBlock ss -> List.iter walk_s ss
    | BWhile { condition; body } | BFor { condition; body; _ } ->
        add (collect_reads_e condition);
        List.iter walk_s body
    | BCallStmt { args; _ } -> List.iter (fun e -> add (collect_reads_e e)) args
    | _ -> ()
  in
  List.iter walk_s stmts;
  !acc

(* ─── RAM inference ───────────────────────────────────────────────────── *)

(* Look at a single sequential process body for an @mem_write call.
 * If found, return the (mem_name, addr_width_estimate, data_width). *)
let find_ram_writes body =
  let rec walk = function
    | BCallStmt { func = "@mem_write";
                  args = [BVar m; addr; data] } ->
        let aw = match addr with
          | BConst { width; _ } -> width
          | _ -> 32
        in
        let dw = match data with
          | BConst { width; _ } -> width
          | _ -> 32
        in
        [(m, aw, dw)]
    | BIf { then_stmts; else_stmts; _ } ->
        List.concat_map walk (then_stmts @ else_stmts)
    | BCase { cases; default; _ } ->
        let cs = List.concat_map (fun (_, ss) ->
          List.concat_map walk ss) cases in
        cs @ List.concat_map walk default
    | BBlock ss -> List.concat_map walk ss
    | _ -> []
  in
  List.concat_map (fun p ->
    match p with
    | BSequential s -> List.concat_map walk s.body
    | _ -> []) body

(* ─── ROM lift from constant case ─────────────────────────────────────── *)

(* If every case arm is a single BAssign to the same lhs with a
 * constant value, return Some (lhs, indexed_const_table) where
 * indexed_const_table is the (key_value, value, width) for each arm.
 * The default arm (if any) becomes the fall-through value. *)
let try_extract_const_rom selector cases default =
  let body_target = function
    | [BAssign { lhs; rhs }] -> Some (lhs, rhs)
    | [BBlock [BAssign { lhs; rhs }]] -> Some (lhs, rhs)
    | _ -> None
  in
  match cases with
  | [] -> None
  | (_, first_ss) :: _ ->
      (match body_target first_ss with
       | None -> None
       | Some (lhs0, _) ->
           let case_pairs = List.filter_map (fun (k, ss) ->
             match body_target ss with
             | Some (lhs, rhs)
               when lhs = lhs0 && is_const_int k && is_const_int rhs ->
                 Some (const_value k, const_value rhs, const_width rhs)
             | _ -> None
           ) cases in
           if List.length case_pairs <> List.length cases then None
           else
             let default_const = match default with
               | [] -> None
               | _ ->
                   (match body_target default with
                    | Some (lhs, rhs)
                      when lhs = lhs0 && is_const_int rhs ->
                        Some (const_value rhs, const_width rhs)
                    | _ -> None)
             in
             Some (lhs0, selector, case_pairs, default_const))

(* Build a chain of BConds equivalent to the case statement.
 *
 *    sel == k0 ? v0 : (sel == k1 ? v1 : ... default)  *)
let build_rom_lookup selector pairs default =
  let default_expr = match default with
    | Some (v, w) -> BConst { value = v; width = w }
    | None ->
        (* No default — fall through to 0 *)
        BConst { value = 0;
                 width = (match pairs with
                          | (_, _, w) :: _ -> w
                          | _ -> 1) }
  in
  List.fold_right (fun (k, v, w) acc ->
    BCond {
      condition = BBinOp { op = BEq;
                           lhs = selector;
                           rhs = BConst { value = k; width = w };
                           result_type = BBool };
      then_val = BConst { value = v; width = w };
      else_val = acc;
    }
  ) pairs default_expr

(* Compute address width from the largest case key. *)
let bits_needed n =
  if n <= 0 then 1
  else
    let rec loop n b = if n = 0 then b else loop (n lsr 1) (b + 1) in
    loop n 0

(* Walk every statement in a process body. When we find a BCase that
 * matches the constant-rom shape, lift it to a BAssign + record a
 * bmem. Returns rewritten body and the list of inferred bmems. *)
let rewrite_body_for_rom body =
  let mems = ref [] in
  let rec walk_s = function
    | BCase { selector; cases; default } as orig ->
        (match try_extract_const_rom selector cases default with
         | Some (lhs, sel, pairs, def) ->
             let rom_name = "rom_" ^ lhs in
             let depth = List.length pairs in
             let addr_w = bits_needed (depth - 1) in
             let data_w =
               match pairs with (_, _, w) :: _ -> w | _ -> 1
             in
             let init_values = List.map (fun (_, v, _) -> v) pairs in
             mems := { mname = rom_name;
                       data_width = data_w;
                       addr_width = addr_w;
                       depth;
                       kind = BRom;
                       init_values } :: !mems;
             BAssign { lhs;
                       rhs = build_rom_lookup sel pairs def }
         | None ->
             let cases' = List.map (fun (k, ss) ->
               (k, List.map walk_s ss)) cases in
             let default' = List.map walk_s default in
             BCase { selector; cases = cases'; default = default' })
        |> fun s -> (match s with _ -> ignore orig; s)
    | BIf { condition; then_stmts; else_stmts } ->
        BIf { condition;
              then_stmts = List.map walk_s then_stmts;
              else_stmts = List.map walk_s else_stmts }
    | BBlock ss -> BBlock (List.map walk_s ss)
    | BWhile { condition; body } ->
        BWhile { condition; body = List.map walk_s body }
    | BFor { init; condition; update; body } ->
        BFor { init; condition; update;
               body = List.map walk_s body }
    | other -> other
  in
  let body' = List.map walk_s body in
  (body', !mems)

let rewrite_process_for_rom = function
  | BCombinational c ->
      let (body', mems) = rewrite_body_for_rom c.body in
      (BCombinational { c with body = body' }, mems)
  | BSequential s ->
      let (body', mems) = rewrite_body_for_rom s.body in
      (BSequential { s with body = body' }, mems)

(* ─── Top-level pass ─────────────────────────────────────────────────── *)

let infer_module (m : bmodule) =
  (* Step 1: case-with-constants → ROM rewrite. *)
  let rewritten, rom_mems_list =
    List.fold_left (fun (acc_p, acc_m) p ->
      let (p', mems) = rewrite_process_for_rom p in
      (p' :: acc_p, mems @ acc_m)
    ) ([], []) m.processes
  in
  let processes = List.rev rewritten in
  let rom_mems = rom_mems_list in

  (* Step 2: RAM inference from sync writes. The widths gleaned from
   * the BConst form of the index/data are usually meaningless (the
   * defaults), so prefer the declared signal types when available. *)
  let lookup_signal name =
    List.find_opt (fun (s : bsignal) -> s.name = name) m.signals
  in
  let signal_data_width name =
    match lookup_signal name with
    | Some { stype = BArray { element = BInt { width; _ }; _ }; _ } -> Some width
    | Some { stype = BInt { width; _ }; _ } -> Some width
    | _ -> None
  in
  let signal_array_depth name =
    match lookup_signal name with
    | Some { stype = BArray { size; _ }; _ } -> Some size
    | _ -> None
  in
  let ram_writes = find_ram_writes processes in
  let ram_mems = List.map (fun (n, _aw, dw) ->
    let data_w = match signal_data_width n with
      | Some w -> w
      | None -> if dw = 0 then 1 else dw
    in
    let depth = match signal_array_depth n with
      | Some d -> d
      | None -> 256  (* heuristic — we don't know the depth *)
    in
    let addr_w = bits_needed (depth - 1) in
    { mname = n;
      data_width = data_w;
      addr_width = max 1 addr_w;
      depth;
      kind = BRam;
      init_values = [] }
  ) ram_writes in

  (* Step 3: read-only memory inference — names appearing in BSelect
   * but never in @mem_write are marked as ROM with empty contents
   * (the contents come from the source declaration which we don't
   * see here). Drop ones already covered by RAM/ROM-from-case. *)
  let writes = walked_writes (List.concat_map (function
    | BCombinational c -> c.body
    | BSequential s -> s.body) processes)
  in
  let reads = collect_reads_s (List.concat_map (function
    | BCombinational c -> c.body
    | BSequential s -> s.body) processes)
  in
  let already_listed n =
    List.exists (fun mm -> mm.mname = n) (rom_mems @ ram_mems)
  in
  let read_only_roms = List.filter_map (fun n ->
    if already_listed n then None
    else if Hashtbl.mem writes n then None
    else begin
      (* Pull dimensions from the declared array type when present.
       * If the name isn't an array (just a scalar that happened to be
       * read by index), bail. *)
      match signal_array_depth n with
      | None -> None
      | Some depth ->
          let data_w = match signal_data_width n with
            | Some w -> w
            | None -> 1
          in
          let addr_w = max 1 (bits_needed (depth - 1)) in
          Some { mname = n;
                 data_width = data_w;
                 addr_width = addr_w;
                 depth;
                 kind = BRom;
                 init_values = [] }
    end
  ) reads
  in

  let new_mems = ram_mems @ rom_mems @ read_only_roms in
  { m with processes; mems = new_mems @ m.mems }

let infer_program (p : bprogram) =
  { p with modules = List.map infer_module p.modules }
