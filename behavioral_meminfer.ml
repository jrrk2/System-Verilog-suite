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
let const_value = function BConst { value; _ } -> Z.to_int value | _ -> 0

(* A constant that does not FIT in an OCaml int cannot become a ROM entry:
 * const_value would raise Z.Overflow.  litesoc hits this -- LiteDRAM at 32-bit
 * with a 1:4 PHY ratio carries a 256-bit user data path, and a case over those
 * values is not a small lookup table.  Excluding such a case from case_pairs
 * makes the length check below abandon the ROM rewrite, which is the right
 * answer: leave the logic alone rather than truncating a 256-bit constant. *)
let const_fits = function BConst { value; _ } -> Z.fits_int value | _ -> false
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
               when lhs = lhs0 && is_const_int k && is_const_int rhs
                    && const_fits k && const_fits rhs ->
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
                      when lhs = lhs0 && is_const_int rhs && const_fits rhs ->
                        Some (const_value rhs, const_width rhs)
                    | _ -> None)
             in
             Some (lhs0, selector, case_pairs, default_const))

(* Build a chain of BConds equivalent to the case statement.
 *
 *    sel == k0 ? v0 : (sel == k1 ? v1 : ... default)  *)
let build_rom_lookup selector pairs default =
  let default_expr = match default with
    | Some (v, w) -> BConst { value = Z.of_int v; width = w }
    | None ->
        (* No default — fall through to 0 *)
        BConst { value = Z.zero;
                 width = (match pairs with
                          | (_, _, w) :: _ -> w
                          | _ -> 1) }
  in
  List.fold_right (fun (k, v, w) acc ->
    BCond {
      condition = BBinOp { op = BEq;
                           lhs = selector;
                           rhs = BConst { value = Z.of_int k; width = w };
                           result_type = BBool };
      then_val = BConst { value = Z.of_int v; width = w };
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
let rewrite_body_for_rom ~is_seq body =
  (* FPGA flow wants the read to survive as a [BSelect] of an
     INIT-initialised BRom so memlower can map it to a block RAM; the
     miter/z3 flow wants the explicit BCond mux (it does not interpret a
     BRom's init_values).  Gate the shape on MEMLOWER_FPGA so the miter
     path is byte-for-byte unchanged. *)
  let fpga = Sys.getenv_opt "MEMLOWER_FPGA" = Some "1" in
  let mems = ref [] in
  (* ROMs used to be named `rom_<lhs>` — keyed only by the assigned
     signal.  A process with several DISTINCT `case(sel)` tables all
     assigning the same signal (e.g. ibex LSU's data_be: word=1111.. ,
     half=0011.. , byte=0001..) then collapsed onto one name, and every
     read picked up whichever table happened to be declared last (the
     byte decoder {1,2,4,8}).  That silently turned word stores into
     single-byte stores.  Key the name by table CONTENT instead:
     identical tables share one ROM (harmless dedup); distinct tables
     that target the same signal get a `_<n>` suffix so each keeps its
     own init.  `fresh` gates the mem-list push so shared tables aren't
     declared twice. *)
  let rom_sig_names :
        (string * (int * int * int) list * (int * int) option, string)
          Hashtbl.t = Hashtbl.create 8 in
  let rom_name_next : (string, int) Hashtbl.t = Hashtbl.create 8 in
  let unique_rom_name lhs pairs def =
    match Hashtbl.find_opt rom_sig_names (lhs, pairs, def) with
    | Some n -> (n, false)
    | None ->
        let name =
          match Hashtbl.find_opt rom_name_next lhs with
          | None -> Hashtbl.replace rom_name_next lhs 1; "rom_" ^ lhs
          | Some i ->
              Hashtbl.replace rom_name_next lhs (i + 1);
              Printf.sprintf "rom_%s_%d" lhs i
        in
        Hashtbl.replace rom_sig_names (lhs, pairs, def) name;
        (name, true)
  in
  let rec walk_s = function
    | BCase { selector; cases; default } as orig ->
        (match try_extract_const_rom selector cases default with
         | Some (lhs, sel, pairs, def) ->
             let (rom_name, fresh) = unique_rom_name lhs pairs def in
             let data_w =
               match pairs with (_, _, w) :: _ -> w | _ -> 1
             in
             if fpga then begin
               (* Index the INIT by case key (address), sized to the full
                  address space so reads in the gaps return the default,
                  matching the source `default:` arm.  read_is_sync tracks
                  the process: a clocked case (e.g. progmem's
                  `always @(posedge clk) case ... mem <= v`) is a sync ROM
                  read that maps directly onto a BRAM output register. *)
               let max_key =
                 List.fold_left (fun a (k, _, _) -> max a k) 0 pairs in
               let addr_w = bits_needed max_key in
               let depth = 1 lsl addr_w in
               let def_val = match def with Some (v, _) -> v | None -> 0 in
               let init = Array.make depth def_val in
               List.iter (fun (k, v, _) ->
                 if k >= 0 && k < depth then init.(k) <- v) pairs;
               if fresh then
                 mems := { mname = rom_name;
                           data_width = data_w;
                           addr_width = addr_w;
                           depth;
                           kind = BRom;
                           init_values = Array.to_list init;
                           n_write_ports = 0;
                           n_read_ports = 1;
                           read_is_sync = is_seq } :: !mems;
               BAssign { lhs; rhs = BSelect { array = BVar rom_name; index = sel } }
             end else begin
               let depth = List.length pairs in
               let addr_w = bits_needed (depth - 1) in
               let init_values = List.map (fun (_, v, _) -> v) pairs in
               if fresh then
                 mems := { mname = rom_name;
                           data_width = data_w;
                           addr_width = addr_w;
                           depth;
                           kind = BRom;
                           init_values;
                           n_write_ports = 0;
                           n_read_ports = 1;
                           read_is_sync = false } :: !mems;
               BAssign { lhs; rhs = build_rom_lookup sel pairs def }
             end
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
      let (body', mems) = rewrite_body_for_rom ~is_seq:false c.body in
      (BCombinational { c with body = body' }, mems)
  | BSequential s ->
      let (body', mems) = rewrite_body_for_rom ~is_seq:true s.body in
      (BSequential { s with body = body' }, mems)

(* ─── Port-count + sync/async classification ─────────────────────────── *)

(* Collect distinct read-address expressions for memory `m` across
 * `processes`. Two reads count as the same port only if the index
 * expressions are structurally identical AND the surrounding process
 * is the same one — the conservative choice is to treat every distinct
 * (process_name, index_expr) tuple as a separate port. *)
(* Logical read-port count.  Same shape as writes (sites along a
 * concurrent path SUM; mutex branches MAX), only the leaf changes:
 * a [BSelect { array = BVar m_name; _ }] site adds one read,
 * regardless of where in the expression tree it appears.  Conditional
 * expressions (BCond) treat then/else as mutex like BIf does. *)
let count_read_sites_in_body m_name body =
  let rec expr = function
    | BSelect { array = BVar n; index } when n = m_name ->
        1 + expr index
    | BBinOp { lhs; rhs; _ } -> expr lhs + expr rhs
    | BUnOp { operand; _ } -> expr operand
    | BSlice { signal; _ } -> expr signal
    | BSelect { array; index } -> expr array + expr index
    | BConcat es -> List.fold_left (fun a e -> a + expr e) 0 es
    | BReplicate { value; _ } -> expr value
    | BCond { condition; then_val; else_val } ->
        expr condition + max (expr then_val) (expr else_val)
    | BCall { args; _ } ->
        List.fold_left (fun a e -> a + expr e) 0 args
    | _ -> 0
  in
  let rec stmt = function
    | BAssign { rhs; _ } -> expr rhs
    | BIf { condition; then_stmts; else_stmts } ->
        expr condition + max (stmts then_stmts) (stmts else_stmts)
    | BCase { selector; cases; default } ->
        let sel = expr selector in
        let arm_counts =
          List.map (fun (k, ss) -> expr k + stmts ss) cases in
        let d = stmts default in
        sel + List.fold_left max d arm_counts
    | BBlock ss -> stmts ss
    | BWhile { condition; body } -> expr condition + stmts body
    | BFor   { condition; body; _ } -> expr condition + stmts body
    | BCallStmt { func = "@mem_write";
                  args = (BVar n) :: rest } when n = m_name ->
        (* RMW reads of the same memory inside its own @mem_write
           data argument are NOT separate read ports — they're the
           macro's "value-before-write" which a byte-mask cell
           computes internally.  Count reads to OTHER memories in
           rest normally; skip self-reads via expr_skip_self.    *)
        List.fold_left (fun a e -> a + expr_skip_self e) 0 rest
    | BCallStmt { args; _ } ->
        List.fold_left (fun a e -> a + expr e) 0 args
    | _ -> 0
  and expr_skip_self = function
    | BSelect { array = BVar n; index } when n = m_name ->
        (* This is the RMW read of m_name — skip it.  Recurse into
           index in case it has reads of other memories.         *)
        expr_skip_self index
    | BBinOp { lhs; rhs; _ } -> expr_skip_self lhs + expr_skip_self rhs
    | BUnOp { operand; _ } -> expr_skip_self operand
    | BSlice { signal; _ } -> expr_skip_self signal
    | BSelect { array; index } -> expr_skip_self array + expr_skip_self index
    | BConcat es -> List.fold_left (fun a e -> a + expr_skip_self e) 0 es
    | BReplicate { value; _ } -> expr_skip_self value
    | BCond { condition; then_val; else_val } ->
        expr_skip_self condition
        + max (expr_skip_self then_val) (expr_skip_self else_val)
    | BCall { args; _ } ->
        List.fold_left (fun a e -> a + expr_skip_self e) 0 args
    | _ -> 0
  and stmts ss = List.fold_left (fun acc s -> acc + stmt s) 0 ss
  in
  stmts body

let count_read_ports m_name processes =
  List.fold_left (fun acc p ->
    let body = match p with
      | BCombinational c -> c.body
      | BSequential s -> s.body
    in
    acc + count_read_sites_in_body m_name body
  ) 0 processes

(* Logical write-port count.
 *
 * The number of physical ports a memory needs is the **maximum number
 * of concurrent writes** that can happen on a single clock edge.  We
 * count it per process by walking the AST:
 *
 *   - Sequential statements (siblings at the same nesting level, OR
 *     N sibling always blocks merged by [merge_seq_processes] into a
 *     single body) all fire concurrently under non-blocking semantics
 *     ⇒ their write counts SUM.
 *   - if/else and case arms are mutually exclusive at runtime ⇒ their
 *     write counts MAX.
 *
 * Then sum across processes.  Sites within mutex branches still get
 * collapsed to 1 port at lowering time (enable=OR, addr/data=priority
 * mux); sibling sites become independent ports.
 *
 * Reads use the same shape (see [count_read_ports] below).
 *)
let count_write_sites_in_body m_name body =
  let rec stmt = function
    | BCallStmt { func = "@mem_write"; args = (BVar n) :: _ }
      when n = m_name -> 1
    | BIf { then_stmts; else_stmts; _ } ->
        max (stmts then_stmts) (stmts else_stmts)
    | BCase { cases; default; _ } ->
        let arms = List.map (fun (_, ss) -> stmts ss) cases in
        let d = stmts default in
        List.fold_left max d arms
    | BBlock ss -> stmts ss
    | BWhile { body; _ } -> stmts body
    | BFor   { body; _ } -> stmts body
    | _ -> 0
  and stmts ss = List.fold_left (fun acc s -> acc + stmt s) 0 ss
  in
  stmts body

let count_write_ports m_name processes =
  List.fold_left (fun acc p ->
    match p with
    | BSequential s -> acc + count_write_sites_in_body m_name s.body
    | _ -> acc
  ) 0 processes

(* True if any read of `m_name` lives inside a BCombinational process —
 * that's the distributed/async-RAM pattern (Vivado infers LUT RAM).
 * False if every read is from a BSequential — block-RAM pattern. *)
let read_is_async m_name processes =
  let found_async = ref false in
  let found_any = ref false in
  let rec walk_e in_comb = function
    | BSelect { array = BVar n; _ } when n = m_name ->
        found_any := true;
        if in_comb then found_async := true
    | BBinOp { lhs; rhs; _ } -> walk_e in_comb lhs; walk_e in_comb rhs
    | BUnOp { operand; _ } -> walk_e in_comb operand
    | BSlice { signal; _ } -> walk_e in_comb signal
    | BSelect { array; index } -> walk_e in_comb array; walk_e in_comb index
    | BConcat es -> List.iter (walk_e in_comb) es
    | BReplicate { value; _ } -> walk_e in_comb value
    | BCond { condition; then_val; else_val } ->
        walk_e in_comb condition;
        walk_e in_comb then_val;
        walk_e in_comb else_val
    | BCall { args; _ } -> List.iter (walk_e in_comb) args
    | _ -> ()
  in
  let rec walk_s in_comb = function
    | BAssign { rhs; _ } -> walk_e in_comb rhs
    | BIf { condition; then_stmts; else_stmts } ->
        walk_e in_comb condition;
        List.iter (walk_s in_comb) then_stmts;
        List.iter (walk_s in_comb) else_stmts
    | BCase { selector; cases; default } ->
        walk_e in_comb selector;
        List.iter (fun (k, ss) ->
          walk_e in_comb k; List.iter (walk_s in_comb) ss) cases;
        List.iter (walk_s in_comb) default
    | BBlock ss -> List.iter (walk_s in_comb) ss
    | BWhile { condition; body } ->
        walk_e in_comb condition; List.iter (walk_s in_comb) body
    | BFor { condition; body; _ } ->
        walk_e in_comb condition; List.iter (walk_s in_comb) body
    | BCallStmt { args; _ } -> List.iter (walk_e in_comb) args
    | _ -> ()
  in
  List.iter (function
    | BCombinational c -> List.iter (walk_s true) c.body
    | BSequential s -> List.iter (walk_s false) s.body
  ) processes;
  if not !found_any then false  (* no reads at all — irrelevant *)
  else !found_async

(* Human-readable categorisation. *)
let kind_label mm =
  match mm.kind, mm.n_write_ports, mm.n_read_ports, mm.read_is_sync with
  | BRom, _, _, _ -> "ROM"
  | BRam, 0, _, _ -> "RAM (read-only — promote to ROM?)"
  | BRam, 1, 1, true  -> "single_port_bram"
  | BRam, 1, 2, true  -> "simple_dual_port_bram"
  | BRam, 2, 2, true  -> "true_dual_port_bram"
  | BRam, 1, 1, false -> "distributed_async_1w1r"
  | BRam, 1, 2, false -> "distributed_async_1w2r"
  | BRam, 1, n, false -> Printf.sprintf "distributed_async_1w%dr" n
  | BRam, w, r, sync ->
      Printf.sprintf "ram_%dw%dr_%s" w r (if sync then "sync" else "async")

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
  (* find_ram_writes returns one (name, aw, dw) tuple per @mem_write
     site, so the same memory shows up N times after generate-unroll
     etc.  Dedup by name, keeping the widest seen data width.

     IMPORTANT: filter out names whose declared type is a scalar BInt
     rather than a BArray.  The Verible converter promotes any indexed
     LHS (incl. `scalar_reg[bit_idx] <= ...`) into the @mem_write
     intermediate so [merge_array_writes] can fold cell-mapped per-bit
     drives back to a full-bus assign.  When that fold doesn't fire
     (non-contiguous bit positions, irregular indices, etc.) the
     @mem_write residue is still a SCALAR bit-blast, not a real memory
     — naively classifying it produces nonsense like next_irq_pending
     w_ports=5 (in fact 5 bit-set sites on a single 32-bit reg). *)
  let ram_writes =
    let h = Hashtbl.create 8 in
    List.iter (fun (n, aw, dw) ->
      match lookup_signal n with
      | Some { stype = BArray _; _ } ->
          (match Hashtbl.find_opt h n with
           | None -> Hashtbl.add h n (n, aw, dw)
           | Some (_, aw', dw') ->
               Hashtbl.replace h n (n, max aw aw', max dw dw'))
      | _ ->
          (* Scalar BInt with @mem_writes — bit-blast residue, not a
             memory.  Leave it for the bit-level lowering path. *)
          ()
    ) (find_ram_writes processes);
    Hashtbl.fold (fun _ v acc -> v :: acc) h []
  in
  (* The $readmemh loader is a combinational whole-array assign
     `mem = {word_{d-1}, ..., word_0}` (MSB-first constants).  For a ROM that
     drive IS the memory; for a read-WRITE RAM we LIFT the constants into
     init_values (baked into the BRAM INIT at config, exactly like real
     hardware) and STRIP the driver below — otherwise it re-clamps mem to the
     boot image every cycle and the RAM is never writable. *)
  let mem_init_words ?(depth = 0) ?(data_w = 0) n =
    let of_stmt = function
      | BAssign { lhs; rhs = BConcat parts }
        when lhs = n && parts <> []
             && List.for_all (function BConst _ -> true | _ -> false) parts ->
          (* MSB-first parts -> address order (word_0 first) via rev_map *)
          Some (List.rev_map (function
                  | BConst { value; _ } -> Z.to_int value | _ -> 0) parts)
      (* A SINGLE FOLDED CONSTANT.  `initial $readmemh(...)` reaches BIR as an
         ordinary process, and unroll/blocking_subst collapse it into ONE packed
         literal assigned to the array -- not the BConcat of per-word constants
         the arm above expects.  The data was therefore invisible: litesoc's
         8191-word BIOS ROM arrived with init_values = [] even though meminfer
         had correctly identified it (nW=0, nR=1, sync_read=true), and the whole
         image then survived only as a 262112-bit constant that the Hardcaml
         path bit-sliced 8191 times -- a literal yosys cannot even lex.
         Word k occupies bits [k*data_w .. k*data_w+data_w-1]: the BConcat arm
         is MSB-first and reverses, so word_0 is in the LOW bits either way. *)
      | BAssign { lhs; rhs = BConst { value; width } }
        when lhs = n && depth > 0 && data_w > 0 && data_w <= 62
             && width >= depth * data_w ->
          Some (List.init depth (fun k ->
                  Z.to_int (Z.extract value (k * data_w) data_w)))
      | _ -> None in
    List.find_map (fun p ->
      let body = match p with BCombinational c -> c.body | BSequential s -> s.body in
      List.find_map of_stmt body) processes in
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
      init_values = (match mem_init_words ~depth ~data_w n with Some vs -> vs | None -> []);
      n_write_ports = count_write_ports n processes;
      n_read_ports = count_read_ports n processes;
      read_is_sync = not (read_is_async n processes) }
  ) ram_writes in
  (* STRIP the $readmemh whole-array const driver for every inferred RAM: its
     values now live in init_values (BRAM INIT), and keeping the combinational
     driver would clobber writes (RAM non-writable) and dangle once mem becomes
     a RAMB primitive. *)
  let ram_names = List.map (fun (n, _, _) -> n) ram_writes in
  let is_mem_init_driver names p =
    let body = match p with BCombinational c -> c.body | BSequential s -> s.body in
    match body with
    | [ BAssign { lhs; rhs = BConcat parts } ]
      when List.mem lhs names
           && List.for_all (function BConst _ -> true | _ -> false) parts -> true
    | _ -> false in
  let processes = List.filter (fun p -> not (is_mem_init_driver ram_names p)) processes in

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
                 (* Harvest the image rather than declaring it empty.  A
                    read-only array IS a ROM, and its contents arrive as a
                    folded constant assignment (see mem_init_words): an
                    `initial $readmemh(...)` becomes an ordinary process that
                    unroll/blocking_subst collapse into one packed literal.
                    Hardcoding [] here is what left litesoc's 8191-word BIOS
                    ROM with no image despite being correctly identified as
                    nW=0/nR=1/sync_read -- so memlower's BRom->BRAM path could
                    never fire, and the data survived only as a 262112-bit
                    constant the Hardcaml path bit-sliced 8191 times. *)
                 init_values =
                   (match mem_init_words ~depth ~data_w n with
                    | Some vs -> vs | None -> []);
                 n_write_ports = 0;
                 n_read_ports = count_read_ports n processes;
                 read_is_sync = not (read_is_async n processes) }
    end
  ) reads
  in

  (* A read-only array's init driver has to go the same way a RAM's does in
     step 2, and for the same reason stated there: once the image is in
     init_values the memory becomes a BRAM primitive, and the leftover
     combinational driver assigns to an array that no longer exists.  Step 2
     could not do this because ROMs are only identified HERE.  Left in, Vivado
     rejects the netlist outright:

       ERROR: [Synth 8-1031] rom is not declared
       ERROR: [Synth 8-1031] mem is not declared

     Drop it ONLY where the image was actually harvested -- with init_values
     still empty the driver is the sole carrier of the contents, and removing
     it would silently blank the ROM. *)
  let rom_init_names =
    List.filter_map (fun (mm : bmem) ->
      if mm.init_values <> [] then Some mm.mname else None) read_only_roms in
  let processes =
    if rom_init_names = [] then processes
    else List.filter (fun p -> not (is_mem_init_driver rom_init_names p)) processes in

  let new_mems = ram_mems @ rom_mems @ read_only_roms in
  { m with processes; mems = new_mems @ m.mems }

let infer_program (p : bprogram) =
  { p with modules = List.map infer_module p.modules }
