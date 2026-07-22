(* Lower BIR memories to instances of OpenRAM-generated macros.
 *
 * Phase-3 v1 scope:
 *   - one BRam per module
 *   - sync read (read_is_sync = true)
 *   - n_write_ports ∈ {1}, n_read_ports ∈ {1, 2}; reject anything else
 *     and leave the bit-blast in place (caller falls back gracefully)
 *   - exactly one top-level @mem_write site, optionally guarded by a
 *     single outer `if (we) …` (no nested if/else mux yet — that's
 *     phase-3 v2 once the mux-fold helper lands)
 *   - exactly one BSelect read site driving an FF in a separate
 *     BSequential process (the typical `q <= mem[r_addr]` pattern)
 *
 * Anything outside that scope: the bmem stays in m.mems, the BArray
 * signal stays, the @mem_write/BSelect stays — bit-blast continues.
 *
 * Lower an in-scope memory by:
 *   1. resolve a 1RW1R OpenRAM macro for the (depth, width, tech).
 *   2. add internal signals matching the macro's port shape:
 *        clk0, csb0, web0, wmask0, addr0, din0, dout0,
 *        clk1, csb1,                addr1, dout1
 *   3. add a binstance referencing the macro module, port_connections
 *      mapping each pin to the new signal.
 *   4. emit driver expressions:
 *        clk0 = orig_write_clk
 *        csb0 = active-low(write_enable_predicate)
 *        web0 = active-low(write_enable_predicate)
 *        wmask0 = '1 (full byte mask, when wmask is present)
 *        addr0 = write_addr_expr
 *        din0  = write_data_expr
 *        clk1  = orig_read_clk
 *        csb1  = 0   (always selected; the read FF gates
 *                     the value visibly)
 *        addr1 = read_addr_expr
 *      All as continuous `BAssign` in a fresh BCombinational process.
 *   5. rewrite the original write-process body to drop the
 *      @mem_write call (the addr/data/we are now driving the macro).
 *   6. rewrite the original read-process: every `BSelect (BVar mem) _`
 *      becomes `BVar dout1`.
 *   7. drop the BArray signal and remove the bmem from m.mems.
 *
 * The new module is structurally identical to the original up to
 * timing / address routing.  Caveat: OpenRAM's behavioural model
 * registers din/addr internally before storing, so the write path has
 * one cycle of latency vs the bit-blasted form.  For RAMs whose RTL
 * already used a registered-write pattern this is a wash; for
 * unregistered writes the user sees a one-cycle delay.  We document
 * this and surface a warning until the resolver produces a write-
 * forwarding wrapper. *)

open Behavioral_ir

(* ──────────── helpers ──────────── *)

let bits_needed n = if n <= 1 then 1 else
  let rec loop b m = if m >= n then b else loop (b + 1) (m * 2) in loop 1 2

(* Walk a stmt list collecting every @mem_write site for [m_name],
 * each tagged with its accumulated path-predicate (the conjunction
 * of `if`/`case` guards leading to it).  An empty path means the
 * site executes unconditionally on every clock edge. *)
let one_bit = BInt { width = 1; signed = Unsigned }

let bool_and a b = BBinOp { op = BAnd; lhs = a; rhs = b; result_type = one_bit }
let bool_or  a b = BBinOp { op = BOr;  lhs = a; rhs = b; result_type = one_bit }
let bool_not a   = BUnOp  { op = BNot; operand = a;     result_type = one_bit }
let bool_eq  a b = BBinOp { op = BEq; lhs = a; rhs = b; result_type = one_bit }

let cond_and p_opt c = match p_opt with
  | None -> Some c
  | Some prev -> Some (bool_and prev c)

(* Returns list of (path_pred_opt, addr_expr, data_expr) in source
 * order; path_pred_opt = None when the site is unconditional. *)
let collect_write_sites m_name body =
  let acc = ref [] in
  let rec walk path = function
    | BCallStmt { func = "@mem_write"; args = [BVar n; addr; data] }
      when n = m_name ->
        acc := (path, addr, data) :: !acc
    | BIf { condition; then_stmts; else_stmts } ->
        let pt = cond_and path condition in
        let pe = cond_and path (bool_not condition) in
        List.iter (walk pt) then_stmts;
        List.iter (walk pe) else_stmts
    | BCase { selector; cases; default } ->
        let arm_preds =
          List.map (fun (k, ss) ->
            let pred = cond_and path (bool_eq selector k) in
            List.iter (walk pred) ss;
            bool_eq selector k
          ) cases
        in
        (* default fires when no key matches *)
        let none_match =
          match arm_preds with
          | [] -> BConst { value = Z.one; width = 1 }
          | first :: rest ->
              List.fold_left (fun acc p -> bool_and acc (bool_not p))
                (bool_not first) rest
        in
        List.iter (walk (cond_and path none_match)) default
    | BBlock ss -> List.iter (walk path) ss
    | BWhile { body; _ } -> List.iter (walk path) body
    | BFor   { body; _ } -> List.iter (walk path) body
    | _ -> ()
  in
  List.iter (walk None) body;
  List.rev !acc

(* Fold a list of write-sites into a single (write_enable, addr, data)
 * triple using last-write-wins priority semantics.  For multiple
 * sequential writes to the same memory in source order, SystemVerilog
 * non-blocking semantics resolve to the LAST write (by source
 * position).  We therefore process sites in reverse order, building
 * a chain of BCond that lets earlier sites be overridden. *)
let fold_write_sites sites =
  match sites with
  | [] -> None
  | (None, addr, data) :: rest ->
      (* unconditional first-listed; later sites can override only if
       * later-listed.  But the unconditional path predicate = 1 means
       * later mutex branches will only fire when their guard does. *)
      let init_we = BConst { value = Z.one; width = 1 } in
      let init_addr = addr and init_data = data in
      let we, addr, data = List.fold_left (fun (we, a, d) (p, na, nd) ->
        let cond = match p with
          | None -> BConst { value = Z.one; width = 1 }
          | Some p -> p in
        bool_or we cond,
        BCond { condition = cond; then_val = na; else_val = a },
        BCond { condition = cond; then_val = nd; else_val = d }
      ) (init_we, init_addr, init_data) rest in
      Some (we, addr, data)
  | (Some p0, addr0, data0) :: rest ->
      let we, addr, data = List.fold_left (fun (we, a, d) (p, na, nd) ->
        let cond = match p with
          | None -> BConst { value = Z.one; width = 1 }
          | Some p -> p in
        bool_or we cond,
        BCond { condition = cond; then_val = na; else_val = a },
        BCond { condition = cond; then_val = nd; else_val = d }
      ) (p0, addr0, data0) rest in
      Some (we, addr, data)

(* Same shape for reads: every BSelect site in expression position
 * contributes one (path_pred, addr) entry.  At the macro pin we drive
 * a priority mux of the addresses; in the original BIR every BSelect
 * gets replaced by [BVar dout].  The dout is a single value so the
 * substitution is uniform — only the address pin needs the mux. *)
let collect_read_sites m_name body =
  let acc = ref [] in
  let rec expr path = function
    | BSelect { array = BVar n; index } when n = m_name ->
        acc := (path, index) :: !acc;
        expr path index
    | BBinOp { lhs; rhs; _ } -> expr path lhs; expr path rhs
    | BUnOp { operand; _ } -> expr path operand
    | BSlice { signal; _ } -> expr path signal
    | BSelect { array; index } -> expr path array; expr path index
    | BConcat es -> List.iter (expr path) es
    | BReplicate { value; _ } -> expr path value
    | BCond { condition; then_val; else_val } ->
        expr path condition;
        expr (cond_and path condition) then_val;
        expr (cond_and path (bool_not condition)) else_val
    | BCall { args; _ } -> List.iter (expr path) args
    | _ -> ()
  in
  let rec stmt path = function
    | BAssign { rhs; _ } -> expr path rhs
    | BIf { condition; then_stmts; else_stmts } ->
        expr path condition;
        List.iter (stmt (cond_and path condition)) then_stmts;
        List.iter (stmt (cond_and path (bool_not condition))) else_stmts
    | BCase { selector; cases; default } ->
        expr path selector;
        let arm_preds = List.map (fun (k, ss) ->
          List.iter (stmt (cond_and path (bool_eq selector k))) ss;
          bool_eq selector k) cases in
        let none =
          match arm_preds with
          | [] -> BConst { value = Z.one; width = 1 }
          | first :: rest ->
              List.fold_left (fun acc p -> bool_and acc (bool_not p))
                (bool_not first) rest in
        List.iter (stmt (cond_and path none)) default
    | BBlock ss -> List.iter (stmt path) ss
    | BCallStmt { args; _ } -> List.iter (expr path) args
    | BWhile { body; _ } -> List.iter (stmt path) body
    | BFor   { body; _ } -> List.iter (stmt path) body
    | _ -> ()
  in
  List.iter (stmt None) body;
  List.rev !acc

let fold_read_sites sites =
  match sites with
  | [] -> None
  | (_, addr0) :: rest ->
      Some (List.fold_left (fun acc (p, na) ->
        let cond = match p with
          | None -> BConst { value = Z.one; width = 1 }
          | Some p -> p in
        BCond { condition = cond; then_val = na; else_val = acc }
      ) addr0 rest)

(* Make signal of the named width as Internal wire. *)
let mk_signal name width =
  { name; stype = BInt { width; signed = Unsigned };
    direction = `Internal; initial_value = None; attrs = [] }

(* Map a Mem_macro_resolve port_shape into an unsigned-zero literal of
 * the right width — used for csb1 = 0, etc. *)
let zero_lit width =
  BConst { value = Z.zero; width }

let one_lit width =
  BConst { value = Z.sub (Z.shift_left Z.one width) Z.one; width }

(* Find the writing process for [m_name] (a BSequential containing an
 * @mem_write of m_name); idem for the reading process (any process
 * containing a BSelect of m_name).  Returns the indices in the
 * original `processes` list for in-place rewriting. *)
let find_writing_process m_name processes =
  let rec body_writes = function
    | [] -> false
    | BCallStmt { func = "@mem_write"; args = (BVar n) :: _ } :: _
      when n = m_name -> true
    | BIf { then_stmts; else_stmts; _ } :: tl ->
        body_writes then_stmts || body_writes else_stmts || body_writes tl
    | BCase { cases; default; _ } :: tl ->
        List.exists (fun (_, ss) -> body_writes ss) cases
        || body_writes default || body_writes tl
    | BBlock ss :: tl -> body_writes ss || body_writes tl
    | _ :: tl -> body_writes tl
  in
  let idx = ref None in
  List.iteri (fun i p ->
    match p with
    | BSequential s when body_writes s.body && !idx = None -> idx := Some i
    | _ -> ()
  ) processes;
  !idx

let find_reading_process m_name processes =
  let rec stmt_reads = function
    | BAssign { rhs; _ } -> expr_reads rhs
    | BIf { condition; then_stmts; else_stmts } ->
        expr_reads condition
        || List.exists stmt_reads then_stmts
        || List.exists stmt_reads else_stmts
    | BCase { selector; cases; default } ->
        expr_reads selector
        || List.exists (fun (_, ss) -> List.exists stmt_reads ss) cases
        || List.exists stmt_reads default
    | BBlock ss -> List.exists stmt_reads ss
    | BCallStmt { args; _ } -> List.exists expr_reads args
    | _ -> false
  and expr_reads = function
    | BSelect { array = BVar n; _ } when n = m_name -> true
    | BBinOp { lhs; rhs; _ } -> expr_reads lhs || expr_reads rhs
    | BUnOp { operand; _ } -> expr_reads operand
    | BSlice { signal; _ } -> expr_reads signal
    | BSelect { array; index } -> expr_reads array || expr_reads index
    | BConcat es -> List.exists expr_reads es
    | BReplicate { value; _ } -> expr_reads value
    | BCond { condition; then_val; else_val } ->
        expr_reads condition || expr_reads then_val || expr_reads else_val
    | BCall { args; _ } -> List.exists expr_reads args
    | _ -> false
  in
  let idx = ref None in
  List.iteri (fun i p ->
    let body = match p with
      | BCombinational c -> c.body
      | BSequential s -> s.body
    in
    if List.exists stmt_reads body && !idx = None then idx := Some i
  ) processes;
  !idx

(* Rewrite a stmt list to remove the @mem_write of m_name. *)
let rec strip_mem_writes m_name = function
  | [] -> []
  | BCallStmt { func = "@mem_write"; args = (BVar n) :: _ } :: rest
    when n = m_name -> strip_mem_writes m_name rest
  | BIf { condition; then_stmts; else_stmts } :: rest ->
      let t = strip_mem_writes m_name then_stmts in
      let e = strip_mem_writes m_name else_stmts in
      if t = [] && e = [] then strip_mem_writes m_name rest
      else BIf { condition; then_stmts = t; else_stmts = e } ::
           strip_mem_writes m_name rest
  | BCase { selector; cases; default } :: rest ->
      let cs = List.map (fun (k, ss) -> (k, strip_mem_writes m_name ss)) cases in
      let d = strip_mem_writes m_name default in
      BCase { selector; cases = cs; default = d } :: strip_mem_writes m_name rest
  | BBlock ss :: rest ->
      BBlock (strip_mem_writes m_name ss) :: strip_mem_writes m_name rest
  | s :: rest -> s :: strip_mem_writes m_name rest

(* Replace `BSelect (BVar m_name) _` with `BVar dout` in expressions
 * and statements. *)
let rec rewrite_reads_e m_name dout = function
  | BSelect { array = BVar n; _ } when n = m_name -> BVar dout
  | BBinOp { op; lhs; rhs; result_type } ->
      BBinOp { op;
               lhs = rewrite_reads_e m_name dout lhs;
               rhs = rewrite_reads_e m_name dout rhs;
               result_type }
  | BUnOp { op; operand; result_type } ->
      BUnOp { op;
              operand = rewrite_reads_e m_name dout operand;
              result_type }
  | BSlice { signal; msb; lsb } ->
      BSlice { signal = rewrite_reads_e m_name dout signal; msb; lsb }
  | BSelect { array; index } ->
      BSelect { array = rewrite_reads_e m_name dout array;
                index = rewrite_reads_e m_name dout index }
  | BConcat es -> BConcat (List.map (rewrite_reads_e m_name dout) es)
  | BReplicate { count; value } ->
      BReplicate { count; value = rewrite_reads_e m_name dout value }
  | BCond { condition; then_val; else_val } ->
      BCond { condition = rewrite_reads_e m_name dout condition;
              then_val = rewrite_reads_e m_name dout then_val;
              else_val = rewrite_reads_e m_name dout else_val }
  | BCall { func; args } ->
      BCall { func; args = List.map (rewrite_reads_e m_name dout) args }
  | other -> other

let rec rewrite_reads_s m_name dout = function
  | BAssign { lhs; rhs } ->
      BAssign { lhs; rhs = rewrite_reads_e m_name dout rhs }
  | BIf { condition; then_stmts; else_stmts } ->
      BIf { condition = rewrite_reads_e m_name dout condition;
            then_stmts = List.map (rewrite_reads_s m_name dout) then_stmts;
            else_stmts = List.map (rewrite_reads_s m_name dout) else_stmts }
  | BCase { selector; cases; default } ->
      BCase { selector = rewrite_reads_e m_name dout selector;
              cases = List.map (fun (k, ss) ->
                (rewrite_reads_e m_name dout k,
                 List.map (rewrite_reads_s m_name dout) ss)) cases;
              default = List.map (rewrite_reads_s m_name dout) default }
  | BBlock ss -> BBlock (List.map (rewrite_reads_s m_name dout) ss)
  | BCallStmt { func; args } ->
      BCallStmt { func; args = List.map (rewrite_reads_e m_name dout) args }
  | BWhile { condition; body } ->
      BWhile { condition = rewrite_reads_e m_name dout condition;
               body = List.map (rewrite_reads_s m_name dout) body }
  | BFor { init; condition; update; body } ->
      BFor { init = rewrite_reads_s m_name dout init;
             condition = rewrite_reads_e m_name dout condition;
             update = rewrite_reads_s m_name dout update;
             body = List.map (rewrite_reads_s m_name dout) body }
  | BReturn e -> BReturn (Option.map (rewrite_reads_e m_name dout) e)

(* ──────────── RAM32M (1W + 2R async) ──────────── *)

(* Replace `BSelect (BVar m_name) idx` with `BVar pin_for_idx`, where
 * `pin_for_idx` is chosen by syntactic equality of the index expression
 * against the keys in `addr_to_pin` (a small list of (addr_bexpr, pin)
 * pairs, typically 2 entries for cpuregs).  Other reads of m_name keep
 * the existing single-pin behaviour via `fallback`.  *)
let rec rewrite_reads_e_per_addr m_name (addr_to_pin : (bexpr * string) list) ~fallback = function
  | BSelect { array = BVar n; index } when n = m_name ->
      (match List.find_opt (fun (a, _) -> a = index) addr_to_pin with
       | Some (_, pin) -> BVar pin
       | None -> BVar fallback)
  | BBinOp { op; lhs; rhs; result_type } ->
      BBinOp { op
             ; lhs = rewrite_reads_e_per_addr m_name addr_to_pin ~fallback lhs
             ; rhs = rewrite_reads_e_per_addr m_name addr_to_pin ~fallback rhs
             ; result_type }
  | BUnOp { op; operand; result_type } ->
      BUnOp { op; result_type
            ; operand = rewrite_reads_e_per_addr m_name addr_to_pin ~fallback operand }
  | BSlice { signal; msb; lsb } ->
      BSlice { signal = rewrite_reads_e_per_addr m_name addr_to_pin ~fallback signal; msb; lsb }
  | BSelect { array; index } ->
      BSelect { array = rewrite_reads_e_per_addr m_name addr_to_pin ~fallback array
              ; index = rewrite_reads_e_per_addr m_name addr_to_pin ~fallback index }
  | BConcat es ->
      BConcat (List.map (rewrite_reads_e_per_addr m_name addr_to_pin ~fallback) es)
  | BReplicate { count; value } ->
      BReplicate { count; value = rewrite_reads_e_per_addr m_name addr_to_pin ~fallback value }
  | BCond { condition; then_val; else_val } ->
      BCond { condition = rewrite_reads_e_per_addr m_name addr_to_pin ~fallback condition
            ; then_val  = rewrite_reads_e_per_addr m_name addr_to_pin ~fallback then_val
            ; else_val  = rewrite_reads_e_per_addr m_name addr_to_pin ~fallback else_val }
  | BCall { func; args } ->
      BCall { func
            ; args = List.map (rewrite_reads_e_per_addr m_name addr_to_pin ~fallback) args }
  | other -> other

let rec rewrite_reads_s_per_addr m_name addr_to_pin ~fallback = function
  | BAssign { lhs; rhs } ->
      BAssign { lhs; rhs = rewrite_reads_e_per_addr m_name addr_to_pin ~fallback rhs }
  | BIf { condition; then_stmts; else_stmts } ->
      BIf { condition = rewrite_reads_e_per_addr m_name addr_to_pin ~fallback condition
          ; then_stmts = List.map (rewrite_reads_s_per_addr m_name addr_to_pin ~fallback) then_stmts
          ; else_stmts = List.map (rewrite_reads_s_per_addr m_name addr_to_pin ~fallback) else_stmts }
  | BCase { selector; cases; default } ->
      BCase { selector = rewrite_reads_e_per_addr m_name addr_to_pin ~fallback selector
            ; cases = List.map (fun (k, ss) ->
                rewrite_reads_e_per_addr m_name addr_to_pin ~fallback k,
                List.map (rewrite_reads_s_per_addr m_name addr_to_pin ~fallback) ss) cases
            ; default = List.map (rewrite_reads_s_per_addr m_name addr_to_pin ~fallback) default }
  | BBlock ss -> BBlock (List.map (rewrite_reads_s_per_addr m_name addr_to_pin ~fallback) ss)
  | BCallStmt { func; args } ->
      BCallStmt { func; args = List.map (rewrite_reads_e_per_addr m_name addr_to_pin ~fallback) args }
  | BWhile { condition; body } ->
      BWhile { condition = rewrite_reads_e_per_addr m_name addr_to_pin ~fallback condition
             ; body = List.map (rewrite_reads_s_per_addr m_name addr_to_pin ~fallback) body }
  | BFor { init; condition; update; body } ->
      BFor { init = rewrite_reads_s_per_addr m_name addr_to_pin ~fallback init
           ; condition = rewrite_reads_e_per_addr m_name addr_to_pin ~fallback condition
           ; update = rewrite_reads_s_per_addr m_name addr_to_pin ~fallback update
           ; body = List.map (rewrite_reads_s_per_addr m_name addr_to_pin ~fallback) body }
  | BReturn e -> BReturn (Option.map (rewrite_reads_e_per_addr m_name addr_to_pin ~fallback) e)

(* RAM32M lowering for 1W + 2R async-read memories with depth ≤ 32.
 * Emits ceil(width/2) RAM32M instances in quad-port mode:
 *   - port A : rs1 read (ADDRA = rs1_addr when WE=0; write_addr when WE=1)
 *   - port B : rs2 read (likewise with rs2_addr)
 *   - port C : unused (ADDRC tied to write_addr-or-zero so writes still go through)
 *   - port D : write only (ADDRD = write_addr)
 * When WE=1, all 4 ADDR* point at write_addr and all 4 DI* are tied to
 * the write data, so the four internal LUTs stay in sync.  When WE=0,
 * ports A/B/C each read their independent address.                    *)
(* Parameterised 1W+2R distributed-RAM lowering.
   RAM32M: ~prim:"RAM32M" ~pb:2 ~depth_cap:32 ~addr_w:5  (6 read bits per RAM)
   RAM64M: ~prim:"RAM64M" ~pb:1 ~depth_cap:64 ~addr_w:6  (3 read bits per RAM)
   Vivado uses exactly this: 2 read copies × ceil(width / (3*pb)) RAMs. *)
let ram_m_lower_dual_port ~prim ~pb ~depth_cap ~addr_w ~m ~(mm : bmem) ~mname ~skip =
  let writer = find_writing_process mname m.processes in
  let reader = find_reading_process mname m.processes in
  match writer, reader with
  | None, _ -> skip "RAM32M: no writing process"
  | _, None -> skip "RAM32M: no reading process"
  | Some w_idx, Some r_idx ->
    let w_proc = List.nth m.processes w_idx in
    let w_body = match w_proc with
      | BSequential s -> s.body | _ -> [] in
    let w_sites = collect_write_sites mname w_body in
    (* The two read ports of a 1W+2R memory are frequently in SEPARATE assigns
       (`assign rd0 = mem[ra0]; assign rd1 = mem[ra1];`), i.e. different
       processes.  find_reading_process only returns the FIRST reader, so
       collecting read sites from that one process finds a single address and the
       RAM32M path bails ("expected 2 distinct read addresses, got 1").  Gather
       read sites from EVERY process (writer has none) so both read ports show. *)
    let r_sites =
      List.concat_map (fun p ->
        let b = match p with BSequential s -> s.body | BCombinational c -> c.body in
        collect_read_sites mname b) m.processes in
    (* Distinct read addresses, in order of first appearance. *)
    let distinct_addrs =
      List.fold_left (fun acc (_, addr) ->
        if List.exists (fun a -> a = addr) acc then acc
        else acc @ [addr]) [] r_sites
    in
    (match fold_write_sites w_sites, distinct_addrs with
     | None, _ -> skip "RAM32M: no write sites"
     | _, ([] | [_]) ->
       skip (Printf.sprintf "RAM32M: expected 2 distinct read addresses, got %d"
               (List.length distinct_addrs))
     | Some (we_expr, w_addr, w_data), addr_a :: addr_b :: _ ->
       let dw = mm.data_width in
       (* Read bits per RAM: 3 read ports (A/B/C), each [pb] bits.
          RAM32M: pb=2 → 6/RAM;  RAM64M: pb=1 → 3/RAM. *)
       let ram_bits = 3 * pb in
       let orig_clk = match w_proc with BSequential s -> s.clock | _ -> "clk" in
       (* Lowering-private pins must not collide with source-level read-result nets. *)
       let rs1_pin = mname ^ "__memlower_rs1" in
       let rs2_pin = mname ^ "__memlower_rs2" in
       let read_sig nm =
         { name = nm
         ; stype = BInt { width = dw; signed = Unsigned }
         ; direction = `Internal
         ; initial_value = None
         ; attrs = [] }
       in
       let addr_to_pin = [ (addr_a, rs1_pin); (addr_b, rs2_pin) ] in
       let rewrite_e = rewrite_reads_e_per_addr mname addr_to_pin ~fallback:rs1_pin in
       let we_expr = rewrite_e we_expr in
       let w_addr  = rewrite_e w_addr in
       let w_data  = rewrite_e w_data in
       let addr_a  = rewrite_e addr_a in
       let addr_b  = rewrite_e addr_b in
       let padA e =
         let aw = bits_needed mm.depth in
         if aw >= addr_w then e
         else BConcat [ BConst { value = Z.zero; width = addr_w - aw }; e ]
       in
       let w_addr5 = padA w_addr in
       let rs1_a5  = padA addr_a in
       let rs2_a5  = padA addr_b in
       (* SDP-style: port D writes (ADDRD = write_addr, DID = data), ports A/B/C
          read (ADDRA/B/C = read_addr).  Each RAM provides [ram_bits] read bits
          per copy (DOA+DOB+DOC); for 1W+2R we duplicate the storage, one copy
          per read port.  n_per_copy = ceil(width / ram_bits). *)
       let n_per_copy = (dw + ram_bits - 1) / ram_bits in
       (* INIT for one port, MSB-first 64-bit string: [pb] bits per address,
          entry [addr] at bit positions [pb*addr .. pb*addr+pb-1]. *)
       let init_for_lut lo_bit =
         let bits = Bytes.make 64 '0' in
         List.iteri (fun addr v ->
           if addr < depth_cap then
             for j = 0 to pb - 1 do
               let b = if lo_bit + j < dw then (v lsr (lo_bit + j)) land 1 else 0 in
               let pos = 63 - (pb * addr + j) in
               if pos >= 0 then Bytes.set bits pos (Char.chr (Char.code '0' + b))
             done) mm.init_values;
         Bytes.to_string bits
       in
       (* Slice a [pb]-bit chunk from write data, zero-padding past width. *)
       let di_slice lo =
         let hi = lo + pb - 1 in
         if hi < dw then BSlice { signal = w_data; msb = hi; lsb = lo }
         else if lo < dw then
           BConcat [ BConst { value = Z.zero; width = hi - (dw - 1) }
                   ; BSlice { signal = w_data; msb = dw - 1; lsb = lo } ]
         else BConst { value = Z.zero; width = pb }
       in
       (* Build one copy's RAM instances + per-instance read output names.
          read_addr5 = the address feeding ADDRA/B/C of this copy. *)
       let build_copy ~tag ~read_addr5 =
         let outs = List.init n_per_copy (fun k ->
           ( Printf.sprintf "%s_%s_a%d" mname tag k
           , Printf.sprintf "%s_%s_b%d" mname tag k
           , Printf.sprintf "%s_%s_c%d" mname tag k )) in
         let mk_sig nm =
           { name = nm; stype = BInt { width = pb; signed = Unsigned }
           ; direction = `Internal; initial_value = None; attrs = [] } in
         let new_sigs =
           List.concat_map (fun (a, b, c) -> [ mk_sig a; mk_sig b; mk_sig c ]) outs in
         let dod_stubs = List.init n_per_copy (fun k ->
           Printf.sprintf "%s_%s_d%d" mname tag k) in
         let dod_sigs = List.map mk_sig dod_stubs in
         let insts = List.mapi (fun k (a_out, b_out, c_out) ->
           (* covers bits [ram_bits*k .. +ram_bits-1] via ports A/B/C ([pb] each);
              port D's DID gets the next [pb] bits (padding — DOD unused here). *)
           let lo = ram_bits * k in
           let dod_name = List.nth dod_stubs k in
           { inst_name = Printf.sprintf "%s_%s_%s_%d" mname tag (String.lowercase_ascii prim) k
           ; module_name = prim
           ; param_values = []
           ; param_strs =
               [ ("INIT_A", init_for_lut lo)
               ; ("INIT_B", init_for_lut (lo + pb))
               ; ("INIT_C", init_for_lut (lo + 2 * pb))
               ; ("INIT_D", init_for_lut (lo + 3 * pb)) ]
           ; port_connections =
               [ ("ADDRA", read_addr5); ("ADDRB", read_addr5)
               ; ("ADDRC", read_addr5); ("ADDRD", w_addr5)
               ; ("DIA", di_slice lo);        ("DIB", di_slice (lo + pb))
               ; ("DIC", di_slice (lo + 2 * pb)); ("DID", di_slice (lo + 3 * pb))
               ; ("WCLK", BVar orig_clk);    ("WE", we_expr)
               ; ("DOA", BVar a_out); ("DOB", BVar b_out)
               ; ("DOC", BVar c_out); ("DOD", BVar dod_name) ]
           }) outs in
         (insts, new_sigs @ dod_sigs, outs)
       in
       let rs1_insts, rs1_sigs, rs1_outs = build_copy ~tag:"r1" ~read_addr5:rs1_a5 in
       let rs2_insts, rs2_sigs, rs2_outs = build_copy ~tag:"r2" ~read_addr5:rs2_a5 in
       (* Read assembly:
          rs1_pin <= {dw-bit slice of (DOC|DOB|DOA across all copy-1 RAMs)} *)
       let assemble outs =
         (* per-RAM: 6 bits = {DOC, DOB, DOA} (MSB first).  Concat all the
            per-RAM 6-bit chunks MSB-first across k.                       *)
         let chunks = List.map (fun (a, b, c) ->
           BConcat [ BVar c; BVar b; BVar a ]) outs in
         let cat = BConcat (List.rev chunks) in
         if dw mod ram_bits = 0 then cat
         else BSlice { signal = cat; msb = dw - 1; lsb = 0 }
       in
       let driver =
         BCombinational
           { name = mname ^ "_drv"; sensitivity = [ BAny ]
           ; body =
               [ BAssign { lhs = rs1_pin; rhs = assemble rs1_outs }
               ; BAssign { lhs = rs2_pin; rhs = assemble rs2_outs } ] }
       in
       let new_sigs = [ read_sig rs1_pin; read_sig rs2_pin ] @ rs1_sigs @ rs2_sigs in
       let rewrite_body body =
         List.map (rewrite_reads_s_per_addr mname addr_to_pin ~fallback:rs1_pin)
           (strip_mem_writes mname body)
       in
       let processes' =
         List.mapi (fun i p ->
           let do_strip = i = w_idx || i = r_idx in
           match p with
           | BSequential s ->
             BSequential
               { s with body =
                   (if do_strip then rewrite_body s.body
                    else List.map (rewrite_reads_s_per_addr mname addr_to_pin
                                     ~fallback:rs1_pin) s.body) }
           | BCombinational c ->
             BCombinational
               { c with body =
                   (if do_strip then rewrite_body c.body
                    else List.map (rewrite_reads_s_per_addr mname addr_to_pin
                                     ~fallback:rs1_pin) c.body) })
           m.processes
       in
       let instances' =
         List.map (fun (i : binstance) ->
           { i with port_connections =
               List.map (fun (p, e) ->
                 (p, rewrite_reads_e_per_addr mname addr_to_pin ~fallback:rs1_pin e))
                 i.port_connections })
           m.instances
       in
       let funcs' =
         List.map (fun (f : bfunc) ->
           { f with body = List.map (rewrite_reads_s_per_addr mname addr_to_pin
                                       ~fallback:rs1_pin) f.body })
           m.funcs
       in
       let signals' =
         List.filter (fun (s : bsignal) -> s.name <> mname) m.signals @ new_sigs
       in
       let m' =
         { m with
           signals = signals'
         ; processes = processes' @ [ driver ]
         ; instances = rs1_insts @ rs2_insts @ instances'
         ; funcs = funcs'
         ; mems = List.filter (fun mm' -> mm'.mname <> mname) m.mems }
       in
       Printf.eprintf
         "[memlower] %s.%s: FPGA 1W+2R async-read → %d × %s (SDP, %d per copy × 2 copies, depth=%d, width=%d)\n"
         m.name mname (2 * n_per_copy) prim n_per_copy mm.depth dw;
       `Lowered (m', None))

(* ──────────── true-dual-port (TDP) lowering ──────────── *)

(* Indices of the processes that WRITE mem (@mem_write of mname).  A genuine
   Xilinx TDP RAM (prim_generic_ram_2p) is written by two independent clocked
   processes — one per port — each of which also reads mem into its own
   registered rdata.  Each such process maps 1:1 onto a RAMB36E1 port. *)
let mem_writer_indices m_name processes =
  let rec writes = function
    | [] -> false
    | BCallStmt { func = "@mem_write"; args = (BVar n) :: _ } :: _ when n = m_name -> true
    | BIf { then_stmts; else_stmts; _ } :: tl ->
        writes then_stmts || writes else_stmts || writes tl
    | BCase { cases; default; _ } :: tl ->
        List.exists (fun (_, ss) -> writes ss) cases || writes default || writes tl
    | BBlock ss :: tl -> writes ss || writes tl
    | (BWhile { body; _ } | BFor { body; _ }) :: tl -> writes body || writes tl
    | _ :: tl -> writes tl
  in
  List.filter (fun i ->
    match List.nth processes i with BSequential s -> writes s.body | _ -> false)
    (List.init (List.length processes) (fun i -> i))

(* Build one Fpga_bram_resolve.ram_port from a single clocked process that
   both masked-byte-writes and registers a read of mem, all at that port's
   address.  [read_pin] is the net build_byte_lane_ram will drive with this
   port's read data; the process's mem-reads are rewritten to it.  Returns
   (port, registered-read-target option) or None if the process doesn't match
   the byte-masked read/write shape. *)
let build_tdp_port m_name ~read_pin ~dw (p : bprocess) =
  let body, clk = match p with
    | BSequential s -> s.body, s.clock
    | BCombinational c -> c.body, "clk" in
  let rw_e = rewrite_reads_e m_name read_pin in
  let w_sites = collect_write_sites m_name body in
  let r_sites = collect_read_sites  m_name body in
  match fold_read_sites r_sites with
  | None -> None
  | Some r_addr0 ->
    let r_addr = rw_e r_addr0 in
    (* Fold a constant integer expression (unroll leaves `i*8` as an
       un-simplified BMul of BConsts rather than a folded BConst). *)
    let rec ceval = function
      | BConst { value; _ } -> Some (Z.to_int value)
      | BBinOp { op = BMul; lhs; rhs; _ } ->
        (match ceval lhs, ceval rhs with Some a, Some b -> Some (a * b) | _ -> None)
      | BBinOp { op = BAdd; lhs; rhs; _ } ->
        (match ceval lhs, ceval rhs with Some a, Some b -> Some (a + b) | _ -> None)
      | BBinOp { op = BShl; lhs; rhs; _ } ->
        (match ceval lhs, ceval rhs with Some a, Some b -> Some (a lsl b) | _ -> None)
      | _ -> None
    in
    (* Recognise one byte-lane write's data as byte `k` of a common base.  Two
       shapes reach here: a clean [base[8k+7:8k]] slice (constant `[7:0]` style
       source), or the indexed part-select `base[i*8 +: 8]` which unroll+lower
       leave as [(base >> i*8) & 8'hFF].  Both mean "byte k <= base's byte k". *)
    let byte_of_data data =
      match data with
      | BSlice { signal; msb; lsb } when msb - lsb = 7 && lsb mod 8 = 0 ->
        Some (lsb / 8, signal)
      | BBinOp { op = BAnd; lhs = BBinOp { op = BShr; lhs = base; rhs = sh; _ }; rhs = mask; _ }
      | BBinOp { op = BAnd; lhs = mask; rhs = BBinOp { op = BShr; lhs = base; rhs = sh; _ }; _ } ->
        (match ceval sh, ceval mask with
         | Some s, Some m when s mod 8 = 0 && m = 0xFF -> Some (s / 8, base)
         | _ -> None)
      | _ -> None
    in
    (* every write site must be byte k of a COMMON base at a COMMON address
       (the sb/sh/sw byte-mask pattern). *)
    let byte_info =
      match w_sites with
      | [] -> None
      | (_, a0, _) :: _ ->
        let parsed = List.map (fun (pred, a, data) ->
          match byte_of_data data with
          | Some (k, base) when a = a0 ->
            Some (k, (match pred with Some pr -> pr | None -> one_lit 1), base)
          | _ -> None) w_sites in
        if List.for_all Option.is_some parsed then begin
          let bytes = List.map Option.get parsed in
          let base = let _, _, s = List.hd bytes in s in
          if List.for_all (fun (_, _, s) -> s = base) bytes then Some (a0, bytes, base)
          else None
        end else None
    in
    let read_reg =
      let rec find_uncond = function
        | [] -> None
        | BAssign { lhs; rhs = BSelect { array = BVar n; _ } } :: _ when n = m_name -> Some lhs
        | BBlock b :: rest -> (match find_uncond b with Some r -> Some r | None -> find_uncond rest)
        | _ :: rest -> find_uncond rest
      in find_uncond body
    in
    let port =
      match byte_info with
      | Some (a0, bytes, base) ->
        let w_addr = rw_e a0 and base = rw_e base in
        let nbytes = dw / 8 in
        let strobe b = match List.find_opt (fun (bl, _, _) -> bl = b) bytes with
          | Some (_, pr, _) -> rw_e pr | None -> zero_lit 1 in
        let wstrb = BConcat (List.rev (List.init nbytes strobe)) in
        let we_any = List.fold_left (fun acc (_, pr, _) -> bool_or acc (rw_e pr)) (zero_lit 1) bytes in
        Fpga_bram_resolve.(
          { p_clk = BVar clk
          ; p_addr = BCond { condition = we_any; then_val = w_addr; else_val = r_addr }
          ; p_we = we_any; p_wdata = base; p_wstrb = Some wstrb })
      | None ->
        (* read-only port: no writes, address = read address. *)
        Fpga_bram_resolve.(
          { p_clk = BVar clk; p_addr = r_addr
          ; p_we = zero_lit 1; p_wdata = zero_lit dw; p_wstrb = None })
    in
    Some (port, read_reg)

(* ──────────── per-memory lowering ──────────── *)

(* Try to lower a single bmem. Returns either:
 *   `Lowered (m', art) - module with this mem replaced; bmem removed
 *   `Skipped reason    - keep the bmem as bit-blast (with logged reason) *)
let try_lower_one_mem ~tech (m : bmodule) (mm : bmem) =
  let mname = mm.mname in
  let skip reason =
    Printf.eprintf "[memlower] %s.%s: %s — keeping bit-blast\n"
      m.name mname reason;
    `Skipped
  in
  let async_ok = Sys.getenv_opt "MEMLOWER_ASYNC_OK" = Some "1" in
  let fpga = Sys.getenv_opt "MEMLOWER_FPGA" = Some "1" in
  (* FPGA ROM path: a sync-read BRom (case-statement lookup, e.g. progmem)
     -> an INIT-initialised read-only BRAM (write enable tied 0).  No
     writer required. *)
  if fpga && mm.kind = BRom && mm.read_is_sync && mm.init_values <> [] then begin
    let dw = mm.data_width in
    if mm.depth > 32768 then skip "ROM depth>32K (deep tiling TODO)"
    else
      match find_reading_process mname m.processes with
      | None -> skip "ROM: no reading process"
      | Some r_idx ->
        let r_proc = List.nth m.processes r_idx in
        let r_body =
          match r_proc with BSequential s -> s.body | BCombinational c -> c.body
        in
        (match fold_read_sites (collect_read_sites mname r_body) with
         | None -> skip "ROM: no read site"
         | Some r_addr0 ->
           let read_pin = mname ^ "_rdata_a" in
           let rec find_uncond = function
             | [] -> None
             | BAssign { lhs; rhs = BSelect { array = BVar n; _ } } :: _
               when n = mname -> Some lhs
             | BBlock b :: rest ->
               (match find_uncond b with Some r -> Some r | None -> find_uncond rest)
             | _ :: rest -> find_uncond rest
           in
           let read_reg = match r_proc with BSequential _ -> find_uncond r_body | _ -> None in
           let rw_e = rewrite_reads_e mname read_pin in
           let r_addr = rw_e r_addr0 in
           let read_clk = match r_proc with BSequential s -> s.clock | _ -> "clk" in
           let port =
             Fpga_bram_resolve.(
               { p_clk = BVar read_clk; p_addr = r_addr; p_we = zero_lit 1;
                 p_wdata = zero_lit dw; p_wstrb = None })
           in
           let absorb = Option.is_some read_reg in
           let insts, new_sigs, drv_stmts, _ =
             Fpga_bram_resolve.build_byte_lane_ram ~name:mname ~depth:mm.depth ~width:dw
               ~init:(Array.of_list mm.init_values) ~ports:[ port ] ()
           in
           let absorb_assign =
             match read_reg with
             | Some rd when absorb -> [ BAssign { lhs = rd; rhs = BVar read_pin } ]
             | _ -> []
           in
           let driver =
             BCombinational
               { name = mname ^ "_drv"; sensitivity = [ BAny ]
               ; body = drv_stmts @ absorb_assign }
           in
           let processes' =
             List.mapi (fun i p ->
               let body = match p with BSequential s -> s.body | BCombinational c -> c.body in
               let body = List.map (rewrite_reads_s mname read_pin) body in
               let body =
                 if i = r_idx then
                   match read_reg with
                   | Some rd when absorb ->
                     let rec drop bb =
                       List.filter_map (function
                         | BAssign { lhs; rhs = BVar n } when lhs = rd && n = read_pin -> None
                         | BBlock x -> Some (BBlock (drop x))
                         | s -> Some s) bb
                     in
                     drop body
                   | _ -> body
                 else body
               in
               match p with
               | BSequential s -> BSequential { s with body }
               | BCombinational c -> BCombinational { c with body })
               m.processes
           in
           let instances' =
             List.map (fun (i : binstance) ->
               { i with port_connections =
                   List.map (fun (p, e) -> (p, rewrite_reads_e mname read_pin e))
                     i.port_connections })
               m.instances
           in
           let funcs' =
             List.map (fun (f : bfunc) ->
               { f with body = List.map (rewrite_reads_s mname read_pin) f.body })
               m.funcs
           in
           let signals' =
             List.filter (fun (s : bsignal) -> s.name <> mname) m.signals @ new_sigs
           in
           let m' =
             { m with signals = signals'; processes = processes' @ [ driver ]
             ; instances = insts @ instances'; funcs = funcs'
             ; mems = List.filter (fun mm' -> mm'.mname <> mname) m.mems }
           in
           `Lowered (m', None))
  end
  else if mm.kind <> BRam then `Skipped  (* non-FPGA ROM: deferred to phase 4 *)
  (* async-read fork.  OpenRAM's behavioural model is sync-only (1-cycle
     read latency), so an async-read source can't drop into OpenRAM
     without changing semantics.  Three branches:
       - FPGA  → distributed RAM (LUTRAM, native async on Xilinx).
                 Current implementation: bit-blast fallback (functional,
                 just larger than DRAM); RAM32M/RAM32X1D primitive
                 emission is TODO once we settle on the address-port
                 count (picorv32 cpuregs needs 2-read+1-write).
       - ASIC + MEMLOWER_ASYNC_OK=1 → fall through to OpenRAM mapping,
                 accepting the 1-cycle read latency change.
       - default (ASIC, async_ok=0) → keep bit-blast (FFs+muxes). *)
  else if not mm.read_is_sync && fpga then begin
    (* FPGA async-read → distributed RAM (LUTRAM).
       Implemented variants:
         - RAM*X1S    : depth ≤ 256, 1W + 1R single-port (read addr = write addr)
         - RAM32M     : depth ≤  32, 1W + 2R quad-port (picorv32 cpuregs)
       For *X1S: one instance per data bit, INIT[b] sliced from init_values.
       For RAM32M: ceil(width/2) instances in quad-port mode — port A holds
       the rs1 view, B holds rs2, D drives writes; ADDR* mux toggles the
       3 ports between write-broadcast (WE=1) and per-port read (WE=0). *)
    if mm.n_write_ports = 1 && mm.n_read_ports = 2 && mm.depth <= 32 then
      ram_m_lower_dual_port ~prim:"RAM32M" ~pb:2 ~depth_cap:32 ~addr_w:5
        ~m ~mm ~mname ~skip
    else if mm.n_write_ports = 1 && mm.n_read_ports = 2 && mm.depth <= 64 then
      (* depth 33-64 1W+2R → RAM64M (3 read bits/RAM), matching Vivado's
         `<width> bits × 2 reads → 2 × ceil(width/3) RAM64M` for the SGMII
         rx_elastic_buffer and similar delay-line/FIFO memories. *)
      ram_m_lower_dual_port ~prim:"RAM64M" ~pb:1 ~depth_cap:64 ~addr_w:6
        ~m ~mm ~mname ~skip
    else if mm.n_write_ports <> 1 || mm.n_read_ports <> 1 then
      skip (Printf.sprintf
              "FPGA async-read needs 1W+1R or 1W+2R (2R depth<=64); got %dW+%dR (depth %d)"
              mm.n_write_ports mm.n_read_ports mm.depth)
    else if mm.depth > 256 then
      skip (Printf.sprintf "FPGA async-read depth %d > 256 (deeper LUTRAM tiling TODO)" mm.depth)
    else begin
      let writer = find_writing_process mname m.processes in
      let reader = find_reading_process mname m.processes in
      match writer, reader with
      | None, _ -> skip "no writing process"
      | _, None -> skip "no reading process"
      | Some w_idx, Some r_idx ->
        let w_proc = List.nth m.processes w_idx in
        let r_proc = List.nth m.processes r_idx in
        let w_body = match w_proc with
          | BSequential s -> s.body | _ -> [] in
        let r_body = match r_proc with
          | BSequential s -> s.body
          | BCombinational c -> c.body in
        let w_sites = collect_write_sites mname w_body in
        let r_sites = collect_read_sites  mname r_body in
        (match fold_write_sites w_sites, fold_read_sites r_sites with
         | None, _ -> skip "no write sites collected"
         | _, None -> skip "no read sites collected"
         | Some (we_expr, w_addr, w_data), Some r_addr ->
           (* 1W+1R with a read address DISTINCT from the write address needs
              the DUAL-port variants (write+SPO at A*, async read at
              DPRA*→DPO).  The X1S single-port form silently READ AT THE
              WRITE ADDRESS — framing's rx_length_axis (write nextbuf, read
              core_lsu_addr_dly[7:3]) returned the wrong buffer's length
              (found by the Vivado↔SVS cross-flow miter). *)
           let dual = not (w_addr = r_addr) in
           let prim, prim_depth, addr_w =
             if dual then
               (if      mm.depth <=  32 then "RAM32X1D",  32, 5
                else if mm.depth <=  64 then "RAM64X1D",  64, 6
                else                         "RAM128X1D", 128, 7)
             else
               (if      mm.depth <=  32 then "RAM32X1S",  32, 5
                else if mm.depth <=  64 then "RAM64X1S",  64, 6
                else if mm.depth <= 128 then "RAM128X1S", 128, 7
                else                         "RAM256X1S", 256, 8)
           in
           if dual && mm.depth > 128 then
             skip (Printf.sprintf
               "FPGA async-read 1W+1R distinct-address depth %d > 128 (no RAM256X1D)"
               mm.depth)
           else
           let dw = mm.data_width in
           let orig_clk = match w_proc with BSequential s -> s.clock | _ -> "clk" in
           let read_pin = mname ^ "_dout" in
           let read_sig =
             { name = read_pin
             ; stype = BInt { width = dw; signed = Unsigned }
             ; direction = `Internal
             ; initial_value = None
             ; attrs = [] }
           in
           let rw_e = rewrite_reads_e mname read_pin in
           let we_expr = rw_e we_expr in
           let w_addr  = rw_e w_addr in
           let w_data  = rw_e w_data in
           let r_addr  = rw_e r_addr in
           (* single-port: one address pin services read and write;
              dual-port: DPRA* carries the independent read address *)
           let pad_to_addr_w e =
             (* Pad/truncate the BIR address expression to addr_w bits.
                bexpr widths are implicit in the suite, so we just feed it
                in via a BConcat with zero-extension where needed. *)
             let zext_const = BConst { value = Z.zero; width = max 1 (addr_w - bits_needed mm.depth) } in
             if bits_needed mm.depth >= addr_w then e
             else BConcat [ zext_const; e ]
           in
           let a_expr = pad_to_addr_w w_addr in
           let dp_expr = pad_to_addr_w r_addr in
           (* Per-bit INIT: bit i of init[addr] -> position addr of INIT[i].
              yosys-JSON expects MSB-first binary string, so address 0 is
              at the rightmost (LSB) position. *)
           let init_for_bit b =
             let buf = Bytes.make prim_depth '0' in
             List.iteri (fun addr v ->
               if addr < prim_depth then
                 let bit = if (v lsr b) land 1 = 1 then '1' else '0' in
                 Bytes.set buf (prim_depth - 1 - addr) bit)
               mm.init_values;
             Bytes.to_string buf
           in
           let per_bit_outs =
             List.init dw (fun b -> Printf.sprintf "%s_o_%d" mname b)
           in
           let new_sigs = read_sig ::
             List.map (fun nm ->
               { name = nm; stype = BInt { width = 1; signed = Unsigned }
               ; direction = `Internal; initial_value = None; attrs = [] })
               per_bit_outs
           in
           let instances = List.mapi (fun b out_name ->
             { inst_name    = Printf.sprintf "%s_b%d" mname b
             ; module_name  = prim
             ; param_values = []
             ; param_strs   =
                 (* plain binary digits, no `%d'b` prefix — see RAM32M note *)
                 [ ("INIT", init_for_bit b) ]
             ; port_connections =
                 (* RAM32X1S/D and RAM64X1S/D expose their addresses as
                    individual 1-bit pins A0../DPRA0.. (that IS the Xilinx
                    primitive interface); the bus form fails strict readers
                    (Vivado read_edif) and the primitive-port check.  RAM128X1D
                    is the exception: its actual primitive interface is a 7-bit
                    BUS A[6:0]/DPRA[6:0], so scalar pins there would not match
                    the unisim VHDL interface (the port-direction resolver then
                    drops the cell).  Emit per-primitive.  Dual-port reads land
                    on DPO (SPO = write-port view, unused). *)
                 ("D",    BSlice { signal = w_data; msb = b; lsb = b })
                 :: (if prim = "RAM128X1D" then
                       ("A", a_expr) :: (if dual then [ ("DPRA", dp_expr) ] else [])
                     else
                       List.init addr_w (fun i ->
                         (Printf.sprintf "A%d" i, BSlice { signal = a_expr; msb = i; lsb = i }))
                       @ (if dual then
                            List.init addr_w (fun i ->
                              (Printf.sprintf "DPRA%d" i,
                               BSlice { signal = dp_expr; msb = i; lsb = i }))
                          else []))
                 @ [ ("WE",   we_expr)
                   ; ("WCLK", BVar orig_clk)
                   ; ((if dual then "DPO" else "O"), BVar out_name) ]
             }) per_bit_outs
           in
           (* Driver concat: read_pin <= {o_(dw-1), ..., o_1, o_0}.       *)
           let driver_stmts =
             [ BAssign
                 { lhs = read_pin
                 ; rhs = BConcat (List.rev_map (fun n -> BVar n) per_bit_outs) } ]
           in
           let driver =
             BCombinational
               { name = mname ^ "_drv"; sensitivity = [ BAny ]
               ; body = driver_stmts }
           in
           let rewrite_body body =
             List.map (rewrite_reads_s mname read_pin) (strip_mem_writes mname body)
           in
           let processes' =
             List.mapi (fun i p ->
               let do_strip = i = w_idx || i = r_idx in
               match p with
               | BSequential s ->
                 BSequential
                   { s with body =
                       (if do_strip then rewrite_body s.body
                        else List.map (rewrite_reads_s mname read_pin) s.body) }
               | BCombinational c ->
                 BCombinational
                   { c with body =
                       (if do_strip then rewrite_body c.body
                        else List.map (rewrite_reads_s mname read_pin) c.body) })
               m.processes
           in
           let instances' =
             List.map (fun (i : binstance) ->
               { i with port_connections =
                   List.map (fun (p, e) -> (p, rewrite_reads_e mname read_pin e))
                     i.port_connections })
               m.instances
           in
           let funcs' =
             List.map (fun (f : bfunc) ->
               { f with body = List.map (rewrite_reads_s mname read_pin) f.body })
               m.funcs
           in
           let signals' =
             List.filter (fun (s : bsignal) -> s.name <> mname) m.signals @ new_sigs
           in
           let m' =
             { m with
               signals = signals'
             ; processes = processes' @ [ driver ]
             ; instances = instances @ instances'
             ; funcs = funcs'
             ; mems = List.filter (fun mm' -> mm'.mname <> mname) m.mems }
           in
           Printf.eprintf
             "[memlower] %s.%s: FPGA async-read → %d × %s (depth=%d, width=%d)\n"
             m.name mname dw prim mm.depth dw;
           `Lowered (m', None))
    end
  end
  else if not mm.read_is_sync && not async_ok then begin
    Printf.eprintf
      "[memlower] %s.%s: ASIC async-read — keeping bit-blast.  OpenRAM's \
       behavioural model is sync-only and would add 1 cycle of read \
       latency vs source.  Set MEMLOWER_ASYNC_OK=1 to map anyway and \
       accept the timing change.\n"
      m.name mname;
    `Skipped
  end
  else if fpga && mm.read_is_sync
          && List.length (mem_writer_indices mname m.processes) = 2 then begin
    (* ── FPGA true-dual-port (TDP) path ──────────────────────────────
       prim_generic_ram_2p: two independent clocked processes, each doing a
       masked byte-write and a registered read at that port's address.  Map
       each process onto one RAMB36E1 port (A/B), both able to read AND write
       simultaneously — the native Xilinx TDP mode.  Kept separate from the
       single-writer path below so that path (picosoc/eth) is untouched. *)
    let dw = mm.data_width in
    if mm.depth > Fpga_bram_resolve.prim_capacity Fpga_bram_resolve.RAMB36E1 / 8 then
      skip (Printf.sprintf
        "TDP byte-write depth %d > %d (deep x1-tiling TODO)" mm.depth
        (Fpga_bram_resolve.prim_capacity Fpga_bram_resolve.RAMB36E1 / 8))
    else begin
      let widx = mem_writer_indices mname m.processes in
      let ia = List.nth widx 0 and ib = List.nth widx 1 in
      let pa_proc = List.nth m.processes ia and pb_proc = List.nth m.processes ib in
      let read_pin_a = mname ^ "_rdata_a" and read_pin_b = mname ^ "_rdata_b" in
      match build_tdp_port mname ~read_pin:read_pin_a ~dw pa_proc,
            build_tdp_port mname ~read_pin:read_pin_b ~dw pb_proc with
      | Some (port_a, rreg_a), Some (port_b, rreg_b) ->
        let insts, new_sigs, drv_stmts, _rdata_names =
          Fpga_bram_resolve.build_byte_lane_ram ~name:mname ~depth:mm.depth ~width:dw
            ?init:(if mm.init_values <> [] then Some (Array.of_list mm.init_values) else None)
            ~ports:[ port_a; port_b ] ()
        in
        (* rdata net names are read_pin_a / read_pin_b by construction. *)
        let absorb rreg pin =
          match rreg with Some rd -> [ BAssign { lhs = rd; rhs = BVar pin } ] | None -> [] in
        let driver =
          BCombinational
            { name = mname ^ "_drv"; sensitivity = [ BAny ]
            ; body = drv_stmts @ absorb rreg_a read_pin_a @ absorb rreg_b read_pin_b }
        in
        let rewrite_port_body pin rreg body =
          let body = List.map (rewrite_reads_s mname pin) (strip_mem_writes mname body) in
          match rreg with
          | Some rd ->
            let rec drop bb =
              List.filter_map (function
                | BAssign { lhs; rhs = BVar n } when lhs = rd && n = pin -> None
                | BBlock b -> Some (BBlock (drop b))
                | s -> Some s) bb
            in drop body
          | None -> body
        in
        let processes' =
          List.mapi (fun i p ->
            match p with
            | BSequential s when i = ia ->
              BSequential { s with body = rewrite_port_body read_pin_a rreg_a s.body }
            | BSequential s when i = ib ->
              BSequential { s with body = rewrite_port_body read_pin_b rreg_b s.body }
            | BSequential s ->
              BSequential { s with body = List.map (rewrite_reads_s mname read_pin_a) s.body }
            | BCombinational c ->
              BCombinational { c with body = List.map (rewrite_reads_s mname read_pin_a) c.body })
            m.processes
        in
        let instances' =
          List.map (fun (i : binstance) ->
            { i with port_connections =
                List.map (fun (pn, e) -> (pn, rewrite_reads_e mname read_pin_a e))
                  i.port_connections })
            m.instances
        in
        let funcs' =
          List.map (fun (f : bfunc) ->
            { f with body = List.map (rewrite_reads_s mname read_pin_a) f.body })
            m.funcs
        in
        let signals' =
          List.filter (fun (s : bsignal) -> s.name <> mname) m.signals @ new_sigs in
        let m' =
          { m with signals = signals'; processes = processes' @ [ driver ]
          ; instances = insts @ instances'; funcs = funcs'
          ; mems = List.filter (fun mm' -> mm'.mname <> mname) m.mems }
        in
        Printf.eprintf
          "[memlower] %s.%s: TDP -> %d RAMB tile(s) (2 ports, depth=%d width=%d)\n"
          m.name mname (List.length insts) mm.depth dw;
        `Lowered (m', None)
      | _ -> skip "TDP: a port process did not match the byte-masked read/write shape"
    end
  end
  else if
    mm.n_write_ports < 1
    || (mm.n_write_ports > 1 && Sys.getenv_opt "MEMLOWER_FPGA" <> Some "1")
  then
    skip
      (Printf.sprintf "w_ports=%d (1 in v2; FPGA allows byte-masked multi-write)"
         mm.n_write_ports)
  else if mm.n_read_ports < 1 || mm.n_read_ports > 2 then
    skip (Printf.sprintf "r_ports=%d (only 1–2 supported in v2)" mm.n_read_ports)
  else
    let writer = find_writing_process mname m.processes in
    let reader = find_reading_process mname m.processes in
    match writer, reader with
    | None, _ -> skip "no writing process"
    | _, None -> skip "no reading process"
    | Some w_idx, Some r_idx ->
        let w_proc = List.nth m.processes w_idx in
        let r_proc = List.nth m.processes r_idx in
        let w_body = match w_proc with
          | BSequential s -> s.body | _ -> [] in
        let r_body = match r_proc with
          | BSequential s -> s.body
          | BCombinational c -> c.body in
        let w_sites = collect_write_sites mname w_body in
        let r_sites = collect_read_sites  mname r_body in
        (match fold_write_sites w_sites, fold_read_sites r_sites with
         | None, _ -> skip "no write sites collected"
         | _, None -> skip "no read sites collected"
         | Some (we_expr, w_addr, w_data), Some r_addr ->
             let aw = bits_needed mm.depth in
             let dw = mm.data_width in
             if Sys.getenv_opt "MEMLOWER_FPGA" = Some "1" then begin
               (* ── FPGA path: byte-lane RAMB18E1 (Fpga_bram_resolve) ──
                  A single true-dual-port port serves read+write: each
                  cycle addr = we ? w_addr : r_addr (degenerates to a wire
                  when w_addr ≡ r_addr — the CPU case).  No OpenRAM macro,
                  no stub module; RAMB18E1 is a nextpnr primitive. *)
               ignore aw;
               let read_pin = mname ^ "_rdata_a" in
               (* find a top-level registered read [rd <= mem[addr]] in the
                  sequential read process — its FF is redundant once the
                  BRAM's own output register provides that cycle. *)
               let rec find_unconditional_read = function
                 | [] -> None
                 | BAssign { lhs; rhs = BSelect { array = BVar n; _ } } :: _
                   when n = mname -> Some lhs
                 | BBlock b :: rest ->
                   (match find_unconditional_read b with
                    | Some r -> Some r
                    | None -> find_unconditional_read rest)
                 | _ :: rest -> find_unconditional_read rest
               in
               let read_reg =
                 match r_proc with
                 | BSequential _ -> find_unconditional_read r_body
                 | _ -> None
               in
               let rw_e = rewrite_reads_e mname read_pin in
               let r_addr = rw_e r_addr in
               let orig_clk = match w_proc with BSequential s -> s.clock | _ -> "clk" in
               let init =
                 if mm.init_values <> [] then Some (Array.of_list mm.init_values) else None
               in
               (* byte-write pattern: every write site is a byte-aligned 8-bit
                  slice of a common base at a common address -> per-byte write
                  strobes (sb/sh/sw write only their bytes, no read-modify-write). *)
               let byte_info =
                 match w_sites with
                 | [] | [ _ ] -> None
                 | (_, a0, _) :: _ ->
                   let parsed =
                     List.map (fun (pred, a, data) ->
                       match data with
                       | BSlice { signal; msb; lsb }
                         when a = a0 && msb - lsb = 7 && lsb mod 8 = 0 ->
                         Some
                           ( lsb / 8
                           , (match pred with Some p -> p | None -> one_lit 1)
                           , signal )
                       | _ -> None) w_sites
                   in
                   if List.for_all Option.is_some parsed then begin
                     let bytes = List.map Option.get parsed in
                     let base = let _, _, s = List.hd bytes in s in
                     if List.for_all (fun (_, _, s) -> s = base) bytes then
                       Some (a0, bytes, base)
                     else None
                   end
                   else None
               in
               let too_deep =
                 match byte_info with Some _ -> mm.depth > 4096 | None -> mm.depth > 32768
               in
               if too_deep then skip "FPGA BRAM depth exceeds one tile (deep tiling TODO)"
               else begin
                 ignore aw;
                 let port, w_addr_used =
                   match byte_info with
                   | Some (a0, bytes, base) ->
                     let w_addr = rw_e a0 and base = rw_e base in
                     let nbytes = dw / 8 in
                     let strobe b =
                       match List.find_opt (fun (bl, _, _) -> bl = b) bytes with
                       | Some (_, p, _) -> rw_e p
                       | None -> zero_lit 1
                     in
                     let wstrb = BConcat (List.rev (List.init nbytes strobe)) in
                     let we_any =
                       List.fold_left (fun acc (_, p, _) -> bool_or acc (rw_e p))
                         (zero_lit 1) bytes
                     in
                     ( Fpga_bram_resolve.(
                         { p_clk = BVar orig_clk
                         ; p_addr =
                             BCond { condition = we_any; then_val = w_addr; else_val = r_addr }
                         ; p_we = we_any; p_wdata = base; p_wstrb = Some wstrb })
                     , w_addr )
                   | None ->
                     let w_addr = rw_e w_addr and w_data = rw_e w_data
                     and we_expr = rw_e we_expr in
                     ( Fpga_bram_resolve.(
                         { p_clk = BVar orig_clk
                         ; p_addr =
                             BCond { condition = we_expr; then_val = w_addr; else_val = r_addr }
                         ; p_we = we_expr; p_wdata = w_data; p_wstrb = None })
                     , w_addr )
                 in
                 let absorb = Option.is_some read_reg && w_addr_used = r_addr in
                 let insts, new_sigs, drv_stmts, _ =
                   Fpga_bram_resolve.build_byte_lane_ram ~name:mname ~depth:mm.depth
                     ~width:dw ?init ~ports:[ port ] ()
                 in
                 let absorb_assign =
                   match read_reg with
                   | Some rd when absorb -> [ BAssign { lhs = rd; rhs = BVar read_pin } ]
                   | _ -> []
                 in
                 let driver =
                   BCombinational
                     { name = mname ^ "_drv"; sensitivity = [ BAny ]
                     ; body = drv_stmts @ absorb_assign }
                 in
                 let rewrite_body body =
                   List.map (rewrite_reads_s mname read_pin) (strip_mem_writes mname body)
                 in
                 let processes' =
                   List.mapi (fun i p ->
                     let do_strip = i = w_idx || i = r_idx in
                     match p with
                     | BSequential s ->
                       BSequential
                         { s with body =
                             (if do_strip then rewrite_body s.body
                              else List.map (rewrite_reads_s mname read_pin) s.body) }
                     | BCombinational c ->
                       BCombinational
                         { c with body =
                             (if do_strip then rewrite_body c.body
                              else List.map (rewrite_reads_s mname read_pin) c.body) })
                     m.processes
                 in
                 (* drop the now-redundant [rd <= read_pin] from the read
                    process; rd is driven combinationally by [driver]. *)
                 let processes' =
                   match read_reg with
                   | Some rd when absorb ->
                     List.mapi (fun i p ->
                       if i <> r_idx then p
                       else begin
                         let rec drop body =
                           List.filter_map (function
                             | BAssign { lhs; rhs = BVar n }
                               when lhs = rd && n = read_pin -> None
                             | BBlock b -> Some (BBlock (drop b))
                             | s -> Some s) body
                         in
                         match p with
                         | BSequential s -> BSequential { s with body = drop s.body }
                         | BCombinational c -> BCombinational { c with body = drop c.body }
                       end)
                       processes'
                   | _ -> processes'
                 in
                 let instances' =
                   List.map (fun (i : binstance) ->
                     { i with port_connections =
                         List.map (fun (p, e) -> (p, rewrite_reads_e mname read_pin e))
                           i.port_connections })
                     m.instances
                 in
                 let funcs' =
                   List.map (fun (f : bfunc) ->
                     { f with body = List.map (rewrite_reads_s mname read_pin) f.body })
                     m.funcs
                 in
                 let signals' =
                   List.filter (fun (s : bsignal) -> s.name <> mname) m.signals @ new_sigs
                 in
                 let m' =
                   { m with
                     signals = signals'
                   ; processes = processes' @ [ driver ]
                   ; instances = insts @ instances'
                   ; funcs = funcs'
                   ; mems = List.filter (fun mm' -> mm'.mname <> mname) m.mems }
                 in
                 `Lowered (m', None)
               end
             end
             else begin
             (* Even when the source has a separate read site that
                always fires (rdata <= mem[r_addr]) and a write site
                guarded by we_expr, a 1RW cell can serve both: each
                cycle is exactly one operation.  Mux the address with
                we_expr, hold ce_in asserted, and let the cell either
                read or write that address per cycle.

                When r_addr is structurally identical to w_addr (the
                picosoc_mem case where both use the same `addr` bus)
                the mux degenerates to a wire — same end result.  For
                designs where the source genuinely needs simultaneous
                read+write at different addresses (true dual-port
                register files), set MEM_REQUIRE_DUAL_PORT=1 to fall
                back to the 1RW1R request shape and require a
                dual-port macro.                                    *)
             let force_dual_port =
               Sys.getenv_opt "MEM_REQUIRE_DUAL_PORT" = Some "1" in
             let kind =
               if force_dual_port
               then Mem_macro_resolve.Sram { n_rw = 1; n_r = 1; n_w = 0 }
               else Mem_macro_resolve.Sram { n_rw = 1; n_r = 0; n_w = 0 } in
             let req = Mem_macro_resolve.{
               tech;
               kind;
               word_size = dw;
               num_words = mm.depth;
             } in
             (* OpenRAM is sometimes unable to generate a particular
                shape (e.g. odd word_size, depth not a power of 2,
                tech-specific bitcell limits).  Fall back to bit-blast
                rather than crashing — matches the spirit of memlower
                being best-effort. *)
             match
               try Some (Mem_macro_resolve.resolve req)
               with e ->
                 Printf.eprintf
                   "[memlower] %s.%s: macro resolve failed (%s) — \
                    keeping bit-blast for this memory\n"
                   m.name mname (Printexc.to_string e);
                 None
             with
             | None -> `Skipped
             | Some art ->
             let ps = art.port_shape in
             (* Suffix all macro pin names with the memory name so
                multi-mem lowering doesn't collide. *)
             let suffix s = s ^ "_" ^ mname in
             let new_sigs = List.concat [
               List.map (fun n -> mk_signal (suffix n) 1) ps.clk;
               List.map (fun n -> mk_signal (suffix n) 1) ps.csb;
               List.filter_map (fun o ->
                 Option.map (fun n -> mk_signal (suffix n) 1) o) ps.web;
               List.filter_map (fun o ->
                 Option.map (fun n -> mk_signal (suffix n) (max 1 (dw / 8))) o) ps.wmask;
               List.map (fun n -> mk_signal (suffix n) aw) ps.addr;
               List.filter_map (fun o ->
                 Option.map (fun n -> mk_signal (suffix n) dw) o) ps.din;
               List.map (fun n -> mk_signal (suffix n) dw) ps.dout;
             ] in
             let pc = List.concat [
               List.map (fun n -> (n, BVar (suffix n))) ps.clk;
               List.map (fun n -> (n, BVar (suffix n))) ps.csb;
               List.filter_map (fun o ->
                 Option.map (fun n -> (n, BVar (suffix n))) o) ps.web;
               List.filter_map (fun o ->
                 Option.map (fun n -> (n, BVar (suffix n))) o) ps.wmask;
               List.map (fun n -> (n, BVar (suffix n))) ps.addr;
               List.filter_map (fun o ->
                 Option.map (fun n -> (n, BVar (suffix n))) o) ps.din;
               List.map (fun n -> (n, BVar (suffix n))) ps.dout;
             ] in
             let inst = {
               inst_name = mname ^ "_macro";
               module_name = ps.module_name;
               param_values = []; param_strs = [];
               port_connections = pc;
             } in
             let clk0   = suffix (List.nth ps.clk 0) in
             let csb0   = suffix (List.nth ps.csb 0) in
             let web0   = suffix (match List.nth ps.web 0 with
                                  | Some n -> n | None -> "web0") in
             let addr0  = suffix (List.nth ps.addr 0) in
             let din0   = suffix (match List.nth ps.din 0 with
                                  | Some n -> n | None -> "din0") in
             let dout0  = suffix (List.nth ps.dout 0) in
             (* read_pin = which macro dout pin replaces every read of
                this memory.  Hoisted above driver_body construction so
                we can pre-rewrite w_data / w_addr / r_addr — the byte-
                merge pass embeds `mem[addr]` reads in w_data for the
                read-modify-write of non-written byte lanes, and those
                need the same BVar dout substitution as the original
                processes' reads.                                       *)
             let read_pin =
               if force_dual_port
               then suffix (List.nth ps.dout 1)
               else dout0
             in
             let orig_clk = match w_proc with
               | BSequential s -> s.clock | _ -> "clk" in
             let read_clk = match r_proc with
               | BSequential s -> s.clock | _ -> "clk" in
             ignore read_clk;   (* unused in 1RW mode; only port 0 clk *)
             (* Address pin: if dual-port, port 0 gets w_addr, port 1
                r_addr.  If 1RW, single port gets we ? w_addr : r_addr
                (degenerates to a wire when w_addr ≡ r_addr).      *)
             (* The byte-write merge in behavioral_mem_merge injects
                BSelect(BVar mname, addr) reads into [w_data] (read-
                modify-write of non-written byte lanes).  And both
                addresses are user expressions that *could* index the
                same memory in pathological designs.  Rewrite all three
                so any surviving `mem[…]` becomes `BVar dout` — without
                this, hardcaml ties them to 32-bit zero and the macro's
                din pin sees garbage.                                  *)
             let rw_e = rewrite_reads_e mname read_pin in
             let w_addr = rw_e w_addr in
             let r_addr = rw_e r_addr in
             let w_data = rw_e w_data in
             let we_expr = rw_e we_expr in
             let inv_we = bool_not we_expr in
             let muxed_addr =
               BCond { condition = we_expr;
                       then_val = w_addr;
                       else_val = r_addr } in
             let driver_body = List.concat [
               [ BAssign { lhs = clk0; rhs = BVar orig_clk };
                 (* csb0 is active-low chip-select.  In 1RW we keep it
                    asserted (=0) every cycle so the cell either reads
                    or writes per `we_in`.  In dual-port we asserted
                    only on writes (since reads use port 1).         *)
                 BAssign { lhs = csb0;
                           rhs = if force_dual_port then inv_we
                                 else zero_lit 1 };
                 BAssign { lhs = web0; rhs = inv_we };
                 BAssign { lhs = addr0;
                           rhs = if force_dual_port then w_addr
                                 else muxed_addr };
                 BAssign { lhs = din0;  rhs = w_data };
               ];
               (match List.nth ps.wmask 0 with
                | Some wm ->
                    [ BAssign { lhs = suffix wm;
                                rhs = one_lit (max 1 (dw / 8)) } ]
                | None -> []);
               (if not force_dual_port then [] else
                let clk1  = suffix (List.nth ps.clk 1) in
                let csb1  = suffix (List.nth ps.csb 1) in
                let addr1 = suffix (List.nth ps.addr 1) in
                [ BAssign { lhs = clk1; rhs = BVar read_clk };
                  BAssign { lhs = csb1; rhs = zero_lit 1 };
                  BAssign { lhs = addr1; rhs = r_addr };
                ]);
             ] in
             let driver = BCombinational {
               name = mname ^ "_drv";
               sensitivity = [BAny];
               body = driver_body;
             } in
             let rewrite_body body =
               let body = strip_mem_writes mname body in
               List.map (rewrite_reads_s mname read_pin) body
             in
             (* Rewrite every process — not just w_idx / r_idx — so
                stray BSelect (BVar mname) reads in other always
                blocks (e.g. peek paths, debug taps) get redirected
                to the macro pin too.  Without this, hier_synth's
                expr_to_signal hits "unbound identifier mem" on
                whatever survives and ties it to a 32-bit zero,
                silently breaking those paths.  w_idx / r_idx still
                get strip_mem_writes; everything else just gets
                read-rewriting (no @mem_write to strip).            *)
             let processes' =
               List.mapi (fun i p ->
                 let do_strip = (i = w_idx || i = r_idx) in
                 match p with
                 | BSequential s ->
                     let body =
                       if do_strip then rewrite_body s.body
                       else List.map (rewrite_reads_s mname read_pin) s.body in
                     BSequential { s with body }
                 | BCombinational c ->
                     let body =
                       if do_strip then rewrite_body c.body
                       else List.map (rewrite_reads_s mname read_pin) c.body in
                     BCombinational { c with body }
               ) m.processes in
             (* Same rewrite for instance port connections — a child
                might be wired to mem[index] and that BSelect needs
                to become BVar dout too.                            *)
             let instances' =
               List.map (fun (i : binstance) ->
                 { i with port_connections =
                     List.map (fun (p, e) ->
                       (p, rewrite_reads_e mname read_pin e))
                       i.port_connections }
               ) m.instances in
             (* And every function/task body in case [Behavioral_inline]
                left a function that closes over `mem`.              *)
             let funcs' =
               List.map (fun (f : bfunc) ->
                 { f with body =
                     List.map (rewrite_reads_s mname read_pin) f.body }
               ) m.funcs in
             let signals' =
               List.filter (fun (s : bsignal) -> s.name <> mname) m.signals
               @ new_sigs in
             let m' = {
               m with
               signals = signals';
               processes = processes' @ [driver];
               instances = inst :: instances';
               funcs = funcs';
               mems = List.filter (fun mm' -> mm'.mname <> mname) m.mems;
             } in
             `Lowered (m', Some art)
             end)

(* ──────────── per-module pass ──────────── *)

let lower_module ~tech (m : bmodule) =
  List.fold_left (fun (m, arts) mm ->
    match try_lower_one_mem ~tech m mm with
    | `Lowered (m', Some art) -> m', art :: arts
    | `Lowered (m', None) -> m', arts
    | `Skipped -> m, arts
  ) (m, []) m.mems

(* Build a tiny bmodule with the macro's IO so [hier_synth] recognises
 * the binstance.  Body is empty — the macro is a blackbox at our
 * synth boundary; ORFS picks up the real Verilog via the manifest. *)
let stub_bmodule_for_art (art : Mem_macro_resolve.artifacts) : bmodule =
  let ps = art.port_shape in
  let dw = ps.data_width in
  let aw = ps.addr_width in
  let mk dir w name : bsignal =
    { name; stype = BInt { width = w; signed = Unsigned };
      direction = dir; initial_value = None; attrs = [] } in
  let sigs = List.concat [
    List.map (mk `Input 1) ps.clk;
    List.map (mk `Input 1) ps.csb;
    List.filter_map (fun o -> Option.map (mk `Input 1) o) ps.web;
    List.filter_map (fun o ->
      Option.map (mk `Input (max 1 (dw / 8))) o) ps.wmask;
    List.map (mk `Input aw) ps.addr;
    List.filter_map (fun o -> Option.map (mk `Input dw) o) ps.din;
    List.map (mk `Output dw) ps.dout;
  ] in
  { name = ps.module_name;
    params = [];
    signals = sigs;
    processes = [];
    instances = [];
    funcs = [];
    mems = [];
    attrs = [("sv_decomp_blackbox", "1");
             ("sv_decomp_macro_v", art.verilog_path);
             ("sv_decomp_macro_lib", art.liberty_path);
             ("sv_decomp_macro_lef",
              match art.lef_path with Some p -> p | None -> "");
             ("sv_decomp_macro_gds",
              match art.gds_path with Some p -> p | None -> "")];
  }

let lower_program ?tech (p : bprogram) =
  let tech = match tech with
    | Some t -> t
    | None -> Mem_macro_resolve.default_tech () in
  let modules', arts =
    List.fold_left (fun (acc_m, acc_a) m ->
      let m', arts = lower_module ~tech m in
      (m' :: acc_m, arts @ acc_a)
    ) ([], []) p.modules in
  let stub_modules =
    let seen = Hashtbl.create 4 in
    List.filter_map (fun a ->
      let n = a.Mem_macro_resolve.module_name in
      if Hashtbl.mem seen n then None
      else begin Hashtbl.add seen n (); Some (stub_bmodule_for_art a) end
    ) arts
  in
  { p with modules = stub_modules @ List.rev modules' }, arts
