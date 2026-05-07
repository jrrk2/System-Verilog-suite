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

(* Take the single @mem_write site of m_name in [body] and return
 * (write_predicate, addr_expr, data_expr).  Predicate is the
 * conjunction of `if` conditions on the path to the site (typically
 * just one outer `if (we)`).  Returns None if the body doesn't match
 * v1's shape. *)
let extract_single_write m_name body =
  let rec walk path = function
    | BCallStmt { func = "@mem_write"; args = [BVar n; addr; data] }
      when n = m_name ->
        Some (path, addr, data)
    | BIf { condition; then_stmts; else_stmts = []; _ } ->
        first_in path condition then_stmts
    | BIf { condition = _; then_stmts = _; else_stmts = _; _ } ->
        None  (* if/else mux — out of v1 scope *)
    | BBlock ss -> first_in_list path ss
    | _ -> None
  and first_in path cond ss =
    let p =
      match path with
      | None -> Some cond
      | Some prev -> Some (BBinOp { op = BAnd; lhs = prev; rhs = cond;
                                    result_type = BInt { width = 1; signed = Unsigned } })
    in
    first_in_list p ss
  and first_in_list path = function
    | [] -> None
    | s :: rest ->
        (match walk path s with
         | Some _ as r -> r
         | None -> first_in_list path rest)
  in
  first_in_list None body

(* Find the single BSelect of m_name in [body].  Return Some (lhs, addr)
 * when the only read site is a [BAssign { lhs; rhs = BSelect (BVar m, addr) }]
 * inside the process.  Anything else returns None. *)
let extract_single_read m_name body =
  let hits = ref [] in
  let rec walk = function
    | BAssign { lhs; rhs = BSelect { array = BVar n; index } } when n = m_name ->
        hits := (lhs, index) :: !hits
    | BIf { then_stmts; else_stmts; _ } ->
        List.iter walk then_stmts; List.iter walk else_stmts
    | BCase { cases; default; _ } ->
        List.iter (fun (_, ss) -> List.iter walk ss) cases;
        List.iter walk default
    | BBlock ss -> List.iter walk ss
    | _ -> ()
  in
  List.iter walk body;
  match !hits with
  | [pair] -> Some pair
  | _ -> None

(* Make signal of the named width as Internal wire. *)
let mk_signal name width =
  { name; stype = BInt { width; signed = Unsigned };
    direction = `Internal; initial_value = None; attrs = [] }

(* Map a Mem_macro_resolve port_shape into an unsigned-zero literal of
 * the right width — used for csb1 = 0, etc. *)
let zero_lit width =
  BConst { value = 0; width }

let one_lit width =
  BConst { value = (1 lsl width) - 1; width }

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

(* ──────────── per-module pass ──────────── *)

let lower_module ~tech (m : bmodule) =
  match m.mems with
  | [] -> m, []
  | mm :: _ when List.length m.mems > 1 ->
      Printf.eprintf
        "[memlower] %s: %d memories, only handling first one — keeping rest as bit-blast\n"
        m.name (List.length m.mems);
      ignore mm; m, []
  | [mm] when mm.kind <> BRam ->
      m, []  (* ROM lowering deferred *)
  | [mm] when not mm.read_is_sync ->
      Printf.eprintf "[memlower] %s.%s: async read — keeping bit-blast\n"
        m.name mm.mname;
      m, []
  | [mm] when mm.n_write_ports <> 1 || mm.n_read_ports < 1 || mm.n_read_ports > 2 ->
      Printf.eprintf
        "[memlower] %s.%s: w_ports=%d, r_ports=%d outside v1 scope — keeping bit-blast\n"
        m.name mm.mname mm.n_write_ports mm.n_read_ports;
      m, []
  | [mm] ->
      let mname = mm.mname in
      let writer = find_writing_process mname m.processes in
      let reader = find_reading_process mname m.processes in
      (match writer, reader with
       | None, _ | _, None ->
           Printf.eprintf "[memlower] %s.%s: no writer or reader process found\n"
             m.name mname;
           m, []
       | Some w_idx, Some r_idx ->
           let w_proc = List.nth m.processes w_idx in
           let r_proc = List.nth m.processes r_idx in
           let w_body = match w_proc with
             | BSequential s -> s.body
             | _ -> []
           in
           let r_body = match r_proc with
             | BSequential s -> s.body
             | BCombinational c -> c.body
           in
           (match extract_single_write mname w_body,
                  extract_single_read  mname r_body with
            | None, _ | _, None ->
                Printf.eprintf
                  "[memlower] %s.%s: write/read pattern not single-site — bit-blast\n"
                  m.name mname;
                m, []
            | Some (we_pred, w_addr, w_data), Some (_r_lhs, r_addr) ->
                let aw = bits_needed mm.depth in
                let dw = mm.data_width in
                let req = Mem_macro_resolve.{
                  tech;
                  kind = Sram { n_rw = 1; n_r = 1; n_w = 0 };
                  word_size = dw;
                  num_words = mm.depth;
                } in
                let art = Mem_macro_resolve.resolve req in
                let ps = art.port_shape in
                (* Step 2: macro signals.  All Internal wires of the
                   right width.  Names come from port_shape. *)
                let new_sigs = List.concat [
                  List.map (fun n -> mk_signal n 1) ps.clk;
                  List.map (fun n -> mk_signal n 1) ps.csb;
                  List.filter_map (fun o ->
                    Option.map (fun n -> mk_signal n 1) o) ps.web;
                  List.filter_map (fun o ->
                    Option.map (fun n -> mk_signal n (max 1 (dw / 8))) o) ps.wmask;
                  List.map (fun n -> mk_signal n aw) ps.addr;
                  List.filter_map (fun o ->
                    Option.map (fun n -> mk_signal n dw) o) ps.din;
                  List.map (fun n -> mk_signal n dw) ps.dout;
                ] in
                (* Step 3: instance. *)
                let pc = List.concat [
                  List.map (fun n -> (n, BVar n)) ps.clk;
                  List.map (fun n -> (n, BVar n)) ps.csb;
                  List.filter_map (fun o ->
                    Option.map (fun n -> (n, BVar n)) o) ps.web;
                  List.filter_map (fun o ->
                    Option.map (fun n -> (n, BVar n)) o) ps.wmask;
                  List.map (fun n -> (n, BVar n)) ps.addr;
                  List.filter_map (fun o ->
                    Option.map (fun n -> (n, BVar n)) o) ps.din;
                  List.map (fun n -> (n, BVar n)) ps.dout;
                ] in
                let inst = {
                  inst_name = mname ^ "_macro";
                  module_name = ps.module_name;
                  param_values = [];
                  port_connections = pc;
                } in
                (* Step 4: driver process. *)
                let clk0 = List.nth ps.clk 0 in
                let csb0 = List.nth ps.csb 0 in
                let web0 = match List.nth ps.web 0 with
                  | Some n -> n | None -> "web0" in
                let addr0 = List.nth ps.addr 0 in
                let din0 = match List.nth ps.din 0 with
                  | Some n -> n | None -> "din0" in
                let clk1 = List.nth ps.clk 1 in
                let csb1 = List.nth ps.csb 1 in
                let addr1 = List.nth ps.addr 1 in
                let dout1 = List.nth ps.dout 1 in
                let inv_we = match we_pred with
                  | None -> zero_lit 1   (* always writing → web=0 *)
                  | Some p -> BUnOp { op = BNot; operand = p;
                                      result_type = BInt { width = 1; signed = Unsigned } }
                in
                let orig_clk = match w_proc with
                  | BSequential s -> s.clock | _ -> "clk" in
                let read_clk = match r_proc with
                  | BSequential s -> s.clock | _ -> "clk" in
                let driver_body = List.concat [
                  [ BAssign { lhs = clk0; rhs = BVar orig_clk };
                    BAssign { lhs = csb0; rhs =
                      (match we_pred with
                       | None -> zero_lit 1
                       | Some _ -> inv_we) };
                    BAssign { lhs = web0; rhs = inv_we };
                    BAssign { lhs = addr0; rhs = w_addr };
                    BAssign { lhs = din0;  rhs = w_data };
                  ];
                  (match List.nth ps.wmask 0 with
                   | Some wm ->
                       [ BAssign { lhs = wm; rhs = one_lit (max 1 (dw / 8)) } ]
                   | None -> []);
                  [ BAssign { lhs = clk1; rhs = BVar read_clk };
                    BAssign { lhs = csb1; rhs = zero_lit 1 };
                    BAssign { lhs = addr1; rhs = r_addr };
                  ];
                ] in
                let driver = BCombinational {
                  name = mname ^ "_drv";
                  sensitivity = [BAny];
                  body = driver_body;
                } in
                (* Step 5–6: rewrite original processes. *)
                let rewrite_body body =
                  let body = strip_mem_writes mname body in
                  List.map (rewrite_reads_s mname dout1) body
                in
                let processes' =
                  List.mapi (fun i p ->
                    if i = w_idx || i = r_idx then
                      match p with
                      | BSequential s ->
                          BSequential { s with body = rewrite_body s.body }
                      | BCombinational c ->
                          BCombinational { c with body = rewrite_body c.body }
                    else p
                  ) m.processes in
                (* Step 7: drop the BArray signal and the bmem record. *)
                let signals' =
                  List.filter (fun (s : bsignal) -> s.name <> mname) m.signals
                  @ new_sigs in
                let m' = {
                  m with
                  signals = signals';
                  processes = processes' @ [driver];
                  instances = inst :: m.instances;
                  mems = [];
                } in
                m', [art]))
  | _ -> m, []

let lower_program ?tech (p : bprogram) =
  let tech = match tech with
    | Some t -> t
    | None -> Mem_macro_resolve.default_tech () in
  let modules', arts =
    List.fold_left (fun (acc_m, acc_a) m ->
      let m', arts = lower_module ~tech m in
      (m' :: acc_m, arts @ acc_a)
    ) ([], []) p.modules in
  { p with modules = List.rev modules' }, arts
