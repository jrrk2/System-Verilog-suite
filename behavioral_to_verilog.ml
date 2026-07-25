(* Convert Behavioral IR to Verilog *)

open Behavioral_ir

(* Sanitize signal names for Verilog *)
let sanitize_name name =
  (* Preserve brackets only if they're at the end and look like array indexing: signal[N]
     Otherwise sanitize them *)
  let len = String.length name in
  if len > 2 && name.[len - 1] = ']' then
    (* Has closing bracket at end - check if it's valid array syntax *)
    try
      let open_bracket = String.rindex name '[' in
      let between = String.sub name (open_bracket + 1) (len - open_bracket - 2) in
      (* Check if the part between brackets is all digits *)
      let is_index = String.length between > 0 &&
        String.fold_left (fun acc c -> acc && (c >= '0' && c <= '9')) true between in
      if is_index && open_bracket > 0 then
        (* Valid array indexing - preserve it, but sanitize everything before the bracket *)
        let base = String.sub name 0 open_bracket in
        let sanitized_base = String.map (fun c -> if c = '@' then '_' else c) base in
        sanitized_base ^ "[" ^ between ^ "]"
      else
        (* Not valid array indexing - sanitize everything *)
        String.map (fun c -> if c = '@' || c = '[' || c = ']' then '_' else c) name
    with Not_found ->
      (* No opening bracket - sanitize *)
      String.map (fun c -> if c = '@' || c = '[' || c = ']' then '_' else c) name
  else
    (* No closing bracket at end - sanitize any brackets *)
    String.map (fun c -> if c = '@' || c = '[' || c = ']' then '_' else c) name

(* Generate Verilog type declarations *)
let rec verilog_of_type = function
  (* `logic [0:0]` rather than scalar `logic` so a 1-bit per-hart signal can be
     bit-indexed by the array/hart write pseudo-ops (`haltreq_o[selected_hart]`);
     behaves identically to a scalar in every other context. *)
  | BInt { width = 1; _ } -> "logic [0:0]"
  | BInt { width; _ } -> Printf.sprintf "logic [%d:0]" (width - 1)
  | BBool -> "logic"
  | BArray { element = BInt { width; _ }; size } ->
      (* PACKED 2-D with the array dimension FIRST, so `a[idx]` selects a
         `width`-bit element AND the whole `a` is a packed vector (needed for
         both `a[idx][..]` reads/`@mem_write` and wholesale `x = a` assigns).
         `logic [31:0][0:1]` would make `a[idx]` index the 32-bit dim instead.
         The outer dimension is DESCENDING ([size-1:0]) so a descending
         part-select `a[1:0]` (gpio's `{gp_i_q[1:0], gp_i}`) matches the
         declared direction — an ascending [0:size-1] made xelab reject the
         part-select (VRFC 10-1219, opposite direction). *)
      Printf.sprintf "logic [%d:0][%d:0]" (size - 1) (width - 1)
  | BArray { element; size } ->
      Printf.sprintf "%s [0:%d]" (verilog_of_type element) (size - 1)
  | BStruct { fields } ->
      (* A struct type must be scalarized before Verilog emit; one reaching here
         would emit a comment in place of the declaration's width, silently
         losing it.  Warn and emit a best-effort packed vector of the summed
         member width so the net is at least declared. *)
      let w = List.fold_left (fun a (_, ty) ->
        a + (match ty with BInt { width; _ } -> width | BBool -> 1 | _ -> 1)) 0 fields in
      lossage_warn "verilog:BStruct"
        (Printf.sprintf "un-scalarized struct type emitted as [%d:0] vector" (max 0 (w - 1)));
      Printf.sprintf "[%d:0]" (max 0 (w - 1))

(* Names of unpacked-array signals in the module being emitted (set per module
   by verilog_of_module); a safety net so a whole-array reset of an unpacked
   array emits `'{default:'0}` rather than the packed `'0`. *)
let current_unpacked : string list ref = ref []
(* Declared width of each signal in the module being emitted — used to detect a
   WIDENING size-cast `N'(v)` that was lowered to an out-of-range BSlice
   `v[N-1:0]` (Verilator tolerates it as zero-extend, Vivado rejects it). *)
let current_widths : (string, int) Hashtbl.t = Hashtbl.create 64

(* Generate binary operator *)
let verilog_of_binop = function
  | BAdd -> "+"
  | BSub -> "-"
  | BMul -> "*"
  | BDiv -> "/"
  | BMod -> "%"
  | BAnd -> "&"
  | BOr -> "|"
  | BXor -> "^"
  | BShl -> "<<"
  | BShr -> ">>"
  | BAshr -> ">>>"
  | BEq -> "=="
  | BNe -> "!="
  | BLt -> "<"
  | BLe -> "<="
  | BGt -> ">"
  | BGe -> ">="

(* Generate unary operator *)
let verilog_of_unop = function
  | BNot -> "~"
  | BNeg -> "-"
  | BRedAnd -> "&"
  | BRedOr -> "|"
  | BRedXor -> "^"

(* Generate expression *)
let rec verilog_of_expr = function
  | BVar name ->
      (* Convert special constant signals.  Gate-mapped BIR ties constant pins
         to the net names `VCC`/`GND` (the tie-cell convention), not the
         `<const1>`/`<const0>` sentinels; leaving them as bare net names emits
         an UNDRIVEN wire, so e.g. a reset sync's `.d_i(VCC)` floated its
         downstream LUT input and opt_design pruned it (Opt 31-67).  Resolve
         VCC/GND to literals here too. *)
      if name = "<const0>" || name = "GND" then "1'b0"
      else if name = "<const1>" || name = "VCC" then "1'b1"
      else sanitize_name name
  | BConst { value; width } ->
      (* Mask to width so a negative IR constant (e.g. `-1` all-ones) emits as a
         valid unsigned Verilog literal (`32'd-1` is a syntax error in xsim). *)
      let w = if width <= 0 then 1 else width in
      let mask = Z.sub (Z.shift_left Z.one w) Z.one in
      let v = Z.logand value mask in
      if width = 1 then
        Printf.sprintf "1'b%s" (Z.to_string v)
      else
        Printf.sprintf "%d'd%s" width (Z.to_string v)
  | BBinOp { op = BAnd;
             lhs = (BBinOp { op = BShr; _ } as inner);
             rhs = (BConst { value; width = 1 });
             _ } when Z.equal (Z.logand value Z.one) Z.one ->
      (* Lowered dynamic single-bit select `(sig >> i) & 1'b1` (see
         verible_to_behavioral).  It is 1 bit in the IR but sig-width in
         Verilog (`32'hXXXX & 1'b1` is 32-bit), so in a concat or replication
         it mis-sizes every field above it — `{8{be[i]}}` became 0x11111111
         (dropped RAM byte-writes) and the ibex divider's
         `{1'b0, rem, (numer>>cnt)&1'b1}` became 65 bits (broke division).
         Pin it to 1 bit at the source so EVERY context is correct, without
         casting other operands (whose IR widths are not always reliable,
         e.g. a bitwise `~op_b` is typed 1 but must stay 32). *)
      Printf.sprintf "(1'(%s & 1'b1))" (verilog_of_expr inner)
  | BBinOp { op = BAshr; lhs; rhs; _ } ->
      (* Verilog `>>>` is an ARITHMETIC shift only when its left operand is
         signed.  The behavioral emitter produces unsigned expressions (no
         signed decls, no `$signed`), so a bare `x >>> n` silently degrades
         to a logical shift — breaking srai/sra (e.g. the ibex ALU emitted
         `{sign,operand} >>> amt`, dropping the source's `$signed(...)`).
         BAshr is only ever generated for genuine arithmetic shifts (logical
         right-shift is BShr), so force the shiftee signed here. *)
      Printf.sprintf "($signed(%s) >>> %s)"
        (verilog_of_expr lhs)
        (verilog_of_expr rhs)
  | BBinOp { op; lhs; rhs; _ } ->
      Printf.sprintf "(%s %s %s)"
        (verilog_of_expr lhs)
        (verilog_of_binop op)
        (verilog_of_expr rhs)
  | BUnOp { op; operand; _ } ->
      Printf.sprintf "(%s%s)"
        (verilog_of_unop op)
        (verilog_of_expr operand)
  | BSelect { array; index } ->
      (* For constant indices, use simple integer format *)
      let index_str = match index with
        | BConst { value; _ } -> Z.to_string value
        | _ -> verilog_of_expr index
      in
      (match array with
       | BVar _ ->
           Printf.sprintf "%s[%s]" (verilog_of_expr array) index_str
       | _ ->
           (* dynamic index into a non-signal (concat / constant / expression) is
              illegal Verilog (`{a,b}[i]`, `32'd0[i]`); emit a shift — the outer
              slice / LHS width truncates.  Exact for the single-element hart
              arrays in this NrHarts=1 config. *)
           Printf.sprintf "((%s) >> (%s))" (verilog_of_expr array) index_str)
  | BSlice { signal; msb; lsb } ->
      (* Flatten nested slices: `(s[a:b])[c:d]` is not legal Verilog, but the IR
         produces it (e.g. `dmi_req_i[31:0][0:0]`).  The inner slice's lsb
         offsets the outer: `(s[a:b])[c:d]` == `s[b+c : b+d]`. *)
      let rec flatten sig_ msb lsb = match sig_ with
        | BSlice { signal = inner; msb = _; lsb = ilsb } ->
            flatten inner (ilsb + msb) (ilsb + lsb)
        | _ -> (sig_, msb, lsb)
      in
      let sig_, msb, lsb = flatten signal msb lsb in
      let hi = max msb lsb and lo = min msb lsb in
      let w = hi - lo + 1 in
      (match sig_ with
       | BConst { value; _ } ->
           (* Fold a slice of a constant, INCLUDING out-of-range slices like
              `9'd0[31:23]` (from `struct = '0` field defaults): slicing zero is
              zero.  In gate_map this is harmless, but a raw out-of-range select
              of a narrow literal simulates as X, so fold it to the real value. *)
           let mask = Z.sub (Z.shift_left Z.one w) Z.one in
           let v = Z.logand (Z.shift_right value lo) mask in
           Printf.sprintf "%d'd%s" w (Z.to_string v)
       | BVar v when (match Hashtbl.find_opt current_widths v with
                      | Some wv -> hi >= wv | None -> false) ->
           (* A WIDENING cast `w'(v)` (w > width v) lowered to an out-of-range
              slice `v[w-1:0]`.  Emit a real SV size cast — legal and equal to a
              zero-extend for an unsigned operand — instead of the illegal
              out-of-range part-select (Vivado 8-524). *)
           Printf.sprintf "%d'(%s)" w (verilog_of_expr sig_)
       | _ when (let rec sliceable = function
                   | BVar _ -> true
                   (* `arr[idx][hi:lo]` is legal only when arr is a real (array)
                      signal; a BSelect into a concat/const emits a shift and is
                      NOT sliceable *)
                   | BSelect { array; _ } -> sliceable array
                   | _ -> false in sliceable sig_) ->
           Printf.sprintf "%s[%d:%d]" (verilog_of_expr sig_) msb lsb
       | _ ->
           (* bit-select of an expression `(a>>b)[hi:lo]` is ILLEGAL Verilog.  A
              SystemVerilog size cast `w'(...)` yields a value of EXACTLY w bits,
              which — unlike a bit-mask (`& w'dMASK` keeps the operand's full
              width) — also sets the operand width correctly inside a concat:
              `{td_i, 31'(idcode_q >> 1)}` stays 32 bits.  A bare mask left it 33
              bits and truncated td_i. *)
           let e = verilog_of_expr sig_ in
           if lo = 0 then Printf.sprintf "%d'(%s)" w e
           else Printf.sprintf "%d'((%s) >> %d)" w e lo)
  | BConcat exprs ->
      Printf.sprintf "{%s}"
        (String.concat ", " (List.map verilog_of_expr exprs))
  | BReplicate { count; value } ->
      Printf.sprintf "{%d{%s}}"
        count
        (verilog_of_expr value)
  | BCond { condition; then_val; else_val } ->
      Printf.sprintf "(%s ? %s : %s)"
        (verilog_of_expr condition)
        (verilog_of_expr then_val)
        (verilog_of_expr else_val)
  | BCall { func = "@signed"; args = [x] } ->
      (* Verible's $signed-cast marker (kept for the behav-verilog path — see
         sv_lua.post_load).  Render as a real Verilog signed cast so signed
         multiplies/shifts/comparisons don't degrade to unsigned. *)
      Printf.sprintf "$signed(%s)" (verilog_of_expr x)
  | BCall { func; args } ->
      Printf.sprintf "%s(%s)"
        func
        (String.concat ", " (List.map verilog_of_expr args))

(* Generate statement with indentation *)
let rec verilog_of_stmt ?(nb=false) indent stmt =
  let ind = String.make (indent * 2) ' ' in
  (* In a sequential (clocked) block, register updates MUST be non-blocking so
     that cross-block ordering follows hardware edge semantics.  The BIR is in
     substituted/SSA form (no intra-block read-after-write), so `<=` is safe.
     Combinational blocks keep blocking `=`.  Emitting `=` everywhere is the bug
     that lagged dmi_jtag_tap's `td_o = tdo_mux` (falling-edge block) by one bit
     under Verilator's derived tck_n clock. *)
  let op = if nb then "<=" else "=" in
  match stmt with
  | BAssign { lhs; rhs } ->
      Printf.sprintf "%s%s %s %s;" ind (sanitize_name lhs) op (verilog_of_expr rhs)

  | BIf { condition; then_stmts; else_stmts } ->
      let if_part = Printf.sprintf "%sif (%s) begin" ind (verilog_of_expr condition) in
      let then_part = String.concat "\n" (List.map (verilog_of_stmt ~nb (indent + 1)) then_stmts) in
      let else_part =
        if List.length else_stmts > 0 then
          let else_stmts_str = String.concat "\n" (List.map (verilog_of_stmt ~nb (indent + 1)) else_stmts) in
          Printf.sprintf "\n%send else begin\n%s\n%send" ind else_stmts_str ind
        else
          Printf.sprintf "\n%send" ind
      in
      Printf.sprintf "%s\n%s%s" if_part then_part else_part

  | BCase { selector; cases; default } ->
      let case_header = Printf.sprintf "%scase (%s)" ind (verilog_of_expr selector) in
      let case_items = List.map (fun (value, stmts) ->
        let case_line = Printf.sprintf "%s  %s: begin" ind (verilog_of_expr value) in
        let case_stmts = String.concat "\n" (List.map (verilog_of_stmt ~nb (indent + 2)) stmts) in
        Printf.sprintf "%s\n%s\n%s  end" case_line case_stmts ind
      ) cases in
      let default_part =
        if List.length default > 0 then
          let default_stmts = String.concat "\n" (List.map (verilog_of_stmt ~nb (indent + 2)) default) in
          Printf.sprintf "\n%s  default: begin\n%s\n%s  end" ind default_stmts ind
        else ""
      in
      Printf.sprintf "%s\n%s%s\n%sendcase"
        case_header
        (String.concat "\n" case_items)
        default_part
        ind

  | BWhile { condition; body } ->
      let while_header = Printf.sprintf "%swhile (%s) begin" ind (verilog_of_expr condition) in
      let body_stmts = String.concat "\n" (List.map (verilog_of_stmt ~nb (indent + 1)) body) in
      Printf.sprintf "%s\n%s\n%send" while_header body_stmts ind

  | BFor { init; condition; update; body } ->
      (* Verilog doesn't have for loops in always blocks, convert to while *)
      let init_stmt = verilog_of_stmt ~nb indent init in
      let while_part = verilog_of_stmt ~nb indent (BWhile { condition; body = body @ [update] }) in
      Printf.sprintf "%s\n%s" init_stmt while_part

  | BBlock stmts ->
      String.concat "\n" (List.map (verilog_of_stmt ~nb indent) stmts)

  (* SVS-internal write pseudo-ops -> real Verilog l-value assignments so the
     behavioral netlist is simulatable.  Semantics mirror behavioral_initeval. *)
  | BCallStmt { func = "@mem_write"; args = [BVar a; idx; v] } ->
      (* unpacked-array word write `a[idx]=v`, or single-bit `a[idx]=v` on a
         vector — both legal since arrays are declared unpacked. *)
      let ix = match idx with BConst { value; _ } -> Z.to_string value
                            | _ -> verilog_of_expr idx in
      (* A memory-element BYTE write `mem[idx][lo +: w] = data` is lowered (by the
         2-D array-write path) to an RMW `mem[idx] = (mem[idx] & ~(M<<lo)) |
         ((data & M)<<lo)`, M = (1<<w)-1.  Emitted verbatim that is (a) not
         inferable as a byte-enabled BRAM and (b) WRONG under non-blocking — four
         sibling `mem[idx] <= f(mem[idx])` all read the pre-edge value, so only the
         last byte survives.  Recognise the shape and emit the direct indexed
         part-select, which is both the Vivado byte-write template and per-lane
         correct. *)
      (* Shared with behavioral_memlower.build_tdp_port via Behavioral_const so
         the emit and the RAMB write-strobe extraction never disagree. *)
      let byte_write = Behavioral_const.match_byte_write_rmw ~mem:a v in
      (match byte_write with
       | Some (lo, w, data) ->
           Printf.sprintf "%s%s[%s][%s +: %d] %s %s;" ind (sanitize_name a) ix
             (verilog_of_expr lo) w op (verilog_of_expr data)
       | None ->
           Printf.sprintf "%s%s[%s] %s %s;" ind (sanitize_name a) ix op (verilog_of_expr v))
  | BCallStmt { func = "@slice_write"; args = [BVar nm; m; l; v] } ->
      (match m, l with
       | BConst { value = mv; _ }, BConst { value = lv; _ } ->
           let mi = Z.to_int mv and li = Z.to_int lv in
           let hi = max mi li and lo = min mi li in
           if hi = lo then
             Printf.sprintf "%s%s[%d] %s %s;" ind (sanitize_name nm) hi op (verilog_of_expr v)
           else
             Printf.sprintf "%s%s[%d:%d] %s %s;" ind (sanitize_name nm) hi lo op (verilog_of_expr v)
       | _ ->
           (* non-constant bounds: upward indexed part-select from lsb *)
           Printf.sprintf "%s%s[%s +: 1] %s %s;" ind (sanitize_name nm)
             (verilog_of_expr l) op (verilog_of_expr v))
  | BCallStmt { func = "@part_sel_write_up"; args = [BVar nm; base; w; v] } ->
      let ws = match w with BConst { value; _ } -> Z.to_string value | _ -> verilog_of_expr w in
      let bs = match base with BConst { value; _ } -> Z.to_string value | _ -> verilog_of_expr base in
      Printf.sprintf "%s%s[%s +: %s] %s %s;" ind (sanitize_name nm) bs ws op (verilog_of_expr v)
  | BCallStmt { func = "@part_sel_write_down"; args = [BVar nm; base; w; v] } ->
      let ws = match w with BConst { value; _ } -> Z.to_string value | _ -> verilog_of_expr w in
      let bs = match base with BConst { value; _ } -> Z.to_string value | _ -> verilog_of_expr base in
      Printf.sprintf "%s%s[%s -: %s] %s %s;" ind (sanitize_name nm) bs ws op (verilog_of_expr v)
  | BCallStmt { func; args } ->
      if String.length func > 0 && func.[0] = '@' then begin
        (* A write pseudo-op whose l-value is NOT a plain signal (constant- or
           field-concat destination): typically a folded-away dead write (hartinfo
           / halted / hartsel array writes).  Emit a no-op so the behavioral
           netlist simulates.  This is asserted-dead, not proven — surface it so a
           real dropped write can't hide. *)
        lossage_warn "verilog:BCallStmt"
          (Printf.sprintf "write pseudo-op %s with non-scalar l-value emitted as no-op" func);
        Printf.sprintf "%s/* skipped %s (non-scalar lvalue) */" ind func
      end
      else
        Printf.sprintf "%s%s(%s);" ind func (String.concat ", " (List.map verilog_of_expr args))

  | BReturn _ ->
      Printf.sprintf "%s/* return not supported in always blocks */" ind

(* Generate sensitivity list *)
let verilog_of_sensitivity = function
  | BPosEdge clk -> Printf.sprintf "posedge %s" clk
  | BNegEdge clk -> Printf.sprintf "negedge %s" clk
  | BLevel sig_ -> sig_
  | BAny -> "*"

(* Does an expression read any signal (BVar)?  A combinational block that reads
   NO signal has a pure-constant RHS, so a star-sensitivity always infers an
   EMPTY sensitivity list and never executes in simulation. *)
let rec expr_mentions_name name = function
  | BVar n -> n = name
  | BConst _ -> false
  | BBinOp { lhs; rhs; _ } -> expr_mentions_name name lhs || expr_mentions_name name rhs
  | BUnOp { operand; _ } -> expr_mentions_name name operand
  | BCond { condition; then_val; else_val } ->
      expr_mentions_name name condition || expr_mentions_name name then_val
      || expr_mentions_name name else_val
  | BConcat es -> List.exists (expr_mentions_name name) es
  | BReplicate { value; _ } -> expr_mentions_name name value
  | BSelect { array; index } -> expr_mentions_name name array || expr_mentions_name name index
  | BSlice { signal; _ } -> expr_mentions_name name signal
  | BCall { args; _ } -> List.exists (expr_mentions_name name) args

let rec expr_reads_var = function
  | BVar _ -> true
  | BConst _ -> false
  | BBinOp { lhs; rhs; _ } -> expr_reads_var lhs || expr_reads_var rhs
  | BUnOp { operand; _ } -> expr_reads_var operand
  | BCond { condition; then_val; else_val } ->
      expr_reads_var condition || expr_reads_var then_val || expr_reads_var else_val
  | BConcat es -> List.exists expr_reads_var es
  | BReplicate { value; _ } -> expr_reads_var value
  | BSelect { array; index } -> expr_reads_var array || expr_reads_var index
  | BSlice { signal; _ } -> expr_reads_var signal
  | BCall { args; _ } -> List.exists expr_reads_var args

let rec stmt_reads_var = function
  | BAssign { rhs; _ } -> expr_reads_var rhs
  | BIf { condition; then_stmts; else_stmts } ->
      expr_reads_var condition
      || List.exists stmt_reads_var then_stmts
      || List.exists stmt_reads_var else_stmts
  | BCase { selector; cases; default } ->
      expr_reads_var selector
      || List.exists (fun (_, ss) -> List.exists stmt_reads_var ss) cases
      || List.exists stmt_reads_var default
  | BBlock ss -> List.exists stmt_reads_var ss
  (* mem_write(arr, idx, data): arr is the write TARGET (lhs), not a read —
     only idx/data are reads.  Counting arr made a per-slot constant write
     `arr[k] = <const>` look signal-dependent, so it emitted a star-sensitivity
     always (empty real sensitivity, never fires, X) instead of always_comb.
     This bit the unpacked bus-config arrays cfg_device_addr_mask/base, which
     (being merge-skipped) stay as per-slot mem_write blocks. *)
  | BCallStmt { func = "@mem_write"; args = _ :: rest } ->
      List.exists expr_reads_var rest
  | BCallStmt { args; _ } -> List.exists expr_reads_var args
  | BReturn (Some e) -> expr_reads_var e
  | BWhile { condition; body } ->
      expr_reads_var condition || List.exists stmt_reads_var body
  | BFor { init; condition; update; body } ->
      stmt_reads_var init || expr_reads_var condition || stmt_reads_var update
      || List.exists stmt_reads_var body
  | _ -> false

(* Generate process *)
let verilog_of_process proc =
  match proc with
  | BCombinational { name; sensitivity; body } ->
      let comment = Printf.sprintf "// Combinational process: %s" name in
      let sens_list = String.concat " or " (List.map verilog_of_sensitivity sensitivity) in
      (* A block with a pure-constant RHS (cfg_device_addr_mask = concat of
         literals) reads no signal, so a star-sensitivity always gets an EMPTY
         sensitivity list and xsim NEVER runs it - the signal stays X, leaving
         the bus address-decode config X and stalling the CPU fetch.  Emit
         always_comb for those (runs at time 0).  Keep the star-sensitivity form
         for blocks that read signals, since always_comb rejects the SVS pattern
         of one variable driven by several combinational blocks. *)
      let always_header =
        if List.exists stmt_reads_var body then Printf.sprintf "always @(%s) begin" sens_list
        else "always_comb begin" in
      let body_stmts = String.concat "\n" (List.map (verilog_of_stmt 1) body) in
      Printf.sprintf "%s\n%s\n%s\nend\n" comment always_header body_stmts

  | BSequential { name; clock; clock_edge; reset; reset_edge; reset_async; body } ->
      let comment = Printf.sprintf "// Sequential process: %s" name in
      let clk_edge = match clock_edge with `Pos -> "posedge" | `Neg -> "negedge" in
      let sens_list = match (reset, reset_async) with
        | (Some rst, true) ->
            let rst_edge = match reset_edge with
              | Some `Pos -> "posedge"
              | Some `Neg -> "negedge"
              | None -> "posedge"
            in
            Printf.sprintf "%s %s or %s %s" clk_edge clock rst_edge rst
        | _ ->
            Printf.sprintf "%s %s" clk_edge clock
      in
      let always_header = Printf.sprintf "always @(%s) begin" sens_list in

      (* Generate reset logic if async reset.  The BIR carries the reset SIGNAL
         but not per-register reset VALUES, and the reset branch used to be an
         empty `// Reset logic` placeholder — so every FF stayed X through reset
         in simulation.  Zero each register assigned in this block (the reset
         value of nearly every DM register); this makes the behavioral netlist
         reset cleanly.  RResetValue FFs (rare, prim_flop wrappers) reset to 0
         here too — refine to per-signal values if one matters. *)
      let collect_targets stmts =
        let tbl = Hashtbl.create 16 and order = ref [] in
        let add n = if not (Hashtbl.mem tbl n) then (Hashtbl.add tbl n (); order := n :: !order) in
        let base s = try String.sub s 0 (String.index s '[') with Not_found -> s in
        let rec go = function
          | BAssign { lhs; _ } -> add (base lhs)
          | BCallStmt { func; args = (BVar a) :: _ }
            when String.length func > 0 && func.[0] = '@' -> add a
          | BIf { then_stmts; else_stmts; _ } -> List.iter go then_stmts; List.iter go else_stmts
          | BCase { cases; default; _ } ->
              List.iter (fun (_, ss) -> List.iter go ss) cases; List.iter go default
          | BBlock ss -> List.iter go ss
          | BWhile { body; _ } | BFor { body; _ } -> List.iter go body
          | _ -> ()
        in
        List.iter go stmts; List.rev !order
      in
      let body_with_reset = match (reset, reset_async) with
        | (Some rst, true) ->
            let rst_check = match reset_edge with
              | Some `Pos -> rst
              | Some `Neg -> Printf.sprintf "!%s" rst
              | None -> rst
            in
            (* Prefer the REAL reset values.  In an ASYNC-reset always block the
               reset guard is the body's OUTERMOST if/ternary — the frontend
               lifts `if(!rst) reg<=RESETVAL else <clocked>` first, so by SHAPE
               its then-branch is the reset path and its else-branch the clocked
               path (a register with no source reset lifts to `!rst ? reg : next`,
               i.e. a hold — still correct).  Recognise that shape and peel it in
               both forms (top-level BIf, or per-register `reg<=(cond?rv:clk)`
               BCond after iflift); zeroing every register instead would reset
               e.g. mtvec/mstatus to 0.  No condition-name matching. *)
            let split_reset s = match s with
              | BIf { then_stmts; else_stmts; _ } ->
                  (then_stmts, else_stmts)
              | BAssign { lhs; rhs = BCond { then_val; else_val; _ } } ->
                  ([BAssign { lhs; rhs = then_val }], [BAssign { lhs; rhs = else_val }])
              | other -> ([], [other])
            in
            (* Flatten BBlock wrappers first — the reset guard is often nested
               inside one, which split_reset would otherwise treat as opaque. *)
            let rec flatten = function
              | BBlock ss :: rest -> flatten (ss @ rest)
              | s :: rest -> s :: flatten rest
              | [] -> [] in
            let resets, clocked =
              List.fold_right (fun s (rs, cs) ->
                  let r, c = split_reset s in (r @ rs, c @ cs)) (flatten body) ([], []) in
            (match (if resets <> [] then Some (resets, clocked) else None) with
             | Some (resets, clocked) ->
                 (* Safety net: any register the block writes but the source
                    reset branch doesn't cover still gets `<= '0` (else it would
                    reset to X in sim — the reason zeroing was added). *)
                 let covered = collect_targets resets in
                 let extra = List.filter_map (fun n ->
                     if List.mem n covered then None
                     else
                       let rhs = if List.mem n !current_unpacked then "'{default: '0}" else "'0" in
                       Some (Printf.sprintf "    %s <= %s;" (sanitize_name n) rhs))
                     (collect_targets body) in
                 Printf.sprintf "  if (%s) begin\n%s\n  end else begin\n%s\n  end"
                   rst_check
                   (String.concat "\n"
                      (List.map (verilog_of_stmt ~nb:true 2) resets @ extra))
                   (String.concat "\n" (List.map (verilog_of_stmt ~nb:true 2) clocked))
             | None ->
                 let resets = List.map (fun n ->
                     (* Unpacked-array reset needs `'{default:'0}` (Vivado rejects
                        a bare `= '0` packed literal on an unpacked target). *)
                     let rhs = if List.mem n !current_unpacked then "'{default: '0}" else "'0" in
                     Printf.sprintf "    %s <= %s;" (sanitize_name n) rhs)
                                (collect_targets body) in
                 Printf.sprintf "  if (%s) begin\n%s\n  end else begin\n%s\n  end"
                   rst_check
                   (String.concat "\n" resets)
                   (String.concat "\n" (List.map (verilog_of_stmt ~nb:true 2) body)))
        | _ ->
            String.concat "\n" (List.map (verilog_of_stmt ~nb:true 1) body)
      in

      Printf.sprintf "%s\n%s\n%s\nend\n" comment always_header body_with_reset

(* Generate instance *)
let verilog_of_instance prog inst =
  let { inst_name; module_name; param_values; param_strs; port_connections } = inst in

  (* Parameter instantiation.  Emit BOTH integer params (param_values) and
   * string params (param_strs) -- the latter carry primitive INIT truth
   * tables (e.g. INIT = "64'hXXXX"), without which a gate netlist is
   * functionless.  A value already a sized Verilog literal (has a quote,
   * or all digits) is emitted raw; anything else (e.g. IOSTANDARD = LVDS)
   * is wrapped in string quotes. *)
  (* A user module is SPECIALIZED (params baked into its name, e.g.
     prim_flop__W1_RResetValue) and declares NO parameters — passing `.Width(1)`
     to it is an elaboration error.  So for a user module, keep only params it
     actually declares; a library cell (FDRE/BSCANE2, not in prog.modules) keeps
     all its params (INIT etc.). *)
  let declared_params =
    match List.find_opt (fun (m : bmodule) -> m.name = module_name) prog.modules with
    | Some m -> Some (List.map fst m.params)
    | None -> None (* library cell -> keep all *)
  in
  let keep name = match declared_params with
    | None -> true | Some ps -> List.mem name ps in
  let int_params = List.filter_map (fun (name, value) ->
    if keep name then Some (name, Printf.sprintf ".%s(%d)" name value) else None) param_values in
  let is_vlit s =
    s <> "" && (String.contains s '\'' ||
                String.for_all (fun c -> c >= '0' && c <= '9') s) in
  (* A REAL literal (`5.000`, `20.000`, `0.500`) must be emitted RAW, not
     quoted: a `real`-typed parameter (MMCME2_ADV.CLKFBOUT_MULT_F/CLKIN1_PERIOD
     etc.) given the string "5.000" makes Vivado pack the string's ASCII bytes
     as the real value (0x352E303030 = 228408176688), so the MMCM gets garbage
     multipliers/periods.  Detect a plain decimal: optional sign, exactly one
     '.' with digits on both sides. *)
  let is_real s =
    let s = if String.length s > 0 && (s.[0] = '+' || s.[0] = '-')
            then String.sub s 1 (String.length s - 1) else s in
    let n = String.length s in
    let dot = ref (-1) and ok = ref true in
    String.iteri (fun i c ->
      if c = '.' then (if !dot >= 0 then ok := false else dot := i)
      else if not (c >= '0' && c <= '9') then ok := false) s;
    !ok && !dot > 0 && !dot < n - 1 in
  let str_params = List.filter_map (fun (name, value) ->
    if not (keep name) then None
    else if is_vlit value || is_real value then Some (name, Printf.sprintf ".%s(%s)" name value)
    else Some (name, Printf.sprintf ".%s(\"%s\")" name value)) param_strs in
  (* Dedup by parameter NAME, keeping the first occurrence.  The param
     extraction can list the same name in BOTH param_values and param_strs, or
     twice in param_strs (a Vivado string INIT re-added already-quoted), which
     emitted e.g. `.DIVCLK_DIVIDE(1), .DIVCLK_DIVIDE(1)` and
     `.STARTUP_WAIT("FALSE"), .STARTUP_WAIT(""FALSE"")` — a duplicate-override
     and a doubled-quote syntax error that made xvlog reject the whole module. *)
  let all_params =
    let seen = Hashtbl.create 16 in
    List.filter_map (fun (name, s) ->
      if Hashtbl.mem seen name then None
      else (Hashtbl.add seen name (); Some s)) (int_params @ str_params) in
  let params_str =
    if all_params <> [] then
      Printf.sprintf " #(%s)" (String.concat ", " all_params)
    else ""
  in

  (* Look up expected ports for this module *)
  let expected_ports =
    (* First check library cells *)
    match List.assoc_opt module_name prog.library_cells with
    | Some lib_ports -> List.map (fun (p : library_port) -> p.port_name) lib_ports
    | None ->
        (* Then check user modules *)
        match List.find_opt (fun (m : bmodule) -> m.name = module_name) prog.modules with
        | Some m ->
            List.filter_map (fun (s : bsignal) ->
              if s.direction <> `Internal then Some (sanitize_name s.name) else None
            ) m.signals
        | None -> []
  in

  (* Build port connections, adding empty connections for unconnected ports *)
  let connected_ports = Hashtbl.create (List.length port_connections) in
  List.iter (fun (port, _) -> Hashtbl.add connected_ports port true) port_connections;

  let all_port_connections =
    if expected_ports <> [] then
      List.map (fun port_name ->
        match List.assoc_opt port_name port_connections with
        | Some expr -> Printf.sprintf ".%s(%s)" port_name (verilog_of_expr expr)
        | None -> Printf.sprintf ".%s()" port_name  (* Empty connection *)
      ) expected_ports
    else
      (* Unknown primitive (LUT/FDRE/RAMB/… not in library_cells and not a user
         module — the usual case for a gate-mapped program): emit the instance's
         ACTUAL port_connections verbatim, else the cell loses ALL connectivity
         (`the_LUT1 ()`) and the whole gate netlist is dead in simulation. *)
      List.map (fun (port, expr) ->
        Printf.sprintf ".%s(%s)" port (verilog_of_expr expr)) port_connections
  in

  let ports_str = String.concat ",\n    " all_port_connections in

  (* Generate-scope-qualified instance names carry a `.` (e.g.
     `gen_rom_snd_scratch.i_debug_rom`), which is illegal for a plain instance
     name — flatten it to an underscore. *)
  let inst_name =
    String.map (fun c -> if c = '.' then '_' else c) inst_name in

  if ports_str <> "" then
    Printf.sprintf "%s%s %s (\n    %s\n);"
      module_name
      params_str
      inst_name
      ports_str
  else
    Printf.sprintf "%s%s %s ();"
      module_name
      params_str
      inst_name

(* Generate signal declaration *)
(* Source-UNPACKED array (tagged "unpacked", i.e. per-slot-written only): emit
   `[W:0] name [0:N-1]` (dim after the name) so each slot is a distinct signal
   that takes its own driver without a packed multi-driver.  Packed 2-D arrays
   and any array with a whole-array write stay packed via verilog_of_type. *)
let unpacked_split sig_ =
  match sig_.stype with
  | BArray { element; size } when List.mem_assoc "unpacked" sig_.attrs ->
      Some (verilog_of_type element, Printf.sprintf " [0:%d]" (size - 1))
  | _ -> None

let verilog_of_signal sig_ =
  let { name; stype; direction; initial_value; attrs = _ } = sig_ in
  (* Skip constant signals - they're handled as literals *)
  if name = "<const0>" || name = "<const1>" then
    None
  else
    let sanitized_name = sanitize_name name in
    let type_str, arr_suffix = match unpacked_split sig_ with
      | Some (t, suffix) -> (t, suffix)
      | None -> (verilog_of_type stype, "") in
    let init_str = match initial_value with
      | Some expr -> Printf.sprintf " = %s" (verilog_of_expr expr)
      | None -> ""
    in
    let decl = match direction with
    | `Input -> Printf.sprintf "input  %s %s%s%s;" type_str sanitized_name arr_suffix init_str
    | `Output -> Printf.sprintf "output %s %s%s%s;" type_str sanitized_name arr_suffix init_str
    | `Internal -> Printf.sprintf "%s %s%s%s;" type_str sanitized_name arr_suffix init_str
    in
    Some decl

(* Generate module *)
(* Emit an inferred memory (ROM/RAM) as a `reg` array plus, when it carries
   init_values, an `initial` block loading them.  meminfer lifts decoder `case`
   tables to BRom + `x = rom_x[sel]` and the program RAM to a BRam with the
   $readmemh image in init_values; without emitting bmod.mems those `rom_*`
   reads (and the program image) had no declaration and xvlog rejected them. *)
let verilog_of_mem ~decl (m : bmem) =
  let dw = max 1 m.data_width in
  (* [decl]=false when the memory is ALSO an already-declared signal array
     (dm_mem's `mem`/`progbuf`, etc.) — emit only the init block to avoid a
     second declaration, but still load init_values (the $readmemh image). *)
  let decl_str =
    if decl then
      Printf.sprintf "  reg [%d:0] %s [0:%d];" (dw - 1) m.mname (max 0 (m.depth - 1))
    else "" in
  if m.init_values = [] then decl_str
  else
    let mask = if dw >= 62 then -1 else (1 lsl dw) - 1 in
    let assigns = List.mapi (fun i v ->
      Printf.sprintf "    %s[%d] = %d'h%x;" m.mname i dw (v land mask)) m.init_values in
    let init_str =
      Printf.sprintf "  initial begin\n%s\n  end" (String.concat "\n" assigns) in
    if decl_str = "" then init_str else decl_str ^ "\n" ^ init_str

let verilog_of_module prog bmod =
  let { name; params; signals; processes; instances } = bmod in

  (* Module header *)
  let module_header = Printf.sprintf "module %s" name in

  (* Parameters *)
  let params_str =
    if List.length params > 0 then
      let param_list = String.concat ",\n    " (List.map (fun (name, value) ->
        Printf.sprintf "parameter %s = %d" name value
      ) params) in
      Printf.sprintf " #(\n    %s\n)" param_list
    else ""
  in

  (* Port list - ANSI style with types and directions *)
  let ports = List.filter (fun (s : bsignal) ->
    s.direction <> `Internal && s.name <> "<const0>" && s.name <> "<const1>"
  ) signals in

  let port_list =
    if List.length ports > 0 then
      String.concat ",\n    " (List.map (fun (s : bsignal) ->
        let sanitized_name = sanitize_name s.name in
        let type_str, arr_suffix = match unpacked_split s with
          | Some (t, suffix) -> (t, suffix)
          | None -> (verilog_of_type s.stype, "") in
        let dir_str = match s.direction with
          | `Input -> "input"
          | `Output -> "output"
          | `Internal -> "logic"  (* Should not happen in port list *)
        in
        Printf.sprintf "%s %s %s%s" dir_str type_str sanitized_name arr_suffix
      ) ports)
    else ""
  in

  let full_header =
    if port_list <> "" then
      Printf.sprintf "%s%s (\n    %s\n);\n" module_header params_str port_list
    else
      Printf.sprintf "%s%s ();\n" module_header params_str
  in

  (* Signal declarations - only internal signals *)
  current_unpacked :=
    List.filter_map (fun (s : bsignal) ->
      if List.mem_assoc "unpacked" s.attrs then Some s.name else None) signals;
  Hashtbl.reset current_widths;
  List.iter (fun (s : bsignal) ->
    Hashtbl.replace current_widths s.name (width_of_type s.stype)) signals;
  let internal_signals = List.filter (fun (s : bsignal) -> s.direction = `Internal) signals in
  let signal_decls = String.concat "\n" (List.filter_map verilog_of_signal internal_signals) in

  (* Inferred memory (ROM/RAM) declarations + init blocks.  Skip the array
     declaration when the name is already a declared signal (emit only its init
     block) to avoid a second declaration. *)
  let mem_decls =
    (* Dedup by name: meminfer can lift the same lhs to a rom_<lhs> in more than
       one branch, producing duplicate bmems that would re-declare the array. *)
    let seen = Hashtbl.create 16 in
    let uniq = List.filter (fun (m : bmem) ->
      if Hashtbl.mem seen m.mname then false
      else (Hashtbl.replace seen m.mname (); true)) bmod.mems in
    String.concat "\n" (List.map (fun (m : bmem) ->
      let already_sig = List.exists (fun (s : bsignal) -> s.name = m.mname) signals in
      verilog_of_mem ~decl:(not already_sig) m) uniq) in

  (* Processes *)
  let process_strs = String.concat "\n" (List.map verilog_of_process processes) in

  (* Dedup instance NAMES within this module.  An unrolled generate-FOR block
     replicates its instances under one shared block label (gpio's
     `for (i) begin : gen_debounce  debounce dbnc(...)` flattens to 8×
     `gen_debounce_dbnc`), which xvlog rejects as re-declared.  Instance names
     are pure labels — connectivity is by net, not by instance name — so
     suffixing the 2nd+ collision of each name is safe.  The first keeps its
     name so the common (non-replicated) case is untouched. *)
  let instances =
    let seen = Hashtbl.create 64 in
    List.map (fun (i : binstance) ->
      match Hashtbl.find_opt seen i.inst_name with
      | None -> Hashtbl.replace seen i.inst_name 0; i
      | Some n ->
          let n = n + 1 in
          Hashtbl.replace seen i.inst_name n;
          { i with inst_name = Printf.sprintf "%s__g%d" i.inst_name n }) instances in
  (* Instances *)
  let instance_strs = String.concat "\n\n" (List.map (verilog_of_instance prog) instances) in

  (* Bridge input-port bits to the scalarized nets the gate cells read.
     For a gate-mapped module, of_circuit ties a cell OUTPUT to an output-port
     slice (`out[N:N]`) but a cell INPUT to a bare scalar net `port__N`, leaving
     every multi-bit INPUT port's bits unconnected — they float to X and kill
     the whole netlist (bir_to_verilog_netlist does this bridge via `assign
     {nets}=port`; behavioral_to_verilog did not).  Emit `assign port__N =
     port[N]` for each input-port bit the instances actually reference. *)
  let port_bit_bridges =
    let used : (string, unit) Hashtbl.t = Hashtbl.create 256 in
    let rec collect = function
      | BVar n -> Hashtbl.replace used n ()
      | BSlice { signal; _ } -> collect signal
      | BConcat es -> List.iter collect es
      | BSelect { array; index } -> collect array; collect index
      | BBinOp { lhs; rhs; _ } -> collect lhs; collect rhs
      | BUnOp { operand; _ } -> collect operand
      | BReplicate { value; _ } -> collect value
      | BCond { condition; then_val; else_val } ->
          collect condition; collect then_val; collect else_val
      | BCall { args; _ } -> List.iter collect args
      | _ -> () in
    List.iter (fun (i : binstance) ->
      List.iter (fun (_p, e) -> collect e) i.port_connections) instances;
    let lines = List.concat_map (fun (s : bsignal) ->
      let w = width_of_type s.stype in
      if s.direction = `Input && w > 1 then
        List.filter_map (fun i ->
          let bit = Printf.sprintf "%s__%d" s.name i in
          if Hashtbl.mem used bit then
            Some (Printf.sprintf "  assign %s = %s[%d];"
                    (sanitize_name bit) (sanitize_name s.name) i)
          else None) (List.init w (fun i -> i))
      else []) signals in
    String.concat "\n" lines
  in

  (* Combine all *)
  let body_parts = [signal_decls; mem_decls; port_bit_bridges; process_strs; instance_strs]
    |> List.filter (fun s -> s <> "")
    |> String.concat "\n\n"
  in

  Printf.sprintf "%s\n%s\nendmodule\n" full_header body_parts

(* Collect unique library cell types from all instances *)
let collect_library_cells prog =
  let cells = Hashtbl.create 64 in
  List.iter (fun bmod ->
    List.iter (fun inst ->
      (* Collect RTL_* and other library cells, but skip user modules *)
      if String.starts_with ~prefix:"RTL_" inst.module_name ||
         String.starts_with ~prefix:"slib_" inst.module_name then
        (* Merge port connections from all instances of same module *)
        let existing_ports = try Hashtbl.find cells inst.module_name with Not_found -> [] in
        let all_port_names = Hashtbl.create 16 in
        (* Add existing ports *)
        List.iter (fun (port, _) -> Hashtbl.replace all_port_names port true) existing_ports;
        (* Add new ports *)
        List.iter (fun (port, _) -> Hashtbl.replace all_port_names port true) inst.port_connections;
        (* Create merged list with any expr (just use first found) *)
        let merged_ports = Hashtbl.fold (fun port _ acc ->
          let expr = try
            List.assoc port inst.port_connections
          with Not_found ->
            List.assoc port existing_ports
          in
          (port, expr) :: acc
        ) all_port_names [] in
        Hashtbl.replace cells inst.module_name merged_ports
    ) bmod.instances
  ) prog.modules;

  (* Convert to sorted list *)
  let cell_list = Hashtbl.fold (fun name ports acc -> (name, ports) :: acc) cells [] in
  List.sort (fun (a, _) (b, _) -> String.compare a b) cell_list

(* Extract port names from port connections *)
let extract_port_info port_connections =
  let ports = Hashtbl.create 16 in
  List.iter (fun (port_name, _) ->
    Hashtbl.replace ports port_name true
  ) port_connections;
  ports

(* Generate behavioral definition for library cell using actual cell definition *)
(* Generate behavioral definition for library cell using actual cell definition *)
let generate_library_cell_def_from_spec (module_name, lib_ports) =
  (* Separate inputs and outputs *)
  let (inputs, outputs) = List.partition (fun (p : library_port) ->
    p.port_direction = `Input
  ) lib_ports in

  (* Helper: check if port exists *)
  let has_port name = List.exists (fun (p : library_port) -> p.port_name = name) lib_ports in

  (* Generate body based on module type *)
  let body =
    if String.starts_with ~prefix:"RTL_REG_ASYNC" module_name then
      (* Register with async controls *)
      let has_clr = has_port "CLR" in
      let has_pre = has_port "PRE" in
      let has_ce = has_port "CE" in

      let sensitivity =
        ["posedge C"] @
        (if has_clr then ["posedge CLR"] else []) @
        (if has_pre then ["posedge PRE"] else [])
        |> String.concat " or "
      in

      let reset_logic =
        if has_clr && has_pre then
          "    if (CLR)\n      Q <= 1'b0;\n    else if (PRE)\n      Q <= 1'b1;"
        else if has_clr then
          "    if (CLR)\n      Q <= 1'b0;"
        else if has_pre then
          "    if (PRE)\n      Q <= 1'b1;"
        else
          ""
      in

      let clock_logic =
        if has_ce then
          "    else if (CE)\n      Q <= D;"
        else
          "    else\n      Q <= D;"
      in

      if reset_logic <> "" then
        Printf.sprintf "  always @(%s) begin\n%s\n%s\n  end" sensitivity reset_logic clock_logic
      else
        "  always @(posedge C) begin\n    Q <= D;\n  end"

    else if String.starts_with ~prefix:"RTL_LATCH" module_name then
      (* Find D, G, and Q ports *)
      let d_port = List.find_opt (fun (p : library_port) -> p.port_name = "D") inputs in
      let g_port = List.find_opt (fun (p : library_port) -> p.port_name = "G") inputs in
      let q_port = List.find_opt (fun (p : library_port) -> p.port_name = "Q") outputs in

      (match (d_port, g_port, q_port) with
       | (Some d, Some g, Some q) when g.port_width > 1 ->
           (* Multi-bit latch - individual enable per bit *)
           let latch_bits = List.init g.port_width (fun i ->
             Printf.sprintf "    if (G[%d]) Q[%d] = D[%d];" i i i
           ) in
           Printf.sprintf "  always @(*) begin\n%s\n  end" (String.concat "\n" latch_bits)
       | (Some d, Some g, Some q) ->
           (* Single-bit latch *)
           "  always @(*) begin\n    if (G)\n      Q = D;\n  end"
       | _ ->
           "  // TODO: Define LATCH behavior")

    else if String.starts_with ~prefix:"RTL_MUX" module_name then
      let output_names = List.map (fun (p : library_port) -> p.port_name) outputs in

      (* Find selector port(s) *)
      let select_ports = List.filter (fun (p : library_port) ->
        p.port_name = "S" || String.starts_with ~prefix:"S" p.port_name
      ) inputs in

      (* Find data input ports *)
      let data_ports = List.filter (fun (p : library_port) ->
        p.port_name <> "S" && not (String.starts_with ~prefix:"S" p.port_name)
      ) inputs in

      (* Sort data ports by name (I0, I1, I2, ...) *)
      let data_ports_sorted = List.sort (fun (p1 : library_port) (p2 : library_port) ->
        String.compare p1.port_name p2.port_name
      ) data_ports in

      (match (data_ports_sorted, select_ports, output_names) with
       | (data_list, [sel_port], [o]) ->
           let num_inputs = List.length data_list in
           let sel_width = sel_port.port_width in

           (* Use continuous assignment with nested ternary operators *)
           let out_port = List.hd outputs in

           (* Helper to generate input expression with width extension *)
           let input_expr (p : library_port) =
             if p.port_width < out_port.port_width then
               let extend = out_port.port_width - p.port_width in
               Printf.sprintf "{%d'b0, %s}" extend p.port_name
             else
               p.port_name
           in

           (* Generate nested ternary expressions *)
           let rec build_ternary idx remaining =
             match remaining with
             | [] -> "1'b0"  (* Shouldn't happen *)
             | [last_input] -> input_expr last_input
             | input :: rest ->
                 Printf.sprintf "(%s == %d'd%d) ? %s :\n           %s"
                   sel_port.port_name sel_width idx (input_expr input)
                   (build_ternary (idx + 1) rest)
           in

           Printf.sprintf "  assign %s = %s;" o (build_ternary 0 data_list)
       | _ -> "  // TODO: Define mux behavior")

    else if String.starts_with ~prefix:"RTL_ADD" module_name then
      (* Helper: extend operand if needed *)
      let extend_operand (p : library_port) target_width =
        if p.port_width < target_width then
          let extend = target_width - p.port_width in
          Printf.sprintf "{%d'b0, %s}" extend p.port_name
        else
          p.port_name
      in
      (match (inputs, outputs) with
       | ([p0; p1], [out_port]) ->
           let max_width = max (max p0.port_width p1.port_width) out_port.port_width in
           let op0 = extend_operand p0 max_width in
           let op1 = extend_operand p1 max_width in
           Printf.sprintf "  assign %s = %s + %s;" out_port.port_name op0 op1
       | _ -> "  // TODO: Define ADD behavior")

    else if String.starts_with ~prefix:"RTL_SUB" module_name then
      let extend_operand (p : library_port) target_width =
        if p.port_width < target_width then
          let extend = target_width - p.port_width in
          Printf.sprintf "{%d'b0, %s}" extend p.port_name
        else
          p.port_name
      in
      (match (inputs, outputs) with
       | ([p0; p1], [out_port]) ->
           let max_width = max (max p0.port_width p1.port_width) out_port.port_width in
           let op0 = extend_operand p0 max_width in
           let op1 = extend_operand p1 max_width in
           Printf.sprintf "  assign %s = %s - %s;" out_port.port_name op0 op1
       | _ -> "  // TODO: Define SUB behavior")

    else if String.starts_with ~prefix:"RTL_NEQ" module_name then
      let extend_operand (p : library_port) target_width =
        if p.port_width < target_width then
          let extend = target_width - p.port_width in
          Printf.sprintf "{%d'b0, %s}" extend p.port_name
        else
          p.port_name
      in
      (match (inputs, outputs) with
       | ([p0; p1], [out_port]) ->
           let max_width = max p0.port_width p1.port_width in
           let op0 = extend_operand p0 max_width in
           let op1 = extend_operand p1 max_width in
           Printf.sprintf "  assign %s = %s != %s;" out_port.port_name op0 op1
       | _ -> "  // TODO: Define NEQ behavior")

    else if String.starts_with ~prefix:"RTL_EQ" module_name then
      let extend_operand (p : library_port) target_width =
        if p.port_width < target_width then
          let extend = target_width - p.port_width in
          Printf.sprintf "{%d'b0, %s}" extend p.port_name
        else
          p.port_name
      in
      (match (inputs, outputs) with
       | ([p0; p1], [out_port]) ->
           let max_width = max p0.port_width p1.port_width in
           let op0 = extend_operand p0 max_width in
           let op1 = extend_operand p1 max_width in
           Printf.sprintf "  assign %s = %s == %s;" out_port.port_name op0 op1
       | _ -> "  // TODO: Define EQ behavior")

    else if String.starts_with ~prefix:"RTL_ROM" module_name then
      let output_names = List.map (fun (p : library_port) -> p.port_name) outputs in
      (match output_names with
       | [o] -> Printf.sprintf "  // Placeholder ROM\n  assign %s = 1'b0;" o
       | _ -> "  // TODO: Define ROM behavior")

    else
      "  // Unknown library cell type"
  in

  (* Check if body uses procedural assignment (always block) *)
  let uses_always = String.starts_with ~prefix:"  always" body in

  (* Generate port declarations - outputs need 'reg' if assigned in always block *)
  let port_decls =
    (List.map (fun (p : library_port) ->
      if p.port_width > 1 then
        Printf.sprintf "input [%d:0] %s" (p.port_width - 1) p.port_name
      else
        Printf.sprintf "input %s" p.port_name
    ) inputs) @
    (List.map (fun (p : library_port) ->
      let output_type = if uses_always then "output reg" else "output" in
      if p.port_width > 1 then
        Printf.sprintf "%s [%d:0] %s" output_type (p.port_width - 1) p.port_name
      else
        Printf.sprintf "%s %s" output_type p.port_name
    ) outputs)
  in

  Printf.sprintf "module %s (\n    %s\n);\n%s\nendmodule"
    module_name
    (String.concat ",\n    " port_decls)
    body
let generate_library_cells prog =
  (* Use actual library cell definitions from the EDIF *)
  let lib_cell_defs = prog.library_cells in
  (* In the behavioral-verilog flow the netlist is always elaborated against the
     simulator's unisims_ver / Vivado's built-in primitive library, which supply
     BSCANE2 / MMCME2_ADV / BUFG / IBUFDS / FDRE / RAMB / … .  Emitting our own
     port-only stubs for them is not just redundant: the instance passes params
     the stub never declares, so xelab / synth error ("does not have a parameter
     named …") and every consumer has to strip the stubs again.  We already know
     which cells are vendor primitives — they were resolved from the Xilinx
     primitive DB by lookup_xil_primitive_ports — so skip them here and keep only
     the internal RTL_* cells that no external library provides.  Gated on
     BEHAV_VERILOG so the EDIF / gate flows (which may genuinely need the stubs)
     are unaffected. *)
  let lib_cell_defs, skipped =
    if Sys.getenv_opt "BEHAV_VERILOG" <> None then
      List.partition (fun (n, _) -> String.starts_with ~prefix:"RTL_" n) lib_cell_defs
    else lib_cell_defs, [] in
  let hdr =
    if skipped = [] then ""
    else Printf.sprintf
      "// Xilinx primitives provided by unisims_ver / Vivado (stubs skipped): %s\n\n"
      (String.concat ", " (List.map fst skipped)) in
  if List.length lib_cell_defs > 0 then
    let defs = List.map generate_library_cell_def_from_spec lib_cell_defs in
    hdr ^ Printf.sprintf "// Library cell definitions\n\n%s\n\n" (String.concat "\n\n" defs)
  else
    hdr

(* Generate program *)
let verilog_of_program prog =
  let lib_cells = generate_library_cells prog in
  let modules = String.concat "\n\n" (List.map (verilog_of_module prog) prog.modules) in
  lib_cells ^ modules

(* Write to file *)
let write_to_file filename prog =
  let oc = open_out filename in
  let verilog = verilog_of_program prog in
  output_string oc verilog;
  close_out oc;
  Printf.printf "Written Verilog to: %s\n" filename
