(* One-hot state-compare reduction (EXPERIMENTAL, off by default).
 *
 * A register every write of which is a constant with at most one bit set, and
 * whose initial value is such a constant too, only ever holds a one-hot value
 * or zero.  On that reachable set `r == K` (K one-hot) is exactly r[log2 K]
 * and `r != K` its inverse -- the 8-bit compare becomes a wire.  picorv32's
 * cpu_state (8 one-hot states) is compared in ~300 places.
 *
 * NOT combinationally equivalent: with r free (as the register-correspondence
 * miter treats it) r = 0b11 separates the two.  It is sequentially equivalent
 * under the inductive invariant onehot0(r), which the miter does not yet
 * assume -- so this is gated behind GATE_MAP_BIR_ONEHOT=1 until it does. *)

open Behavioral_ir

let popcount z = if Z.sign z < 0 then max_int else Z.popcount z

let reduce_module (m : bmodule) : bmodule * int =
  let ok = Hashtbl.create 16 and bad = Hashtbl.create 16 in
  let widths = Behavioral_const.signal_widths_of_module m in
  List.iter (fun (s : bsignal) ->
    match s.direction, s.stype, s.initial_value with
    | `Internal, BInt { width; _ }, (None | Some (BConst _)) when width >= 3 ->
        (match s.initial_value with
         | Some (BConst { value; _ }) when popcount value > 1 -> ()
         | _ -> Hashtbl.replace ok s.name ())
    | _ -> ()) m.signals;
  let rec scan ~seq = function
    | BAssign { lhs; rhs } ->
        (match rhs with
         | BConst { value; _ } when seq && popcount value <= 1 -> ()
         | _ -> Hashtbl.replace bad lhs ())
    | BIf { then_stmts; else_stmts; _ } -> List.iter (scan ~seq) then_stmts; List.iter (scan ~seq) else_stmts
    | BCase { cases; default; _ } -> List.iter (fun (_, b) -> List.iter (scan ~seq) b) cases; List.iter (scan ~seq) default
    | BBlock l -> List.iter (scan ~seq) l
    | BWhile { body; _ } | BFor { body; _ } -> List.iter (fun s -> match s with BAssign { lhs; _ } -> Hashtbl.replace bad lhs () | _ -> ()) body
    | _ -> () in
  List.iter (function
    | BSequential { body; _ } -> List.iter (scan ~seq:true) body
    | BCombinational { body; _ } -> List.iter (scan ~seq:false) body) m.processes;
  List.iter (fun (i : binstance) ->
    List.iter (fun (_, e) -> match e with BVar v -> Hashtbl.replace bad v () | _ -> ()) i.port_connections)
    m.instances;
  let onehot v = Hashtbl.mem ok v && not (Hashtbl.mem bad v) in
  let n = ref 0 in
  let bit_of r value =
    let i = Z.log2 value in
    match Hashtbl.find_opt widths r with
    | Some w when i < w -> Some (BSlice { signal = BVar r; msb = i; lsb = i })
    | Some _ -> Some (bconst_int 0 1)
    | None -> None in
  let rec rw e =
    match e with
    | BBinOp { op = (BEq | BNe) as op; lhs; rhs; result_type } ->
        let pick = match lhs, rhs with
          | BVar r, BConst { value; _ } | BConst { value; _ }, BVar r
            when onehot r && popcount value = 1 -> bit_of r value
          | _ -> None in
        (match pick with
         | Some b ->
             incr n;
             if op = BEq then b else BUnOp { op = BNot; operand = b; result_type }
         | None -> BBinOp { op; lhs = rw lhs; rhs = rw rhs; result_type })
    | BBinOp r -> BBinOp { r with lhs = rw r.lhs; rhs = rw r.rhs }
    | BUnOp r -> BUnOp { r with operand = rw r.operand }
    | BSelect { array; index } -> BSelect { array = rw array; index = rw index }
    | BSlice r -> BSlice { r with signal = rw r.signal }
    | BConcat l -> BConcat (List.map rw l)
    | BReplicate r -> BReplicate { r with value = rw r.value }
    | BCond { condition; then_val; else_val } ->
        BCond { condition = rw condition; then_val = rw then_val; else_val = rw else_val }
    | BCall r -> BCall { r with args = List.map rw r.args }
    | BVar _ | BConst _ -> e in
  let rec rs = function
    | BAssign { lhs; rhs } -> BAssign { lhs; rhs = rw rhs }
    | BIf { condition; then_stmts; else_stmts } ->
        BIf { condition = rw condition; then_stmts = List.map rs then_stmts; else_stmts = List.map rs else_stmts }
    | BCase { selector; cases; default } ->
        BCase { selector = rw selector; cases = List.map (fun (k, b) -> (k, List.map rs b)) cases
              ; default = List.map rs default }
    | BBlock l -> BBlock (List.map rs l)
    | s -> s in
  let processes = List.map (function
    | BCombinational r -> BCombinational { r with body = List.map rs r.body }
    | BSequential r -> BSequential { r with body = List.map rs r.body }) m.processes in
  { m with processes }, !n

let reduce_program (p : bprogram) : bprogram =
  { p with modules = List.map (fun m ->
      let m', n = reduce_module m in
      if n > 0 then Printf.eprintf "[onehot] %s: %d state compare(s) reduced to a bit\n%!" m.name n;
      m') p.modules }
