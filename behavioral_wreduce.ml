(* Narrow arithmetic and bitwise operators to the bits their consumers
   use (yosys's wreduce).  An assignment `x := e` with x of w bits uses
   only e's low w bits, and the low bits of +, -, *, &, |, ^, ~ depend only
   on the low bits of their operands, so the operator is rebuilt at w bits
   with its operands sliced to w.  `pc := (pc + offset) - 1` with a 24-bit
   pc and 32-bit offset is two 24-bit adders, not two 32-bit ones (6 CARRY4
   each instead of 8).  The demand flows through muxes, slices and the
   parts of a concatenation; comparisons, shifts, reductions, selects and
   calls are barriers (their operands are narrowed only to the operator's
   own width).  GATE_MAP_BIR_NOWREDUCE=1 skips. *)
open Behavioral_ir

let narrow_module (m : bmodule) : bmodule * int =
  let widths = Behavioral_const.signal_widths_of_module m in
  (* Behavioral_const.width_of_expr_with gives a mux the width of its THEN
     arm; the lowering sizes it to the wider arm (`reset ? 32'1 : accu` with
     a 36-bit accu is 36 bits wide), and narrowing to the then arm would
     drop accu[35:32]. *)
  let rec wof e =
    match e with
    | BCond { then_val; else_val; _ } ->
        (match wof then_val, wof else_val with
         | Some a, Some b -> Some (max a b)
         | _ -> None)
    | BConcat parts ->
        let ws = List.map wof parts in
        if List.for_all Option.is_some ws then Some (List.fold_left (fun a w -> a + Option.get w) 0 ws)
        else None
    | BReplicate { count; value } -> Option.map (fun w -> count * w) (wof value)
    | BSlice { msb; lsb; _ } -> Some (msb - lsb + 1)
    | _ -> Behavioral_const.width_of_expr_with widths e in
  let n = ref 0 in
  let retype d = function
    | BInt { signed; _ } -> BInt { width = d; signed }
    | t -> t in
  (* e's low d bits, as an expression of exactly min(d, width e) bits *)
  let fit d e =
    match wof e with
    | Some w when w > d ->
        (match e with
         | BConst { value; _ } -> BConst { value = Z.extract value 0 d; width = d }
         | _ -> BSlice { signal = e; msb = d - 1; lsb = 0 })
    | _ -> e in
  (* bring e to exactly d bits (zero-extend a narrower one): the layout of
     a concatenation or a mux arm must not move *)
  let exact d e =
    match wof e with
    | Some w when w < d -> BConcat [ BConst { value = Z.zero; width = d - w }; e ]
    | _ -> fit d e in
  let rec nw d e =
    match e with
    | BBinOp ({ op = (BAdd | BSub | BMul | BAnd | BOr | BXor); lhs; rhs; result_type } as r) ->
        let w = Behavioral_const.width_of_btype_full result_type in
        let d' = min d w in
        if d' < w then incr n;
        BBinOp { r with lhs = fit d' (nw d' lhs); rhs = fit d' (nw d' rhs)
                      ; result_type = retype d' result_type }
    | BUnOp ({ op = BNot; operand; result_type } as r) ->
        let w = Behavioral_const.width_of_btype_full result_type in
        let d' = min d w in
        if d' < w then incr n;
        BUnOp { r with operand = fit d' (nw d' operand); result_type = retype d' result_type }
    | BCond { condition; then_val; else_val } ->
        (match wof e with
         | Some w ->
             let d' = min d w in
             (* each arm at the mux's new width, as the lowering would size
                them to the wider arm *)
             BCond { condition = self condition
                   ; then_val = exact d' (nw d' then_val)
                   ; else_val = exact d' (nw d' else_val) }
         | None -> BCond { condition = self condition; then_val = self then_val; else_val = self else_val })
    | BSlice { signal; msb; lsb } ->
        (match wof signal with
         | Some w when msb + 1 <= w ->
             let s' = nw (msb + 1) signal in
             (* the slice keeps its position: s' has at least msb+1 bits *)
             BSlice { signal = exact (msb + 1) s'; msb; lsb }
         | _ -> e)
    | BConcat parts ->
        (* MSB first; demand d covers the low parts *)
        let ws = List.map wof parts in
        if List.for_all Option.is_some ws then begin
          let total = List.fold_left (fun a w -> a + Option.get w) 0 ws in
          let d' = min d total in
          (* walk from the LSB end *)
          let rec go rem = function
            | [] -> []
            | (p, Some w) :: rest ->
                let need = min w (max 0 rem) in
                let p' =
                  if need = 0 then BConst { value = Z.zero; width = w }
                  else if need = w then self p
                  else exact w (nw need p) in
                p' :: go (rem - w) rest
            | (p, None) :: rest -> p :: go rem rest in
          let parts' = List.rev (go d' (List.rev (List.combine parts ws))) in
          if List.exists2 (fun a b -> a != b) parts parts' then incr n;
          BConcat parts'
        end else BConcat (List.map self parts)
    | BBinOp ({ lhs; rhs; _ } as r) -> BBinOp { r with lhs = self lhs; rhs = self rhs }
    | BUnOp ({ operand; _ } as r) -> BUnOp { r with operand = self operand }
    | BReplicate ({ value; _ } as r) -> BReplicate { r with value = self value }
    | BSelect { array; index } -> BSelect { array = self array; index = self index }
    | BCall ({ args; _ } as r) -> BCall { r with args = List.map self args }
    | BVar _ | BConst _ -> fit d e
  (* no demand from above: narrow only what the node's own width allows *)
  and self e = match wof e with Some w -> nw w e | None -> e in
  let rec stmt = function
    | BAssign { lhs; rhs } ->
        (match Hashtbl.find_opt widths lhs with
         | Some w -> BAssign { lhs; rhs = exact (min w (Option.value (wof rhs) ~default:w)) (nw w rhs) }
         | None -> BAssign { lhs; rhs = self rhs })
    | BIf { condition; then_stmts; else_stmts } ->
        BIf { condition = self condition; then_stmts = List.map stmt then_stmts
            ; else_stmts = List.map stmt else_stmts }
    | BCase { selector; cases; default } ->
        BCase { selector = self selector
              ; cases = List.map (fun (k, b) -> (k, List.map stmt b)) cases
              ; default = List.map stmt default }
    | BBlock b -> BBlock (List.map stmt b)
    | s -> s in
  let processes = List.map (function
    | BSequential s -> BSequential { s with body = List.map stmt s.body }
    | BCombinational c -> BCombinational { c with body = List.map stmt c.body }) m.processes in
  { m with processes }, !n

let narrow_program (p : bprogram) : bprogram =
  let modules = List.map (fun m ->
    let m', n = narrow_module m in
    if n > 0 then Printf.eprintf "[wreduce] %s: %d operator(s) narrowed\n%!" m.name n;
    m') p.modules in
  { p with modules }
