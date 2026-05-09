(* Post-unroll pass: collapse adjacent @mem_write statements into
   a single write when the unrolled writes form a clean wide-write.

   After Behavioral_unroll runs, an SV `for (int i=0; i<N; i++)
   mem[base+i] <= data[i*W +: W];` body becomes N consecutive
   @mem_write statements in the same block.  Lowering each @mem_write
   independently in behavioral_to_hardcaml chains them through
   Hardcaml's Always.compile_mux (each successive If's default
   becomes the prior If's compiled mux), producing an O(N × depth ×
   W) graph and a comb-loop diagnostic for designs like smollm_layer
   (16 lanes × 7 buffers).

   This pass detects a run of consecutive @mem_write statements with
   shared base index and contiguous const offsets, plus matching
   data slices, and rewrites them as a single statement:
     - if all cells of the array are covered → BAssign(arr, concat)
     - if a contiguous aligned slice → @part_sel_write_up(arr, base*W,
       N*W, data_slice).  The existing @part_sel_write_up handler in
       behavioral_to_hardcaml emits per-slot decode (Vivado-style)
       without the chain.

   Last-write-wins for any duplicate const offsets within a run
   (matches SV non-blocking semantics where the final assign to a
   given location prevails). *)

open Behavioral_ir

(* Decompose an index bexpr into (base_expr, const_offset).
   `mem[chunk*16 + 5]` → idx = BBinOp{BAdd, BBinOp{BMul, chunk, 16}, BConst 5}
                            → (chunk*16, 5) *)
let decompose_idx = function
  | BBinOp { op = BAdd; lhs; rhs = BConst { value; _ } } -> (lhs, value)
  | BBinOp { op = BAdd; lhs = BConst { value; _ }; rhs } -> (rhs, value)
  | BConst { value; width } -> (BConst { value = 0; width }, value)
  | other -> (other, 0)

(* Const-fold trivial arithmetic on bexprs: BAdd/BSub/BMul of two
   BConsts.  Used to resolve dynamic part-select offsets like
   `lane*16` to a literal once unroll has substituted lane. *)
let rec fold_const = function
  | BBinOp r ->
      let l' = fold_const r.lhs and r' = fold_const r.rhs in
      (match r.op, l', r' with
       | BAdd, BConst { value = a; width = w }, BConst { value = b; _ } ->
           BConst { value = a + b; width = w }
       | BSub, BConst { value = a; width = w }, BConst { value = b; _ } ->
           BConst { value = a - b; width = w }
       | BMul, BConst { value = a; width = w }, BConst { value = b; _ } ->
           BConst { value = a * b; width = w }
       | _ -> BBinOp { r with lhs = l'; rhs = r' })
  | other -> other

(* Decompose a data slice bexpr into (signal, lsb, width).
   `eng_out[5*16 +: 16]` after const-fold = BSlice { signal=eng_out;
   msb=95; lsb=80 } → (eng_out, 80, 16).  Also recognises the
   parser's `@part_select_up(signal, base, width)` shape, folding
   `base` if it reduces to a literal once a surrounding for-loop
   variable has been substituted. *)
let decompose_data = function
  | BSlice { signal; msb; lsb } when msb >= lsb ->
      Some (signal, lsb, msb - lsb + 1)
  | BCall { func = "@part_select_up";
            args = [signal; base_e; BConst { value = w; _ }] } when w > 0 ->
      (match fold_const base_e with
       | BConst { value = b; _ } -> Some (signal, b, w)
       | _ -> None)
  | _ -> None

(* Structural equality of bexprs — sufficient for matching same base
   index expression across writes within one block. *)
let rec bexpr_eq a b = match a, b with
  | BVar x, BVar y -> x = y
  | BConst a, BConst b -> a.value = b.value && a.width = b.width
  | BBinOp a, BBinOp b ->
      a.op = b.op && bexpr_eq a.lhs b.lhs && bexpr_eq a.rhs b.rhs
  | BUnOp a, BUnOp b -> a.op = b.op && bexpr_eq a.operand b.operand
  | BSelect a, BSelect b -> bexpr_eq a.array b.array && bexpr_eq a.index b.index
  | BSlice a, BSlice b ->
      bexpr_eq a.signal b.signal && a.msb = b.msb && a.lsb = b.lsb
  | BConcat xs, BConcat ys ->
      List.length xs = List.length ys && List.for_all2 bexpr_eq xs ys
  | _ -> false

(* For a list of (offset, data_signal, data_lsb, data_w) tuples sharing
   the same base and arr, try to detect the wide-write pattern:
   offsets are contiguous [k, k+N-1] and each i-th offset has data slice
   at lsb = (offset_i - k_min) * elem_w, width = elem_w.
   On success returns Some (k_min, N, data_signal, total_w). *)
let try_recognise_block writes elem_w =
  match writes with
  | [] -> None
  | _ ->
      (* Sort by const offset. *)
      let sorted = List.sort (fun (a, _, _, _) (b, _, _, _) -> compare a b) writes in
      (* Dedupe by offset, keeping the LAST (most recent in source
         order — which is the last in the original list, but after
         sort we lose source order; we instead dedupe on input pass
         in the caller). *)
      (match sorted with
       | [] -> None
       | (k_min, _, _, _) :: _ ->
           let n = List.length sorted in
           let expected = List.init n (fun i -> k_min + i) in
           let actual = List.map (fun (off, _, _, _) -> off) sorted in
           if expected <> actual then None
           else
             let first_signal = match sorted with
               | (_, s, _, _) :: _ -> s
               | [] -> assert false in
             let first_lsb = match sorted with
               | (_, _, l, _) :: _ -> l
               | [] -> assert false in
             (* Check all writes use the same data signal, all widths
                are elem_w, and the lsbs follow (first_lsb + i * elem_w). *)
             let ok = List.for_all (fun (off, sig_, lsb, w) ->
               w = elem_w
               && bexpr_eq sig_ first_signal
               && lsb = first_lsb + (off - k_min) * elem_w) sorted in
             if ok then
               Some (k_min, n, first_signal, first_lsb, n * elem_w)
             else None)

(* Walk a flat statement list, finding runs of @mem_write to the
   same array and folding successful merges in place.  The pass
   tracks an array_size table so it can recognise full-overwrites. *)
let merge_run array_widths arr base_expr writes =
  let elem_w =
    try Hashtbl.find array_widths arr with Not_found -> 16
  in
  match try_recognise_block writes elem_w with
  | None -> None
  | Some (k_min, n, data_signal, first_lsb, slice_w) ->
      (* Bit-level base for the array: (base_expr + k_min) * elem_w. *)
      let bit_base =
        let off = if k_min = 0 then base_expr
                  else BBinOp { op = BAdd;
                                lhs = base_expr;
                                rhs = BConst { value = k_min; width = 32 };
                                result_type = BInt { width = 32; signed = Unsigned } } in
        if elem_w = 1 then off
        else BBinOp { op = BMul;
                      lhs = off;
                      rhs = BConst { value = elem_w; width = 32 };
                      result_type = BInt { width = 32; signed = Unsigned } }
      in
      let data_slice =
        if first_lsb = 0 && slice_w = 0 (* shouldn't happen *) then data_signal
        else BSlice { signal = data_signal;
                      msb = first_lsb + slice_w - 1;
                      lsb = first_lsb }
      in
      let stmt = BCallStmt {
        func = "@part_sel_write_up";
        args = [
          BVar arr;
          bit_base;
          BConst { value = slice_w; width = 32 };
          data_slice;
        ];
      } in
      Some (stmt, n)

(* Try to detect a run of consecutive @part_sel_write_up calls to the
   SAME scalar signal that together cover a contiguous bit range.
   When they cover the entire signal width, collapse to a full
   BAssign(target, BConcat parts) per the "all locations written →
   single write" rule.  Returns Some (merged_stmt, count_consumed)
   if successful. *)
let try_merge_part_sel_run target_widths arr writes total_w =
  (* writes : (bit_offset, slice_w, data) list, ordered by appearance. *)
  let _ = target_widths in
  match writes with
  | [] | [_] -> None
  | _ ->
      (* Sort by bit offset ascending. *)
      let sorted = List.sort (fun (a,_,_) (b,_,_) -> compare a b) writes in
      let first_w = match sorted with (_, w, _) :: _ -> w | [] -> 0 in
      (* Require all slices same width and offsets form a contiguous
         arithmetic progression starting at first_off, step first_w. *)
      let n = List.length sorted in
      let first_off = match sorted with (o, _, _) :: _ -> o | [] -> 0 in
      let expected = List.init n (fun i -> first_off + i * first_w) in
      let actual = List.map (fun (o, _, _) -> o) sorted in
      let widths_ok = List.for_all (fun (_, w, _) -> w = first_w) sorted in
      if not widths_ok || actual <> expected then None
      else
        let span = n * first_w in
        if first_off = 0 && span = total_w then
          (* Full overwrite: BAssign target = concat(data_msb-first). *)
          let parts_msb_first =
            List.rev_map (fun (_, _, d) -> d) sorted in
          let rhs = match parts_msb_first with
            | [single] -> single
            | many -> BConcat many in
          Some (BAssign { lhs = arr; rhs }, n)
        else
          (* Partial: emit one @part_sel_write_up over the merged span. *)
          let parts_msb_first =
            List.rev_map (fun (_, _, d) -> d) sorted in
          let merged_data = match parts_msb_first with
            | [single] -> single
            | many -> BConcat many in
          let stmt = BCallStmt {
            func = "@part_sel_write_up";
            args = [
              BVar arr;
              BConst { value = first_off; width = 32 };
              BConst { value = span; width = 32 };
              merged_data;
            ];
          } in
          Some (stmt, n)

(* Bundle of width tables for both arrays (elem_w) and scalars
   (total bit width).  Plumbed through the scan recursion. *)
type widths_ctx = {
  arr_elem : (string, int) Hashtbl.t;     (* array name → elem_w *)
  scalar   : (string, int) Hashtbl.t;     (* scalar name → bit width *)
}

(* Try to fold an @part_sel_write_up's base argument to a literal
   bit offset.  Returns Some lit on success. *)
let fold_offset = function
  | e -> (match fold_const e with
          | BConst { value; _ } -> Some value
          | _ -> None)

(* Scan a flat list of statements and merge adjacent runs of
   @mem_write (to BArray) or @part_sel_write_up (to scalar).  *)
let rec scan_block (ctx : widths_ctx) stmts =
  match stmts with
  | [] -> []
  | (BCallStmt { func = "@mem_write";
                 args = [BVar arr; idx; data] }) as s :: rest ->
      let (base_expr, off) = decompose_idx idx in
      (match decompose_data data with
       | None ->
           let _ = off in
           recurse_one ctx s :: scan_block ctx rest
       | Some (data_signal, data_lsb, data_w) ->
           let writes = ref [(off, data_signal, data_lsb, data_w)] in
           let consumed = ref 1 in
           let tail = ref rest in
           let stop = ref false in
           while not !stop do
             match !tail with
             | (BCallStmt { func = "@mem_write";
                            args = [BVar arr2; idx2; data2] }) :: rest2
               when arr2 = arr ->
                 let (base2, off2) = decompose_idx idx2 in
                 (match decompose_data data2 with
                  | Some (s2, lsb2, w2) when bexpr_eq base2 base_expr ->
                      writes := (off2, s2, lsb2, w2) :: !writes;
                      incr consumed;
                      tail := rest2
                  | _ -> stop := true)
             | _ -> stop := true
           done;
           if !consumed < 2 then
             recurse_one ctx s :: scan_block ctx rest
           else
             let deduped =
               let seen = Hashtbl.create 8 in
               List.filter (fun (off, _, _, _) ->
                 if Hashtbl.mem seen off then false
                 else (Hashtbl.add seen off (); true)
               ) !writes
             in
             match merge_run ctx.arr_elem arr base_expr deduped with
             | None ->
                 recurse_one ctx s :: scan_block ctx rest
             | Some (merged, n_merged) ->
                 if Sys.getenv_opt "MEM_MERGE_DEBUG" <> None then
                   Printf.eprintf "[mem_merge] %s: collapsed %d @mem_writes into one part_sel_write_up\n%!"
                     arr n_merged;
                 merged :: scan_block ctx !tail)
  | (BCallStmt { func = "@part_sel_write_up";
                 args = [BVar arr; base_e; BConst { value = w; _ }; data] })
    as s :: rest when w > 0 ->
      (* Adjacent @part_sel_write_up runs to the SAME scalar register
         with constant base offsets — collapse to a single write
         per the "all locations written → single write" rule.
         Tolerates the writes being in source order; we sort and
         dedupe by offset (last-write-wins) before checking. *)
      (match fold_offset base_e with
       | None -> recurse_one ctx s :: scan_block ctx rest
       | Some off ->
           let writes = ref [(off, w, data)] in
           let consumed = ref 1 in
           let tail = ref rest in
           let stop = ref false in
           while not !stop do
             match !tail with
             | (BCallStmt { func = "@part_sel_write_up";
                            args = [BVar arr2; base2;
                                    BConst { value = w2; _ }; data2] }) :: rest2
               when arr2 = arr && w2 > 0 ->
                 (match fold_offset base2 with
                  | Some off2 ->
                      writes := (off2, w2, data2) :: !writes;
                      incr consumed;
                      tail := rest2
                  | None -> stop := true)
             | _ -> stop := true
           done;
           if !consumed < 2 then
             recurse_one ctx s :: scan_block ctx rest
           else
             (* Dedupe by offset — last write wins (= first in our
                reversed accumulator). *)
             let deduped =
               let seen = Hashtbl.create 8 in
               List.filter (fun (o, _, _) ->
                 if Hashtbl.mem seen o then false
                 else (Hashtbl.add seen o (); true)
               ) !writes
             in
             let total_w =
               try Hashtbl.find ctx.scalar arr with Not_found -> 0
             in
             match try_merge_part_sel_run ctx.scalar arr deduped total_w with
             | None ->
                 recurse_one ctx s :: scan_block ctx rest
             | Some (merged, n_merged) ->
                 if Sys.getenv_opt "MEM_MERGE_DEBUG" <> None then
                   Printf.eprintf "[mem_merge] %s: collapsed %d @part_sel_write_up into one %s\n%!"
                     arr n_merged
                     (match merged with
                      | BAssign _ -> "BAssign"
                      | _ -> "@part_sel_write_up");
                 merged :: scan_block ctx !tail)
  | s :: rest ->
      recurse_one ctx s :: scan_block ctx rest

(* Recurse into nested control-flow constructs. *)
and recurse_one ctx = function
  | BBlock ss -> BBlock (scan_block ctx ss)
  | BIf { condition; then_stmts; else_stmts } ->
      BIf { condition;
            then_stmts = scan_block ctx then_stmts;
            else_stmts = scan_block ctx else_stmts }
  | BCase { selector; cases; default } ->
      BCase { selector;
              cases = List.map (fun (e, ss) -> (e, scan_block ctx ss)) cases;
              default = scan_block ctx default }
  | BWhile { condition; body } ->
      BWhile { condition; body = scan_block ctx body }
  | BFor { init; condition; update; body } ->
      BFor { init; condition; update; body = scan_block ctx body }
  | other -> other

(* Build separate (array elem_w) and (scalar bit-width) tables from a
   module's signal list. *)
let build_widths (signals : bsignal list) : widths_ctx =
  let arr_elem = Hashtbl.create 16 in
  let scalar = Hashtbl.create 16 in
  List.iter (fun (s : bsignal) ->
    match s.stype with
    | BArray { element = BInt { width; _ }; _ } ->
        Hashtbl.add arr_elem s.name width
    | BInt { width; _ } ->
        Hashtbl.add scalar s.name width
    | _ -> ()
  ) signals;
  { arr_elem; scalar }

let count_part_sel m =
  let n = ref 0 in
  let rec walk = function
    | BCallStmt { func = "@part_sel_write_up"; _ } -> incr n
    | BBlock ss -> List.iter walk ss
    | BIf { then_stmts; else_stmts; _ } ->
        List.iter walk then_stmts; List.iter walk else_stmts
    | BCase { cases; default; _ } ->
        List.iter (fun (_, ss) -> List.iter walk ss) cases;
        List.iter walk default
    | BWhile { body; _ } | BFor { body; _ } -> List.iter walk body
    | _ -> ()
  in
  let walk_proc = function
    | BSequential s -> List.iter walk s.body
    | BCombinational c -> List.iter walk c.body in
  List.iter walk_proc m.processes;
  !n

(* Coalesce all BCombinational processes into a single one so
   per-iteration generate-loop bodies (each emitted as its own
   process by the verible converter) end up in one body where
   scan_block can see them as adjacent and merge. *)
let coalesce_combinational processes =
  let combs = List.filter_map (function
    | BCombinational c -> Some (c.name, c.sensitivity, c.body)
    | _ -> None) processes in
  let others = List.filter (function
    | BCombinational _ -> false
    | _ -> true) processes in
  match combs with
  | [] | [_] -> processes
  | (first_name, first_sens, _) :: _ ->
      let merged_body = List.concat_map (fun (_, _, b) -> b) combs in
      BCombinational {
        name = first_name;
        sensitivity = first_sens;
        body = merged_body;
      } :: others

let merge_module (m : bmodule) =
  let widths = build_widths m.signals in
  if Sys.getenv_opt "MEM_MERGE_DEBUG" <> None then begin
    let n_comb = List.length (List.filter (function
      | BCombinational _ -> true | _ -> false) m.processes) in
    let n_seq = List.length (List.filter (function
      | BSequential _ -> true | _ -> false) m.processes) in
    Printf.eprintf "[mem_merge_pre] %s: %d comb proc, %d seq proc\n%!"
      m.name n_comb n_seq
  end;
  let processes = coalesce_combinational m.processes in
  let merge_proc = function
    | BSequential s ->
        BSequential { s with body = scan_block widths s.body }
    | BCombinational c ->
        BCombinational { c with body = scan_block widths c.body }
  in
  let m' = { m with processes = List.map merge_proc processes } in
  if Sys.getenv_opt "MEM_MERGE_DEBUG" <> None then
    Printf.eprintf "[mem_merge_post] %s: %d @part_sel_write_up in BIR\n%!"
      m.name (count_part_sel m');
  m'

let merge_program (p : bprogram) =
  { p with modules = List.map merge_module p.modules }

(* ── @slice_write → full-signal RMW ──────────────────────────────────
   Convert `@slice_write(name, hi, lo, val)` into a BAssign of the
   whole signal whose RHS reads the unwritten bits back via BSlice
   and concats the new slice into place:

       @slice_write(buf, 7, 0, d)   ;; w(buf) = 24
   →   buf := { buf[23:8], d }

   The picorv32 / spimemio pattern that prompted this:

       always @(posedge clk) begin
         if (dout_valid && dout_tag == 1) buffer[ 7: 0] <= dout_data;
         if (dout_valid && dout_tag == 2) buffer[15: 8] <= dout_data;
         if (dout_valid && dout_tag == 3) buffer[23:16] <= dout_data;
       end

   produces three @slice_write statements that — when lowered
   independently to Hardcaml — leave the 24-bit `buffer` only
   partially driven (the concat-of-slice-updates trick can't unify
   them because each is in a different BIf branch).  Result:
   Circuit.create_exn rejects the "unassigned 24-bit wire" and
   spimemio falls into the HIER_SYNTH_STUB_ON_FAIL path.

   Rewriting each slice write as a full-buffer RMW BAssign means
   the lowering sees a single coherent driver per branch — last-
   write-wins under the natural BIR semantics, just as Verilog
   non-blocking assigns specify.  Branches that don't write keep
   the prior value via Hardcaml's Always.Variable wire fallback.

   Const-bounds only — @part_sel_write_up (variable index) keeps
   its existing behavioral_to_hardcaml path.                      *)

let rewrite_slice_write widths = function
  | BCallStmt { func = "@slice_write";
                args = [BVar name;
                        BConst { value = hi; _ };
                        BConst { value = lo; _ };
                        rhs] } as orig ->
      (match Hashtbl.find_opt widths.scalar name with
       | Some w when hi >= lo && hi < w && lo >= 0 ->
           let top =
             if hi + 1 <= w - 1
             then [BSlice { signal = BVar name; msb = w - 1; lsb = hi + 1 }]
             else [] in
           let bot =
             if lo > 0
             then [BSlice { signal = BVar name; msb = lo - 1; lsb = 0 }]
             else [] in
           let parts = top @ [rhs] @ bot in
           let new_rhs = match parts with
             | [single] -> single
             | many -> BConcat many
           in
           BAssign { lhs = name; rhs = new_rhs }
       | _ -> orig)
  | other -> other

let rec rewrite_slice_writes widths stmt =
  let rstmt = rewrite_slice_writes widths in
  let rstmts = List.map rstmt in
  match rewrite_slice_write widths stmt with
  | BBlock ss -> BBlock (rstmts ss)
  | BIf r ->
      BIf { r with
            then_stmts = rstmts r.then_stmts;
            else_stmts = rstmts r.else_stmts }
  | BCase r ->
      BCase { r with
              cases   = List.map (fun (k, ss) -> (k, rstmts ss)) r.cases;
              default = rstmts r.default }
  | BWhile r -> BWhile { r with body = rstmts r.body }
  | BFor   r -> BFor   { r with body = rstmts r.body }
  | other -> other

let merge_slice_writes_module (m : bmodule) =
  let widths = build_widths m.signals in
  let walk_proc = function
    | BSequential s ->
        BSequential { s with body = List.map (rewrite_slice_writes widths) s.body }
    | BCombinational c ->
        BCombinational { c with body = List.map (rewrite_slice_writes widths) c.body }
  in
  { m with processes = List.map walk_proc m.processes }

let merge_slice_writes_program (p : bprogram) =
  { p with modules = List.map merge_slice_writes_module p.modules }
