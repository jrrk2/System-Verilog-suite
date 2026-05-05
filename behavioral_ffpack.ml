(* FF-pack pass (#74).
 *
 * Yosys's gate-level output bit-blasts a multi-bit bus FF into N
 * 1-bit DFF cells driving `<bus>__b<idx>`, plus a combinational
 * reconstruction `<bus> := {…__b<N-1>, …, __b0}`. Until they're
 * re-packed, the miter's `Behavioral_ffrip` produces N independent
 * Q/D pairs (`<bus>__b<idx>__Q` / `…__D`) per side, while the
 * behavioural reference produces a single bus-level pair (`<bus>__Q`
 * / `…__D`). The names don't match, miter fails on interface check
 * even though the designs are equivalent.
 *
 * This pass is the inverse of `Behavioral_ffrip`'s eventual bit
 * splitting at the BIR level — it groups per-bit BSequential
 * processes whose:
 *
 *   - clock, clock_edge, reset, reset_edge, reset_async are equal,
 *   - body is exactly `[BAssign { lhs = "<bus>__b<idx>"; rhs }]`,
 *
 * and when all indices `0…max` of a bus are present, replaces them
 * with a single bus-level BSequential whose body assigns the
 * reconstructed concat to the bus directly. The matching
 * combinational reconstruction process (if any) is dropped. *)

open Behavioral_ir

let bit_suffix_re = Str.regexp
  "\\([A-Za-z_][A-Za-z0-9_.]*\\)__b\\([0-9]+\\)$"

(* Sequential semantics tuple — BSequentials with matching tuples can
 * be packed together. *)
type ff_sem =
  string                                (* clock *)
  * [`Pos | `Neg]                       (* clock_edge *)
  * string option                       (* reset *)
  * [`Pos | `Neg] option                (* reset_edge *)
  * bool                                (* reset_async *)

let pack_module (m : bmodule) : bmodule =
  if Sys.getenv_opt "FFPACK_OFF" <> None then m
  else
    let debug = Sys.getenv_opt "FFPACK_DEBUG" <> None in
    let candidates = List.filter_map (fun proc ->
      match proc with
      | BSequential { clock; clock_edge; reset; reset_edge;
                      reset_async; body = [BAssign { lhs; rhs }]; _ } ->
          if Str.string_match bit_suffix_re lhs 0 then
            let bus = Str.matched_group 1 lhs in
            let idx = int_of_string (Str.matched_group 2 lhs) in
            let sem : ff_sem =
              (clock, clock_edge, reset, reset_edge, reset_async) in
            Some (proc, bus, idx, rhs, sem)
          else None
      | _ -> None
    ) m.processes in

    (* Group by (sem, bus). *)
    let groups : (ff_sem * string,
                  (int * bexpr * bprocess) list) Hashtbl.t =
      Hashtbl.create 8 in
    List.iter (fun (proc, bus, idx, rhs, sem) ->
      let key = (sem, bus) in
      let prev =
        try Hashtbl.find groups key with Not_found -> [] in
      Hashtbl.replace groups key ((idx, rhs, proc) :: prev)
    ) candidates;

    let to_drop = ref [] in
    let packed_procs = ref [] in
    let buses_packed = ref [] in
    Hashtbl.iter (fun (sem, bus) entries ->
      let max_idx = List.fold_left (fun a (i, _, _) -> max a i) 0 entries in
      let needed = max_idx + 1 in
      if debug then
        Printf.eprintf "[ffpack] %s bus=%s |entries|=%d max=%d\n"
          m.name bus (List.length entries) max_idx;
      if List.length entries = needed then begin
        let sorted_msb_first =
          List.sort (fun (a, _, _) (b, _, _) -> compare b a) entries in
        let rhs_list = List.map (fun (_, r, _) -> r) sorted_msb_first in
        let (clock, clock_edge, reset, reset_edge, reset_async) = sem in
        let bool_w = BInt { width = needed; signed = Unsigned } in
        let _ = bool_w in
        let rhs_packed =
          if needed = 1 then List.hd rhs_list else BConcat rhs_list in
        let packed = BSequential {
          name = bus ^ "_pack";
          clock; clock_edge; reset; reset_edge; reset_async;
          body = [BAssign { lhs = bus; rhs = rhs_packed }];
        } in
        packed_procs := packed :: !packed_procs;
        List.iter (fun (_, _, p) -> to_drop := p :: !to_drop) entries;
        buses_packed := bus :: !buses_packed
      end
    ) groups;

    let drop_recon = function
      | BCombinational { body = [BAssign { lhs; rhs = BConcat _ }]; _ }
        when List.mem lhs !buses_packed -> true
      | _ -> false
    in
    let kept = List.filter (fun p ->
      not (List.memq p !to_drop) && not (drop_recon p)
    ) m.processes in
    (* Drop the per-bit signal declarations for buses we've packed —
     * after pack they have no writer (the bit-FFs are gone) and no
     * reader (the recon assign is gone). Leaving them in m.signals
     * makes Behavioral_ffrip promote them to primary inputs in the
     * miter, breaking interface-match against the behavioural side. *)
    let bit_signal_re bus =
      Str.regexp ("^" ^ Str.quote bus ^ "__b[0-9]+$") in
    let kept_signals = List.filter (fun (s : bsignal) ->
      not (List.exists (fun bus ->
        Str.string_match (bit_signal_re bus) s.name 0
      ) !buses_packed)
    ) m.signals in
    { m with signals = kept_signals;
             processes = kept @ List.rev !packed_procs }

let pack_program (p : bprogram) : bprogram =
  { p with modules = List.map pack_module p.modules }
