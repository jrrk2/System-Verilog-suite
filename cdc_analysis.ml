(* Clock-domain-crossing (CDC) analysis pass (#137).
 *
 * Pure analysis over a bmodule: identify FF→FF data paths where the
 * source and destination clocks differ, and classify each crossing as
 * Unsynchronised, TwoFf (a 2-FF synchroniser chain in the destination
 * domain), or Unknown.
 *
 * Domains derive from the `clock` field on each BSequential — set by
 * the elaborator from always-block structure, never from a name
 * heuristic. This pass just compares clock strings; if the elaborator
 * has assigned the same name to two distinct clocks (e.g. a renamed
 * gated clock buffered through a different process), the pass will
 * under-report. That's acceptable for now — fixing it belongs in
 * elaboration, not here. *)

open Behavioral_ir

type sync_kind = Unsynchronised | TwoFf | Unknown

type cdc_edge = {
  src_signal : string;
  src_clock : string;
  dst_signal : string;
  dst_clock : string;
  sync : sync_kind;
}

type cdc_report = {
  module_name : string;
  domains : string list;
  edges : cdc_edge list;
}

let rec read_vars (e : bexpr) (acc : string list) : string list =
  match e with
  | BVar n -> n :: acc
  | BConst _ -> acc
  | BBinOp { lhs; rhs; _ } -> read_vars rhs (read_vars lhs acc)
  | BUnOp { operand; _ } -> read_vars operand acc
  | BSelect { array; index } -> read_vars index (read_vars array acc)
  | BSlice { signal; _ } -> read_vars signal acc
  | BConcat es -> List.fold_left (fun a e -> read_vars e a) acc es
  | BReplicate { value; _ } -> read_vars value acc
  | BCond { condition; then_val; else_val } ->
      read_vars else_val (read_vars then_val (read_vars condition acc))
  | BCall { args; _ } -> List.fold_left (fun a e -> read_vars e a) acc args

(* Walk a stmt list; return (assign_pairs, control_reads). Control reads
 * are vars referenced in if/case/while conditions inside this body;
 * they reach the FF data input through the mux tree generated for the
 * conditional, so they count as crossing reads. *)
let walk_stmts (stmts : bstmt list) : (string * bexpr) list * string list =
  let assigns = ref [] and ctrl_reads = ref [] in
  let rec go = function
    | BAssign { lhs; rhs } ->
        assigns := (lhs, rhs) :: !assigns
    | BIf { condition; then_stmts; else_stmts } ->
        ctrl_reads := read_vars condition !ctrl_reads;
        List.iter go then_stmts;
        List.iter go else_stmts
    | BCase { selector; cases; default } ->
        ctrl_reads := read_vars selector !ctrl_reads;
        List.iter (fun (e, ss) ->
          ctrl_reads := read_vars e !ctrl_reads;
          List.iter go ss) cases;
        List.iter go default
    | BWhile { condition; body } ->
        ctrl_reads := read_vars condition !ctrl_reads;
        List.iter go body
    | BFor { init; condition; update; body } ->
        go init;
        ctrl_reads := read_vars condition !ctrl_reads;
        go update;
        List.iter go body
    | BBlock ss -> List.iter go ss
    | BCallStmt { args; _ } ->
        List.iter (fun e -> ctrl_reads := read_vars e !ctrl_reads) args
    | BReturn (Some e) -> ctrl_reads := read_vars e !ctrl_reads
    | BReturn None -> ()
  in
  List.iter go stmts;
  (List.rev !assigns, !ctrl_reads)

(* Map lhs -> (clock, body). If a name is driven by more than one
 * BSequential we keep the first; that's pathological and orthogonal to
 * what we're trying to detect. *)
let build_domain_map (m : bmodule) =
  let tbl : (string, string * bstmt list) Hashtbl.t = Hashtbl.create 64 in
  List.iter (function
    | BSequential { clock; body; _ } ->
        let (assigns, _) = walk_stmts body in
        List.iter (fun (lhs, _) ->
          if not (Hashtbl.mem tbl lhs) then
            Hashtbl.add tbl lhs (clock, body)) assigns
    | _ -> ()
  ) m.processes;
  tbl

(* Is `dst_lhs <= BVar src_signal` one of the literal assignments in
 * `body`? (Outside any if/case — that would be a gated capture, not a
 * pure pass-through.) *)
let direct_passthrough (body : bstmt list)
                       (dst_lhs : string)
                       (src_signal : string) : bool =
  List.exists (function
    | BAssign { lhs; rhs = BVar n } -> lhs = dst_lhs && n = src_signal
    | _ -> false) body

let classify_sync (domain_tbl : (string, string * bstmt list) Hashtbl.t)
                  (dst_clock : string)
                  (dst_lhs : string)
                  (src_signal : string) : sync_kind =
  match Hashtbl.find_opt domain_tbl dst_lhs with
  | None -> Unknown
  | Some (_, body) ->
      if direct_passthrough body dst_lhs src_signal then
        Unsynchronised
      else begin
        let intermediates =
          List.filter_map (function
            | BAssign { lhs; rhs = BVar n } when lhs = dst_lhs -> Some n
            | _ -> None) body in
        let two_ff = List.exists (fun mid ->
          match Hashtbl.find_opt domain_tbl mid with
          | Some (clk, mbody) when clk = dst_clock ->
              direct_passthrough mbody mid src_signal
          | _ -> false) intermediates in
        if two_ff then TwoFf else Unsynchronised
      end

let analyse (m : bmodule) : cdc_report =
  let tbl = build_domain_map m in
  let seen : (string * string * string * string, unit) Hashtbl.t =
    Hashtbl.create 64 in
  let edges = ref [] in
  let domains = ref [] in
  let add_domain c =
    if not (List.mem c !domains) then domains := c :: !domains in
  List.iter (function
    | BSequential { clock = dst_clock; body; _ } ->
        add_domain dst_clock;
        let (assigns, ctrl_reads) = walk_stmts body in
        let process_read dst_lhs r =
          match Hashtbl.find_opt tbl r with
          | Some (src_clock, _) when src_clock <> dst_clock ->
              let key = (r, src_clock, dst_lhs, dst_clock) in
              if not (Hashtbl.mem seen key) then begin
                Hashtbl.add seen key ();
                let sync = classify_sync tbl dst_clock dst_lhs r in
                edges := { src_signal = r; src_clock;
                           dst_signal = dst_lhs;
                           dst_clock; sync } :: !edges
              end
          | _ -> ()
        in
        List.iter (fun (dst_lhs, rhs) ->
          List.iter (process_read dst_lhs) (read_vars rhs []);
          List.iter (process_read dst_lhs) ctrl_reads
        ) assigns
    | _ -> ()
  ) m.processes;
  { module_name = m.name;
    domains = List.rev !domains;
    edges = List.rev !edges }

let format_sync = function
  | Unsynchronised -> "UNSYNC"
  | TwoFf -> "2FF"
  | Unknown -> "?"

let format_report (r : cdc_report) : string =
  let buf = Buffer.create 512 in
  Buffer.add_string buf (Printf.sprintf "Module: %s\n" r.module_name);
  Buffer.add_string buf (Printf.sprintf "Clock domains (%d):"
                           (List.length r.domains));
  List.iter (fun c -> Buffer.add_string buf (" " ^ c)) r.domains;
  Buffer.add_char buf '\n';
  Buffer.add_string buf (Printf.sprintf "CDC edges (%d):\n"
                           (List.length r.edges));
  if r.edges = [] then Buffer.add_string buf "  (none)\n"
  else
    List.iter (fun e ->
      Buffer.add_string buf
        (Printf.sprintf "  [%-6s] %s @%s --> %s @%s\n"
           (format_sync e.sync) e.src_signal e.src_clock
           e.dst_signal e.dst_clock)
    ) r.edges;
  Buffer.contents buf
