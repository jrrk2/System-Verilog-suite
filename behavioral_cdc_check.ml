(* Clock-domain BOUNDARY CHECKER.
 *
 * A module boundary is only safe to freeze as a hard macro if it obeys the
 * rule stated at the top of ethsoc/eth_macro.sv, the hand-written exemplar:
 *
 *     Boundary discipline: no same-clock FF->FF arc crosses this module's
 *     interface.
 *
 * Everything that DOES cross there is async by construction -- gray pointers
 * read through a 2-flop synchroniser on the far side, distributed-RAM read
 * ports whose stability is guaranteed by the FIFO occupancy argument, and
 * quasi-static status bits.
 *
 * The reason to check rather than assume: a same-clock FF->FF arc that crosses
 * a frozen boundary is a SILENT timing break.  The macro closes timing on its
 * own, the parent closes timing on its own, and the arc between them is timed
 * by nobody.  Nothing fails loudly; the board is just wrong.  That is the exact
 * class of defect this project has repeatedly paid for, so the splitter must
 * REFUSE a cut it cannot prove safe rather than emit a boundary that merely
 * looks like eth_macro.sv.
 *
 * This module answers one question:
 *
 *     check_boundary prog ~macro:"eth_macro"  ->  crossing list
 *
 * WHAT AN ENDPOINT IS.  The naive reading -- "is this port driven by a register
 * in this module" -- is worthless, and calibrating against eth_macro is what
 * proved it: every one of eth_macro's ports is driven through a CHILD INSTANCE,
 * so a process-local scan sees no register on either end and pronounces all 23
 * crossings safe without having looked at anything.  A launching register is
 * whatever reaches the port through a cone of COMBINATIONAL logic, wherever in
 * the hierarchy it happens to live.  So this pass computes, per signal:
 *
 *   launch  -- the clock domains of registers that reach it combinationally
 *   capture -- the clock domains of registers it combinationally reaches
 *
 * as a fixpoint over a combinational-edge graph, where registers terminate the
 * cone and child instances contribute an edge/seed summary computed
 * recursively.  A port whose cone is open at the module boundary carries an
 * [Ext] marker naming the port it escapes through; the boundary check
 * substitutes the far side's cone for it, which is what makes a combinational
 * feed-through (macro input -> comb -> macro output) resolve to the registers
 * that actually launch and capture it.
 *
 * THREE VERDICTS, NOT TWO.  "Not proven unsafe" is not the same as safe, and
 * conflating them is how the vacuous version passed.  A crossing whose domain
 * cannot be named -- a black-box instance, a clock that is internal to a child
 * and never appears on its interface -- is [Unproven], reported separately, and
 * the splitter must refuse it exactly as it refuses [Unsafe].
 *
 * Validate against ethsoc/eth_macro.sv before trusting this on a new cut: that
 * boundary is known good, so the checker must return no violations for it --
 * AND must show real domains at the endpoints while doing so.  A checker
 * calibrated only on the cut it is about to bless is worthless.
 *)

open Behavioral_ir

module SS = Set.Make (String)

(* A clock domain, named in the namespace of the module being analysed.
   [Opq] is a domain we could not name here (internal to a child, or behind a
   black box); it is deliberately NOT equal to any named clock, and a
   comparison involving one yields [Unproven] rather than a verdict.
   [Ext] means the cone leaves through this module's own port and the domain
   is whatever the far side supplies. *)
type dom =
  | Clk of string
  | Opq of string
  | Ext of string

module DS = Set.Make (struct
  type t = dom
  let compare = compare
end)

let dom_str = function
  | Clk c -> c
  | Opq o -> "?" ^ o
  | Ext p -> "^" ^ p

(* A domain set can run to dozens of entries when a black box contributes one
   unnameable domain per pin; print enough to identify the culprit and say how
   many more there are rather than filling the terminal. *)
let ds_str ?(max = 3) s =
  if DS.is_empty s then "-"
  else
    let all = List.map dom_str (DS.elements s) in
    let n = List.length all in
    if n <= max then String.concat "," all
    else
      String.concat "," (List.filteri (fun i _ -> i < max) all)
      ^ Printf.sprintf ",+%d" (n - max)

type verdict = Safe | Unproven | Unsafe

type crossing = {
  x_signal : string;              (* the macro's port name *)
  x_in_launch : DS.t;             (* registers inside that reach the port *)
  x_in_capture : DS.t;            (* registers inside the port reaches *)
  x_out_launch : DS.t;            (* registers outside that reach the actual *)
  x_out_capture : DS.t;           (* registers outside the actual reaches *)
  x_verdict : verdict;
  x_why : string;
}

(* --- clock-name canonicalisation ----------------------------------------- *)

(* Two names for one clock must compare EQUAL or a same-clock arc reads as a
   CDC and we bless a broken cut.  Union names joined by a plain wire alias
   (`assign a = b`) and by the clock buffers, whose whole purpose is to hand
   the same clock a new name.  A port name wins the representative election so
   that cross-boundary translation has something on the interface to map. *)
let clock_buffers =
  SS.of_list
    [ "BUFG"; "BUFGCE"; "BUFH"; "BUFHCE"; "BUFR"; "BUFIO"; "BUFMR";
      "IBUF"; "IBUFG"; "IBUFDS"; "OBUF" ]

(* A tie-off is not a clock.  A netlist ties unused clock pins to the constant
   nets, so letting one into the alias graph merges every tied-off pin into one
   "domain" -- and since a PORT name wins the representative election, that
   domain gets named after whatever port happens to sit in the same class.  On
   the flattened ethmin that produced a clock domain called `an_enable`, an
   output the netlist drives with `assign an_enable = <const0>`, holding 14
   instances whose only connection to each other was being tied off. *)
let is_const_net n =
  let base =
    match String.rindex_opt n '.' with
    | Some i -> String.sub n (i + 1) (String.length n - i - 1)
    | None -> n
  in
  let l = String.lowercase_ascii base in
  l = "gnd" || l = "vcc" || l = "<const0>" || l = "<const1>"
  || (String.length l >= 6 && String.sub l 0 6 = "<const")

let clock_aliases (m : bmodule) =
  let parent : (string, string) Hashtbl.t = Hashtbl.create 16 in
  let rec find x =
    match Hashtbl.find_opt parent x with
    | None -> x
    | Some p when p = x -> x
    | Some p ->
        let r = find p in
        Hashtbl.replace parent x r;
        r
  in
  let ports =
    SS.of_list
      (List.filter_map
         (fun s -> if s.direction <> `Internal then Some s.name else None)
         m.signals)
  in
  (* Which of two names for the same clock to keep.  A port wins, because a
     clock named on an interface is the one a constraint can refer to.  Failing
     that, prefer the name with the fewest hierarchy separators and then the
     shorter one: after flattening, one clock carries both `eth_clk` and
     `eth.i_pcs_pma.inst.core_clocking_i/clkout0`, and a plain lexicographic
     tie-break picks the second ('.' sorts below '_').  The user then has to
     write THAT into the period spec, which is a bad enough interface to be
     worth four lines of comparison. *)
  let depth n =
    String.fold_left (fun a c -> if c = '.' || c = '/' then a + 1 else a) 0 n
  in
  let nicer a b =
    let da = depth a and db = depth b in
    if da <> db then da < db
    else if String.length a <> String.length b then
      String.length a < String.length b
    else a <= b
  in
  let union a b =
    if is_const_net a || is_const_net b then () else
    let ra = find a and rb = find b in
    if ra <> rb then begin
      let keep, drop =
        match SS.mem ra ports, SS.mem rb ports with
        | true, false -> ra, rb
        | false, true -> rb, ra
        | _ -> if nicer ra rb then ra, rb else rb, ra
      in
      Hashtbl.replace parent keep keep;
      Hashtbl.replace parent drop keep
    end
  in
  List.iter
    (function
      | BCombinational { body; _ } ->
          List.iter
            (function BAssign { lhs; rhs = BVar r } -> union lhs r | _ -> ())
            body
      | BSequential _ -> ())
    m.processes;
  (* a clock buffer is an alias, not logic: I and O are the same clock *)
  List.iter
    (fun i ->
      if SS.mem i.module_name clock_buffers then
        match
          List.assoc_opt "I" i.port_connections,
          List.assoc_opt "O" i.port_connections
        with
        | Some (BVar a), Some (BVar b) -> union a b
        | _ -> ())
    m.instances;
  find

(* --- per-module cone analysis -------------------------------------------- *)

(* Per-port cones, in the analysed module's own namespace. *)
type summary = { s_launch : (string * DS.t) list; s_capture : (string * DS.t) list }

type analysis = {
  a_summary : summary;
  a_launch : (string, DS.t) Hashtbl.t;   (* every signal, not just ports *)
  a_capture : (string, DS.t) Hashtbl.t;
  a_canon : string -> string;
}

let names_of_expr e =
  Behavioral_dce.StringSet.fold (fun n a -> SS.add n a)
    (Behavioral_dce.collect_uses_expr e) SS.empty

let hadd tbl k v =
  let cur = match Hashtbl.find_opt tbl k with Some s -> s | None -> DS.empty in
  Hashtbl.replace tbl k (DS.union cur v)

let memo : (string, analysis) Hashtbl.t = Hashtbl.create 64

(* Propagate a seeded domain map along combinational edges to a fixpoint.
   [edges] maps a signal to the signals it feeds; sets only grow, so the
   iteration terminates even through a combinational loop. *)
let saturate (edges : (string, SS.t) Hashtbl.t) (tbl : (string, DS.t) Hashtbl.t) =
  let work = Queue.create () in
  Hashtbl.iter (fun k _ -> Queue.add k work) tbl;
  while not (Queue.is_empty work) do
    let s = Queue.pop work in
    let ds = match Hashtbl.find_opt tbl s with Some d -> d | None -> DS.empty in
    match Hashtbl.find_opt edges s with
    | None -> ()
    | Some outs ->
        SS.iter
          (fun d ->
            let before =
              match Hashtbl.find_opt tbl d with Some x -> x | None -> DS.empty
            in
            let after = DS.union before ds in
            if not (DS.equal before after) then begin
              Hashtbl.replace tbl d after;
              Queue.add d work
            end)
          outs
  done

let rec analyse ~(lookup : string -> bmodule option) ?(exclude = "") ?(depth = 0)
    (m : bmodule) : analysis =
  match (if exclude = "" then Hashtbl.find_opt memo m.name else None) with
  | Some a -> a
  | None ->
      let canon = clock_aliases m in
      let launch : (string, DS.t) Hashtbl.t = Hashtbl.create 128 in
      let capture : (string, DS.t) Hashtbl.t = Hashtbl.create 128 in
      let fwd : (string, SS.t) Hashtbl.t = Hashtbl.create 128 in
      let rev : (string, SS.t) Hashtbl.t = Hashtbl.create 128 in
      let edge u d =
        if u <> d then begin
          let cur = match Hashtbl.find_opt fwd u with Some s -> s | None -> SS.empty in
          Hashtbl.replace fwd u (SS.add d cur);
          let cur = match Hashtbl.find_opt rev d with Some s -> s | None -> SS.empty in
          Hashtbl.replace rev d (SS.add u cur)
        end
      in
      (* --- processes --- *)
      List.iter
        (fun p ->
          let uses, defs = Behavioral_dce.collect_uses_defs_process p in
          match p with
          | BCombinational _ ->
              Behavioral_dce.StringSet.iter
                (fun d ->
                  Behavioral_dce.StringSet.iter (fun u -> edge u d) uses)
                defs
          | BSequential { clock; reset; blocking_vars; _ } ->
              (* the collector counts clock/reset as uses so DCE keeps them --
                 correct there, wrong here: a clock net is not a data arc into
                 this domain, and counting it makes every clock port look like
                 a captured endpoint. *)
              let uses = Behavioral_dce.StringSet.remove clock uses in
              let uses =
                match reset with
                | Some r -> Behavioral_dce.StringSet.remove r uses
                | None -> uses
              in
              let dom = DS.singleton (Clk (canon clock)) in
              Behavioral_dce.StringSet.iter
                (fun d ->
                  (* an SV blocking `=` inside always_ff is an in-cycle
                     combinational temp, not a register: it does not terminate
                     the cone. *)
                  if List.mem d blocking_vars then
                    Behavioral_dce.StringSet.iter (fun u -> edge u d) uses
                  else hadd launch d dom)
                defs;
              Behavioral_dce.StringSet.iter (fun u -> hadd capture u dom) uses)
        m.processes;
      (* --- instances --- *)
      List.iter
        (fun i ->
          if i.inst_name = exclude then ()
          else
            let actual p =
              match List.assoc_opt p i.port_connections with
              | Some e -> names_of_expr e
              | None -> SS.empty
            in
            match (if depth > 32 then None else lookup i.module_name) with
            | Some child when not (SS.mem i.module_name clock_buffers) ->
                let ca = analyse ~lookup ~depth:(depth + 1) child in
                (* a clock named inside the child is only nameable out here if
                   it reaches the child's interface *)
                let translate c =
                  let c' = ca.a_canon c in
                  match List.assoc_opt c' i.port_connections with
                  | Some e -> (
                      match SS.elements (names_of_expr e) with
                      | [ n ] -> Clk (canon n)
                      | _ -> Opq (i.inst_name ^ "/" ^ c'))
                  | None -> Opq (i.inst_name ^ "/" ^ c')
                in
                let lift d =
                  match d with
                  | Clk c -> Some (translate c)
                  | Opq o -> Some (Opq (i.inst_name ^ "/" ^ o))
                  | Ext _ -> None
                in
                List.iter
                  (fun (p, ds) ->
                    let pn = actual p in
                    DS.iter
                      (fun d ->
                        match d with
                        | Ext q ->
                            (* comb feed-through q -> p inside the child *)
                            SS.iter (fun a -> SS.iter (fun b -> edge a b) pn)
                              (actual q)
                        | _ -> (
                            match lift d with
                            | Some d' ->
                                SS.iter (fun n -> hadd launch n (DS.singleton d')) pn
                            | None -> ()))
                      ds)
                  ca.a_summary.s_launch;
                List.iter
                  (fun (p, ds) ->
                    let pn = actual p in
                    DS.iter
                      (fun d ->
                        match d with
                        | Ext q ->
                            SS.iter (fun a -> SS.iter (fun b -> edge a b) (actual q)) pn
                        | _ -> (
                            match lift d with
                            | Some d' ->
                                SS.iter (fun n -> hadd capture n (DS.singleton d')) pn
                            | None -> ()))
                      ds)
                  ca.a_summary.s_capture;
                (* the child's own clock inputs are clock nets out here too:
                   keep the alias so both sides name the domain identically *)
                ()
            | Some _ | None ->
                if SS.mem i.module_name clock_buffers then
                  (* pure rename of a clock: a comb edge each way keeps any
                     data path through it intact, and clock_aliases has
                     already merged the two names *)
                  List.iter
                    (fun (_, e) ->
                      SS.iter
                        (fun a ->
                          List.iter
                            (fun (_, e2) ->
                              SS.iter (fun b -> edge a b) (names_of_expr e2))
                            i.port_connections)
                        (names_of_expr e))
                    i.port_connections
                else
                  (* Black box.  We do not know its port directions, let alone
                     its internal clocking, so every pin both launches from and
                     captures into a domain we cannot name.  That makes any
                     boundary through it [Unproven] -- which is the point. *)
                  let o = Opq (i.inst_name ^ ":" ^ i.module_name) in
                  List.iter
                    (fun (_, e) ->
                      SS.iter
                        (fun n ->
                          hadd launch n (DS.singleton o);
                          hadd capture n (DS.singleton o))
                        (names_of_expr e))
                    i.port_connections)
        m.instances;
      (* --- the module's own ports open the cone --- *)
      List.iter
        (fun s ->
          match s.direction with
          | `Input -> hadd launch s.name (DS.singleton (Ext s.name))
          | `Output -> hadd capture s.name (DS.singleton (Ext s.name))
          (* bidirectional: it opens the cone at BOTH ends *)
          | `Inout ->
              hadd launch s.name (DS.singleton (Ext s.name));
              hadd capture s.name (DS.singleton (Ext s.name))
          | `Internal -> ())
        m.signals;
      saturate fwd launch;
      saturate rev capture;
      let port_names =
        List.filter_map
          (fun s -> if s.direction <> `Internal then Some s.name else None)
          m.signals
      in
      let get tbl p =
        match Hashtbl.find_opt tbl p with Some d -> d | None -> DS.empty
      in
      let summary =
        {
          s_launch =
            List.map
              (fun p -> (p, DS.remove (Ext p) (get launch p)))
              port_names;
          s_capture =
            List.map
              (fun p -> (p, DS.remove (Ext p) (get capture p)))
              port_names;
        }
      in
      let a =
        { a_summary = summary; a_launch = launch; a_capture = capture; a_canon = canon }
      in
      if exclude = "" then Hashtbl.replace memo m.name a;
      a

(* --- the check ------------------------------------------------------------ *)

let concrete_clash a b =
  DS.exists (function Clk _ as d -> DS.mem d b | _ -> false) a

let has_opq s = DS.exists (function Opq _ -> true | _ -> false) s
let has_dom s = DS.exists (function Clk _ | Opq _ -> true | _ -> false) s

let check_boundary (prog : bprogram) ~(macro : string) : crossing list =
  Hashtbl.reset memo;
  (* Give the Xilinx primitives their bodies first.  A netlist-level design is
     mostly LUT/FD/CARRY4 instances, and without models every one of them is a
     black box: the cone stops dead at the first LUT and the whole boundary
     reports [Unproven].  These models are the difference between an analysis
     and a shrug. *)
  let prog = Xil_prim_models.augment_program prog in
  let lookup n = List.find_opt (fun m -> m.name = n) prog.modules in
  match lookup macro with
  | None -> failwith ("check_boundary: no module named '" ^ macro ^ "'")
  | Some mmod ->
      let parents =
        List.filter
          (fun p -> List.exists (fun i -> i.module_name = macro) p.instances)
          prog.modules
      in
      if parents = [] then
        failwith ("check_boundary: '" ^ macro ^ "' is not instantiated anywhere");
      let msum = (analyse ~lookup mmod).a_summary in
      List.concat_map
        (fun parent ->
          List.concat_map
            (fun inst ->
              if inst.module_name <> macro then []
              else begin
                (* The parent must be read WITHOUT this instance: with it, the
                   macro's own contribution comes back round as the far-side
                   driver and every arc looks like a loop through itself. *)
                let pa = analyse ~lookup ~exclude:inst.inst_name parent in
                let actual p =
                  match List.assoc_opt p inst.port_connections with
                  | Some e -> names_of_expr e
                  | None -> SS.empty
                in
                let outside tbl p =
                  SS.fold
                    (fun n acc ->
                      DS.union acc
                        (match Hashtbl.find_opt tbl n with
                         | Some d -> d
                         | None -> DS.empty))
                    (actual p) DS.empty
                in
                let translate c =
                  let c' = (analyse ~lookup mmod).a_canon c in
                  match List.assoc_opt c' inst.port_connections with
                  | Some e -> (
                      match SS.elements (names_of_expr e) with
                      | [ n ] -> Clk (pa.a_canon n)
                      | _ -> Opq (inst.inst_name ^ "/" ^ c'))
                  | None -> Opq (inst.inst_name ^ "/" ^ c')
                in
                (* Lift a macro-side cone into the parent's namespace.  An
                   [Ext q] is a combinational feed-through: the arc's real
                   endpoint is whatever the parent has on the far side of the
                   macro's OTHER port q.  The parent was analysed without this
                   instance, so one substitution closes it -- no path can lead
                   back through the macro. *)
                let lift tbl_out ds =
                  DS.fold
                    (fun d acc ->
                      match d with
                      | Clk c -> DS.add (translate c) acc
                      | Opq o -> DS.add (Opq (inst.inst_name ^ "/" ^ o)) acc
                      | Ext q -> DS.union acc (outside tbl_out q))
                    ds DS.empty
                in
                List.map
                  (fun (port, _) ->
                    let in_launch =
                      lift pa.a_launch
                        (try List.assoc port msum.s_launch with Not_found -> DS.empty)
                    in
                    let in_capture =
                      lift pa.a_capture
                        (try List.assoc port msum.s_capture with Not_found -> DS.empty)
                    in
                    let out_launch = outside pa.a_launch port in
                    let out_capture = outside pa.a_capture port in
                    let unsafe_out = concrete_clash in_launch out_capture in
                    let unsafe_in = concrete_clash out_launch in_capture in
                    let unproven =
                      (has_opq in_launch && has_dom out_capture)
                      || (has_opq out_capture && has_dom in_launch)
                      || (has_opq out_launch && has_dom in_capture)
                      || (has_opq in_capture && has_dom out_launch)
                    in
                    let verdict =
                      if unsafe_out || unsafe_in then Unsafe
                      else if unproven then Unproven
                      else Safe
                    in
                    let why =
                      if unsafe_out || unsafe_in then
                        Printf.sprintf
                          "SAME-CLOCK FF->FF across the boundary (%s) -- timed by neither side"
                          (if unsafe_out then "out" else "in")
                      else if unproven then
                        "domain not nameable on one end (black box / child-internal clock) \
                         -- cannot prove this arc is a CDC"
                      else if has_dom in_launch && has_dom out_capture then
                        "FF->FF but different clocks (CDC)"
                      else if
                        DS.is_empty in_launch && DS.is_empty in_capture
                        && DS.is_empty out_launch && DS.is_empty out_capture
                      then "no logic on either end"
                      else "no register on one end"
                    in
                    {
                      x_signal = port;
                      x_in_launch = in_launch;
                      x_in_capture = in_capture;
                      x_out_launch = out_launch;
                      x_out_capture = out_capture;
                      x_verdict = verdict;
                      x_why = why;
                    })
                  msum.s_launch
              end)
            parent.instances)
        parents

let verdict_str = function
  | Safe -> "ok"
  | Unproven -> "UNPROVEN"
  | Unsafe -> "UNSAFE"

let report (xs : crossing list) =
  let bad = List.filter (fun x -> x.x_verdict <> Safe) xs in
  let n_unsafe = List.length (List.filter (fun x -> x.x_verdict = Unsafe) xs) in
  let n_unproven = List.length (List.filter (fun x -> x.x_verdict = Unproven) xs) in
  Printf.printf "[cdc-check] %d crossing(s), %d unsafe, %d unproven\n"
    (List.length xs) n_unsafe n_unproven;
  List.iter
    (fun x ->
      Printf.printf "  %-8s %-24s in:launch=%-20s capture=%-20s out:launch=%-20s capture=%-20s %s\n"
        (verdict_str x.x_verdict) x.x_signal (ds_str x.x_in_launch)
        (ds_str x.x_in_capture) (ds_str x.x_out_launch) (ds_str x.x_out_capture)
        x.x_why)
    xs;
  bad
