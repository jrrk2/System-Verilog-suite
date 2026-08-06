(* PER-CLOCK-DOMAIN FLATTENER.
 *
 * Re-draws a design's module boundaries along its CLOCK DOMAINS instead of
 * along whatever hierarchy the source happened to be written with.  Flatten
 * first, then split: the output is one module per clock domain, plus a top
 * that instantiates them and keeps whatever belongs to no single domain (the
 * global resources -- MMCM, BUFG, GT, IO -- and any logic that spans domains).
 *
 * WHY.  A domain is the unit that can be placed, routed, timed and FROZEN on
 * its own: ethsoc/eth_macro.sv is exactly this cut made by hand, built once by
 * Vivado OOC and imported as a placed+routed macro.  Doing it by hand is
 * error-prone in a way that does not announce itself, which is the whole
 * reason for the checker below.
 *
 * THE RULE THE CUT MUST OBEY.  No same-clock FF->FF arc may cross a generated
 * boundary.  Such an arc is timed by nobody: each side closes timing on its
 * own and the arc between them is never analysed, so nothing fails and the
 * board is simply wrong.  Splitting BY clock domain ought to make that
 * impossible by construction -- every register on a clock lands in the same
 * module -- but "ought to" is not a proof, and the combinational logic is
 * where it can go wrong: a cone assigned to the wrong side puts two same-clock
 * registers on opposite sides of the cut with the logic between them.
 *
 * So the split does not trust itself.  It emits the cut and then hands the
 * result to [Behavioral_cdc_check], an INDEPENDENT analysis that re-derives
 * the launch/capture domains of every port from scratch, and REFUSES to return
 * a program whose own boundaries it cannot prove safe.  A refusal is a
 * message, not an exception: the caller sees which port, in which module, and
 * why.
 *
 * WHAT IT WILL NOT DO.  Two structures cannot be cut without changing the
 * design, so this pass refuses them rather than guessing:
 *
 *   * a signal driven by registers in more than one domain -- there is no
 *     module to put the driver in;
 *   * a memory accessed from more than one domain -- an async FIFO's RAM.  The
 *     hand-written exemplar solves this by keeping the array on one side and
 *     exporting the far side's read port (rx_rd_addr in / rx_rd_data out),
 *     with the FIFO occupancy argument -- not something to infer.
 *
 * A HARD BLOCK SPANNING DOMAINS -- a dual-clock BRAM, an MMCM, a transceiver --
 * goes to the fastest domain it touches, and its pins on the other domains
 * cross the cut.  It is one object; it cannot be in two modules; and leaving it
 * at the top means NEITHER domain can be cut at all, which is what the pass
 * used to do.  Choosing the fastest decides which arc crosses: the tight
 * constraint stays inside the module that enforces it, and what crosses belongs
 * to the slower domain, with the most slack to absorb a boundary nobody times.
 * That is least-bad, not safe, so every arc it creates is listed in
 * [o_constraints] as a path the caller must constrain across the cut.  A
 * crossing the rule did NOT cause is still a refusal.
 *
 * A COMBINATIONAL CONE FEEDING SEVERAL DOMAINS goes to the FASTEST of them --
 * the one with the shortest clock period.  A cone is timed by the flop it
 * drives, so a cone driving both a 125 MHz and a 25 MHz flop has to fit inside
 * 8 ns whichever module it ends up in.  Put it in the slow module and its
 * delay is charged against 40 ns while the real capture needs 8: the fast path
 * is under-constrained, closes on paper and fails on the board.  Put it in the
 * fast module and the constraint that binds is the one that matters; the slow
 * domain's copy of the arc has 40 ns for a cone already proven to fit in 8.
 *
 * That needs the periods.  With none supplied for a cone's domains the pass
 * falls back to REPLICATING it -- emitting the cone into each domain, which is
 * free of consequence for stateless logic and, unlike picking a side blind, can
 * not strand a same-clock arc across the cut.  The report says which policy
 * each cone got, because "replicated" silently standing in for "assigned to the
 * fast domain" would hide a missing constraint.
 *)

open Behavioral_ir
module SS = Set.Make (String)

let ss_of_dce s =
  Behavioral_dce.StringSet.fold (fun n a -> SS.add n a) s SS.empty

type item = Proc of bprocess | Inst of binstance

(* One clock domain's worth of the design. *)
type group = {
  g_clock : string;              (* canonical clock name, in the top's namespace *)
  g_module : string;             (* generated module name *)
  mutable g_items : item list;
}

type outcome = {
  o_prog : bprogram;             (* the split program (valid only if o_refusals = []) *)
  o_domains : (string * int) list;  (* clock -> item count *)
  o_leftover : int;              (* items kept at the top *)
  o_shared : (string * string) list;  (* multi-domain cone -> where it went, and why *)
  (* hard block -> (module, domain it went to, that domain's period, all its
     domains).  An empty domain means it stayed at the top for want of a
     period. *)
  o_hardblocks : (string * string * float * string list) list;
  (* arcs that cross a generated boundary BECAUSE a hard block had to be put on
     one side of it -- the known price of the fastest-domain rule, and a list
     of paths the caller must constrain across the cut *)
  o_constraints : string list;
  o_refusals : string list;      (* empty = nothing beyond o_constraints to settle *)
}

(* --- clock periods -------------------------------------------------------- *)

(* Accepts either an inline spec  "clk_a=8.0,clk_b=40"  or the path to a file
   with one  "<clock> <period_ns>"  per line (# comments allowed).  Periods in
   ns, matching every other timing number in this tree. *)
let parse_periods (spec : string) : (string * float) list =
  let of_pair s =
    let s = String.trim s in
    if s = "" || s.[0] = '#' then None
    else
      let parts =
        String.split_on_char '=' s
        |> List.concat_map (String.split_on_char ' ')
        |> List.concat_map (String.split_on_char '\t')
        |> List.filter (fun x -> x <> "")
      in
      match parts with
      | [ k; v ] -> (
          match float_of_string_opt v with
          | Some f -> Some (k, f)
          | None -> failwith ("domain_split: bad period '" ^ s ^ "'"))
      | _ -> failwith ("domain_split: bad period spec '" ^ s ^ "'")
  in
  if spec = "" then []
  else if Sys.file_exists spec then begin
    let ic = open_in spec in
    let rec go acc =
      match input_line ic with
      | line -> go (match of_pair line with Some p -> p :: acc | None -> acc)
      | exception End_of_file -> close_in ic; List.rev acc
    in
    go []
  end
  else List.filter_map of_pair (String.split_on_char ',' spec)

let sanitise s =
  String.map (fun c ->
      if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
         || (c >= '0' && c <= '9') then c
      else '_')
    s

(* --- what an item reads and writes ---------------------------------------- *)

(* An instance's writes are its OUTPUT-port actuals, which needs the child's
   interface.  [lookup] takes the INSTANCE, not the module name, so a cell with
   no bmodule in the program can still be resolved from the primitive models --
   on a netlist-shaped design nearly every instance is an FDRE or a LUT, and
   treating those as opaque makes every register output look undriven.

   With no body available at all (a hard block: GTXE2, an encrypted core),
   claim no writes: a spurious driver would collide with the real one and turn
   a sound split into a refusal, whereas a missing one is reported by name --
   and the undriven check below knows to exempt hard-block pins. *)
let inst_uses_defs ~lookup (i : binstance) =
  let names e = ss_of_dce (Behavioral_dce.collect_uses_expr e) in
  match lookup i with
  | None ->
      let all =
        List.fold_left (fun a (_, e) -> SS.union a (names e)) SS.empty
          i.port_connections
      in
      (all, SS.empty)
  | Some (child : bmodule) ->
      let dir p =
        match List.find_opt (fun (s : bsignal) -> s.name = p) child.signals with
        | Some s -> s.direction
        | None -> `Input
      in
      List.fold_left
        (fun (u, d) (p, e) ->
          match dir p with
          | `Output -> (u, SS.union d (names e))
          | _ -> (SS.union u (names e), d))
        (SS.empty, SS.empty) i.port_connections

let item_uses_defs ~lookup = function
  | Proc p ->
      let u, d = Behavioral_dce.collect_uses_defs_process p in
      (ss_of_dce u, ss_of_dce d)
  | Inst i -> inst_uses_defs ~lookup i

(* --- assigning items to domains ------------------------------------------- *)

(* Last-resort clock-pin test, for a cell whose body we do not have.  Spelling
   is all that is left, so require the name to END in CLK/CLOCK rather than
   merely contain it: a GTXE2_CHANNEL has CPLLREFCLKLOST and CLKRSVD among its
   pins, and a substring test reports a transceiver as living in a dozen clock
   domains, one of them a status output. *)
let clockish_name p =
  let l = String.lowercase_ascii p in
  let n = String.length l in
  let ends s =
    let m = String.length s in
    n >= m && String.sub l (n - m) m = s
  in
  l = "c" || l = "clk" || ends "clk" || ends "clock"

(* An instance's clock PINS, taken from what the cell actually clocks rather
   than from what its pins are called.  A port is a clock pin if some register
   inside the cell is clocked by it, transitively through the cell's own
   instances.  Only where no body exists at all -- a hard block -- does this
   fall back to [clockish_name]. *)
let rec clock_pins ~lookup ?(depth = 0) (m : bmodule) : SS.t =
  if depth > 16 then SS.empty
  else
    let canon = Behavioral_cdc_check.clock_aliases m in
    let ports =
      SS.of_list
        (List.filter_map
           (fun (s : bsignal) -> if s.direction <> `Internal then Some s.name else None)
           m.signals)
    in
    let acc =
      List.fold_left
        (fun acc p ->
          match p with
          | BSequential { clock; _ } ->
              let c = canon clock in
              if SS.mem c ports then SS.add c acc else acc
          | BCombinational _ -> acc)
        SS.empty m.processes
    in
    List.fold_left
      (fun acc (i : binstance) ->
        match lookup i with
        | None -> acc
        | Some child ->
            SS.fold
              (fun cp acc ->
                match List.assoc_opt cp i.port_connections with
                | Some (BVar n) ->
                    let c = canon n in
                    if SS.mem c ports then SS.add c acc else acc
                | _ -> acc)
              (clock_pins ~lookup ~depth:(depth + 1) child)
              acc)
      acc m.instances

(* The domains an instance belongs to.  Zero or several distinct ones (a true
   dual-clock BRAM, an MMCM, a transceiver) means it belongs to no single
   domain and stays at the top. *)
let inst_domains ~canon ~lookup (i : binstance) =
  let add acc = function
    | BVar n when not (Behavioral_cdc_check.is_const_net n) -> SS.add (canon n) acc
    | _ -> acc
  in
  match lookup i with
  | Some child ->
      let pins = clock_pins ~lookup child in
      List.fold_left
        (fun acc (p, e) -> if SS.mem p pins then add acc e else acc)
        SS.empty i.port_connections
  | None ->
      List.fold_left
        (fun acc (p, e) -> if clockish_name p then add acc e else acc)
        SS.empty i.port_connections

(* --- the split ------------------------------------------------------------ *)

let split ?(periods : (string * float) list = []) (prog : bprogram) ~(top : string) :
    outcome =
  let lookup_name n = List.find_opt (fun (m : bmodule) -> m.name = n) prog.modules in
  (* Resolve an instance to a body: the program first, then the Xilinx
     primitive models.  Without the second half a flattened netlist is a sea of
     black boxes and the split can prove nothing about any of it. *)
  let lookup (i : binstance) =
    match lookup_name i.module_name with
    | Some m -> Some m
    | None -> Xil_prim_models.synth i
  in
  let m =
    match lookup_name top with
    | Some m -> m
    | None -> failwith ("domain_split: no module named '" ^ top ^ "'")
  in
  let canon = Behavioral_cdc_check.clock_aliases m in
  let ana = Behavioral_cdc_check.analyse ~lookup:lookup_name m in
  let refusals = ref [] in
  let refuse fmt = Printf.ksprintf (fun s -> refusals := s :: !refusals) fmt in

  (* The domains a set of signals is CAPTURED by -- i.e. the registers a
     combinational cone ultimately feeds.  That, not the cone's own inputs, is
     the domain the logic belongs to: a LUT is timed by the flop it drives. *)
  let capture_domains sigs =
    SS.fold
      (fun n acc ->
        match Hashtbl.find_opt ana.Behavioral_cdc_check.a_capture n with
        | None -> acc
        | Some ds ->
            Behavioral_cdc_check.DS.fold
              (fun d acc ->
                match d with
                | Behavioral_cdc_check.Clk c -> SS.add c acc
                | _ -> acc)
              ds acc)
      sigs SS.empty
  in

  (* --- 1. bucket every item ------------------------------------------------ *)
  let groups : (string, group) Hashtbl.t = Hashtbl.create 8 in
  let leftover = ref [] in
  let group_of clk =
    match Hashtbl.find_opt groups clk with
    | Some g -> g
    | None ->
        let g =
          { g_clock = clk; g_module = top ^ "_" ^ sanitise clk; g_items = [] }
        in
        Hashtbl.replace groups clk g;
        g
  in
  let place clk it =
    let g = group_of clk in
    g.g_items <- it :: g.g_items
  in
  let shared = ref [] in
  (* The tightest constraint among a cone's capture domains.  [None] when any
     one of them has no period: a cone is only provably safe in the fast module
     if we know which module that is, and guessing from clock NAMES would be a
     constraint invented by the tool. *)
  let fastest ds =
    let looked = List.map (fun d -> (d, List.assoc_opt d periods)) ds in
    if List.exists (fun (_, p) -> p = None) looked then None
    else
      (* by period, then by NAME: two clocks at the same period are common (a
         125 MHz refclk and a 125 MHz recovered clock) and which one wins must
         not depend on the order the domains happened to be discovered in *)
      match
        List.sort (fun (n1, a) (n2, b) -> compare (a, n1) (b, n2)) looked
      with
      | (d, Some p) :: _ -> Some (d, p)
      | _ -> None
  in
  (* A hard block spanning several domains goes to the FASTEST of them, for the
     same reason a shared cone does -- and with a consequence a cone does not
     have.  Its pins on the other domains become ports of the module it lands
     in, so those arcs cross the cut.  That is unavoidable: the block is one
     object, it cannot be in two modules, and keeping it at the top means
     NEITHER domain can be cut at all.  Choosing the fastest decides WHICH arc
     crosses: the tight constraint stays inside the module that enforces it,
     and the arc left crossing belongs to the slower domain, which has the most
     slack to absorb a boundary nobody times.  Least-bad, not safe -- so every
     resulting crossing is enumerated below as a path needing a cross-module
     constraint, never quietly blessed. *)
  let hardblocks = ref [] in
  (* Nets reaching a hard block we placed by the fastest-domain rule.  NOT keyed
     by the domain that ended up holding the block: the arc it creates leaves
     one module and enters the other, so it shows up on BOTH boundaries and has
     to be recognised at both. *)
  let hb_nets = ref SS.empty in
  let note_hb_nets (i : binstance) =
    hb_nets :=
      List.fold_left
        (fun a (_, e) -> SS.union a (ss_of_dce (Behavioral_dce.collect_uses_expr e)))
        !hb_nets i.port_connections
  in
  List.iter
    (fun i ->
      match SS.elements (inst_domains ~canon ~lookup i) with
      | [ d ] -> place d (Inst i)
      | [] -> (
          (* No clock pin at all: a combinational cell.  On a netlist-shaped
             design that is most of the instances, and dropping them at the top
             leaves a LUT sitting between two registers of the same domain in
             the module below -- a same-clock arc across the cut for no reason
             other than the cell being an instance rather than a process.  Place
             it the way a combinational process is placed: by the domain it
             FEEDS.  Where it feeds several, the fastest, for the same reason as
             a cone; with no period to rank them, the top keeps it, because
             duplicating a cell is a heavier thing to do silently than
             duplicating a process. *)
          let _, defs = inst_uses_defs ~lookup i in
          match SS.elements (capture_domains defs) with
          | [ d ] -> place d (Inst i)
          | [] -> leftover := Inst i :: !leftover
          | ds -> (
              match fastest ds with
              | Some (d, _) -> place d (Inst i)
              | None -> leftover := Inst i :: !leftover))
      | ds -> (
          match fastest ds with
          | Some (d, per) ->
              place d (Inst i);
              note_hb_nets i;
              hardblocks := (i.module_name, d, per, ds) :: !hardblocks
          | None ->
              leftover := Inst i :: !leftover;
              hardblocks := (i.module_name, "", 0.0, ds) :: !hardblocks))
    m.instances;
  let hardblocks = List.rev !hardblocks in

  (* Where a net goes once the instances are placed.  A cone feeding a hard
     block has an UNNAMEABLE capture domain -- the block's insides are opaque --
     so [capture_domains] reports it as feeding nothing and the top would keep
     it.  On the flattened board design that is ~880 cones, all of them the
     PCS's reset and control logic feeding transceiver pins, every one of them
     then sitting at the top between registers of the domain it belongs to.  So
     placing the instances FIRST buys a second answer to "what does this cone
     feed": the domain of the instance it feeds. *)
  let sink_domain : (string, SS.t) Hashtbl.t = Hashtbl.create 256 in
  List.iter
    (fun g ->
      List.iter
        (function
          | Inst i ->
              List.iter
                (fun (_, e) ->
                  SS.iter
                    (fun n ->
                      let cur =
                        match Hashtbl.find_opt sink_domain n with
                        | Some s -> s
                        | None -> SS.empty
                      in
                      Hashtbl.replace sink_domain n (SS.add g.g_clock cur))
                    (ss_of_dce (Behavioral_dce.collect_uses_expr e)))
                i.port_connections
          | Proc _ -> ())
        g.g_items)
    (Hashtbl.fold (fun _ g acc -> g :: acc) groups []);
  let feeds_domains defs =
    SS.fold
      (fun n acc ->
        match Hashtbl.find_opt sink_domain n with
        | Some s -> SS.union acc s
        | None -> acc)
      defs SS.empty
  in
  List.iter
    (fun p ->
      match p with
      | BSequential { clock; _ } -> place (canon clock) (Proc p)
      | BCombinational _ -> (
          let _, defs = Behavioral_dce.collect_uses_defs_process p in
          let defs = ss_of_dce defs in
          (* Registers first, instances only as a fallback.  Unioning the two
             would drag a cone into the domain of whatever hard block it
             happens to touch: every clk_sys cone feeding the picosoc BRAM's
             port A would follow the BRAM into rx_clk, taking the SoC's address
             decode with it.  A cone that reaches a register belongs with that
             register; the instance answer is for cones that reach no nameable
             register at all. *)
          let by_reg = capture_domains defs in
          let doms = if SS.is_empty by_reg then feeds_domains defs else by_reg in
          match SS.elements doms with
          | [] ->
              (* feeds no register and no placed instance: an output driver or
                 dead logic.  Nothing to be same-clock with, so the top keeps
                 it. *)
              leftover := Proc p :: !leftover
          | [ d ] -> place d (Proc p)
          | ds -> (
              let label =
                match p with BCombinational { name; _ } -> name | _ -> "?"
              in
              match fastest ds with
              | Some (d, per) ->
                  (* the cone must fit the shortest period whichever module
                     holds it; only the fast module constrains it to that *)
                  place d (Proc p);
                  shared :=
                    (label,
                     Printf.sprintf "-> %s (fastest of %s, %.3f ns)" d
                       (String.concat "," ds) per)
                    :: !shared
              | None ->
                  List.iter (fun d -> place d (Proc p)) ds;
                  shared :=
                    (label,
                     Printf.sprintf "replicated into %s (no period given for %s)"
                       (String.concat "," ds)
                       (String.concat ","
                          (List.filter (fun d -> List.assoc_opt d periods = None) ds)))
                    :: !shared)))
    m.processes;

  let groups = Hashtbl.fold (fun _ g acc -> g :: acc) groups [] in
  let groups = List.sort (fun a b -> compare a.g_clock b.g_clock) groups in
  List.iter (fun g -> g.g_items <- List.rev g.g_items) groups;

  (* A domain with logic ALSO at the top cannot be cut.  The top keeps whatever
     belongs to no single domain -- a dual-clock RAM, an instance straddling two
     clocks -- and if such an item carries registers on clock C, then pulling
     C's other logic into a module of its own puts same-clock registers on both
     sides of the new boundary.  Every arc between them then crosses a cut that
     nobody times, which the checker would (correctly) reject port by port.
     Leaving C whole says the same thing once, in terms of the structure that
     caused it, and still splits the domains that CAN be cut.  Flattening first
     usually dissolves the straddling instance and lets C split after all. *)
  let leftover_domains =
    List.fold_left
      (fun acc it ->
        match it with
        | Proc (BSequential { clock; _ }) -> SS.add (canon clock) acc
        | Proc (BCombinational _) -> acc
        | Inst i -> SS.union acc (inst_domains ~canon ~lookup i))
      SS.empty !leftover
  in
  let blocked, groups =
    List.partition (fun g -> SS.mem g.g_clock leftover_domains) groups
  in
  List.iter
    (fun g ->
      (* Name the STRUCTURE, not every instance of it: sixteen identical
         RAMB18E1s are one fact about the design, not sixteen. *)
      let straddlers =
        List.filter_map
          (function
            | Inst i when SS.mem g.g_clock (inst_domains ~canon ~lookup i) ->
                Some (i.module_name, SS.elements (inst_domains ~canon ~lookup i))
            | _ -> None)
          !leftover
      in
      let kinds =
        List.sort_uniq compare (List.map (fun (m, ds) -> (m, ds)) straddlers)
      in
      let count m = List.length (List.filter (fun (m', _) -> m' = m) straddlers) in
      let why =
        String.concat "; "
          (List.map
             (fun (m, ds) ->
               Printf.sprintf "%d x %s (on %s)" (count m) m (String.concat "+" ds))
             kinds)
      in
      (* Only say "memory" when a memory is what straddles.  The advice that
         follows is about async FIFOs, and attaching it to an MMCM or a
         transceiver -- which straddle because generating or recovering several
         clocks is their JOB -- would be nonsense dressed as guidance. *)
      let is_memlike (n, _) =
        let l = String.lowercase_ascii n in
        let has sub =
          let a = String.length sub and b = String.length l in
          let rec go i = i + a <= b && (String.sub l i a = sub || go (i + 1)) in
          go 0
        in
        has "ram" || has "mem" || has "fifo" || has "bram"
      in
      if kinds = [] then
        refuse "domain '%s' left unsplit: the top also holds registers on this clock, so cutting it would strand same-clock arcs across the boundary"
          g.g_clock
      else if List.exists is_memlike kinds then
        refuse
          "domain '%s' left unsplit: %s straddles it and stays at the top, so cutting %s would strand same-clock arcs across the boundary. A dual-clock memory is the one structure a domain split cannot move: whichever side holds it, the other side's port arcs cross a cut nobody times. The exemplar's answer is an async FIFO -- gray pointers plus a stability argument -- which is a design change, not a transformation."
          g.g_clock why g.g_clock
      else
        refuse
          "domain '%s' left unsplit: %s straddles it and stays at the top, so cutting %s would strand same-clock arcs across the boundary"
          g.g_clock why g.g_clock;
      leftover := !leftover @ g.g_items)
    blocked;

  (* --- 2. refuse what cannot be cut ---------------------------------------- *)
  (* A signal written from two domains has no module to live in.  Only
     SEQUENTIAL writes count: a replicated combinational cone legitimately
     defines the same name in several groups, and each copy is local. *)
  let seq_defs_by_domain = Hashtbl.create 64 in
  List.iter
    (fun g ->
      List.iter
        (fun it ->
          match it with
          | Proc (BSequential _ as p) ->
              let _, d = Behavioral_dce.collect_uses_defs_process p in
              Behavioral_dce.StringSet.iter
                (fun s ->
                  let cur =
                    match Hashtbl.find_opt seq_defs_by_domain s with
                    | Some x -> x
                    | None -> SS.empty
                  in
                  Hashtbl.replace seq_defs_by_domain s (SS.add g.g_clock cur))
                d
          | _ -> ())
        g.g_items)
    groups;
  Hashtbl.iter
    (fun s ds ->
      if SS.cardinal ds > 1 then
        refuse "signal '%s' is driven by registers in %d domains (%s) -- no module to put the driver in"
          s (SS.cardinal ds) (String.concat "," (SS.elements ds)))
    seq_defs_by_domain;

  (* A memory reached from two domains is an async-FIFO array.  Splitting it
     means exporting a read port and asserting an occupancy argument, which is
     a design decision, not an inference. *)
  List.iter
    (fun (mem : bmem) ->
      let touches g =
        List.exists
          (fun it ->
            let u, d = item_uses_defs ~lookup it in
            SS.mem mem.mname u || SS.mem mem.mname d)
          g.g_items
      in
      let ds = List.filter touches groups in
      if List.length ds > 1 then
        refuse "memory '%s' is accessed from %d domains (%s) -- cut it by hand, exporting a read port"
          mem.mname (List.length ds)
          (String.concat "," (List.map (fun g -> g.g_clock) ds)))
    m.mems;

  (* --- 3. build the domain modules ---------------------------------------- *)
  let sig_of n =
    match List.find_opt (fun (s : bsignal) -> s.name = n) m.signals with
    | Some s -> s
    | None ->
        (* not declared at the top: an implicit 1-bit net. Keeping the width
           guess visible beats silently emitting a port of unknown width. *)
        { name = n; stype = BInt { width = 1; signed = Unsigned };
          direction = `Internal; initial_value = None; attrs = [] }
  in
  let top_out =
    SS.of_list
      (List.filter_map
         (fun (s : bsignal) -> if s.direction = `Output then Some s.name else None)
         m.signals)
  in
  let top_in =
    SS.of_list
      (List.filter_map
         (fun (s : bsignal) -> if s.direction = `Input then Some s.name else None)
         m.signals)
  in
  let uses_defs_of items =
    List.fold_left
      (fun (u, d) it ->
        let iu, id = item_uses_defs ~lookup it in
        (SS.union u iu, SS.union d id))
      (SS.empty, SS.empty) items
  in
  let g_ud = List.map (fun g -> (g, uses_defs_of g.g_items)) groups in
  let leftover_u, leftover_d = uses_defs_of !leftover in

  (* A replicated combinational signal is defined in several groups.  Exactly
     one copy may drive the top-level net, or the split creates a multi-driver;
     elect the first group in sorted order and keep the others local. *)
  let exporter : (string, string) Hashtbl.t = Hashtbl.create 32 in
  List.iter
    (fun (g, (_, defs)) ->
      SS.iter
        (fun s -> if not (Hashtbl.mem exporter s) then Hashtbl.replace exporter s g.g_clock)
        defs)
    g_ud;

  let module_of (g, (uses, defs)) =
    let others_use =
      List.fold_left
        (fun acc (g', (u', _)) -> if g' == g then acc else SS.union acc u')
        leftover_u g_ud
    in
    let inputs = SS.diff uses defs in
    let outputs =
      SS.filter
        (fun s ->
          Hashtbl.find_opt exporter s = Some g.g_clock
          && (SS.mem s others_use || SS.mem s top_out))
        defs
    in
    let internals = SS.diff defs outputs in
    let mk dir n = { (sig_of n) with direction = dir } in
    let signals =
      List.map (mk `Input) (SS.elements inputs)
      @ List.map (mk `Output) (SS.elements outputs)
      @ List.map (mk `Internal) (SS.elements internals)
    in
    let processes =
      List.filter_map (function Proc p -> Some p | Inst _ -> None) g.g_items
    in
    let instances =
      List.filter_map (function Inst i -> Some i | Proc _ -> None) g.g_items
    in
    let mems =
      List.filter
        (fun (mm : bmem) -> SS.mem mm.mname uses || SS.mem mm.mname defs)
        m.mems
    in
    ({ name = g.g_module; params = []; signals; processes; instances;
       funcs = m.funcs; mems; attrs = [ ("keep_hierarchy", "yes") ] },
     SS.elements inputs @ SS.elements outputs)
  in
  let built = List.map (fun (g, ud) -> (g, module_of (g, ud))) g_ud in
  let built_mods = List.map (fun (_, bm) -> bm) built in

  (* --- 4. the new top ------------------------------------------------------ *)
  let domain_insts =
    List.map2
      (fun (g, _) ((dm : bmodule), ports) ->
        ignore dm;
        { inst_name = "u_" ^ sanitise g.g_clock; module_name = g.g_module;
          param_values = []; param_strs = [];
          port_connections = List.map (fun p -> (p, BVar p)) ports })
      g_ud built_mods
  in
  let kept_signals =
    let needed =
      List.fold_left
        (fun acc (i : binstance) ->
          List.fold_left (fun a (_, e) ->
              SS.union a (ss_of_dce (Behavioral_dce.collect_uses_expr e)))
            acc i.port_connections)
        (SS.union leftover_u leftover_d) domain_insts
    in
    List.filter
      (fun (s : bsignal) -> s.direction <> `Internal || SS.mem s.name needed)
      m.signals
  in
  let new_top =
    { m with
      signals = kept_signals;
      processes =
        List.filter_map (function Proc p -> Some p | Inst _ -> None) (List.rev !leftover);
      instances =
        List.filter_map (function Inst i -> Some i | Proc _ -> None) (List.rev !leftover)
        @ domain_insts;
      mems =
        List.filter
          (fun (mm : bmem) ->
            not (List.exists (fun ((dm : bmodule), _) ->
                     List.exists (fun (x : bmem) -> x.mname = mm.mname) dm.mems)
                   built_mods))
          m.mems }
  in
  let prog' =
    { prog with
      modules =
        List.map (fun (mm : bmodule) -> if mm.name = top then new_top else mm) prog.modules
        @ List.map fst built_mods }
  in

  (* --- 5. an input nobody drives is a broken cut, not a warning ------------- *)
  (* ...but only if the SPLIT is what un-drove it.  picorv32 leaves pcpi_rd and
     friends unconnected when the co-processor interface is off, so they float
     in the source too; reporting those is reporting the design, not the
     transformation.  The condition is "driven before, undriven after". *)
  let pseudo_defs =
    (* Array writes reach the IR as `@mem_write` / `@slice_write` /
       `@part_sel_write_up|down` pseudo-calls whose FIRST argument is the array,
       and the shared use/def collector counts that argument as a use only.  So
       a register file written only through them (picorv32's cpuregs) reads as
       having no driver anywhere. *)
    let acc = ref SS.empty in
    let rec stmt = function
      | BCallStmt { func; args = BVar a :: _ } when func <> "" && func.[0] = '@' ->
          acc := SS.add a !acc
      | BIf { then_stmts; else_stmts; _ } ->
          List.iter stmt then_stmts; List.iter stmt else_stmts
      | BCase { cases; default; _ } ->
          List.iter (fun (_, ss) -> List.iter stmt ss) cases;
          List.iter stmt default
      | BBlock ss -> List.iter stmt ss
      | BWhile { body; _ } -> List.iter stmt body
      | BFor { body; _ } -> List.iter stmt body
      | _ -> ()
    in
    List.iter
      (function
        | BCombinational { body; _ } | BSequential { body; _ } -> List.iter stmt body)
      m.processes;
    !acc
  in
  let driven_before =
    List.fold_left
      (fun acc it -> let _, d = item_uses_defs ~lookup it in SS.union acc d)
      (SS.union top_in pseudo_defs)
      (!leftover @ List.concat_map (fun g -> g.g_items) groups)
  in
  let driven =
    List.fold_left
      (fun acc ((dm : bmodule), _) ->
        SS.union acc
          (SS.of_list
             (List.filter_map
                (fun (s : bsignal) -> if s.direction = `Output then Some s.name else None)
                dm.signals)))
      (SS.union (SS.union leftover_d top_in) pseudo_defs) built_mods
  in
  (* Pins of a body-less hard block.  [inst_uses_defs] deliberately claims no
     writes for those, so their outputs would every one of them read as
     undriven -- 20-odd of them on the flattened ethmin, all of them the
     transceiver's.  We do not know these are driven; we know we cannot tell,
     and reporting a guess as a finding is worse than not reporting it. *)
  let opaque_pins =
    List.fold_left
      (fun acc it ->
        match it with
        | Inst i when lookup i = None ->
            List.fold_left
              (fun a (_, e) -> SS.union a (ss_of_dce (Behavioral_dce.collect_uses_expr e)))
              acc i.port_connections
        | _ -> acc)
      SS.empty
      (!leftover @ List.concat_map (fun g -> g.g_items) groups)
  in
  List.iter
    (fun ((dm : bmodule), _) ->
      List.iter
        (fun (s : bsignal) ->
          if s.direction = `Input && not (SS.mem s.name driven)
             && not (SS.mem s.name opaque_pins)
             && SS.mem s.name driven_before then
            refuse "module '%s' input '%s' has no driver after the split" dm.name s.name)
        dm.signals)
    built_mods;

  (* --- 6. do not trust the split: prove each generated boundary ------------- *)
  (* A crossing on a net that reaches a hard block placed by the fastest-domain
     rule is not a surprise: it is the known price of that rule, since the
     block's other-domain pins had to cross something.  Those are collected as
     paths REQUIRING a cross-module constraint, named one by one so the list can
     be turned into set_max_delay entries.  They are not silently downgraded and
     they are not called safe.  A crossing the policy did not cause stays a
     refusal -- nobody decided to accept that one. *)
  let constraints = ref [] in
  List.iter
    (fun (_g, ((dm : bmodule), _)) ->
      let xs = Behavioral_cdc_check.check_boundary prog' ~macro:dm.name in
      List.iter
        (fun (x : Behavioral_cdc_check.crossing) ->
          let port = x.Behavioral_cdc_check.x_signal in
            (* Whose problem each crossing is.

               UNSAFE is ours: a same-clock FF->FF arc across a boundary we
               drew, provably timed by nobody.  That is a refusal -- unless the
               net reaches a hard block we PLACED, in which case the arc is the
               accepted price of the fastest-domain rule and is listed instead.

               UNPROVEN is nobody's to settle here: the verdict exists precisely
               because a domain could not be named -- a transceiver's insides, a
               clock that never reaches an interface -- and the cone carries
               that all the way downstream, so it lands on nets that never touch
               the block themselves.  No analysis at this level can decide those,
               and refusing them means refusing every design with a GT in it.
               They are enumerated, individually, as paths the designer must
               constrain or waive.  Enumerated is the point: a count is not a
               list, and a list is what turns into set_max_delay lines. *)
            let placed = SS.mem port !hb_nets in
            match x.Behavioral_cdc_check.x_verdict with
            | Behavioral_cdc_check.Unproven ->
                constraints :=
                  Printf.sprintf "%s %s.%s -- %s"
                    (if placed then "[placed]" else "[opaque]") dm.name port
                    x.Behavioral_cdc_check.x_why
                  :: !constraints
            | Behavioral_cdc_check.Unsafe when placed ->
                constraints :=
                  Printf.sprintf "[placed] %s.%s -- SAME-CLOCK arc created by placing a hard block on one side"
                    dm.name port
                  :: !constraints
            | Behavioral_cdc_check.Unsafe ->
                refuse "%s.%s: UNSAFE (%s)" dm.name port x.Behavioral_cdc_check.x_why
            | Behavioral_cdc_check.Safe -> ())
        xs)
    built;

  {
    o_prog = prog';
    o_domains = List.map (fun g -> (g.g_clock, List.length g.g_items)) groups;
    o_leftover = List.length !leftover;
    o_shared = List.rev !shared;
    o_hardblocks = hardblocks;
    o_constraints = List.rev !constraints;
    o_refusals = List.rev !refusals;
  }

let report (o : outcome) =
  Printf.printf "[domain-split] %d domain(s), %d item(s) left at the top\n"
    (List.length o.o_domains) o.o_leftover;
  List.iter (fun (c, n) -> Printf.printf "  %-24s %d item(s)\n" c n) o.o_domains;
  if o.o_shared <> [] then begin
    Printf.printf "[domain-split] %d cone(s) feeding more than one domain:\n"
      (List.length o.o_shared);
    List.iter (fun (c, w) -> Printf.printf "  %-28s %s\n" c w) o.o_shared
  end;
  if o.o_hardblocks <> [] then begin
    (* one line per KIND, not per instance: sixteen identical BRAMs are one
       fact about the design *)
    let kinds =
      List.sort_uniq compare
        (List.map (fun (n, d, p, ds) -> (n, d, p, ds)) o.o_hardblocks)
    in
    Printf.printf "[domain-split] %d hard block(s) spanning domains:\n"
      (List.length o.o_hardblocks);
    List.iter
      (fun (n, d, p, ds) ->
        let count =
          List.length (List.filter (fun (n', _, _, _) -> n' = n) o.o_hardblocks)
        in
        if d = "" then
          Printf.printf "  %2d x %-16s STAYS AT THE TOP -- no period for %s\n" count n
            (String.concat "," ds)
        else
          Printf.printf "  %2d x %-16s -> %s (fastest of %s, %.3f ns)\n" count n d
            (String.concat "," ds) p)
      kinds
  end;
  if o.o_constraints <> [] then begin
    Printf.printf
      "[domain-split] %d arc(s) MUST be timed by a cross-module constraint (set_max_delay\n\
      \              across the boundary): [placed] = a hard block we put on one side, so its\n\
      \              other-domain pins cross; [opaque] = a path into a block nobody can see\n\
      \              inside, so no analysis can settle it:\n"
      (List.length o.o_constraints);
    List.iter (fun c -> Printf.printf "  %s\n" c) o.o_constraints
  end;
  if o.o_refusals = [] then
    if o.o_constraints = [] then print_string "[domain-split] cut PROVEN safe\n"
    else
      Printf.printf
        "[domain-split] cut proven safe APART from the %d listed arc(s), which need the constraint above\n"
        (List.length o.o_constraints)
  else begin
    Printf.printf "[domain-split] REFUSED -- %d problem(s):\n"
      (List.length o.o_refusals);
    List.iter (fun r -> Printf.printf "  %s\n" r) o.o_refusals
  end
