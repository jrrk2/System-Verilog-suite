(* opendcp_xml.ml -- read an opendcp XML physical database (dcp2xml) and check
   it for structural consistency.

   WHY THIS EXISTS.  The RapidWright json2dcp reconstruction of a nextpnr routed
   design used to segfault Vivado 2020.1 in report_timing / report_route_status,
   and every diagnosis of it so far was an ad-hoc script that counted tokens.
   Two of those counts were WRONG in ways that pointed the investigation at the
   wrong subsystem:

     * "27% of the routing is dropped" -- counted every '->' in the nextpnr
       ROUTING string as a PIP, but the string interleaves SITEWIRE entries
       (intra-site, which correctly become siteroutes, not PIPs).  Real figure
       was ~2%.
     * "clk_sys lost 881 pips" -- the DCP records a pip against a tile the
       estimate guessed wrong, so matching pips were counted as missing.

   The fix is not a better script: it is to parse the database into a TYPED
   model once and ask structural questions of it, so a check either holds or
   names the cells that break it.  Every check below reports the offending
   objects, never a bare count, because a count cannot be acted on.

   The checks are deliberately device-agnostic where they can be: they test
   INTERNAL consistency (a LUT6 has six connected inputs, a routed net has one
   driver) rather than comparing against a golden file, so a single database is
   enough to find a defect. *)

type cell = {
  c_name  : string;
  c_type  : string;
  c_site  : string;
  c_bel   : string;
  c_pins  : (string * string) list;   (* BEL pin -> logical pin *)
  c_props : (string * string) list;
  c_rt    : bool;
    (* routethru="true": this is NOT a placement of the cell, it is a LUT being
       used as a wire to carry one of the cell's INPUT pins into the site.  The
       same cell name therefore appears several times -- once really placed (no
       routethru attribute) and once per LUT it passes through.  Keyed without
       this distinction, "last entry wins" put 268 cells on a LUT bel when they
       actually sit on CARRY4/xFF. *)
}

type sitepip = { sp_bel : string; sp_in : string; sp_out : string }

type siteroute = {
  sr_net : string;
  sr_bel : string;                       (* the BEL that DRIVES inside the site *)
  sr_pin : string;
  sr_snks : (string * string) list;      (* <sink bel= pin=/>: what it feeds *)
}

(* A placed site, with the intra-site state Vivado records for it.  Modelled
   because a whole class of import defects lives here and nowhere else -- the
   slice output mux, the IOB's routing bels, the GT's 420 intra-site nets -- and
   answering "does our site look like Vivado's" with grep over the XML produced
   three wrong conclusions in one session. *)
type site = {
  s_name  : string;
  s_type  : string;
  s_pips  : sitepip list;
  s_routes: siteroute list;
}

type net = {
  n_name    : string;
  n_type    : string;
  n_pips    : (string * string * string * bool) list;
    (* tile, src, dst, REVERSED.  src/dst are the pip's start/end wires as the
       device stores them; `reversed` says the net actually drives the other way
       through a BIDIRECTIONAL pip (long lines, the BUFG rebuffer taps).  Vivado
       records the distinction and dcp2xml writes it as rev="true"; dropping it
       makes 85 pips come back pointing the wrong way. *)
  n_srcpins : (string * string) list;           (* site, pin  (dir=out) *)
  n_snkpins : (string * string) list;           (* site, pin  (dir=in)  *)
}

type db = {
  part  : string;
  top   : string;
  cells : cell list;
  nets  : net list;
  sites : site list;
}

(* ---------------------------------------------------------------- parsing *)

let attr name el = try Some (Xml.attrib el name) with Xml.No_attribute _ -> None
let attr_or d name el = match attr name el with Some v -> v | None -> d

let parse_cell site el =
  let pins = ref [] and props = ref [] in
  Xml.iter
    (fun ch ->
       match String.lowercase_ascii (Xml.tag ch) with
       | "pin" ->
           (match attr "bel" ch, attr "log" ch with
            | Some b, Some l -> pins := (b, l) :: !pins
            | _ -> ())
       | "prop" ->
           (match attr "key" ch, attr "val" ch with
            | Some k, Some v -> props := (k, v) :: !props
            | _ -> ())
       | _ -> ())
    el;
  { c_name = attr_or "" "name" el;
    c_type = attr_or "" "type" el;
    c_site = site;
    c_bel  = attr_or "" "bel" el;
    c_pins = List.rev !pins;
    c_props = List.rev !props;
    c_rt = (match attr "routethru" el with
            | Some v -> String.lowercase_ascii v = "true"
            | None -> false) }

let parse_net el =
  let pips = ref [] and src = ref [] and snk = ref [] in
  Xml.iter
    (fun ch ->
       match String.lowercase_ascii (Xml.tag ch) with
       | "pip" ->
           pips := (attr_or "" "tile" ch, attr_or "" "src" ch,
                    attr_or "" "dst" ch,
                    String.lowercase_ascii (attr_or "" "rev" ch) = "true") :: !pips
       | "sitepin" ->
           let e = (attr_or "" "site" ch, attr_or "" "pin" ch) in
           if attr_or "" "dir" ch = "out" then src := e :: !src
           else snk := e :: !snk
       | _ -> ())
    el;
  { n_name = attr_or "" "name" el;
    n_type = attr_or "" "type" el;
    n_pips = List.rev !pips;
    n_srcpins = List.rev !src;
    n_snkpins = List.rev !snk }

let parse_site el =
  let pips = ref [] and routes = ref [] in
  Xml.iter
    (fun ch ->
       match String.lowercase_ascii (Xml.tag ch) with
       | "sitepip" ->
           pips := { sp_bel = attr_or "" "bel" ch;
                     sp_in  = attr_or "" "in" ch;
                     sp_out = attr_or "" "out" ch } :: !pips
       | "siteroute" ->
           (* The element is not empty: its <sink bel= pin=/> children say
              which BEL pins the driver feeds.  Direction inside a site is
              therefore STATED by the database and never needs inferring. *)
           let snks = ref [] in
           Xml.iter
             (fun s ->
                if String.lowercase_ascii (Xml.tag s) = "sink" then
                  snks := (attr_or "" "bel" s, attr_or "" "pin" s) :: !snks)
             ch;
           routes := { sr_net = attr_or "" "net" ch;
                       sr_bel = attr_or "" "srcbel" ch;
                       sr_pin = attr_or "" "srcpin" ch;
                       sr_snks = List.rev !snks } :: !routes
       | _ -> ())
    el;
  { s_name = attr_or "" "name" el; s_type = attr_or "" "type" el;
    s_pips = List.rev !pips; s_routes = List.rev !routes }

let load path =
  let root = Xml.parse_file path in
  let cells = ref [] and nets = ref [] and sites = ref [] in
  Xml.iter
    (fun sec ->
       match String.lowercase_ascii (Xml.tag sec) with
       | "physical" ->
           Xml.iter
             (fun si ->
                if String.lowercase_ascii (Xml.tag si) = "siteinst" then begin
                  let site = attr_or "" "name" si in
                  sites := parse_site si :: !sites;
                  Xml.iter
                    (fun c ->
                       if String.lowercase_ascii (Xml.tag c) = "cell" then
                         cells := parse_cell site c :: !cells)
                    si
                end)
             sec
       | "routing" ->
           Xml.iter
             (fun n ->
                if String.lowercase_ascii (Xml.tag n) = "net" then
                  nets := parse_net n :: !nets)
             sec
       | _ -> ())
    root;
  { part = attr_or "" "part" root; top = attr_or "" "top" root;
    cells = List.rev !cells; nets = List.rev !nets; sites = List.rev !sites }

(* ------------------------------------------------------------ diagnostics *)

type finding = { f_check : string; f_where : string; f_detail : string }

let findings : finding list ref = ref []
let report check where detail =
  findings := { f_check = check; f_where = where; f_detail = detail } :: !findings

(* LUT<n> must have exactly I0..I(n-1) driven and an INIT of 2^n bits.

   This is the check that found the real defect: nextpnr merges two logical LUT
   inputs carrying the SAME net onto one physical pin and records both names in
   X_ORIG_PORT ("I3 I4"), but RapidWright's physical->logical pin map is 1:1, so
   the second overwrote the first and the cell arrived as a LUT6 with 5 inputs.
   Vivado answers that with 20-756 and then a segfault. *)
let lut_arity db =
  let is_lut t =
    String.length t = 4 && String.sub t 0 3 = "LUT"
    && t.[3] >= '1' && t.[3] <= '6' in
  List.iter
    (fun c ->
       if is_lut c.c_type then begin
         let n = Char.code c.c_type.[3] - Char.code '0' in
         let ins =
           List.filter_map
             (fun (_, l) -> if String.length l > 1 && l.[0] = 'I' then Some l else None)
             c.c_pins in
         let want = List.init n (fun i -> "I" ^ string_of_int i) in
         let missing = List.filter (fun w -> not (List.mem w ins)) want in
         let extra = List.filter (fun i -> not (List.mem i want)) ins in
         if missing <> [] || extra <> [] then
           report "lut-arity" c.c_name
             (Printf.sprintf "%s at %s/%s: missing [%s]%s"
                c.c_type c.c_site c.c_bel (String.concat "," missing)
                (if extra = [] then "" else " extra [" ^ String.concat "," extra ^ "]"));
         (* INIT width must match the arity the pins imply *)
         match List.assoc_opt "INIT" c.c_props with
         | None -> ()
         | Some v ->
             (* dcp2xml prints Vivado form: 64'hDEADBEEF... *)
             (match String.index_opt v '\'' with
              | None -> ()
              | Some i ->
                  let w = try int_of_string (String.sub v 0 i) with _ -> 0 in
                  if w > 0 && w <> 1 lsl n then
                    report "init-width" c.c_name
                      (Printf.sprintf "%s has %d-bit INIT, expected %d"
                         c.c_type w (1 lsl n)))
       end)
    db.cells

(* No BEL pin may carry two logical pins -- a DCP's physical->logical map is 1:1,
   so the second silently overwrites the first and a connection is lost.

   The CONVERSE is legal and must not be reported: one logical pin routinely
   drives two BEL pins (a RAMB36's L/U halves both take ADDRBWRADDR[10]; a
   distributed-RAM RAMD takes ADR0 on both WA1 and A1).  Vivado's own database
   does this 271 times in this design, which is how the over-strict version of
   this check was caught -- it is calibrated against a golden DCP, not invented. *)
let pin_map_injective db =
  List.iter
    (fun c ->
       let seen_bel = Hashtbl.create 16 in
       List.iter
         (fun (b, l) ->
            if Hashtbl.mem seen_bel b then
              report "pin-map" c.c_name
                (Printf.sprintf "BEL pin %s carries both %s and %s"
                   b (Hashtbl.find seen_bel b) l)
            else Hashtbl.add seen_bel b l)
         c.c_pins)
    db.cells

(* Two cells may not occupy one BEL. *)
let bel_exclusive db =
  let seen = Hashtbl.create 8192 in
  List.iter
    (fun c ->
       if c.c_bel <> "" then begin
         let k = c.c_site ^ "/" ^ c.c_bel in
         match Hashtbl.find_opt seen k with
         | Some prev -> report "bel-clash" k (prev ^ " and " ^ c.c_name)
         | None -> Hashtbl.add seen k c.c_name
       end)
    db.cells

(* A net that carries routing must have exactly one driver site pin.

   Zero is the GT defect: json2dcp used to `continue` past a driver whose cell
   had no X_ORIG_PORT (true of every hard block, whose type already IS the
   Unisim), which skipped the driver AND every user, leaving a physical net with
   no logical driver -- the null Vivado's delay estimator dereferences. *)
let is_const_net name =
  let pre p = String.length name >= String.length p
              && String.sub name 0 (String.length p) = p in
  pre "GLOBAL_LOGIC" || pre "$PACKER_"

let net_driver db =
  List.iter
    (fun n ->
       (* A constant net is fed from every TIEOFF it reaches -- Vivado's own
          database gives GLOBAL_LOGIC0 256 source pins -- so "one driver" is
          simply not a property of these. *)
       if is_const_net n.n_name then () else
       let routed = n.n_pips <> [] || n.n_snkpins <> [] in
       match n.n_srcpins with
       | [] when routed ->
           report "net-no-driver" n.n_name
             (Printf.sprintf "%d pips, %d sinks, NO source site pin"
                (List.length n.n_pips) (List.length n.n_snkpins))
       | _ :: _ :: _ ->
           report "net-multi-driver" n.n_name
             (Printf.sprintf "%d source site pins: %s"
                (List.length n.n_srcpins)
                (String.concat " " (List.map (fun (s, p) -> s ^ "/" ^ p) n.n_srcpins)))
       | _ -> ())
    db.nets

(* A net with sinks but no PIPs never physically reaches them. *)
let net_unrouted db =
  List.iter
    (fun n ->
       if n.n_pips = [] && n.n_snkpins <> [] && n.n_srcpins <> [] then
         (* intra-site only is legal, but flag it so it can be eyeballed *)
         report "net-no-pips" n.n_name
           (Printf.sprintf "%d sinks, %d sources, no inter-tile PIPs"
              (List.length n.n_snkpins) (List.length n.n_srcpins)))
    db.nets

(* Every site a net's pins name must actually hold a cell -- EXCEPT TIEOFF
   sites, which are pure constant sources and never hold one.  Vivado's own
   database has 402 such pins; without the exemption this check is all noise. *)
let sitepin_sites_exist db =
  let sites = Hashtbl.create 4096 in
  List.iter (fun c -> Hashtbl.replace sites c.c_site ()) db.cells;
  let is_tieoff s =
    String.length s >= 6 && String.sub s 0 6 = "TIEOFF" in
  List.iter
    (fun n ->
       List.iter
         (fun (s, p) ->
            if not (is_tieoff s) && not (Hashtbl.mem sites s) then
              report "orphan-sitepin" n.n_name (s ^ "/" ^ p ^ " has no placed cell"))
         (n.n_srcpins @ n.n_snkpins))
    db.nets

(* Pin-set signature outliers: group cells by (type, sorted logical pins) and
   flag the rare shapes.  Needs no per-primitive spec, so it catches classes the
   explicit checks above were never written for. *)
let signature_outliers db =
  let tbl = Hashtbl.create 64 in
  List.iter
    (fun c ->
       let sg = String.concat "," (List.sort compare (List.map snd c.c_pins)) in
       let per = try Hashtbl.find tbl c.c_type with Not_found ->
         let h = Hashtbl.create 8 in Hashtbl.add tbl c.c_type h; h in
       let prev = try Hashtbl.find per sg with Not_found -> (0, c.c_name) in
       Hashtbl.replace per sg (fst prev + 1, snd prev))
    db.cells;
  Hashtbl.iter
    (fun ty per ->
       let total = Hashtbl.fold (fun _ (n, _) a -> a + n) per 0 in
       Hashtbl.iter
         (fun sg (n, ex) ->
            if n <= 3 && total >= 20 && n * 20 < total then
              report "rare-shape" (ty ^ " x" ^ string_of_int n)
                (Printf.sprintf "of %d; pins [%s]; e.g. %s" total sg ex))
         per)
    tbl

let all_checks = [
  "lut-arity", lut_arity;
  "pin-map", pin_map_injective;
  "bel-clash", bel_exclusive;
  "net-driver", net_driver;
  "net-unrouted", net_unrouted;
  "orphan-sitepin", sitepin_sites_exist;
  "rare-shape", signature_outliers;
]

let check ?(only = []) db =
  findings := [];
  List.iter
    (fun (nm, f) -> if only = [] || List.mem nm only then f db)
    all_checks;
  List.rev !findings

(* ------------------------------------------------------------- comparison *)

(* Join two databases on a canonical cell name: Vivado punctuates hierarchy
   with '/', the flattened yosys names use '.', and both bracket bus indices.
   The JOIN RATE is returned with the result -- a low one makes every number
   downstream meaningless, which is how a previous placement experiment
   silently measured Vivado's own placement instead of ours. *)
let canon s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun ch -> match ch with
       | '/' | '.' | '_' | '[' | ']' | '\\' -> ()
       | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

type diff = {
  d_site_only_a : (string * int * int) list;  (* site, npips, nroutes *)
  d_site_only_b : (string * int * int) list;
  d_site_diff   : (string * string) list;     (* site, description *)
  d_joined     : int;
  d_only_a     : int;
  d_only_b     : int;
  d_type_diff  : (string * string * string) list;  (* name, a type, b type *)
  d_loc_diff   : (string * string * string) list;  (* name, a site/bel, b site/bel *)
  d_pin_diff   : (string * string) list;           (* name, description *)
}

let index db =
  let h = Hashtbl.create 16384 in
  List.iter
    (fun c ->
       let k = canon c.c_name in
       if Hashtbl.mem h k then Hashtbl.replace h k None
       else Hashtbl.add h k (Some c))
    db.cells;
  h

let compare_db a b =
  let ha = index a and hb = index b in
  let joined = ref 0 and ty = ref [] and loc = ref [] and pin = ref [] in
  Hashtbl.iter
    (fun k va ->
       match va, Hashtbl.find_opt hb k with
       | Some ca, Some (Some cb) ->
           incr joined;
           if ca.c_type <> cb.c_type then ty := (ca.c_name, ca.c_type, cb.c_type) :: !ty;
           if ca.c_site <> cb.c_site || ca.c_bel <> cb.c_bel then
             loc := (ca.c_name, ca.c_site ^ "/" ^ ca.c_bel, cb.c_site ^ "/" ^ cb.c_bel) :: !loc;
           let sa = List.sort compare ca.c_pins and sb = List.sort compare cb.c_pins in
           if sa <> sb then begin
             (* a pure permutation of LUT inputs is expected -- the two placers
                choose different input pins and compensate in INIT -- so report
                only when the SET of logical pins differs, not the assignment *)
             let la = List.sort compare (List.map snd sa)
             and lb = List.sort compare (List.map snd sb) in
             if la <> lb then
               pin := (ca.c_name,
                       Printf.sprintf "logical pins [%s] vs [%s]"
                         (String.concat "," la) (String.concat "," lb)) :: !pin
           end
       | _ -> ())
    ha;
  let count h other =
    Hashtbl.fold
      (fun k v acc -> match v with
         | Some _ -> (match Hashtbl.find_opt other k with
             | Some (Some _) -> acc | _ -> acc + 1)
         | None -> acc)
      h 0 in
  (* Site-level comparison.  Sites join on their NAME, which is a device
     coordinate and therefore identical across tools -- unlike cell names, which
     only join at ~36% between two different synthesisers.  So this half of the
     comparison is trustworthy even when the cell half is not. *)
  let sidx d = List.fold_left (fun m st -> (st.s_name, st) :: m) [] d.sites in
  let sa = sidx a and sb = sidx b in
  let site_only x y =
    List.filter_map
      (fun (nm, st) ->
         if List.mem_assoc nm y then None
         else Some (nm, List.length st.s_pips, List.length st.s_routes))
      x in
  let sdiff =
    List.filter_map
      (fun (nm, st) ->
         match List.assoc_opt nm sb with
         | None -> None
         | Some st2 ->
             let np1 = List.length st.s_pips and np2 = List.length st2.s_pips in
             let nr1 = List.length st.s_routes and nr2 = List.length st2.s_routes in
             (* Compare the SHAPE (bel/in/out, bel/pin), not the net names --
                escape_name rewrites '/' to '_', so every site route's net name
                differs cosmetically between a golden and an imported DCP. *)
             let shape l = List.sort compare (List.map (fun r -> (r.sr_bel, r.sr_pin)) l) in
             let pshape l = List.sort compare (List.map (fun q -> (q.sp_bel, q.sp_in, q.sp_out)) l) in
             if np1 = np2 && nr1 = nr2 && shape st.s_routes = shape st2.s_routes
                && pshape st.s_pips = pshape st2.s_pips then None
             else Some (nm, Printf.sprintf "sitepips %d vs %d, siteroutes %d vs %d%s"
                              np1 np2 nr1 nr2
                              (if shape st.s_routes <> shape st2.s_routes
                               then " (route SHAPE differs)" else "")))
      sa in
  { d_site_only_a = site_only sa sb;
    d_site_only_b = site_only sb sa;
    d_site_diff = sdiff;
    d_joined = !joined;
    d_only_a = count ha hb;
    d_only_b = count hb ha;
    d_type_diff = List.rev !ty;
    d_loc_diff = List.rev !loc;
    d_pin_diff = List.rev !pin }


(* ---------------------------------------------------------- route statistics

   WHICH ROUTING RESOURCES DOES VIVADO USE THAT WE DO NOT?

   When nextpnr fails to route arcs on a placement Vivado routed fine, the
   endpoints are usually IDENTICAL (measured: 106 of 131 failed arcs) -- so the
   difference is the PATH, not the pins, and comparing sitepins says nothing.
   What differs is which interconnect classes the path is allowed to use.

   This classifies every pip of every net by the 7-series wire naming, then
   contrasts the nets nextpnr FAILED against the ones it managed.  A class that
   is over-represented in the failed set is a technique we are missing rather
   than congestion we were unlucky with -- which is the difference between a
   fix and a knob.

   Deliberately a RATIO, not a count: long nets use more of everything, so raw
   totals just rank nets by length. *)

(* 7-series interconnect naming.  The prefix says how far a wire reaches, and
   that is exactly the axis of interest: a router that never uses the short
   hops has to spend a long one wherever a short one would have done. *)
let wire_class w =
  (* Take the last path component: a pip endpoint is "TILE/WIRE". *)
  let w = match String.rindex_opt w '/' with
    | Some i -> String.sub w (i + 1) (String.length w - i - 1)
    | None -> w in
  let contains sub =
    let n = String.length w and m = String.length sub in
    let rec go i = i + m <= n && (String.sub w i m = sub || go (i + 1)) in
    m > 0 && go 0 in
  (* 7-series interconnect: a segment is <dir><dir-or-turn><length>BEG/END,
     e.g. NR1END (north-right, ONE tile), NW2END (north-west, TWO), NN4, SS6.
     The LENGTH digit is the axis of interest -- how far one hop reaches.
     Classifying by the leading direction pair alone misses that entirely,
     which is how an earlier version dropped every single-tile wire into
     "other" and left 48% of the sample unexplained. *)
  let seg_len () =
    let n = String.length w in
    let rec go i =
      if i + 1 >= n then None
      else
        let c = w.[i] in
        let dir c = c = 'N' || c = 'S' || c = 'E' || c = 'W'
                    || c = 'R' || c = 'L' in
        if dir c && i + 2 <= n && dir w.[i] then
          (* look for <letter><letter><digit> *)
          (if i + 2 < n && dir w.[i] && dir w.[i + 1]
              && w.[i + 2] >= '1' && w.[i + 2] <= '9'
           then Some (Char.code w.[i + 2] - Char.code '0')
           else go (i + 1))
        else go (i + 1) in
    go 0 in
  (* CLB SITE PIN wires: the tile-level stubs that reach into the slice.
     "CLBLM_M_A3" is the M slice's A-LUT input 3; "CLBLM_L_DMUX" is the L
     slice's D output mux.  They are neither interconnect segments nor IMUX, so
     an earlier version left them ALL in "other" -- 16% of the sample, and it
     hid the fact that the LUT input pins are the single largest class of pip
     endpoint in the design. *)
  let clb_pin () =
    (* strip a leading CLBLL_L_ / CLBLL_LL_ / CLBLM_L_ / CLBLM_M_ *)
    let strip pfx =
      let lp = String.length pfx and lw = String.length w in
      if lw > lp && String.sub w 0 lp = pfx
      then Some (String.sub w lp (lw - lp)) else None in
    let rest =
      match strip "CLBLL_LL_" with Some r -> Some r | None ->
      match strip "CLBLL_L_" with Some r -> Some r | None ->
      match strip "CLBLM_M_" with Some r -> Some r | None ->
      match strip "CLBLM_L_" with Some r -> Some r | None -> None in
    match rest with
    | None -> None
    | Some r ->
      let n = String.length r in
      let lane c = c >= 'A' && c <= 'D' in
      if n = 2 && lane r.[0] && r.[1] >= '1' && r.[1] <= '6' then
        Some "site LUT input pin"
      else if n = 4 && lane r.[0] && String.sub r 1 3 = "MUX" then
        Some "site output pin (xMUX)"
      else if (n = 1 && lane r.[0]) || (n = 2 && lane r.[0] && r.[1] = 'Q') then
        Some "site output pin (direct)"
      else if r = "CIN" || r = "COUT" then Some "carry chain"
      else None in
  match clb_pin () with
  | Some c -> c
  | None ->
  if contains "VCC_WIRE" || contains "GND_WIRE" then "constant tie"
  else if contains "LOGIC_OUTS" then "LOGIC_OUTS (site output)"
  else if contains "IMUX" then "IMUX (site input)"
  else if contains "BYP" then "BYP (bypass, short hop)"
  else if contains "FAN" then "FAN (fanout, short hop)"
  else if contains "CTRL" then "CTRL"
  else if contains "CLK" || contains "GCLK" then "CLK"
  else if contains "LH" || contains "LV" then "LONG (12+ tiles)"
  else
    match seg_len () with
    | Some 1 -> "SINGLE (1 tile)"
    | Some 2 -> "DOUBLE (2 tiles)"
    | Some 4 -> "QUAD (4 tiles)"
    | Some 6 -> "HEX (6 tiles)"
    | Some n -> Printf.sprintf "segment (%d tiles)" n
    | None -> "other"

(* histogram of wire classes over a set of nets, as (class, pips, share) *)
let class_histogram (nets : net list) =
  let tbl = Hashtbl.create 32 in
  let total = ref 0 in
  List.iter (fun n ->
      List.iter (fun (_tile, src, dst, _rev) ->
          List.iter (fun w ->
              let c = wire_class w in
              incr total;
              Hashtbl.replace tbl c (1 + (try Hashtbl.find tbl c with Not_found -> 0)))
            [ src; dst ])
        n.n_pips)
    nets;
  let l = Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl [] in
  let l = List.sort (fun (_, a) (_, b) -> compare b a) l in
  (!total, List.map (fun (k, v) ->
       (k, v, if !total = 0 then 0.0 else 100.0 *. float v /. float !total)) l)

(* Compare the nets nextpnr failed against the ones it routed.  [failed] is the
   set of net names taken from its SKIP_FAILED_ARCS warnings. *)
let route_stats db (failed : string list) =
  let fset = Hashtbl.create 64 in
  List.iter (fun n -> Hashtbl.replace fset n ()) failed;
  let is_failed n = Hashtbl.mem fset n.n_name in
  let f_nets = List.filter is_failed db.nets in
  let o_nets = List.filter (fun n -> not (is_failed n) && n.n_pips <> []) db.nets in
  let buf = Buffer.create 4096 in
  Buffer.add_string buf
    (Printf.sprintf "nets: %d failed by nextpnr, %d routed (of %d in the db)\n"
       (List.length f_nets) (List.length o_nets) (List.length db.nets));
  if f_nets = [] then
    Buffer.add_string buf
      "  no failed net matched the database -- check the names line up\n"
  else begin
    let (ft, fh) = class_histogram f_nets in
    let (ot, oh) = class_histogram o_nets in
    let share tbl c =
      try let (_, _, s) = List.find (fun (k, _, _) -> k = c) tbl in s
      with Not_found -> 0.0 in
    Buffer.add_string buf
      (Printf.sprintf "  pip endpoints: %d on failed nets, %d on routed nets\n\n" ft ot);
    Buffer.add_string buf
      (Printf.sprintf "  %-28s %9s %9s %9s\n" "wire class" "failed%" "routed%" "ratio");
    List.iter (fun (c, _, fs) ->
        let os = share oh c in
        let ratio = if os = 0.0 then infinity else fs /. os in
        Buffer.add_string buf
          (Printf.sprintf "  %-28s %8.1f%% %8.1f%% %9s\n" c fs os
             (if ratio = infinity then "only-failed"
              else Printf.sprintf "%.2fx" ratio)))
      fh;
    (* a class Vivado uses on failed nets and NOWHERE on the routed ones is the
       strongest signal available: we never had to produce it before *)
    List.iter (fun (c, _, fs) ->
        if share oh c = 0.0 && fs > 0.0 then
          Buffer.add_string buf
            (Printf.sprintf "\n  NOTE: %s appears ONLY on nets we failed\n" c))
      fh
  end;
  Buffer.contents buf


(* ------------------------------------------------- export Vivado's routing

   Write the physical database's per-net pips in nextpnr's --fixed-routes
   format ("<net>\t<src>-><dst>", one pip per line).  Feeding that alongside
   the placed JSON gives an OPEN routed database that is Vivado's placement AND
   Vivado's routing, expressed entirely in our flow -- so a bitstream can be
   built from it, and our own router's output (--write-fixed-routes) can be
   compared against it line for line instead of by inference.

   Nets with no pips are skipped rather than emitted empty: an intra-site net
   has no fabric routing to lock, and a blank entry only produces a warning. *)
let write_fixed_routes db path =
  (* EXPORT EVERYTHING.  An earlier version filtered clock tiles (because
     routeClock built a conflicting tree) and the VCC/GND nets (because routeVcc
     did) -- but dropping data to suit a downstream pass is the wrong way round:
     it silently loses 542 clock features and leaves the replay incomplete.
     The right fix is on the consumer side, where NEXTPNR_ROUTE_FIXED_ONLY now
     skips routeClock/routeVcc entirely, so nothing competes with the import. *)
  (* CONSTANT NETS: Vivado drives them from TIEOFF sites (TIEOFF_X137Y252/HARD1,
     TIEOFF_X137Y265/HARD0) onto two global nets, GLOBAL_LOGIC1 and
     GLOBAL_LOGIC0.  nextpnr invents its own, $PACKER_VCC_NET and
     $PACKER_GND_NET, so an imported const net matches neither by name nor by
     driver topology (arch.cc says as much) and its routing is silently dropped
     -- which is why skipping routeVcc cost 6273 features.  Rename them on the
     way out so they bind. *)
  let const_rename n =
    match n with
    | "GLOBAL_LOGIC1" -> "$PACKER_VCC_NET"
    | "GLOBAL_LOGIC0" -> "$PACKER_GND_NET"
    | other -> other in
  let oc = open_out path in
  let nets = ref 0 and pips = ref 0 in
  List.iter (fun n ->
      if n.n_pips <> [] then begin
        incr nets;
        List.iter (fun (tile, src, dst, rev) ->
            incr pips;
            let src, dst = if rev then dst, src else src, dst in
            (* Endpoints must be TILE-QUALIFIED: "CLBLM_L_A" names a wire in
               every CLBLM_L tile on the die, so bare names parse happily and
               bind to the wrong pips. *)
            Printf.fprintf oc "%s\t%s/%s->%s/%s\n" (const_rename n.n_name) tile src tile dst)
          n.n_pips
      end)
    db.nets;
  close_out oc;
  Printf.sprintf "wrote %s: %d net(s), %d pip(s) (nothing filtered)" path !nets !pips

(* ---------------------------------------------------------------- XDC read *)

(* Per-PORT PACKAGE_PIN / IOSTANDARD, read from the design's XDC.

   WHY: these are not in the physical database at all.  Vivado's own opendcp XML
   records IOSTANDARD as absent on every IO cell -- the value lives only in the
   constraints, which the golden checkpoint bundles (vc707_ethmin.xdc) and a
   rebuilt one does not.  Every port then opens on IOSTANDARD DEFAULT, Vivado
   raises NSTD-1 ("13 out of 19 logical ports use ... 'DEFAULT'") and refuses
   write_bitstream, and the FASM picks LVCMOS33_LVTTL where the golden has
   LVCMOS15_LVCMOS18.  json2dcp will emit the constraints itself, but only from
   PACKAGE_PIN / IOSTANDARD attributes on the PAD cell (json2dcp.java:1904),
   which is what this feeds.

   Both spellings occur in the same file:
     set_property -dict {PACKAGE_PIN E19 IOSTANDARD LVDS} [get_ports IO_CLK_P]
     set_property IOSTANDARD LVCMOS18 [get_ports eth_rst_n]
   and a bus-indexed port arrives braced, [get_ports {LED[0]}], so the port name
   itself contains brackets. *)
type xdc_port = { xp_pin : string option; xp_iostd : string option }

let read_xdc path =
  let tbl : (string, xdc_port) Hashtbl.t = Hashtbl.create 64 in
  let set port key v =
    let cur = match Hashtbl.find_opt tbl port with
      | Some c -> c | None -> { xp_pin = None; xp_iostd = None } in
    let cur =
      if key = "PACKAGE_PIN" then { cur with xp_pin = Some v }
      else if key = "IOSTANDARD" then { cur with xp_iostd = Some v }
      else cur in
    Hashtbl.replace tbl port cur in
  (* [get_ports {LED[0]}] or [get_ports IO_CLK_P] *)
  let re_braced = Str.regexp "\\[get_ports[ \t]+{\\([^}]+\\)}" in
  let re_plain  = Str.regexp "\\[get_ports[ \t]+\\([A-Za-z0-9_/]+\\)" in
  let port_of line =
    if (try ignore (Str.search_forward re_braced line 0); true with Not_found -> false)
    then Some (Str.matched_group 1 line)
    else if (try ignore (Str.search_forward re_plain line 0); true with Not_found -> false)
    then Some (Str.matched_group 1 line)
    else None in
  let re_dict = Str.regexp "set_property[ \t]+-dict[ \t]+{\\([^}]*\\)}" in
  let re_one =
    Str.regexp "set_property[ \t]+\\(PACKAGE_PIN\\|IOSTANDARD\\)[ \t]+\\([^ \t]+\\)" in
  (try
     let ic = open_in path in
     (try
        while true do
          let line = input_line ic in
          let line = match String.index_opt line '#' with
            | Some i -> String.sub line 0 i | None -> line in
          match port_of line with
          | None -> ()
          | Some port ->
              if (try ignore (Str.search_forward re_dict line 0); true
                  with Not_found -> false) then begin
                let body = Str.matched_group 1 line in
                let toks =
                  List.filter (fun t -> t <> "")
                    (String.split_on_char ' '
                       (String.map (fun c -> if c = '\t' then ' ' else c) body)) in
                let rec pairs = function
                  | k :: v :: rest -> set port k v; pairs rest
                  | _ -> () in
                pairs toks
              end else if (try ignore (Str.search_forward re_one line 0); true
                           with Not_found -> false) then
                set port (Str.matched_group 1 line) (Str.matched_group 2 line)
        done
      with End_of_file -> ());
     close_in ic
   with Sys_error _ -> ());
  tbl

(* ------------------------------------------- nextpnr JSON, emitted directly *)

(* WHY THIS EXISTS.  The replay import used to be a four-tool pipeline:
   dcp2xml (RapidWright) -> xml2json (RapidWright) -> merge_hardblock_bels.py
   -> apply_vivado_lut_pins.py, with the last two reopening and rewriting the
   same JSON to patch things xml2json could not know about.  Every one of those
   patches was found the same way -- a feature count came out wrong and the
   cause turned out to be a pass silently disagreeing with the pass before it:

     * const nets called xml2json_gnd_src, so nextpnr's literal comparison
       against "$PACKER_GND_NET" failed and 160 CARRY4s lost PRECYINIT.C0;
     * CARRY4 O/CO emitted 1 bit wide, so port_xform found no wire for O[i];
     * hard-block BELs absent, so nextpnr scattered BUFGs and 542 clock
       features vanished;
     * the GT refclk buffer left unplaced, so it landed in the LEFT GT column
       and wrote three features into a tile with empty tilegrid bits{}.

   None of those were hard to fix once seen; the problem is that a chain of
   independent rewrites gives each fix its own opportunity to disagree with the
   others, and the only symptom is a number in a FASM diff.  Emitting the JSON
   ONCE, from the typed model, puts every one of these decisions in a single
   place where it can be read.

   LOGICAL CONNECTIVITY COMES FROM THE SIDECAR EDIF, not from the XML.  The XML
   knows placement (cell -> site/bel), intra-site state and routing, but a net's
   cell-pin membership is only recoverable from it by reimplementing the
   site-pin naming convention for every bel family -- LUT inputs are site pins
   <letter><digit>, an FF's D is reached through the FFMUX sitepip, and the GT
   has 420 intra-site nets.  That is a device model, and getting it subtly wrong
   is exactly the silent-corruption failure mode this suite keeps paying for.
   dcp2xml already writes the EDIF beside the XML; it is the authoritative
   logical netlist, so use it and keep the XML for what only it knows. *)

(* PROPERTY VALUE: bit vector or integer?
   Vivado's XML distinguishes them and we must not lose it.  A sized literal
   carries an apostrophe ("2'h3", "5'h10") and becomes a bit string; a plain
   decimal ("10000") is an INTEGER property.  Stringifying both loses the
   difference, and the consumer then has to guess: json2dcp treats any string of
   only 0/1/x as a bit vector, so SS_MOD_PERIOD=10000 arrived as binary 10000 =
   5'h10 and Vivado rejected it as out of range.  It only NOTICED that one
   because it range-checks it -- 28 property names here are decimals that look
   binary (CLKOUT2_DIVIDE, DIVCLK_DIVIDE, CPLL_REFCLK_DIV, READ_WIDTH_B ...),
   and the rest were being silently re-interpreted.
   Emitting an integer as a JSON NUMBER is the yosys convention and removes the
   ambiguity at the source. *)
let prop_json ~numbers v =
  let v = String.trim v in
  let all_digits = v <> "" && String.for_all (fun c -> c >= '0' && c <= '9') v in
  (* numbers:false keeps everything a STRING.  nextpnr reads every parameter
     with as_string() and asserts (is_string, nextpnr.h:365) on a JSON number,
     so the numeric form is only for json2dcp. *)
  if numbers && all_digits && not (String.contains v '\'') then
    match int_of_string_opt v with
    | Some i -> `Int i
    | None -> `String v
  else `String (Bir_to_nextpnr_json.verilog_lit_to_binary v)

let json_bel_leaf_map =
  [ (("BUFG", "BUFG"), "BUFGCTRL");
    (("BUFGCTRL", "BUFG"), "BUFGCTRL") ]

(* NOTHING is skipped here, and the empty list is the point.
   The inherited policy was "IO is nextpnr's to place: it derives pad sites from
   the pin constraints, and its bel names for them do not correspond to Vivado's
   at all".  The first half is true and the second is not: the DCP records IO
   placement like any other cell (IOB_X0Y140/OUTBUF_DCIEN), nextpnr names those
   bels identically, and it already ran to completion on a JSON carrying them.
   Skipping them made the replay depend on the XDC reproducing Vivado's pin
   assignment rather than on the checkpoint itself -- which is not a replay, and
   is what let the GT refclk buffer wander to the wrong quad when a PACKAGE_PIN
   went missing.
   The GT serial pads (sgmii_refclk_p/n, txp/txn, rxp/rxn) carry no bel in the
   XML at all: they are IPAD/OPAD sites inside the GT tile, not IOBs, so they
   fall through the "no XML cell" path and stay nextpnr's to place. *)
let json_io_skip_bel : string list = []

(* The SAME constant net has three names in play, one per tool, and they must
   all collapse onto nextpnr's or a constant looks like an ordinary signal:
     EDIF (logical side)   "<const0>" / "<const1>"
     XML  (physical side)  GLOBAL_LOGIC0 / GLOBAL_LOGIC1, driven from TIEOFFs
     nextpnr               $PACKER_GND_NET / $PACKER_VCC_NET, compared LITERALLY
   Missing the physical pair cost 6273 routing features; missing the EDIF pair
   leaves 160 CARRY4s with a CYINIT that looks dynamic, so fasm.cc emits a
   routing bel instead of PRECYINIT.C0 -- measured as 819 C0 features in
   Vivado's fasm against 22 in ours.  write_fixed_routes renames the physical
   pair identically, so both sides meet on the same name. *)
let json_const_rename = function
  | "GLOBAL_LOGIC1" | "<const1>" -> "$PACKER_VCC_NET"
  | "GLOBAL_LOGIC0" | "<const0>" -> "$PACKER_GND_NET"
  | s -> s

let dir_json : Edif_parser.port_direction -> string = function
  | Edif_parser.Input -> "input"
  | Edif_parser.Output -> "output"
  | Edif_parser.Inout -> "inout"

(* Split a Vivado logical pin reference into (port, bit): "S[0]" -> ("S", 0),
   "D" -> ("D", 0).  Used for routethrus, whose <pin log="..."> names the PARENT
   pin the LUT is carrying. *)
let split_pin p =
  match String.index_opt p '[' with
  | None -> (p, 0)
  | Some i ->
      let close = try String.index_from p i ']' with Not_found -> String.length p in
      let base = String.sub p 0 i in
      let idx =
        try int_of_string (String.sub p (i + 1) (close - i - 1)) with _ -> 0 in
      (base, idx)

let pin_suffix p =
  String.concat "" (String.split_on_char ']' (String.concat "" (String.split_on_char '[' p)))

(* EDIF ARRAY MEMBER INDEX -> VERILOG BIT INDEX.
   `(portref (member S 0) ...)` does NOT mean S[0].  An EDIF array's members are
   numbered 0..N-1 in DECLARATION order, and Xilinx declares every primitive bus
   descending -- `(port (array (rename S "S[3:0]") 4))` -- so member 0 is the
   MSB, S[3].  Taking the member index as the bit index mirrors every bus.

   PROVEN, not assumed: on send_divcnt_reg[0]_i_1 the O/CO/DI arrays came out as
   exact reversals of Vivado's, and the routethru that Vivado records on pin
   S[3] carries send_divcnt_reg[3] -- the net our member-0 slot held.  Checked
   across this EDIF, all 117 array ports are [H:L] with H>L and every declared
   width matches its member count, so the mapping is unconditional here; an
   ascending [L:H] declaration would need the range itself, which
   parse_array_name currently discards. *)
let edif_member_to_bit width k = if width > 1 then width - 1 - k else k

let write_nextpnr_json db edif_path out_path =
  let ed = Edif_parser.parse_schematic edif_path in

  (* ONE dialect.  The emitted JSON is the SUPERSET: physical port names with
     X_ORIG_PORT_* mapping back, NEXTPNR_BEL alongside BEL, integer parameters as
     JSON numbers, PAD cells for the top-level ports.  Consumers are expected to
     tolerate what they do not need rather than us shipping two incompatible
     files -- keeping two dialects means every future fix has to be made, and
     measured, twice. *)
  let phys_ports = true in
  (* SVS_JSON_XDC=<file>: per-port PACKAGE_PIN / IOSTANDARD for the PAD cells. *)
  let xdc = match Sys.getenv_opt "SVS_JSON_XDC" with
    | Some f -> read_xdc f
    | None -> Hashtbl.create 1 in

  (* Real placements only.  A routethru entry names the same cell but records a
     LUT it passes through, not where the cell lives. *)
  (* Site PERSONALITY.  An IOB site is typed IOB18, IOB18M or IOB18S depending
     on whether it is a plain single-ended pad or the master/slave half of an
     LVDS pair, and RapidWright refuses a differential cell on the wrong one
     ("Site type IOB18 not supported for cell type IBUFDS").  json2dcp takes the
     personality from a THREE-part bel string, SITE/SITETYPE/BEL
     (json2dcp.java:784), which is also what nextpnr writes for IO. *)
  let site_type = Hashtbl.create 4096 in
  List.iter (fun st -> Hashtbl.replace site_type st.s_name st.s_type) db.sites;
  let npnr_bel site leaf =
    let three =
      match Hashtbl.find_opt site_type site with
      | Some ty when ty <> "" ->
          let io_site =
            List.exists (fun p ->
                String.length site >= String.length p && String.sub site 0 (String.length p) = p)
              [ "IOB"; "IPAD"; "OPAD" ] in
          if io_site then Some (site ^ "/" ^ ty ^ "/" ^ leaf) else None
      | _ -> None in
    match three with Some b -> b | None -> site ^ "/" ^ leaf in

  let xml_by_name = Hashtbl.create 16384 in
  List.iter (fun c -> if not c.c_rt then Hashtbl.replace xml_by_name c.c_name c)
    db.cells;
  (* (site, bel) -> cell name, for sitepips whose source is a CELL output rather
     than a siteroute: a slice mux fed by "XOR" or "CY" is carrying the CARRY4's
     own O[i]/CO[i], which is a port on the cell, not a wire in the siteroutes. *)
  let cell_at = Hashtbl.create 8192 in
  List.iter (fun c -> if not c.c_rt && c.c_site <> "" then
                Hashtbl.replace cell_at (c.c_site, c.c_bel) c.c_name)
    db.cells;
  (* MACRO PLACEMENT.  RAM256X1S/RAM32M/RAM64M have no <cell> of their own in
     the XML -- only their decomposed leaves, "<macro>/RAMS64E_A" and friends,
     one per LUT of the slice.  Vivado's own placement file does record a line
     for the macro, and it points at the leaf on the HIGHEST lane letter
     (SLICE_X204Y14/D6LUT for a RAM256X1S whose leaves span A6LUT..D6LUT), which
     is what nextpnr's pack_dram then propagates down to the base sub-cell.
     Reproduce that from the leaves: maximal lane letter, ties broken by the
     smallest bel name so a slice holding both D5LUT and D6LUT is not ambiguous.
     Checked against the reference importer on all 38 macros in this design:
     38 agree, 0 differ.  Without it those 38 cells carry no BEL and nextpnr
     places the distributed RAMs itself, which is not a replay. *)
  let xml_leaf = Hashtbl.create 2048 in
  List.iter
    (fun c ->
       if not c.c_rt then
         match String.rindex_opt c.c_name '/' with
         | Some i ->
             Hashtbl.add xml_leaf (String.sub c.c_name 0 i) (c.c_site, c.c_bel, c.c_type)
         | None -> ())
    db.cells;
  let macro_bel name =
    let is_mem (_, bel, ty) =
      (try ignore (Str.search_forward (Str.regexp_string "LUT") bel 0); true
       with Not_found -> false)
      && String.length ty >= 3 && String.sub ty 0 3 = "RAM" in
    match List.filter is_mem (Hashtbl.find_all xml_leaf name) with
    | [] -> None
    | l ->
        let top = List.fold_left (fun a (_, b, _) -> max a b.[0]) '\000' l in
        let cand = List.filter (fun (_, b, _) -> b.[0] = top) l in
        let (site, bel, _) =
          List.fold_left (fun a (s, b, t) ->
              let (_, ab, _) = a in if b < ab then (s, b, t) else a)
            (List.hd cand) cand in
        Some (site ^ "/" ^ bel) in

  (* parent cell name -> (site, lut bel, parent logical pin) *)
  let rt_by_name = Hashtbl.create 1024 in
  List.iter
    (fun c ->
       if c.c_rt then
         List.iter (fun (belpin, log) ->
             Hashtbl.add rt_by_name c.c_name (c.c_site, c.c_bel, log, belpin))
           c.c_pins)
    db.cells;

  (* Yosys reserves bit ids 0 and 1 for the constant bits, so allocate from 2.
     Every EDIF net is scalar after flattening, hence one id per net. *)
  let next_id = ref 2 in
  let net_id = Hashtbl.create 16384 in
  let id_order = ref [] in
  let id_of_net n =
    match Hashtbl.find_opt net_id n with
    | Some i -> i
    | None ->
        let i = !next_id in
        incr next_id;
        Hashtbl.replace net_id n i;
        id_order := (n, i) :: !id_order;
        i in

  (* (instance, pin) -> (bit index option, net id).  The EDIF's instanceref uses
     the SAFE identifier, so this table is keyed by that, not the Vivado name. *)
  let conns = Hashtbl.create 65536 in
  let top_bits = Hashtbl.create 64 in
  List.iter
    (fun (nt : Edif_parser.net_info) ->
       let id = id_of_net (json_const_rename nt.name) in
       List.iter
         (fun (p : Edif_parser.net_pin) ->
            match p.inst with
            | Some i -> Hashtbl.add conns (i, p.pin) (p.index, id)
            | None -> Hashtbl.add top_bits p.pin (p.index, id))
         nt.connections)
    ed.nets;

  (* An unconnected bit of an otherwise-used bus gets its OWN named net.
     It must not be 0: that is the constant ZERO in this dialect, so padding
     with it would quietly tie the bit low instead of leaving it floating.
     The yosys literal "x" is the other candidate, and the reference importer
     spells it that way -- but "x" is a display convention, not a bit: json2dcp
     reads connection bits as integers and dies with
     "NumberFormatException: For input string: \"x\"" before writing a DCP, so
     a JSON spelled that way cannot be round-tripped back through RapidWright.
     A named dangling net satisfies both consumers at the cost of some extra
     netnames entries. *)
  let unconn = ref 0 in
  let fresh_unconn () =
    incr unconn; id_of_net (Printf.sprintf "$unconn$%d" !unconn) in
  let bit_json x = if x = -1 then `Int (fresh_unconn ()) else `Int x in

  let placed = ref 0 and unplaced = ref [] in

  (* Pass 1: resolve each instance to (vivado name, type, bel, params, ports) and
     a MUTABLE per-port bit array, so the routethru pass can rewire a single pin
     afterwards without rebuilding the cell. *)
  let resolved =
    List.map
      (fun (i : Edif_parser.instance_info) ->
         let bel =
           if List.mem i.cell_type json_io_skip_bel then None
           else
             match Hashtbl.find_opt xml_by_name i.vivado_name with
             | None -> macro_bel i.vivado_name
             | Some c when c.c_site = "" -> None
             | Some c ->
                 let leaf =
                   match List.assoc_opt (i.cell_type, c.c_bel) json_bel_leaf_map with
                   | Some l -> l
                   | None -> c.c_bel in
                 Some (if leaf = "" then c.c_site else c.c_site ^ "/" ^ leaf) in
         (match bel with
          | Some _ -> incr placed
          | None -> if not (List.mem i.cell_type json_io_skip_bel)
                    then unplaced := (i.vivado_name, i.cell_type) :: !unplaced);

         (* X_ORIG_* -- what RapidWright's json2dcp needs to rebuild a cell.
            X_ORIG_TYPE is the Unisim type and X_ORIG_PORT_<belpin> the logical
            pin on it; nextpnr writes them when its packer folds a primitive into
            a SLICE_LUTX/SLICE_FFX, and json2dcp refuses a cell without them
            ("has no X_ORIG_TYPE attribute ... aborting rather than silently
            dropping the cell").  We never pack, so our types ARE the Unisim
            types, and the XML already records the physical-to-logical pin map
            per cell -- <pin bel="A2" log="I2"/> is exactly X_ORIG_PORT_A2="I2".
            Emitting them lets the JSON round-trip back through json2dcp into a
            DCP without going through nextpnr at all. *)
         let orig_attrs =
           match Hashtbl.find_opt xml_by_name i.vivado_name with
           (* No <cell> of its own: a MACRO (RAM32M/RAM64M/RAM256X1S), recorded
              in the XML only as its decomposed leaves.  Tag the type anyway --
              json2dcp aborts without it -- and leave the pin map to
              RapidWright's default, which is what it does for any cell with no
              X_ORIG_PORT (json2dcp.java:398). *)
           | None -> [ ("X_ORIG_TYPE", `String i.cell_type) ]
           | Some c ->
               (* NEXTPNR_BEL is the DCP dialect's spelling and can be THREE
                  parts for IO (SITE/SITETYPE/BEL).  nextpnr's own
                  attributesToArchInfo consumes this attribute preferentially and
                  cannot parse that form -- it resolves to an empty BelId and
                  bindBel asserts during JSON load.  Emit it only when targeting
                  json2dcp; nextpnr uses the two-part "BEL". *)
               (if c.c_site = "" || not phys_ports then []
                else [ ("NEXTPNR_BEL", `String (npnr_bel c.c_site c.c_bel)) ])
               @ ("X_ORIG_TYPE", `String c.c_type)
                 :: List.map (fun (belpin, log) ->
                        ("X_ORIG_PORT_" ^ belpin, `String log))
                      c.c_pins in

         let ports =
           match Hashtbl.find_opt ed.library_cells i.cell_type with
           | Some ps -> ps
           | None -> [] in

         let conn =
           List.filter_map
             (fun (p : Edif_parser.port_info) ->
                match Hashtbl.find_all conns (i.name, p.name) with
                | [] -> None
                | entries ->
                    let declared = if p.width > 0 then p.width else 1 in
                    let widest =
                      List.fold_left
                        (fun a (ix, _) ->
                           match ix with Some k -> max a (k + 1) | None -> a)
                        1 entries in
                    let width = max declared widest in
                    let arr = Array.make width (-1) in
                    List.iter
                      (fun (ix, id) ->
                         match ix with
                         | Some k when k >= 0 && k < width ->
                             arr.(edif_member_to_bit width k) <- id
                         | _ -> if arr.(0) = -1 then arr.(0) <- id)
                      entries;
                    Some (p.name, arr))
             ports in

         let params =
           let init =
             match i.init with
             | Some v -> [ ("INIT", `String (Bir_to_nextpnr_json.verilog_lit_to_binary v)) ]
             | None -> [] in
           (* Drop properties with an EMPTY value.  Vivado emits bookkeeping
              entries such as METHODOLOGY_DRC_VIOS with no value at all (22 of
              them here, on RAM32M/RAM64M); they carry nothing, and a consumer
              that reads parameters as numbers chokes -- json2dcp dies with
              "NumberFormatException: Zero length BigInteger" before writing a
              DCP.  The reference importer passes them through and fails the
              same way, so this is not a difference between the two. *)
           init
           @ List.filter_map
               (fun (k, v) ->
                  if String.trim v = "" then None else Some (k, prop_json ~numbers:phys_ports v))
               i.properties in

         (i.vivado_name, i.cell_type, bel, params, ports, conn, orig_attrs))
      ed.instances in

  (* Pass 1b: MACRO EXPANSION.  RapidWright will not accept a macro as a cell --
     "The unisim RAM32M is a transformed prim and cannot be directly
     instantiated.  Its transform includes [RAMS32, RAMD32, ...] which should be
     used instead" -- and Vivado's own physical database agrees: the XML records
     no <cell> for the macro, only its leaves (<macro>/RAMS64E_A, <macro>/F7.A,
     <macro>/F8 ...), each with its own bel and pin map.  Only the EDIF keeps the
     macro whole, because that is how the design instantiated it.

     So flatten one level: instantiate the macro's own cell definition, which the
     EDIF carries as a cell WITH a (contents) section, and wire it up:
       * an internal net that reaches a macro PORT becomes the net the macro
         instance has on that port bit;
       * a purely internal net becomes a fresh net named <macro>/<net>.
     The leaves then join the XML on <macro>/<leaf>, which is exactly how Vivado
     names them, so each gets its true bel and X_ORIG pin map. *)
  let sub_by_type = Hashtbl.create 16 in
  List.iter
    (fun (c : Edif_parser.edif_data) ->
       if c.module_name <> ed.module_name then
         Hashtbl.replace sub_by_type c.module_name c)
    (Edif_parser.parse_all_netlist_cells (Edif_parser.read_file edif_path));

  let macro_leaves = ref [] and macros_expanded = ref 0 and leaves_made = ref 0 in
  (* Keep each macro's PORT connections after the macro cell itself is gone.
     Vivado routes the macro's lane outputs under names that live inside it
     ("<macro>/DOC1"), and resolving those to a logical net needs the macro's
     own port map -- which expansion is about to remove from the netlist. *)
  let macro_conn = Hashtbl.create 64 in
  let port_width_of ty pname =
    match Hashtbl.find_opt ed.library_cells ty with
    | None -> 1
    | Some ps ->
        (match List.find_opt (fun (p : Edif_parser.port_info) -> p.name = pname) ps with
         | Some p -> max 1 p.width
         | None -> 1) in

  let expand_macro (mname, mty, _bel, _params, _ports, mconn, _oa) =
    match Hashtbl.find_opt sub_by_type mty with
    | None -> false
    | Some (def : Edif_parser.edif_data) ->
        incr macros_expanded;
        Hashtbl.replace macro_conn mname mconn;
        (* internal (instance safe id, pin) -> (member index option, net id) *)
        let icon = Hashtbl.create 64 in
        List.iter
          (fun (nt : Edif_parser.net_info) ->
             (* Does this internal net touch a macro port?  If so it IS the
                outside net; otherwise it needs one of its own. *)
             let outside =
               List.fold_left
                 (fun acc (p : Edif_parser.net_pin) ->
                    match acc, p.inst with
                    | Some _, _ -> acc
                    | None, Some _ -> None
                    | None, None ->
                        let w = port_width_of mty p.pin in
                        let bit = match p.index with
                          | Some k -> edif_member_to_bit w k
                          | None -> 0 in
                        (match List.assoc_opt p.pin mconn with
                         | Some arr when bit >= 0 && bit < Array.length arr && arr.(bit) >= 0 ->
                             Some arr.(bit)
                         | _ -> None))
                 None nt.connections in
             let id = match outside with
               | Some i -> i
               | None -> id_of_net (mname ^ "/" ^ nt.name) in
             List.iter
               (fun (p : Edif_parser.net_pin) ->
                  match p.inst with
                  | Some li -> Hashtbl.add icon (li, p.pin) (p.index, id)
                  | None -> ())
               nt.connections)
          def.nets;

        List.iter
          (fun (li : Edif_parser.instance_info) ->
             let lname = mname ^ "/" ^ li.vivado_name in
             incr leaves_made;
             let bel, orig =
               match Hashtbl.find_opt xml_by_name lname with
               | None -> (None, [ ("X_ORIG_TYPE", `String li.cell_type) ])
               | Some c ->
                   ((if c.c_site = "" then None else Some (c.c_site ^ "/" ^ c.c_bel)),
                    (if c.c_site = "" || not phys_ports then []
                     else [ ("NEXTPNR_BEL", `String (npnr_bel c.c_site c.c_bel)) ])
                    @ ("X_ORIG_TYPE", `String c.c_type)
                      :: List.map (fun (bp, lg) -> ("X_ORIG_PORT_" ^ bp, `String lg))
                           c.c_pins) in
             let lports =
               match Hashtbl.find_opt ed.library_cells li.cell_type with
               | Some ps -> ps | None -> [] in
             let lconn =
               List.filter_map
                 (fun (p : Edif_parser.port_info) ->
                    match Hashtbl.find_all icon (li.name, p.name) with
                    | [] -> None
                    | entries ->
                        let width = max (if p.width > 0 then p.width else 1)
                                      (List.fold_left
                                         (fun a (ix, _) ->
                                            match ix with Some k -> max a (k + 1) | None -> a)
                                         1 entries) in
                        let arr = Array.make width (-1) in
                        List.iter
                          (fun (ix, id) ->
                             match ix with
                             | Some k when k >= 0 && k < width ->
                                 arr.(edif_member_to_bit width k) <- id
                             | _ -> if arr.(0) = -1 then arr.(0) <- id)
                          entries;
                        Some (p.name, arr))
                 lports in
             let lparams =
               (match li.init with
                | Some v -> [ ("INIT", `String (Bir_to_nextpnr_json.verilog_lit_to_binary v)) ]
                | None -> [])
               @ List.filter_map
                   (fun (k, v) ->
                      if String.trim v = "" then None else Some (k, prop_json ~numbers:phys_ports v))
                   li.properties in
             macro_leaves :=
               (lname, li.cell_type, bel, lparams, lports, lconn, orig) :: !macro_leaves)
          def.instances;
        true in

  (* Sequence these explicitly.  OCaml evaluates arguments RIGHT TO LEFT, so
     writing `List.filter ... resolved @ List.rev !macro_leaves` reads
     macro_leaves BEFORE the filter runs and populates it -- the expansion still
     happens, the counters still say 248, and every leaf is silently discarded. *)
  (* Vivado's physical database contains NO GND/VCC cells and no TIEOFF site
     instances -- constants are implicit, carried by the GLOBAL_LOGIC0/1 nets we
     rename to $PACKER_GND_NET/$PACKER_VCC_NET.  The EDIF still instantiates a
     GND and a VCC to drive them, and those two are the only cells left with no
     placement; json2dcp then dereferences their absent NEXTPNR_BEL
     (json2dcp.java:772).  Drop them and keep the constant NETS, which both
     consumers recognise by name. *)
  let is_const_cell (_, ty, _, _, _, _, _) = ty = "GND" || ty = "VCC" in
  let kept =
    List.filter (fun r -> not (is_const_cell r) && not (expand_macro r)) resolved in
  let resolved = kept @ List.rev !macro_leaves in


  (* Pass 2: routethrus.  Vivado records a LUT carrying one INPUT pin of another
     cell into the site.  nextpnr has no such concept, so make it explicit: a
     LUT1 pass-through (INIT "10") placed on that LUT bel, spliced between the
     original net and the parent pin.  Without these 305 cells the LUT bels look
     free, and nextpnr is entitled to pack something else onto them. *)
  let rt_cells = ref [] in
  (* (site, lut bel) -> the net the routethru LUT now drives.  Vivado models a
     routethru as the SAME net passing through the LUT, so its siteroutes name
     the original; we split it in two (orig -> LUT -> $RTOUT$x -> sink).  Any
     sitepip fed by that LUT output must therefore follow the NEW net, or the
     mux and the flop end up claiming different nets on one intra-site path --
     "RTSTAT-6 Partial route conflicts: 190 net(s)", every one a $RTOUT$D. *)
  let rt_out_at = Hashtbl.create 1024 in
  List.iter
    (fun (vname, _ty, _bel, _params, _ports, conn, _oa) ->
       List.iter
         (fun (site, lutbel, logpin, belpin) ->
            let (port, bit) = split_pin logpin in
            match List.assoc_opt port conn with
            | Some arr when bit >= 0 && bit < Array.length arr && arr.(bit) <> -1 ->
                let orig = arr.(bit) in
                let sfx = pin_suffix logpin in
                let outpin =
                  if String.length lutbel >= 5 && String.sub lutbel 1 1 = "5"
                  then "O5" else "O6" in
                let out_id = id_of_net (vname ^ "$RTOUT$" ^ sfx) in
                arr.(bit) <- out_id;
                Hashtbl.replace rt_out_at (site, lutbel) (vname ^ "$RTOUT$" ^ sfx);
                rt_cells :=
                  (vname ^ "$RT$" ^ sfx,
                   `Assoc
                     [ ("hide_name", `Int 1);
                       ("type", `String "LUT1");
                       ("parameters", `Assoc [ ("INIT", `String "10") ]);
                       ("attributes",
                        (* Tag the synthesised pass-through exactly as a packer
                           would, so json2dcp can rebuild it: its physical input
                           is the LUT pin Vivado routed through (belpin) and its
                           output the matching O5/O6. *)
                        `Assoc ([ ("BEL", `String (site ^ "/" ^ lutbel)) ]
                                @ (if phys_ports
                                   then [ ("NEXTPNR_BEL", `String (site ^ "/" ^ lutbel)) ]
                                   else []) @ [
                                 ("X_ORIG_TYPE", `String "LUT1");
                                 ("X_ORIG_PORT_" ^ belpin, `String "I0");
                                 ("X_ORIG_PORT_" ^ outpin, `String "O") ]));
                       (* Same port dialect as every other cell.  These are
                          built here rather than by cell_json, so they were
                          missed when the physical spelling went in: their ports
                          stayed I0/O while their X_ORIG_PORT keys were the bel
                          pins, json2dcp wiped the default pin map and matched
                          neither, and all 569 arrived with ZERO input pins --
                          "Invalid physical equation ... the bit width of the
                          INIT value does not match the number of used input
                          pins '0'", 100 of them reported by Vivado. *)
                       ("port_directions",
                        `Assoc (if phys_ports
                                then [ (belpin, `String "input");
                                       (outpin, `String "output") ]
                                else [ ("I0", `String "input");
                                       ("O", `String "output") ]));
                       ("connections",
                        `Assoc (if phys_ports
                                then [ (belpin, `List [ `Int orig ]);
                                       (outpin, `List [ `Int out_id ]) ]
                                else [ ("I0", `List [ `Int orig ]);
                                       ("O", `List [ `Int out_id ]) ])) ])
                  :: !rt_cells
            | _ -> ())
         (Hashtbl.find_all rt_by_name vname))
    resolved;

  (* PORT NAMING DIALECT.
     Our natural spelling is the LOGICAL one -- a CARRY4 has CI/S/O/CO, an FDRE
     has C/D/CE/R/Q -- which is what nextpnr's JSON frontend wants on INPUT.
     json2dcp wants the other one: nextpnr's OUTPUT, where a packed cell's ports
     carry the PHYSICAL bel pin names (CIN, S0, A6, CK) and X_ORIG_PORT_<phys>
     names the logical pin behind each.  The two are not interchangeable, and
     the mismatch is not benign: json2dcp treats the presence of ANY
     X_ORIG_PORT_* as "this cell was repacked", REMOVES every default pin
     mapping RapidWright made, and rebuilds the map only for JSON ports that
     have a matching key (json2dcp.java:876).  Keyed physically while the ports
     are named logically, almost nothing is re-added: the cell ends up with no
     usable pin map, no logical connection is created, and 736 instances --
     every cascading CARRY4 among them -- arrived in the rebuilt EDIF with no
     portrefs at all.
     SVS_JSON_PHYS_PORTS=1 emits the physical spelling for that consumer.  The
     logical spelling stays the default because nextpnr cannot pack a LUT6 whose
     ports are A1..A6. *)
  let cell_json (vname, ty, bel, params, ports, conn, orig_attrs) =
    let logical_dir pname =
      match List.find_opt (fun (p : Edif_parser.port_info) -> p.name = pname) ports with
      | Some p -> dir_json p.direction
      | None -> "input" in
    let dirs, conn_json =
      if not phys_ports then
        (List.map (fun (p : Edif_parser.port_info) ->
             (p.name, `String (dir_json p.direction))) ports,
         List.map (fun (nm, arr) -> (nm, `List (Array.to_list (Array.map bit_json arr)))) conn)
      else begin
        (* One scalar port per PHYSICAL pin, taken from the X_ORIG_PORT_<phys>
           entries the XML already gave us; the net is the bit of the logical
           port that pin carries.  A logical pin shared across two physical pins
           (nextpnr records "I3 I4") yields a port for each, which is exactly
           what the packed netlist looks like. *)
        let ds = ref [] and cs = ref [] in
        List.iter
          (fun (k, v) ->
             let pfx = "X_ORIG_PORT_" in
             let lk = String.length pfx in
             if String.length k > lk && String.sub k 0 lk = pfx then begin
               let belpin = String.sub k lk (String.length k - lk) in
               match v with
               | `String logs ->
                   List.iter
                     (fun log ->
                        if log = "GLOBAL_LOGIC0" || log = "GLOBAL_LOGIC1" then begin
                          (* Vivado records a constant-tied bel pin with the NET
                             in the log field ("<pin bel=\"DCITERMDISABLE\"
                             log=\"GLOBAL_LOGIC0\"/>"), not a logical pin name.
                             Treated as a port name it resolves to nothing and
                             the pin is dropped -- which is how every OBUF lost
                             its DCITERMDISABLE.  Wire it straight to the
                             constant net instead. *)
                          ds := (belpin, `String "input") :: !ds;
                          cs := (belpin,
                                 `List [ `Int (id_of_net (json_const_rename log)) ]) :: !cs
                        end else if log <> "" then begin
                          let (port, bit) = split_pin log in
                          match List.assoc_opt port conn with
                          | Some arr when bit >= 0 && bit < Array.length arr ->
                              ds := (belpin, `String (logical_dir port)) :: !ds;
                              cs := (belpin, `List [ bit_json arr.(bit) ]) :: !cs
                          | _ ->
                              (* The XML records the pin but the logical netlist
                                 carries no net for it.  A RAM64M's leaves are the
                                 clear case: Vivado maps WA7->WADR6 and WA8->WADR7
                                 on every RAMD64E, while the macro definition has
                                 ZERO portrefs to them -- the pins exist in the
                                 physical pin MAP, unconnected, because the
                                 primitive supports deeper configurations.
                                 Emitting nothing loses the mapping entirely
                                 (json2dcp builds it from the JSON's ports), and
                                 the rebuilt cell comes back short of those pins:
                                 40 DRAM leaves and 31 CARRY4s in the golden-vs-
                                 latest compare.  Declare the port WITHOUT a
                                 connection: json2dcp builds cell.ports from
                                 port_directions (json2dcp.java:360) and only
                                 attaches nets from connections, so the pin
                                 mapping survives while the pin stays netless --
                                 which is what Vivado records.  Giving it a net
                                 instead makes the line look USED and dcp2fasm
                                 emits WA7USED/WA8USED that the golden lacks
                                 (20 spurious features), plus driverless nets. *)
                              ds := (belpin, `String (logical_dir port)) :: !ds
                        end)
                     (String.split_on_char ' ' logs)
               | _ -> ()
             end)
          orig_attrs;
        (List.rev !ds, List.rev !cs)
      end in
    (vname,
     `Assoc
       [ ("hide_name", `Int 1);
         ("type", `String ty);
         ("parameters", `Assoc params);
         (* BEL and NEXTPNR_BEL carry the same string for different readers:
            nextpnr's JSON frontend honours "BEL" on INPUT, while json2dcp reads
            "NEXTPNR_BEL" because that is what nextpnr writes on OUTPUT
            (json2dcp.java:625, nc.attrs.get("NEXTPNR_BEL").split("/")).  Emit
            both so one file serves both directions. *)
         ("attributes",
          `Assoc ((match bel with
                   | Some b -> [ ("BEL", `String b) ]
                   | None -> []) @ orig_attrs));
         ("port_directions", `Assoc dirs);
         ("connections", `Assoc conn_json) ]) in

  let cells = List.map cell_json resolved @ List.rev !rt_cells in

  (* Top-level ports.  A port's bits come from the nets that reference it with
     inst = None; width from the EDIF interface declaration. *)
  let port_bits_tbl = Hashtbl.create 64 in
  let ports =
    List.map
      (fun (p : Edif_parser.port_info) ->
         let entries = Hashtbl.find_all top_bits p.name in
         let width =
           List.fold_left
             (fun a (ix, _) -> match ix with Some k -> max a (k + 1) | None -> a)
             (max 1 p.width) entries in
         let arr = Array.make width (-1) in
         List.iter
           (fun (ix, id) ->
              match ix with
              | Some k when k >= 0 && k < width ->
                  arr.(edif_member_to_bit width k) <- id
              | _ -> if arr.(0) = -1 then arr.(0) <- id)
           entries;
         Hashtbl.replace port_bits_tbl p.name (arr, dir_json p.direction);
         (p.name,
          `Assoc
            [ ("direction", `String (dir_json p.direction));
              ("bits", `List (Array.to_list (Array.map bit_json arr))) ]))
      ed.ports in

  (* TOP-LEVEL PAD CELLS.  Vivado's physical database puts a <PORT> cell on the
     PAD bel of every IO site it uses -- 20 of them here, one per top-level port
     ("UART_RX" on IOB_X0Y31/PAD, "sgmii_rxp" on IPAD_X2Y7/PAD).  We were
     dropping them, because they have no counterpart in the logical netlist, and
     then nothing anchored the port to its site: XDEF restore reported
     "Instance UART_RX_IBUF_inst does not exist", "Net UART_RX_IBUF does not
     exist" and "Site IOB_X0Y31 ... 0 placed instances but we read indicates it
     has 1" -- 32 messages, and placement for 9 sites silently not restored.
     json2dcp rebuilds them from a cell of type IOB_PAD carrying X_IO_DIR and a
     PAD port (json2dcp.java:1821), which is nextpnr's spelling for the same
     thing, so emit that. *)
  let pad_cells =
    if not phys_ports then []
    else
      List.filter_map
        (fun c ->
           (* The XML escapes the type as "&lt;PORT&gt;"; whether the reader
              hands it back decoded depends on the parser, so accept both. *)
           let is_port_cell =
             c.c_type = "<PORT>" || c.c_type = "&lt;PORT&gt;" || c.c_type = "PORT" in
           if not is_port_cell || c.c_site = "" then None
           else
             let (pname, bit) = split_pin c.c_name in
             match Hashtbl.find_opt port_bits_tbl pname with
             | Some (arr, dir) when bit >= 0 && bit < Array.length arr ->
                 let iodir = match dir with
                   | "input" -> "IN" | "output" -> "OUT" | _ -> "INOUT" in
                 Some (c.c_name,
                       `Assoc
                         [ ("hide_name", `Int 1);
                           (* "PAD" is the 7-series spelling (json2dcp:727 takes
                              either that or UltraScale's "IOB_PAD").  X_ORIG_TYPE
                              is needed only to clear the guard at :715, which
                              aborts on any cell without one; the PAD branch
                              short-circuits before the value is used as a
                              Unisim. *)
                           ("type", `String "PAD");
                           ("parameters", `Assoc []);
                           ("attributes",
                            `Assoc ([ ("X_ORIG_TYPE", `String "PAD");
                                      ("BEL", `String (c.c_site ^ "/" ^ c.c_bel));
                                      ("NEXTPNR_BEL",
                                       `String (npnr_bel c.c_site c.c_bel));
                                      ("X_IO_DIR", `String iodir) ]
                                     @ (match Hashtbl.find_opt xdc c.c_name with
                                        | None -> []
                                        | Some x ->
                                            (match x.xp_pin with
                                             | Some v -> [ ("PACKAGE_PIN", `String v) ]
                                             | None -> [])
                                            @ (match x.xp_iostd with
                                               | Some v -> [ ("IOSTANDARD", `String v) ]
                                               | None -> []))));
                           (* Direction is from the CELL's point of view and is
                              the opposite of the port's: on an INPUT port the pad
                              DRIVES the net into the die, on an OUTPUT port it
                              RECEIVES it.  Stated the other way round, the pad
                              becomes a second driver of the output net and
                              Vivado reports "Output of OBUF instance
                              LED_OBUF[0]_inst is not driving any port". *)
                           ("port_directions",
                            `Assoc [ ("PAD", `String (if iodir = "IN" then "output" else "input")) ]);
                           ("connections",
                            `Assoc [ ("PAD", `List [ bit_json arr.(bit) ]) ]) ])
             | _ -> None)
        db.cells in

  (* ROUTING, so the JSON describes the whole implementation and not just its
     placement.  nextpnr's own routed-JSON dialect carries it as a per-net
     attribute of "wire;pip;strength" triples (common/nextpnr.cc): an EMPTY pip
     field means bindWire on that wire, otherwise bindPip on that pip and the
     wire field is ignored.  Names are exactly what getWireName/getPipName
     produce -- "TILE/WIRE" or "SITEWIRE/SITE/PIN" for wires, "SRC->DST" for a
     fabric pip -- which is the same spelling write_fixed_routes already emits,
     so the two exports cannot drift apart.
     Strength 4 is STRENGTH_LOCKED, what applyFixedRoutes binds at, so router2
     treats an imported route as immovable rather than a suggestion.

     A CAVEAT worth knowing before relying on this as a nextpnr INPUT: the
     ROUTING attribute is consumed by attributesToArchInfo() at JSON LOAD, i.e.
     BEFORE packing, whereas --fixed-routes is applied inside Arch::route(),
     after packing and placement.  Packing rewrites this netlist heavily, so
     pre-pack bindings are not equivalent to the fixed-routes path.  It is
     nonetheless what makes the JSON self-contained for FASM generation, where
     the interconnect bits come from exactly these pips. *)
  let want_routing = Sys.getenv_opt "SVS_JSON_ROUTING" <> None in
  let routing_of = Hashtbl.create 8192 in
  let routed_nets = ref 0 and routed_pips = ref 0 and remapped = ref 0 in

  (* MACRO-INTERNAL NET NAMES.  Vivado routes a macro's lane outputs under names
     that live INSIDE the macro -- "…/cpuregs_reg_r2_0_31_18_23/DOC1" for a
     RAM32M, "…/u_ram/O0" for a RAM256X1S -- while the logical netlist knows the
     same signal only at the macro's PORT.  Keyed by the raw name, 105 routed
     nets matched nothing and their 821 pips were silently dropped from the
     export: a FASM built from that JSON would be missing exactly the
     interconnect feeding the distributed RAMs.
     Resolve "<cell>/<PORT><index>" through the macro's own connections, which
     pass 1 has already built.  Checked on this design: 821 of 821 pips resolve,
     none left over. *)
  let name_of_id = Hashtbl.create 16384 in
  List.iter (fun (n, i) -> Hashtbl.replace name_of_id i n) !id_order;
  let conn_by_cell = Hashtbl.create 8192 in
  List.iter (fun (vname, _, _, _, _, conn, _) -> Hashtbl.replace conn_by_cell vname conn)
    resolved;
  let json_net_name raw =
    let n = json_const_rename raw in
    if Hashtbl.mem net_id n then n
    else
      match String.rindex_opt n '/' with
      | None -> n
      | Some i ->
          let parent = String.sub n 0 i
          and leaf = String.sub n (i + 1) (String.length n - i - 1) in
          let j = ref (String.length leaf) in
          while !j > 0 && leaf.[!j - 1] >= '0' && leaf.[!j - 1] <= '9' do decr j done;
          let port = String.sub leaf 0 !j in
          let idx =
            if !j = String.length leaf then 0
            else try int_of_string (String.sub leaf !j (String.length leaf - !j))
                 with _ -> 0 in
          (match (match Hashtbl.find_opt conn_by_cell parent with
                  | Some c -> Some c
                  | None -> Hashtbl.find_opt macro_conn parent) with
           | None -> n
           | Some conn ->
               (match List.assoc_opt port conn with
                | Some arr when idx >= 0 && idx < Array.length arr && arr.(idx) >= 0 ->
                    (match Hashtbl.find_opt name_of_id arr.(idx) with
                     | Some nm -> incr remapped; nm
                     | None -> n)
                | _ -> n)) in

  if want_routing then
    List.iter
      (fun n ->
         if n.n_pips <> [] then begin
           incr routed_nets;
           let b = Buffer.create 512 in
           let first = ref true in
           let add wire pip =
             if not !first then Buffer.add_char b ';';
             first := false;
             Buffer.add_string b wire; Buffer.add_char b ';';
             Buffer.add_string b pip;  Buffer.add_string b ";4" in
           (* BOTH ends.  A net's wires include the site pins it LEAVES from and
              the ones it ARRIVES at; emitting only the sources leaves the net
              stopping short of its sinks.  It shows up first on the carry
              cascade, whose entire physical evidence is
                sitepin SLICE_X202Y9/COUT (out)
                pip     CLBLM_M_COUT -> CLBLM_M_COUT_N
                sitepin SLICE_X202Y10/CIN (in)
              -- with the sink pin missing the net never reaches the next
              slice's CIN, so dcp2fasm sees no carry-in and writes
              PRECYINIT.C0 where Vivado has PRECYINIT.CIN: 138 of them, the
              exact count of CARRY4s cascading in this design.  It also cost
              site pins generally, 26023 in Vivado's database against 5586 in
              the round-tripped one. *)
           List.iter
             (fun (site, pin) -> add (Printf.sprintf "SITEWIRE/%s/%s" site pin) "")
             (n.n_srcpins @ n.n_snkpins);
           List.iter
             (fun (tile, src, dst, rev) ->
                incr routed_pips;
                (* A REVERSED bidirectional pip must be asked for in the
                   direction the net actually drives, not in the device's
                   storage order.  json2dcp resolves that request through its
                   REVERSE_BIDIR path and sets the PIP's reversed flag, which is
                   what dcp2fasm reads to decide which wire is the source.  Ask
                   for the storage order instead and the flag stays clear: the
                   feature comes out as TILE.src.dst where Vivado has
                   TILE.dst.src -- 75 features spelled backwards, each one also
                   counting as a miss for the one it should have been. *)
                let src, dst = if rev then dst, src else src, dst in
                add (Printf.sprintf "%s/%s" tile dst)
                    (Printf.sprintf "%s/%s->%s/%s" tile src tile dst))
             n.n_pips;
           (* Two XML nets can resolve to one JSON net (a macro lane output and
              the logical net it feeds), so APPEND -- replacing would discard
              one of the two routes and the loss would be invisible. *)
           let key = json_net_name n.n_name in
           let prev = match Hashtbl.find_opt routing_of key with
             | Some p -> p ^ ";" | None -> "" in
           Hashtbl.replace routing_of key (prev ^ Buffer.contents b)
         end)
      db.nets;

  (* INTRA-SITE ROUTING.  The fabric pips above are only half of a route: the
     other half is the SITE PIPs -- the slice's own muxes (xFFMUX, xOUTMUX,
     CLKINV, SRUSEDMUX, CEUSEDMUX ...).  Leaving them out cost 4,506 FASM
     features, every one of them a CLB site feature, measured against the golden.
     nextpnr spells them SITEPIP/<site>/<bel>/<inpin> (arch.cc:334) and json2dcp
     applies them with addSitePIP (json2dcp.java:1635).

     ATTRIBUTION is the awkward part: the XML lists sitepips per SITEINST with no
     net, while ROUTING is per net.  json2dcp only uses the net for
     routeIntraSiteNet -- addSitePIP happens regardless -- so a sitepip we cannot
     place on the right net is dropped rather than guessed onto a wrong one, which
     would invent an intra-site connection that does not exist.
     The rules below recover 7,405 of 8,690 (85.2%); each one is a lookup in the
     site's OWN siteroutes, not a device assumption:
       R1  <L>{FF,5FF,OUT}MUX fed by O6/O5  -> that slice letter's 6LUT/5LUT
       R2  input <L>Q / <L>5Q              -> that slice letter's FF / 5FF
       R3  SRUSEDMUX.IN                    -> the net entering on the SR site pin
       R4  CEUSEDMUX.IN                    -> the net entering on the CE site pin
       R5  CLKINV                          -> the net entering on the CLK site pin
       R6  input "0"/"1"                   -> the constant nets
       R7  otherwise, a UNIQUE siteroute with that source pin *)
  let sitepips_added = ref 0 and sitepips_skipped = ref 0 in
  (* (site, pin) -> the net that LEAVES the site on that pin.  The xUSED muxes
     gate exactly that: AUSED enables the slice's "A" output.  Attributing them
     to the driven net -- rather than to GND, which is what the bare in="0"
     invited -- is what keeps the output path connected. *)
  let src_at = Hashtbl.create 8192 and snk_at = Hashtbl.create 8192 in
  List.iter
    (fun n ->
       List.iter (fun (site, pin) ->
           if not (Hashtbl.mem src_at (site, pin)) then
             Hashtbl.replace src_at (site, pin) n.n_name)
         n.n_srcpins;
       List.iter (fun (site, pin) ->
           if not (Hashtbl.mem snk_at (site, pin)) then
             Hashtbl.replace snk_at (site, pin) n.n_name)
         n.n_snkpins)
    db.nets;
  if want_routing then
    List.iter
      (fun st ->
         if st.s_pips <> [] then begin
           let by_belpin = Hashtbl.create 32 and by_bel = Hashtbl.create 32 in
           List.iter (fun r ->
               if not (Hashtbl.mem by_belpin (r.sr_bel, r.sr_pin)) then
                 Hashtbl.replace by_belpin (r.sr_bel, r.sr_pin) r.sr_net;
               if not (Hashtbl.mem by_bel r.sr_bel) then
                 Hashtbl.replace by_bel r.sr_bel r.sr_net)
             st.s_routes;
           let slice_letter b =
             if String.length b > 0 && b.[0] >= 'A' && b.[0] <= 'D'
             then Some (String.make 1 b.[0]) else None in
           let ends_with suf b =
             let lb = String.length b and ls = String.length suf in
             lb >= ls && String.sub b (lb - ls) ls = suf in
           List.iter
             (fun sp ->
                let net =
                  (* R1 *)
                  let lut_out_net l pin =
                    let bel = l ^ (if pin = "O6" then "6LUT" else "5LUT") in
                    match Hashtbl.find_opt rt_out_at (st.s_name, bel) with
                    | Some rt -> Some rt          (* rewired by a routethru *)
                    | None -> Hashtbl.find_opt by_belpin (bel, pin) in
                  let r1 =
                    if (ends_with "FFMUX" sp.sp_bel || ends_with "OUTMUX" sp.sp_bel)
                       && (sp.sp_in = "O6" || sp.sp_in = "O5") then
                      match slice_letter sp.sp_bel with
                      | Some l -> lut_out_net l sp.sp_in
                      | None -> None
                    else None in
                  let r2 () =
                    let n = String.length sp.sp_in in
                    if n >= 2 && sp.sp_in.[n-1] = 'Q'
                       && sp.sp_in.[0] >= 'A' && sp.sp_in.[0] <= 'D' then
                      let l = String.make 1 sp.sp_in.[0] in
                      let five = n >= 3 && sp.sp_in.[1] = '5' in
                      Hashtbl.find_opt by_belpin (l ^ (if five then "5FF" else "FF"), "Q")
                    else None in
                  let r345 () =
                    if sp.sp_bel = "SRUSEDMUX" && sp.sp_in = "IN" then Hashtbl.find_opt by_bel "SR"
                    else if sp.sp_bel = "CEUSEDMUX" && sp.sp_in = "IN" then Hashtbl.find_opt by_bel "CE"
                    else if sp.sp_bel = "CLKINV" then Hashtbl.find_opt by_bel "CLK"
                    else None in
                  (* R6 -- but NOT for the slice's output "used" markers.
                     AUSED/BUSED/CUSED/DUSED/COUTUSED carry in="0", yet that 0 is
                     a marker, not a constant tie: attributing them to the GND net
                     makes json2dcp route GLOBAL_LOGIC0 through the slice's output
                     path, and Vivado then refuses the real signal --
                     "Invalid configuration for site SLICE_X177Y17. Cannot replace
                     net GLOBAL_LOGIC0 with net ifg_reg[7]_i_5_n_0 on bel pin D/D,
                     ... on bel pin DOUTMUX/O6", 143 PDIL-1 errors.  They earn
                     nothing either way: neither Vivado's FASM nor ours contains a
                     single xUSED feature.  SRUSEDMUX/CEUSEDMUX are genuine ties
                     and keep the rule -- hence "USED" but not "USEDMUX". *)
                  let r6 () =
                    let out_used =
                      ends_with "USED" sp.sp_bel && not (ends_with "USEDMUX" sp.sp_bel) in
                    if out_used then None
                    else if sp.sp_in = "0" then Some "GLOBAL_LOGIC0"
                    else if sp.sp_in = "1" then Some "GLOBAL_LOGIC1" else None in
                  let r7 () =
                    let c = Hashtbl.fold
                              (fun (_, pin) n acc -> if pin = sp.sp_in then n :: acc else acc)
                              by_belpin [] in
                    match c with [ one ] -> Some one | _ -> None in
                  (* R8  <L>{FF,5FF,OUT}MUX fed by XOR / CY -- the CARRY4's own
                         O[i] / CO[i].  Those are CELL ports, so read them from
                         the placed CARRY4 rather than from the siteroutes,
                         which record them on Vivado's GLOBAL_USEDNET marker. *)
                  let carry_port port =
                    match slice_letter sp.sp_bel with
                    | None -> None
                    | Some l ->
                        let bit = Char.code l.[0] - Char.code 'A' in
                        (match Hashtbl.find_opt cell_at (st.s_name, "CARRY4") with
                         | None -> None
                         | Some cname ->
                             (match Hashtbl.find_opt conn_by_cell cname with
                              | None -> None
                              | Some conn ->
                                  (match List.assoc_opt port conn with
                                   | Some arr when bit < Array.length arr && arr.(bit) >= 0 ->
                                       Hashtbl.find_opt name_of_id arr.(bit)
                                   | _ -> None))) in
                  let r8 () =
                    if sp.sp_in = "XOR" then carry_port "O"
                    else if sp.sp_in = "CY" then carry_port "CO"
                    else None in
                  (* R9  <L>5FFMUX.IN_A takes the 5LUT's O5; .IN_B the bypass
                         site pin <L>X. *)
                  let r9 () =
                    match slice_letter sp.sp_bel with
                    | None -> None
                    | Some l ->
                        if sp.sp_in = "IN_A" then Hashtbl.find_opt by_belpin (l ^ "5LUT", "O5")
                        else if sp.sp_in = "IN_B" then Hashtbl.find_opt by_bel (l ^ "X")
                        else None in
                  (* R10 <L>CY0 selects the 5LUT's O5 as the carry DI. *)
                  let r10 () =
                    if sp.sp_in = "O5" && ends_with "CY0" sp.sp_bel then
                      match slice_letter sp.sp_bel with
                      | Some l -> Hashtbl.find_opt by_belpin (l ^ "5LUT", "O5")
                      | None -> None
                    else None in
                  (* R11 any *INV takes the site pin it inverts (generalises the
                         CLKINV case to BRAM/GT clock inverters). *)
                  let r11 () =
                    if ends_with "INV" sp.sp_bel then Hashtbl.find_opt by_bel sp.sp_in
                    else None in
                  (* R12 spelled-out constants, as well as "0"/"1". *)
                  let r12 () =
                    if sp.sp_in = "GND" then Some "GLOBAL_LOGIC0"
                    else if sp.sp_in = "VCC" then Some "GLOBAL_LOGIC1" else None in
                  (* R13 <L>OUTMUX fed by the wide muxes. *)
                  let r13 () =
                    match slice_letter sp.sp_bel, sp.sp_in with
                    | Some "A", "F7" -> Hashtbl.find_opt by_belpin ("F7AMUX", "OUT")
                    | Some "C", "F7" -> Hashtbl.find_opt by_belpin ("F7BMUX", "OUT")
                    | Some _, "F8"   -> Hashtbl.find_opt by_belpin ("F8MUX", "OUT")
                    | _ -> None in
                  let rec first = function
                    | [] -> None
                    | f :: rest -> (match f () with Some n -> Some n | None -> first rest) in
                  (* The output "used" markers -- AUSED..DUSED, COUTUSED -- gate
                     the slice's own outputs, so they belong to the net LEAVING on
                     that pin.  Their in="0" is a marker, not a tie: routing them
                     to GND (via the constant rule, and then again via R7 matching
                     Vivado's <siteroute net="GLOBAL_LOGIC0" srcbel="SRUSEDGND"
                     srcpin="0"> on the bare "0") produced 143 PDIL-1 errors.
                     Dropping them instead cured that but severed the output path:
                     1442 nets came back as ANTENNAS, their fabric routing
                     byte-identical to Vivado's but never reaching the bel.
                     Attribute them to the driven net and both hold.
                     SRUSEDMUX/CEUSEDMUX are genuine ties and keep their rules --
                     hence "USED" but not "USEDMUX". *)
                  (* "<PIN>USED" names the site pin it gates, so strip the
                     suffix and look the pin up: AUSED..DUSED and COUTUSED gate
                     slice OUTPUTS (source pins), while an IOB's OUSED gates the
                     data arriving on its "O" input (a sink pin) and IUSED the
                     "I" it drives back to the fabric.  Matching only the A-D
                     slice letters left OUSED unattributed, and without it the
                     IOB output path is never programmed: LED_OBUF[7:0],
                     UART_TX_OBUF and friends came back as partial antennas. *)
                  let net_at_pin pin =
                    match Hashtbl.find_opt src_at (st.s_name, pin) with
                    | Some x -> Some x
                    | None -> Hashtbl.find_opt snk_at (st.s_name, pin) in
                  let r_used () =
                    if ends_with "USEDMUX" sp.sp_bel then None
                    else if ends_with "USED" sp.sp_bel then begin
                      let n = String.length sp.sp_bel in
                      let pin = String.sub sp.sp_bel 0 (n - 4) in
                      (* Most <PIN>USED gates the pin it is named after, but the
                         distributed-RAM high address bits do not: a SLICEM takes
                         WA7 from the AX site pin and WA8 from CX (UG474).  Looked
                         up literally, neither exists, so both were skipped and
                         the RAM256X1S write-address never reached the LUTs --
                         "Unrouted Pins: ...u_ram__RAMS64E_A/WADR6". *)
                      let pin = match pin with
                        | "WA7" -> "AX" | "WA8" -> "CX" | p -> p in
                      net_at_pin pin
                    end else None in
                  (* The DRAM data-in cascade: CDI1MUX/BDI1MUX take the site's DI
                     pin and ADI1MUX takes B's DI1 output, all carrying the one
                     data net.  Unattributed, the RAMS64E "I" pins stay unrouted
                     (eth_gmii_rxd[7:0]). *)
                  let r_dram () =
                    if ends_with "DI1MUX" sp.sp_bel then begin
                      (* Two different sources feed a DI1MUX.  "DI" is the site's
                         data pin (distributed RAM).  "<X>MC31" is the SRL
                         CASCADE: lane X's shift-register output, which the
                         siteroutes record at <X>6LUT/MC31 -- CDI1MUX takes
                         DMC31, BDI1MUX takes CMC31, ADI1MUX takes BMC31.
                         Falling back to the DI net for those put the cascade on
                         the wrong net and Vivado reported partial conflicts on
                         the ..._srl32_n_1 nets. *)
                      let n = String.length sp.sp_in in
                      if n = 5 && String.sub sp.sp_in 1 4 = "MC31"
                         && sp.sp_in.[0] >= 'A' && sp.sp_in.[0] <= 'D' then
                        Hashtbl.find_opt by_belpin
                          (String.make 1 sp.sp_in.[0] ^ "6LUT", "MC31")
                      else
                        match net_at_pin sp.sp_in with
                        | Some x -> Some x
                        | None -> net_at_pin "DI"
                    end else None in
                  if ends_with "USED" sp.sp_bel && not (ends_with "USEDMUX" sp.sp_bel)
                  then r_used ()
                  else
                  first [ (fun () -> r1); r2; r345; r_dram; r6; r7; r8; r9; r10; r11; r12; r13 ] in
                match net with
                | None -> incr sitepips_skipped
                | Some raw ->
                    incr sitepips_added;
                    let key = json_net_name raw in
                    let entry =
                      Printf.sprintf "SITEWIRE/%s/%s;SITEPIP/%s/%s/%s;4"
                        st.s_name sp.sp_out st.s_name sp.sp_bel sp.sp_in in
                    let prev = match Hashtbl.find_opt routing_of key with
                      | Some p -> p ^ ";" | None -> "" in
                    Hashtbl.replace routing_of key (prev ^ entry))
             st.s_pips
         end)
      db.sites;

  let netnames =
    List.rev_map (fun (n, i) ->
        (n, `Assoc [ ("hide_name", `Int 1);
                     ("bits", `List [ `Int i ]);
                     (* EVERY net gets a ROUTING attribute when routing is on,
                        empty string included.  nextpnr's writer does the same,
                        and json2dcp reads it unguarded --
                        nn.attrs.get("ROUTING").split(";") at json2dcp.java:1205
                        NPEs on the first net that lacks one, which is any net
                        with no fabric pips (intra-site nets, unconnected pads). *)
                     ("attributes",
                      `Assoc (if not want_routing then []
                              else [ ("ROUTING",
                                      `String (match Hashtbl.find_opt routing_of n with
                                               | Some r -> r | None -> "")) ])) ]))
      !id_order in

  let json =
    `Assoc
      [ ("creator", `String "SVS opendcp_xml.write_nextpnr_json");
        ("modules",
         `Assoc
           [ (ed.module_name,
              `Assoc
                [ ("attributes",
                   `Assoc [ ("top", `String (String.make 31 '0' ^ "1")) ]);
                  ("ports", `Assoc ports);
                  ("cells", `Assoc (cells @ pad_cells));
                  ("netnames", `Assoc netnames) ]) ]) ] in

  let oc = open_out out_path in
  Yojson.Safe.pretty_to_channel oc json;
  output_char oc '\n';
  close_out oc;
  Printf.sprintf
    "wrote %s: %d cell(s) = %d logical + %d routethru LUT1 (%d placed, %d unplaced non-IO), %d macro(s) -> %d leaves, %d net(s), %d port(s), routing %s"
    out_path (List.length cells) (List.length resolved) (List.length !rt_cells)
    !placed (List.length !unplaced) !macros_expanded !leaves_made
    (List.length netnames) (List.length ports)
    (if want_routing
     then Printf.sprintf "on (%d net(s), %d pip(s), %d sitepip(s) [%d unattributed], %d macro-internal remapped)"
            !routed_nets !routed_pips !sitepips_added !sitepips_skipped !remapped
     else "off (set SVS_JSON_ROUTING=1)")

(* ------------------------------------------------- DEF export for OpenROAD *)

(* Vivado's layout, on the device plane, as LEF+DEF so OpenROAD can display it.

   WHY A TILEGRID.  FPGA site names carry PER-TYPE coordinates -- SLICE_X0..X219,
   RAMB18_X0..X13, IOB_X0..X1 -- so plotting each site at its own X/Y overlays
   the block RAM column on top of the logic.  prjxray's tilegrid gives every tile
   a single grid_x/grid_y on one plane, and lists the sites it contains, which is
   the only common coordinate system available.  One DEF component per OCCUPIED
   SITE (not per cell): a slice holds up to a dozen cells and a 1x1 site model
   cannot stack them, and site occupancy is what a layout view is for.

   The macro is chosen by what the site CONTAINS, so the picture is coloured by
   function -- carry chains, distributed RAM and shift registers stand out from
   plain logic, which is what makes a placement readable. *)

(* PIPS AS VIAS.  A pip is a programmable CONNECTION POINT between two wires,
   which is what a via is -- not a length of wire.  Modelling it that way makes
   the DEF structurally honest and, more usefully, makes via COUNTS a real cost
   term and the layer of each segment carry the wire's reach.

   The stack is ordered by how far one hop travels, taken from the measured
   distribution over this design's 146,302 pip endpoints:
     SITE   site pins, IMUX, LOGIC_OUTS, carry, constant ties   ~47%
     LOCAL  BYP / FAN / CTRL short hops                         ~14%
     SINGLE 1 tile                                              16.2%
     DOUBLE 2-3 tiles                                           11.6%
     QUAD   4-5 tiles                                            1.5%
     HEX    6-9 tiles                                            2.0%
     LONG   12+ tiles                                            0.1%
     CLK    clock spine                                          3.5%
   One cut layer (PIP) serves every pair: a via here is a programmable point,
   not a metal transition, so the usual adjacent-layers-only rule does not
   apply and a pair-per-via keeps the encoding flat instead of stacking. *)
let def_layers =
  [| "SITE"; "LOCAL"; "SINGLE"; "DOUBLE"; "QUAD"; "HEX"; "LONG"; "CLK" |]

let def_layer_of_class c =
  let has sub =
    let n = String.length c and m = String.length sub in
    let rec go i = i + m <= n && (String.sub c i m = sub || go (i + 1)) in
    m > 0 && go 0 in
  if has "CLK" then 7
  else if has "LONG" then 6
  else if has "HEX" || has "(7 tiles)" || has "(9 tiles)" then 5
  else if has "QUAD" || has "(5 tiles)" then 4
  else if has "DOUBLE" || has "(3 tiles)" then 3
  else if has "SINGLE" then 2
  else if has "BYP" || has "FAN" || has "CTRL" then 1
  else if has "IMUX" || has "site" || has "LOGIC_OUTS"
          || has "carry" || has "constant" then 0
  else 1

let def_macro_of (site_type : string) (cell_types : string list) =
  let has p = List.exists (fun t ->
      let lt = String.length t and lp = String.length p in
      lt >= lp && String.sub t 0 lp = p) cell_types in
  let pre p =
    String.length site_type >= String.length p
    && String.sub site_type 0 (String.length p) = p in
  if pre "SLICE" then
    (if has "CARRY" then "SLICE_CARRY"
     else if has "RAMS" || has "RAMD" then "SLICEM_DRAM"
     else if has "SRL" then "SLICEM_SRL"
     else if has "MUXF" then "SLICE_MUX"
     else if has "LUT" then "SLICE_LOGIC"
     else "SLICE_FF")
  else if pre "RAMB18" then "RAMB18"
  else if pre "RAMBFIFO36" || pre "RAMB36" then "RAMB36"
  else if pre "DSP" then "DSP48"
  else if pre "IOB" || pre "IPAD" || pre "OPAD" then "IOB"
  else if pre "BUFG" then "BUFG"
  else if pre "MMCM" || pre "PLL" then "MMCM"
  else if pre "GT" || pre "IBUFDS_GTE2" then "GT"
  else "SLICE_LOGIC"

(* ── BEL geometry ─────────────────────────────────────────────────────────
   A site drawn as one square says nothing about what was PACKED into it.  The
   database already carries c_bel, so each cell can be given its own box inside
   the site: the four SLICE rows A..D (A at the bottom, as Xilinx draws it) by
   five function columns -- 6LUT, 5LUT, carry/wide-mux, 5FF, FF.

   Boxes are fractions of the site.  CARRY4 and the F7/F8 muxes share the
   middle column: a slice can hold both, so they can overlap, which is
   preferable to giving either a column too thin to see. *)
let bel_box (site_type : string) (bel : string) (cell_type : string) =
  let pre s p =
    String.length s >= String.length p && String.sub s 0 (String.length p) = p in
  let suf s p =
    let ls = String.length s and lp = String.length p in
    ls >= lp && String.sub s (ls - lp) lp = p in
  let row_of c =                      (* A at the bottom *)
    match c with 'A' -> 0 | 'B' -> 1 | 'C' -> 2 | 'D' -> 3 | _ -> 0 in
  let rowy r = 0.03 +. float_of_int r *. 0.245 in
  let rh = 0.20 in
  if pre site_type "SLICE" && bel <> "" then begin
    let c = bel.[0] in
    let r = row_of c in
    if suf bel "6LUT" then
      (* a 6LUT holding a RAM/SRL cell is worth telling apart from logic *)
      ((if pre cell_type "RAMD" || pre cell_type "RAMS" then "DRAM"
        else if pre cell_type "SRL" then "SRL" else "LUT6"),
       0.03, rowy r, 0.30, rh)
    else if suf bel "5LUT" then
      ((if pre cell_type "RAMD" || pre cell_type "RAMS" then "DRAM"
        else if pre cell_type "SRL" then "SRL" else "LUT5"),
       0.35, rowy r, 0.15, rh)
    else if bel = "CARRY4" then ("CARRY4", 0.52, 0.03, 0.10, 0.94)
    else if pre bel "F7" then
      (* F7AMUX spans rows A-B, F7BMUX rows C-D *)
      ("MUXF7", 0.52, (if c = 'F' && String.length bel > 2 && bel.[2] = 'B'
                       then rowy 2 else rowy 0), 0.10, rh +. 0.245)
    else if pre bel "F8" then ("MUXF8", 0.52, 0.03, 0.10, 0.94)
    else if suf bel "5FF" then ("FF5", 0.64, rowy r, 0.15, rh)
    else if suf bel "FF" then ("FF", 0.81, rowy r, 0.16, rh)
    else ("SLICE_OTHER", 0.03, rowy r, 0.94, rh)
  end
  else begin
    (* Hard blocks: one box for the whole site, classed by what it holds. *)
    let cls =
      if pre site_type "RAMB18" then "RAMB18"
      else if pre site_type "RAMB36" || pre site_type "RAMBFIFO36" then "RAMB36"
      else if pre site_type "DSP" then "DSP48"
      else if pre site_type "IOB" || pre site_type "IPAD" || pre site_type "OPAD"
      then "IOB"
      else if pre site_type "BUFG" then "BUFG"
      else if pre site_type "MMCM" || pre site_type "PLL" then "MMCM"
      else if pre site_type "GT" || pre cell_type "IBUFDS_GTE2" then "GT"
      else "OTHER" in
    (cls, 0.05, 0.05, 0.90, 0.90)
  end

let write_def db tilegrid_path out_base =
  (* site -> (grid_x, grid_y) from the tilegrid *)
  let site_xy = Hashtbl.create 65536 and tile_xy = Hashtbl.create 65536 in
  (try
     let j = Yojson.Safe.from_file tilegrid_path in
     (match j with
      | `Assoc tiles ->
          List.iter
            (fun (_tile, info) ->
               match info with
               | `Assoc kv ->
                   let geti k = match List.assoc_opt k kv with
                     | Some (`Int i) -> Some i | _ -> None in
                   (match geti "grid_x", geti "grid_y" with
                    | Some gx, Some gy ->
                        Hashtbl.replace tile_xy _tile (gx, gy);
                        (match List.assoc_opt "sites" kv with
                         | Some (`Assoc sites) ->
                             List.iter (fun (sname, _) -> Hashtbl.replace site_xy sname (gx, gy)) sites
                         | _ -> ())
                    | _ -> ())
               | _ -> ())
            tiles
      | _ -> ())
   with e ->
     Printf.eprintf "[opendcp] DEF: cannot read tilegrid %s: %s\n"
       tilegrid_path (Printexc.to_string e));

  (* occupied sites, with the cell types in each *)
  let by_site = Hashtbl.create 4096 in
  List.iter
    (fun c ->
       if not c.c_rt && c.c_site <> "" then begin
         let prev = try Hashtbl.find by_site c.c_site with Not_found -> [] in
         Hashtbl.replace by_site c.c_site (c.c_type :: prev)
       end)
    db.cells;
  let site_type = Hashtbl.create 4096 in
  List.iter (fun st -> Hashtbl.replace site_type st.s_name st.s_type) db.sites;

  let comps = ref [] and placed = ref 0 and no_xy = ref 0 in
  let maxx = ref 0 and maxy = ref 0 in
  Hashtbl.iter
    (fun site types ->
       match Hashtbl.find_opt site_xy site with
       | None -> incr no_xy
       | Some (gx, gy) ->
           let sty = match Hashtbl.find_opt site_type site with Some t -> t | None -> "" in
           incr placed;
           if gx > !maxx then maxx := gx;
           if gy > !maxy then maxy := gy;
           comps := (site, def_macro_of sty types, gx, gy) :: !comps)
    by_site;

  (* site -> the macro we placed there, so a net's site pins can be turned into
     DEF terminals on a component that actually exists. *)
  let macro_at = Hashtbl.create 4096 in
  List.iter (fun (site, m, _, _) -> Hashtbl.replace macro_at site m) !comps;

  (* CONNECTIVITY.  Geometry alone gives a picture; without terminals OpenROAD
     reports "0 connections" and can compute neither HPWL nor a timing graph, so
     nothing can be calibrated against this layout.  The net's site pins are
     already in the database and were verified against Vivado (26,023 of them),
     so use exactly those: pin names per macro CLASS, collected from the sites of
     that class, so the DEF can never reference a pin the LEF lacks.

     A physical pin carries ONE net.  Our components are SITES, so two nets could
     in principle claim one (site, pin); the first wins and the clash is counted
     rather than silently dropped. *)
  let macro_pins = Hashtbl.create 256 in          (* (macro, pin) -> is_out *)
  let pin_owner = Hashtbl.create 65536 in         (* (site, pin) -> net name *)
  let conflicts = ref 0 and orphan_pins = ref 0 in
  let note_pin site pin is_out netname =
    match Hashtbl.find_opt macro_at site with
    | None -> incr orphan_pins; false
    | Some m ->
        (match Hashtbl.find_opt pin_owner (site, pin) with
         | Some owner when owner <> netname -> incr conflicts; false
         | Some _ -> false                        (* already emitted for this net *)
         | None ->
             Hashtbl.replace pin_owner (site, pin) netname;
             let prev = try Hashtbl.find macro_pins (m, pin) with Not_found -> false in
             Hashtbl.replace macro_pins (m, pin) (prev || is_out);
             true) in
  let net_terms = Hashtbl.create 8192 in
  List.iter
    (fun n ->
       let nm = json_const_rename n.n_name in
       let acc = ref [] in
       List.iter (fun (site, pin) ->
           if note_pin site pin true nm then acc := (site, pin) :: !acc) n.n_srcpins;
       List.iter (fun (site, pin) ->
           if note_pin site pin false nm then acc := (site, pin) :: !acc) n.n_snkpins;
       if !acc <> [] then
         Hashtbl.replace net_terms nm
           (List.rev_append !acc (try Hashtbl.find net_terms nm with Not_found -> [])))
    db.nets;

  let u = 2000 in
  (* Y AXIS.  prjxray's grid_y counts DOWNWARD from the top of the device, while
     DEF and GDS put Y increasing upward -- so emitting grid_y directly draws the
     die upside down against the way Vivado shows it.  Flip once, here, so every
     consumer (OpenROAD, KLayout) agrees with the vendor view.  HPWL is invariant
     under the mirror, so measurements taken before this are unaffected. *)
  let ydef gy = (!maxy - gy) * u in
  (* self-contained LEF: one 1x1 macro per class we actually place, so the DEF
     cannot disagree with the library it is read against. *)
  let macros =
    List.sort_uniq compare (List.map (fun (_, m, _, _) -> m) !comps) in
  let lef = out_base ^ ".lef" in
  let oc = open_out lef in
  Printf.fprintf oc "VERSION 5.8 ;\nBUSBITCHARS \"[]\" ;\nDIVIDERCHAR \"/\" ;\n";
  Printf.fprintf oc "UNITS\n  DATABASE MICRONS %d ;\nEND UNITS\n\n" u;
  (* The stack must be declared INTERLEAVED -- routing, cut, routing, cut -- and
     a via may only join layers separated by one cut.  OpenROAD works out a
     via's exit layer from that ordering, and a via between non-adjacent layers
     leaves it unable to ("invalid VIA layers ... cannot determine exit layer of
     path"), which rejects the whole DEF. *)
  Array.iteri
    (fun i l ->
       Printf.fprintf oc
         "LAYER %s\n  TYPE ROUTING ;\n  DIRECTION %s ;\n  PITCH 0.200 ;\n  WIDTH 0.100 ;\nEND %s\n\n"
         l (if i land 1 = 0 then "HORIZONTAL" else "VERTICAL") l;
       if i + 1 < Array.length def_layers then
         Printf.fprintf oc "LAYER PIP%d\n  TYPE CUT ;\nEND PIP%d\n\n" i i)
    def_layers;
  Array.iteri
    (fun i a ->
       if i + 1 < Array.length def_layers then begin
         let b = def_layers.(i + 1) in
         Printf.fprintf oc "VIA PIP_%s_%s DEFAULT\n" a b;
         Printf.fprintf oc "  LAYER %s ;\n    RECT -0.050 -0.050 0.050 0.050 ;\n" a;
         Printf.fprintf oc "  LAYER PIP%d ;\n    RECT -0.025 -0.025 0.025 0.025 ;\n" i;
         Printf.fprintf oc "  LAYER %s ;\n    RECT -0.050 -0.050 0.050 0.050 ;\n" b;
         Printf.fprintf oc "END PIP_%s_%s\n\n" a b
       end)
    def_layers;
  Printf.fprintf oc "SITE FPGA\n  CLASS CORE ;\n  SIZE 1.000 BY 1.000 ;\nEND FPGA\n\n";
  List.iter
    (fun m ->
       Printf.fprintf oc "MACRO %s\n  CLASS CORE ;\n  ORIGIN 0 0 ;\n  SIZE 1.000 BY 1.000 ;\n  SITE FPGA ;\n" m;
       (* every pin at the site centre: at site granularity the position within
          the 1x1 cell is meaningless, and centring makes HPWL exactly the
          site-to-site distance, which is the quantity of interest. *)
       Hashtbl.iter
         (fun (mm, pin) is_out ->
            if mm = m then begin
              Printf.fprintf oc "  PIN %s\n    DIRECTION %s ;\n    USE SIGNAL ;\n    PORT\n"
                pin (if is_out then "OUTPUT" else "INPUT");
              Printf.fprintf oc "      LAYER SITE ;\n        RECT 0.450 0.450 0.550 0.550 ;\n";
              Printf.fprintf oc "    END\n  END %s\n" pin
            end)
         macro_pins;
       Printf.fprintf oc "END %s\n\n" m)
    macros;
  Printf.fprintf oc "END LIBRARY\n";
  close_out oc;

  let def = out_base ^ ".def" in
  let oc = open_out def in
  Printf.fprintf oc "VERSION 5.8 ;\nDIVIDERCHAR \"/\" ;\nBUSBITCHARS \"[]\" ;\n";
  Printf.fprintf oc "DESIGN %s ;\nUNITS DISTANCE MICRONS %d ;\n" db.top u;
  Printf.fprintf oc "DIEAREA ( 0 0 ) ( %d %d ) ;\n" ((!maxx + 2) * u) ((!maxy + 2) * u);
  (* A DEF ROW is ONE-dimensional -- "DO n BY 1" -- so the site array is emitted
     as one row per y, not as a 2-D array (which the parser rejects outright:
     "Invalid syntax ... The valid syntax is either DO 1 BY num or DO num BY 1"). *)
  for y = 0 to !maxy + 1 do
    Printf.fprintf oc "ROW ROW_%d FPGA 0 %d N DO %d BY 1 STEP %d 0 ;\n"
      y (y * u) (!maxx + 2) u
  done;
  Printf.fprintf oc "COMPONENTS %d ;\n" (List.length !comps);
  List.iter
    (fun (site, macro, gx, gy) ->
       (* FIXED by default: this is Vivado's placement being displayed, not a
          seed.  SVS_DEF_UNFIX=<comma-separated macro prefixes> releases those
          classes as PLACED so a placer may move them -- e.g. SVS_DEF_UNFIX=SLICE
          hands the logic to OpenROAD while the hard resources (IO, GT, BRAM,
          DSP, clocking), which are pinned by the package and the column
          structure, stay where Vivado put them. *)
       let unfix =
         match Sys.getenv_opt "SVS_DEF_UNFIX" with
         | None -> false
         | Some spec ->
             List.exists
               (fun pre ->
                  pre <> "" && String.length macro >= String.length pre
                  && String.sub macro 0 (String.length pre) = pre)
               (String.split_on_char ',' spec) in
       Printf.fprintf oc "  - %s %s + %s ( %d %d ) N ;\n" site macro
         (if unfix then "PLACED" else "FIXED") (gx * u) (ydef gy))
    !comps;
  Printf.fprintf oc "END COMPONENTS\n";

  (* ROUTING.  Each pip is a hop between two wires; drawn on the device plane it
     is a step from one TILE to another, so a net's route becomes a polyline
     through the tiles its pips occupy.  DEF segments must be orthogonal, so a
     diagonal hop is emitted as an L (two segments).  This is a picture of where
     the routing physically goes -- not a routing database OpenROAD can re-time,
     which would need the real wire geometry. *)
  let routed = ref 0 and seg = ref 0 and vias = ref 0 and unknown_tile = ref 0 in
  let stub_on = ref 0 and stub_off = ref 0 in
  (* Built as a function because the routing is emitted TWICE: once into the
     site-level DEF (with terminals, so OpenROAD has a connectivity graph) and
     once into the BEL-level DEF below, whose components are cells -- there the
     site terminals would name components that do not exist in that file. *)
  let build_nets ~with_terms ~stubs =
    routed := 0; seg := 0; vias := 0;
    let buf = Buffer.create (1 lsl 22) in
  List.iter
    (fun n ->
       if n.n_pips <> [] then begin
         (* per pip: the tile it sits in, and the layers it joins *)
         let hops = List.filter_map
             (fun (tile, src, dst, rev) ->
                let src, dst = if rev then dst, src else src, dst in
                match Hashtbl.find_opt tile_xy tile with
                | Some (x, y) ->
                    Some (x * u + u / 2, ydef y + u / 2,
                          def_layer_of_class (wire_class src),
                          def_layer_of_class (wire_class dst))
                | None -> incr unknown_tile; None)
             n.n_pips in
         match hops with
         | [] -> ()
         | _ ->
             incr routed;
             let nm = json_const_rename n.n_name in
             Buffer.add_string buf (Printf.sprintf "  - %s" nm);
             (match (if with_terms then Hashtbl.find_opt net_terms nm else None) with
              | None -> ()
              | Some ts ->
                  List.iter (fun (site, pin) ->
                      Buffer.add_string buf (Printf.sprintf " ( %s %s )" site pin)) ts);
             Buffer.add_char buf '\n';
             let first = ref true in
             let emit_head lay =
               let kw = if !first then "    + ROUTED" else "    NEW" in
               first := false; Printf.sprintf "%s %s" kw def_layers.(lay) in
             let prev = ref None in
             List.iter
               (fun (x, y, ls, ld) ->
                  (* the WIRE the signal rode to get here, on its own layer *)
                  (match !prev with
                   | Some (px, py, pl) when px <> x || py <> y ->
                       if px <> x then begin
                         Buffer.add_string buf
                           (Printf.sprintf "%s ( %d %d ) ( %d %d )\n" (emit_head pl) px py x py);
                         incr seg
                       end;
                       if py <> y then begin
                         Buffer.add_string buf
                           (Printf.sprintf "%s ( %d %d ) ( %d %d )\n" (emit_head pl) x py x y);
                         incr seg
                       end
                   | _ -> ());
                  (* the PIP itself: a via between the two wire classes *)
                  if ls <> ld then begin
                    (* One pip, but the DEF encoding walks the stack: a via per
                       cut between the two classes, the rest at "( * * )" -- the
                       standard spelling for stacked vias at one point. *)
                    let lo = min ls ld and hi = max ls ld in
                    let b = Buffer.create 64 in
                    Buffer.add_string b (Printf.sprintf "%s ( %d %d )" (emit_head ls) x y);
                    let step = if ld > ls then 1 else -1 in
                    let k = ref ls in
                    while !k <> ld do
                      let a = min !k (!k + step) in
                      Buffer.add_string b
                        (Printf.sprintf " PIP_%s_%s" def_layers.(a) def_layers.(a + 1));
                      incr vias;
                      k := !k + step;
                      if !k <> ld then Buffer.add_string b " ( * * )"
                    done;
                    ignore lo; ignore hi;
                    Buffer.add_string b "\n";
                    Buffer.add_buffer buf b
                  end;
                  prev := Some (x, y, ld))
               hops;
             (* pin stubs, so the route actually lands on the pins it serves.
                A stub aims at its OWN site's tile centre, but pips sit in INT
                tiles while a slice belongs to the CLB tile beside it -- so
                check, rather than assume, that the point a stub targets is one
                the route really visits, and if it is not, redirect it to the
                nearest point that is. *)
             let hop_pts = List.map (fun (x, y, _, _) -> (x, y)) hops in
             List.iter
               (fun (px, py, cx0, cy0) ->
                  let on = List.exists (fun (x, y) -> x = cx0 && y = cy0) hop_pts in
                  if on then incr stub_on else incr stub_off;
                  let (cx, cy) =
                    if on then (cx0, cy0)
                    else
                      (* nearest hop point, so the stub meets the polyline *)
                      List.fold_left
                        (fun (bx, by) (x, y) ->
                           let d (a, b) = abs (a - px) + abs (b - py) in
                           if d (x, y) < d (bx, by) then (x, y) else (bx, by))
                        (cx0, cy0) hop_pts in
                  if px <> cx then begin
                    Buffer.add_string buf
                      (Printf.sprintf "%s ( %d %d ) ( %d %d )\n"
                         (emit_head 0) px py cx py);
                    incr seg
                  end;
                  if py <> cy then begin
                    Buffer.add_string buf
                      (Printf.sprintf "%s ( %d %d ) ( %d %d )\n"
                         (emit_head 0) cx py cx cy);
                    incr seg
                  end)
               (stubs nm);
             Buffer.add_string buf "    ;\n"
       end)
    db.nets;
    buf in
  (* The site-level file needs no stubs: its pins are at the site centre, which
     is where the route already runs. *)
  let buf = build_nets ~with_terms:true ~stubs:(fun _ -> []) in
  Printf.fprintf oc "NETS %d ;\n" !routed;
  Buffer.output_buffer oc buf;
  Printf.fprintf oc "END NETS\nEND DESIGN\n";
  close_out oc;
  (* Snapshot the counters: build_nets runs a SECOND time below and resets them,
     which would otherwise put the BEL file's totals in this file's report. *)
  let s_routed = !routed and s_seg = !seg and s_vias = !vias in

  (* ── BEL-resolution pair, for LOOKING at ──────────────────────────────────
     The site-level pair above is what OpenROAD places and what HPWL is
     measured on, so it is left exactly as it was.  This second pair puts one
     component per CELL at its BEL box, with pins, so the packing inside a
     slice is visible instead of a uniform square. *)
  let bel_base = out_base ^ "_bels" in
  let bcomps = ref [] and bplaced = ref 0 and bno_xy = ref 0 in
  (* Pin DIRECTION comes from the connectivity, not from a table of pin names:
     a pin the database lists as a net's source is an output, one it lists as a
     sink is an input -- exactly the rule the site-level LEF above uses.  A BEL
     pin the nets never mention keeps direction UNKNOWN and is drawn on the
     bottom edge rather than guessed onto one side. *)
  let spdir = Hashtbl.create 65536 in           (* (site, pin) -> is_out *)
  List.iter
    (fun n ->
       List.iter (fun sp -> Hashtbl.replace spdir sp true)  n.n_srcpins;
       List.iter (fun sp -> Hashtbl.replace spdir sp false) n.n_snkpins)
    db.nets;
  (* That name match only ever fires where a BEL pin happens to be spelled like
     a site pin, which is true of LUT inputs and almost nothing else -- a LUT's
     output is O6 on the BEL and A/AMUX on the site, so every output fell
     through.  The real join is the SITEROUTE: it names the (bel, pin) each net
     touches INSIDE a site.  Combined with where the net enters and leaves --
     source site pin here means this BEL drives it, sink site pin means it is
     being read -- that gives direction from the netlist proper. *)
  let bpdir = Hashtbl.create 65536 in           (* (site, bel, pin) -> is_out *)
  List.iter
    (fun st ->
       List.iter
         (fun sr ->
            Hashtbl.replace bpdir (st.s_name, sr.sr_bel, sr.sr_pin) true;
            List.iter
              (fun (b, p) ->
                 if not (Hashtbl.mem bpdir (st.s_name, b, p)) then
                   Hashtbl.replace bpdir (st.s_name, b, p) false)
              sr.sr_snks)
         st.s_routes)
    db.sites;
  let bmacro_pins = Hashtbl.create 256 in       (* (macro, pin) -> dir option *)
  let cell_box = Hashtbl.create 8192 in         (* (site, bel) -> cls, x, y *)
  let unknown_dir = ref 0 in
  List.iter
    (fun c ->
       if not c.c_rt && c.c_site <> "" then
         match Hashtbl.find_opt site_xy c.c_site with
         | None -> incr bno_xy
         | Some (gx, gy) ->
           let sty = match Hashtbl.find_opt site_type c.c_site with
             | Some t -> t | None -> "" in
           let (cls, fx, fy, fw, fh) = bel_box sty c.c_bel c.c_type in
           let uf = float_of_int u in
           let x = gx * u + int_of_float (fx *. uf) in
           let y = ydef gy + int_of_float (fy *. uf) in
           incr bplaced;
           Hashtbl.replace cell_box (c.c_site, c.c_bel) (cls, x, y);
           List.iter
             (fun (bp, _) ->
                let d =
                  match Hashtbl.find_opt bpdir (c.c_site, c.c_bel, bp) with
                  | Some b -> Some b
                  | None -> Hashtbl.find_opt spdir (c.c_site, bp) in
                if d = None then incr unknown_dir;
                let merged =
                  match Hashtbl.find_opt bmacro_pins (cls, bp), d with
                  | Some (Some true), _ | _, Some true -> Some true
                  | Some (Some false), _ | _, Some false -> Some false
                  | _ -> None in
                Hashtbl.replace bmacro_pins (cls, bp) merged)
             c.c_pins;
           bcomps := (c.c_name, cls, x, y, fw, fh) :: !bcomps)
    db.cells;
  (* One macro per class.  A class always gets the SAME box size, so the LEF a
     component is read against matches the geometry it was placed with. *)
  let bsize = Hashtbl.create 32 in
  List.iter (fun (_, cls, _, _, fw, fh) -> Hashtbl.replace bsize cls (fw, fh)) !bcomps;
  let oc = open_out (bel_base ^ ".lef") in
  Printf.fprintf oc "VERSION 5.8 ;\nBUSBITCHARS \"[]\" ;\nDIVIDERCHAR \"/\" ;\n";
  Printf.fprintf oc "UNITS\n  DATABASE MICRONS %d ;\nEND UNITS\n\n" u;
  Printf.fprintf oc "LAYER SITE\n  TYPE ROUTING ;\n  DIRECTION HORIZONTAL ;\n\
                     \  PITCH 0.200 ;\n  WIDTH 0.100 ;\nEND SITE\n\n";
  Printf.fprintf oc "SITE FPGA\n  CLASS CORE ;\n  SIZE 1.000 BY 1.000 ;\nEND FPGA\n\n";
  (* Pin geometry is computed ONCE, into a table, and then used both to write
     the LEF and to stitch the routing to the pins below.  Computing it twice
     is how a viewer ends up drawing wires that stop just short of the pin they
     are supposed to land on. *)
  let pin_geom = Hashtbl.create 2048 in   (* (cls, pin) -> dir, x1,y1,x2,y2 μm *)
  Hashtbl.iter
    (fun cls (fw, fh) ->
       (* Inputs down the left edge, outputs down the right, so a cell reads
          like a schematic symbol at zoom.  Unknown direction goes on the
          bottom edge: the file should say what the data says, not a guess. *)
       let ins = ref [] and outs = ref [] and unk = ref [] in
       Hashtbl.iter
         (fun (m, pin) d ->
            if m = cls then
              match d with
              | Some true  -> outs := pin :: !outs
              | Some false -> ins  := pin :: !ins
              | None       -> unk  := pin :: !unk)
         bmacro_pins;
       let place lst x_frac dir =
         let n = List.length lst in
         List.iteri
           (fun i pin ->
              let step = fh /. float_of_int (max 1 n) in
              let y = step *. (float_of_int i +. 0.5) in
              let ph = min 0.020 (step *. 0.5) and pw = fw *. 0.10 in
              Hashtbl.replace pin_geom (cls, pin)
                (dir, fw *. x_frac -. pw /. 2.0, y -. ph /. 2.0,
                      fw *. x_frac +. pw /. 2.0, y +. ph /. 2.0))
           (List.sort compare lst) in
       place !ins 0.04 "INPUT";
       place !outs 0.96 "OUTPUT";
       place !unk 0.50 "INOUT")
    bsize;
  let bpins = ref 0 in
  Hashtbl.iter
    (fun cls (fw, fh) ->
       Printf.fprintf oc
         "MACRO %s\n  CLASS CORE ;\n  ORIGIN 0 0 ;\n  SIZE %.3f BY %.3f ;\n  SITE FPGA ;\n"
         cls fw fh;
       Hashtbl.iter
         (fun (c, pin) (dir, x1, y1, x2, y2) ->
            if c = cls then begin
              incr bpins;
              Printf.fprintf oc
                "  PIN %s\n    DIRECTION %s ;\n    USE SIGNAL ;\n    PORT\n\
                 \      LAYER SITE ;\n        RECT %.4f %.4f %.4f %.4f ;\n\
                 \    END\n  END %s\n"
                pin dir x1 y1 x2 y2 pin
            end)
         pin_geom;
       Printf.fprintf oc "END %s\n\n" cls)
    bsize;
  Printf.fprintf oc "END LIBRARY\n";
  close_out oc;

  (* STITCHING.  The route polyline is drawn through TILE CENTRES, because that
     is the resolution a pip has; the pins sit at sub-site offsets.  Nothing
     joins the two, so the wires stop short of every pin they serve.  The
     siteroutes say exactly which (bel, pin) each net touches inside a site, so
     each of those pins gets a stub to its own tile centre, where the route
     already passes.  Emitted as an L because DEF segments are orthogonal. *)
  let net_stubs = Hashtbl.create 8192 in
  let seen_stub = Hashtbl.create 65536 in
  let stub_pins = ref 0 in
  List.iter
    (fun st ->
       match Hashtbl.find_opt site_xy st.s_name with
       | None -> ()
       | Some (gx, gy) ->
         let cx = gx * u + u / 2 and cy = ydef gy + u / 2 in
         List.iter
           (fun sr ->
              let add bel pin =
                match Hashtbl.find_opt cell_box (st.s_name, bel) with
                | None -> ()                      (* a routing bel, not a cell *)
                | Some (cls, bx, by) ->
                  match Hashtbl.find_opt pin_geom (cls, pin) with
                  | None -> ()
                  | Some (_, x1, y1, x2, y2) ->
                    let uf = float_of_int u in
                    let px = bx + int_of_float ((x1 +. x2) /. 2.0 *. uf)
                    and py = by + int_of_float ((y1 +. y2) /. 2.0 *. uf) in
                    let nm = json_const_rename sr.sr_net in
                    (* A pin is named by every siteroute that touches it, so the
                       same stub arrives many times over; keep one per
                       (net, point) or the geometry multiplies. *)
                    if not (Hashtbl.mem seen_stub (nm, px, py)) then begin
                      Hashtbl.replace seen_stub (nm, px, py) ();
                      incr stub_pins;
                      Hashtbl.replace net_stubs nm
                        ((px, py, cx, cy)
                         :: (try Hashtbl.find net_stubs nm with Not_found -> []))
                    end in
              add sr.sr_bel sr.sr_pin;
              List.iter (fun (b, p) -> add b p) sr.sr_snks)
           st.s_routes)
    db.sites;

  let oc = open_out (bel_base ^ ".def") in
  Printf.fprintf oc "VERSION 5.8 ;\nDIVIDERCHAR \"/\" ;\nBUSBITCHARS \"[]\" ;\n";
  Printf.fprintf oc "DESIGN %s ;\nUNITS DISTANCE MICRONS %d ;\n" db.top u;
  Printf.fprintf oc "DIEAREA ( 0 0 ) ( %d %d ) ;\n" ((!maxx + 2) * u) ((!maxy + 2) * u);
  for y = 0 to !maxy + 1 do
    Printf.fprintf oc "ROW ROW_%d FPGA 0 %d N DO %d BY 1 STEP %d 0 ;\n"
      y (y * u) (!maxx + 2) u
  done;
  Printf.fprintf oc "COMPONENTS %d ;\n" (List.length !bcomps);
  List.iter
    (fun (name, cls, x, y, _, _) ->
       Printf.fprintf oc "  - %s %s + FIXED ( %d %d ) N ;\n" name cls x y)
    !bcomps;
  Printf.fprintf oc "END COMPONENTS\n";
  let bbuf =
    build_nets ~with_terms:false
      ~stubs:(fun nm -> match Hashtbl.find_opt net_stubs nm with
                        | Some l -> l | None -> []) in
  Printf.fprintf oc "NETS %d ;\n" !routed;
  Buffer.output_buffer oc bbuf;
  Printf.fprintf oc "END NETS\nEND DESIGN\n";
  close_out oc;
  Printf.eprintf
    "[opendcp] DEF: %s.def + .lef: %d cell(s) at BEL resolution, %d class(es), \
     %d macro pin(s), %d cell(s) with no tilegrid entry, %d BEL pin(s) with no \
     direction in the netlist, %d pin(s) stitched to the routing as %d \
     segment(s) [%d already on a route vertex, %d redirected to the nearest \
     one]\n%!"
    bel_base !bplaced (Hashtbl.length bsize) !bpins !bno_xy !unknown_dir
    !stub_pins !seg !stub_on !stub_off;

  Printf.sprintf
    "wrote %s + %s: %d occupied site(s), %d with no tilegrid entry, grid %dx%d, %d macro class(es), %d routed net(s) as %d segment(s)%s"
    lef def !placed !no_xy (!maxx + 1) (!maxy + 1) (List.length macros) s_routed s_seg
    (Printf.sprintf ", %d via(s), %d terminal(s) on %d macro pin(s)%s%s" s_vias
       (Hashtbl.length pin_owner) (Hashtbl.length macro_pins)
       (if !conflicts = 0 then "" else Printf.sprintf " [%d pin conflict(s)]" !conflicts)
       (if !orphan_pins = 0 then "" else Printf.sprintf " [%d pin(s) on unplaced sites]" !orphan_pins))
