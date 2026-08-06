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
}

type sitepip = { sp_bel : string; sp_in : string; sp_out : string }

type siteroute = { sr_net : string; sr_bel : string; sr_pin : string }

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
  n_pips    : (string * string * string) list;  (* tile, src, dst *)
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
    c_props = List.rev !props }

let parse_net el =
  let pips = ref [] and src = ref [] and snk = ref [] in
  Xml.iter
    (fun ch ->
       match String.lowercase_ascii (Xml.tag ch) with
       | "pip" ->
           pips := (attr_or "" "tile" ch, attr_or "" "src" ch,
                    attr_or "" "dst" ch) :: !pips
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
           routes := { sr_net = attr_or "" "net" ch;
                       sr_bel = attr_or "" "srcbel" ch;
                       sr_pin = attr_or "" "srcpin" ch } :: !routes
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
