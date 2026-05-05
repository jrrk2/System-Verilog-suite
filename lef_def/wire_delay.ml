(* First-order wire-delay estimate from HPWL.

   We use a single (R,C) per dbu to convert HPWL into picoseconds.
   For Nangate45 in the OpenROAD reference flow a reasonable
   ballpark is R = 0.3 Ω/dbu, C = 0.0002 fF/dbu (since 1 dbu =
   1/2000 µm and metal2 has ~0.6 fF/µm), giving an Elmore RC/2
   around 3e-5 ps/dbu² — i.e. a 100k-dbu net contributes ~0.3 ns.

   These numbers are a stand-in; user code should override
   [params] when better extraction data is available. *)

type params = {
  r_per_dbu : float;  (* Ω per dbu of routing length *)
  c_per_dbu : float;  (* F per dbu of routing length, in absolute units *)
}

(* Defaults — order-of-magnitude only. *)
let default_params = {
  r_per_dbu = 3e-4;     (* 0.3 mΩ/dbu *)
  c_per_dbu = 2e-19;    (* 0.2 aF/dbu — i.e. ~0.4 fF/µm at 2000 dbu/µm *)
}

(* Lumped Elmore RC for an HPWL of [hpwl] dbu, in picoseconds.
   Treats the wire as a single π-section: τ = R*C/2 where R,C are
   the totals over the whole HPWL. *)
let elmore_ps ?(p=default_params) hpwl =
  let l = float_of_int hpwl in
  let r = p.r_per_dbu *. l in
  let c = p.c_per_dbu *. l in
  let tau_seconds = r *. c /. 2. in
  tau_seconds *. 1e12

(* Per-net wire delay: [hpwl_of_net] then [elmore_ps]. *)
let net_delay_ps ?p plc_tbl net =
  let h = Hpwl.hpwl_of_net plc_tbl net in
  elmore_ps ?p h, h
