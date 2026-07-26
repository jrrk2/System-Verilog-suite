(* Recognition packer: map a BIR structural Xilinx-primitive netlist onto the
   virtex7 LEF cell configs (xilinx_lef/virtex7_cells.lef), for topographical
   ASIC-style placement with nextpnr used only for legalisation.

   This is the OCaml port of xilinx_lef/pack_to_lef.py, operating natively on
   Behavioral_ir so it can be called inside SVS.  The recognition it performs
   is the semantic structure nextpnr's soup-of-cells placer discards:

     - a CARRY4 + its S-driving LUTs + its sum FFs is ONE carry slice that
       CHAINS vertically (CI-bottom / CO-top pins); the counter feedback
       cnt[i] -> S[i] is absorbed INSIDE the packed cell, so the placer never
       has to route it and the legaliser keeps it in-slice.
     - MMCM / BUFG / IO map to dedicated site cells.
     - leftover LUT / FF fall back to SLICE_LOGIC / SLICE_FF.

   Net identity follows bir_to_nextpnr_json's convention: a bexpr resolves to
   a per-bit list of [netkey]; two pins connect iff they share a [Net]. *)

open Behavioral_ir

type netkey = Const of bool | Net of string * int

let string_of_netkey = function
  | Const b -> if b then "VCC" else "GND"
  | Net (b, i) -> Printf.sprintf "%s[%d]" b i

(* ---- widths from module signals ------------------------------------------ *)
let rec width_of_btype = function
  | BInt { width; _ } -> width
  | BBool -> 1
  | BArray { element; size } -> size * width_of_btype element
  | BStruct { fields } ->
      List.fold_left (fun a (_, t) -> a + width_of_btype t) 0 fields

let widths_of_module (m : bmodule) : (string, int) Hashtbl.t =
  let h = Hashtbl.create 256 in
  List.iter (fun (s : bsignal) ->
      Hashtbl.replace h s.name (width_of_btype s.stype)) m.signals;
  h

let const_of_name = function
  | "<const0>" | "GND" | "$false" -> Some false
  | "<const1>" | "VCC" | "$true" -> Some true
  | _ -> None

(* Resolve a port connection's bexpr to its LSB-first list of net bits,
   mirroring bir_to_nextpnr_json.bits_of_conn. *)
let rec net_bits widths (e : bexpr) : netkey list =
  match e with
  | BConst { value; width } ->
      List.init width (fun i -> Const (Z.testbit value i))
  | BConcat es -> List.concat_map (net_bits widths) (List.rev es)
  | BSlice { signal = BVar base; msb; lsb } ->
      List.init (msb - lsb + 1) (fun k ->
          let i = lsb + k in
          match const_of_name base with Some b -> Const b | None -> Net (base, i))
  | BVar nm -> (
      match const_of_name nm with
      | Some b -> [ Const b ]
      | None ->
          let w = try Hashtbl.find widths nm with Not_found -> 1 in
          List.init w (fun i -> Net (nm, i)))
  | BSelect { array = BVar nm; index = BConst { value; _ } } -> (
      match const_of_name nm with Some b -> [ Const b ] | None -> [ Net (nm, Z.to_int value) ])
  | _ -> []

(* ---- output-pin recognition per Xilinx primitive ------------------------- *)
(* [starts_with s pre] : does string [s] begin with prefix [pre]?  (value
   first, prefix second -- matches every call site below.) *)
let starts_with s pre =
  String.length s >= String.length pre && String.sub s 0 (String.length pre) = pre

let is_output mn port =
  let m = String.uppercase_ascii mn and p = String.uppercase_ascii port in
  if starts_with m "CARRY4" then p = "O" || p = "CO"
  else if starts_with m "FD" then p = "Q"
  else if starts_with m "LUT" then p = "O"
  else if starts_with m "RAMB" then starts_with p "DO" || starts_with p "CASCADEOUT"
  else if starts_with m "DSP" then p = "P" || p = "PCOUT" || p = "CARRYOUT"
  else if m = "MMCME2_ADV" || m = "PLLE2_ADV" then
    starts_with p "CLKOUT" || p = "CLKFBOUT" || p = "LOCKED"
  else if starts_with m "BUFG" || starts_with m "BUFH" then p = "O"
  else if starts_with m "IBUF" || starts_with m "OBUF" || m = "IBUFDS" then p = "O"
  else p = "O" || p = "Q"

(* ---- packed result ------------------------------------------------------- *)
type packed_cell = {
  pc_name : string;
  pc_lef : string;
  pc_conns : (string * netkey) list;
  (* primitive inst -> BEL suffix within the placed site (for legalisation:
     NEXTPNR_BEL = <placed site>/<suffix>).  e.g. CARRY4, A6LUT, AFF. *)
  pc_bels : (string * string) list;
}

let lane_letter k = String.make 1 (Char.chr (Char.code 'A' + k))

type result = { cells : packed_cell list; report : (string * int) list }

let io_map = function
  | "IBUF" | "OBUF" | "IBUFDS" | "OBUFDS" | "IOBUF" | "IBUFDS_GTE2" -> Some "IOB"
  | "MMCME2_ADV" | "PLLE2_ADV" -> Some "MMCM"
  | "BUFG" | "BUFGCTRL" -> Some "BUFG"
  | "BUFH" | "BUFHCE" -> Some "BUFH"
  | "GTXE2_CHANNEL" | "GTXE2_COMMON" | "GTHE2_CHANNEL" | "GTHE2_COMMON" -> Some "GT"
  | _ -> None

(* Non-SLICE fabric hard cells -> their LEF site + BEL suffix. *)
let hard_map t =
  let u = String.uppercase_ascii t in
  if starts_with u "RAMB36" then Some ("RAMB36", "RAMB36E1")
  else if starts_with u "RAMB18" || starts_with u "FIFO18" then Some ("RAMB18", "RAMB18E1")
  else if starts_with u "DSP48" then Some ("DSP48", "DSP48E1")
  else None

(* SLICEM-resident cells (distributed RAM / SRL) -> their SLICE LEF config. *)
let slicem_map t =
  let u = String.uppercase_ascii t in
  if starts_with u "SRL" then Some ("SLICEM_SRL", "A6LUT")
  else if starts_with u "RAMD" || starts_with u "RAMS" || starts_with u "RAM32" || starts_with u "RAM64" then
    Some ("SLICEM_DRAM", "A6LUT")
  else if starts_with u "MUXF7" then Some ("SLICE_MUX", "F7AMUX")
  else if starts_with u "MUXF8" then Some ("SLICE_MUX", "F8MUX")
  else None

let pack (m : bmodule) : result =
  let widths = widths_of_module m in
  (* per-instance: port -> netkey array (LSB first) *)
  let inst_ports = Hashtbl.create 256 in
  List.iter (fun (i : binstance) ->
      let ports =
        List.map (fun (p, e) -> (p, Array.of_list (net_bits widths e)))
          i.port_connections in
      Hashtbl.replace inst_ports i.inst_name ports) m.instances;
  let inst_by_name = Hashtbl.create 256 in
  List.iter (fun (i : binstance) -> Hashtbl.replace inst_by_name i.inst_name i)
    m.instances;
  let port_bit iname pname idx =
    match List.assoc_opt pname (Hashtbl.find inst_ports iname) with
    | Some arr when idx < Array.length arr -> Some arr.(idx)
    | _ -> None in
  (* driver / sink maps keyed by netkey (only Net keys matter) *)
  let drv = Hashtbl.create 1024 and sinks = Hashtbl.create 1024 in
  List.iter (fun (i : binstance) ->
      List.iter (fun (p, arr) ->
          Array.iteri (fun bi nk ->
              match nk with
              | Net _ ->
                  if is_output i.module_name p then Hashtbl.replace drv nk (i.inst_name, p, bi)
                  else
                    let cur = try Hashtbl.find sinks nk with Not_found -> [] in
                    Hashtbl.replace sinks nk ((i.inst_name, p, bi) :: cur)
              | Const _ -> ())
            arr)
        (Hashtbl.find inst_ports i.inst_name))
    m.instances;

  let absorbed = Hashtbl.create 256 in
  let report = Hashtbl.create 32 in
  let bump k = Hashtbl.replace report k (1 + (try Hashtbl.find report k with Not_found -> 0)) in
  let packed = ref [] in
  let add ?(bels = []) name lef conns =
    packed := { pc_name = name; pc_lef = lef; pc_conns = conns; pc_bels = bels } :: !packed in
  let mtype n = (Hashtbl.find inst_by_name n).module_name in

  (* 1. CARRY4 -> SLICE_CARRY, absorb S-LUTs + sum FFs ---------------------- *)
  List.iter (fun (i : binstance) ->
      if starts_with (String.uppercase_ascii i.module_name) "CARRY4" then begin
        let conns = ref [] in
        let put k v = conns := (k, v) :: !conns in
        (* BEL stamp: constrain ONLY the CARRY4 primitive to the site SVS chose,
           anchoring the carry column (else nextpnr HeAP-scatters it and strands
           the short feedback arcs into the chain -> 119 unroutable arcs).  We do
           NOT stamp the absorbed sum-FFs / S-LUTs: nextpnr's carry-aware packer
           fills the slice legally, including the DI route-thru $LUTs it inserts
           -- stamping all 4 LUT slots leaves no room for those and unbinds. *)
        let bels = ref [ (i.inst_name, "CARRY4") ] in
        (match port_bit i.inst_name "CI" 0 with Some v -> put "CI" v | None -> ());
        (match port_bit i.inst_name "CYINIT" 0 with Some v -> put "CYINIT" v | None -> ());
        (match port_bit i.inst_name "CO" 3 with Some v -> put "CO" v | None -> ());
        Hashtbl.replace absorbed i.inst_name (); bump "CARRY4->SLICE_CARRY";
        for k = 0 to 3 do
          (match port_bit i.inst_name "S" k with Some v -> put (Printf.sprintf "S%d" k) v | None -> ());
          (match port_bit i.inst_name "DI" k with Some v -> put (Printf.sprintf "DI%d" k) v | None -> ());
          (match port_bit i.inst_name "O" k with
           | Some obit ->
               put (Printf.sprintf "O%d" k) obit;
               (* absorb sum FF: FDxE whose D == this O[k] *)
               (match Hashtbl.find_opt sinks obit with
                | Some ss ->
                    List.iter (fun (sc, sp, _) ->
                        if starts_with (String.uppercase_ascii (mtype sc)) "FD"
                           && String.uppercase_ascii sp = "D"
                           && not (Hashtbl.mem absorbed sc) then begin
                          Hashtbl.replace absorbed sc (); bump "sum-FF absorbed";
                          (match port_bit sc "Q" 0 with Some q -> put (Printf.sprintf "Q%d" k) q | None -> ());
                          (match port_bit sc "C" 0 with Some c -> put "CLK" c | None -> ());
                          (match port_bit sc "CE" 0 with Some c -> put "CE" c | None -> ());
                          (match port_bit sc "R" 0 with Some c -> put "SR" c
                           | None -> match port_bit sc "S" 0 with Some c -> put "SR" c | None -> ())
                        end) ss
                | None -> ())
           | None -> ());
          (* absorb S-LUT: a LUT driving this S[k] *)
          (match port_bit i.inst_name "S" k with
           | Some sbit -> (
               match Hashtbl.find_opt drv sbit with
               | Some (dn, _, _) when starts_with (String.uppercase_ascii (mtype dn)) "LUT"
                                      && not (Hashtbl.mem absorbed dn) ->
                   Hashtbl.replace absorbed dn (); bump "S-LUT absorbed";
                   List.iteri (fun li (lp, e) ->
                       if String.uppercase_ascii lp <> "O" then
                         match net_bits widths e with
                         | nk :: _ -> put (Printf.sprintf "S%d_%s" k lp) nk
                         | [] -> ignore li)
                     (Hashtbl.find inst_by_name dn).port_connections
               | _ -> ())
           | None -> ())
        done;
        let base = try
            let s = i.inst_name in
            let idx = Str.search_forward (Str.regexp "_i_1$") s 0 in
            String.sub s 0 idx
          with Not_found -> i.inst_name in
        add ~bels:(List.rev !bels) (base ^ "$carry") "SLICE_CARRY" (List.rev !conns)
      end)
    m.instances;

  (* 1a. MUXF7/MUXF8 wide-mux -> ONE SLICE_MUX pc, absorbing the mux(es) + their
        driving LUT6.  A MUXF7's two data inputs come from two LUT6 in the SAME
        physical slice (dedicated F7 routing); a MUXF8 combines the two F7 muxes
        of one slice.  Placing them as separate cells splits that dedicated path
        across slices and the mux nets become unroutable (the sender_ip readback
        muxes).  Pin the whole group to one slice: F8MUX + F7AMUX(A,B 6LUT) +
        F7BMUX(C,D 6LUT). *)
  let is_lut_t t = starts_with (String.uppercase_ascii t) "LUT" in
  let absorb_mux7 mn la lb =
    (* absorb MUXF7 [mn] as F7[la]MUX; its two driving LUT6 -> lanes la,lb.
       returns (bels, conns); marks mn + its LUTs absorbed. *)
    Hashtbl.replace absorbed mn ();
    let bels = ref [ (mn, (if la = "A" then "F7AMUX" else "F7BMUX")) ] in
    let conns = ref [] in
    (match port_bit mn "S" 0 with Some v -> conns := (("F7" ^ la ^ "S"), v) :: !conns | None -> ());
    (match port_bit mn "O" 0 with Some v -> conns := (("F7" ^ la ^ "O"), v) :: !conns | None -> ());
    let do_lut inp lane =
      match port_bit mn inp 0 with
      | Some sbit ->
          (match Hashtbl.find_opt drv sbit with
           | Some (dn, _, _) when is_lut_t (mtype dn) && not (Hashtbl.mem absorbed dn) ->
               Hashtbl.replace absorbed dn (); bump "mux-LUT absorbed";
               bels := (dn, lane ^ "6LUT") :: !bels;
               List.iter (fun (lp, e) ->
                   if String.uppercase_ascii lp <> "O" then
                     match net_bits widths e with
                     | nk :: _ -> conns := ((lane ^ lp), nk) :: !conns
                     | [] -> ())
                 (Hashtbl.find inst_by_name dn).port_connections
           | _ -> ())
      | None -> () in
    (* nextpnr's F7[la]MUX reads I0 from the SECOND lane (B/D) and I1 from the
       FIRST (A/C): I0<->B6LUT, I1<->A6LUT.  Assigning I0->la crosses both mux
       inputs, so every MUXF7 data path becomes an impossible A6LUT_O6<->B6LUT_O6
       route (the whole sender_mac/sender_ip/target_ip readback residual). *)
    do_lut "I0" lb; do_lut "I1" la;
    (!bels, !conns) in
  (* MUXF8 groups first (absorb the two feeding MUXF7 + their LUTs) *)
  List.iter (fun (i : binstance) ->
      if starts_with (String.uppercase_ascii i.module_name) "MUXF8"
         && not (Hashtbl.mem absorbed i.inst_name) then begin
        let bels = ref [ (i.inst_name, "F8MUX") ] and conns = ref [] in
        (match port_bit i.inst_name "S" 0 with Some v -> conns := ("F8S", v) :: !conns | None -> ());
        (match port_bit i.inst_name "O" 0 with Some v -> conns := ("F8O", v) :: !conns | None -> ());
        let grab inp la lb =
          match port_bit i.inst_name inp 0 with
          | Some mbit ->
              (match Hashtbl.find_opt drv mbit with
               | Some (mn, _, _) when starts_with (String.uppercase_ascii (mtype mn)) "MUXF7"
                                      && not (Hashtbl.mem absorbed mn) ->
                   let (b, c) = absorb_mux7 mn la lb in
                   bels := b @ !bels; conns := c @ !conns
               | _ -> ())
          | None -> () in
        grab "I1" "A" "B"; grab "I0" "C" "D";
        Hashtbl.replace absorbed i.inst_name (); bump "MUXF8->SLICE_MUX";
        add ~bels:(List.rev !bels) (i.inst_name ^ "$mux") "SLICE_MUX" (List.rev !conns)
      end)
    m.instances;
  (* standalone MUXF7 (not consumed by a MUXF8) *)
  List.iter (fun (i : binstance) ->
      if starts_with (String.uppercase_ascii i.module_name) "MUXF7"
         && not (Hashtbl.mem absorbed i.inst_name) then begin
        let (bels, conns) = absorb_mux7 i.inst_name "A" "B" in
        bump "MUXF7->SLICE_MUX";
        add ~bels:(List.rev bels) (i.inst_name ^ "$mux") "SLICE_MUX" (List.rev conns)
      end)
    m.instances;

  (* 1c. Distributed-RAM write-port grouping: RAM32X1D / RAM64X1D (and the _1
        inverted-WCLK variants) -> whole-slice SLICEM_DRAM packed cells.  yosys
        memory_libmap emits BIT-SLICE LUTRAM primitives (one RAM32X1D per data
        bit); instances sharing a WRITE PORT -- same WCLK, same WE, same write
        address A0..A{4,5} -- can legally cohabit one SLICEM (that is exactly
        what Vivado's RAM32M/RAM64M macros encode).  Left to the generic
        fallback each bit claimed its OWN SLICEM site, scattering a 72-bit
        memory across 72 slices and stranding the shared write-address nets.

        Slot allocation mirrors nextpnr xilinx/pack_dram.cc's RAM32X1D /
        RAM64X1D group path (z descends from D=3): each new slice burns the
        D6LUT on the write-address base cell nextpnr creates, EXCEPT that the
        first member's SPO folds into that base (z==2 fold).  Each connected
        read port (SPO/DPO) takes one 6LUT slot (RAM32X1D is packed via
        create_dram_lut = RAMD64E on a 6LUT with GND-padded address, NOT as
        5/6LUT RAMD32 pairs -- only the RAM32M macro path uses those).  So the
        capacity is 3 DPO-only or 2 dual-port X1D per slice, not 4.

        pc_bels stamps EACH ORIGINAL primitive at its primary slot (SP slot if
        SPO is connected, else the DP slot).  NB: nextpnr's pack_dram X1D path
        currently DISSOLVES the original cells into /ADDR /SP /DP subcells and
        drops their BEL attrs (the b649145a propagation fix covers only
        RAM32M/RAM64M) -- the stamps pin the SVS placement and are ready for
        the same fix to be extended to the X1D path.

        The _1 variants write on the FALLING WCLK edge: they are grouped
        SEPARATELY even when all port nets match (the group key includes the
        primitive type), and the packed cell carries a WCLK_INV Const-true
        marker so downstream never merges opposite-polarity groups. *)
  let dram_kind t = match String.uppercase_ascii t with
    | "RAM32X1D" -> Some (5, false)
    | "RAM32X1D_1" -> Some (5, true)
    | "RAM64X1D" -> Some (6, false)
    | "RAM64X1D_1" -> Some (6, true)
    | _ -> None in
  let dg_order = ref [] in
  let dgroups : (string * netkey option * netkey option * netkey option list,
                 binstance list ref) Hashtbl.t = Hashtbl.create 16 in
  List.iter (fun (i : binstance) ->
      match dram_kind i.module_name with
      | Some (abits, _) when not (Hashtbl.mem absorbed i.inst_name) ->
          let wa = List.init abits (fun k -> port_bit i.inst_name (Printf.sprintf "A%d" k) 0) in
          let key = (String.uppercase_ascii i.module_name,
                     port_bit i.inst_name "WCLK" 0, port_bit i.inst_name "WE" 0, wa) in
          (match Hashtbl.find_opt dgroups key with
           | Some l -> l := i :: !l
           | None -> Hashtbl.add dgroups key (ref [ i ]); dg_order := key :: !dg_order)
      | _ -> ())
    m.instances;
  List.iter (fun key ->
      let (ty, wclk, we, wa) = key in
      let members = List.rev !(Hashtbl.find dgroups key) in
      let abits, inv = match dram_kind ty with Some x -> x | None -> assert false in
      let slice_idx = ref 0 in
      (* current slice under construction: (inst, sp_slot opt, dp_slot opt) *)
      let cur = ref [] and z = ref (-1) in
      let flush () =
        (match List.rev !cur with
         | [] -> ()
         | ((i0 : binstance), _, _) :: _ as mems ->
             let conns = ref [] and bels = ref [] in
             let put k v = conns := (k, v) :: !conns in
             (match wclk with Some v -> put "WCLK" v | None -> ());
             (match we with Some v -> put "WE" v | None -> ());
             List.iteri (fun k b ->
                 match b with Some v -> put (Printf.sprintf "WA%d" k) v | None -> ()) wa;
             if inv then put "WCLK_INV" (Const true);   (* polarity marker, no HPWL *)
             List.iter (fun ((inst : binstance), sp, dp) ->
                 let prim = match sp with Some s -> s | None ->
                   (match dp with Some d -> d | None -> 3) in
                 bels := (inst.inst_name, lane_letter prim ^ "6LUT") :: !bels;
                 (match port_bit inst.inst_name "D" 0 with
                  | Some v -> put (lane_letter prim ^ "D") v | None -> ());
                 (match sp, port_bit inst.inst_name "SPO" 0 with
                  | Some s, Some v -> put (lane_letter s ^ "SPO") v | _ -> ());
                 (match dp with
                  | Some d ->
                      (match port_bit inst.inst_name "DPO" 0 with
                       | Some v -> put (lane_letter d ^ "DPO") v | None -> ());
                      for k = 0 to abits - 1 do
                        match port_bit inst.inst_name (Printf.sprintf "DPRA%d" k) 0 with
                        | Some v -> put (Printf.sprintf "%sDPRA%d" (lane_letter d) k) v
                        | None -> ()
                      done
                  | None -> ())) mems;
             add ~bels:(List.rev !bels)
               (Printf.sprintf "%s$dram%s%d" i0.inst_name (if inv then "_n" else "") !slice_idx)
               "SLICEM_DRAM" (List.rev !conns);
             incr slice_idx; bump "DRAM-group->SLICEM_DRAM");
        cur := []; z := -1 in
      List.iter (fun (inst : binstance) ->
          let has p = match port_bit inst.inst_name p 0 with Some (Net _) -> true | _ -> false in
          let spo = has "SPO" and dpo = has "DPO" in
          let zsz = (if spo then 1 else 0) + (if dpo then 1 else 0) in
          if zsz > 0 then begin      (* dead RAM (no read port): generic fallback *)
            if !z < 0 || !z - zsz + 1 < 0 then (flush (); z := 2);
            let sp_slot =
              if not spo then None
              else if !z = 2 then Some 3          (* fold into the D6LUT base *)
              else (let s = Some !z in decr z; s) in
            let dp_slot = if dpo then (let s = Some !z in decr z; s) else None in
            Hashtbl.replace absorbed inst.inst_name (); bump (ty ^ " grouped");
            cur := (inst, sp_slot, dp_slot) :: !cur
          end)
        members;
      flush ())
    (List.rev !dg_order);

  (* 1b. LUT + FF pairing -> SLICE_LOGIC (the recognition for the LUT-mapped
        main flow: gate_map is AIG+LUT-cover, no CARRY4).  Pack an FF with the
        LUT that drives its D (and that LUT's fanin), up to 4 pairs per slice,
        so each LUT->FF net stays in-slice.  Greedy: fill a slice with pairs as
        we encounter un-absorbed FFs. *)
  let is_lut t = starts_with (String.uppercase_ascii t) "LUT" in
  let is_ff t = starts_with (String.uppercase_ascii t) "FD" in
  let pending = ref [] and lane = ref 0 and slice_no = ref 0 and slice_conns = ref [] and slice_bels = ref [] in
  let cur_cs = ref None in
  let flush_slice () =
    if !slice_conns <> [] then begin
      add ~bels:(List.rev !slice_bels)
        (Printf.sprintf "logic_slice_%d" !slice_no) "SLICE_LOGIC" (List.rev !slice_conns);
      incr slice_no; slice_conns := []; slice_bels := []; lane := 0; cur_cs := None
    end in
  (* control set of an FF: {CLK, CE, SR-net, SR-kind}.  A 7-series slice shares
     ONE clock + ONE CE + ONE SRUSEDMUX across its FFs, so FFs with differing
     control sets CANNOT share a slice -- packing them together yields an
     unroutable slice (the SR/CE can't be driven two ways).  Vivado always packs
     by control set; we must too. *)
  let ff_cs (i : binstance) =
    let g p = port_bit i.inst_name p 0 in
    let srk, srn =
      match g "R" with Some n -> ("R", Some n) | None ->
      match g "S" with Some n -> ("S", Some n) | None ->
      match g "PRE" with Some n -> ("PRE", Some n) | None ->
      match g "CLR" with Some n -> ("CLR", Some n) | None -> ("", None) in
    (g "C", g "CE", srk, srn) in
  List.iter (fun (i : binstance) ->
      if is_ff i.module_name && not (Hashtbl.mem absorbed i.inst_name) then begin
        let cs = ff_cs i in
        if !lane > 0 && !cur_cs <> Some cs then flush_slice ();
        cur_cs := Some cs;
        let l = lane_letter !lane in
        let dnet = port_bit i.inst_name "D" 0 in
        (* absorb the driving LUT if present and free *)
        (match dnet with
         | Some (Net _ as nk) -> (match Hashtbl.find_opt drv nk with
             | Some (dn, _, _) when is_lut (mtype dn) && not (Hashtbl.mem absorbed dn) ->
                 Hashtbl.replace absorbed dn (); bump "LUT+FF paired";
                 slice_bels := (dn, l ^ "6LUT") :: !slice_bels;
                 List.iteri (fun li (lp, e) ->
                     if String.uppercase_ascii lp <> "O" then match net_bits widths e with
                       | nk :: _ -> slice_conns := (Printf.sprintf "%s%s" l lp, nk) :: !slice_conns | [] -> ignore li)
                   (Hashtbl.find inst_by_name dn).port_connections
             | _ -> ())
         | _ -> ());
        Hashtbl.replace absorbed i.inst_name (); bump "FF packed";
        slice_bels := (i.inst_name, l ^ "FF") :: !slice_bels;
        (match port_bit i.inst_name "Q" 0 with Some q -> slice_conns := (Printf.sprintf "%sQ" l, q) :: !slice_conns | None -> ());
        (match port_bit i.inst_name "C" 0 with Some c -> slice_conns := ("CLK", c) :: !slice_conns | None -> ());
        (match port_bit i.inst_name "CE" 0 with Some c -> slice_conns := ("CE", c) :: !slice_conns | None -> ());
        (match port_bit i.inst_name "R" 0 with Some c -> slice_conns := ("SR", c) :: !slice_conns
         | None -> match port_bit i.inst_name "S" 0 with Some c -> slice_conns := ("SR", c) :: !slice_conns | None -> ());
        incr lane; if !lane >= 4 then flush_slice ()
      end)
    m.instances;
  flush_slice ();
  ignore pending;

  (* 2. dedicated sites: MMCM / BUFG / IO ---------------------------------- *)
  List.iter (fun (i : binstance) ->
      if not (Hashtbl.mem absorbed i.inst_name)
         && i.module_name <> "GND" && i.module_name <> "VCC" then
        match io_map i.module_name with
        | Some lef ->
            let conns = List.filter_map (fun (p, arr) ->
                if Array.length arr > 0 then Some (p, arr.(0)) else None)
                (Hashtbl.find inst_ports i.inst_name) in
            (* IO is XDC pin-constrained (nextpnr pack_io); only clock sites get
               an explicit BEL suffix for the legaliser. *)
            let suffix = match lef with
              | "BUFG" -> ["BUFGCTRL"] | "BUFH" -> ["BUFH"] | "MMCM" -> ["MMCME2_ADV"] | _ -> [] in
            add ~bels:(List.map (fun s -> (i.inst_name, s)) suffix) (i.inst_name ^ "$site") lef conns;
            Hashtbl.replace absorbed i.inst_name ();
            bump (Printf.sprintf "%s->%s" i.module_name lef)
        | None -> ())
    m.instances;

  (* 3. leftover LUT / FF -> SLICE_LOGIC / SLICE_FF ------------------------- *)
  List.iter (fun (i : binstance) ->
      if not (Hashtbl.mem absorbed i.inst_name)
         && i.module_name <> "GND" && i.module_name <> "VCC" then begin
        let conns = List.filter_map (fun (p, arr) ->
            if Array.length arr > 0 then Some (p, arr.(0)) else None)
            (Hashtbl.find inst_ports i.inst_name) in
        let u = String.uppercase_ascii i.module_name in
        (match hard_map i.module_name, slicem_map i.module_name with
         | Some (lef, suffix), _ ->
             add ~bels:[ (i.inst_name, suffix) ] (i.inst_name ^ "$hard") lef conns;
             bump (i.module_name ^ "->" ^ lef)
         | None, Some (lef, suffix) ->
             add ~bels:[ (i.inst_name, suffix) ] (i.inst_name ^ "$m") lef conns;
             bump (i.module_name ^ "->" ^ lef)
         | None, None ->
             if starts_with u "LUT" || u = "INV" then
               (add ~bels:[ (i.inst_name, "A6LUT") ] (i.inst_name ^ "$logic") "SLICE_LOGIC" conns; bump "LUT->SLICE_LOGIC")
             else if starts_with u "FD" then
               (add ~bels:[ (i.inst_name, "AFF") ] (i.inst_name ^ "$ff") "SLICE_FF" conns; bump "FF->SLICE_FF")
             else (add (i.inst_name ^ "$?") ("UNKNOWN:" ^ i.module_name) conns; bump ("UNMAPPED " ^ i.module_name)));
        Hashtbl.replace absorbed i.inst_name ()
      end)
    m.instances;

  { cells = List.rev !packed;
    report = Hashtbl.fold (fun k v a -> (k, v) :: a) report [] }

(* ---- reporting ----------------------------------------------------------- *)
let print_result (r : result) =
  Printf.printf "=== recognition report ===\n";
  List.iter (fun (k, v) -> Printf.printf "  %4d  %s\n" v k)
    (List.sort (fun (_, a) (_, b) -> compare b a) r.report);
  let ncarry = List.length (List.filter (fun c -> c.pc_lef = "SLICE_CARRY") r.cells) in
  Printf.printf "packed -> %d LEF cells (%d SLICE_CARRY chained via CI/CO)\n"
    (List.length r.cells) ncarry;
  (* carry chain CI<-CO linkage *)
  let co2cell = Hashtbl.create 16 in
  List.iter (fun c ->
      if c.pc_lef = "SLICE_CARRY" then
        match List.assoc_opt "CO" c.pc_conns with Some nk -> Hashtbl.replace co2cell nk c.pc_name | None -> ())
    r.cells;
  Printf.printf "=== carry chain (CI<-CO) ===\n";
  List.iter (fun c ->
      if c.pc_lef = "SLICE_CARRY" then begin
        let ci = List.assoc_opt "CI" c.pc_conns in
        let prev = match ci with
          | Some nk -> (try Hashtbl.find co2cell nk with Not_found -> string_of_netkey nk)
          | None -> "-" in
        Printf.printf "  %-26s CI<-%s\n" c.pc_name prev
      end)
    (List.sort (fun a b -> compare a.pc_name b.pc_name) r.cells)

(* ---- standalone test loader: yosys JSON -> bmodule ----------------------- *)
(* read_netlist expects nextpnr POST-pack X_ORIG_PORT attrs; for the pre-place
   packer we read the yosys netlist directly, where the integer bit-id IS the
   net identity (two pins connect iff they share a bit-id).  Each bit-id b maps
   to a width-1 signal "n<b>"; a port's LSB-first bits become an MSB-first
   BConcat so net_bits reverses back to LSB-first.  The real SVS caller passes a
   bmodule straight to [pack]; this is only for the CLI regression. *)
(* Build a BIR bmodule from an already-parsed yosys/nextpnr JSON tree.  Lets the
   in-SVS flow (gate-map -> Bir_to_nextpnr_json.yosys_json -> this) reach the
   placer with NO file round-trip; bmodule_of_yosys_json is the file wrapper. *)
let bmodule_of_yosys_tree (j : Yojson.Safe.t) : bmodule =
  let module U = Yojson.Safe.Util in
  let mods = j |> U.member "modules" |> U.to_assoc in
  (* pick the real top: the module with the most instantiated cells (blackbox
     library defs and $specify2-only stubs have few/none). *)
  let ncells (_, mj) = try List.length (mj |> U.member "cells" |> U.to_assoc) with _ -> 0 in
  let _, mj = List.fold_left (fun best m -> if ncells m > ncells best then m else best)
      (List.hd mods) (List.tl mods) in
  let cells = mj |> U.member "cells" |> U.to_assoc in
  let bit_expr = function
    | `Int id -> BVar (Printf.sprintf "n%d" id)
    | `String "1" -> bconst_int 1 1
    | `String "0" -> bconst_int 0 1
    | _ -> bconst_int 0 1 in
  let instances = List.map (fun (inst, cj) ->
      let ptype = cj |> U.member "type" |> U.to_string in
      let conns = try cj |> U.member "connections" |> U.to_assoc with _ -> [] in
      let pcs = List.map (fun (pin, bj) ->
          let bits = U.to_list bj in
          (pin, BConcat (List.rev_map bit_expr bits)))  (* json LSB-first -> MSB-first concat *)
          conns in
      { inst_name = inst; module_name = ptype; param_values = [];
        param_strs = []; port_connections = pcs }) cells in
  { name = "top"; params = []; signals = []; processes = [];
    instances; funcs = []; mems = []; attrs = [] }

(* File wrapper: parse the JSON then delegate to the in-memory tree builder. *)
let bmodule_of_yosys_json path : bmodule =
  bmodule_of_yosys_tree (Yojson.Safe.from_file path)
