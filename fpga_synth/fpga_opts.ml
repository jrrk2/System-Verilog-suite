(* One configuration record for the FPGA synthesis flow.
 *
 * Every knob that used to be a free-standing environment variable read deep
 * inside a pass (GATE_MAP_*, MEMLOWER_FPGA, LUTCOVER_BACKEND, BALANCED_CASE)
 * is collected here with an explicit DEFAULT PROFILE, so the flow a user gets
 * without setting anything is the measured-best one rather than the
 * historical-first one.  The environment variables still work as overrides
 * (each accepts 1 / 0 to force on / off), and SVS_PROFILE=legacy restores the
 * old everything-off defaults for A/B comparison.
 *
 * Why these defaults (picorv32, xc7, k=6; yosys synth_xilinx = 1026 LUT /
 * 573 FF / 97 CARRY4 / 12 RAM32M for reference):
 *   - bir_opt   on : 2088 -> 1244 LUT, 703 -> 564 FF (const-prop + DCE on the
 *                    BIR before create_circuit; the single largest lever).
 *   - fpga_mem  on : without it the register file bit-blasts to 1024 FDREs +
 *                    read muxes (4365 LUT); with it 12 x RAM32M like yosys.
 *   - backend  abc : the same AIG through abc's `if` mapper is 828 LUT vs
 *                    1244 native.  Auto-selected only when a yosys-abc binary
 *                    is found; native otherwise.
 *   - dsp_infer on : BMul -> DSP48E1 (cascade_mac: 9 DSPs instead of ~1000
 *                    LUTs of array multiplier).
 *   - balanced_case / lutpack / mfs2 / aig_balance OFF: measured worse on area
 *                    (transmitter 49 -> 56/57 LUT, picorv32 2088 -> 2160/2226)
 *                    with no depth improvement on picorv32; kept as opt-ins. *)

open! Base

type backend = Native | Abc

type t =
  { bir_opt : bool        (* const-prop + DCE on the module BIR before lowering *)
  ; bir_cse : bool        (* CSE too (opt-in: creates temps create_circuit must bind) *)
  ; fpga_mem : bool       (* memlower targets LUTRAM/BRAM, not ASIC macros *)
  ; dsp_infer : bool      (* BMul -> DSP48E1 *)
  ; dsp_min_width : int   (* smallest operand width worth a DSP *)
  ; backend : backend     (* LUT cover: native Lut_cover or abc `if` *)
  ; abc_bin : string option
  ; mode : Lut_cover.cost_mode
  ; lutpack : bool
  ; mfs2_var : bool
  ; mfs2_odc : bool
  ; aig_balance : int
  ; balanced_case : bool  (* parallel case tree when keys are proven disjoint *)
  }

let home () = Option.value (Stdlib.Sys.getenv_opt "HOME") ~default:""

(* yosys-abc: SVS_ABC, then the in-tree deps copy, then the ORFS install, then
   PATH.  None -> native cover. *)
let find_abc () : string option =
  let exists p = Stdlib.Sys.file_exists p in
  match Stdlib.Sys.getenv_opt "SVS_ABC" with
  | Some s when String.length s > 0 && exists s -> Some s
  | _ ->
    let cands =
      [ "deps/yosys/yosys-abc"
      ; home () ^ "/OpenROAD-flow-scripts/tools/install/yosys/bin/yosys-abc"
      ; home () ^ "/oss-cad-suite/bin/yosys-abc"
      ; "/usr/local/bin/yosys-abc"
      ; "/usr/bin/yosys-abc" ]
    in
    match List.find cands ~f:exists with
    | Some p -> Some p
    | None ->
      if Stdlib.Sys.command "command -v yosys-abc > /dev/null 2>&1" = 0
      then Some "yosys-abc" else None

let legacy =
  { bir_opt = false; bir_cse = false; fpga_mem = false; dsp_infer = false
  ; dsp_min_width = 4; backend = Native; abc_bin = None; mode = `Area
  ; lutpack = false; mfs2_var = false; mfs2_odc = false; aig_balance = 0
  ; balanced_case = false }

let default () =
  let abc = find_abc () in
  { legacy with
    bir_opt = true; fpga_mem = true; dsp_infer = true
  ; backend = (match abc with Some _ -> Abc | None -> Native)
  ; abc_bin = abc }

let env = Stdlib.Sys.getenv_opt

(* "1" -> on, "0" -> off, anything else -> keep *)
let flag name d =
  match env name with Some "1" -> true | Some "0" -> false | _ -> d

let int_env name d =
  match env name with
  | Some s -> (try Int.of_string s with _ -> d)
  | None -> d

let mode_of_env d : Lut_cover.cost_mode =
  match env "GATE_MAP_MODE" with
  | Some "area" -> `Area
  | Some "delay" -> `Delay
  | Some s when String.length s >= 5 && String.equal (String.sub s ~pos:0 ~len:5) "mixed" ->
    let tol =
      if String.length s > 6 && Char.equal s.[5] ':'
      then (try Int.of_string (String.sub s ~pos:6 ~len:(String.length s - 6)) with _ -> 1)
      else 1
    in
    `Mixed tol
  | _ -> d

let from_env () : t =
  let base =
    match env "SVS_PROFILE" with
    | Some "legacy" -> legacy
    | _ -> default ()
  in
  let backend, abc_bin =
    match env "LUTCOVER_BACKEND" with
    | Some "abc" ->
      (match base.abc_bin with
       | Some b -> Abc, Some b
       | None -> (match find_abc () with Some b -> Abc, Some b | None -> Native, None))
    | Some "native" -> Native, base.abc_bin
    | _ -> base.backend, base.abc_bin
  in
  let mfs2 = flag "GATE_MAP_MFS2" false in
  { bir_opt = flag "GATE_MAP_BIR_OPT" base.bir_opt
  ; bir_cse = flag "GATE_MAP_BIR_CSE" base.bir_cse
  ; fpga_mem = flag "MEMLOWER_FPGA" base.fpga_mem
  ; dsp_infer = flag "GATE_MAP_DSP" base.dsp_infer
  ; dsp_min_width = int_env "GATE_MAP_DSP_MIN" base.dsp_min_width
  ; backend; abc_bin
  ; mode = mode_of_env base.mode
  ; lutpack = flag "GATE_MAP_LUTPACK" base.lutpack
  ; mfs2_var = flag "GATE_MAP_MFS2_VAR" (base.mfs2_var || mfs2)
  ; mfs2_odc = flag "GATE_MAP_MFS2_ODC" (base.mfs2_odc || mfs2)
  ; aig_balance = int_env "GATE_MAP_AIG_BALANCE" base.aig_balance
  ; balanced_case = flag "BALANCED_CASE" base.balanced_case }

let current : t Lazy.t = lazy (from_env ())
let get () = Lazy.force current

let describe (o : t) =
  Printf.sprintf
    "profile=%s bir_opt=%b bir_cse=%b fpga_mem=%b dsp=%b(min %d) backend=%s mode=%s \
     lutpack=%b mfs2=%b/%b aig_balance=%d balanced_case=%b"
    (match env "SVS_PROFILE" with Some p -> p | None -> "default")
    o.bir_opt o.bir_cse o.fpga_mem o.dsp_infer o.dsp_min_width
    (match o.backend with
     | Abc -> "abc(" ^ Option.value o.abc_bin ~default:"?" ^ ")"
     | Native -> "native")
    (match o.mode with `Area -> "area" | `Delay -> "delay" | `Mixed t -> Printf.sprintf "mixed:%d" t)
    o.lutpack o.mfs2_var o.mfs2_odc o.aig_balance o.balanced_case

let announced = ref false
let announce () =
  if not !announced then begin
    announced := true;
    Stdlib.Printf.eprintf "[fpga_opts] %s\n%!" (describe (get ()))
  end
