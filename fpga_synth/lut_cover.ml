(* LUT covering: the new kernel the old read_library mapper didn't need.
 *
 * A std-cell mapper recognises a cone against a *fixed* set of gate
 * functions.  A LUT computes an ARBITRARY k-input function, so instead
 * we (1) enumerate k-feasible cuts per node of the subject graph,
 * (2) evaluate each chosen cut's cone over all 2^k input assignments to
 * get its truth table, which is exactly the LUTk INIT, and (3) select a
 * cut per root (area- or depth-oriented) to cover the graph.
 *
 * Scaffold: data types are real; the three passes are stubs with the
 * intended signatures so xil_prim/fpga_emit can be wired against them. *)

open! Base

(* Subject graph: an And-Inverter-ish DAG.  Inputs and constants are
 * leaves; internal nodes are 2-input gates with optional input
 * inversions (AIG form keeps cut enumeration + truth-table eval
 * uniform).  Real lowering from Behavioral_ir/hardcaml populates this. *)
type gate =
  | Input of string
  | Const of bool
  | And2 of { a : int; b : int; a_inv : bool; b_inv : bool }

type node = { id : int; gate : gate }

type graph = { nodes : node array; outputs : (string * int * bool) list }
(* outputs: (port_name, node id, inverted) *)

(* A k-feasible cut: a cone rooted at [root] whose [leaves] (<= k node
 * ids) are the LUT inputs. *)
type cut = { root : int; leaves : int list }

(* Truth table of a cut's cone: 2^|leaves| values, index i giving the
 * cone output when leaf j carries bit j of i.  This list is fed
 * directly to Xil_prim.lutk ~truth. *)
let truth_table_of_cut (_g : graph) (_cut : cut) : bool list =
  (* TODO: topologically evaluate the cone between root and leaves for
     each of the 2^|leaves| leaf assignments. *)
  failwith "lut_cover.truth_table_of_cut: TODO"

(* All k-feasible cuts per node (priority-cut style: keep the best C
 * cuts per node by area/depth to bound the blow-up). *)
let enumerate_cuts ~(k : int) (_g : graph) : cut list array =
  ignore k;
  (* TODO: cut enumeration — leaf cut {n} for each node, then for an
     And2 merge child cuts and keep those with <= k leaves. *)
  failwith "lut_cover.enumerate_cuts: TODO"

(* Pick one cut per root to form the cover (area-flow / depth-relax). *)
let cover ~(k : int) (g : graph) : cut list =
  ignore (enumerate_cuts ~k g);
  (* TODO: select cuts (from outputs backwards) minimising LUT count
     subject to the depth target. *)
  failwith "lut_cover.cover: TODO"
