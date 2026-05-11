(* Critical-path cell-set parsed from a 6_finish.rpt.
   Used by structural passes (kary_merge, mux_chain_flatten) as a
   feedback signal: "this inst is on the worst-slack path of the prior
   ORFS run, so be conservative with rewrites that touch it".

   The parse is intentionally minimal — just collect every inst_name
   on every max-path block.  Tools that want richer info (per-hop
   arrival, cell type) can re-parse the rpt themselves; this module
   only answers `mem`.                                                *)

let path_block_re = Str.regexp "^Path Type: max"
let startpoint_re = Str.regexp "^Startpoint:"

(* Hop line layout (OpenSTA, after [synth_pipeline]'s greedy regex fix):
     fanout cap slew delay time direction inst_path/pin (cell)
   We capture the inst part — greedy [^ \t]+ matches up to the LAST
   "/", same trick as sv_gui's hop_re. *)
let hop_re =
  Str.regexp
    "^[ \t]*[0-9]+[ \t]+[0-9.]+[ \t]+[0-9.]+[ \t]+[0-9.]+[ \t]+[0-9.]+\
     [ \t]+[v^][ \t]+\\([^ \t]+\\)/[^ \t/]+[ \t]+(\\([^ \t)]+\\))"

(* Store both the full hierarchical path AND the leaf name (the segment
   after the last "/").  hier_synth synthesises per-module, so each
   netlist's inst_name is the leaf — but the rpt records full paths
   like `cpu/_T__…__OR2_X1__2446_`.  Indexing both lets the kary_merge
   gate match either form without forcing the caller to pre-strip. *)
let load path : (string, unit) Hashtbl.t =
  let h = Hashtbl.create 256 in
  if not (Sys.file_exists path) then h
  else begin
    let ic = open_in path in
    let in_max = ref false in
    let n_full = ref 0 in
    (try
      while true do
        let line = input_line ic in
        if Str.string_match startpoint_re line 0 then in_max := false
        else if Str.string_match path_block_re line 0 then in_max := true
        else if !in_max && Str.string_match hop_re line 0 then begin
          let inst = Str.matched_group 1 line in
          if not (Hashtbl.mem h inst) then incr n_full;
          Hashtbl.replace h inst ();
          let leaf =
            try
              let i = String.rindex inst '/' in
              String.sub inst (i + 1) (String.length inst - i - 1)
            with Not_found -> inst in
          Hashtbl.replace h leaf ()
        end
      done
    with End_of_file -> ());
    close_in ic;
    Printf.eprintf "[timing_ref] loaded %d critical-path hop(s) (%d distinct \
                    full paths, %d total entries incl. leaves) from %s\n%!"
      !n_full !n_full (Hashtbl.length h) path;
    h
  end

(* Convenience accessor — returns Some table if SV_DECOMP_TIMING_REF is
   set and points at a readable rpt, None otherwise.  The kary_merge
   pass calls this once per module-synth invocation; passing the table
   around is cheaper than re-parsing.  *)
let from_env () : (string, unit) Hashtbl.t option =
  match Sys.getenv_opt "SV_DECOMP_TIMING_REF" with
  | None | Some "" -> None
  | Some p when Sys.file_exists p -> Some (load p)
  | Some p ->
      Printf.eprintf "[timing_ref] WARN: %s not readable — proceeding without slack gate\n%!" p;
      None

let mem (tbl : (string, unit) Hashtbl.t option) (inst_name : string) : bool =
  match tbl with
  | None -> false
  | Some h -> Hashtbl.mem h inst_name
