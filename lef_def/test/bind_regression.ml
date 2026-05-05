(* Unit-style regression for Bir_def_bind.

   We build a synthetic placement set with names that exercise
   each separator rule (".", "_", "[", digit, escaped) and assert
   the binding gathers them correctly. *)

open Lef_def

let p name = { Placement.inst = name; cell = "BUF_X1";
               x = 0; y = 0; orient = Placement.N }

let placements = [
  p "u_add";              p "u_add._123_";
  p "u_add[0]";           p "u_add_dout_reg";
  p "u_add.subnet._9_";
  p "u_addressing";       (* must NOT match — "ressing" is alpha *)
  p "rebuffer";  p "rebuffer1";  p "rebuffer42";
  p "\\u_add"; p "\\u_add._wire_";
  p "_345_";              (* anon, no prefix match *)
]

let assert_eq label want got =
  if want = got then Printf.printf "  OK   %s\n" label
  else begin
    Printf.printf "  FAIL %s: want %d, got %d\n" label want got;
    exit 1
  end

let () =
  let bs = Bir_def_bind.bind_by_prefix
             [ "u_add"; "rebuffer"; "_NEVER_" ] placements in
  let by_name = List.map (fun b ->
    b.Bir_def_bind.bir_path, List.length b.Bir_def_bind.members) bs in
  Printf.printf "Bindings: %s\n"
    (String.concat ", "
       (List.map (fun (n,c) -> Printf.sprintf "%s=%d" n c) by_name));

  (* u_add: u_add, u_add._123_, u_add[0], u_add_dout_reg,
            u_add.subnet._9_, \u_add, \u_add._wire_  → 7
     (NOT u_addressing — letter follows the prefix) *)
  assert_eq "u_add prefix"   7 (List.assoc "u_add"   by_name);

  (* rebuffer: rebuffer, rebuffer1, rebuffer42 → 3 *)
  assert_eq "rebuffer prefix" 3 (List.assoc "rebuffer" by_name);

  (* _NEVER_: nothing matches *)
  assert_eq "_NEVER_ empty"   0 (List.assoc "_NEVER_" by_name);

  let unbound = Bir_def_bind.unbound bs placements in
  (* unbound: u_addressing, _345_  → 2 *)
  assert_eq "unbound count"   2 (List.length unbound);

  print_endline "OK   bind_regression PASS"
