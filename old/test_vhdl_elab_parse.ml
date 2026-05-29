(* Dump shape of Vivado-emitted VHDL after vhd_front parsing.
 * Used to design the VHDL→ver_front-tree converter. *)
open Vhd_front.VhdlTree

let tag = function
  | VhdNone -> "VhdNone"
  | Str s -> Printf.sprintf "Str(%s)" s
  | Char c -> Printf.sprintf "Char(%c)" c
  | Real f -> Printf.sprintf "Real(%f)" f
  | Num s -> Printf.sprintf "Num(%s)" s
  | List _ -> "List"
  | Double _ -> "Double" | Triple _ -> "Triple" | Quadruple _ -> "Quadruple"
  | Quintuple _ -> "Quintuple" | Sextuple _ -> "Sextuple"
  | Septuple _ -> "Septuple" | Octuple _ -> "Octuple"
  (* For the leaf Vhd* tags we just print which constructor it is using
   * Obj — this is a debug script, not production code. *)
  | other ->
      let r = Obj.repr other in
      if Obj.is_block r then
        Printf.sprintf "Vhd<tag=%d>" (Obj.tag r)
      else
        Printf.sprintf "Vhd<int=%d>" (Obj.magic r : int)

let rec dump indent t =
  let pad = String.make (indent * 2) ' ' in
  match t with
  | List xs ->
      Printf.printf "%sList[%d]\n" pad (List.length xs);
      List.iter (dump (indent + 1)) xs
  | Double (a, b) ->
      Printf.printf "%sDouble(%s)\n" pad (tag a);
      dump (indent + 1) b
  | Triple (a, b, c) ->
      Printf.printf "%sTriple(%s)\n" pad (tag a);
      dump (indent + 1) b; dump (indent + 1) c
  | Quadruple (a, b, c, d) ->
      Printf.printf "%sQuadruple(%s)\n" pad (tag a);
      List.iter (dump (indent + 1)) [b; c; d]
  | Quintuple (a, b, c, d, e) ->
      Printf.printf "%sQuintuple(%s)\n" pad (tag a);
      List.iter (dump (indent + 1)) [b; c; d; e]
  | Sextuple (a, b, c, d, e, f) ->
      Printf.printf "%sSextuple(%s)\n" pad (tag a);
      List.iter (dump (indent + 1)) [b; c; d; e; f]
  | Septuple (a, b, c, d, e, f, g) ->
      Printf.printf "%sSeptuple(%s)\n" pad (tag a);
      List.iter (dump (indent + 1)) [b; c; d; e; f; g]
  | other -> Printf.printf "%s%s\n" pad (tag other)

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "usage: %s <file.vhd>\n" Sys.argv.(0); exit 2
  end;
  let f = Sys.argv.(1) in
  let fresh = Hashtbl.create 256 in
  Vhd_front.Vabstraction.vhdlhash := fresh;
  let succ = ref true in
  Vhd_front.VhdlMain.main succ [f];
  Printf.printf "parse success=%b, %d trees\n%!" !succ (Hashtbl.length fresh);
  Hashtbl.iter (fun (k, _) _ ->
    Printf.printf "\n=== Tree ===\n";
    dump 0 k
  ) fresh
