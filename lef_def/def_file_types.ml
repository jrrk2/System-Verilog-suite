(* Stub referenced by def_file.mly's prelude.  packhash/typehash are
   carry-overs from a previous bison grammar that recorded
   user-defined identifiers; they are unused by the DEF rules but
   must exist for the prelude to compile. *)

let typehash : (string, unit) Hashtbl.t = Hashtbl.create 257
let packhash : (string, unit) Hashtbl.t = Hashtbl.create 257
