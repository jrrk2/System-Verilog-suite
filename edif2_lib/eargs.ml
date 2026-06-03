open Eparse
open Edif2
open Emain

let _ = let tail = List.tl (Array.to_list Sys.argv) in eargs tail
