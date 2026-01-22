open Psl
open Dump
open Setup
open Vparser
open Semantics

let psltest str =
  let rslt = Vparse.parse str in
  Semantics.endscan();

  if rslt then
    begin
      List.iter (fun (str,_,_) -> print_endline str) !Semantics.psl_lst
    end
  else for i = 0 to hsiz-1 do let idx = (hsiz-i+(!histcnt))mod hsiz in
                     let item = (history.(idx)) in (match item.tok with
                     | IDSTR id
		     | TokenProp id -> Printf.printf "\nBacktrace %d : %s (%d-%d) %s" 
                     i id item.strt item.stop (string_of_bool item.psl)
		     | _ -> Printf.printf "\nBacktrace %d : %s (%d-%d) %s" 
                     i (str_token (item.tok)) item.strt item.stop (string_of_bool item.psl)) done

let _ = List.iter psltest (List.tl (Array.to_list Sys.argv))
