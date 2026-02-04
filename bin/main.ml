open Aps_syntax
(* open PrologTerm *)
open PrologTerm
open Manip_sys


let print_prog () =
  let fname = Sys.argv.(1) in
  let p = get_prog fname in

    pp_prog Format.str_formatter p;
    let s = Format.flush_str_formatter () in
    Format.printf "==== Test du pretty printer de termes ====\n %s " s ;
    Format.printf "==== Test du typage du programme ====\n" ;
    match cmd_typ  s with
    | Ok(s,_) -> Format.printf "%s\n" s
    | Error (`Msg m) -> print_endline m 


let _ = print_prog ()