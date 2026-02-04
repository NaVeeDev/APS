open Aps_syntax.Manip_sys
open Aps_syntax.PrologTerm

let l_test_0 = [(testfile_name 0 0, "OK")]



let test_prologTerm (l_test : string list) =
List.fold_right
(fun fname _ ->
  let p = get_prog fname in
      Format.printf "%s |\t %a\n" fname pp_prog p ;

) l_test ()

let test_typeur (l_test : (string * string) list ) =
List.fold_right
(fun (fname,expected) _ ->
  let p = get_prog fname  in
      pp_prog Format.str_formatter p;
      let s = Format.flush_str_formatter () in
      match cmd_typ  s with
      | Ok(s,_) -> Format.printf "%s |\t Résultat du typeur : %s\t Résultat attendu : %s\n" fname s expected
      | Error (`Msg m) -> print_endline m

) l_test ()

let _ =
  Format.printf "========== Tests de APS 0 ==========\n";
  Format.printf "- Test de PrologTerm\n";
  test_prologTerm (fst (List.split l_test_0 ));
  print_endline "- Test du typeur\n";
  test_typeur l_test_0

