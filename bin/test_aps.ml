open Aps_syntax.Manip_sys
open Aps_syntax.PrologTerm
open Aps_syntax.Semantic

let l_test_0 = [(testfile_name 0 0, "OK"); (testfile_name 0 1, "KO");
                (testfile_name 0 2, "KO"); (testfile_name 0 3, "KO");
                (testfile_name 0 4, "OK"); (testfile_name 0 5, "OK");
                (testfile_name 0 6, "KO"); (testfile_name 0 7, "OK");
                (testfile_name 0 8, "OK"); (testfile_name 0 9, "OK");
                (testfile_name 0 10, "OK");(testfile_name 0 11, "OK");
                (testfile_name 0 12, "KO");(testfile_name 0 13, "OK");
                (testfile_name 0 14, "OK");(testfile_name 0 15, "OK");
                (testfile_name 0 16, "OK");(testfile_name 0 17, "OK");
                (testfile_name 0 18, "KO");(testfile_name 0 19, "KO");
                (testfile_name 0 20, "KO");(testfile_name 0 21, "KO")]



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

let test_semantic (l_test : string list) =
List.fold_right
(fun fname _ ->
  let p = get_prog fname in
  Format.printf "%s |\t" fname ;
      pp_prog Format.str_formatter p;
      let s = Format.flush_str_formatter () in
      match cmd_typ s with
      | Ok(res,_) -> 
          if String.trim res = "OK" then (
            Format.printf " -> Résultat de l'évaluation : " ;
            (try 
              eval_prog p;
              print_newline ()
            with e -> Format.printf "Erreur : %s\n\n" (Printexc.to_string e))
          ) else (
            Format.printf " -> Skipping evaluation (Typer KO)\n\n"
          );
      | Error (`Msg m) -> print_endline m ;
      print_newline ();
      ) l_test ()

let _ =
  Format.printf "========== Tests de APS 0 ==========\n";
  Format.printf "- Test de PrologTerm\n";
  test_prologTerm (fst (List.split l_test_0 ));
  print_endline "- Test du typeur\n";
  test_typeur l_test_0;
  print_newline ();
  print_endline "- Test de la sémantique\n";
  test_semantic (fst (List.split l_test_0 ))

