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
                (testfile_name 0 20, "KO");(testfile_name 0 21, "KO");
                (testfile_name 0 22, "OK")]

let l_test_1 = [(testfile_name 1 0, "OK"); (testfile_name 1 1, "OK");
                (testfile_name 1 2, "OK"); (testfile_name 1 3, "OK");
                (testfile_name 1 4, "OK"); (testfile_name 1 5, "OK");
                (testfile_name 1 6, "OK"); (testfile_name 1 7, "OK");
                (testfile_name 1 8, "OK"); (testfile_name 1 9, "OK");
                (testfile_name 1 10, "OK");(testfile_name 1 11, "OK");
                (testfile_name 1 12, "OK");(testfile_name 1 13, "KO");
                (testfile_name 1 14, "KO");(testfile_name 1 15, "KO")]

let l_test_1a = [(testfile_name_1a 0, "OK"); (testfile_name_1a 1, "OK");
                 (testfile_name_1a 2, "OK"); (testfile_name_1a 3, "OK")]

let l_test_2 = [(testfile_name 2 0, "OK"); (testfile_name 2 1, "OK");
                (testfile_name 2 2, "OK"); (testfile_name 2 3, "OK");
                (testfile_name 2 4, "OK"); (testfile_name 2 5, "OK");
                (testfile_name 2 6, "OK"); (testfile_name 2 7, "OK");
                (testfile_name 2 8, "KO"); (testfile_name 2 9, "KO");
                (testfile_name 2 10, "KO");(testfile_name 2 11, "KO");
                (testfile_name 2 12, "KO");(testfile_name 2 13, "KO");
                (testfile_name 2 14, "OK");(testfile_name 2 15, "OK");
                (testfile_name 2 16, "OK");(testfile_name 2 17, "OK");
                (testfile_name 2 18, "OK")]



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
      | Ok(s,_) -> let match_ok = (s = expected) in
                   let green_bg = if match_ok then "\027[42m" else "\027[41m" in
                   let reset = "\027[0m" in 
                   Format.printf "%s%s%s |\t Résultat du typeur : %s\t Résultat attendu : %s\n" green_bg fname reset s expected
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

let run_tests aps prolog typer semantic =
  let aps_testfiles = [("0", l_test_0); ("1", l_test_1); ("1a", l_test_1a); ("2", l_test_2)] in
  let selected = match aps with [] -> aps_testfiles | _ -> List.filter (fun (k,_) -> List.mem k aps) aps_testfiles in
  List.iter (fun (name, typed_tests) ->
    let file_list = fst (List.split typed_tests) in
    Format.printf "========== Tests de APS %s ==========\n" name;
    if prolog then (
      Format.printf "- Test de PrologTerm\n";
      test_prologTerm file_list);
    if typer then (
      Format.printf "- Test du typeur \n";
      test_typeur typed_tests);
    if semantic then (
      Format.printf "- Test de la sémantique\n";
      test_semantic file_list);
    Format.printf "\n\n"
  ) selected

let _ =
  let args = List.tl (Array.to_list Sys.argv) in
  let prolog = ref false
  and typer = ref false
  and semantic = ref false
  and aps_list = ref [] in

  let rec parse = function
    | [] -> ()
    | "--prologTerm" :: ll -> prolog := true; parse ll
    | "--typer" :: ll -> typer := true; parse ll
    | "--semantic" :: ll -> semantic := true; parse ll
    | ("--aps0" | "--aps1" | "--aps1a" | "--aps2") as opt :: ll ->
        let aps = String.sub opt 5 (String.length opt - 5) in
        aps_list := aps :: !aps_list;
        parse ll
    | h :: _ -> Printf.eprintf "Option inconnue : %s\n" h; exit 1
  in
  parse args;

  let run_prolog = not !typer && not !semantic || !prolog in
  let run_typer = not !prolog && not !semantic || !typer in
  let run_semantic = not !prolog && not !typer || !semantic in
  run_tests !aps_list run_prolog run_typer run_semantic
