(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: prologTerm.ml                                               == *)
(* ==  Génération de termes Prolog                                         == *)
(* ========================================================================== *)
open Ast
open Format

let sep_cma fmt () = fprintf fmt ", "

let pp_lst_cma p = pp_print_list ~pp_sep:sep_cma p

let rec pp_expr fmt e =
  match e with
    | ASTNum n -> fprintf fmt "num(%d)" n
    | ASTId x -> fprintf fmt "ident(%s)" x
    | ASTApp(e, es) -> fprintf fmt "app(%a,[%a])" pp_expr  e  pp_exprs es
and pp_exprs fmt es = pp_lst_cma pp_expr fmt es

let pp_stat fmt s =
  match s with
  ASTEcho e -> fprintf fmt "echo(%a)" pp_expr e
let pp_cmd fmt c =
  match c with
  ASTStat s ->
    fprintf fmt "stat(%a)" pp_stat s
let pp_cmds fmt cmds =
  match cmds with
  c :: [] -> pp_cmd fmt c
  | _ -> failwith "TODO"
let pp_prog fmt p =
  fprintf fmt "prog(%a).\n" pp_cmds p




