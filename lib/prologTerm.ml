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

let rec pp_arg fmt (x, t) =
  fprintf fmt "(%s, %a)" x pp_typee t
and pp_typee fmt t =
  match t with
    ASTInt -> fprintf fmt "int"
  | ASTBool -> fprintf fmt "bool"
  | ASTArrow(args, res) -> fprintf fmt "arrow(%a, %a)" pp_star args pp_typee res
and pp_star fmt ts =
  match ts with
  | [] -> ()
  | [t] -> pp_typee fmt t
  | t :: rest -> fprintf fmt "star(%a, %a)" pp_typee t pp_star rest

let rec pp_expr fmt e =
  match e with
    | ASTNum n -> fprintf fmt "num(%d)" n
    | ASTId x -> fprintf fmt "ident(%s)" x
    | ASTIf(e1, e2, e3) -> fprintf fmt "if(%a, %a, %a)" pp_expr e1 pp_expr e2 pp_expr e3
    | ASTAnd(e1, e2) -> fprintf fmt "and(%a, %a)" pp_expr e1 pp_expr e2
    | ASTOr(e1, e2) -> fprintf fmt "or(%a, %a)" pp_expr e1 pp_expr e2
    | ASTApp(e, es) -> fprintf fmt "app(%a,[%a])" pp_expr  e  pp_exprs es
    | ASTAbs(args, e) -> fprintf fmt "abs([%a], %a)" (pp_lst_cma pp_arg) args pp_expr e
and pp_exprs fmt es = pp_lst_cma pp_expr fmt es

let pp_def fmt d =
  match d with
    ASTConst(x, t, e) -> fprintf fmt "const(%s,%a,%a)" x pp_typee t pp_expr e
    | ASTFun(x, t, args, e) -> fprintf fmt "fun(%s, %a, [%a], %a)" x pp_typee t (pp_lst_cma pp_arg) args pp_expr e
    | ASTFunRec(x, t, args,e) -> fprintf fmt "funrec(%s, %a, [%a], %a)" x pp_typee t (pp_lst_cma pp_arg) args pp_expr e
    | ASTVar(x, t) -> fprintf fmt "var(%s, %a)" x pp_typee t
    | ASTProc(x, args, cs) -> fprintf fmt "proc(%s, [%a], [%a])" x (pp_lst_cma pp_arg) args pp_cmds cs
    | ASTProcRec(x, args, cs) -> fprintf fmt "procrec(%s, [%a], [%a])" x (pp_lst_cma pp_arg) args pp_cmds cs

let pp_stat fmt s =
  match s with
  ASTEcho e -> fprintf fmt "echo(%a)" pp_expr e
  | ASTSet (x, e) -> fprintf fmt "set(%s, %a)"  x pp expr e
  | ASTIfi (e, cs1, cs2) -> fprintf fmt "ifi(%a, [%a], [%a])" pp_expr e pp_cmds cd1 pp_cmds cd2
  | ASTWhile (e, cs) -> fprintf fmt "while(%a, [%a])" pp_expr e pp_cmds cs
  | ASTCall (f, es) -> fprintf fmt "call(%s, [%a])" f pp_exprs es
let pp_cmd fmt c =
  match c with
  ASTStat s -> fprintf fmt "stat(%a)" pp_stat s
  | ASTDef d -> fprintf fmt "%a" pp_def d

let pp_cmds fmt cmds =
  match cmds with
  [] -> ()
  | c :: [] -> pp_cmd fmt c
  | _ -> fprintf fmt "[%a]" (pp_lst_cma pp_cmd) cmds
  
let pp_prog fmt p =
  fprintf fmt "prog(%a).\n" pp_cmds p




