(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)


type expr =
    ASTNum of int
  | ASTId of string
  | ASTIf of expr * expr * expr 
  | ASTAnd of expr * expr
  | ASTOr of expr * expr
  | ASTApp of expr * expr list  
  | ASTAbs of (string * typee) list * expr

and stat =
    ASTEcho of expr
  | ASTSet of string * expr
  | ASTIfi of expr * cmds list * cmds list
  | ASTWhile of expr * cmds list
  | ASTCall of string * expr list

and cmds =
    ASTStat of stat
  | ASTDef of def

and def =
    ASTConst of string * typee * expr
  | ASTFun of string * typee * (string * typee) list * expr
  | ASTFunRec of string * typee * (string * typee) list * expr
  | ASTVar of string * typee
  | ASTProc of string * (string * typee) list * cmds list
  | ASTProcRec of string * (string * typee) list * cmds list

and typee =
    ASTInt
  | ASTBool
  | ASTArrow of typee list * typee





