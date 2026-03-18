%{
(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017                          == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == Analyse syntaxique                                                   == *)
(* ========================================================================== *)

open Ast

%}
  
%token <int> NUM
%token <string> IDENT
%token LPAR RPAR 
%token LBRA RBRA
%token SEMI COLON COMMA STAR ARROW
%token CONST FUN REC VAR PROC
%token ECHO SET IFi WHILE CALL
%token IF AND OR BOOL INT

%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.cmds list> cmds
%type <Ast.cmds list> prog

%start prog

%%
prog: block             { $1 }
;

block: LBRA cmds RBRA   { $2 }
;

cmds:
  stat                  { [ASTStat $1] }
| def SEMI cmds         { ASTDef $1 :: $3 }
| stat SEMI cmds        { ASTStat $1 :: $3 }
;

def:
  CONST IDENT typee expr { ASTConst($2, $3, $4) }
| FUN IDENT typee LBRA args RBRA expr { ASTFun($2, $3, $5, $7) }
| FUN REC IDENT typee LBRA args RBRA expr { ASTFunRec($3, $4, $6, $8) }
| VAR IDENT typee       { ASTVar($2, $3) }
| PROC IDENT LBRA args RBRA block { ASTProc($2, $4, $6) }
| PROC REC IDENT LBRA args RBRA block { ASTProcRec($3, $5, $7) }

typee:
  INT                   { ASTInt }
| BOOL                  { ASTBool }
| LPAR types ARROW typee RPAR { ASTArrow($2, $4) }
;

types:
  typee                   { [$1] }
| typee STAR types        { $1::$3 }
;

args:
  arg                   { [$1] }
| arg COMMA args        { $1::$3 }

arg:
  IDENT COLON typee      { ($1, $3) }

stat:
  ECHO expr             { ASTEcho($2) }
| SET IDENT expr        { ASTSet($2, $3) }
| IFi expr block block  { ASTIfi($2, $3, $4) }
| WHILE expr block      { ASTWhile($2, $3) }
| CALL IDENT exprs      { ASTCall($2, $3) }
;

expr:
  NUM                   { ASTNum($1) }
| IDENT                 { ASTId($1) }
| LPAR IF expr expr expr RPAR { ASTIf($3, $4, $5) }
| LPAR AND expr expr RPAR { ASTAnd($3, $4) }
| LPAR OR expr expr RPAR { ASTOr($3, $4) }
| LPAR expr exprs RPAR  { ASTApp($2, $3) }
| LBRA args RBRA expr   { ASTAbs($2, $4) }
;

exprs :
  expr       { [$1] }
| expr exprs { $1::$2 }
;

