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
%token IF AND OR BOOL INT VEC VARP ADR ALLOC LEN NTH VSET

// Pour vérifier si %type manquant : menhir --explain lib/Parsing/parser.mly
%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.exprp> exprp
%type <Ast.exprp list> exprsp
%type <Ast.cmds list> cmds
%type <Ast.cmds list> prog
%type <Ast.cmds list> block
%type <Ast.def> def
%type <Ast.stat> stat
%type <Ast.typee> typee
%type <Ast.typee list> types
%type <string * Ast.typee> arg
%type <(string * Ast.typee) list> args
%type <Ast.argp> argp
%type <Ast.argp list> argsp
%type <Ast.lval> lval


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
  CONST IDENT typee expr                  { ASTConst($2, $3, $4) }
| FUN IDENT typee LBRA args RBRA expr     { ASTFun($2, $3, $5, $7) }
| FUN REC IDENT typee LBRA args RBRA expr { ASTFunRec($3, $4, $6, $8) }
| VAR IDENT typee                         { ASTVar($2, $3) }
| PROC IDENT LBRA argsp RBRA block        { ASTProc($2, $4, $6) }
| PROC REC IDENT LBRA argsp RBRA block    { ASTProcRec($3, $5, $7) }
;

typee:
  INT                         { ASTInt }
| BOOL                        { ASTBool }
| LPAR types ARROW typee RPAR { ASTArrow($2, $4) }
| LPAR VEC typee RPAR         { ASTVec($3) }  
;

types:
  typee                   { [$1] }
| typee STAR types        { $1::$3 }
;

args:
  arg                   { [$1] }
| arg COMMA args        { $1::$3 }
;

arg: IDENT COLON typee  { ($1, $3) }
;

argsp:
  argp                  { [$1] }
| argp COMMA argsp      { $1::$3 }
;

argp:
  IDENT COLON typee      { ASTArg($1, $3) }
| VARP IDENT COLON typee { ASTVarp($2, $4) }
;

stat:
  ECHO expr             { ASTEcho($2) }
| SET lval expr         { ASTSet($2, $3) }
| IFi expr block block  { ASTIfi($2, $3, $4) }
| WHILE expr block      { ASTWhile($2, $3) }
| CALL IDENT exprsp     { ASTCall($2, $3) }
;

lval:
  IDENT                   { ASTLId($1) }
| LPAR NTH lval expr RPAR { ASTLNth($3, $4) }
;

exprsp:
  exprp                 { [$1] }
| exprp exprsp          { $1::$2 }
;

exprp:
  expr                  { ASTExpr($1) }
| LPAR ADR IDENT RPAR   { ASTAdr($3) }
;

expr:
  NUM                           { ASTNum($1) }
| IDENT                         { ASTId($1) }
| LPAR IF expr expr expr RPAR   { ASTIf($3, $4, $5) }
| LPAR AND expr expr RPAR       { ASTAnd($3, $4) }
| LPAR OR expr expr RPAR        { ASTOr($3, $4) }
| LPAR expr exprs RPAR          { ASTApp($2, $3) }
| LBRA args RBRA expr           { ASTAbs($2, $4) }
| LPAR ALLOC expr RPAR          { ASTAlloc($3) }
| LPAR LEN expr RPAR            { ASTLen($3) }
| LPAR NTH expr expr RPAR       { ASTNth($3, $4) }
| LPAR VSET expr expr expr RPAR { ASTVSet($3, $4, $5) }
;

exprs :
  expr       { [$1] }
| expr exprs { $1::$2 }
;

