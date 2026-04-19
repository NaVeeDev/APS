(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* ==  Lexique                                                             == *)
(* ========================================================================== *)

{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof

}
rule token = parse
    [' ' '\t' '\n']       { token lexbuf }     (* skip blanks *)
  | '['              { LBRA }
  | ']'              { RBRA }
  | '('              { LPAR }
  | ')'              { RPAR }
  | ';'              { SEMI }
  | ':'              { COLON }
  | ','              { COMMA }
  | '*'              { STAR }
  | "->"             { ARROW }
  | "CONST"          { CONST }
  | "FUN"            { FUN } 
  | "REC"            { REC }
  | "VAR"            { VAR }
  | "PROC"           { PROC }
  | "ECHO"           { ECHO }
  | "SET"            { SET }
  | "IF"             { IFi }
  | "WHILE"          { WHILE }
  | "CALL"           { CALL }
  | "if"             { IF }
  | "and"            { AND }
  | "or"             { OR }
  | "bool"           { BOOL }
  | "int"            { INT }
  | "vec"            { VEC }
  | "var"            { VARP }
  | "adr"            { ADR }
  | "alloc"          { ALLOC }
  | "len"            { LEN }
  | "nth"            { NTH }
  | "vset"           { VSET }
  | "RETURN"         { RETURN }
  | ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
  | ['a'-'z']['a'-'z''A'-'Z''0'-'9''_']* as lxm { IDENT(lxm) }
  | eof              { raise Eof }
