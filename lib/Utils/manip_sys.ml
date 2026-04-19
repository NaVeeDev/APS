open Bos


let testfile_name ver i = Printf.sprintf "examples/APS%d/prog%d.aps" ver i

let testfile_name_1a i = Printf.sprintf "examples/APS1a/prog%d.aps" i

let typ_path = "lib/typing_rules.pl"

let cmd_typ pl_term =
  OS.Cmd.(
    in_string pl_term |>
    run_io (Cmd.(v "swipl" % "-g" % "main" % "-t" %"halt" % typ_path)) |>
    out_string)

let get_prog (fname : string) : Ast.cmds list option =
  let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
    close_in ic;
    Some p
  with 
  | Lexer.Eof -> close_in ic; None
  | Parser.Error -> close_in ic; None
  | e -> close_in ic; raise e

