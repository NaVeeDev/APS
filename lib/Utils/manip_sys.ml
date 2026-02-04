open Bos


let testfile_name ver i = Printf.sprintf "examples/APS%d/prog%d.aps" ver i

let typ_path = "lib/typing_rules.pl"

let cmd_typ pl_term =
  OS.Cmd.(
    in_string pl_term |>
    run_io (Cmd.(v "swipl" % "-g" % "main" % "-t" %"halt" % typ_path)) |>
    out_string)

let get_prog (fname : string) : Ast.cmd list =
  let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
      p
  with Lexer.Eof ->
    exit 0
