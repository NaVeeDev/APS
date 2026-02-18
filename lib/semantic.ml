open Ast
open Hashtbl

type values =
  | InZ of int
  | InF of string * string list * (string, values) Hashtbl.t
  | InFR of string * string * string list * (string, values) Hashtbl.t

(* ==== PRIMITIVES ==== *)
let pi1 name arg =
  match name with
  | "not" -> (match arg with
              | InZ 0 -> InZ 1
              | InZ 1 -> InZ 0
              | _ -> failwith "Argument de not doit être 0 ou 1")
  | _ -> failwith ("Primitive inconnue : " ^ name)


let pi2 name arg1 arg2 =
  match name with
  | "eq" -> (match (arg1, arg2) with
              | (InZ n1, InZ n2) -> if n1 = n2 then InZ 1 else InZ 0
              | _ -> failwith "Arguments de eq doivent être des entiers")
  | "lt" -> (match (arg1, arg2) with
              | (InZ n1, InZ n2) -> if n1 < n2 then InZ 1 else InZ 0
              | _ -> failwith "Arguments de lt doivent être des entiers")
  | "add" -> (match (arg1, arg2) with
              | (InZ n1, InZ n2) -> InZ (n1 + n2)
              | _ -> failwith "Arguments de add doivent être des entiers")
  | "sub" -> (match (arg1, arg2) with
              | (InZ n1, InZ n2) -> InZ (n1 - n2)
              | _ -> failwith "Arguments de sub doivent être des entiers")
  | "mul" -> (match (arg1, arg2) with
              | (InZ n1, InZ n2) -> InZ (n1 * n2)
              | _ -> failwith "Arguments de mul doivent être des entiers")
  | "div" -> (match (arg1, arg2) with
              | (InZ _, InZ 0) -> failwith "Division par zéro"
              | (InZ n1, InZ n2) -> InZ (n1 / n2)
              | _ -> failwith "Arguments de div doivent être des entiers")
  | _ -> failwith ("Primitive inconnue : " ^ name)

(* ==== INITIALISATION DE L'ENV DE BASE ==== *)
let init_env () =
  let env = create 10 in
  let dummy = InF("", ["x"; "y"], Hashtbl.create 0) in
  Hashtbl.add env "not" (InF("", ["x"], Hashtbl.create 0)); 
  Hashtbl.add env "eq" dummy;
  Hashtbl.add env "lt" dummy;
  Hashtbl.add env "add" dummy;
  Hashtbl.add env "sub" dummy;
  Hashtbl.add env "mul" dummy;
  Hashtbl.add env "div" dummy;
  Hashtbl.add env "true" (InZ 1);
  Hashtbl.add env "false" (InZ 0);
  env

let rec eval_prog p  =
  let env0 = init_env () in
  eval_cmds p env0

and eval_cmds cmds env =
  match cmds with
  | [] -> ()
  | c :: cs -> eval_cmds cs (eval_cmd c env)

and eval_cmd c env =
  match c with 
  | ASTStat s -> eval_stat s env
  | ASTDef d -> eval_def d env

and eval_stat s env =
  match s with
  | ASTEcho e -> let v = eval_expr e env in print_int v; print_newline (); env

and eval_def d env =
  match d with
  | ASTConst (x, _, e) -> Hashtbl.add env x (eval_expr e env); env
  | ASTFun (_, _, _, _) -> failwith "TODO: eval_def - fonction non récursive" (* A COMPLETER *)
  | ASTFunRec (_, _, _, _) -> failwith "TODO: eval_def - fonction récursive" (* A COMPLETER *)

and eval_expr e env =
  match e with
  | ASTNum n -> InZ n
  | ASTId x -> (try find env x with Not_found -> failwith ("Variable non définie : " ^ x))
  | ASTIf (e1, e2, e3) -> (match eval_expr e1 env with
                          | InZ 0 -> eval_expr e3 env
                          | InZ _ -> eval_expr e2 env
                          | _ -> failwith "Condition d'un if doit être 0 ou 1")
  | ASTAnd (e1, e2) -> (match eval_expr e1 env with
                          | InZ 0 -> InZ 0
                          | InZ 1 -> eval_expr e2 env
                          | _ -> failwith "Argument d'un and doit être 0 ou 1")
  | ASTOr (e1, e2) -> let v1 = eval_expr e1 env in if v1 <> 0 then InZ 1 else eval_expr e2 env
  | ASTApp (e, _) -> let _ = (try find env (match e with ASTId f -> f | _ -> failwith "Application d'une expression non fonction") with Not_found -> failwith "Fonction non définie") in
                      failwith "TODO: eval_expr - application de fonction" (* A COMPLETER *)
  | ASTAbs (_, _) -> failwith "TODO: eval_expr - fonction anonyme" (* A COMPLETER *)

and eval_typee t _ =
  match t with
  | ASTInt -> "int"
  | ASTBool -> "bool"
  | ASTArrow (_, _) -> failwith "TODO: eval_typee - type de fonction" (* A COMPLETER *)