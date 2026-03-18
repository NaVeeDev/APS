open Ast
open Hashtbl

type values =
  | InZ of int
  | InF of expr * string list * (string, values) Hashtbl.t
  | InFR of expr * string * string list * (string, values) Hashtbl.t

(* ==== PRIMITIVES ==== *)
let prim1 name arg =
  match name with
  | "not" -> (match arg with
              | InZ 0 -> InZ 1
              | InZ 1 -> InZ 0
              | _ -> failwith "Argument de not doit être 0 ou 1")
  | _ -> failwith ("Primitive inconnue : " ^ name)


let prim2 name arg1 arg2 =
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
              | (InZ _, InZ 0) -> failwith "Division par zero"
              | (InZ n1, InZ n2) -> InZ (n1 / n2)
              | _ -> failwith "Arguments de div doivent être des entiers")
  | _ -> failwith ("Primitive inconnue : " ^ name)


let is_prim1 = function
  | "not" -> true
  | _ -> false

let is_prim2 = function
  | "eq" | "lt" | "add" | "sub" | "mul" | "div" -> true
  | _ -> false

(* ==== INITIALISATION DE L'ENV DE BASE ==== *)
let init_env () =
  let env = create 10 in

  Hashtbl.add env "not" (InF (ASTNum 0, ["x"], env));
  Hashtbl.add env "eq" (InF (ASTNum 0, ["x"; "y"], env));
  Hashtbl.add env "lt" (InF (ASTNum 0, ["x"; "y"], env));
  Hashtbl.add env "add" (InF (ASTNum 0, ["x"; "y"], env));
  Hashtbl.add env "sub" (InF (ASTNum 0, ["x"; "y"], env));
  Hashtbl.add env "mul" (InF (ASTNum 0, ["x"; "y"], env));
  Hashtbl.add env "div" (InF (ASTNum 0, ["x"; "y"], env));

  (* (TRUE) *)
  Hashtbl.add env "true" (InZ 1);

  (* (FALSE) *)
  Hashtbl.add env "false" (InZ 0);
  env

(* ==== FONCTIONS D'EVAL ==== *)
let rec eval_prog p  =
  let env0 = init_env () in
  let (_, stack) = eval_cmds p env0 [] in
  List.iter (fun n -> print_int n; print_newline ()) stack
  

and eval_cmds cmds env stack =
  match cmds with
  | [] -> (env, stack)
  | c :: cs -> 
      let (new_env, new_stack) = eval_cmd c env stack in
      eval_cmds cs new_env new_stack

and eval_cmd c env stack =
  match c with 
  | ASTStat s -> eval_stat s env stack
  | ASTDef d -> (eval_def d env, stack)

and eval_stat s env stack =
  match s with
  | ASTEcho e -> match eval_expr e env with
                   | InZ n -> (env, n :: stack)
                   | _ -> failwith "Echo d'une valeur non entière"

and eval_def d env =
  match d with
  | ASTConst (x, _, e) -> Hashtbl.add env x (eval_expr e (Hashtbl.copy env)); env
  | ASTFun (f, _, args, e) -> Hashtbl.add env f (InF(e, List.map (fst) args, (Hashtbl.copy env))); env
  | ASTFunRec (fr, _ , args, e) -> Hashtbl.add env fr (InFR(e, fr, List.map (fst) args, (Hashtbl.copy env))); env

and eval_expr e env =
  match e with

  (* (NUM) *)
  | ASTNum n -> InZ n

  (* (ID) *)
  | ASTId x -> (try find env x with Not_found -> failwith ("Variable non définie : " ^ x))


  (* (AND) *)
  | ASTAnd (e1, e2) -> (match eval_expr e1 env with

                          (* (AND1) *)
                          | InZ 1 -> eval_expr e2 env

                          (* (AND0) *)
                          | InZ 0 -> InZ 0

                          | _ -> failwith "Argument d'un and doit être 0 ou 1 (AND)")

  (* (OR) *)
  | ASTOr (e1, e2) -> (match eval_expr e1 env with

                          (* (OR1) *)
                          | InZ 1 -> InZ 1

                          (* (OR0) *)
                          | InZ 0 -> eval_expr e2 env

                          | _ -> failwith "Argument d'un or doit être 0 ou 1 (OR)")


  (* (IF) *)
  | ASTIf (e1, e2, e3) -> (match eval_expr e1 env with

                          (* (IF1) *)
                          | InZ 1 -> eval_expr e2 env

                          (* (IF0) *)
                          | InZ 0 -> eval_expr e3 env
                          
                          | _ -> failwith "Condition d'un if doit être 0 ou 1 (IF)")

  (* (ABS) *)
  | ASTAbs (args, e) -> InF(e, List.map (fst) args, env)


  | ASTApp (e, el) -> (match e with
                         (* (PRIM1) *) 
                         | ASTId x when is_prim1 x -> (match el with
                                                         | [e'] -> prim1 x (eval_expr e' env)
                                                         | _ -> failwith (x ^ " attend 1 argument"))
                         
                         (* (PRIM2) *)
                         | ASTId x when is_prim2 x -> (match el with
                                                         | [e1; e2] -> prim2 x (eval_expr e1 env) (eval_expr e2 env)
                                                         | _ -> failwith (x ^ " attend 2 arguments"))
                         
                         | _ -> (match eval_expr e env with

                                   (* (APP) *)
                                   | InF (e', args, env') -> let values = List.map (fun ex -> eval_expr ex env) el in
                                                             let new_env = Hashtbl.copy env' in
                                                             List.iter2 (fun arg value -> Hashtbl.add new_env arg value) args values;
                                                             eval_expr e' new_env

                                   (* (APPR) *)
                                   | InFR(e', f, args, env') -> let values = List.map (fun ex -> eval_expr ex env) el in
                                                                let new_env = Hashtbl.copy env' in
                                                                List.iter2 (fun arg value -> Hashtbl.add new_env arg value) args values;
                                                                Hashtbl.add new_env f (InFR(e', f, args, env'));
                                                                eval_expr e' new_env
                                     
                                   | _ -> failwith "Expression appliquée n'est pas une fonction (APP)"))


and eval_typee t =
  match t with
  | ASTInt -> "int"
  | ASTBool -> "bool"
  | ASTArrow (t1, t2) -> List.fold_left (fun acc t -> acc ^ " -> " ^ eval_typee t) "" t1 ^ " -> " ^ eval_typee t2