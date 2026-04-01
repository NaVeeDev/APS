open Ast
open Hashtbl

type values =
  | InZ of int
  | InF of expr * string list * (string, values) Hashtbl.t
  | InFR of expr * string * string list * (string, values) Hashtbl.t
  | InA of int
  | InP of cmds list * string list * (string, values) Hashtbl.t
  | InPR of cmds list * string * string list * (string, values) Hashtbl.t
  

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

let alloc sigma =
  let new_addr = Hashtbl.length sigma in
  Hashtbl.add sigma new_addr (InZ 0);
  (new_addr, sigma)

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
let rec eval_prog p : unit =
  let env0 = init_env () in
  let mem0 = Hashtbl.create 10 in
  let (_, _, stack) = eval_block env0 mem0 [] p in
  List.iter (fun n -> print_int n; print_newline ()) (List.rev stack)


and eval_block env mem stack cmds  =
  match cmds with
  | [] -> (env, mem, stack)
  | c :: cs -> 
      let (new_env,_, new_stack) = eval_cmd env mem stack c in
      eval_block new_env mem new_stack cs

and eval_cmd env mem stack c =
  match c with 
  | ASTStat s ->  eval_stat env mem stack s
  | ASTDef d -> eval_def env mem stack d

and eval_stat env mem stack s =
  match s with
  | ASTSet(x,e) -> (match Hashtbl.find env x with
                    | InA a -> let v = eval_expr env mem e in
                                Hashtbl.replace mem a v; (env, mem, stack)
                    | _ -> failwith ("Variable " ^ x ^ " n'est pas assignable"))
  | ASTIfi (e, then_block, else_block) -> (match eval_expr env mem e with
                                           | InZ 1 -> eval_block env mem stack then_block
                                           | InZ 0 -> eval_block env mem stack else_block
                                           | _ -> failwith "Condition d'un if doit être 0 ou 1 (IFi)")
  | ASTWhile (e, body) -> (match eval_expr env mem e with
                           | InZ 0 -> (env, mem, stack)
                           | InZ 1 -> let (new_env, new_mem, new_stack) = eval_block env mem stack body in
                                      eval_stat new_env new_mem new_stack (ASTWhile (e, body))
                           | _ -> failwith "Condition d'un while doit être 0 ou 1 (While)")
  | ASTCall (f, args) -> (match Hashtbl.find env f with
                          | InP (cmds, params, env') -> let values = List.map (fun arg -> eval_expr env mem arg) args in
                                                        let new_env = Hashtbl.copy env' in
                                                        List.iter2 (fun param value -> Hashtbl.add new_env param value) params values;
                                                        eval_block new_env mem stack cmds
                          | InPR (cmds, f', params, env') -> let values = List.map (fun arg -> eval_expr env mem arg) args in
                                                             let new_env = Hashtbl.copy env' in
                                                             List.iter2 (fun param value -> Hashtbl.add new_env param value) params values;
                                                             Hashtbl.add new_env f' (InPR(cmds, f', params, env'));
                                                             eval_block new_env mem stack cmds
                          | _ -> failwith ("Proc " ^ f ^ " n'est pas défini"))
  | ASTEcho e -> match eval_expr env mem e with
                   | InZ n -> (env, mem, n :: stack)
                   | _ -> failwith "Echo d'une valeur non entière"

and eval_def env (mem : (int, values) t) stack d =
  match d with
  | ASTConst (x, _, e) -> Hashtbl.add env x (eval_expr env mem e); (env, mem, stack)
  | ASTFun (f, _, args, e) -> Hashtbl.add env f (InF(e, List.map (fst) args, (Hashtbl.copy env))); (env, mem, stack)
  | ASTFunRec (fr, _ , args, e) -> Hashtbl.add env fr (InFR(e, fr, List.map (fst) args, (Hashtbl.copy env))); (env, mem, stack)
  | ASTVar (x,_) -> let (a, sigma') = alloc mem in 
                    Hashtbl.add env x (InA (a));
                    (env, sigma', stack)
  | ASTProc (p, args, cmds) -> Hashtbl.add env p (InP(cmds, List.map (fst) args, (Hashtbl.copy env))); (env, mem, stack)
  | ASTProcRec (pr, args, cmds) -> Hashtbl.add env pr (InPR(cmds, pr, List.map (fst) args, (Hashtbl.copy env))); (env, mem, stack)


and eval_expr env mem e =
  match e with
  (* (NUM) *)
  | ASTNum n -> InZ n

  (* (ID) *)
  | ASTId x -> (match (try find env x with Not_found -> failwith ("Variable non définie : " ^ x)) with
                | InA a -> let v = Hashtbl.find mem a in
                            v
                | v -> v)

  (* (AND) *)
  | ASTAnd (e1, e2) -> (match eval_expr env mem e1 with

                          (* (AND1) *)
                          | InZ 1 -> eval_expr env mem e2

                          (* (AND0) *)
                          | InZ 0 -> InZ 0

                          | _ -> failwith "Argument d'un and doit être 0 ou 1 (AND)")

  (* (OR) *)
  | ASTOr (e1, e2) -> (match eval_expr env mem e1 with

                          (* (OR1) *)
                          | InZ 1 -> InZ 1

                          (* (OR0) *)
                          | InZ 0 -> eval_expr env mem e2

                          | _ -> failwith "Argument d'un or doit être 0 ou 1 (OR)")


  (* (IF) *)
  | ASTIf (e1, e2, e3) -> (match eval_expr env mem e1 with

                          (* (IF1) *)
                          | InZ 1 -> eval_expr env mem e2

                          (* (IF0) *)
                          | InZ 0 -> eval_expr env mem e3
                          
                          | _ -> failwith "Condition d'un if doit être 0 ou 1 (IF)")

  (* (ABS) *)
  | ASTAbs (args, e) -> InF(e, List.map (fst) args, env)


  | ASTApp (e, el) -> (match e with
                         (* (PRIM1) *) 
                         | ASTId x when is_prim1 x -> (match el with
                                                         | [e'] -> prim1 x (eval_expr env mem e')
                                                         | _ -> failwith (x ^ " attend 1 argument"))
                         
                         (* (PRIM2) *)
                         | ASTId x when is_prim2 x -> (match el with
                                                         | [e1; e2] -> prim2 x (eval_expr env mem e1) (eval_expr env mem e2)
                                                         | _ -> failwith (x ^ " attend 2 arguments"))
                         
                         | _ -> (match eval_expr env mem e with

                                   (* (APP) *)
                                   | InF (e', args, env') -> let values = List.map (fun ex -> eval_expr env mem ex) el in
                                                             let new_env = Hashtbl.copy env' in
                                                             List.iter2 (fun arg value -> Hashtbl.add new_env arg value) args values;
                                                             eval_expr new_env mem e'

                                   (* (APPR) *)
                                   | InFR(e', f, args, env') -> let values = List.map (fun ex -> eval_expr env mem ex) el in
                                                                let new_env = Hashtbl.copy env' in
                                                                List.iter2 (fun arg value -> Hashtbl.add new_env arg value) args values;
                                                                Hashtbl.add new_env f (InFR(e', f, args, env'));
                                                                eval_expr new_env mem e'
                                     
                                   | _ -> failwith "Expression appliquée n'est pas une fonction (APP)"))


and eval_typee t =
  match t with
  | ASTInt -> "int"
  | ASTBool -> "bool"
  | ASTArrow (t1, t2) -> List.fold_left (fun acc t -> acc ^ " -> " ^ eval_typee t) "" t1 ^ " -> " ^ eval_typee t2