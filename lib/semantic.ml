open Ast
open Hashtbl

(* rho *)
type envT = (string, values) Hashtbl.t

(* sigma *)
and memT = (int, values) Hashtbl.t

(* omega *)
and stackT = int list

and values =
  | InZ of int
  | InF of expr * string list * envT
  | InFR of expr * string * string list * envT
  | InA of int
  | InP of cmds list * string list * envT
  | InPR of cmds list * string list * envT
  | InB of int * int

(* ==== PRIMITIVES ==== *)
(* APS1a & APS2 *)
let next_addr = ref 0

(* APS1a *)
let alloc (sigma : memT) : int * memT =
  let new_addr = !next_addr in
  next_addr := new_addr + 1;
  Hashtbl.add sigma new_addr (InZ 0);
  (new_addr, sigma)

(* APS2 *)
let allocn (sigma : memT) (n : int) : int * memT =
  let new_addr = !next_addr in
  next_addr := new_addr + n;
  for i = 0 to n-1 do
    Hashtbl.add sigma (new_addr+i) (InZ 0);
  done;
  (new_addr, sigma)

(* APS0 *)
let prim1 (name : string) (arg : values) (mem : memT) : values * memT =
  match name with
  | "not" -> (match arg with
              | InZ 0 -> (InZ 1, mem)
              | InZ 1 -> (InZ 0, mem)
              | _ -> failwith "Argument de not doit être 0 ou 1")
  | _ -> failwith ("Primitive inconnue : " ^ name)

(* APS0 *)
let prim2 (name : string) (arg1 : values) (arg2 : values) (mem : memT) : values * memT =
  match name with
  | "eq" -> (match (arg1, arg2) with
              | (InZ n1, InZ n2) -> ((if n1 = n2 then InZ 1 else InZ 0), mem)
              | _ -> failwith "Arguments de eq doivent être des entiers")
  | "lt" -> (match (arg1, arg2) with
              | (InZ n1, InZ n2) -> ((if n1 < n2 then InZ 1 else InZ 0), mem)
              | _ -> failwith "Arguments de lt doivent être des entiers")
  | "add" -> (match (arg1, arg2) with
              | (InZ n1, InZ n2) -> (InZ (n1 + n2), mem)
              | _ -> failwith "Arguments de add doivent être des entiers")
  | "sub" -> (match (arg1, arg2) with
              | (InZ n1, InZ n2) -> (InZ (n1 - n2), mem)
              | _ -> failwith "Arguments de sub doivent être des entiers")
  | "mul" -> (match (arg1, arg2) with
              | (InZ n1, InZ n2) -> (InZ (n1 * n2), mem)
              | _ -> failwith "Arguments de mul doivent être des entiers")
  | "div" -> (match (arg1, arg2) with
              | (InZ _, InZ 0) -> failwith "Division par zero"
              | (InZ n1, InZ n2) -> (InZ (n1 / n2), mem)
              | _ -> failwith "Arguments de div doivent être des entiers")
  | _ -> failwith ("Primitive inconnue : " ^ name)

(* APS0 *)
let is_prim1 (name : string) : bool =
  match name with
  | "not" -> true
  | _ -> false

(* APS0 *)
let is_prim2 (name : string) : bool =
  match name with
  | "eq" | "lt" | "add" | "sub" | "mul" | "div" -> true
  | _ -> false


(* ==== INITIALISATION DE L'ENV DE BASE ==== *)

(* APS0 *)
let init_env () : envT =
  let env = create 10 in

  (* PRIM *)
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

(* (PROG) *)
let rec eval_prog (p : cmds list) : unit =
  let env0 = init_env () in
  let mem0 = Hashtbl.create 10 in
  let (_, _, stack) = eval_block env0 mem0 [] p in
  List.iter (fun n -> print_int n; print_newline ()) (List.rev stack)
  

(* APS1 *)
(* (BLOCK) *)
and eval_block (env : envT) (mem : memT) (stack : int list) (cmds : cmds list) : envT * memT * int list =
  match cmds with
  | [] -> (env, mem, stack)
  | c :: cs -> 
      let (new_env,_, new_stack) = eval_cmd env mem stack c in
      eval_block new_env mem new_stack cs


(* APS0 *)
(* (CMDS) *)
and eval_cmd (env : envT) (mem : memT) (stack : int list) (c : cmds) : envT * memT * int list =
  match c with 
  | ASTStat s ->  eval_stat env mem stack s
  | ASTDef d -> eval_def env mem stack d


(* APS0, APS1, APS2 *)
(* (STAT) *)
and eval_stat (env : envT) (mem : memT) (stack : int list) (s : stat) : envT * memT * int list =
  match s with

  (* (SET) *)
  | ASTSet(x,e) -> (let (a, mem') = eval_lval env mem x in
                    let (v, mem_) = eval_expr env mem' e in
                    Hashtbl.replace mem' a v;
                    (env, mem_, stack))

  (* (IF) *)
  | ASTIfi (e, then_block, else_block) -> let (e', mem') = eval_expr env mem e in (match e' with

                                           (* (IF1) *)
                                           | InZ 1 -> eval_block env mem' stack then_block

                                           (* (IF0) *)
                                           | InZ 0 -> eval_block env mem' stack else_block

                                           | _ -> failwith "Condition d'un if doit être 0 ou 1 (IFi)")

  (* (LOOP) *)
  | ASTWhile (e, body) -> let (e', mem') = eval_expr env mem e in (match e' with

                           (* (LOOP0) *)
                           | InZ 0 -> (env, mem', stack)

                            (* (LOOP1) *)
                           | InZ 1 -> let (new_env, new_mem, new_stack) = eval_block env mem' stack body in
                                      eval_stat new_env new_mem new_stack (ASTWhile (e, body))
                          
                           | _ -> failwith "Condition d'un while doit être 0 ou 1 (While)")

  | ASTCall (f, args) -> (try (match Hashtbl.find env f with

                          (* (CALL) *)
                          | InP (cmds, params, env') -> let values = List.map (fun arg -> eval_exprp env mem arg) args in
                                                        let new_env = Hashtbl.copy env' in
                                                        List.iter2 (fun param value -> Hashtbl.add new_env param value) params values;
                                                        let (_, new_mem, new_stack) = eval_block new_env mem stack cmds in
                                                        (env, new_mem, new_stack)

                          (* (CALLR) *)
                          | InPR (cmds, params, env') -> let values = List.map (fun arg -> eval_exprp env mem arg) args in
                                                         let new_env = Hashtbl.copy env' in
                                                         List.iter2 (fun param value -> Hashtbl.add new_env param value) params values;
                                                         Hashtbl.add new_env f (InPR(cmds, params, env'));
                                                         let (_, new_mem, new_stack) = eval_block new_env mem stack cmds in
                                                         (env, new_mem, new_stack)

                          | _ -> failwith (f ^ " n'est pas une procédure")) with Not_found -> failwith (f ^" n'est pas défini"))

  (* (ECHO) *)
  | ASTEcho e -> let (e', mem') = eval_expr env mem e in  (match e' with
                   | InZ n -> (env, mem', n :: stack)
                   | _ -> failwith "Echo d'une valeur non entière")


(* APS2 *)
(* (LVAL) *)
and eval_lval (env : envT) (mem : memT) (lv : lval) : int * memT =
  match lv with

  (* (LID) *)
  | ASTLId x -> (try (match Hashtbl.find env x with
                    | InA a -> (a, mem)
                    | _ -> failwith ("Variable " ^ x ^ " n'est pas une adresse")) 
                with Not_found -> failwith ("Variable non définie : " ^ x))

  (* (LNTH) *)
  | ASTLNth (lv, e) -> let (a, mem') = eval_lval env mem lv in
                      try (match Hashtbl.find mem' a with
                          | InB(ad, size) -> let (v, mem_) = eval_expr env mem' e in
                                            (match v with
                                            | InZ i -> if i < 0 || i >= size then failwith "Index out of bounds";
                                                       (ad + i, mem_)
                                            | _ -> failwith "Index doit être un entier")
                          | _ -> failwith "Lvalue doit être de type vec(t)")
                      with Not_found -> failwith ("Adresse mémoire introuvable : " ^ string_of_int a)


(* APS0, AP1, APS2 *)
(* (DEF) *)
and eval_def (env : envT) (mem : memT) (stack : int list) (d : def) : envT * memT * int list =
  match d with

  (* (CONST) *)
  | ASTConst (x, _, e) -> let (e', mem') = eval_expr env mem e in Hashtbl.add env x e'; (env, mem', stack)

  (* (FUN) *)
  | ASTFun (f, _, args, e) -> Hashtbl.add env f (InF(e, List.map (fst) args, (Hashtbl.copy env))); (env, mem, stack)

  (* (FUNREC) *)
  | ASTFunRec (fr, _ , args, e) -> Hashtbl.add env fr (InFR(e, fr, List.map (fst) args, (Hashtbl.copy env))); (env, mem, stack)

  (* (VAR) *)
  | ASTVar (x,_) -> let (a, sigma') = alloc mem in 
                    Hashtbl.add env x (InA a);
                    (env, sigma', stack)
                
  (* (PROC) *)
  | ASTProc (p, args, cmds) -> let params = List.map (function ASTArg(x,_) -> x | ASTVarp(x,_) -> x) args in Hashtbl.add env p (InP(cmds, params, (Hashtbl.copy env))); (env, mem, stack)

  (* (PROCREC) *)
  | ASTProcRec (pr, args, cmds) -> let params = List.map (function ASTArg(x,_) -> x | ASTVarp(x,_) -> x) args in Hashtbl.add env pr (InPR(cmds, params, (Hashtbl.copy env))); (env, mem, stack)


(* APS1a *)
(* (EXPRP) *)
and eval_exprp (env : envT) (mem : memT) (e : exprp) : values =
  match e with

  (* (VAL) *)
  | ASTExpr e' -> let (ee, _) = eval_expr env mem e' in ee

  (* (REF) *)
  | ASTAdr x -> try (match Hashtbl.find env x with
                | InA a -> InA a
                | _ -> failwith ("Variable " ^ x ^ " n'est pas une adresse")) with Not_found -> failwith ("Variable non définie : " ^ x)

(* APS0, APS2 *)
(* (EXPR) *)
and eval_expr (env : envT) (mem : memT) (e : expr) : values * memT =
  match e with
  (* (NUM) *)
  | ASTNum n -> (InZ n, mem)

  (* (ID) *)
  | ASTId x -> ((match (try Hashtbl.find env x with Not_found -> failwith ("Variable non définie : " ^ x)) with
                
                (* (ID1) *)
                | InA a -> (try let v = Hashtbl.find mem a in v
                            with Not_found -> failwith ("Adresse mémoire introuvable : " ^ string_of_int a))
                
                (* (ID2) *)
                | v -> v), mem)

  (* (AND) *)
  | ASTAnd (e1, e2) -> let (e, mem') = eval_expr env mem e1 in (match e with

                          (* (AND1) *)
                          | InZ 1 -> eval_expr env mem' e2

                          (* (AND0) *)
                          | InZ 0 -> (InZ 0, mem')

                          | _ -> failwith "Argument d'un and doit être 0 ou 1 (AND)")

  (* (OR) *)
  | ASTOr (e1, e2) -> let (e, mem') = eval_expr env mem e1 in (match e with

                          (* (OR1) *)
                          | InZ 1 -> (InZ 1, mem')

                          (* (OR0) *)
                          | InZ 0 -> eval_expr env mem' e2

                          | _ -> failwith "Argument d'un or doit être 0 ou 1 (OR)")


  (* (IF) *)
  | ASTIf (e1, e2, e3) -> let (e, mem') = eval_expr env mem e1 in (match e with

                          (* (IF1) *)
                          | InZ 1 -> eval_expr env mem' e2

                          (* (IF0) *)
                          | InZ 0 -> eval_expr env mem' e3
                          
                          | _ -> failwith "Condition d'un if doit être 0 ou 1 (IF)")

  (* (ABS) *)
  | ASTAbs (args, e) -> (InF(e, List.map (fst) args, env), mem)


  | ASTApp (e, el) -> (match e with
                         (* (PRIM1) *) 
                         | ASTId x when is_prim1 x -> (match el with
                                                         | [e'] -> let (e', mem') = eval_expr env mem e' in prim1 x e' mem'
                                                         | _ -> failwith (x ^ " attend 1 argument"))
                         
                         (* (PRIM2) *)
                         | ASTId x when is_prim2 x -> (match el with
                                                         | [e1; e2] -> let (e1', mem1) = eval_expr env mem e1 in
                                                                       let (e2', mem2) = eval_expr env mem1 e2 in
                                                                       prim2 x e1' e2' mem2
                                                         | _ -> failwith (x ^ " attend 2 arguments"))

                         | _ -> let (ee, memm) = eval_expr env mem e in (match ee with

                                   (* (APP) *)
                                   | InF (e', args, env') -> let (mem2, values) = List.fold_left_map (fun acc ex -> let (v, mem') = eval_expr env acc ex in (mem', v)) memm el in
                                                             let new_env = Hashtbl.copy env' in
                                                             List.iter2 (fun arg value -> Hashtbl.add new_env arg value) args values;
                                                             eval_expr new_env mem2 e'

                                   (* (APPR) *)
                                   | InFR(e', f, args, env') -> let (mem2, values) = List.fold_left_map (fun acc ex -> let (v, mem') = eval_expr env acc ex in (mem', v)) memm el in
                                                                let new_env = Hashtbl.copy env' in
                                                                List.iter2 (fun arg value -> Hashtbl.add new_env arg value) args values;
                                                                Hashtbl.add new_env f (InFR(e', f, args, env'));
                                                                eval_expr new_env mem2 e'
                                     
                                   | _ -> failwith "Expression appliquée n'est pas une fonction (APP)"))
  (* (ALLOC) *)
  | ASTAlloc e -> let (e', mem') = eval_expr env mem e in (match e' with
                      | InZ n -> let (a, mem'') = allocn mem' n in (InB(a, n), mem'')
                      | _ -> failwith "Argument de alloc doit être un entier")

  (* (LEN) *)
  | ASTLen e -> let (e', mem') = eval_expr env mem e in (match e' with
                      | InB (_, size) -> (InZ size, mem')
                      | _ -> failwith "Argument de len doit être un vec")

  (* (NTH) *)
  | ASTNth (e1, e2) -> let (e1', mem1) = eval_expr env mem e1 in
                       let (e2', mem2) = eval_expr env mem1 e2 in
                       (match (e1', e2') with
                        | InB (a, size), InZ n -> if n < 0 || n >= size then failwith "nth: index out of bounds"
                                                  else let addr = a + n in (Hashtbl.find mem2 addr, mem2)
                        | _ -> failwith "Arguments de nth doivent être de type vec(t) et entiers")

  (* (VSET) *)
  | ASTVSet (e1, e2, e3) -> let (e1', mem1) = eval_expr env mem e1 in
                            let (e2', mem2) = eval_expr env mem1 e2 in
                            let (e3', mem3) = eval_expr env mem2 e3 in
                            (match (e1', e2', e3') with
                              | InB(a, size), InZ n, v -> if n < 0 || n >= size then failwith "vset: index out of bounds"
                                                          else let addr = a + n in Hashtbl.replace mem3 addr v; (InB(a, size), mem3)
                              | _ -> failwith "Arguments de vset doivent être de type vec(t), entier et t")
