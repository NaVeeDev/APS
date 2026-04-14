trace. 

main :- read(user_input, X), check(X, void).

check(Expr, T) :- env0(Env), (type_check(Env, Expr, T) -> write("OK\n"); write("KO\n")).

% ===== CONTEXTE INITIAL =====

% ENV DE BASE = gamma0
env0([
    (true, bool),
    (false, bool),
    (not, arrow(bool, bool)),
    (eq, arrow(star(int, int), bool)),
    (lt, arrow(star(int, int), bool)),
    (add, arrow(star(int, int), int)),
    (sub, arrow(star(int, int), int)),
    (mul, arrow(star(int, int), int)),
    (div, arrow(star(int, int), int))
]).

% ===== PROGRAMMES =====
% prog
type_check(Env, prog(Cs), void) :- type_check(Env, Cs, void).

% ===== BLOCS =====
% type_check(Env, [[Cs]], void) :- type_check(Env, [Cs], void).


% ===== SUITE DE COMMANDES =====
% stats
type_check(Env, [stat(S)|Cs], void) :- type_check(Env, S, void), type_check(Env, Cs, void).

% defs
type_check(Env, [D|Cs], void) :- type_check(Env, D, Envv), type_check(Envv, Cs, void).

% end
type_check(_, [], void).

% ===== DEFINITIONS =====
% const
type_check(Env, const(X, T, E), [(X,T)|Env]) :- type_check(Env, E, T).

% fun
type_check(Env, fun(X, T, Args, E), [(X, ArrowType)|Env]) :-
    add_args_to_env(Args, Env, NewEnv),
    type_check(NewEnv, E, T),
    args_to_arrowtype(Args, T, ArrowType).

% funrec
type_check(Env, funrec(X, T, Args, E), [(X, ArrowType)|Env]) :-
    args_to_arrowtype(Args, T, ArrowType),
    add_args_to_env(Args, [(X, ArrowType)|Env], NewEnv),
    type_check(NewEnv, E, T).

% var
type_check(Env, var(X, T), [(X, ref(T))|Env]).

% proc
type_check(Env, proc(X, Args, Cs), [(X, ProcType)|Env]) :- 
    argsp_to_arrowtype(Args, ProcType),
    add_argsp_to_env(Args, Env, NewEnv),
    type_check(NewEnv, Cs, void).

% procrec
type_check(Env, procrec(X, Args, Cs), [(X, ProcType)|Env]) :- 
    argsp_to_arrowtype(Args, ProcType),
    add_argsp_to_env(Args, [(X, ProcType)|Env], NewEnv),
    type_check(NewEnv, Cs, void).


% ===== INSTRUCTIONS =====
% echo
type_check(Env, echo(X), void) :- type_check(Env, X, int).

% set
type_check(Env, set(LV, E), void) :-
    type_check_lval(Env, LV, T),
    type_check(Env, E, T).

% ifi
type_check(Env, ifi(E, Cs1, Cs2), void) :- 
    type_check(Env, E, bool),
    type_check(Env, Cs1, void),
    type_check(Env, Cs2, void).

% while
type_check(Env, while(E, Cs), void) :- 
    type_check(Env, E, bool),
    type_check(Env, Cs, void).

% call
type_check(Env, call(X, Args), void) :-
    member((X, ProcType), Env),
    funtype_to_args_res(ProcType, ArgsTypes, void),
    check_args(Env, Args, ArgsTypes). 

% ==== PARAMETRES D'APPEL =====
% ref
type_check(Env, adr(X), ref(T)) :- member((X, ref(T)), Env).

% val
type_check(Env, expr(E), T) :- type_check(Env, E, T).

% ===== EXPRESSIONS =====
% num
type_check(_, num(_), int).

% idv
type_check(Env, ident(X), T) :- 
    member((X,T), Env).

% idr
type_check(Env, ident(X), T) :- member((X, ref(T)), Env).

% if
type_check(Env, if(E1, E2, E3), T) :- type_check(Env, E1, bool), type_check(Env, E2, T), type_check(Env, E3, T).

% and
type_check(Env, and(E1, E2), bool) :- type_check(Env, E1, bool), type_check(Env, E2, bool).

% or 
type_check(Env, or(E1, E2), bool) :- type_check(Env, E1, bool), type_check(Env, E2, bool).

% app
type_check(Env, app(E, Args), T) :- 
    type_check(Env, E, FunType),
    funtype_to_args_res(FunType, ArgsTypes, T),
    check_args(Env, Args, ArgsTypes).

% abs
type_check(Env, abs(Args, E), ArrowType) :-
    add_args_to_env(Args, Env, NewEnv),
    type_check(NewEnv, E, T),
    args_to_arrowtype(Args, T, ArrowType).

% alloc
type_check(Env, alloc(E), vec(_)) :- type_check(Env, E, int).

% nth 
type_check(Env, nth(E1, E2), T) :- type_check(Env, E1, vec(T)), type_check(Env, E2, int).

% len
type_check(Env, len(E), int) :- type_check(Env, E, vec(_)).

% vset
type_check(Env, vset(E1, E2, E3), vec(T)) :- type_check(Env, E1, vec(T)), type_check(Env, E2, int), type_check(Env, E3, T).

% ===== FONCTIONS AUXILLIAIRES ======

% nb : 
% \+ = not en prolog

% args_to_arrowtype : converti une liste d'args en type fonction
args_to_arrowtype(Args, T, arrow(Stars, T)) :-
    args_to_types(Args, Types),
    types_to_star(Types, Stars).

% args_to_types : extraire les types des args
args_to_types([], []).
args_to_types([arg(_, ArgType) | Args], [ArgType | Rest]) :- args_to_types(Args, Rest).

% add_args_to_env : ajoute les args a l'env -> la fonction A du formulaire 
add_args_to_env([], Env, Env).
add_args_to_env([arg(Name, Type) | Args], Env, NewEnv) :- add_args_to_env(Args, [(Name, Type)|Env], NewEnv).

% argsp_to_arrowtype : converti une liste d'args en type arrow
argsp_to_arrowtype(Args, arrow(Stars, void)) :-
    argsp_to_types(Args, Types),
    types_to_star(Types, Stars).

% argsp_to_types : extraire les types des argsp
argsp_to_types([], []).
argsp_to_types([argp(_, T) | Args], [T | Ts]) :- argsp_to_types(Args, Ts).
argsp_to_types([varp(_, T) | Args], [ref(T) | Ts]) :- argsp_to_types(Args, Ts).

% add_argsp_to_env : ajoute les argsp a l'env
add_argsp_to_env([], Env, Env).
add_argsp_to_env([argp(X, T) | Rest], Env, NewEnv) :- add_argsp_to_env(Rest, [(X, T) | Env], NewEnv).
add_argsp_to_env([varp(X, T) | Rest], Env, NewEnv) :- add_argsp_to_env(Rest, [(X, ref(T)) | Env], NewEnv).

% types_to_star : construire un star imbrique
types_to_star([T], T).
types_to_star([T1, T2 | Rest], star(T1, StarRest)) :- types_to_star([T2 | Rest], StarRest).

% check_args : check que les args ont les bons types
check_args(_, [], []).
check_args(Env, [E|Es], [T|Ts]) :- type_check(Env, E, T), check_args(Env, Es, Ts).

% arrowtype_to_args converti une fonction en une liste d'args et type de retour
funtype_to_args_res(arrow(Arg, Ret), Args, Ret) :-
    flatten_star(Arg, Args).

% proctype_to_argsp_res : converti une procedure en une liste d'args avec un type de retour
proctype_to_argsp_res(arrow(void, void), []).
proctype_to_argsp_res(arrow(T, void), [T]).


% flatten_star : pour obtenir les types d'un star
flatten_star(star(T1, T2), [T1|Rest]) :- flatten_star(T2, Rest).
flatten_star(T, [T]).

% ref
check_type(_, ref(T), T).

% type_check_lval : verfie si l'argument est un lvalue comme spécifié dans aps2
type_check_lval(Env, lval_id(X), T) :- member((X, ref(T)), Env). 
type_check_lval(Env, lval_nth(LV, Idx), T) :- 
    type_check_lval(Env, LV, vec(T)), 
    type_check(Env, Idx, int).
