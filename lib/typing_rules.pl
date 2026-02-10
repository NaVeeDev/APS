
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

% ===== SUITE DE COMMANDES =====
% defs
type_check(Env, [D|Cs], void) :- type_check(Env, D, Envv), type_check(Envv, Cs, void).

% end
type_check(Env, stat(S), void) :- type_check(Env, S, void).

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


% ===== INSTRUCTION =====
% echo
type_check(Env, echo(X), void) :- type_check(Env, X, int).


% ===== EXPRESSIONS =====
% num
type_check(_, num(_), int).

% id
type_check(Env, ident(X), T) :- member((X,T),Env).

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


% ===== FONCTIONS AUXILLIAIRES ======

% nb : 
% \+ = not en prolog

% args_to_arrowtype : converti une liste d'args en fonction
args_to_arrowtype([], T, T).
args_to_arrowtype([(_, ArgType) | Args], T, arrow(ArgType, ArrowType)) :- args_to_arrowtype(Args, T, ArrowType).

% add_args_to_env : ajoute les args a l'env
add_args_to_env([], Env, Env).
add_args_to_env([(Name, Type) | Args], Env, NewEnv) :- add_args_to_env(Args, [(Name, Type)|Env], NewEnv).

% check_args : check que les args ont les bons types
check_args(_, [], []).
check_args(Env, [E|Es], [T|Ts]) :- type_check(Env, E, T), check_args(Env, Es, Ts).

% arrowtype_to_args converti une fonction en une liste d'args et type de retour
funtype_to_args_res(arrow(Arg, Ret), [Arg], Ret) :- \+ is_star(Arg).
funtype_to_args_res(arrow(star(T1, T2), Ret), Args, Ret) :-
    flatten_star(star(T1, T2), Args).

% flatten_star : pour obtenir les types d'un star
flatten_star(star(T1, T2), [T1|Rest]) :- flatten_star(T2, Rest).
flatten_star(T, [T]) :- \+ T = star(_,_).

% star : return true si c'est star
is_star(star(_, _)).