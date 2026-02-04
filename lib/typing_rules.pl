
main :- read(user_input, X), check(X, void).

check(Expr, T) :- env0(Env), (type_check(Env, Expr, T) -> write("OK\n"); write("KO\n")).

% CONTEXTE INITIAL
env0([
        (true, bool),
        (false, bool),
        (not, arrow(bool, bool)),
        (eq, arrow(int, arrow(int, bool))),
        (lt, arrow(int, arrow(int, bool))),
        (add, arrow(int, arrow(int, int))),
        (sub, arrow(int, arrow(int, int))),
        (mul, arrow(int, arrow(int, int))),
        (div, arrow(int, arrow(int, int)))
    ]
).

% PROGRAMMES
% prog
type_check(Env, prog(Cs), void) :- type_check(Env, Cs, void).

% SUITE DE COMMANDES
% defs
type_check(Env, [D|Cs], void) :- type_check(Env, D, Envv), type_check(Envv, Cs, void).

% end
type_check(Env, stat(S), void) :- type_check(Env, S, void).

% DEFINITION
% const
type_check(Env, const(X, T, E), [(X,T)|Env]) :- type_check(Env, E, T).

% fun

% funrec



% INSTRUCTION
% echo
type_check(Env, echo(X), void) :- type_check(Env, X, int).


% EXPRESSIONS
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

% abs
