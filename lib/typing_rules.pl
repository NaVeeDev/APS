
main :- read(user_input, X), type_check(X).

type_check(_) :- write("KO\n").
