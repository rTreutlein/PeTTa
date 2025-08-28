:- ensure_loaded(metta).

prologfunc(X,Y) :- Y is X+1.

prolog_interop_example :-
    register_fun(prologfunc),
    flatten_clause(
      "(= (mettafunc $x) (prologfunc $x))", C3),
    assertz(C3),
    mettafunc(30, R),
    format("mettafunc(30) = ~w~n", [R]).

main :-
    current_prolog_flag(argv, Args),
    (   Args = []                         % no arguments
    ->  prolog_interop_example
    ;   Args = [File|_],                  % at least one argument
        load_metta_file(File),
        run(R),
        format("~w~n", [[R]])
    ).
