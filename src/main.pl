:- ensure_loaded(metta).

prologfunc(X,Y) :- Y is X+1.

prolog_interop_example :- register_fun(prologfunc), flatten_clause("(= (mettafunc $x) (prologfunc $x))", C3),
                          assertz(C3), listing(mettafunc), mettafunc(30, R), format("mettafunc(30) = ~w~n", [R]).

main :- current_prolog_flag(argv, Args),
        ( Args = [] ->  prolog_interop_example ;
                        Args = [File|_], load_metta_file(File), findall(R, run(R), Results), format("~w~n", [Results]) ).
