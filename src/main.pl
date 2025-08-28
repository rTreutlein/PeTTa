:- ensure_loaded(metta).

test1 :-
    flatten_clause("(= (prog $Y) (let $X $Y
                         (collapse (superpose (12 (plus $X 4))))))", Clause),
    %flatten_clause("(= (prog $Y) (superpose (let $L (1 2 3)
    %                     (collapse (superpose $L)))))", Clause),
    assertz(Clause),
    portray_clause(Clause),

    findall(Result, prog(10, Result), Results),
    format("prog(10, Results) -> Results = ~w~n", [Results]).

fib_demo :-
    register_fun(fib),
    register_fun(minus),
    register_fun(lt),
    flatten_clause(
      "(= (fib $N)
          (if (lt $N 2)
              $N
              (plus (fib (minus $N 1))
                    (fib (minus $N 2)))))", C3),
    assertz(C3),
    fib(30, R),
    format("fib(30) = ~w~n", [R]).




demo_file :- load_metta_file('./examples/fib.metta'),
   run(R),
   format("~w~n", [[R]]).


main :-
    demo_file.
