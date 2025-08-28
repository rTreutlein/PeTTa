%:- use_module(library(clpfd)).
:- dynamic fun/1.

register_fun(Name)   :- must_be(atom, Name), (fun(Name)->true;asserta(fun(Name))).
unregister_fun(Name) :- retractall(fun(Name)).

%% --------- Flatten expression -> (Goals, Out) ---------
flatten_s(Input, Goals, Out) :-
    (   (string(Input); atom(Input)) -> sread(Input, Term)
    ;   Term = Input
    ),
    flatten_(Term, Goals, Out).

%% --------- Flatten (= Head Body) into Clause ---------
flatten_clause(Input, Clause) :-
    (   (string(Input); atom(Input)) -> sread(Input, Term)
    ;   Term = Input
    ),
    flatten_eq(Term, Clause).

flatten_eq([=, HeadExpr, BodyExpr], Clause) :- !,
    flatten_head(HeadExpr, Head0),             % e.g. (prog Y) -> prog(Y)
    Head0 =.. [F|Args],
    append(Args, [Out], HeadArgs),            % add extra Out arg
    Head =.. [F|HeadArgs],                    % build prog(Y,Out)
    flatten_(BodyExpr, GoalsB, OutBody),      % flatten body, OutBody is last value
    list_to_conj(GoalsB, Body0),
    Body = (Body0, Out = OutBody),            % unify Out with body result
    Clause = (Head :- Body).

flatten_head([F|Args], Head) :- Head =.. [F|Args].

%% ---------------- Core ----------------
flatten_(X, [], X) :- (var(X); atomic(X)), !.
flatten_([H|T], Goals, Out) :- !,
    flatten_(H, GsH, HV),
    flatten_list_(T, GsT, AVs),
    append(GsH, GsT, Inner),
    ( atom(HV), fun(HV) ->
        Out = V,
        append(AVs, [V], ArgsV),
        Goal =.. [HV|ArgsV],
        ( HV == let ->
            Goals = [Goal|Inner]            % let before its body
        ;   append(Inner, [Goal], Goals)    % default: after subgoals
        )
    ;   Out = [HV|AVs],
        Goals = Inner
    ).
flatten_(Term, Goals, Out) :-
    compound(Term),
    Term =.. [F|Args],
    flatten_([F|Args], Goals, Out).

flatten_list_([], [], []).
flatten_list_([X|Xs], Goals, [V|Vs]) :-
    flatten_(X, G1, V),
    flatten_list_(Xs, G2, Vs),
    append(G1, G2, Goals).

%% --------- Pretty printer (unchanged) ---------
flatten_pretty(Input, String) :-
    flatten_s(Input, Goals, _Out),
    term_variables(Goals, Vars),
    findall(Name=V, (nth1(I, Vars, V), format(atom(Name), 'T~d', [I])), VN),
    with_output_to(string(String), write_goals_(Goals, VN)).

write_goals_([], _).
write_goals_([G], VN)   :- write_term(G, [variable_names(VN)]).
write_goals_([G|R], VN) :- write_term(G, [variable_names(VN)]), write(', '), write_goals_(R, VN).

/* ---------- Tiny S-expression reader (with vars shared) ---------- */
sread(S, Term) :-
    ( string(S) -> atom_string(A,S) ; A = S ),
    atom_codes(A, Cs),
    phrase(sexpr(Term, [], _Env), Cs).

sexpr(T,E0,E) --> ws, "(", ws, seq(T,E0,E), ws, ")", ws, !.
sexpr(N,E,E)  --> ws, number_(N), ws, !.
sexpr(V,E0,E) --> ws, var_symbol(V,E0,E), ws, !.
sexpr(A,E,E)  --> ws, atom_symbol(A), ws.

seq([X|Xs],E0,E2) --> sexpr(X,E0,E1), ws, seq(Xs,E1,E2).
seq([],E,E)       --> [].

number_(N)     --> digits(Ds), { number_codes(N, Ds) }.
digits([D|Ds]) --> [D], { code_type(D, digit) }, digits_rest(Ds).
digits_rest([D|Ds]) --> [D], { code_type(D, digit) }, digits_rest(Ds).
digits_rest([])     --> [].

% MeTTa variables: reuse existing, else create fresh
var_symbol(V,E0,E) -->
    "$", sym_tail(Cs), !,
    { atom_codes(Name, [0'$|Cs]),
      ( memberchk(Name-V0,E0) -> V = V0, E = E0
      ; V = _, E = [Name-V|E0]
      ) }.


% atoms: lowercase or symbol-starting
atom_symbol(A) -->
    [C], { \+ sp(C), C \= 0'(, C \= 0'), \+ code_type(C, upper) },
    sym_tail(Cs),
    { atom_codes(A, [C|Cs]) }.

sym_tail([C|Cs]) --> [C], { \+ sp(C), C \= 0'(, C \= 0') }, !, sym_tail(Cs).
sym_tail([])     --> [].

ws        --> [C], { sp(C) }, !, ws.
ws        --> [].
sp(0' ). sp(0'\t). sp(0'\n). sp(0'\r).

%% --------- Conjunction builder ---------
list_to_conj([G], G).
list_to_conj([G|Gs], (G,Rest)) :- list_to_conj(Gs, Rest).

%% --------- Sample semantics ---------
plus(A,B,R) :- R is A+B.       % or: R #= A + B
let(Var,Val,In,Out) :- Var = Val, Out = In.
if(Cond,Then,Else,Out) :- ( call(Cond) -> Out = Then ; Out = Else ).

%% superpose/2: pick one element of a list on backtracking
superpose(List, X) :-
    must_be(list, List),
    member(X, List).

%% --------- Demo ---------
main :-
    register_fun(let),
    register_fun(plus),
    register_fun(if),
    register_fun(superpose),

    flatten_clause("(= (prog $Y) (let $X $Y 
                                     (if false 42 (superpose (12 
                                                              (plus $X 4))))))", Clause),
    assertz(Clause),
    portray_clause(Clause),

    findall(Result, prog(10, Result), Results),
    format("prog(10, Results) -> Results = ~w~n", [Results]).
    
