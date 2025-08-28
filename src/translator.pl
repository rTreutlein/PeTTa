write_goals_([], _).
write_goals_([G], VN)   :- write_term(G, [variable_names(VN)]).
write_goals_([G|R], VN) :- write_term(G, [variable_names(VN)]), write(', '), write_goals_(R, VN).

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
    append(Args, [Out], HeadArgs),             % add extra Out arg
    Head =.. [F|HeadArgs],                     % build prog(Y,Out)
    flatten_(BodyExpr, GoalsB, OutBody),       % flatten body, OutBody is last value
    list_to_conj(GoalsB, Body0),
    Body = (Body0, Out = OutBody),             % unify Out with body result
    Clause = (Head :- Body).

flatten_head([F|Args], Head) :- Head =.. [F|Args].

%% ---------- Safe conjunction builder ----------
list_to_conj([], true) :- !.
list_to_conj([G], G) :- !.
list_to_conj([G|Gs], (G,Rest)) :-
    nonvar(Gs),
    list_to_conj(Gs, Rest).

%% ---------------- Core: ALL flatten_/3 clauses together ----------------

% base case
flatten_(X, [], X) :- (var(X); atomic(X)), !.

% ---- IF short-circuit (build conjunctions, no sequence_/1) ----
flatten_([if, C, T, E], Goals, Out) :- !,
    flatten_(C, Gc, Cv),
    flatten_(T, Gt, Tv),
    flatten_(E, Ge, Ev),
    list_to_conj(Gc, ConC),
    list_to_conj(Gt, ConT),
    list_to_conj(Ge, ConE),
    Goals = [ ( ConC,
               ( Cv == true
               -> (ConT, Out = Tv)
               ;  (ConE, Out = Ev)
               )
             ) ].

% (collapse E): collect all nondet results of E into a list
flatten_([collapse, E], Goals, Out) :- !,
    flatten_(E, GsE, EV),
    list_to_conj(GsE, Conj),
    Goals = [findall(EV, Conj, Out)].

% list (S-expr) case
flatten_([H|T], Goals, Out) :- !,
    flatten_(H, GsH, HV),
    flatten_list_(T, GsT, AVs),
    append(GsH, GsT, Inner),
    ( atom(HV), fun(HV), HV \== if ->
        Out = V,
        append(AVs, [V], ArgsV),
        Goal =.. [HV|ArgsV],
        ( HV == let -> Goals = [Goal|Inner]          % let before its body
        ;             append(Inner, [Goal], Goals)   % default: after subgoals
        )
    ;   Out = [HV|AVs],
        Goals = Inner
    ).

% treat non-list compounds as S-expr [F|Args]
flatten_(Term, Goals, Out) :-
    compound(Term),
    \+ is_list(Term),
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
