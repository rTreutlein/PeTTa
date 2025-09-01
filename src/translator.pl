%% --------- Input normalizer (dedupe string/atom → term) ---------
to_term(Input, T) :- ( (string(Input); atom(Input)) -> sread(Input, T0) ;  T0 = Input ), T = T0.

%% --------- Flatten (= Head Body) into Clause ---------
flatten_clause(Input, (Head :- (BodyConj, Out=OutBody))) :- to_term(Input, [=, [F|Args0], BodyExpr]), 
                                                            append(Args0, [Out], Args),
                                                            compound_name_arguments(Head, F, Args),
                                                            flatten_(BodyExpr, GoalsB, OutBody),
                                                            list_to_conj(GoalsB, BodyConj).

%% ---------- Safe conjunction builder ----------
list_to_conj([], true)   :- !.
list_to_conj([G], G)     :- !.
list_to_conj([G|Gs], (G,R)) :- list_to_conj(Gs, R).

%% ---------------- Core: ALL flatten_/3 clauses together ----------------

% START Detect assoc: list of [Var,_] pairs
is_assoc_pair([K,_]) :- var(K).
is_assoc_list(L) :- is_list(L), L \= [], maplist(is_assoc_pair, L).

flatten_assoc_list_dl([], Gs, Gs, Out, Out).
flatten_assoc_list_dl([[K,Vx]|Rs], Gs0, Gs, [[K,V]|Out0], Out) :- flatten_(Vx, GV, V), append(GV, Gs1, Gs0),
                                                                  flatten_assoc_list_dl(Rs, Gs1, Gs, Out0, Out).

flatten_(AL, Goals, Out) :- is_assoc_list(AL), !, flatten_assoc_list_dl(AL, Goals, [], Out, []).
% END

% base case
flatten_(X, [], X) :- (var(X); atomic(X)), !.

% ---- IF short-circuit (build conjunctions, no sequence_/1) ----
flatten_([if, C, T, E], Goals, Out) :- !,
    flatten_(C, Gc, Cv), list_to_conj(Gc, ConC),
    flatten_(T, Gt, Tv), list_to_conj(Gt, ConT),
    flatten_(E, Ge, Ev), list_to_conj(Ge, ConE),
    Goals = [(ConC, (Cv == true
                        -> (ConT, Out = Tv)
                        ;  (ConE, Out = Ev)))].

% (collapse E): collect all nondet results of E into a list
flatten_([collapse, E], Goals, Out) :- !, flatten_(E, GsE, EV), list_to_conj(GsE, Conj),
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
        ( memberchk(HV, [let,'let*']) -> Goals = [Goal|Inner]
        ; append(Inner, [Goal], Goals)
        )
    ;   Out = [HV|AVs],
        Goals = Inner
    ).

flatten_list_([], [], []).
flatten_list_([X|Xs], Goals, [V|Vs]) :-
    flatten_(X, G1, V),
    flatten_list_(Xs, G2, Vs),
    append(G1, G2, Goals).

