%Flatten (= Head Body) MeTTa function into Prolog Clause:
translate_clause(Input, (Head :- (BodyConj, Out=OutBody))) :- sread(Input, Term),
                                                              Term = [=, [F|Args0], BodyExpr],
                                                              append(Args0, [Out], Args),
                                                              compound_name_arguments(Head, F, Args),
                                                              translate_expr(BodyExpr, GoalsB, OutBody),
                                                              goals_list_to_conj(GoalsB, BodyConj).

%Conjunction builder, turning goals list to a flat conjunction
% Conjunction builder that strips 'true' goals
goals_list_to_conj([], true)      :- !.
goals_list_to_conj([G], G)        :- !.
goals_list_to_conj([G|Gs], (G,R)) :- goals_list_to_conj(Gs, R).

%Extract arguments or superpose arguments as list
arg_to_list([superpose|T], T) :- !.
arg_to_list(A, [A]).

% Runtime dispatcher: call F if it's a registered fun/1, else keep as list
maybe_call(F, Args, Out) :- ( nonvar(F), atom(F), fun(F) -> append(Args, [Out], CallArgs),
                                                            Goal =.. [F|CallArgs],
                                                            call(Goal)
                                                          ; Out = [F|Args] ).

%Turn MeTTa code S-expression into goals list
translate_expr(X, [], X)          :- (var(X) ; atomic(X)), !.
translate_expr([H|T], Goals, Out) :-
        !, translate_expr(H, GsH, HV),
        ( HV == superpose, T = [Args], is_list(Args), Args = [F1|Rest], F1 = [superpose|Tail1]
          -> maplist(arg_to_list, Rest, RestLists),
             append([Tail1|RestLists], Union),
             append(GsH, [( member(Sub, Union), ( is_list(Sub) -> member(Out, Sub) ; Out = Sub ))], Goals)
        ; HV == collapse, T = [E] -> translate_expr(E, GsE, EV),
                                     goals_list_to_conj(GsE, Conj),
                                     append(GsH, [findall(EV, Conj, Out)], Goals)
        ; HV == if, T = [C,T1,E1] -> translate_expr(C, Gc, Cv), goals_list_to_conj(Gc, ConC),
                                     translate_expr(T1, Gt, Tv), goals_list_to_conj(Gt, ConT),
                                     translate_expr(E1, Ge, Ev), goals_list_to_conj(Ge, ConE),
                                     ( ConT == true -> BT = (Out = Tv) ; BT = (ConT, Out = Tv) ),
                                     ( ConE == true -> BE = (Out = Ev) ; BE = (ConE, Out = Ev) ),
                                     ( ConC == true -> append(GsH, [ (Cv == true -> BT ; BE) ], Goals)
                                                     ; append(GsH, [ (ConC, (Cv == true -> BT ; BE)) ], Goals))
        ;  HV == case, T = [KeyExpr, PairsExpr] -> translate_expr(KeyExpr, Gk, Kv),
                                                   translate_case(PairsExpr, Kv, Out, IfGoal),
                                                   append(GsH, Gk, G0),
                                                   append(G0, [IfGoal], Goals)
        ; HV == let, T = [Pat, Val, In] -> translate_expr(Pat, Gp, P),
                                           translate_expr(Val, Gv, V),
                                           translate_expr(In,  Gi, I),
                                           Goal = let(P, V, I, Out),
                                           append(GsH, Gp, A), append(A, Gv, B), append(B, Gi, Inner),
                                           Goals = [Goal | Inner]
        ; HV == 'let*', T = [Binds, Body] -> translate_bindings(Binds, Gb, Bs),
                                             translate_expr(Body,  Gd, B),
                                             Goal = 'let*'(Bs, B, Out),
                                             append(GsH, Gb, A), append(A, Gd, Inner),
                                             Goals = [Goal | Inner]
        ; translate_args(T, GsT, AVs),
          append(GsH, GsT, Inner),
          ( atom(HV), fun(HV) -> Out = V,                        %Known function => direct call
                                 append(AVs, [V], ArgsV),
                                 Goal =.. [HV|ArgsV],
                                 append(Inner, [Goal], Goals)
          ; atomic(HV), \+ atom(HV) -> Out = [HV|AVs],           %Literals (numbers, strings, etc.) => data, no dispatcher
                                       Goals = Inner
          ; atom(HV), \+ fun(HV) -> Out = [HV|AVs],              %Known non-function atom => data
                                    Goals = Inner
          ; append(Inner, [maybe_call(HV, AVs, Out)], Goals) )). %Unknown head (var/compound) => runtime dispatch

%Translate bindings without invoking call
translate_bindings([], [], []).
translate_bindings([[Pat, Val]|Rest], Goals, [[P,V]|Bs]) :- translate_pattern(Pat, P),  %Handle LHS as pure data
                                                            translate_expr(Val, Gv, V), %RHS as normal expr
                                                            translate_bindings(Rest, Gr, Bs),
                                                            append(Gv, Gr, Goals).

%Patterns: variables, atoms, numbers, lists
translate_pattern(X, X) :- var(X), !.
translate_pattern(X, X) :- atomic(X), !.
translate_pattern([H|T], [P|Ps]) :- !, translate_pattern(H, P),
                                       translate_pattern(T, Ps).


%Translate case expression recursively into nested if
translate_case([[K,VExpr]|Rs], Kv, Out, Goal) :- translate_expr(VExpr, Gv, VOut),
                                                 goals_list_to_conj(Gv, ConV),
                                                 ( ConV == true -> Test = (Kv = K)
                                                                 ; Test = (Kv = K, ConV) ),
                                                 ( Rs == [] -> Goal = (Test -> Out = VOut)
                                                             ; translate_case(Rs, Kv, Out, Next),
                                                               Goal = (Test -> Out = VOut ; Next) ).

%Translate arguments recursively
translate_args([], [], []).
translate_args([X|Xs], Goals, [V|Vs]) :- translate_expr(X, G1, V),
                                         translate_args(Xs, G2, Vs),
                                         append(G1, G2, Goals).
