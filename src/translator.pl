%Flatten (= Head Body) MeTTa function into Prolog Clause:
translate_clause(Input, (Head :- BodyConj)) :- Input = [=, [F|Args0], BodyExpr],
                                               append(Args0, [Out], Args),
                                               compound_name_arguments(Head, F, Args),
                                               translate_expr(BodyExpr, GoalsB, Out),
                                               goals_list_to_conj(GoalsB, BodyConj).

%Conjunction builder, turning goals list to a flat conjunction:
goals_list_to_conj([], true)      :- !.
goals_list_to_conj([G], G)        :- !.
goals_list_to_conj([G|Gs], (G,R)) :- goals_list_to_conj(Gs, R).

% Runtime dispatcher: call F if it's a registered fun/1, else keep as list:
reduce(F, Args, Out) :- ( nonvar(F), atom(F), fun(F) -> append(Args, [Out], CallArgs),
                                                        Goal =.. [F|CallArgs],
                                                        call(Goal)
                                                      ; Out = [F|Args] ).

%Combined expr translation to goals list
translate_expr_to_conj(Input, Conj, Out) :- translate_expr(Input, Goals, Out),
                                            goals_list_to_conj(Goals, Conj).

%Turn MeTTa code S-expression into goals list:
translate_expr(X, [], X)          :- (var(X) ; atomic(X)), !.
translate_expr([H|T], Goals, Out) :-
        translate_expr(H, GsH, HV),
        ( HV == superpose, T = [Args], is_list(Args) -> build_superpose_branches(Args, Out, Branches),
                                                        disj_list(Branches, Disj),
                                                        append(GsH, [Disj], Goals)
        ; HV == collapse, T = [E] -> translate_expr_to_conj(E, Conj, EV),
                                     append(GsH, [findall(EV, Conj, Out)], Goals)
        ; HV == if, T = [Cond, Then, Else] -> ( translate_expr_to_conj(Cond, ConC, true),
                                                translate_expr_to_conj(Then, ConT, Vt)
                                                -> handle_if_condition(ConC, ConT, Cond, Then, Else, GsH, Goals, Vt, Out)
                                                 ; translate_expr_to_conj(Else, ConE, Out),
                                                   ( ConE == true -> GsH = Goals ; append(GsH, [ConE], Goals) ))
        ; HV == case, T = [KeyExpr, PairsExpr] -> translate_expr(KeyExpr, Gk, Kv),
                                                  translate_case(PairsExpr, Kv, Out, IfGoal),
                                                  append(GsH, Gk, G0),
                                                  append(G0, [IfGoal], Goals)
        ; HV == sealed, T = [Vars, Expr] -> translate_expr_to_conj(Expr, Con, Out),
                                            Goals = [copy_term(Vars,Con,_,Ncon),Ncon]
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
        ; HV == cut, T = [] -> append(GsH, [(!)], Goals),
                               Out = true
        ; HV == once, T = [X] -> translate_expr_to_conj(X, Conj, Out),
                                 append(GsH, [once(Conj)], Goals)
        ; HV == 'forall', T = [[GF], TF] -> GCall =.. [GF, X],
                                            TCall =.. [TF, X, Truth],
                                            U = [( forall(GCall, (TCall, Truth==true)) -> Out=true ; Out=false )],
                                            append(GsH, U, Goals)
        ; HV == 'foldall', T = [AF, [GF], InitS] -> translate_expr_to_conj(InitS, ConjInit, Init),
                                                    Agg   =.. [AF, X],
                                                    GCall =.. [GF, X],
                                                    append(GsH, [ConjInit, foldall(Agg, GCall, Init, Out)], Goals)
        ; HV == hyperpose,                                         %Hyperponse via (hyperpose (E1 ... En))
          T = [L],
          build_hyperpose_branches(L, Branches),
          append(GsH, [concurrent_and(member((Goal,Res), Branches), (call(Goal), Out = Res))], Goals)
        ; ( HV == 'add-atom' ; HV == 'remove-atom' ) -> append(T, [Out], RawArgs),
                                                        Goal =.. [HV|RawArgs],
                                                        append(GsH, [Goal], Goals)
        ; translate_args(T, GsT, AVs),
          append(GsH, GsT, Inner),
          ( atom(HV), fun(HV) -> append(AVs, [Out], ArgsV),        %Known function => direct call
                                 Goal =.. [HV|ArgsV],
                                 append(Inner, [Goal], Goals)
          ; ( number(HV) ; string(HV) ; HV == true ; HV == false ) %Value head, process all tail args
            -> translate_args(AVs, GsTail, AVs1),
               append(Inner, GsTail, Inner1),
               Out = [HV|AVs1],
               Goals = Inner1
          ; is_list(HV) -> eval_data_term(HV, Gd, HV1),            %Plain data list: evaluate inner fun-sublists
                           append(Inner, Gd, Goals),
                           Out = [HV1|AVs]
          ; append(Inner, [reduce(HV, AVs, Out)], Goals) )).       %Unknown head (var/compound) => runtime dispatch

%Handle data list:
eval_data_term(X, [], X) :- (var(X); atomic(X)), !.
eval_data_term([F|As], Goals, Val) :- ( atom(F), fun(F) -> translate_expr([F|As], Goals, Val)
                                                         ; eval_data_list([F|As], Goals, Val) ).

%Handle data list entry:
eval_data_list([], [], []).
eval_data_list([E|Es], Goals, [V|Vs]) :- ( is_list(E) -> eval_data_term(E, G1, V) ; V = E, G1 = [] ),
                                         eval_data_list(Es, G2, Vs),
                                         append(G1, G2, Goals).

%Translate bindings without invoking call:
translate_bindings([], [], []).
translate_bindings([[Pat, Val]|Rest], Goals, [[P,V]|Bs]) :- translate_pattern(Pat, P),  %Handle LHS as pure data
                                                            translate_expr(Val, Gv, V), %RHS as normal expr
                                                            translate_bindings(Rest, Gr, Bs),
                                                            append(Gv, Gr, Goals).

%Patterns: variables, atoms, numbers, lists:
translate_pattern(X, X) :- var(X), !.
translate_pattern(X, X) :- atomic(X), !.
translate_pattern([H|T], [P|Ps]) :- !, translate_pattern(H, P),
                                       translate_pattern(T, Ps).

% Handles cases where condition translation succeeded.
handle_if_condition(true, ConT, _, _, _, GsH, Goals, Vt, Out) :- ( ConT == true -> GsH = Goals, Out = Vt
                                                                                 ; append(GsH, [ConT], Goals) ), !.
handle_if_condition(ConC, ConT, _Cond, _Then, Else, GsH, Goals, Vt, Out) :- translate_expr(Else, Ge, Ve),
                                                                            goals_list_to_conj(Ge, ConE),
                                                                            build_branch(ConT, Vt, Out, Bt),
                                                                            build_branch(ConE, Ve, Out, Be),
                                                                            append(GsH, [ConC -> Bt ; Be], Goals).

% Constructs the goal for a single branch of an if-then-else/case.
build_branch(true, Val, Out, (Out = Val)) :- !.
build_branch(Con, Val, Out, Goal) :- var(Val) -> Val = Out, Goal = Con
                                               ; Goal = (Val = Out, Con).

%Translate case expression recursively into nested if:
translate_case([[K,VExpr]|Rs], Kv, Out, Goal) :- translate_expr_to_conj(VExpr, ConV, VOut),
                                                 build_branch(ConV,VOut,Out,Then),
                                                 (Rs == [] -> Goal = ((Kv = K) -> Then)
                                                            ; translate_case(Rs, Kv, Out, Next),
                                                              Goal = ((Kv = K) -> Then ; Next)).

%Translate arguments recursively:
translate_args([], [], []).
translate_args([X|Xs], Goals, [V|Vs]) :- translate_expr(X, G1, V),
                                         translate_args(Xs, G2, Vs),
                                         append(G1, G2, Goals).

%Build A ; B ; C ... from a list:
disj_list([G], G).
disj_list([G|Gs], (G ; R)) :- disj_list(Gs, R).

%Build one disjunct per branch: (Conj, Out = Val):
build_superpose_branches([], _, []).
build_superpose_branches([E|Es], Out, [B|Bs]) :- translate_expr_to_conj(E, Conj, Val),
                                                 build_branch(Conj, Val, Out, B),
                                                 build_superpose_branches(Es, Out, Bs).

%Build hyperpose branch as a goal list for concurrent_maplist to consume:
build_hyperpose_branches([], []).
build_hyperpose_branches([E|Es], [(Goal, Res)|Bs]) :- translate_expr_to_conj(E, Goal, Res),
                                                      build_hyperpose_branches(Es, Bs).
