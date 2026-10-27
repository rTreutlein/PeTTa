:- ensure_loaded('../src/metta').

:- begin_tests(tuple_lowering).

:- retractall(user:silent(_)), assertz(user:silent(true)).
:- process_metta_string("
(: tuple-values (-[det]-> Number (Number Number)))
(= (tuple-values $x) ($x (+ $x 1)))
(: tuple-total (-[det]-> Number Number))
(= (tuple-total $x) (let ($a $b) (tuple-values $x) (+ $a $b)))
(: tuple-inc (-[det]-> Number Number))
(= (tuple-inc $x) (+ $x 1))
(: functional-pattern (-[semidet]-> Number Number))
(= (functional-pattern $x) (let (tuple-inc $x) 3 $x))
", _).

test(fresh_numeric_destructuring_uses_no_reducer) :-
    clause('tuple-total'(_,_),Body),
    \+ (sub_term(G,Body),nonvar(G),functor(G,reduce,2)),
    'tuple-total'(2,R),R =:= 5.

test(literal_functional_pattern_still_executes) :-
    'functional-pattern'(2,2),\+ 'functional-pattern'(1,_).

test(fresh_tuple_is_constructed_at_its_original_point) :-
    Goal=(reduce([X,Y],Pair),var(X),Pair=[one,two]),
    fold_clause_bindings(Goal,none,Folded),
    var(X),var(Y),var(Pair),
    Folded == (fresh_data_list([X,Y],Pair),var(X),Pair=[one,two]),
    call(Folded),X==one,Y==two.

test(tuple_object_sharing_is_preserved) :-
    Goal=(reduce([X,Y],Pair),nb_setarg(1,Pair,changed),arg(1,Pair,Seen)),
    fold_clause_bindings(Goal,result(Seen),Folded),
    var(Pair),call(Folded),Seen==changed,var(X),var(Y).

test(head_visible_field_keeps_dispatch) :-
    Goal=reduce([F,X],Pair),
    fold_clause_bindings(Goal,input(F),Folded),
    Folded == Goal, F='tuple-inc',X=2,call(Folded),Pair=:=3.

test(earlier_field_observation_keeps_dispatch) :-
    Goal=(var(F),reduce([F,X],Pair)),
    fold_clause_bindings(Goal,none,Folded),
    Folded == Goal,var(F),var(X),var(Pair).

test(existing_result_keeps_dispatch) :-
    Goal=reduce([F,X],Pair),
    fold_clause_bindings(Goal,result(Pair),Folded),
    Folded == Goal,var(F),var(X).

test(result_occurring_in_tuple_keeps_cycle_check) :-
    Goal=reduce([Pair,X],Pair),
    fold_clause_bindings(Goal,none,Folded),
    Folded == Goal,\+ call(Folded),var(Pair),var(X).

test(cyclic_literal_field_keeps_cycle_check) :-
    Cyclic=loop(Cyclic),Goal=reduce([F,Cyclic],Pair),
    fold_clause_bindings(Goal,none,Folded),
    Folded == Goal,\+ call(Folded),var(F),var(Pair).

test(open_list_spine_is_not_lowered) :-
    Goal=reduce([F|Tail],Pair),
    fold_clause_bindings(Goal,none,Folded),
    Folded == Goal,var(F),var(Tail),var(Pair).

test(sibling_branch_fields_are_not_narrowed) :-
    Goal=(fail -> reduce([F,X],Pair) ; (F='tuple-inc',X=2)),
    fold_clause_bindings(Goal,none,Folded),
    Folded == Goal,var(Pair).

test(nested_fresh_data_preserves_variables) :-
    Goal=(reduce([F,box(X),X],Pair),Pair=[tag,box(value),value]),
    fold_clause_bindings(Goal,none,Folded),
    call(Folded),F==tag,X==value.

test(duplicate_fields_preserve_equality_constraint) :-
    Goal=(reduce([X,X],Pair),Pair=[one,two]),
    fold_clause_bindings(Goal,none,Folded),
    var(X),\+ call(Folded).

test(producer_side_effect_order_is_preserved) :-
    Goal=(reduce([X,Y],Pair),var(X),X=left,Y=right,Pair=[left,right]),
    fold_clause_bindings(Goal,none,Folded),
    call(Folded).

:- end_tests(tuple_lowering).
