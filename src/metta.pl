:- ensure_loaded([parser, translator, filereader, spaces]).

%%%%%%%%%% Standard Library for MeTTa %%%%%%%%%%

%%% Let bindings: %%%
'let*'([], B, B).
'let*'([[V,Val]|Rs], B, Out) :- V = Val, 'let*'(Rs, B, Out).
let(V, Val, In, Out) :- 'let*'([[V,Val]], In, Out).

%%% Arithmetic & Comparison: %%%
'+'(A,B,R)  :- R is A + B.
'-'(A,B,R)  :- R is A - B.
'*'(A,B,R)  :- R is A * B.
'/'(A,B,R)  :- R is A / B.
'%'(A,B,R)  :- R is A mod B.
'<'(A,B,R)  :- (A<B -> R=true ; R=false).
'>'(A,B,R)  :- (A>B -> R=true ; R=false).
'=='(A,B,R) :- (A==B -> R=true ; R=false).
'='(A,B,R) :-  (A=B -> R=true ; R=false).
'=alpha'(A,B,R) :- (A =@= B -> R=true ; R=false).
'=@='(A,B,R) :- (A =@= B -> R=true ; R=false).
'<='(A,B,R) :- (A =< B -> R=true ; R=false).
'>='(A,B,R) :- (A => B -> R=true ; R=false).
min(A,B,R)  :- R is min(A,B).
max(A,B,R)  :- R is max(A,B).

%%% Boolean Logic: %%%
and(true,  X, X).
and(false, _, false).
or( false, X, X).
or( true,  _, true).
not(true,  false).
not(false, true).

%%% Nondeterminism: %%%
superpose(L,X) :- member(X,L).
empty(_) :- fail.

%%% Lists / Tuples: %%%
'car-atom'([H|_], H).
'cdr-atom'([_|T], T).
'decons'([H|T], [H|[T]]).
memberfast(X, List, true) :- memberchk(X, List), !.
memberfast(_, _, false).
excludefast(A, L, R) :- exclude(==(A), L, R).

%%% Type system: %%%
get_function_type([F,Arg], T) :- match('&self', [':',F,['->',A,B]], _, _),
                                 'get-type'(Arg, A),
                                 T = B.
'get-type'(X, 'Number')   :- number(X), !.
'get-type'(X, 'Variable') :- var(X), !.
'get-type'(X, 'String')   :- string(X), !.
'get-type'(true, 'Bool')  :- !.
'get-type'(false, 'Bool') :- !.
'get-type'(X, T) :- get_function_type(X,T).
'get-type'(X, T) :- \+ get_function_type(X, _),
                    is_list(X),
                    maplist('get-type', X, T).
'get-type'(X, T) :- match('&self', [':',X,T], T, _).
'get-metatype'(X, 'Variable') :- var(X), !.
'get-metatype'(X, 'Grounded') :- number(X), !.
'get-metatype'(X, 'Grounded') :- string(X), !.
'get-metatype'(true,  'Grounded') :- !.
'get-metatype'(false, 'Grounded') :- !.
'get-metatype'(X, 'Grounded') :- atom(X), fun(X), !.  % e.g., '+' is a registered fun/1
'get-metatype'(X, 'Expression') :- is_list(X), !.     % e.g., (+ 1 2), (a b)
'get-metatype'(X, 'Symbol') :- atom(X), !.            % e.g., a

%Commonly used predicates:
'is-var'(A,R) :- (var(A) -> R=true ; R=false).
'is-expr'(A,R) :- (is_list(A) -> R=true ; R=false).

member_strict(X, [Y|_]) :- X == Y.
member_strict(X, [_|T]) :- member_strict(X, T).

union([],[],[]).
union(List1,[],List1).
union(List1, [Head2|Tail2], [Head2|Output]):-
    \+(member_strict(Head2,List1)), union(List1,Tail2,Output).
union(List1, [Head2|Tail2], Output):-
    member_strict(Head2,List1), union(List1,Tail2,Output).  

intersection([], _, _, []) :- !.
intersection(_, [], _, []) :- !.
intersection([Head1|Tail1], List2, Pred, [Head1|Output]) :-
    member_with_pred(Head1, List2, Pred),
    intersection(Tail1, List2, Pred, Output).
intersection([Head1|Tail1], List2, Pred, Output) :-
    \+ member_with_pred(Head1, List2, Pred),
    intersection(Tail1, List2, Pred, Output).

member_with_pred(Element, [Head|_], Pred) :-
    call(Pred, Element, Head, true).
member_with_pred(Element, [_|Tail], Pred) :-
    member_with_pred(Element, Tail, Pred).

subtract([], _, R) =>
    R = [].
subtract([E|T], D, R) =>
    (   member_strict(E, D)
    ->  subtract(T, D, R)
    ;   R = [E|R1],
        subtract(T, D, R1)
    ).



%%% Diagnostics / Testing: %%%
'trace!'(In, Content, Out) :- format('~w~n', [In]), Out = Content.
test(A,B,R) :- (A == B -> E = '✅' ; E = '❌'),
               format(string(R), "is ~w, should ~w. ~w ~n", [A, B, E]).

%%% Python bindings: %%%
'py-call'(SpecList, Result) :- 'py-call'(SpecList, Result, []).
'py-call'([Spec|Args], Result, Opts) :- ( string(Spec) -> atom_string(A, Spec) ; A = Spec ),
                                        must_be(atom, A),
                                        ( sub_atom(A, 0, 1, _, '.')         % ".method"
                                          -> sub_atom(A, 1, _, 0, Fun),
                                             Args = [Obj|Rest],
                                             ( Rest == []
                                               -> compound_name_arguments(Meth, Fun, [])
                                                ; Meth =.. [Fun|Rest] ),
                                             py_call(Obj:Meth, Result, Opts)
                                           ; atomic_list_concat([M,F], '.', A) % "mod.fun"
                                             -> ( Args == []
                                                  -> compound_name_arguments(Call0, F, [])
                                                   ; Call0 =.. [F|Args] ),
                                                py_call(M:Call0, Result, Opts)
                                              ; ( Args == []                      % bare "fun"
                                                  -> compound_name_arguments(Call0, A, [])
                                                   ; Call0 =.. [A|Args] ),
                                                py_call(builtins:Call0, Result, Opts) ).

%%% Hihger-order predicates: %%%

fold([], Acc, _Combiner, Acc).

fold([Head|Tail], Acc, Combiner, Result) :-
    call(Combiner, Acc, Head, NewAcc),  % Apply Combiner(Acc, Head, NewAcc)
    fold(Tail, NewAcc, Combiner, Result).

foldexp([], Acc, _Combiner, Acc). 

foldexp(A, Acc, Combiner, Result) :-
    atom(A),
    call(Combiner, Acc, A, Result).

foldexp([Head|Tail], Acc, Combiner, Result) :-
    \+is_list(Head),
    call(Combiner, Acc, Head, NewAcc),  % Apply Combiner(Acc, Head, NewAcc)
    foldexp(Tail, NewAcc, Combiner, Result).

foldexp([Head|Tail], Acc, Combiner, Result) :-
    is_list(Head),
    foldexp(Head, Acc, Combiner, NewAcc),
    foldexp(Tail, NewAcc, Combiner, Result).

%Registration:
:- dynamic fun/1.
register_fun(N) :- (fun(N) -> true ; assertz(fun(N))).
unregister_fun(N/Arity) :- retractall(fun(N)),
                           abolish(N, Arity).

:- maplist(register_fun, [superpose, empty, let, 'let*', '+','-','*','/', '%', min, max,
                          '<','>','==', '=', '<=', '>=', and, or, not, 'car-atom', 'cdr-atom', 'trace!', test,
                          append, length, sort, msort, memberfast, excludefast, list_to_set, maplist,
                          'add-atom', 'remove-atom', 'get-atoms', 'match', 'match-once', 'is-var', 'is-expr', 'get-mettatype',
                          'decons', 'fold', 'foldexp', 'union', 'intersection', 'subtract', 'unify', 'py-call', 'get-type', 'get-metatype',
                          '=alpha','=@=']).
