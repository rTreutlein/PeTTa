:- ensure_loaded([parser, translator, filereader]).

%%%%%%%%%% Standard Library for MeTTa %%%%%%%%%%

%%% Let bindings: %%%
'let*'([], B, B).
'let*'([[V,Val]|Rs], B, Out) :- V = Val, 'let*'(Rs, B, Out).
let(V,Val,In,Out) :- 'let*'([[V,Val]], In, Out).

%%% Arithmetic & Comparison: %%%
'+'(A,B,R)  :- R is A + B.
'-'(A,B,R)  :- R is A - B.
'*'(A,B,R)  :- R is A * B.
'/'(A,B,R)  :- R is A / B.
'%'(A,B,R)  :- R is A mod B.
'<'(A,B,R)  :- (A<B -> R=true ; R=false).
'>'(A,B,R)  :- (A>B -> R=true ; R=false).
'=='(A,B,R) :- (A==B -> R=true ; R=false).
'='(A,B,R) :- (A=B -> R=true ; R=false).
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
memberfast(X, List, true) :- memberchk(X, List), !.
memberfast(_, _, false).
excludefast(A,L,R) :- exclude(==(A), L, R).

%%% Diagnostics / Testing: %%%
'trace!'(In, Content, Out) :- format('~w~n', [In]), Out = Content.
test(A,B,R) :- (A==B -> E='✅' ; E='❌'),
               format(string(R), "is ~w, should ~w. ~w", [A,B,E]).

%%% Spaces: %%%

%Arity expander for JIT-indexing-efficient representation of space entries:
ensure_dynamic_arity(Space,Arity) :- ( current_predicate(Space/Arity)
                                       -> true ; dynamic(Space/Arity) ).

%Add an atom to the space:
'add-atom'(Space, [Rel|Args], true) :- length(Args, N), Arity is N + 2,
                                       ensure_dynamic_arity(Space, Arity),
                                       Term =.. [Space, Rel | Args],
                                       assertz(Term).

%Remove all same atoms:
'remove-atom'(Space, [Rel|Args], true) :- length(Args, N), Arity is N + 2,
                                          ensure_dynamic_arity(Space, Arity),
                                          Term =.. [Space, Rel | Args],
                                          ( clause(Term, true)
                                            -> retractall(Term) ).

%Match only a single instance, existential check:
'match-once'(Space, Pattern, OutPattern, Result) :- once(match(Space, Pattern, OutPattern, Result)).

%Function evaluation matches, where the unification returned true, so it unified:
match('&self', true, Arg2, Result) :- Result=Arg2.

%Match for pattern:
match(Space, [Rel|PatArgs], OutPattern, Result) :- Term =.. [Space, Rel | PatArgs],
                                                   Term, Result = OutPattern.

%Get all atoms in space, irregard of arity:
'get-atoms'(Space, Pattern) :- current_predicate(Space/Arity),
                               functor(Head, Space, Arity),
                               clause(Head, true),
                               Head =.. [Space | Pattern].


%%% Python bindings: %%%

python(SpecList, Result) :- python(SpecList, Result, []).

python([Spec|Args], Result, Opts) :- normalize_spec(Spec, Kind, Mod, Fun),
                                     ( Kind == method
                                       -> ( Args = [Obj|Rest]
                                            -> build_fun_term(Fun, Rest, Meth),
                                               py_call(Obj:Meth, Result, Opts)
                                             ; domain_error(obj_for_method, Args))
                                        ; % function
                                          build_fun_term(Fun, Args, Call0),
                                          ( var(Mod) -> Call = Call0 ; Call = Mod:Call0 ),
                                          py_call(Call, Result, Opts)).

normalize_spec(Spec, Kind, Mod, Fun) :- ( string(Spec) -> atom_string(A, Spec) ; A = Spec ),
                                        must_be(atom, A),
                                        ( sub_atom(A, 0, 1, _, '.')                         % ".method"
                                          -> Kind = method, Mod = _, sub_atom(A, 1, _, 0, Fun)
                                           ; atomic_list_concat([M,F], :, A)                % "mod:fun"
                                             -> Kind = function, Mod = M, Fun = F
                                              ; Kind = function, Mod = builtins, Fun = A ). % bare "fun"

build_fun_term(Fun, Args, Term) :- ( Args == [] -> compound_name_arguments(Term, Fun, [])
                                                 ; Term =.. [Fun|Args] ).

%%% Registration: %%%
:- dynamic fun/1.
register_fun(N)   :- (fun(N)->true ; assertz(fun(N))).
unregister_fun(N) :- retractall(fun(N)).
:- maplist(register_fun, [superpose, empty, let, 'let*', '+','-','*','/', '%', min, max,
                          '<','>','==', '=', '<=', '>=', and, or, not, 'car-atom', 'cdr-atom', 'trace!', test,
                          append, length, sort, msort, memberfast, excludefast, list_to_set,
                          'add-atom', 'remove-atom', 'get-atoms', 'match', 'match-once', python]).
