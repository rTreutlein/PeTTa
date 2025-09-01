:- ensure_loaded([parser, translator, filereader]).

%%%%%%%%%% Standard Library for MeTTa %%%%%%%%%%

%Let bindings:
'let*'([], B, B).
'let*'([[V,Val]|Rs], B, Out) :- V = Val, 'let*'(Rs, B, Out).
let(V,Val,In,Out) :- 'let*'([[V,Val]], In, Out).

%Arithmetic & Comparison:
'+'(A,B,R)  :- R is A+B.
'-'(A,B,R)  :- R is A-B.
'*'(A,B,R)  :- R is A*B.
'/'(A,B,R)  :- R is A/B.
'<'(A,B,R)  :- (A<B -> R=true ; R=false).
'>'(A,B,R)  :- (A>B -> R=true ; R=false).
'=='(A,B,R) :- (A==B -> R=true ; R=false).

%Boolean Logic:
and(true,  X, X).
and(false, _, false).
or( false, X, X).
or( true,  _, true).
not(true,  false).
not(false, true).

%Nondeterminism:
superpose(L,X) :- member(X,L).
empty(_) :- fail.

%Diagnostics / Testing:
test(A,B,R) :- (A==B -> E='✅' ; E='❌'), format(string(R), "is ~w, should ~w. ~w", [A,B,E]).

%Registration:
:- dynamic fun/1.
register_fun(N)   :- (fun(N)->true ; assertz(fun(N))).
unregister_fun(N) :- retractall(fun(N)).
:- maplist(register_fun, [superpose, empty, let, 'let*', '+','-','*','/','<','>','==', and, or, not, test]).
