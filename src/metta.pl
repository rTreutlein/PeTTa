:- ensure_loaded(parser).
:- ensure_loaded(translator).
:- ensure_loaded(filereader).

%% --------- Sample semantics ---------
test(A,B,R) :- ( A == B -> format(string(R), "is ~w, should ~w. ✅", [A,B])
                         ; format(string(R), "is ~w, should ~w. ❌", [A,B])).
'=='(A,B,R)    :- (A==B -> R=true ; R=false).
'<'(A,B,R)    :- (A<B -> R=true ; R=false).
'*'(A,B,R)   :- R is A*B.
'/'(A,B,R)   :- R is A/B.
'-'(A,B,R) :- R is A-B.
'+'(A,B,R)  :- R is A+B.
and(true,  true,  true).
and(true,  false, false).
and(false, true,  false).
and(false, false, false).
or(true,  true,  true).
or(true,  false, true).
or(false, true,  true).
or(false, false, false).
empty(_) :- fail.

%let/let*:
'let*'([], Body, Body).
'let*'([[Var,Val]|Rest], Body, Out) :- Var = Val, 'let*'(Rest, Body, Out).
let(Var,Val,In,Out) :- 'let*'([[Var,Val]], In, Out).

%% superpose/2: pick one element of a list on backtracking
superpose(List, X) :- member(X, List).
%Collapse is in flattener as it needs to inject findall

%% ---------------- Demo / Setup ----------------
:- dynamic fun/1.
register_fun(Name)   :- must_be(atom, Name), (fun(Name)->true; asserta(fun(Name))).
unregister_fun(Name) :- retractall(fun(Name)).
:- register_fun(superpose),
   register_fun(empty),
   register_fun(let),
   register_fun('let*'),
   register_fun('+'),
   register_fun('-'),
   register_fun('*'),
   register_fun('/'),
   register_fun('<'),
   register_fun('=='),
   register_fun('and'),
   register_fun('or'),
   register_fun('test').


