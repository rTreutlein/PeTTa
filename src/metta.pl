:- ensure_loaded(sreader).
:- ensure_loaded(flattener).
:- ensure_loaded(filereader).

%% --------- Sample semantics ---------
lt(A,B,R)    :- (A<B -> R=true ; R=false).
mul(A,B,R)   :- R is A*B.
div(A,B,R)   :- R is A/B.
minus(A,B,R) :- R is A-B.
plus(A,B,R)  :- R is A+B.
let(Var,Val,In,Out) :- Var = Val, Out = In.
if(Cond,Then,Else,Out) :- ( call(Cond) -> Out = Then ; Out = Else ).

%% superpose/2: pick one element of a list on backtracking
superpose(List, X) :- member(X, List).
%Collapse is in flattener as it needs to inject findall

%% ---------------- Demo / Setup ----------------
:- dynamic fun/1.
register_fun(Name)   :- must_be(atom, Name), (fun(Name)->true; asserta(fun(Name))).
unregister_fun(Name) :- retractall(fun(Name)).
:- register_fun(let),
   register_fun(plus),
   register_fun(superpose),
   register_fun(minus),
   register_fun(lt).




