%/* ---------- Tiny S-expression reader (with vars shared) ---------- */
sread(S, Term) :-
    ( string(S) -> atom_string(A,S) ; A = S ),
    atom_codes(A, Cs),
    phrase(sexpr(Term, [], _Env), Cs).

sexpr(T,E0,E) --> ws, "(", ws, seq(T,E0,E), ws, ")", ws, !.
sexpr(N,E,E)  --> ws, number_(N), ws, !.
sexpr(V,E0,E) --> ws, var_symbol(V,E0,E), ws, !.
sexpr(A,E,E)  --> ws, atom_symbol(A), ws.

seq([X|Xs],E0,E2) --> sexpr(X,E0,E1), ws, seq(Xs,E1,E2).
seq([],E,E)       --> [].

number_(N)     --> digits(Ds), { number_codes(N, Ds) }.
digits([D|Ds]) --> [D], { code_type(D, digit) }, digits_rest(Ds).
digits_rest([D|Ds]) --> [D], { code_type(D, digit) }, digits_rest(Ds).
digits_rest([])     --> [].

% MeTTa variables: reuse existing, else create fresh
var_symbol(V,E0,E) -->
    "$", sym_tail(Cs), !,
    { atom_codes(Name, [0'$|Cs]),
      ( memberchk(Name-V0,E0) -> V = V0, E = E0
      ; V = _, E = [Name-V|E0]
      ) }.

% atoms: lowercase or symbol-starting
atom_symbol(A) -->
    [C], { \+ sp(C), C \= 0'(, C \= 0'), \+ code_type(C, upper) },
    sym_tail(Cs),
    { atom_codes(A, [C|Cs]) }.

sym_tail([C|Cs]) --> [C], { \+ sp(C), C \= 0'(, C \= 0') }, !, sym_tail(Cs).
sym_tail([])     --> [].

ws        --> [C], { sp(C) }, !, ws.
ws        --> [].
sp(0' ). sp(0'\t). sp(0'\n). sp(0'\r).
