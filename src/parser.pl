:- use_module(library(dcg/basics)). %blanks/0, number/1, string_without/2

%Read S string or atom, extract codes, and apply DCG:
sread(S,T) :- ( string(S)->atom_string(A,S) ; A=S ), atom_codes(A,Cs), phrase(sexpr(T,[],_), Cs).

%An S-Expression is a parentheses-nesting of S-Expressions that are either numbers, variables, or atoms:
sexpr(T,E0,E) --> blanks, "(", blanks, seq(T,E0,E), blanks, ")", blanks, !.
sexpr(N,E,E)  --> blanks, number(N), blanks, !.
sexpr(V,E0,E) --> blanks, var_symbol(V,E0,E), blanks, !.
sexpr(A,E,E)  --> blanks, atom_symbol(A), blanks.

%Recursive processing of S-Expressions within S-Expressions:
seq([X|Xs],E0,E2) --> sexpr(X,E0,E1), blanks, seq(Xs,E1,E2).
seq([],E,E)       --> [].

%Variables start with $, and keep track of them: re-using exising Prolog variables for variables of same name:
var_symbol(V,E0,E) --> "$", token(Cs), { atom_chars(N, Cs), ( memberchk(N-V0, E0) -> V=V0, E=E0 ; V=_, E=[N-V|E0] ) }.

%Atoms are just tokens:
atom_symbol(A) --> token(Cs), { atom_codes(A0,Cs), downcase_atom(A0,A) }.

%A token is a non-empty string without whitespace:
token(Cs) --> string_without(" \t\r\n()", Cs), { Cs \= [] }.
