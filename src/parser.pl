:- use_module(library(dcg/basics)). %blanks/0, number/1, string_without/2

%Generate a MeTTa S-expression string from the Prolog list (inverse parsing):
swrite(Term, String) :- phrase(swrite_exp(Term), Codes),
                        string_codes(String, Codes).
swrite_exp(Var)   --> { var(Var) }, !, "$", { term_to_atom(Var, A), atom_codes(A, Cs) }, Cs.
swrite_exp(Num)   --> { number(Num) }, !, { number_codes(Num, Cs) }, Cs.
swrite_exp(Str)   --> { string(Str) }, !, { string_codes(Str, Cs) }, Cs.
swrite_exp(Atom)  --> { atom(Atom) }, !, atom(Atom).
swrite_exp([H|T]) --> !, "(", seq([H|T]), ")".
swrite_exp(Term)  --> { Term =.. [F|Args] }, "(", atom(F), ( { Args == [] } -> [] ; " ", seq(Args) ), ")".
seq([X])    --> swrite_exp(X).
seq([X|Xs]) --> swrite_exp(X), " ", seq(Xs).

%Read S string or atom, extract codes, and apply DCG (parsing):
sread(S,T) :- atom_string(A,S),
              atom_codes(A,Cs),
              phrase(sexpr(T,[],_), Cs).

%An S-Expression is a parentheses-nesting of S-Expressions that are either numbers, variables, sttrings, or atoms:
sexpr(S,E,E)  --> blanks, string_lit(S), blanks, !.
sexpr(T,E0,E) --> blanks, "(", blanks, seq(T,E0,E), blanks, ")", blanks, !.
sexpr(N,E,E)  --> blanks, number(N), lookahead_any(" ()\t\n\r"), blanks, !.
sexpr(V,E0,E) --> blanks, var_symbol(V,E0,E), blanks, !.
sexpr(A,E,E)  --> blanks, atom_symbol(A), blanks.

lookahead_any(Terms, S, E) :- 
  string_codes(Terms,SC),
  S = [Head | _],
  member(Head,SC),
  !, S = E.

%Helper for strange atoms that aren't numbers, e.g. 1_2_3:
rest([C|Cs]) --> [C], { code_type(C,digit) ; C=0'_; C=0'. }, !, rest(Cs).
rest([]) --> [].

%Recursive processing of S-Expressions within S-Expressions:
seq([X|Xs],E0,E2) --> sexpr(X,E0,E1), blanks, seq(Xs,E1,E2).
seq([],E,E)       --> [].

%Variables start with $, and keep track of them: re-using exising Prolog variables for variables of same name:
var_symbol(V,E0,E) --> "$", token(Cs), { atom_chars(N, Cs), ( memberchk(N-V0, E0) -> V=V0, E=E0 ; V=_, E=[N-V|E0] ) }.

%Atoms are derived from tokens:
atom_symbol(A) --> token(Cs), { string_codes("\"", [Q]), ( Cs = [Q|_] -> append([Q|Body], [Q], Cs), %"str" as string
                                                                         string_codes(A, Body)
                                                                       ; atom_codes(R, Cs),         %others are atoms
                                                                         ( R = 'True' -> A = true
                                                                                       ; R = 'False'
                                                                                         -> A = false
                                                                                          ; A = R ))}.

%A token is a non-empty string without whitespace:
token(Cs) --> string_without(" \t\r\n()", Cs), { Cs \= [] }.

%Just string literal handling from here-on:
string_lit(S) --> "\"", string_chars(Cs), "\"", { string_codes(S, Cs) }.
string_chars([]) --> [].
string_chars([C|Cs]) --> [C], { C =\= 0'", C =\= 0'\\ }, !, string_chars(Cs).
string_chars([C|Cs]) --> "\\", [X], { (X=0'n->C=10; X=0't->C=9; X=0'r->C=13; C=X) }, string_chars(Cs).
