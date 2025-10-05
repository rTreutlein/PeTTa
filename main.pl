:- ensure_loaded([parser]).

% list_to_pat(+Tokens:list, -Out:string)
% Example: list_to_pat([friend,sam,A], Out).
%          Out = "[3] friend sam $".
list_to_pat(Toks, Out) :-
    length(Toks, N),
    maplist(tok_to_atom, Toks, Norm),              % normalize vars/$n -> '$', numbers -> atoms, etc.
    atomic_list_concat(Norm, ' ', Mid),
    format(string(Out), "[~d] ~w", [N, Mid]).

tok_to_atom(T, '$') :- var(T), !.                  % any Prolog var -> '$'
tok_to_atom(A, '$') :-                             % $1, $2, ... -> '$'
    atom(A), atom_concat('$', Ds, A), Ds \= '',
    atom_chars(Ds, Cs), maplist(char_type_digit, Cs), !.
tok_to_atom(N, A)  :- number(N), !, atom_number(A, N).
tok_to_atom(S, A)  :- string(S), !, atom_string(A, S).
tok_to_atom(A, A)  :- atom(A), !.
tok_to_atom(T, A)  :- term_string(T, Str), atom_string(A, Str).

char_type_digit(C) :- char_type(C, digit).

%main :- use_foreign_library('./mylib.so'), mork("upper","test",A),
%        format(string(R), "result: ~w~n", [A]),
%               writeln(R).


% list_to_sexpr(+List, -SExprString)
list_to_sexpr(List, S) :-
    sexpr_tokens(List, Ts),
    atomic_list_concat(Ts, ' ', Mid),
    format(string(S), "(~w)", [Mid]).

sexpr_tokens([], []).
sexpr_tokens([H|T], [Tok|Ts]) :-
    sexpr_token(H, Tok),
    sexpr_tokens(T, Ts).

sexpr_token(X, '$') :- var(X), !.
sexpr_token(X, Tok) :-
    is_list(X), !,
    sexpr_tokens(X, In),
    atomic_list_concat(In, ' ', Mid),
    format(atom(Tok), "(~w)", [Mid]).
sexpr_token(X, Tok) :- term_to_atom(X, Tok).

%mork_query(Pattern, Out) :- sread(Pattern, L),
%                            list_to_pat(L, MorkPat),
%                            mork("query", MorkPat, Temp),
%                            split_string(Temp, "\n", "", Raw),
%                            exclude(==(""), Raw, Lines),
%                            maplist(sread, Lines, Out).


% mork_query(+Pattern:list, +OutPattern:list(with one var), -Instantiated:list)
mork_query(Pattern, OutPattern, Instantiation) :-
    % build MORK query pattern from already-parsed list
    list_to_pat(Pattern, MorkPat),
    % run query
    mork("query", MorkPat, Temp),
    % split results, drop trailing empty line
    split_string(Temp, "\n", "", Raw),
    exclude(==(""), Raw, Lines),
    % parse each returned value with your s-expr reader
    maplist(sread, Lines, Values),
    % instantiate the single var in OutPattern with each value
    maplist(instantiate_one(OutPattern), Values, Instantiated),
    member(Instantiation, Instantiated).

instantiate_one(Template0, Value, Instantiated) :-
    copy_term(Template0, T),
    term_variables(T, [V|_]),   % assumes exactly one variable
    V = Value,
    Instantiated = T.

mork_init :- mork("init","",A),
             format(string(SA), "init result: ~w ~n", [A]),
             writeln(SA).

'mork_add-atom'(Space, Atom) :- list_to_sexpr(Atom, S), 
                                mork("load",S,B),
                                format(string(SB), "load result: ~w ~n", [B]),
                                writeln(SB).

main :- use_foreign_library('./mylib.so'),
        mork_init,
        'mork_add-atom'(Space, [friend,sam,tim]),
        'mork_add-atom'(Space, [friend,sam,joe]),
        findall(C, mork_query([friend,sam,X], [friend,sam,X], C), Cs),
        %mork("load","(friend sam tim) (friend sam tom) (friend mam tim)",B),
        %format(string(SB), "load result: ~w ~n", [B]), writeln(SB),
        %mork_query("(friend sam $1)", C),
        %mork_query([friend,sam,X], [friend,sam,X], C),
        %C=["tim","tom"],
        format(string(SC), "query result: ~w ~n", [Cs]), writeln(SC).
               
