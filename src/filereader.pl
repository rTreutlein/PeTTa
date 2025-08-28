:- use_module(library(readutil)).

%% load_metta_file(+Path)
load_metta_file(Path) :-
    read_file_to_string(Path, S, []),
    load_metta_string(S).

%% load_metta_string(+String)
load_metta_string(S) :-
    string_codes(S, Cs),
    metta_split_top(Cs, Forms),              % Forms = [ "(= ...)", "(= ...)", ... ]
    maplist(metta_assert_eq_if_any, Forms).

%% --- process one top-level form string (correct order!) ---
metta_assert_eq_if_any(FormStr) :-
    sread(FormStr, Term),                        % parse S-expression
    ( Term = [=, HeadExpr, _BodyExpr] ->
        head_name_from_expr(HeadExpr, F),       % e.g. (fib $N) -> fib
        register_fun(F),                        % <-- register BEFORE flattening
        flatten_clause(FormStr, Clause),        % now recursive calls compile as goals
        assertz(Clause)
    ; true ).

head_name_from_expr([FAtom|_], F) :- atom(FAtom), !, F = FAtom.
head_name_from_expr(Head, _) :-
    format(user_error, 'Unsupported head form: ~q~n', [Head]),
    fail.

%% --- balanced top-level splitter over codes (no char_type/2 anywhere) ---
metta_split_top(Cs, Forms) :-
    char_code('(', O), char_code(')', C),
    metta_collect(Cs, 0, [], [], O, C, R), reverse(R, Forms).

% metta_collect(Input,Depth,BufRev,AccRev,Open,Close,OutRev)
metta_collect([], 0, [], Acc, _, _, Acc) :- !.
metta_collect([], _, BufR, Acc, _, _, [Form|Acc]) :-
    reverse(BufR, Buf), string_codes(Form, Buf).
metta_collect([X|Xs], D, [], Acc, O, C, Out) :-
    ( X=:=O -> D1 is D+1, metta_collect(Xs, D1, [X], Acc, O, C, Out)
    ;           metta_collect(Xs, D,  [],  Acc, O, C, Out) ).
metta_collect([X|Xs], D, BufR, Acc, O, C, Out) :-
    ( X=:=O -> D1 is D+1
    ; X=:=C -> D1 is D-1
    ;          D1 = D ),
    BufR1 = [X|BufR],
    ( D1=:=0 ->
        reverse(BufR1, Buf), string_codes(Form, Buf),
        metta_collect(Xs, 0, [], [Form|Acc], O, C, Out)
    ; metta_collect(Xs, D1, BufR1, Acc, O, C, Out) ).
