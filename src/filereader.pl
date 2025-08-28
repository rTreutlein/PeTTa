:- use_module(library(readutil)).
:- use_module(library(pcre)).   % re_replace/5

%% load_metta_file(+Path)
load_metta_file(Path) :-
    read_file_to_string(Path, S, []),
    load_metta_string(S).

%% load_metta_string(+String)
load_metta_string(S) :-
    % Rewrite lines like:    !(fib 10)
    % into balanced form:    (= (run) (fib 10))
    % Note: anchors per line (?m), capture inner "(...)" as group 1, put it back with \\1
    re_replace("(?m)^\\s*!\\s*\\((.*)\\)\\s*$", "(= (run) (\\1))", S, S1, [global]),
    string_codes(S1, Cs),
    metta_split_top(Cs, Forms),              % unchanged
    maplist(metta_assert_eq_if_any, Forms).

%% --- process one top-level form string (correct order!) ---
metta_assert_eq_if_any(FormStr) :-
    sread(FormStr, Term),
    ( Term = [=, HeadExpr, _BodyExpr] ->
        head_name_from_expr(HeadExpr, F),    % (fib $N) -> fib   |  (run) -> run
        register_fun(F),                     % register BEFORE flattening
        flatten_clause(FormStr, Clause),
        assertz(Clause)
    ; true ).

head_name_from_expr([FAtom|_], F) :- atom(FAtom), !, F = FAtom.
head_name_from_expr(Head, _) :-
    format(user_error, 'Unsupported head form: ~q~n', [Head]),
    fail.

%% --- balanced top-level splitter over codes (unchanged) ---
metta_split_top(Cs, Forms) :-
    char_code('(', O), char_code(')', C),
    metta_collect(Cs, 0, [], [], O, C, R), reverse(R, Forms).

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
