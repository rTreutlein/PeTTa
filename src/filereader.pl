:- use_module(library(readutil)). % read_file_to_string/3
:- use_module(library(pcre)).     % re_replace/4

%Read Filename into string S and process it (S holds MeTTa code):
load_metta_file(Filename, RunArg) :- read_file_to_string(Filename, S, []),
                                     process_metta_string(S, RunArg).

%Extract function definitions and process each, whereby !(Z) is transformed to (= (run ARG) (Z)):
process_metta_string(S, RunArg) :- split_string(S, "\n", "", L0),
                                   findall(C, (member(L,L0), split_string(L,";","",[C|_])), L1),
                                   atomic_list_concat(L1, '\n', CodeWithoutComment),
                                   atomic_list_concat(['(= (run ', RunArg, ') (\\1))'], Replacement),
                                   re_replace("(?m)^\\s*!\\s*\\(((?:[^()]|\\((?-1)\\))*)\\)"/g,
                                              Replacement, CodeWithoutComment, FunctionizedCode),
                                   string_codes(FunctionizedCode, Codes),
                                   phrase(top_forms(Forms), Codes),
                                   maplist(assert_function, Forms).

%Functions stay functions and runaway S-expressions become add-atom calls with result omitted:
to_function_form(T, T) :- T = [=, [_|_], _], !.
to_function_form(T, [=, [run, default], [['add-atom','&self', T], [empty]]]).

%From a function string: parse, extract first atom as name, register, transform to relation, assert:
assert_function(FormStr) :- sread(FormStr, Orig),
                            to_function_form(Orig, Term),
                            Term = [=, [FAtom|_], _BodyExpr],
                            atom(FAtom),
                            register_fun(FAtom),
                            translate_clause(Term, Clause),
                            assertz(Clause, Ref),
                            ( current_prolog_flag(argv, Args) -> true ; Args = [] ),
                            ( \+ ( member(Flag, Args),
                                   (Flag == silent ; Flag == '--silent' ; Flag == '-s') )
                                 -> format("~w~n---->~n", [FormStr]),
                                    clause(Head, Body, Ref),
                                    ( Body == true -> Show = Head ; Show = (Head :- Body) ),
                                    portray_clause(current_output, Show),
                                    format("^^^^^~n")
                                  ; true ).

%Collect characters until all parentheses are balanced (depth 0), accumulating codes:
grab_until_balanced(D,Acc,Cs) --> [C], { ( C=0'( -> D1 is D+1 ; C=0') -> D1 is D-1 ; D1=D ), Acc1=[C|Acc] },
                                  ( { D1=:=0 } -> { reverse(Acc1,Cs) } ; grab_until_balanced(D1,Acc1,Cs) ).

%Read a balanced (...) block if available, turn into string, then continue with rest, ignoring comment lines:
top_forms([])     --> blanks, eos.
top_forms([F|Fs]) --> blanks, "(", grab_until_balanced(1, [0'(], Cs), { string_codes(F, Cs) }, top_forms(Fs).
