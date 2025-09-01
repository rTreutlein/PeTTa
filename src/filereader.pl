:- use_module(library(readutil)). % read_file_to_string/3
:- use_module(library(pcre)).     % re_replace/4

%Read Filename into string S and process it (S holds MeTTa code):
load_metta_file(Filename) :- read_file_to_string(Filename, S, []), process_metta_string(S).

%Extract function definitions and process each, whereby !(Z) is transformed to (= (run) (Z)):
process_metta_string(S) :- re_replace("(?m)^\\s*!\\s*\\((.*)\\)\\s*$", "(= (run) (\\1))", S, S1, [global]),
                           string_codes(S1, Cs), phrase(top_forms(Forms), Cs), maplist(assert_function, Forms).

%From a function string: parse, extract first atom as name, register, transform to relation, assert.
assert_function(FormStr) :- sread(FormStr, Term), ( Term = [=, [FAtom|_], _BodyExpr], atom(FAtom) ->
                                                    register_fun(FAtom), translate_clause(FormStr, Clause),
                                                    assertz(Clause) ; true ).

% Collect characters until all parentheses are balanced (depth 0), accumulating codes
grab_until_balanced(D,Acc,Cs) --> [C], { ( C=0'( -> D1 is D+1; C=0') -> D1 is D-1; D1=D ), Acc1=[C|Acc] },
                                         ( { D1=:=0 } -> { reverse(Acc1,Cs) } ; grab_until_balanced(D1,Acc1,Cs) ).

%Read a balanced (...) block if available, turn into string, then continue with rest, ignoring comment lines
top_forms([])     --> blanks, eos.
top_forms(Fs) --> blanks, ";", string_without("\n", _), (eol ; eos), !, top_forms(Fs).
top_forms([F|Fs]) --> blanks, "(", grab_until_balanced(1, [0'(], Cs), { string_codes(F, Cs) }, top_forms(Fs).
