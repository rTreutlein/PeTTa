:- module(unified_checker_bridge,
          [ with_unified_file_analysis/2,
            with_unified_clause_analysis/2,
            with_unified_ad_hoc_clause_analysis/2,
            with_unified_edge_facts/3,
            with_unified_preanalyzed_form/1,
            current_unified_clause_analysis/5,
            current_unified_source_variable/1,
            unified_function_result_fact/3,
            unified_function_summary/6,
            unified_checker_invalidate_event/1
          ]).

/** <module> Sole adapter between the unified analyzer and legacy compilation

This module may query PeTTa's canonical declaration/function stores and may
project already-computed edge facts into the old code generator.  It must not
contain a second recursive source checker.  All control-flow, cardinality and
result-shape decisions come from `ir_analyzer` records.
*/

:- use_module(abstract_domain).
:- use_module(ir_analyzer).
:- use_module(relational_ir).
:- use_module(library(lists)).

:- meta_predicate with_unified_file_analysis(+, 0).
:- meta_predicate with_unified_clause_analysis(+, 0).
:- meta_predicate with_unified_ad_hoc_clause_analysis(+, 0).
:- meta_predicate with_unified_edge_facts(+, +, 0).
:- meta_predicate with_unified_preanalyzed_form(0).

:- dynamic unified_function_summary/6.
% unified_function_summary(F, N, Card, ResultFacts, Effects, Diagnostics)


with_unified_file_analysis(ParsedForms, Goal) :-
    pending_clause_sources(ParsedForms, Pending),
    ( Pending == []
      -> call(Goal)
    ; solve_file_analysis(Pending, Summaries, ClauseRecords),
      current_bridge_generation(Generation),
      setup_call_cleanup(
          push_bridge_scope(scope(Generation, Summaries, ClauseRecords), Saved),
          call(Goal),
          pop_bridge_scope(Saved)) ).

with_unified_clause_analysis(Source, Goal) :-
    ( bridge_scope(scope(_, _, Records)),
      record_for_source(Records, Source, Record)
      -> with_current_clause(Record, Goal)
    ; bridge_scope(scope(_, _, Records)),
      aligned_record_for_source(Records, Source, Record)
      -> with_current_clause(Record, Goal)
    ; raw_bridge_scope(scope(_, _, StaleRecords)),
      record_for_source(StaleRecords, Source, StaleRecord),
      current_stored_summaries(Source, Summaries),
      refresh_clause_record(StaleRecord, Summaries, FreshRecord)
      -> current_bridge_generation(Generation),
         % A nested import or runnable mutation invalidates batch summaries,
         % but the occurrence's already-lowered IR is still valid. Recompute
         % the current stored call closure and reanalyze this clause against
         % those fresh, scoped summaries while its code is generated.
         setup_call_cleanup(
             push_bridge_scope(
                 scope(Generation, Summaries, [FreshRecord]), ScopeSaved),
             with_current_clause(FreshRecord, Goal),
             pop_bridge_scope(ScopeSaved))
    ; raw_bridge_scope(scope(_, _, StaleRecords)),
      aligned_record_for_source(StaleRecords, Source, StaleRecord),
      current_stored_summaries(Source, Summaries),
      refresh_clause_record(StaleRecord, Summaries, FreshRecord)
      -> current_bridge_generation(Generation),
         setup_call_cleanup(
             push_bridge_scope(
                 scope(Generation, Summaries, [FreshRecord]), ScopeSaved),
             with_current_clause(FreshRecord, Goal),
             pop_bridge_scope(ScopeSaved))
    ; stored_clause_source(Source),
      current_stored_summaries(Source, Summaries),
      fresh_clause_record(Source, Summaries, FreshRecord)
      -> current_bridge_generation(Generation),
         % Dependency recompilation may happen after the source file's batch
         % scope has ended. Rebuild the current stored call closure instead of
         % trusting invalidated persistent or batch summaries.
         setup_call_cleanup(
             push_bridge_scope(
                 scope(Generation, Summaries, [FreshRecord]), ScopeSaved),
             with_current_clause(FreshRecord, Goal),
             pop_bridge_scope(ScopeSaved))
    ; call(Goal) ).

% Compiler-generated clauses (currently lambdas) are not members of the
% parsed file batch, but their bodies need the same edge isolation as ordinary
% clauses.  Give one such clause an ephemeral record using the active batch's
% already-solved call summaries.  The record is nested under, and restores,
% the enclosing clause record; it is never added to a persistent cache.
with_unified_ad_hoc_clause_analysis(Source, Goal) :-
    ( current_analysis_summaries(Summaries),
      try_lower_source_clause(Source, lowered(IR, Env, Origins)),
      analyze_lowered_clause(IR, Summaries, Analysis)
      -> Record = clause_record(Source, IR, Env, Origins, Analysis),
         with_current_clause(Record, Goal)
    ; call(Goal) ).

current_analysis_summaries(Summaries) :-
    bridge_scope(scope(_, Summaries, _)), !.
current_analysis_summaries([]).

record_for_source([Record|_], Source, Record) :-
    Record = clause_record(Stored, _, _, _, _),
    Stored == Source, !.
record_for_source([_|Records], Source, Record) :-
    record_for_source(Records, Source, Record).

aligned_record_for_source(Records, Source, Record) :-
    member(StoredRecord, Records),
    StoredRecord = clause_record(Stored, _, _, _, _),
    Stored =@= Source, !,
    copy_term_nat(StoredRecord, Record),
    Record = clause_record(Aligned, _, _, _, _),
    Aligned = Source.

refresh_clause_record(clause_record(Source, IR, Env, Origins, _), Summaries,
                      clause_record(Source, IR, Env, Origins, Analysis)) :-
    analyze_lowered_clause(IR, Summaries, Analysis).

stored_clause_source(Source) :-
    catch(user:translated_from(Ref, Stored), _, fail),
    clause_property(Ref, predicate(_)),
    Stored =@= Source, !.

fresh_clause_record(Source, Summaries,
                    clause_record(Source, IR, Env, Origins, Analysis)) :-
    try_lower_source_clause(Source, lowered(IR, Env, Origins)),
    analyze_lowered_clause(IR, Summaries, Analysis).

with_current_clause(Record, Goal) :-
    setup_call_cleanup(
        push_current_clause(Record, Saved),
        call(Goal),
        pop_current_clause(Saved)).

with_unified_edge_facts(SourceCondition, Truth, Goal) :-
    ( current_unified_clause_analysis(_, _, Env, Origins, Analysis),
      once(( origin_result_id(Origins, SourceCondition, TestId),
             analysis_node_state(Analysis, TestId, BaseState) ))
      -> ( analysis_edge_state(Analysis, TestId, Truth, EdgeState)
           -> project_edge_types(Env, BaseState, EdgeState, Goal)
         ; isolate_environment(Env, Goal) )
    ; call(Goal) ).

with_unified_preanalyzed_form(Goal) :-
    ( catch(b_getval('$unified_source_form', Saved0), _, fail)
      -> Saved = Saved0
    ; Saved = false ),
    setup_call_cleanup(
        b_setval('$unified_source_form', true),
        call(Goal),
        b_setval('$unified_source_form', Saved)).

current_unified_clause_analysis(Source, IR, Env, Origins, Analysis) :-
    catch(b_getval('$unified_current_clause', Record), _, fail),
    Record = clause_record(Source, IR, Env, Origins, Analysis).

current_unified_source_variable(Var) :-
    var(Var),
    current_unified_clause_analysis(_, _, Env, _, _),
    member(binding(_, Stored), Env),
    Stored == Var, !.

unified_function_result_fact(F, N, Fact) :-
    current_unified_clause_analysis(_, _, _, _, _),
    ( bridge_scope(scope(_, Summaries, _)),
      member(function_summary(F, N, _, ScopedFacts, _, _), Summaries)
      -> Facts = ScopedFacts
    ; unified_function_summary(F, N, _, Facts, _, _) ),
    member(Stored, Facts),
    Stored =@= Fact, !.

% Summaries do not yet carry dependency edges.  Clearing the complete cache is
% the conservative contract until decl/effect/clause/constructor dependencies
% are part of the summary record and graph invalidation can be selective.
unified_checker_invalidate_event(Event) :-
    maybe_invalidate_active_scope(Event),
    retractall(unified_function_summary(_, _, _, _, _, _)).

maybe_invalidate_active_scope(clause_changed(_, prevalidated)) :- !.
maybe_invalidate_active_scope(clause_changed(_, derived)) :- !.
% Ordinary source forms belong to the batch being compiled. Runtime mutation
% from a runnable does not enter this scope and invalidates the generation.
maybe_invalidate_active_scope(_) :-
    catch(b_getval('$unified_source_form', true), _, fail), !.
maybe_invalidate_active_scope(_) :- bump_bridge_generation.



% -- Batch solve ---------------------------------------------------------

pending_clause_sources(ParsedForms, Pending) :-
    pending_clause_sources_(ParsedForms, Pending).

pending_clause_sources_([], []).
pending_clause_sources_([parsed(function, _, _, Term)|Forms], Pending) :- !,
    ( Term = [Eq, [F|Args], _], Eq == (=), atom(F), is_list(Args)
      -> length(Args, N),
         Pending = [clause_source(F, N, Term)|Rest]
    ; Pending = Rest ),
    pending_clause_sources_(Forms, Rest).
pending_clause_sources_([_|Forms], Pending) :-
    pending_clause_sources_(Forms, Pending).

solve_file_analysis(Pending, Summaries, ClauseRecords) :-
    unified_checker_invalidate_event(new_file_batch),
    touched_keys(Pending, Keys),
    clause_universe(Keys, Pending, ClauseSources),
    prepare_clause_universe(ClauseSources, Clauses),
    initial_touched_summaries(Keys, Initial),
    solve_fixed_point(Clauses, Keys, Initial, 0, Solved),
    include(summary_for_keys(Keys), Solved, Summaries),
    analyze_pending(Pending, Clauses, Solved, ClauseRecords).

touched_keys(Pending, Keys) :-
    findall(F/N, member(clause_source(F, N, _), Pending), Keys0),
    sort(Keys0, Keys).

clause_universe(Keys, Pending, Clauses) :-
    findall(clause_source(F, N, Source),
            ( member(F/N, Keys),
              existing_clause_source(F, N, Source) ),
            Existing),
    append(Existing, Pending, Clauses).

existing_clause_source(F, N, Source) :-
    catch(user:translated_from(Ref, Source), _, fail),
    clause_property(Ref, predicate(_)),
    Source = [Eq, [F|Args], _], Eq == (=),
    is_list(Args), length(Args, N).

% Recompilation runs outside the file solver which originally supplied call
% summaries.  Reusing that solver's records after a mutation is unsound, while
% analyzing the consumer alone loses result-shape facts from unchanged callees.
% Build a fresh, invocation-local fixed point over the target's direct-call
% transitive closure in the clauses which are executable now.  Mutual recursion
% is naturally retained because closure is computed before solve_fixed_point/5.
% The target is added when it is a not-yet-stored pending clause following a
% mutation in the same source batch.  Nothing from this computation is asserted
% into unified_function_summary/6.
current_stored_summaries(Source, Summaries) :-
    source_clause_key(Source, Root),
    current_stored_clause_sources(Stored),
    ensure_source_in_universe(Source, Root, Stored, Sources),
    prepare_clause_universe(Sources, Prepared),
    reachable_prepared_keys([Root], Prepared, Keys),
    include(prepared_for_keys(Keys), Prepared, Relevant),
    initial_touched_summaries(Keys, Initial),
    solve_fixed_point(Relevant, Keys, Initial, 0, Summaries).

source_clause_key([Eq, [F|Args], _], F/N) :-
    Eq == (=), atom(F), is_list(Args), length(Args, N).

current_stored_clause_sources(Sources) :-
    findall(clause_source(F, N, Source),
            ( catch(user:translated_from(Ref, Source), _, fail),
              clause_property(Ref, predicate(_)),
              source_clause_key(Source, F/N) ),
            Sources).

ensure_source_in_universe(Source, Root, Stored, Sources) :-
    ( member(clause_source(F, N, Existing), Stored),
      Root = F/N,
      Existing =@= Source
      -> Sources = Stored
    ; clause_source_for_key(Root, Source, Clause),
      Sources = [Clause|Stored] ).

clause_source_for_key(F/N, Source, clause_source(F, N, Source)).

reachable_prepared_keys(Roots, Prepared, Keys) :-
    findall(F/N, member(prepared_clause(F, N, _, _), Prepared), Available0),
    sort(Available0, Available),
    include(key_in(Available), Roots, Seeds0),
    sort(Seeds0, Seeds),
    reachable_prepared_keys_(Seeds, Prepared, Available, Keys).

reachable_prepared_keys_(Current, Prepared, Available, Keys) :-
    findall(Callee,
            ( member(Owner, Current),
              member(Clause, Prepared),
              prepared_clause_key(Clause, Owner),
              prepared_clause_call_key(Clause, Callee),
              memberchk(Callee, Available) ),
            Called),
    append(Current, Called, Expanded0),
    sort(Expanded0, Expanded),
    ( Expanded == Current
      -> Keys = Current
    ; reachable_prepared_keys_(Expanded, Prepared, Available, Keys) ).

prepared_clause_key(prepared_clause(F, N, _, _), F/N).

prepared_clause_call_key(
        prepared_clause(_, _, _, lowered(IR, _, _)), F/N) :-
    ir_node(IR, call(_, F, Args)),
    atom(F), is_list(Args), length(Args, N).

prepared_for_keys(Keys, Clause) :-
    prepared_clause_key(Clause, Key),
    memberchk(Key, Keys).

key_in(Keys, Key) :- memberchk(Key, Keys).

prepare_clause_universe([], []).
prepare_clause_universe([clause_source(F, N, Source)|Sources],
                        [prepared_clause(F, N, Source, Lowered)|Clauses]) :-
    try_lower_source_clause(Source, Lowered),
    prepare_clause_universe(Sources, Clauses).

initial_touched_summaries([], []).
initial_touched_summaries([F/N|Keys],
                          [function_summary(F, N, Card, [], [], [initial])|Rest]) :-
    declared_call_card(F, N, Card), !,
    initial_touched_summaries(Keys, Rest).
initial_touched_summaries([F/N|Keys],
                          [function_summary(F, N, card(0,many), [], [opaque],
                                            [initial])|Rest]) :-
    initial_touched_summaries(Keys, Rest).

solve_fixed_point(Clauses, Keys, Current, Iteration, Solved) :-
    analyze_clause_universe(Clauses, Current, Analyses),
    summarize_keys(Keys, Analyses, Derived),
    replace_key_summaries(Keys, Current, Derived, Next),
    ( summaries_equivalent(Current, Next)
      -> Solved = Next
    ; Iteration >= 31
      -> Solved = Next
    ; NextIteration is Iteration + 1,
      solve_fixed_point(Clauses, Keys, Next, NextIteration, Solved) ).

analyze_clause_universe([], _, []).
analyze_clause_universe([prepared_clause(F, N, Source, Lowered)|Clauses], Summaries,
                        [clause_result(F, N, Source, Outcome)|Results]) :-
    analyze_prepared_clause(Lowered, Summaries, Outcome),
    analyze_clause_universe(Clauses, Summaries, Results).

analyze_pending([], _, _, []).
analyze_pending([clause_source(_, _, Source)|Pending], Clauses, Summaries,
                Records) :-
    ( prepared_for_source(Clauses, Source, Lowered)
      -> analyze_prepared_clause(Lowered, Summaries, Outcome)
    ; Outcome = unsupported(missing_prepared_clause) ),
    ( Outcome = analyzed(IR, Env, Origins, Analysis)
      -> Records = [clause_record(Source, IR, Env, Origins, Analysis)|Rest]
    ; Records = Rest ),
    analyze_pending(Pending, Clauses, Summaries, Rest).

prepared_for_source([prepared_clause(_, _, Stored, Lowered)|_], Source,
                    Lowered) :-
    Stored == Source, !.
prepared_for_source([_|Clauses], Source, Lowered) :-
    prepared_for_source(Clauses, Source, Lowered).

try_lower_source_clause(Source, Outcome) :-
    catch(( lower_clause_with_origins(Source, IR, _, Env, Origins)
            -> Outcome = lowered(IR, Env, Origins)
          ; Outcome = unsupported(lowering_failed) ),
          Error,
          unsupported_lowering_error(Error, Outcome)).

unsupported_lowering_error(error(domain_error(Domain, _), Context),
                           unsupported(Domain)) :-
    memberchk(Domain, [case_pair, let_binding, nonempty_sequence]),
    lowering_error_context(Context), !.
unsupported_lowering_error(Error, _) :- throw(Error).

lowering_error_context(lower_expr/4).
lowering_error_context(relational_ir:lower_expr/4).

analyze_prepared_clause(unsupported(Reason), _, unsupported(Reason)) :- !.
analyze_prepared_clause(lowered(IR, Env, Origins), Summaries, Outcome) :-
    ( analyze_lowered_clause(IR, Summaries, Analysis)
      -> Outcome = analyzed(IR, Env, Origins, Analysis)
    ; Outcome = unsupported(analysis_failed) ).

analyze_lowered_clause(IR, Summaries, Analysis) :-
    state_empty(State),
    Options = [resolve_type(unified_checker_bridge:bridge_resolve_type),
               constructor_signature(
                   unified_checker_bridge:bridge_constructor_signature),
               resolve_call(
                   unified_checker_bridge:bridge_resolve_call(Summaries))],
    analyze_ir(IR, State, Options, Analysis).

summarize_keys([], _, []).
summarize_keys([F/N|Keys], Analyses,
               [function_summary(F, N, Card, Facts, Effects, Diagnostics)|Rest]) :-
    include(clause_result_key(F, N), Analyses, FunctionAnalyses),
    summarize_function(F, N, FunctionAnalyses,
                       Card, Facts, Effects, Diagnostics),
    summarize_keys(Keys, Analyses, Rest).

clause_result_key(F, N, clause_result(F, N, _, _)).

summarize_function(F, N, Results, Card, Facts, Effects, Diagnostics) :-
    findall(Reason,
            member(clause_result(F, N, _, unsupported(Reason)), Results),
            Unsupported),
    ( Unsupported = [_|_]
      -> Card = card(0,many), Facts = [], Effects = [opaque],
         sort(Unsupported, Reasons),
         Diagnostics = [unsupported_clauses(Reasons)]
    ; maplist(clause_analysis, Results, Analyses),
      analyses_common_result_facts(Analyses, Facts),
      analyses_effects(Analyses, Effects0), sort(Effects0, Effects),
      analyses_diagnostics(Analyses, Diagnostics0),
      sort(Diagnostics0, Diagnostics),
      ( declared_call_card(F, N, DeclaredCard)
        -> Card = DeclaredCard
      ; analyses_choice_card(Analyses, Card) ) ).

clause_analysis(clause_result(_, _, _, analyzed(_, _, _, Analysis)), Analysis).

analyses_common_result_facts([], []).
analyses_common_result_facts([Analysis|Analyses], Facts) :-
    analysis_result_facts(Analysis, First),
    foldl(intersect_analysis_result_facts, Analyses, First, Common),
    include(exportable_fact, Common, Facts0),
    variant_dedup(Facts0, Facts).

analysis_result_facts(Analysis, Facts) :-
    analysis_result(Analysis, Result),
    analysis_state(Analysis, State),
    state_facts(State, Result, Facts).

intersect_analysis_result_facts(Analysis, Facts0, Facts) :-
    analysis_result_facts(Analysis, Other),
    include(fact_in(Other), Facts0, Facts).

fact_in(Facts, Fact) :- member(Stored, Facts), Stored =@= Fact, !.

exportable_fact(type(Type)) :- ground(Type).
exportable_fact(proper_bool).
exportable_fact(proper_list).
exportable_fact(nonempty_list).
exportable_fact(expr).
exportable_fact(number).
exportable_fact(ground).
exportable_fact(nonvar).
exportable_fact(duplicate_free).

analyses_effects([], []).
analyses_effects([Analysis|Analyses], Effects) :-
    analysis_effects(Analysis, Here),
    analyses_effects(Analyses, Rest),
    append(Here, Rest, Effects).

analyses_diagnostics([], []).
analyses_diagnostics([Analysis|Analyses], Diagnostics) :-
    analysis_diagnostics(Analysis, Here),
    analyses_diagnostics(Analyses, Rest),
    append(Here, Rest, Diagnostics).

analyses_choice_card([], card(0,0)).
analyses_choice_card([Analysis|Analyses], Card) :-
    analysis_card(Analysis, First),
    foldl(choice_analysis_card, Analyses, First, Card).

choice_analysis_card(Analysis, Card0, Card) :-
    analysis_card(Analysis, Other),
    card_choice(Card0, Other, Card).

replace_key_summaries(Keys, Current, Derived, Next) :-
    exclude(summary_for_keys(Keys), Current, External),
    append(Derived, External, Next0),
    sort(Next0, Next).

summary_for_keys(Keys, function_summary(F, N, _, _, _, _)) :-
    memberchk(F/N, Keys).

summaries_equivalent(A, B) :- A =@= B.

% -- Legacy resolvers ----------------------------------------------------

bridge_resolve_type(declared_arg(F, Arity, Index), Type) :-
    findall(ATs, user:fn_decl_arity(F, Arity, ATs, _), Declarations0),
    variant_dedup(Declarations0, Declarations),
    Declarations = [ArgTypes],
    nth0(Index, ArgTypes, Type).

bridge_constructor_signature(Tag, Arity, ArgTypes, ResultType) :-
    atom(Tag), integer(Arity),
    findall(ATs-OT,
            user:fn_decl_arity(Tag, Arity, ATs, OT),
            Candidates0),
    variant_dedup(Candidates0, Candidates),
    Candidates = [ArgTypes-ResultType].

bridge_resolve_call(Summaries, F, ArgIds, _, Resolution) :-
    atom(F), length(ArgIds, N),
    ( member(function_summary(F, N, Card, Facts, Effects, _), Summaries)
      -> facts_posts(Facts, Posts),
         Resolution = summary(Posts, Card, Effects)
    ; unified_function_summary(F, N, Card, Facts, Effects, _)
      -> facts_posts(Facts, Posts),
         Resolution = summary(Posts, Card, Effects)
    ; declared_call_card(F, N, Card)
      -> declared_result_facts(F, N, Facts),
         facts_posts(Facts, Posts),
         Resolution = summary(Posts, Card, [call(F/N)])
    ; \+ catch(user:fun(F), _, fail)
      -> Resolution = data
    ; Resolution = unknown ).

declared_result_facts(F, N, Facts) :-
    findall(OT, user:fn_decl_arity(F, N, _, OT), Outputs0),
    variant_dedup(Outputs0, [Output]), !,
    ( ground(Output) -> Facts = [type(Output)] ; Facts = [] ).
declared_result_facts(_, _, []).

facts_posts([], []).
facts_posts([Fact|Facts], [ensure(result, Fact)|Posts]) :-
    facts_posts(Facts, Posts).

declared_call_card(F, N, Card) :-
    catch(user:fn_determinism(F, N, Det), _, fail),
    declared_det_card(Det, Card).

declared_det_card(det, card(1,1)).
declared_det_card(semidet, card(0,1)).
declared_det_card(nondet, card(0,many)).
declared_det_card(effect(det), card(1,1)).
declared_det_card(effect(semidet), card(0,1)).
declared_det_card(effect(nondet), card(0,many)).


% -- Projection into code generation -----------------------------------

project_edge_types(Env, BaseState, EdgeState, Goal) :-
    edge_typed_variables(Env, BaseState, EdgeState, Typed),
    snapshot_environment_attrs(Env, OriginalAttrs),
    copy_term(OriginalAttrs, BranchAttrs),
    setup_call_cleanup(
        ( install_environment_attrs(Env, BranchAttrs),
          apply_edge_types(Typed) ),
        ( call(Goal),
          branch_inference_updates(
              Env, Typed, OriginalAttrs, BranchAttrs, Updates) ),
        restore_environment_attrs(Env, OriginalAttrs, Updates)).

% Even an analyzer-proven unreachable arm must be compiled for runtime code
% shape, but none of the facts or inference constraints learned while doing so
% may escape.  This is the no-edge counterpart of project_edge_types/4.
isolate_environment(Env, Goal) :-
    snapshot_environment_attrs(Env, OriginalAttrs),
    copy_term(OriginalAttrs, BranchAttrs),
    setup_call_cleanup(
        install_environment_attrs(Env, BranchAttrs),
        call(Goal),
        install_environment_attrs(Env, OriginalAttrs)).

restore_environment_attrs(Env, OriginalAttrs, Updates) :-
    install_environment_attrs(Env, OriginalAttrs),
    ( var(Updates) -> true ; apply_inference_updates(Updates) ).

% Branch isolation must not discard genuine input requirements discovered by
% legacy inference.  Preserve only bindings of the active inference engine's
% own open parameter variables, and never preserve a binding for a variable
% whose type came from the analyzed edge itself.  Thus arithmetic in either
% reachable arm can still infer a Number parameter, while a successful
% constructor match cannot specialize a declared polymorphic parameter.
branch_inference_updates([], _, [], [], []).
branch_inference_updates([binding(_, Var)|Bindings], Typed,
                         [attrs(OriginalKnown, _, _)|OriginalAttrs],
                         [attrs(BranchKnown, _, _)|BranchAttrs], Updates) :- !,
    ( \+ typed_variable(Typed, Var),
      OriginalKnown = some([OriginalType]), var(OriginalType),
      BranchKnown = some([BranchType]), nonvar(BranchType),
      current_inference_assumption(Var, OriginalType),
      concrete_inference_constraint(BranchType)
      -> Updates = [bind(OriginalType, BranchType)|Rest]
    ; Updates = Rest ),
    branch_inference_updates(Bindings, Typed, OriginalAttrs, BranchAttrs, Rest).
branch_inference_updates([_|Bindings], Typed, [_|OriginalAttrs],
                         [_|BranchAttrs], Updates) :-
    branch_inference_updates(Bindings, Typed, OriginalAttrs, BranchAttrs, Updates).

typed_variable([typed(Stored, _)|_], Var) :- Stored == Var, !.
typed_variable([_|Typed], Var) :- typed_variable(Typed, Var).

current_inference_assumption(Var, Type) :-
    catch(b_getval('$assumptions', Pairs), _, fail),
    member(a(StoredVar, StoredType), Pairs),
    StoredVar == Var,
    StoredType == Type, !.

concrete_inference_constraint(Type) :-
    nonvar(Type),
    \+ catch(user:wildcard_type_t(Type), _, fail),
    \+ catch(user:unknown_candidate(Type), _, fail).

apply_inference_updates([]).
apply_inference_updates([bind(Open, Type)|Updates]) :-
    Open = Type,
    apply_inference_updates(Updates).

edge_typed_variables([], _, _, []).
edge_typed_variables([binding(Id, Var)|Bindings], BaseState, State, Typed) :- !,
    findall(Type,
            ( state_type_fact(State, Id, Type),
              \+ state_has_fact(BaseState, Id, type(Type)),
              concrete_projectable_type(Type),
              type_is_new_for_variable(Var, Type) ),
            Types0),
    variant_dedup(Types0, Types),
    ( Types == [] -> Typed = Rest ; Typed = [typed(Var, Types)|Rest] ),
    edge_typed_variables(Bindings, BaseState, State, Rest).
edge_typed_variables([_|Bindings], BaseState, State, Typed) :-
    edge_typed_variables(Bindings, BaseState, State, Typed).

state_type_fact(State, Id, Type) :-
    state_has_fact(State, Id, Fact),
    Fact = type(Type).

concrete_projectable_type(Type) :-
    ground(Type),
    \+ catch(user:wildcard_type_t(Type), _, fail).

type_is_new_for_variable(Var, Type) :-
    ( catch(user:known_candidates(Var, Known), _, fail)
      -> \+ ( member(Stored, Known), Stored =@= Type )
    ; true ).

apply_edge_types([]).
apply_edge_types([typed(Var, Types)|Typed]) :-
    apply_variable_types(Types, Var),
    apply_edge_types(Typed).

% These are branch-local refinements, not new constraints on the declaration's
% type variables. add_known_type/2 deliberately binds an existing open tknown
% candidate, which would turn `-[det]-> $t ...` into one concrete type merely
% because a conditional pattern inspected it. Replace only the temporary
% candidate view; restore_environment_attrs/2 reinstates the outer view afterward.
apply_variable_types(Types, Var) :-
    put_attr(Var, tknown, Types).

snapshot_environment_attrs([], []).
snapshot_environment_attrs([binding(_, Var)|Bindings],
                           [attrs(TKnown, MReq, ProperList)|Attrs]) :- !,
    snapshot_attribute(Var, tknown, TKnown),
    snapshot_attribute(Var, mreq, MReq),
    snapshot_attribute(Var, proper_list_cert, ProperList),
    snapshot_environment_attrs(Bindings, Attrs).
snapshot_environment_attrs([_|Bindings], [attrs(none, none, none)|Attrs]) :-
    snapshot_environment_attrs(Bindings, Attrs).

snapshot_attribute(Var, Module, some(Value)) :-
    get_attr(Var, Module, Value), !.
snapshot_attribute(_, _, none).

install_environment_attrs([], []).
install_environment_attrs([binding(_, Var)|Bindings],
                          [attrs(TKnown, MReq, ProperList)|Attrs]) :- !,
    del_attrs(Var),
    install_attribute(Var, tknown, TKnown),
    install_attribute(Var, mreq, MReq),
    install_attribute(Var, proper_list_cert, ProperList),
    install_environment_attrs(Bindings, Attrs).
install_environment_attrs([_|Bindings], [_|Attrs]) :-
    install_environment_attrs(Bindings, Attrs).

install_attribute(_, _, none) :- !.
install_attribute(Var, Module, some(Value)) :-
    put_attr(Var, Module, Value).


% -- Scope and set helpers ----------------------------------------------

bridge_scope(Scope) :-
    raw_bridge_scope(Scope),
    Scope = scope(Generation, _, _),
    current_bridge_generation(Current),
    Generation =:= Current.

raw_bridge_scope(Scope) :-
    catch(b_getval('$unified_checker_scope', Scope), _, fail),
    Scope = scope(_, _, _).

current_bridge_generation(Generation) :-
    ( catch(nb_getval('$unified_checker_generation', Stored), _, fail)
      -> Generation = Stored
    ; Generation = 0,
      nb_setval('$unified_checker_generation', Generation) ).

bump_bridge_generation :-
    current_bridge_generation(Current),
    Next is Current + 1,
    nb_setval('$unified_checker_generation', Next).

push_bridge_scope(Scope, saved(Had, Previous)) :-
    ( catch(b_getval('$unified_checker_scope', Previous0), _, fail)
      -> Had = yes, Previous = Previous0
    ; Had = no, Previous = none ),
    b_setval('$unified_checker_scope', Scope).

pop_bridge_scope(saved(yes, Previous)) :- !,
    b_setval('$unified_checker_scope', Previous).
pop_bridge_scope(_) :- b_setval('$unified_checker_scope', inactive).

push_current_clause(Record, saved(Had, Previous)) :-
    ( catch(b_getval('$unified_current_clause', Previous0), _, fail)
      -> Had = yes, Previous = Previous0
    ; Had = no, Previous = none ),
    b_setval('$unified_current_clause', Record).

pop_current_clause(saved(yes, Previous)) :- !,
    b_setval('$unified_current_clause', Previous).
pop_current_clause(_) :- b_setval('$unified_current_clause', inactive).

variant_dedup([], []).
variant_dedup([X|Xs], Ys) :-
    ( variant_member_local(X, Xs)
      -> variant_dedup(Xs, Ys)
    ; Ys = [X|Rest], variant_dedup(Xs, Rest) ).

variant_member_local(X, [Y|_]) :- X =@= Y, !.
variant_member_local(X, [_|Ys]) :- variant_member_local(X, Ys).


:- begin_tests(unified_checker_bridge).

test(open_types_do_not_cross_summary_boundary) :-
    \+ exportable_fact(type(_)),
    exportable_fact(type('Bool')).

test(unsupported_lowering_is_a_clause_local_fallback) :-
    Source = [=, [fallback_case, X], [case, X, [bad]]],
    try_lower_source_clause(Source, unsupported(case_pair)),
    var(X).

test(variant_clauses_keep_occurrence_identity) :-
    First = [=, [duplicate_identity, X], [==, X, a]],
    Second = [=, [duplicate_identity, Y], [==, Y, a]],
    Pending = [clause_source(duplicate_identity, 1, First),
               clause_source(duplicate_identity, 1, Second)],
    clause_universe([duplicate_identity/1], Pending, Universe),
    prepare_clause_universe(Universe, Prepared),
    prepared_for_source(Prepared, First, _),
    prepared_for_source(Prepared, Second, _),
    length(Prepared, 2),
    X \== Y.

test(variant_record_is_aligned_to_recompiled_source) :-
    Stored = [=, [aligned_clause, X], [pair, X, Y]],
    StoredRecord = clause_record(
                       Stored, aligned_ir,
                       [binding(id(1), X), binding(id(2), Y)],
                       [origin(id(1), X), origin(id(2), Y)], aligned_analysis),
    copy_term_nat(Stored, Source),
    Source = [=, [aligned_clause, SourceX], [pair, SourceX, SourceY]],
    aligned_record_for_source([StoredRecord], Source, Record),
    Record = clause_record(Aligned, aligned_ir,
                           [binding(id(1), EnvX), binding(id(2), EnvY)],
                           _, aligned_analysis),
    Aligned == Source,
    EnvX == SourceX,
    EnvY == SourceY,
    X \== SourceX,
    Y \== SourceY.

test(recompile_summary_universe_is_transitive_and_scc_complete) :-
    Sources = [
        clause_source(recompile_consumer, 0,
                      [=, [recompile_consumer], [recompile_producer]]),
        clause_source(recompile_producer, 0,
                      [=, [recompile_producer], [recompile_consumer]]),
        clause_source(recompile_unrelated, 0,
                      [=, [recompile_unrelated], true])
    ],
    prepare_clause_universe(Sources, Prepared),
    reachable_prepared_keys([recompile_consumer/0], Prepared, Keys),
    assertion(memberchk(recompile_consumer/0, Keys)),
    assertion(memberchk(recompile_producer/0, Keys)),
    assertion(\+ memberchk(recompile_unrelated/0, Keys)).

test(branch_projection_does_not_bind_parametric_candidate,
     [cleanup(del_attrs(Value))]) :-
    put_attr(Value, tknown, [OpenType]),
    apply_variable_types(['Goal'], Value),
    var(OpenType),
    get_attr(Value, tknown, ['Goal']).

test(branch_snapshot_restores_all_source_variable_attrs,
     [cleanup((del_attrs(Matched), del_attrs(Derived)))]) :-
    put_attr(Matched, tknown, [outer_type]),
    Env = [binding(id(1), Matched), binding(id(2), Derived)],
    snapshot_environment_attrs(Env, OriginalAttrs),
    copy_term(OriginalAttrs, BranchAttrs),
    install_environment_attrs(Env, BranchAttrs),
    put_attr(Matched, tknown, [edge_type]),
    put_attr(Derived, tknown, [derived_type]),
    install_environment_attrs(Env, OriginalAttrs),
    get_attr(Matched, tknown, [outer_type]),
    \+ get_attr(Derived, tknown, _).

test(branch_snapshot_preserves_external_open_type_identity,
     [cleanup(del_attrs(Value))]) :-
    put_attr(Value, tknown, [OpenType]),
    Env = [binding(id(1), Value)],
    snapshot_environment_attrs(Env, OriginalAttrs),
    copy_term(OriginalAttrs, BranchAttrs),
    install_environment_attrs(Env, BranchAttrs),
    get_attr(Value, tknown, [BranchOpen]),
    BranchOpen = 'TemporaryType',
    var(OpenType),
    install_environment_attrs(Env, OriginalAttrs),
    get_attr(Value, tknown, [Restored]),
    Restored == OpenType.

test(reachable_branch_preserves_inference_requirement) :-
    catch(b_getval('$assumptions', Saved0), _, Saved0 = []),
    setup_call_cleanup(
        b_setval('$assumptions', [a(Value, OpenType)]),
        ( branch_inference_updates(
              [binding(id(1), Value)], [],
              [attrs(some([OpenType]), none, none)],
              [attrs(some(['Number']), none, none)], Updates),
          apply_inference_updates(Updates),
          assertion(OpenType == 'Number') ),
        b_setval('$assumptions', Saved0)).

test(edge_type_is_not_promoted_to_inference_requirement) :-
    catch(b_getval('$assumptions', Saved0), _, Saved0 = []),
    setup_call_cleanup(
        b_setval('$assumptions', [a(Value, OpenType)]),
        ( branch_inference_updates(
              [binding(id(1), Value)], [typed(Value, ['Number'])],
              [attrs(some([OpenType]), none, none)],
              [attrs(some(['Number']), none, none)], Updates),
          assertion(Updates == []),
          assertion(var(OpenType)) ),
        b_setval('$assumptions', Saved0)).

test(variant_signature_candidates_collapse_to_one) :-
    Candidates0 = [[A]-result(A), [B]-result(B)],
    variant_dedup(Candidates0, Candidates),
    Candidates = [[Type]-result(Type)].

test(incompatible_signature_candidates_remain_ambiguous) :-
    Candidates0 = [['Number']-'Number', ['Atom']-'Atom'],
    variant_dedup(Candidates0, Candidates),
    Candidates = [_, _].

test(declared_arg_resolution_is_arity_qualified,
     [setup((assertz((user:fn_decl_arity(
                          bridge_arity_probe, 1,
                          ['Number'], 'Number'))),
             assertz((user:fn_decl_arity(
                          bridge_arity_probe, 2,
                          ['Atom', 'String'], 'Bool'))))),
      cleanup(retractall(user:fn_decl_arity(
                             bridge_arity_probe, _, _, _)))]) :-
    bridge_resolve_type(declared_arg(bridge_arity_probe, 2, 0), 'Atom'),
    bridge_resolve_type(declared_arg(bridge_arity_probe, 2, 1), 'String'),
    \+ bridge_resolve_type(declared_arg(bridge_arity_probe, 1, 1), _).

test(constructor_resolver_collapses_variant_type_signatures,
     [setup((assertz((user:fn_decl_arity(
                          bridge_duplicate_ctor, 1,
                          [A], result(A)))),
             assertz((user:fn_decl_arity(
                          bridge_duplicate_ctor, 1,
                          [B], result(B)))))),
      cleanup(retractall(user:fn_decl_arity(
                             bridge_duplicate_ctor, _, _, _)))]) :-
    bridge_constructor_signature(
        bridge_duplicate_ctor, 1, [Type], result(Type)).

test(constructor_resolver_refuses_genuine_same_arity_ambiguity,
     [setup((assertz((user:fn_decl_arity(
                          bridge_ambiguous_ctor, 1,
                          ['Number'], number_result))),
             assertz((user:fn_decl_arity(
                          bridge_ambiguous_ctor, 1,
                          ['Atom'], atom_result))))),
      cleanup(retractall(user:fn_decl_arity(
                             bridge_ambiguous_ctor, _, _, _)))]) :-
    \+ bridge_constructor_signature(
           bridge_ambiguous_ctor, 1, _, _).

test(semantic_mutation_clears_transitive_summary_cache,
     [setup((assertz(unified_checker_bridge:unified_function_summary(
                         test_callee, 0, card(1,1), [proper_bool], [], [])),
             assertz(unified_checker_bridge:unified_function_summary(
                         test_caller, 0, card(1,1), [proper_bool], [], [])))),
      cleanup((retractall(unified_checker_bridge:unified_function_summary(
                              test_callee, _, _, _, _, _)),
               retractall(unified_checker_bridge:unified_function_summary(
                              test_caller, _, _, _, _, _))))]) :-
    unified_checker_invalidate_event(clause_changed(test_callee/0, runtime)),
    \+ unified_checker_bridge:unified_function_summary(
           test_callee, _, _, _, _, _),
    \+ unified_checker_bridge:unified_function_summary(
           test_caller, _, _, _, _, _).

test(scoped_summary_shadows_persistent_facts,
     [setup(assertz(unified_checker_bridge:unified_function_summary(
                        shadowed, 0, card(1,1), [proper_bool], [], []))),
      cleanup(retractall(unified_checker_bridge:unified_function_summary(
                             shadowed, _, _, _, _, _)))]) :-
    current_bridge_generation(Generation),
    Record = clause_record(scope_test, scope_ir, [], [], scope_analysis),
    setup_call_cleanup(
        push_bridge_scope(
            scope(Generation,
                  [function_summary(shadowed, 0, card(1,1), [], [], [])],
                  [Record]),
            Saved),
        setup_call_cleanup(
            push_current_clause(Record, ClauseSaved),
            \+ unified_function_result_fact(shadowed, 0, proper_bool),
            pop_current_clause(ClauseSaved)),
        pop_bridge_scope(Saved)).

test(runtime_mutation_invalidates_active_scope) :-
    current_bridge_generation(Generation),
    Record = clause_record(scope_test, scope_ir, [], [], scope_analysis),
    setup_call_cleanup(
        push_bridge_scope(
            scope(Generation,
                  [function_summary(producer, 0, card(1,1),
                                    [proper_bool], [], [])],
                  [Record]),
            Saved),
        setup_call_cleanup(
            push_current_clause(Record, ClauseSaved),
            ( unified_function_result_fact(producer, 0, proper_bool),
              unified_checker_invalidate_event(
                  clause_changed(producer/0, runtime)),
              \+ unified_function_result_fact(producer, 0, proper_bool),
              \+ bridge_scope(_) ),
            pop_current_clause(ClauseSaved)),
        pop_bridge_scope(Saved)).

test(source_form_mutation_keeps_active_scope) :-
    current_bridge_generation(Generation),
    Record = clause_record(scope_test, scope_ir, [], [], scope_analysis),
    setup_call_cleanup(
        push_bridge_scope(
            scope(Generation,
                  [function_summary(producer, 0, card(1,1),
                                    [proper_bool], [], [])],
                  [Record]),
            Saved),
        setup_call_cleanup(
            push_current_clause(Record, ClauseSaved),
            ( with_unified_preanalyzed_form(
                  unified_checker_invalidate_event(
                      declaration_changed(value, token, added))),
              unified_function_result_fact(producer, 0, proper_bool),
              bridge_scope(_) ),
            pop_current_clause(ClauseSaved)),
        pop_bridge_scope(Saved)).

:- end_tests(unified_checker_bridge).
