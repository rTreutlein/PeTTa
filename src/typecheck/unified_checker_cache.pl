:- module(unified_checker_cache,
          [ unified_summary_cache_store/2,
            unified_summary_cache_lookup/3,
            unified_summary_cache_lookup/4,
            unified_summary_cache_remove/2,
            unified_summary_cache_invalidate_event/1,
            unified_summary_cache_invalidate_event/2,
            unified_summary_cache_clear/0,
            unified_summary_cache_stats/1,
            unified_summary_cache_reset/0
          ]).

/** <module> Closed in-process cache for unified function summaries

The cache stores only ground

  function_summary(F, N, Card, ResultFacts, Effects, Diagnostics)

records and ground dependency sets.  Dependencies supplied to store/2 are
flattened, sorted and augmented with the summary's own clause, declaration and
effect dependencies.  A caller may either copy all transitive dependencies
into its row or record `summary(Callee/Arity)`.  Removal and invalidation follow
the latter edges transitively.

This module deliberately owns no source clauses, IR, attributed variables or
source-occurrence records.  It also has no dependency on the bridge or the
compiled-clause dependency graph, which keeps it usable at their integration
boundary without introducing a module cycle.

Every public operation is serialized.  Store validates the complete new row
before changing the cache.  Replacing a changed row evicts its existing
dependents before publishing the replacement; storing an identical row is a
no-op.
*/

:- dynamic cached_summary/4.
% cached_summary(F, N, FunctionSummary, Dependencies)

:- dynamic cached_summary_dependency/3.
% cached_summary_dependency(Dependency, OwnerF, OwnerN)


%!  unified_summary_cache_store(+FunctionSummary, +Dependencies) is det.
%
%   Store or replace a closed summary.  Throws on an open or malformed row.
%   Dependencies must be one flat, ground list.  `summary(F/N)` dependencies
%   are optional when the caller instead supplies a flattened transitive set.

unified_summary_cache_store(Summary, Dependencies0) :-
    validate_summary(Summary, F, N),
    validate_dependencies(Dependencies0),
    summary_self_dependencies(F, N, Self),
    append(Self, Dependencies0, All0),
    sort(All0, Dependencies),
    with_mutex(unified_checker_summary_cache,
               store_summary_locked(F, N, Summary, Dependencies)).


%!  unified_summary_cache_lookup(+F, +N, -FunctionSummary) is semidet.

unified_summary_cache_lookup(F, N, Summary) :-
    unified_summary_cache_lookup(F, N, Summary, _).


%!  unified_summary_cache_lookup(+F, +N, -FunctionSummary,
%!                               -Dependencies) is semidet.
%
%   Return detached copies.  Stored rows are ground, so no variable identity
%   can escape through this API.

unified_summary_cache_lookup(F, N, Summary, Dependencies) :-
    validate_key(F, N),
    with_mutex(unified_checker_summary_cache,
               lookup_summary_locked(F, N, Stored, StoredDependencies)),
    copy_term_nat(Stored-StoredDependencies, Summary-Dependencies).


%!  unified_summary_cache_remove(+F, +N) is det.
%
%   Remove this row and every row which depends on it through one or more
%   `summary(F/N)` edges.  Removing a missing key is harmless, but can still
%   remove a dependent row whose callee had not itself been cached.

unified_summary_cache_remove(F, N) :-
    validate_key(F, N),
    with_mutex(unified_checker_summary_cache,
               remove_keys_transitively_locked([F/N], _)).


%!  unified_summary_cache_invalidate_event(+Event) is det.

unified_summary_cache_invalidate_event(Event) :-
    unified_summary_cache_invalidate_event(Event, _).


%!  unified_summary_cache_invalidate_event(+Event, -RemovedKeys) is det.
%
%   Selectively invalidate rows affected by a semantic lifecycle event, then
%   follow summary dependency edges.  RemovedKeys is a sorted list of F/N keys
%   whose rows actually existed.  Unknown ground events conservatively clear
%   the cache; a future mutation vocabulary therefore cannot silently retain
%   stale summaries.

unified_summary_cache_invalidate_event(Event, RemovedKeys) :-
    require_ground(Event, cache_event),
    with_mutex(unified_checker_summary_cache,
               invalidate_event_locked(Event, RemovedKeys)).


%!  unified_summary_cache_clear is det.

unified_summary_cache_clear :-
    with_mutex(unified_checker_summary_cache, clear_cache_locked).


%!  unified_summary_cache_reset is det.
%
%   Explicit test/process-reset alias.  It has the same semantics as clear/0.

unified_summary_cache_reset :-
    unified_summary_cache_clear.


%!  unified_summary_cache_stats(-Stats) is det.
%
%   Stats is `cache_stats(Rows, DependencyEdges)`.

unified_summary_cache_stats(cache_stats(Rows, DependencyEdges)) :-
    with_mutex(
        unified_checker_summary_cache,
        ( aggregate_all(count, cached_summary(_, _, _, _), Rows),
          aggregate_all(count,
                        cached_summary_dependency(_, _, _),
                        DependencyEdges) )).


% -- Validation ---------------------------------------------------------

validate_summary(Summary, F, N) :-
    require_ground(Summary, function_summary),
    ( Summary = function_summary(F, N, Card, ResultFacts,
                                 Effects, Diagnostics),
      atom(F), integer(N), N >= 0,
      valid_cardinality(Card),
      is_list(ResultFacts), is_list(Effects), is_list(Diagnostics)
      -> true
    ; throw(error(domain_error(unified_function_summary, Summary),
                  unified_checker_cache)) ).

valid_cardinality(card(Lower, Upper)) :-
    memberchk(Lower, [0, 1]),
    ( memberchk(Upper, [0, 1]), Upper >= Lower
    ; Upper == many ).

validate_dependencies(Dependencies) :-
    require_ground(Dependencies, summary_dependencies),
    ( is_list(Dependencies), maplist(flat_dependency, Dependencies)
      -> true
    ; throw(error(domain_error(flat_summary_dependencies, Dependencies),
                  unified_checker_cache)) ).

flat_dependency(Dependency) :- \+ is_list(Dependency).

validate_key(F, N) :-
    ( atom(F), integer(N), N >= 0
      -> true
    ; throw(error(domain_error(function_key, F/N), unified_checker_cache)) ).

require_ground(Term, _) :- ground(Term), !.
require_ground(_, Subject) :-
    throw(error(instantiation_error,
                context(unified_checker_cache, Subject))).

summary_self_dependencies(F, N,
                          [clause_set(F/N), decl(F/N), effect(F/N)]).


% -- Store operations ---------------------------------------------------

store_summary_locked(F, N, Summary, Dependencies) :-
    ( cached_summary(F, N, Existing, ExistingDependencies),
      Existing == Summary,
      ExistingDependencies == Dependencies
      -> true
    ; remove_keys_transitively_locked([F/N], _),
      assertz(cached_summary(F, N, Summary, Dependencies)),
      forall(member(Dependency, Dependencies),
             assertz(cached_summary_dependency(Dependency, F, N))) ).

lookup_summary_locked(F, N, Summary, Dependencies) :-
    cached_summary(F, N, Summary, Dependencies).

clear_cache_locked :-
    retractall(cached_summary(_, _, _, _)),
    retractall(cached_summary_dependency(_, _, _)).

remove_keys_transitively_locked(Seeds0, RemovedKeys) :-
    sort(Seeds0, Seeds),
    dependent_key_closure(Seeds, Closure),
    findall(F/N,
            ( member(F/N, Closure), cached_summary(F, N, _, _) ),
            Existing0),
    sort(Existing0, RemovedKeys),
    forall(member(F/N, Closure), remove_one_key_locked(F, N)).

dependent_key_closure(Keys0, Closure) :-
    findall(OwnerF/OwnerN,
            ( member(F/N, Keys0),
              cached_summary_dependency(summary(F/N), OwnerF, OwnerN) ),
            Dependents),
    append(Keys0, Dependents, Expanded0),
    sort(Expanded0, Expanded),
    ( Expanded == Keys0
      -> Closure = Keys0
    ; dependent_key_closure(Expanded, Closure) ).

remove_one_key_locked(F, N) :-
    retractall(cached_summary(F, N, _, _)),
    retractall(cached_summary_dependency(_, F, N)).


% -- Event invalidation -------------------------------------------------

invalidate_event_locked(Event, RemovedKeys) :-
    ( global_cache_event(Event)
      -> all_cached_keys(Keys),
         remove_keys_transitively_locked(Keys, RemovedKeys)
    ; known_cache_event(Event)
      -> findall(F/N,
                 ( cached_summary_dependency(Dependency, F, N),
                   event_matches_dependency(Event, Dependency) ),
                 Keys0),
         sort(Keys0, Keys),
         remove_keys_transitively_locked(Keys, RemovedKeys)
    ; all_cached_keys(Keys),
      remove_keys_transitively_locked(Keys, RemovedKeys) ).

all_cached_keys(Keys) :-
    findall(F/N, cached_summary(F, N, _, _), Keys0),
    sort(Keys0, Keys).

global_cache_event(new_file_batch).
global_cache_event(broad_mutation(_)).

known_cache_event(clause_changed(F/N)) :- valid_event_key(F, N).
known_cache_event(clause_changed(F/N, _)) :- valid_event_key(F, N).
known_cache_event(declaration_changed(F/N, _)) :- valid_event_key(F, N).
known_cache_event(declaration_changed(Kind, Name, _)) :-
    atom(Kind), atom(Name).
known_cache_event(constructor_set_changed(_, _)).
known_cache_event(generated_specialization_removed(Name)) :- atom(Name).

valid_event_key(F, N) :- atom(F), integer(N), N >= 0.

event_matches_dependency(clause_changed(F/N), Dependency) :-
    event_matches_dependency(clause_changed(F/N, runtime), Dependency).
event_matches_dependency(clause_changed(F/N, _), Dependency) :-
    function_dependency_matches(F, N, Dependency).

event_matches_dependency(declaration_changed(F/N, _), Dependency) :-
    function_dependency_matches(F, N, Dependency).
event_matches_dependency(declaration_changed(Kind, Name, _),
                         declaration(Kind, Name)).
event_matches_dependency(declaration_changed(_, Name, _), late_symbol(Name)).
event_matches_dependency(declaration_changed(_, Name, _), Dependency) :-
    symbol_dependency_matches(Name, Dependency).

event_matches_dependency(constructor_set_changed(Type, _), ctor_set(Type)).

event_matches_dependency(generated_specialization_removed(Name), Dependency) :-
    symbol_dependency_matches(Name, Dependency).

function_dependency_matches(F, N, clause_set(F/N)).
function_dependency_matches(F, N, decl(F/N)).
function_dependency_matches(F, N, effect(F/N)).
function_dependency_matches(F, N, output_cert(_, F/N)).
function_dependency_matches(F, N, late_call(F/N)).
function_dependency_matches(F, _, late_symbol(F)).

symbol_dependency_matches(Name, clause_set(Name/_)).
symbol_dependency_matches(Name, decl(Name/_)).
symbol_dependency_matches(Name, effect(Name/_)).
symbol_dependency_matches(Name, output_cert(_, Name/_)).
symbol_dependency_matches(Name, late_call(Name/_)).
symbol_dependency_matches(Name, late_symbol(Name)).


% -- Tests --------------------------------------------------------------

:- begin_tests(unified_checker_cache).

test(rejects_open_summary,
     [ setup(unified_summary_cache_reset),
       cleanup(unified_summary_cache_reset),
       throws(error(instantiation_error, _)) ]) :-
    unified_summary_cache_store(
        function_summary(open_summary, 0, card(1,1), [type(_)], [], []),
        []).

test(rejects_open_dependencies,
     [ setup(unified_summary_cache_reset),
       cleanup(unified_summary_cache_reset),
       throws(error(instantiation_error, _)) ]) :-
    unified_summary_cache_store(
        function_summary(open_dependency, 0, card(1,1), [], [], []),
        [clause_set(_)]).

test(store_lookup_is_closed_and_adds_self_dependencies,
     [ setup(unified_summary_cache_reset),
       cleanup(unified_summary_cache_reset) ]) :-
    Summary = function_summary(producer, 1, card(1,1),
                               [proper_bool], [pure], []),
    unified_summary_cache_store(Summary, [ctor_set('Goal')]),
    unified_summary_cache_lookup(producer, 1, Stored, Dependencies),
    assertion(Stored == Summary),
    assertion(Dependencies ==
              [clause_set(producer/1), ctor_set('Goal'),
               decl(producer/1), effect(producer/1)]),
    unified_summary_cache_stats(cache_stats(1, 4)).

test(runtime_clause_event_is_selective,
     [ setup(unified_summary_cache_reset),
       cleanup(unified_summary_cache_reset) ]) :-
    cache_test_summary(consumer, [clause_set(callee/0)]),
    cache_test_summary(unrelated, [clause_set(other/0)]),
    unified_summary_cache_invalidate_event(
        clause_changed(callee/0, runtime), Removed),
    assertion(Removed == [consumer/0]),
    assertion(\+ unified_summary_cache_lookup(consumer, 0, _)),
    assertion(unified_summary_cache_lookup(unrelated, 0, _)).

test(prevalidated_clause_event_invalidates_clause_dependents,
     [ setup(unified_summary_cache_reset),
       cleanup(unified_summary_cache_reset) ]) :-
    cache_test_summary(early_consumer, [clause_set(later/2)]),
    cache_test_summary(unrelated, [clause_set(other/0)]),
    unified_summary_cache_invalidate_event(
        clause_changed(later/2, prevalidated), Removed),
    assertion(Removed == [early_consumer/0]),
    assertion(unified_summary_cache_lookup(unrelated, 0, _)).

test(summary_edges_invalidate_transitively,
     [ setup(unified_summary_cache_reset),
       cleanup(unified_summary_cache_reset) ]) :-
    cache_test_summary(leaf, []),
    cache_test_summary(middle, [summary(leaf/0)]),
    cache_test_summary(top, [summary(middle/0)]),
    cache_test_summary(unrelated, []),
    unified_summary_cache_invalidate_event(
        clause_changed(leaf/0, runtime), Removed),
    assertion(Removed == [leaf/0, middle/0, top/0]),
    assertion(unified_summary_cache_lookup(unrelated, 0, _)).

test(changed_replacement_removes_summary_dependents,
     [ setup(unified_summary_cache_reset),
       cleanup(unified_summary_cache_reset) ]) :-
    cache_test_summary(callee, []),
    cache_test_summary(caller, [summary(callee/0)]),
    unified_summary_cache_store(
        function_summary(callee, 0, card(0,1), [], [pure], []), []),
    assertion(unified_summary_cache_lookup(callee, 0, _)),
    assertion(\+ unified_summary_cache_lookup(caller, 0, _)).

test(declaration_and_constructor_events_are_selective,
     [ setup(unified_summary_cache_reset),
       cleanup(unified_summary_cache_reset) ]) :-
    cache_test_summary(by_origin, [declaration(origin, callee)]),
    cache_test_summary(by_constructor, [ctor_set('Goal')]),
    cache_test_summary(unrelated, []),
    unified_summary_cache_invalidate_event(
        declaration_changed(origin, callee, changed), First),
    assertion(First == [by_origin/0]),
    unified_summary_cache_invalidate_event(
        constructor_set_changed('Goal', 'GPU'), Second),
    assertion(Second == [by_constructor/0]),
    assertion(unified_summary_cache_lookup(unrelated, 0, _)).

test(function_declaration_event_invalidates_function_dependents,
     [ setup(unified_summary_cache_reset),
       cleanup(unified_summary_cache_reset) ]) :-
    cache_test_summary(caller, [decl(callee/2)]),
    cache_test_summary(unrelated, []),
    unified_summary_cache_invalidate_event(
        declaration_changed(callee/2, changed), Removed),
    assertion(Removed == [caller/0]),
    assertion(unified_summary_cache_lookup(unrelated, 0, _)).

test(generated_specialization_removal_is_symbol_selective,
     [ setup(unified_summary_cache_reset),
       cleanup(unified_summary_cache_reset) ]) :-
    unified_summary_cache_store(
        function_summary('worker_Spec_1', 1, card(1,1),
                         [proper_bool], [pure], []), []),
    cache_test_summary(spec_consumer,
                       [summary('worker_Spec_1'/1)]),
    cache_test_summary(unrelated, []),
    unified_summary_cache_invalidate_event(
        generated_specialization_removed('worker_Spec_1'), Removed),
    assertion(Removed == [spec_consumer/0, 'worker_Spec_1'/1]),
    assertion(unified_summary_cache_lookup(unrelated, 0, _)).

test(broad_event_clears_everything,
     [ setup(unified_summary_cache_reset),
       cleanup(unified_summary_cache_reset) ]) :-
    cache_test_summary(first, []),
    cache_test_summary(second, []),
    unified_summary_cache_invalidate_event(
        broad_mutation(test), Removed),
    assertion(Removed == [first/0, second/0]),
    unified_summary_cache_stats(cache_stats(0, 0)).

test(new_file_batch_uses_global_safety_boundary,
     [ setup(unified_summary_cache_reset),
       cleanup(unified_summary_cache_reset) ]) :-
    cache_test_summary(old_file_row, []),
    unified_summary_cache_invalidate_event(new_file_batch,
                                           [old_file_row/0]),
    assertion(\+ unified_summary_cache_lookup(old_file_row, 0, _)).

test(unknown_event_is_conservative,
     [ setup(unified_summary_cache_reset),
       cleanup(unified_summary_cache_reset) ]) :-
    cache_test_summary(cached, []),
    unified_summary_cache_invalidate_event(future_event(payload), [cached/0]),
    assertion(\+ unified_summary_cache_lookup(cached, 0, _)).

cache_test_summary(F, Dependencies) :-
    unified_summary_cache_store(
        function_summary(F, 0, card(1,1), [proper_bool], [pure], []),
        Dependencies).

:- end_tests(unified_checker_cache).
