-module(schrodinger_experiment_test).

-compile([export_all]).

fixture_name() ->
  io_lib:format("Test Observation ~p", [schrodinger_utils:timestamp()]).

fixture_predicate() ->
  fun () -> ok end.

fixture_type() ->
  candidate.

fixture_observation() -> #{
  name => fixture_name(),
  predicate => fixture_predicate(),
  type => fixture_type()
 }.

fixture_collector() ->
  self().

fixture_test_experiment() ->
  Observation = fixture_observation(),
  schrodinger_experiment:run(Observation, fixture_collector()).

run_test() ->
  Result = fixture_test_experiment(),
  true = erlang:is_map(Result),
  #{ name := Name,
     predicate := Predicate,
     type := Type,
     started_at := StartedAt,
     pid := Pid
   } = Result,
  Name = fixture_name(),
  Predicate = fixture_predicate(),
  Type = fixture_type(),
  true = StartedAt =< schrodinger_utils:timestamp(),
  true = erlang:is_pid(Pid).
