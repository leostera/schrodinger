%%%-------------------------------------------------------------------
%% @doc Schrodinger's Box is responsible for spawning all candidates,
%% collecting the results, and broadcasting to all publishers.
%% @end
%%%-------------------------------------------------------------------
-module(schrodinger_box).

-export([
         start/1
        ]).

-include("schrodinger.hrl").

%%====================================================================
%% API functions
%%====================================================================

-spec start({name(), control(), candidates(), publishers(), options()}) -> observations().
start({Name, Control, Candidates, Reporters, _Options}) ->
  Observations = [ control(Control) | lists:map(fun candidate/1, Candidates) ],
  collect(Name, Observations, Reporters).

%%====================================================================
%% Internal functions
%%====================================================================

-spec collect(name(), observations(), publishers()) -> observations().
collect(Name, Observations, Reporters) -> collect(Name, Observations, Reporters, []).

-spec collect(name(), observations(), publishers(), observations()) -> observations().
collect(Name, [], Reporters, Results) ->
  publish({summary, {Name, Results}}, Reporters),
  Results;
collect(Name, Observations, Reporters, Results) ->
  receive
    #observation{pid=Pid}=Observation ->
      RemainingObservations = lists:filter(fun (Obs) ->
                                           Pid =/= Obs#observation.pid
                                       end, Observations),
      publish({measurement, {Name, Observation}}, Reporters),
      collect(Name, RemainingObservations, Reporters, [ Observation | Results ])
  after infinity -> {error, infinity}
  end.

-spec control(control()) -> observation().
control(Control) -> run(control, Control, control).
-spec candidate(candidate()) -> observation().
candidate({Name, Predicate}) -> run(Name, Predicate, candidate).

-spec run(name(), predicate(), type()) -> observation().
run(Name, Predicate, Type) ->
  schrodinger_experiment:run(#observation{
                                name=Name,
                                predicate=Predicate,
                                type=Type
                               }, self()).

-spec publish(any(), list()) -> atom().
publish(_, []) -> ok;
publish(Data, [H|T]) -> H ! Data, publish(Data, T).
