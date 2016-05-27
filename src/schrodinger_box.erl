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

-type start_params() :: {schrodinger:name(), schrodinger:control(), schrodinger:candidates(), schrodinger:publishers(), schrodinger:options()}.

-spec start(start_params()) -> schrodinger:observations().
start({Name, Control, Candidates, Reporters, _Options}) ->
  Observations = [ control(Control) | lists:map(fun candidate/1, Candidates) ],
  collect(Name, Observations, Reporters).

%%====================================================================
%% Internal functions
%%====================================================================

-spec collect(schrodinger:name(), schrodinger:observations(), schrodinger:publishers()) -> schrodinger:observations().
collect(Name, Observations, Reporters) -> collect(Name, Observations, Reporters, []).

-spec collect(schrodinger:name(), schrodinger:observations(), schrodinger:publishers(), schrodinger:observations()) -> schrodinger:observations().
collect(Name, [], Reporters, Results) ->
  publish({summary, {Name, Results}}, Reporters),
  Results;
collect(Name, Observations, Reporters, Results) ->
  receive
    #{ pid := Pid}=Observation ->
      RemainingObservations = lists:filter(fun (Obs) ->
                                               #{ pid := Pid2 }=Obs,
                                               Pid =/= Pid2
                                           end, Observations),
      publish({measurement, {Name, Observation}}, Reporters),
      collect(Name, RemainingObservations, Reporters, [ Observation | Results ])
  after infinity -> {error, infinity}
  end.

-spec control(schrodinger:control()) -> schrodinger:observation().
control(Control) -> run(control, Control, control).
-spec candidate(schrodinger:candidate()) -> schrodinger:observation().
candidate({Name, Predicate}) -> run(Name, Predicate, candidate).

-spec run(schrodinger:name(), schrodinger:predicate(), schrodinger:type()) -> schrodinger:observation().
run(Name, Predicate, Type) ->
  schrodinger_experiment:run(#{
    name => Name,
    predicate => Predicate,
    type => Type
   }, self()).

-spec publish(any(), list()) -> atom().
publish(_, []) -> ok;
publish(Data, [H|T]) -> H ! Data, publish(Data, T).
