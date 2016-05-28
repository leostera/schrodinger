%%%-------------------------------------------------------------------
%% @doc Schrodinger's Experiment takes care of running the predicate
%% in it's own process, storing the return value and measuring the run
%% duration.
%% @end
%%%-------------------------------------------------------------------
-module(schrodinger_experiment).

-export([run/2]).

-include("schrodinger.hrl").

%%====================================================================
%% API functions
%%====================================================================

-spec run(schrodinger:observation(), schrodinger:publisher()) -> schrodinger:observation().
run(#{predicate := F}=O1, Collector) when is_function(F) ->
  Observation = O1#{ started_at => timestamp() },
  Pid=spawn( fun () ->
                 Result = F(),
                 Done = timestamp(),
                 #{ started_at := StartedAt } = Observation,
                 UpdatedObservation = Observation#{
                                        result := Result,
                                        finished_at := Done,
                                        duration := Done-StartedAt,
                                        pid := self()
                                       },
                 publish(Collector, UpdatedObservation)
             end ),
  Observation#{
    pid := Pid
   }.

%%====================================================================
%% Internal functions
%%====================================================================

-spec publish(schrodinger:publisher(), schrodinger:observation()) -> #{}.
publish(Collector, Observation) -> Collector ! Observation.

-spec timestamp() -> integer().
timestamp() -> erlang:system_time(milli_seconds).
