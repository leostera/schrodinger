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
run(#observation{predicate=F}=O1, Collector) when is_function(F) ->
  Observation = O1#observation{
                   started_at=timestamp()
                  },
  Pid=spawn( fun () ->
               Result = F(),
               Done = timestamp(),
               UpdatedObservation = Observation#observation {
                 result=Result,
                 finished_at=Done,
                 duration=Done-Observation#observation.started_at,
                 pid=self()
                },
               Collector ! UpdatedObservation
             end ),
  Observation#observation{
     pid=Pid
  }.

%%====================================================================
%% Internal functions
%%====================================================================

-spec timestamp() -> integer().
timestamp() -> erlang:system_time(milli_seconds).
