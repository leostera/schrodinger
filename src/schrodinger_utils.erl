%%%-------------------------------------------------------------------
%% @doc Schrodinger's Utilities
%% @end
%%%-------------------------------------------------------------------
-module(schrodinger_utils).

-export([
        publish/2,
        timestamp/0
        ]).

-include("schrodinger.hrl").

%%====================================================================
%% API functions
%%====================================================================

-spec publish(schrodinger:publisher(), schrodinger:observation()) -> #{}.
publish(Collector, Observation) -> Collector ! Observation.

-spec timestamp() -> integer().
timestamp() -> erlang:system_time(milli_seconds).
