-module(schrodinger).

-export([
         experiment/3,
         experiment/5
        ]).

-include("schrodinger.hrl").

%%====================================================================
%% API functions
%%====================================================================

-spec experiment(name(), control(), candidates()) -> any().
experiment(Name, Control, Candidates) ->
  experiment(Name, Control, Candidates, default_publishers(), default_options()).

-spec experiment(name(), control(), candidates(), publishers(), options()) -> any().
experiment(Name, Control, Candidates, Publishers, Options) ->
  gen_server:cast(schrodinger_lab, {experiment, {Name, Control, Candidates, Publishers, Options}}),
  receive_loop(Options).

-spec receive_loop(options()) -> any().
receive_loop(Options) ->
  Timeout = proplists:get_value(timeout, Options),
  receive
    {measurement, {_, #observation{type=control}=Observation}} ->
      Observation#observation.result
  after Timeout -> {error, timeout}
  end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec default_options() -> options().
default_options() -> [
  {timeout, default_timeout()}
 ].

-spec default_publishers() -> publishers().
default_publishers() -> [self()].

default_timeout() -> 5000.
