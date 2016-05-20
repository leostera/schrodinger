-module(schrodinger).

-compile([export_all]).

-include("../include/schrodinger.hrl").

%%====================================================================
%% API functions
%%====================================================================

experiment(Name, Control, Candidates) ->
  experiment([
    {name, Name},
    {control, Control},
    {candidates, Candidates},
    {options, default_options()},
    {publishers, default_publishers()}
  ]).

experiment(Spec) ->
  gen_server:cast(schrodinger_lab, {experiment, Spec}),
  receive_loop(Spec).

receive_loop(Spec) ->
  receive
    {measurement, {_, #observation{type=control}=Observation}} ->
      Observation#observation.result
  after Timeout -> {error, timeout}
  end.

%%====================================================================
%% Internal functions
%%====================================================================

default_options() -> [
  {timeout, default_timeout()}
 ].

default_publishers() -> [self()].

default_timeout() -> 5000.
