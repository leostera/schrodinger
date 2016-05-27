-module(schrodinger).
-include("schrodinger.hrl").

%%====================================================================
%% Public API
%%====================================================================

-export([
         experiment/3,
         experiment/5
        ]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type name() :: atom() | [char()] | <<>>.
-type timestamp() :: integer() | unset.

-type type() :: candidate | control.

-type predicate()  :: fun( () -> any() ).
-type control()    :: predicate().
-type candidate()  :: { name(), predicate() }.
-type candidates() :: [ candidate() ].

-type publisher()  :: pid().
-type publishers() :: [ publisher() ].

-type option()  :: { name(), any() }.
-type options() :: [ option() ].

-type observation()  :: #{}.
-type observations() :: [ observation() ].

-export_type([
              name/0,
              timestamp/0,
              type/0,
              predicate/0,
              control/0,
              candidate/0,
              candidates/0,
              publisher/0,
              publishers/0,
              option/0,
              options/0,
              observation/0,
              observations/0
             ]).

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
    {measurement, {_, #{ type := control }=Observation}} ->
      #{ result := Result } =  Observation,
      Result
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
