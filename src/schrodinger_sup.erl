%%%-------------------------------------------------------------------
%% @doc schrodinger top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(schrodinger_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    {ok, { supervision_flags(), child_specs() } }.

%%====================================================================
%% Internal functions
%%====================================================================

supervision_flags() -> #{
  strategy  => one_for_all,
  intensity => 0,
  peiord    => 1
}.

child_specs() -> [ #{
  id       => schrodinger_lab,
  restart  => permanent,
  shutdown => brutal_kill,
  start    => { schrodinger_lab, start_link, [] },
  type     => supervisor
 } ].
