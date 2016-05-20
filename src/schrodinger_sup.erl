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

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { supervision_flags(), child_specs() } }.

%%====================================================================
%% Internal functions
%%====================================================================

supervision_flags() -> #{
  strategy => one_for_all,
  intensity => 0,
  peiord => 1
}.

child_specs() -> [ #{
  id => schrodinger_lab,
  start => { schrodinger_lab, start_link, [] },
  restart => permanent,
  shutdown => brutal_kill,
  type => supervisor
 } ].

