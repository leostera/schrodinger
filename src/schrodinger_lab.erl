%%%-------------------------------------------------------------------
%% @doc main server for spawning experiments
%% @end
%%%-------------------------------------------------------------------
-module(schrodinger_lab).

-behavior(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  Db = ets:new(schrodinger_boxes, [named_table]),
  {ok, Db}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%====================================================================
%% Handler functions
%%====================================================================

handle_call(_, _, _) -> not_implemented.

handle_cast({experiment, {Name, _Control, _Candidates, _Reporters, _Options}=Spec}, Db) ->
  BoxPid=spawn(schrodinger_box, start, [Spec]),
  ets:insert(Db, {Name, BoxPid}),
  {noreply, Db}.
