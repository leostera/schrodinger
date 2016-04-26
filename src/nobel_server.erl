-module(nobel_server).

-compile([export_all]).

-behavior(gen_server).

-include("../include/nobel.hrl").

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  Experiments = ets:new(nobel_experiments, [named_table]),
  {ok, Experiments}.

handle_call({report, Name}, _From, Db) ->
  Experiment = ets:lookup(Db, Name),
  {reply, Experiment, Db}.

handle_cast({measure, {Name, Control, Candidates}, Scientist}, Db) ->
  experiment(Name, Control, Candidates, Scientist, Db),
  {noreply, Db}.

handle_info(_Info, Db) ->
  {noreply, Db}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, Db, _Extra) ->
  {ok, Db}.

experiment(Name, Control, Candidates, Scientist, Db) ->
  Observations = [ run(Control, control) |
                  lists:map(fun (F) -> run(F) end, Candidates) ],
  Results = collect(Name, Observations, Scientist),
  io:format("Got results: ~p", [Results]),
  ets:insert(Db, {Name, Results}).

collect(Name, Observations, Scientist) -> collect(Name, Observations, Scientist, []).
collect(Name, [], Scientist, Results) ->
  Scientist ! {results, {Name, Results}},
  Results;
collect(Name, Observations, Scientist, Results) ->
  receive
    #observation{pid=Pid}=Observation ->
      RemainingObservations = lists:filter(fun (Obs) ->
                                           Pid =/= Obs#observation.pid
                                       end, Observations),
      Scientist ! {measurement, {Name, Observation}},
      collect(Name, RemainingObservations, Scientist, [ Observation | Results ])
  end.

timestamp() -> erlang:monotonic_time(seconds).

run(Spec) -> run(Spec, candidate).

run({Name, F}, Type) when is_function(F) ->
  Collector=self(),
  Observation = #observation{
                   name=Name,
                   predicate=F,
                   type=Type,
                   started_at=timestamp()
                  },
  Pid=erlang:spawn( fun () ->
                        Result = F(),
                        Done = timestamp(),
                        Collector ! Observation#observation {
                          result=Result,
                          finished_at=Done,
                          time_delta=Done-Observation#observation.started_at,
                          pid=self()
                         }
                    end ),
  Observation#observation{
     pid=Pid
  }.
