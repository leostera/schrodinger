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

-record(state, {}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast({measure, {Name, Control, Candidates}, Scientist}, State) ->
  experiment(Name, Control, Candidates, Scientist),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

experiment(Name, Control, Candidates, Scientist) ->
  Observations = [ run(Control, true) |
                  lists:map(fun (F) -> run(F) end, Candidates) ],
  io:format("~p (~p experiments)", [Name, length(Candidates)+1]),
  io:format("\n\n"),
  io:format("---------------------------------------------------------------------------------\n"),
  io:format("Name\t\tStart\t\tEnd\t\tTime\tResult Observation\n"),
  io:format("---------------------------------------------------------------------------------\n"),
  collect(Observations, Scientist).

collect(Observations, Scientist) -> collect(Observations, Scientist, []).
collect([], Scientist, Results) -> Scientist ! {results, Results};
collect(Observations, Scientist, Results) ->
  receive
    {Pid, Result} ->
      RemainingObservations = lists:filter(fun (Observation) ->
                                           Pid =/= Observation#observation.pid
                                       end, Observations),
      Observation = hd(lists:filter( fun (Observation) ->
                                  Pid =:= Observation#observation.pid
                              end, Observations)),
      Done = timestamp(),
      NewObservation = Observation#observation{
                        result=Result,
                        finished_at=Done,
                        time_delta=Done-Observation#observation.started_at
                       },
      Scientist ! {measurement, NewObservation},
      print_measurement(NewObservation),
      AccResults = [ NewObservation | Results ],
      collect(RemainingObservations, Scientist, AccResults)
  end.

timestamp() -> erlang:monotonic_time(seconds).

run(Spec) -> run(Spec, candidate).

run({Name, F}, Type) when is_function(F) ->
  Collector=self(),
  Pid=erlang:spawn( fun () ->
                        Result = F(),
                        Collector ! { self(), Result }
                    end ),
  #observation{
     name=Name,
     predicate=F,
     pid=Pid,
     type=Type
  }.

print_measurement(M) ->
  io:format("~s\t~p\t~p\t~p\t~p\t~p\n", [
    M#observation.name,
    M#observation.started_at,
    M#observation.finished_at,
    M#observation.time_delta,
    M#observation.result,
    M#observation.type
  ]).
