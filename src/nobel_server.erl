-module(nobel).

-compile([export_all]).

-behavior(gen_server).

-include("../include/nobel.hrl").

experiment(Name, Foos) ->
  Experiments = [ run(hd(Foos), true) |
                  lists:map(fun (F) -> run(F) end, tl(Foos)) ],
  io:format("~p (~p experiments)", [Name, length(Foos)]),
  io:format("\n\n"),
  io:format("---------------------------------------------------------------------------------\n"),
  io:format("Name\t\tStart\t\tEnd\t\tTime\tResult Experiment\n"),
  io:format("---------------------------------------------------------------------------------\n"),
  Measurements = collect(Experiments),
  {Name, Measurements}.

collect(Experiments) -> collect(Experiments, []).
collect([], Results) -> Results;
collect(Experiments, Results) ->
  receive
    {Pid, Result} ->
      RemainingExperiments = lists:filter(fun (Experiment) ->
                                           Pid =/= Experiment#measurement.pid
                                       end, Experiments),
      Experiment = hd(lists:filter( fun (Experiment) ->
                                  Pid =:= Experiment#measurement.pid
                              end, Experiments)),
      Done = timestamp(),
      NewExperiment = Experiment#measurement{
                        result=Result,
                        finished_at=Done,
                        delta=Done-Experiment#measurement.started_at
                       },
      print_measurement(NewExperiment),
      AccResults = [ NewExperiment | Results ],
      collect(RemainingExperiments, AccResults)
  end.

timestamp() -> erlang:monotonic_time(seconds).

run(Spec) -> run(Spec, false).

run({Name, F}, Control) when is_function(F) ->
  Collector=self(),
  Pid=erlang:spawn( fun () ->
                        Result = F(),
                        Collector ! { self(), Result }
                    end ),
  #measurement{
     name=Name,
     foo=F,
     pid=Pid,
     control=Control
  }.

print_measurement(M) ->
  io:format("~s\t~p\t~p\t~p\t~p\t~p\n", [
    M#measurement.name,
    M#measurement.started_at,
    M#measurement.finished_at,
    M#measurement.delta,
    M#measurement.result,
    not M#measurement.control
  ]).
