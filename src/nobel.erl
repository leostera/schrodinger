-module(nobel).

-compile([export_all]).

-record(measurement, {
          name,
          started_at=timestamp(),
          finished_at=not_finished,
          delta=0,
          result=no_result,
          foo=not_set,
          pid=no_pid,
          control=false
         }).

sample(I) ->
  Wait = fun (Id, Time, Return) ->
             { io_lib:format("Experiment #~p", [Id]),
               fun () ->
                   receive
                   after round(random:uniform()*Time*1000) -> Return
                   end
               end
             }
         end,
  experiment("Test Experiment",
             [ Wait(0, 2, true) |
               [ Wait(N, N, N) || N <- lists:seq(1,I-1) ]
             ]).

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
  Experiment = #measurement{
                  name=Name,
                  foo=F
                 },
  Pid=erlang:spawn( fun () ->
                        Result = F(),
                        Collector ! { self(), Result }
                    end ),
  Experiment#measurement{
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
