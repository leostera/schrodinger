-module(nobel).

-compile([export_all]).

-record(measurement, {
          name,
          started_at=timestamp(),
          finished_at=not_run,
          delta=0,
          result=not_run,
          foo=not_set,
          pid,
          control=false
         }).

sample(I) ->
  Wait = fun (Seconds, Return) ->
          fun () ->
           receive
           after round(Seconds*1000) -> Return
           end
          end
         end,
  experiment("Test Experiment",
             [ Wait(2, true) |
               [ Wait(random:uniform()*N, false) || N <- lists:seq(0,I) ]
             ]).

experiment(Name, Foos) ->
  Experiments = [ run(Name, hd(Foos), true) |
                  lists:map(fun (F) -> run(Name, F) end, tl(Foos)) ],
  Measurements = collect(Name, Experiments),
  compare(Measurements),
  Measurements.

collect(Name, Experiments) -> collect(Name, Experiments, []).
collect(_Name, [], Results) -> Results;
collect(Name, Experiments, Results) ->
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
      AccResults = [ NewExperiment | Results ],
      print_measurement(NewExperiment),
      collect(Name, RemainingExperiments, AccResults)
  end.

timestamp() -> erlang:monotonic_time(seconds).

run(Name, F) -> run(Name, F, false).

run(Name, F, Control) when is_function(F) ->
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

compare(Measurements) ->
  io:format("\n\n"),
  io:format("---------------------------------------------------------------------------------\n"),
  io:format("Name\t\t\tStart\t\tEnd\t\tTime\tResult Experiment\n"),
  io:format("---------------------------------------------------------------------------------\n"),
  lists:foreach(fun print_measurement/1, Measurements),
  io:format("---------------------------------------------------------------------------------\n").

print_measurement(M) ->
  io:format("~p\t~p\t~p\t~p\t~p\t~p\n", [
    M#measurement.name,
    M#measurement.started_at,
    M#measurement.finished_at,
    M#measurement.delta,
    M#measurement.result,
    M#measurement.control
  ]).
