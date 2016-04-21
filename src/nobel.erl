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

sample() ->
  Wait = fun (Seconds, Return) ->
          fun () ->
           receive
           after Seconds*1000 -> Return
           end
          end
         end,
  experiment("Test Experiment", [Wait(2, true), Wait(3, false)]).

experiment(Name, Foos) ->
  Experiments = [ run(Name, hd(Foos), true) |
                  lists:map(fun (F) -> run(Name, F) end, tl(Foos)) ],
  Measurements = collect(Name, Experiments),
  Measurements.

collect(Name, Experiments) -> collect(Name, Experiments, []).
collect(_Name, [], Results) -> Results;
collect(Name, Experiments, Results) ->
  io:format("~p ~p ~p\n\n", [Name, Experiments, Results]),
  receive
    {Pid, Result} ->
      RemainingExperiments = lists:filter(fun (Experiment) ->
                                           Pid =/= Experiment#measurement.pid
                                       end, Experiments),
      Experiment = hd(lists:filter( fun (Experiment) ->
                                  Pid =:= Experiment#measurement.pid
                              end, Experiments)),
      io:format("~p ~p ~p ~p\n\n", [RemainingExperiments, Experiment, Pid, Result]),
      Done = timestamp(),
      NewExperiment = Experiment#measurement{
                     result=Result,
                     finished_at=Done,
                     delta=Done-Experiment#measurement.started_at
                    },
      AccResults = [ NewExperiment | Results ],
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

compare(Name, R1, R2) ->
  io:format("Experiment : ~p\n-------------------\n", [Name]),
  io:format("Started At : ~p ~p \n", [R1#measurement.started_at,  R2#measurement.started_at]),
  io:format("Finished At: ~p ~p \n", [R1#measurement.finished_at, R2#measurement.finished_at]),
  io:format("Delta      : ~p \t\t ~p \n", [R1#measurement.delta,       R2#measurement.delta]),
  io:format("Result     : ~p \t ~p \n", [R1#measurement.result,      R2#measurement.result]).
