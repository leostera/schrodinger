-module(nobel).

-compile([export_all]).

-include("../include/nobel.hrl").

sample(Server, I) ->
  Wait = fun (Id, Time, Return) ->
             { io_lib:format("Experiment #~p", [Id]),
               fun () ->
                   receive
                   after round(random:uniform()*Time*1000) -> Return
                   end
               end
             }
         end,
  experiment(Server,
             "Test Experiment",
             Wait(0, 2, true),
             [ Wait(N, N, N) || N <- lists:seq(1,I-1) ]).

report(Server, Name) ->
  Measurements = gen_server:call(Server, {report, Name}),
  io:format("~p (~p experiments)", [Name, length(Measurements)]),
  io:format("\n\n"),
  io:format("---------------------------------------------------------------------------------\n"),
  io:format("Name\t\tStart\t\tEnd\t\tTime\tResult Observation\n"),
  io:format("---------------------------------------------------------------------------------\n"),
  [ print_measurement(M) || M <- Measurements ].

experiment(Server, Name, Control, Candidates) ->
  gen_server:cast(Server, {measure, {Name, Control, Candidates}, self()}),
  receive
    {measurement, {_, #observation{type=control}=Observation}} ->
      print_measurement(Observation),
      Observation#observation.result
  end.

print_measurement(M) ->
  io:format("~s\t~p\t~p\t~p\t~p\t~p\n", [
    M#observation.name,
    M#observation.started_at,
    M#observation.finished_at,
    M#observation.time_delta,
    M#observation.result,
    M#observation.type
  ]).
