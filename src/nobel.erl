-module(nobel).

-compile([export_all]).

-include("../include/nobel.hrl").

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
             [Wait(0, 2, true),
             [ Wait(N, N, N) || N <- lists:seq(1,I-1) ]
             ]).

experiment(Name, [Control|Candidates]) ->
  gen_server:cast(nobel_server, {measure, {Name, Control, Candidates}}),
  receive
    {control, Observation} -> Observation#observation.result
  after infinity -> {error, timeout}
  end.
