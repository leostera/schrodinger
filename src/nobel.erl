-module(nobel).

-compile([export_all]).

-include("../include/nobel.hrl").

sample(I) ->
  {ok, Server} = nobel_server:start_link(),
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

experiment(Server, Name, Control, Candidates) ->
  gen_server:cast(Server, {measure, {Name, Control, Candidates}, self()}),
  receive
    {measurement, #observation{type=control}=Observation} -> Observation#observation.result
  end.
