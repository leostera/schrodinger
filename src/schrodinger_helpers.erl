-module(schrodinger_helpers).

-compile([export_all]).

-include("../include/schrodinger.hrl").

%%====================================================================
%% API functions
%%====================================================================

sample(Name, I) ->
  Wait = fun (Id, Time, Return) ->
             { io_lib:format("Experiment #~p", [Id]),
               fun () ->
                   receive
                   after round(random:uniform()*Time*1000) -> Return
                   end
               end
             }
         end,
  schrodinger:experiment(Name,
                         Wait(0, 2, true),
                         [ Wait(N, N, N) || N <- lists:seq(1,I-1) ]).
