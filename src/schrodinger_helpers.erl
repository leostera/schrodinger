-module(schrodinger_helpers).

-export([
         sample/2
        ]).

-include("schrodinger.hrl").

%%====================================================================
%% API functions
%%====================================================================

-spec sample(schrodinger:name(), integer()) -> any().
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
  {_, Control} = Wait(0, 2, true),
  schrodinger:experiment(Name, Control,
                         [ Wait(N, N, N) || N <- lists:seq(1,I-1) ]).
