-module(translate_service).
-export([loop/0, translate/2]).

loop() ->
  receive
    {From, "casa"} ->
      From ! "house",
      loop();
    {From, "blanca"} ->
      From ! "white",
      loop();
    {From,M} ->
      From ! "I do not understand",
      exit({M, not_understood, received_at, erlang:time()})
  end.

translate(To, Word) ->
  To ! {self(), Word},
  receive
    Translation -> Translation
  end.
