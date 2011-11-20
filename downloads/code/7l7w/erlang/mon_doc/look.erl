-module(look).
-export([look_for/2]).

look_for(A, F) ->
    Found = lists:member(A, registered()) andalso erlang:is_process_alive(whereis(A)),
    if
        Found ->
            io:format("reattaching to running process ~p.~n", [A]),
            link(whereis(A)),
            false;
        true ->
            io:format("Creating and monitoring process, attaching to atom ~p.~n", [A]),
            F(),
            true
    end.
