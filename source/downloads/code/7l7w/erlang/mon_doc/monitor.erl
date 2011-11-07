-module(monitor).
-export([loop/0, start/0]).

loop() ->
    process_flag(trap_exit, true),
    receive
        start ->
            io:format("Looking for doctor...~n"),
            look:look_for(doctor, fun doctor:start/0),
            loop();
        die ->
            io:format("Aaargh...~n"),
            exit({monitor, died, at, erlang:time()});
        {'EXIT', From, Reason} ->
            io:format("The process ~p died with reason ~p.~n", [From, Reason]),
            self() ! start,
            loop()
    end.

start() ->
    register(monitor, spawn_link(fun monitor:loop/0)),
    monitor ! start.
