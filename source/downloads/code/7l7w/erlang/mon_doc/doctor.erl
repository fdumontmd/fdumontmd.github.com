-module(doctor).
-export([loop/0, start/0]).

loop() ->
    process_flag(trap_exit, true),
    receive
        start ->
            io:format("Looking for monitor...~n"),
            look:look_for(monitor, fun monitor:start/0),
            self() ! new,
            loop();
        new ->
            io:format("Looking for revolver...~n"),
            look:look_for(revolver, fun roulette:start/0),
            loop();
        die ->
            io:format("Aaargh...~n"),
            exit({doctor, died, at, erlang:time()});
        {'EXIT', From, Reason} ->
            io:format("The process ~p died with reason ~p.~n", [From, Reason]),
            self() ! start,
            loop()
    end.

start() ->
    register(doctor, spawn_link(fun doctor:loop/0)),
    doctor ! start.
