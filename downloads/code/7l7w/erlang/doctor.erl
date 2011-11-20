-module(doctor).
-export([start/2]).
-export([loop/2]).

start(F, A) -> 
	D = spawn(doctor, loop, [F, A]),
	D ! new,
	D.

loop(F, A) ->
  process_flag(trap_exit, true),
  receive
    new -> 
      io:format("Creating and monitoring process, attaching to atom ~p.~n", [A]),
      register(A, spawn_link(F)),
      loop(F, A);
    {'EXIT', From, Reason} ->
      io:format("The process ~p ~p died with reason ~p.", [A, From, Reason]),
      io:format(" Restarting~n."),
      self() ! new,
      loop(F, A)
  end.
