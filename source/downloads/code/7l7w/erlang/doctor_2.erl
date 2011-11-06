-module(doctor_2).
-export([start/2]).
-export([loop/3]).

start(F, A) -> 
	d_monitor(F, {out, A}, -1).

d_monitor(F, A, N) -> 
	D = spawn(doctor_2, loop, [F, A, N]),
	D ! new,
	D.

loop(_, _, 0) -> exit({running, out_of, time, erlang:time()});
loop(F, A, N) ->
  process_flag(trap_exit, true),
  receive
    new -> 
      case A of
        {out, At} -> io:format("Creating and monitoring process.~n"),
                P = d_monitor(F, At, 3),
				link(P);
        _ -> check_for(A, F)
      end,
      loop(F, A, N);
    {'EXIT', From, Reason} ->
      io:format("The process ~p ~p died with reason ~p.", [A, From, Reason]),
      io:format(" Restarting~n."),
      self() ! new,
      loop(F, A, N-1)
  end.

check_for(A, F) ->
  Found = lists:member(A, registered()) andalso erlang:is_process_alive(whereis(A)),
  if
    Found -> 
	  io:format("reattaching to running process ~p.~n", [A]),
	  link(whereis(A)),
	  true;
	true ->
      io:format("Creating and monitoring process, attaching to atom ~p.~n", [A]),
      register(A, spawn_link(F)),
      true
  end.