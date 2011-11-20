-module(erl_tcp).
-export([start_server/0, connect/1, recv_loop/1]).

-define(LISTEN_PORT, 9000).
-define(TCP_OPTS, [binary, {packet, raw}, {nodelay, true}, {reuseaddr, true}, {active, once}]).

start_server() ->                               
% start up the service and error out if we cannot
    case gen_tcp:listen(?LISTEN_PORT, ?TCP_OPTS) of
        {ok, Listen} -> spawn(?MODULE, connect, [Listen]),
                        io:format("~p Server Started.~n", [erlang:localtime()]);
        Error -> io:format("Error: ~p~n", [Error])
    end.

connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    inet:setopts(Socket, ?TCP_OPTS),
% kick off another process to handle connections concurrently
    spawn(fun() -> connect(Listen) end),
    recv_loop(Socket),
    gen_tcp:close(Socket).

recv_loop(Socket) ->
% reset the socket for flow control
    inet:setopts(Socket, [{active, once}]),
    receive
% do something with the data you receive
        {tcp, Socket, Data} ->
            io:format("~p ~p ~p~n", [inet:peername(Socket), erlang:localtime(), Data]),
            gen_tcp:send(Socket, "I Received " ++ Data),
            recv_loop(Socket);
% exit loop if the client disconnects
        {tcp_closed, Socket} ->
            io:format("~p Client Disconnected.~n", [erlang:localtime()])
    end.
