-module(logger).
-export([start_server/0, connect/1, recv_loop/2]).

-define(LISTEN_PORT, 9000).
-define(TCP_OPTS, [binary, {packet, raw}, {nodelay, true}, {reuseaddr, true}, {active, once}]).

start_server() ->
    case gen_tcp:listen(?LISTEN_PORT, ?TCP_OPTS) of
        {ok, Listen} -> spawn(?MODULE, connect, [Listen]),
                        io:format("~p Server Started.~n", [erlang:localtime()]);
        Error ->
            io:format("Error: ~p~n", [Error])
    end.

connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    inet:setopts(Socket, ?TCP_OPTS),
    spawn(fun() -> connect(Listen) end),
    {ok, File} = file:open("log.txt", [append]),
    recv_loop(Socket,File),
    file:close(File),
    gen_tcp:close(Socket).

recv_loop(Socket,File) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            io:format("~p ~p ~p~n", [inet:peername(Socket), erlang:localtime(), Data]),
            file:write(File, Data),
            gen_tcp:send(Socket, "I wrote down " ++ Data),
            recv_loop(Socket,File);
        {tcp_closed, Socket} ->
            io:format("~p Client Disconnected.~n", [erlang:localtime()])
    end.
