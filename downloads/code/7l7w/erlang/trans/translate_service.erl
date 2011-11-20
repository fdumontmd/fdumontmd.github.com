-module(translate_service).
-export([start_server/0]).
-export([connect/1]).
-export([cleanup/1]).

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
    recv_loop(Socket,[{"casa", "house"}, {"blanca", "white"}]),
    gen_tcp:close(Socket).

recv_loop(Socket,Dict) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            io:format("~p ~p ~p~n", [inet:peername(Socket), erlang:localtime(), Data]),
            {Msg, Dict2} = process_message(Dict, cleanup(Data)),
            gen_tcp:send(Socket, binary:list_to_bin(Msg)),
            recv_loop(Socket, Dict2);
        {tcp_closed, Socket} ->
            io:format("~p Client Disconnected.~n", [erlang:localtime()])
    end.

cleanup(Data) ->
    binary:bin_to_list(Data, {0, byte_size(Data) - 2}).

process_message(Dict, Data) ->
    case Data of
        [$p,$u,$t,$ |Def] ->
            [Word, Meaning] = string:tokens(Def, ":"),
            {"Noted\r\n", [{Word, Meaning}|Dict]};
        _ -> {translate(Data, Dict) ++ "\r\n", Dict}
    end.

translate(Word, Dict) ->
    M = lists:keyfind(Word, 1, Dict),
    case M of
        false ->
            "I do not understand " ++ Word;
        {_, Meaning} -> Meaning
    end.
