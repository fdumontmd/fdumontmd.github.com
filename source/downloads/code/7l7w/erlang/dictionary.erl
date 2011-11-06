-module(dictionary).
-export([lookup/2]).
-export([lookup_alt/2]).

lookup(_, []) -> false;
lookup(K, [{K, V}|_]) -> V;
lookup(K, [_|T]) -> lookup(K, T).

lookup_alt(K, L) -> lists:keyfind(K, 1, L).