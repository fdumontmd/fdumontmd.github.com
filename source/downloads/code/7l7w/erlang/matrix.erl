-module(matrix).
-export([transpose/1]).

head([H|_]) -> H.
tail([_|T]) -> T.

transpose([]) -> [];
transpose([[]|_]) -> [];
transpose(M) -> 
	Heads = lists:map(fun head/1, M),
	Tails = lists:map(fun tail/1, M),
	[Heads|transpose(Tails)].
