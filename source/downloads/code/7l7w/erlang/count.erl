-module(count).
-export([count_to_10/0]).
-export([count_to/1]).
-export([count_up_to/1]).

count_to_10() -> count_to(10).

count_to(N) -> 
	L = count_up_to(N),
	lists:map(fun(X) -> io:fwrite("~w\n", [X]) end, L),
	L.

count_up_to(N) -> lists:seq(1,N).

