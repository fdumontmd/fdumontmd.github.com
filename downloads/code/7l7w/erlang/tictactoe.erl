-module(tictactoe).
-export([check/1]).
-export([make_winners/1]).

make_rows(N) -> [[{X,Y} || X <- lists:seq(1,N)] || Y <- lists:seq(1,N)].

make_lines(N) ->
	Rows = make_rows(N),
	Rows ++ matrix:transpose(Rows).
	
make_diag(N) -> [{X, N-X+1} || X <- lists:seq(1, N)].


make_winners(N) ->
	Lines = make_lines(N),
	Diag1 = make_diag(N),
	Diag2 = lists:map(fun({X,Y}) -> {4-X, Y} end, Diag1),
	[Diag1, Diag2] ++ Lines.

get_pos({X, Y}, B) -> lists:nth(X+3*(Y-1), B).

check_align(L, B) -> 
	Line = lists:map(fun(P) -> get_pos(P, B) end, L),
	case Line of
		[x,x,x] -> x;
		[o,o,o] -> o;
		_ -> no_winners
	end.

player(x) -> true;
player(o) -> true;
player(_) -> false.

free(P) -> not player(P).

check(B) -> 
	Checks = lists:map(fun(L) -> check_align(L, B) end, make_winners(3)),
	Win = lists:filter(fun player/1, Checks),
	case Win of
		[R|_] -> R;
		_ -> case lists:filter(fun free/1, B) of
			[_|_] -> no_winner;
			_ -> cat
		end
	end.
