my_sort([], []).
my_sort([H|T], S2) :- my_sort(T, S1), my_insert(H, S1, S2).

my_insert(H, [], [H]).
my_insert(H, [S1|S1T], [H|[S1|S1T]]) :- H =< S1.
my_insert(H, [S1|S1T], [S1|S1T2]) :- H > S1, my_insert(H, S1T, S1T2).
