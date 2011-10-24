my_reverse(L, R) :- compound(R), !, my_reverse(R, L).
my_reverse(L, R) :- my_reverse(L, R, []).
my_reverse([], A, A).
my_reverse([H|T], R, A) :- my_reverse(T, R, [H|A]).
