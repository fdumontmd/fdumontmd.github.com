my_min([M], M).
my_min([H|T], M) :- my_min(T, M1), M is min(H, M1).
