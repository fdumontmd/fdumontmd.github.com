hanoi(N) :- move(N, left, right, center).

move(1, P1, P2, _) :- move_one(P1, P2).
move(N, P1, P2, P3) :- 
    N > 1, N1 is N - 1, 
    move(N1, P1, P3, P2),
    move(1, P1, P2, P3),
    move(N1, P3, P2, P1).

move_one(P1, P2) :- format("Move disk from ~k to ~k", [P1, P2]), nl.
