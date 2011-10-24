%% Solver predicates 

exclude_diag(_, [], []).
exclude_diag(Diff, [X|T], [L, R| RE]) :-
    L is X + Diff,
    R is X - Diff,
    Diff1 is Diff + 1,
    exclude_diag(Diff1, T, RE).

valid(0, _, S, S).
valid(Pos, Range, Sol, R) :- 
    exclude_diag(1, Sol, Excl),
    subtract(Range, Excl, Poss),
    member(X, Poss),            % pick one location
    select(X, Range, Rest),     % don't reuse it
    Pos1 is Pos - 1,
    valid(Pos1, Rest, [X|Sol], R).

queens(Max, Sol) :- 
    make_range(Max, Range),
    make_var(Max, Sol),
    valid(Max, Range, [], Sol).

%% Formatting predicates

format_board(Max, In, Out) :- maplist(format_line(Max), In, Out).

put_queen(Pos, Pos, _, 'Q').
put_queen(P1, P2, E, E) :- P1 \= P2.
format_line(Max, Pos, L) :- make_line(Max, ' ', R), Prev is Pos - 1, 
    take(Prev, R, P, [_|S]), append(P, ['Q'|S], L).

%% Pretty Printer predicates

print_board(B) :-
    length(B, L),
    LL is L * 2 + 1,
    make_line(LL, '-', LineSep),
    nl, print_list(LineSep), nl,
    maplist_(print_line(LineSep), B).

print_line(LineSep, L) :-
    print('|'),
    maplist_(print_square, L), nl,
    print_list(LineSep), nl.

print_square(S) :- print(S), print('|').

%% Toplevel predicate
run_queens(N) :- queens(N, S), format_board(N, S, B), print_board(B).
