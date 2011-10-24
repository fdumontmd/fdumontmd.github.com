% apply a clause to each element of a list, and collect the results
maplist(_, [], []).
maplist(P, [H|T], [R|Rs]) :- call(P, H, R), maplist(P, T, Rs).

% same, but without return value - only side effect
maplist_(_, []).
maplist_(P, [H|T]) :- call(P, H), maplist_(P, T).

% same as maplist, but passes the current index to the predicate
maplistidx(_, _, [], []).
maplistidx(Pred, N, [H|T], [X|R]) :- 
    call(Pred, N, H, X), 
    N1 is N + 1,
    maplistidx(Pred, N1, T, R).

% subtract(S, Es, R) deletes all elements in Es from S, and puts the result in R
subtract(Set, [], Set).
subtract(Set, [H|T], Result) :- delete(Set, H, R1), subtract(R1, T, Result).

% Matrix transposition. A Matrix is a list of list
transpose([], []).
transpose([[]|_], []).
transpose(M, [Hs|M1]) :- maplist(head, M, Hs), maplist(tail, M, Ts), 
    transpose(Ts, M1).

% simple utility predicates used in transpose
head([H|_], H).
tail([_|T], T). 

% make_var makes a list of variables
make_var(N, L) :- length(L, N).

% take up to N element from F, return them as P. The rest is returned as S
take(N, F, P, S) :-
    length(F, Fl), Sl is min(Fl, N), make_var(Sl, P),
    append(P, S, F).

% split a list into sublists of N elements
chunk(_, [], []).
chunk(N, [L|Ls], [H|R]) :- take(N, [L|Ls], H, R1), chunk(N, R1, R). 

% concatenate a list of list. Unlike flatten, only operate on one level
concatenate([], A, A).
concatenate([H|T], R, A) :- append(H, A, A1), concatenate(T, R, A1).
concatenate(L, R) :- concatenate(L, R, []).

% make a list of N Char
const(N, _, N).
make_line(N, Char, Out) :- make_var(N, In), maplist(const(Char), In, Out).

% make a list with values 1 to N
make_range(N, R) :- make_var(N, L), maplistidx(const, 1, L, R).

% print_line print each element in a list without list formatting
print_list(List) :- maplist_(write, List).
