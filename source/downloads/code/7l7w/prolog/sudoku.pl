sudoku(Puzzle, Solution) :- 
    Solution = Puzzle,
    length(Solution, Len), 
    Side is floor(sqrt(Len)),   % side of the board
    SH is floor(sqrt(Side)),    % horizontal length of a Square
    SV is floor(sqrt(Side)),    % vertical length of a Square

    sudoku_(Len, Side, SH, SV, Puzzle).

sudoku_(Len, Side, SH, SV, Puzzle) :-
    make_var(Len, Puzzle),      % create the Puzzle list 

    fd_domain(Puzzle, 1, Side),

    chunk(Side, Puzzle, Cols),  % split the Puzzle into columns
    
    transpose(Cols, Rows),      % transpose the columns into rows

    make_squares(SH, SV, Cols, Squares),
    concatenate([Cols, Rows, Squares], Constrains),
    valid(Constrains),
    fd_labeling(Puzzle).

valid([]).
valid([Head|Tail]) :-
    fd_all_different(Head),
    valid(Tail).

make_squares(SH, SV, Cols, Squares) :-
    maplist(chunk(SH), Cols, ColSplit), % split each line into SH long segments
    transpose(ColSplit, RowSplit), % transpose the result to process columns
    maplist(chunk(SV), RowSplit, ListOfListOfSquares), % split each column into SV long segments
    maplist(maplist(concatenate), ListOfListOfSquares, ListOfSquares), % group the squares together into each column 
    concatenate(ListOfSquares, Squares). % group all the columns together, so at to have a list of squares
