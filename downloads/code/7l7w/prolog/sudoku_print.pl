sudoku_print(Board) :- 
    length(Board, LB),          % complete board length
    LL is floor(sqrt(LB)),      % a line length is the square root of board length
    SL is floor(sqrt(LL)),      % a square length is the square root of line length
    sudoku_print_(LL, SL, SL, Board).

% assuming values up to 9. Otherwise would need to
% configure cell width.
sudoku_print_(LL, SH, SV, Board) :-
    chunk(LL, Board, Lines),             % cut the board in lines
    maplist(chunk(SH), Lines, LSquares), % cut each line in squares
    chunk(SV, LSquares, Squares),        % group each SV lines
    SepL is (2 * LL) + round(LL/SH) + 1, % 2 spaces for each number
    make_line(SepL, '-', Line),
    print_list(Line), nl,
    maplist_(out_squares(Line), Squares).

out_squares(LineSep, SBlock) :- 
    maplist_(in_squares, SBlock),
    print_list(LineSep), nl.

in_squares(Line) :-
    write('|'),
    maplist_(line, Line),nl.

line(SubLine) :-
    maplist_(print_number, SubLine),
    write('|').
    
print_number(N) :- format("~k ", [N]).
