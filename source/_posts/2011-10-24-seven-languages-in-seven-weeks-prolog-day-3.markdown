---
layout: post
title: "Seven Languages in Seven Weeks Prolog Day 3"
date: 2011-10-24 13:33
comments: true
categories: [Books]
tags: [7languages7weeks, prolog]
series: "Seven Languages in Seven Weeks"
---
The final day with Prolog is called "Blowing Up Vegas" in the book, and was certainly intended as a Shock and Awe moment. Unfortunately, it feels more like Razzle Dazzle.
<!--more-->
Certainly, the code for both solvers (8 Queens and Sudoku) is short (if slightly incomplete) and effective. But it has one, significant shortcoming: it is not Prolog code.

Prolog's main control mechanism, backtracking, means the language is a natural match for problems that can be expressed as searches. But to be efficient, it is important to pay attention to the shape of the tree that is defined by the rules. Cutting and pruning are critical to ensure that Prolog will answer quickly (that is, before the Sun blows up or the Universe cools down).

As I was looking at the code for Day 3, I was specifically looking for the code that would shape the search tree; there were a couple of predicates (`fd_domain`, `fd_all_different`) which I didn't know about, so I mentally replaced them with code whose meaning was derived from the names of the unknown predicates (so `fd_domain` would try to assign a value between 1 and the passed maximum to each variables in the passed list, while `fd_all_different` would ensure that all values in the passed list was indeed different). The explanation in the book supported this interpretation.

Now, my problem was that the code as I understood it would be terribly slow: the `fd_domain` would generate a lot of different solutions, and `fd_all_different` would invalidate all but a few. Certainly, I was thinking, the 9x9 Sudoku will never work, even if the 4x4 seems to.

So I was surprised when I ran the 9x9 Sudoku (see below for the code): it was really fast. My assumptions regarding the unknown predicates were all wrong. Time to look at the [manual](http://www.gprolog.org/manual/html_node/gprolog055.html). It turns out that these predicates below to a specific GNU Prolog library (so it is not portable) designed to solve [Finite Domain](http://en.wikipedia.org/wiki/Constraint_logic_programming) problems. Now, clearly, this is a great library, it simplifies things a lot. But there's also the problem: what is so great about today's code is due to the library, not specifically to Prolog. Many languages have such a library, so the case for Prolog is kind of weakened.

Before I finally understood the role of the finite domain solver in today's code, I had reimplemented the 8 Queens (generalized to N Queens) with explicit search tree pruning. So here I will show variants of the book code, which I hope will be better witnesses to Prolog's strengths.

Exercises
---------

As stated above, I will depart considerably from the exercises, although the ones mentioned in the book are all here.

### Input/Ouput

There are a number of such predicates, listed [here](http://www.gprolog.org/manual/html_node/gprolog038.html#toc126).

### Print only successful solutions

I must say I'm not sure I understand this one. My approach is always to organise clauses so that the printing occurs last, when solutions are fully known. See below the code for the solvers.

### 9x9 Sudoku Solver

A first thing: when using GNU Prolog's Finite Domain Solver, the variables must be assigned a label from the domain, using `fd_labeling`. Otherwise, solutions will be displayed with a superset of the possible values for each unknown, rather than just the possible ones.

{% include_code 9x9 Sudoku Solver lang:prolog 7l7w/prolog/sudoku_book.pl %}

This was clearly tedious to write (I wrote a few Emacs functions to do the job). I will show a better (I think) way below.

{% codeblock Sudoko test lang:prolog %}
| ?- sudoku([_, _, _, 2, _, _, _, 6, 3,
        3, _, _, _, _, 5, 4, _, 1,
        _, _, 1, _, _, 3, 9, 8, _,
        _, _, _, _, _, _, _, 9, _, 
        _, _, _, 5, 3, 8, _, _, _, 
        _, 3, _, _, _, _, _, _, _, 
        _, 2, 6, 3, _, _, 5, _, _, 
        5, _, 3, 7, _, _, _, _, 8,
        4, 7, _, _, _, 1, _, _, _],
       Solution).

Solution = [8,5,4,2,1,9,7,6,3,3,9,7,8,6,5,4,2,1,2,6,1,4,7,3,9,8,5,7,8,5,1,2,6,3,9,4,6,4,9,5,3,8,1,7,2,1,3,2,9,4,7,8,5,6,9,2,6,3,8,4,5,1,7,5,1,3,7,9,2,6,4,8,4,7,8,6,5,1,2,3,9] ? 
{% endcodeblock %}

Note: I found a few implementations missing the `fd_labeling` clause, which causes the test above to return:
{% codeblock Sudoko test without fd_labeling lang:prolog %}
| ?- sudoku([_, _, _, 2, _, _, _, 6, 3,
        3, _, _, _, _, 5, 4, _, 1,
        _, _, 1, _, _, 3, 9, 8, _,
        _, _, _, _, _, _, _, 9, _, 
        _, _, _, 5, 3, 8, _, _, _, 
        _, 3, _, _, _, _, _, _, _, 
        _, 2, 6, 3, _, _, 5, _, _, 
        5, _, 3, 7, _, _, _, _, 8,
        4, 7, _, _, _, 1, _, _, _],
       Solution).

Solution = [_#3(8..9),_#25(4..5:8..9),_#47(4..5:8..9),2,_#83(1:4:8..9),_#105(4:9),7,6,3,3,_#191(6:8..9),_#213(7..9),_#235(6:8..9),_#257(6..9),5,4,2,1,_#343(2:6..7),_#365(4:6),1,_#401(4:6),_#423(4:6..7),3,9,8,5,_#509(1..2:6..8),_#531(1:4..6:8),_#553(2:4..5:7..8),_#575(1:4:6),_#597(1..2:4:6..7),_#619(2:4:6..7),_#641(1..3:6:8),9,_#677(2:4:6..7),_#699(1..2:6..7:9),_#721(1:4:6:9),_#743(2:4:7:9),5,3,8,_#807(1..2:6),_#829(1:4:7),_#851(2:4:6..7),_#873(1..2:6..9),3,_#909(2:4..5:7..9),_#931(1:4:6:9),_#953(1..2:4:6..7:9),_#975(2:4:6..7:9),_#997(1..2:6:8),_#1019(1:4..5:7),_#1041(2:4:6..7),_#1063(1:8..9),2,6,3,_#1127(4:8..9),_#1149(4:9),5,_#1185(1:4:7),_#1207(4:7:9),5,_#1243(1:9),3,7,_#1293(2:4:6:9),_#1315(2:4:6:9),_#1337(1..2:6),_#1359(1:4),8,4,7,_#1423(8..9),_#1445(6:8..9),_#1467(2:5..6:8..9),1,_#1503(2:6),3,_#1547(2:6:9)]

(1 ms) yes
{% endcodeblock %}

### Utility predicates

First, I need to introduce a few helper predicates which will come handy later on.

{% include_code Utility predicates lang:prolog 7l7w/prolog/utils.pl %}

`maplist` is just the same as the `map` function found in many functional programming language. 

{% codeblock maplist: apply sort to each sublist in a list lang:prolog %}
| ?- maplist(sort, [[3,1,2], [7,9,8], [6,5,4]], O).

O = [[1,2,3],[7,8,9],[4,5,6]] ? ;

no
{% endcodeblock %}

`maplist_` is the same, but called only for side effects.

{% codeblock maplist_: output each element lang:prolog %}
| ?- maplist_(print, [1,2,3]).
123

true ? ;

no
{% endcodeblock %}

`maplistidx` is the same as `maplist` again, but additionally passes the index (position within the list) to the predicate.

{% codeblock maplistidx lang:prolog %}
| ?- maplistidx(const, 1, [4,3,2,1], O).

O = [1,2,3,4] ? ;

no
{% endcodeblock %}

`subtract` generalizes `delete`: it removes all the elements of a list from another one.

{% codeblock subtract lang:prolog %}
| ?- subtract([1,2,3,4,5,6,7,8,9,10], [4,5,6], O).

O = [1,2,3,7,8,9,10] ? ;

no
{% endcodeblock %}


`transpose` is a matrix transposition predicate. It uses `head` and `tail` to split a list.

{% codeblock transpose lang:prolog %}
| ?- transpose([[1,2,3], [4,5,6]], O).

O = [[1,4],[2,5],[3,6]] ? ;

no
{% endcodeblock %}

`make_var` makes a list of `N` vars

{% codeblock make_var lang:prolog %}
| ?- make_var(5, L).

L = [_,_,_,_,_]

yes
{% endcodeblock %}

`take` is similar to the `take` function in Haskell: it split a list in two, the prefix being up to `N` elements long.

{% codeblock take lang:prolog %}
| ?- take(3, [1,2,3,4,5], T, R).

R = [4,5]
T = [1,2,3]

yes
{% endcodeblock %}

`chunk` splits a list into chunks of size `N`.

{% codeblock chunk lang:prolog %}
| ?- chunk(3, [1,2,3,4,5,6,7,8,9], O).

O = [[1,2,3],[4,5,6],[7,8,9]] ? ;

no
{% endcodeblock %}

`make_line` create a list of length `N`, and set each element to `Char`.

{% codeblock make_line: creating and printing a line of '-' lang:prolog %}
| ?- make_line(20, '-', O), maplist_(print, O), nl.
--------------------

O = [-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-] ? ;

no
{% endcodeblock %}

Finally, `make_range` creates a list with elements ranging from 1 to `N`

{% codeblock make_range lang:prolog %}
| ?- make_range(10, L).

L = [1,2,3,4,5,6,7,8,9,10] ? ;

no
{% endcodeblock %}

### Pretty printing Sudoku solutions

{% include_code Pretty Printing Sudoku lang:prolog 7l7w/prolog/sudoku_print.pl %}

First, a test to show what the output looks like:

{% codeblock Sudoku Pretty Printer lang:prolog %}
GNU Prolog 1.4.0
By Daniel Diaz
Copyright (C) 1999-2011 Daniel Diaz
| ?- consult('utils').
...

| ?- consult('sudoku_book').
...

| ?- consult('sudoku_print').
....

| ?- sudoku([_, _, _, 2, _, _, _, 6, 3,
        3, _, _, _, _, 5, 4, _, 1,
        _, _, 1, _, _, 3, 9, 8, _,
        _, _, _, _, _, _, _, 9, _, 
        _, _, _, 5, 3, 8, _, _, _, 
        _, 3, _, _, _, _, _, _, _, 
        _, 2, 6, 3, _, _, 5, _, _, 
        5, _, 3, 7, _, _, _, _, 8,
        4, 7, _, _, _, 1, _, _, _],
       Solution), sudoku_print(Solution).
----------------------
|8 5 4 |2 1 9 |7 6 3 |
|3 9 7 |8 6 5 |4 2 1 |
|2 6 1 |4 7 3 |9 8 5 |
----------------------
|7 8 5 |1 2 6 |3 9 4 |
|6 4 9 |5 3 8 |1 7 2 |
|1 3 2 |9 4 7 |8 5 6 |
----------------------
|9 2 6 |3 8 4 |5 1 7 |
|5 1 3 |7 9 2 |6 4 8 |
|4 7 8 |6 5 1 |2 3 9 |
----------------------

Solution = [8,5,4,2,1,9,7,6,3,3,9,7,8,6,5,4,2,1,2,6,1,4,7,3,9,8,5,7,8,5,1,2,6,3,9,4,6,4,9,5,3,8,1,7,2,1,3,2,9,4,7,8,5,6,9,2,6,3,8,4,5,1,7,5,1,3,7,9,2,6,4,8,4,7,8,6,5,1,2,3,9] ? 
{% endcodeblock %}

First the various modules have to be loaded (GNU Prolog does not have a module system, so this is tedious). Then the solver is run on a hard problem (from [this](http://www.sudoku.ws/hard.htm) site), and finally pretty printed. As usual, Prolog then lists the variables introduced in the query, here only `Solution`.

Pretty printing a 6x6 (dummy) board works as well:

{% codeblock A 6x6 board lang:prolog %}
| ?- make_line(36, 1, L), sudoku_print_(6, 3, 2, L).
---------------
|1 1 1 |1 1 1 |
|1 1 1 |1 1 1 |
---------------
|1 1 1 |1 1 1 |
|1 1 1 |1 1 1 |
---------------
|1 1 1 |1 1 1 |
|1 1 1 |1 1 1 |
---------------

L = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1] ? ;

no
{% endcodeblock %}

The code is fairly simple; the major difficulty is to find names for the various segments that are printed.

`sudoku_print` computes some parameters using assumptions that are valid for some board sizes, but not all. `sudoku_print_` should be used for these other sizes. The paramaters are the number of columns or rows `LL` and the size of the subdivisions, horizontally `SH`, and vertically `SV`. 

`sudoku_print_` then splits the `Board` into `LL` lines; each line is split into `SH` long segments (which will form the squares); and the list of lines is split into `SV` long vertical segments. A `LineSep` as large as the board: counting 2 characters per value, `2*LL`, plus 1 character for each separation before, between and after subdivisions (each subdivision is `SH` long, so there are `LL/SH` subdivisions). That `LineSep` is first printed (the general logic is that the various predicates print the separation after their output; the calling predicate emits the separation before the output).

`out_squares` iterates over the group of lines (each `SV` long). It prints a `LineSep` after each group.

`in_squares` iterates over a group of lines inside a square. It prints the `|` character that begins a line.

`line` iterates over the groups within a line. It prints the `|` after each group.

Finally, `print_number` iterate over each number inside a group. It prints the number, then a space.

### The n Queens problem

Here I depart from the book, as the code below solves the n Queens problem (that is, it is general over the number of queens), and I do not use the Finite Domain predicates, so as to show how Prolog can constrain the search tree.

The constraints on a n Queens problems are easy:

 * no two queens on the same row
 * no two queens on the same column
 * no two queens on the same diagonal

The first constraint it easy to ensure: the solution is the list of rows, each one giving the position of a single queen in that row. In other words, by the nature of the format for the solution, it is already impossible to have two queens in the same row.

For instance, the (dummy and wrong) board solution
```
[1,3,2]
```
puts one queen in column 1 of row 1, one queen in column 3 of row 2, and one queen in column 2 of row 3.

With such the board, the design of the algorithm starts to emerge:

 * select a column for each row top to bottom,
 * for each row, know the columns selected for rows above it
 * use the selected columns to filter out the potential candidate columns for the current row
 * if no candidate exists, backtrack
 * if all rows have a column, emit the result

{% include_code n Queens Solver lang:prolog 7l7w/prolog/queens.pl %}

With the utility predicates defined above, the code becomes fairly simple. It is divided into three groups:

 * solver
 * formatting
 * pretty printer

#### Solver

The solver is made of three predicates:

 * `queens/2`
 * `valid/4`
 * `exclude_diag/3`

`queens` prepares the work: it generates the list of possible columns in `Range` (similar to the `fd_domain` predicate the book uses), and a list for the solutions in `Sol`. The actual solution is computed by `valid`.

`valid` keeps a number of parameters:

 * `Pos` runs from `Max` to 0. When `Pos` is 0, it means we have a valid column for each row in `Sol`.
 * `Range` is the currently available columns
 * `Sol` is the columns of the queens in the rows above the current one
 * the last parameter is the actual solution. It is copied from `Sol` when `Pos` is 0.

The logic to remove possible positions is the following:

 * any selected column is removed from the `Range` for the rows below it
 * at each row, the diagonals of previous solutions is computed by `exclude_diag`, and removed from the `Range` only for the current row

The `member(X, Poss)` clause is the core of the backtracking: from the list of not excluded columns, each element is selected, and then we try to fill the remaining rows by calling `valid` recursively. If there are no remaining columns, Prolog backtracks until the more recent `member(X, Poss)`. When `Poss` is exhausted, the backtracking continues up to the next most recent `member(X, Poss)`. So `member(X, Poss)` generates branching, and `valid` generally closes them (or finds a solution). The branching is limited as much as possible by the design of the solution, and `exclude_diag`.

`exclude_diag` rely on a trick: let's say a queen has been put on column `C` in a given column. Then on the next row, it blocks the columns `C+1` and `C-1`. On the row to the below, it blocks the columns `C+2` and `C-2`. In other words, it blocks columns left and right its own by a number equal to the distance in rows.

So `exclude_diag` iterates over the existing solutions, keeping track of the difference in column in `Diff`, and collects the blocked diagonals.

And yes, `exclude_diag` could be written with `maplistidx` and `flatten`. This is left as an exercise to the reader.

With the code above, it is already possible to compute solutions. Here is a partial list of the 8 Queens boards:

{% codeblock queens solver lang:prolog %}
| ?- queens(8, Sol).

Sol = [4,2,7,3,6,8,5,1] ? ;

Sol = [5,2,4,7,3,8,6,1] ? ;

Sol = [3,5,2,8,6,4,7,1] ? ;

Sol = [3,6,4,2,8,5,7,1] ? ;

Sol = [5,7,1,3,8,6,4,2] ? ;

Sol = [4,6,8,3,1,7,5,2] ? 

(3 ms) yes
{% endcodeblock %}

and a complete list of the 4 Queens boards (there's only two):

{% codeblock 4 queens solutions lang:prolog %}
| ?- queens(4, Sol).

Sol = [3,1,4,2] ? a

Sol = [2,4,1,3]

no
{% endcodeblock %}

#### Formatting Predicates

The formatting predicates simply replace the basic solution with a list of lists, each representing a line of the board. Each cell is either a space for empty, or a 'Q' character for a queen.

There are 2 predicates:

 * `format_board/3`
 * `format_line/3`

`format_board` applies `format_line` to each column.

`format_line` make a empty line, then split it to insert the 'Q' at the right location.

#### Pretty Printer

The pretty printer predicates follow pretty much the same strategy as the pretty printer for Sudoku boards above.

There are 3 predicates:

 * `print_board/1`
 * `print_line/2`
 * `print_squares/1`

As the design is the same as the Sudoku pretty printer, it is not repeated here.

#### Computing and printing solutions

Finally, `run_queens` provide a top level predicate that computes then render each solution board. Here are the two boards from the 4 Queens problem:

{% codeblock 4 Queens boards lang:prolog %}
| ?- run_queens(4).

---------
| | |Q| |
---------
|Q| | | |
---------
| | | |Q|
---------
| |Q| | |
---------

true ? a
---------
| |Q| | |
---------
| | | |Q|
---------
|Q| | | |
---------
| | |Q| |
---------

true

(1 ms) no
{% endcodeblock %}

and the first two boards from the 8 Queens problem:

{% codeblock 8 Queens - first 2 boards lang:prolog %}
| ?- run_queens(8).

-----------------
| | | |Q| | | | |
-----------------
| |Q| | | | | | |
-----------------
| | | | | | |Q| |
-----------------
| | |Q| | | | | |
-----------------
| | | | | |Q| | |
-----------------
| | | | | | | |Q|
-----------------
| | | | |Q| | | |
-----------------
|Q| | | | | | | |
-----------------

true ? ;

-----------------
| | | | |Q| | | |
-----------------
| |Q| | | | | | |
-----------------
| | | |Q| | | | |
-----------------
| | | | | | |Q| |
-----------------
| | |Q| | | | | |
-----------------
| | | | | | | |Q|
-----------------
| | | | | |Q| | |
-----------------
|Q| | | | | | | |
-----------------

true ? 

(2 ms) yes
{% endcodeblock %}

#### Wrapping up n Queens problem

Given suitable utility predicates, the code to solve the n Queens problem (rather than just the 8 Queens problem) _without_ the dedicated Finite Domain predicates is actually very short (slightly more than half is the formatting and pretty printer code).

The trick is to identify the best location for branching (the `member(X, Poss)` in `validate` above), and making sure the branching is as pruned as possible.

Prolog supports many other tricks (such at the cut `!` operator) to further constrain the search tree; and of course the availability of Finite Domain extensions add expressivity and power to an already powerful base.

### Improving Sudoku

The code above is tied to the size of the board, and the list of constrains has to be written by hand (or using code outside Prolog). It does not have to be this way.

Using techniques similar to the n Queens solver, and a judicious combination of utility predicates, the list of constrains can be abstracted over.

{% include_code Flexible Sudoku lang:prolog 7l7w/prolog/sudoku.pl %}

The new code is about the size of the original 4x4 Sudoku code from the book, and clearly shorter than the 9x9 Sudoku code above. Of course it hides some or the complexity in utility predicates, but that's what libraries are for.

Now the Sudoku solver can be used on the old 4x4 problem:

{% codeblock Testing new Sudoku solver on 4x4 problem lang:prolog %}
GNU Prolog 1.4.0
By Daniel Diaz
Copyright (C) 1999-2011 Daniel Diaz
| ?- consult('utils').
....

| ?- consult('sudoku').
....

| ?- consult('sudoku_print').
....

| ?- sudoku([_, _, 2, 3,
             _, _, _, _,
             _, _, _, _,
             3, 4, _, _],
             Solution), sudoku_print(Solution).
-----------
|4 1 |2 3 |
|2 3 |4 1 |
-----------
|1 2 |3 4 |
|3 4 |1 2 |
-----------

Solution = [4,1,2,3,2,3,4,1,1,2,3,4,3,4,1,2] ? a

no
{% endcodeblock %}

as well as on 8x8 (hard) problems:

{% codeblock Testing new Sudoku solver on 8x8 problem lang:prolog %}
| ?- sudoku([_, _, _, 2, _, _, _, 6, 3,
        3, _, _, _, _, 5, 4, _, 1,
        _, _, 1, _, _, 3, 9, 8, _,
        _, _, _, _, _, _, _, 9, _, 
        _, _, _, 5, 3, 8, _, _, _, 
        _, 3, _, _, _, _, _, _, _, 
        _, 2, 6, 3, _, _, 5, _, _, 
        5, _, 3, 7, _, _, _, _, 8,
        4, 7, _, _, _, 1, _, _, _],
       Solution), sudoku_print(Solution).
----------------------
|8 5 4 |2 1 9 |7 6 3 |
|3 9 7 |8 6 5 |4 2 1 |
|2 6 1 |4 7 3 |9 8 5 |
----------------------
|7 8 5 |1 2 6 |3 9 4 |
|6 4 9 |5 3 8 |1 7 2 |
|1 3 2 |9 4 7 |8 5 6 |
----------------------
|9 2 6 |3 8 4 |5 1 7 |
|5 1 3 |7 9 2 |6 4 8 |
|4 7 8 |6 5 1 |2 3 9 |
----------------------

Solution = [8,5,4,2,1,9,7,6,3,3,9,7,8,6,5,4,2,1,2,6,1,4,7,3,9,8,5,7,8,5,1,2,6,3,9,4,6,4,9,5,3,8,1,7,2,1,3,2,9,4,7,8,5,6,9,2,6,3,8,4,5,1,7,5,1,3,7,9,2,6,4,8,4,7,8,6,5,1,2,3,9] ? a

(1 ms) no
{% endcodeblock %}

Wrapping Day 3 and Prolog
-------------------------

I really enjoyed coding in Prolog again. Backtracking is a powerful mechanism which allows clear and concise descriptions of some problems. And I can feel that there is yet a more interesting language lurking just beyond my current understanding.

The book gives a fair account of Prolog strengths and weaknesses. While I see little to no use for it in my daily activities, I wished my copy of "The Art of Prolog" was not on another continent.