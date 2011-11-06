---
layout: post
title: "Seven Languages in Seven Weeks Erlang Day 2"
date: 2011-11-05 18:22
comments: true
categories: [Books]
tags: [7languages7weeks, erlang]
series: "Seven Languages in Seven Weeks"
---
Second day with Erlang, this time to cover basic controls, and more functional goodies such as anonymous and higher-order functions, list functions and list comprehensions.
<!--more-->
Erlang's support for list processing is quite extensive (as it should be for a language with limited mutable state). Using is properly requires the kind of mental twist that is needed for effective (set oriented) SQL usage. But once acquired, it is hard to get back to generic imperative programming.

This chapter introduces various functions from the `lists` module: `lists:map`, and `lists:foreach`. I probably should not have used the former in [yesterday](/blog/2011/11/04/seven-languages-in-seven-weeks-erlang-day-1/)'s "Counting to 10" exercise. The latter was more appropriate:

{% include_code New version of Count module lang:erlang 7l7w/erlang/count_2.erl %}

(also changed: I'm using the tilde format escape character, rather than backslash. Erlang is a child of Lisp, not C).

Exercises
---------

### Dictionary lookup

Just using pattern matching, the solution is very short an clean.

I named the module `dictionary` as the name `dict` was already used in the standard library. Erlang has a flat module naming system, so conflicts are bound to happen.

As it happens, Erlang [lists:keyfind]() TODO add reference already implements this feature, as shown in `lookup_alt`.

{% include_code Dictionary module lang:erlang 7l7w/erlang/dictionary.erl %}

Testing the code:

{% codeblock Testing Module dictionary lang:erlang %}
1> c(dictionary).
{ok,dictionary}
2> D = [{erlang, "a functional language"}, {ruby, "an OO language"}, {java, "a soso language"}].
[{erlang,"a functional language"},
 {ruby,"an OO language"},
 {java,"a soso language"}]
3> dictionary:lookup(erlang, D).
"a functional language"
4> dictionary:lookup(java, D).  
"a soso language"
5> dictionary:lookup(c, D).   
false
6> dictionary:lookup_alt(erlang, D).
{erlang,"a functional language"}
{% endcodeblock %}

### Computing prices

Once again, pattern matching makes it really easy to write such function. For extra credit (ok, no credit. Just for fun), I also wrote a function that depends on `lists:map` (TODO add reference). 

It just shows how easy it is to process lists of things in Erlang or any other decent functional language (one with pattern matching and list comprehension, that is).

{% include_code Price module lang:erlang 7l7w/erlang/price.erl %}

Testing both functions, they return the same answer (always a good thing for functions meant to have identical meaning):

{% codeblock Testing Module price lang:erlang %}
1> c(price).
{ok,price}
2> Items = [{processor, 5, 200}, {memory, 4, 100}, {screen, 1, 1000}, {drive, 3, 150}].
[{processor,5,200},
 {memory,4,100},
 {screen,1,1000},
 {drive,3,150}]
3> price:compute_map(Items).
[{processor,1000},{memory,400},{screen,1000},{drive,450}]
4> price:compute_lc(Items).
[{processor,1000},{memory,400},{screen,1000},{drive,450}]
{% endcodeblock %}

### Playing Tic-Tac-Toe

I have to admit I cannot stand hard coding anything. Whenever I have a choice between hard coding and generating coding, I'll pick the latter every single time.

So for the Tic-Tac-Toe exercise, I wrote code that computes the list of potential victory lines, even though the list for a board of 3 by 3 is much shorter. When Tic-Tac-Toe is finally played on 19 by 19 boards (as grown up games tend to be), my code will be ready...

I used a small utility module to transpose a matrix; the code is very similar, and indeed, lifted, from a previous Prolog [exercise](/blog/2011/10/24/seven-languages-in-seven-weeks-prolog-day-3/) (the Sudoku one).

{% include_code Matrix module lang:erlang 7l7w/erlang/matrix.erl %}

Once thing to note: as in the `export` statement, a function name must include its arity when passed to higher order functions. See above for `fun head/1` for instance.

Once this is defined, the code for Tic-Tac-Toe is fairly simple:

 * generate a list of lines on a board (`tictactoe:make_winners`) *
 * for each of these lines, determine if it belongs to a single player (`tictactoe:check_align`, called from `tictactoe:check`)
 * if any player owns a line, declare her the winner (`tictactoe:check`, using `tictactoe:player`)
 * otherwise, depending on whether there is any non player own square, declare the game over or undecided (`tictactoe:check`, using `tictactoe:free`).

{% include_code Checking Tic-Tac-Toe lang:erlang 7l7w/erlang/tictactoe.erl %}

Testing is a bit tedious, because of the way the board is defined. First a winner on either diagonal is checked, then a draw, then an unfinished game:

{% codeblock Testing Module tictactoe lang:erlang %}
1> c(tictactoe).
{ok,tictactoe}
2> tictactoe:check([x, n, n, o, x, n, n, o, x]).
x
3> tictactoe:check([n, o, x, n, x, n, x, o, n]).
x
4> tictactoe:check([x, o, x, o, x, x, o, x, o]).
cat
5> tictactoe:check([o, x, n, x, x, o, n, o, n]).
no_winner
{% endcodeblock %}

Wrapping Up Day 2
-----------------

Erlang functional features make it very concise (adding currying would be sweet, though); the resulting code is short, readable, and flexible.

Despite a non mainstream pedigree, clearly this is a language whose design has been guided by actual usage and experience.