---
layout: post
title: "Seven Languages in Seven Weeks Haskell Day 3"
date: 2011-11-19 14:16
comments: true
categories: [Books]
tags: [7languages7weeks, haskell]
series: "Seven Languages in Seven Weeks"
---
Last day with Haskell, and this time we grapple with classes and monads. That's pretty much where most beginners give up in disgust...
<!--more-->

And that's too bad, because both (and the rest of advanced Haskell features) are very expressive (see below the Maze problem) and powerful. But classes are unusual (and the name tends to confuse Object Oriented people), while monads appear to solve a problem that is trivial in other languages (which is not true, or mostly not true).

Classes
-------

Classes are more properly understood as interfaces, but the comparison can be misleading. Class elements are types that are guaranteed to provide implementation for specific functions. Combined with polymorphism, it allows writing functions that are more intimate with their arguments, while keeping purity and referential transparency (in some sense, it plays a role similar to functors in ML languages).

Classes nicely support monads (and other composition mechanisms) by providing interfaces these abstractions can build upon.

Monads
------

Monads are mechanisms to compose calculations. They also happen to solve the IO problem in Haskell, which is a nice (and significant) bonus. But the calculation composition is core. With it, one can have backtracking, continuation, probabilistic computing, anything you could think off. Thinking of anything actually is quite hard, as most programmers are not used to such freedom. For IO, it just happens to guarantee sequential evaluation.

There are a number of resources to learn about Monads (indeed, it seems the path to Haskell mastery must include writing at least one Monad tutorial). I found  [All About Monads](http://monads.haskell.cz/html/) very useful. The introduction is really good, but links to this tutorial tend to disappear, unfortunately. There is also the [Monads](http://book.realworldhaskell.org/read/monads.html) chapter of [Real World Haskell](http://book.realworldhaskell.org/) (which you should read, but wait for the second edition to buy). This introduction is more complex, as it builds a State Monad rather than the Maybe Monad.

Finally, [A Fistful of Monads](http://learnyouahaskell.com/a-fistful-of-monads) from [Learn You a Haskell for Great Good](http://learnyouahaskell.com/) also covers the Maybe Monad as an introduction. I have the book but did not read it yet, so I cannot comment on it, but I have seen great reviews.

Exercises
---------

### Lookup function returning Maybe

The function `my_lookup` is easy; it iterates over a list of pairs key, value, and returns `Just value` when the key matches. On empty list, it returns `Nothing`. There is no need to think about monads at this point. A key,value map is really some data structure that `Maybe` contains a specific key.

Slightly more difficult was the `testData`. The nesting was somewhat tricky to get right.

{% include_code lang:haskell 7l7w/haskell/lookup.hs %}

Once this is defined, using it with the `>>=` operator is really simple:

```
*Lookup> my_lookup 2 testData >>= my_lookup "a" >>= my_lookup "i"
Just "tada!"
*Lookup> my_lookup 2 testData >>= my_lookup "b" >>= my_lookup "i"
Nothing
```

### Solving Maze

Using the List Monad to solve problems is very similar to using Prolog: elements in a list are alternative paths; failure (which must be explicit by calling [`fail`](http://www.haskell.org/ghc/docs/latest/html/libraries/base-4.4.1.0/Prelude.html#v:fail) or [`guard`](http://www.haskell.org/ghc/docs/latest/html/libraries/base-4.4.1.0/Control-Monad.html#v:guard)) backtracks to the next alternative; [`return`] adds a solution to the list of solutions (even if there's only one possible solution, the List Monad produces a list).

To solve the maze, the algorithm do the following:

 * the `loop` subfunction is used to explore a given solution; the List Monad hides the iteration and backtracking over alternatives
 * the `loop` function is always called with the reverse path so far: the first element is actually the current position
 * if the current position is the exit position, the path is reversed then returned as solution
 * otherwise, the current node is checked, and its exits retrieved
 * the positions in the path are first removed from the exits, to avoid looping (so we never go over the same position twice)
 * the List Monad main logic starts there:
   * first `guard` that the list of possible exits is not empty
   * then select and alternative new position
   * call `loop` on the new path to explore it

The code is fairly short, and perhaps could be shorter. The backtracking is provided for free by the List Monad, but very effective to implement a search (I used it to solve Sudoku problems).

To be fair, I took some time to track a bug: instead of adding the whole current path to the recursive `loop` call, I only passed the tail. That caused `loop` to actually ignore the current path, and run in circle forever. Once fixed, the search was instantaneous.

{% include_code lang:haskell 7l7w/haskell/maze.hs %}

The rest of the code includes a parser for a specific maze description (I found sample mazes [here](http://benjamin-meyer.blogspot.com/2005/01/ascii-maze-ment-puzzle.html)), and code to process a problem into a `Maze` instance, and code to display a solution.

Testing (test data [here](https://github.com/icefox/asciimaze/blob/master/sample-mazes/input1.txt)):
```
*Maze> solveProblem "input1.txt"
 ___________________
|   | |   |        *|
| __| | __|___  __  |
|   | |       |   |*|
| __| |_____  |_  | |
|   | | | |       |*|
| __| | | |_______| |
|            * * * *|
| __________  __    |
| |   |    * *| | | |
|_|   |___  __| | |_|
|   |     |*| | | | |
|_  |_    | | | |_| |
|   |   |  *|       |
| __|_  |_  | __    |
| |     |  * *| | | |
|_| __  | __  | |_|_|
| |   | | |  *  |   |
| |   | |_|_  __|_  |
|   | | |   |* *  | |
| __|_| | __|_    | |
| | |   |  * * *|   |
|_| | __|_  __  |___|
| | |     |*| | |   |
| | | ____| | |_|_  |
|   | |* * *        |
|_  |_|   __  ______|
|    * *| |     |   |
|     __| |_  __| __|
| | |*  |   |   |   |
| |_| __|___|___|_  |
|   |* *|     |     |
|___|   |   __| ____|
|   | |* *|     |   |
| __|_|_  |     | __|
|       |*| | |     |
|_____  | |_| |_  __|
|* * *|  *|     |   |
|     |_  | __  |   |
|*| |* * *|   | | | |
|_|_|_____|___|_|_|_|
```

Wrapping up Day 3 and Haskell
-----------------------------
The exercises (ok, just the maze one) were challenging and show or at least hint at Haskell strength: the ability to compose calculations from basic elements and add advanced control mechanisms almost transparently.

I enjoy Haskell; it allows to think at a higher level, to see computations from different angles, in a way that expands the mind (sometimes painfully). And while it is certainly not mainstream, there are high quality niches (banking and financial companies) that justify investing (stupid pun intended) in Haskell.