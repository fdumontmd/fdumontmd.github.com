---
layout: post
title: "Seven Languages in Seven Weeks Scala Day 1"
date: 2011-10-28 16:43
comments: true
categories: [Books]
tags: [7languages7weeks, scala]
series: "Seven Languages in Seven Weeks"
---
The language for this week is [Scala](http://www.scala-lang.org/), which attempts (among other things) to brings functional programming and a new concurrency model to the JVM.
<!--more-->
This was a language I was really keen to know more about. I am using Java a lot, professionally, so I am always looking for options in this ecosystem. I am pretty sure the next book after this one will be a Scala book.

Scala Types
-----------
The first thing we learn about Scala is that it is "strongly typed." This is not a very helpful description, as the [definition](http://en.wikipedia.org/wiki/Strong_typing) is fairly loose. And the proof offered by the author that the language is strongly typed is strange:

{% codeblock Scala is strongly typed? lang:scala %}
scala> 4 * "abc"
<console>:8: error: overloaded method value * with alternatives:
  (x: Double)Double <and>
  (x: Float)Float <and>
  (x: Long)Long <and>
  (x: Int)Int <and>
  (x: Char)Int <and>
  (x: Short)Int <and>
  (x: Byte)Int
 cannot be applied to (java.lang.String)
              4 * "abc"
                ^
{% endcodeblock %}

So a string cannot be multiplied by a number? By this standard, [C](http://en.wikipedia.org/wiki/C_%28programming_language%29) is strongly typed as well:

{% codeblock What about C? lang:c %}
#include <stdio.h>

int main() {
	char *str = "abc";

	printf("%s\n", (4*str));

	return 0;
}
{% endcodeblock %}

```
$ gcc -o typed typed.c 
typed.c: In function ‘main’:
typed.c:6: error: invalid operands to binary * (have ‘int’ and ‘char *’)
```

As Scala has a theoretical background, perhaps it is useful to move to the language of [Type Theory](http://en.wikipedia.org/wiki/Type_theory).

A more useful definition than strong typing, is whether a type system is expressive: what do types of expressions tell about how the expressions can be combined, and what to expect when these expressions are evaluated. From this perspective, C types are unexpressive (barely above assembly language), Java's much better, [OCaml](http://caml.inria.fr/ocaml/), and [Haskell](http://haskell.org/haskellwiki/Haskell) better still, and [Coq](http://coq.inria.fr/) and other dependently-typed languages are perhaps the most expressive of all.

{% blockquote Benjamin C. Pierce, Types and Programming Languages, http://www.cis.upenn.edu/~bcpierce/tapl/ %}
A type system is a tractable syntactic method for proving the absence of certain program behaviors by classifying phrases according to the kinds of values they compute.
{% endblockquote %}

From this perspective, type expressivity is a static (i.e. compile time) feature only. The dynamic equivalent is a type is a tag, i.e. the runtime information about what operations a value supports, so it is impossible to prove any behaviour as impossible before running the program.

Another scale to judge a type system is how verbose it is. In C, as in Java, the type of everything has to be declared. In dynamic languages, no type is needed (or even possible). OCaml and Haskell can figure out the type of most expressions with no need for declarations (which are still useful for documentation). Scala falls somewhere between Java and OCaml. It has [type inference](http://en.wikipedia.org/wiki/Type_inference) over portions of a program.

So when it comes to Scala types (or any type system, really), I have two criteria: whether it is expressive enough to be used to enforce specific properties over portions of a program, and whether it is concise enough that using it does not become a major effort.

Scala syntax
------------

Scala reminds me of OCaml, vaguely. The syntax is fairly concise (especially compared to Java). Scala has list comprehension (as in Python or Haskell), ranges, singletons (`object`), and mixins (`trait`).

The syntax for the definition of functions (that is, methods that return a value) is somewhat strange, as it requires an equal sign:

{% codeblock Function definition lang:scala %}
def my_fun(i: Int): Int = {
	return i+1
}
{% endcodeblock %}

Procedures (methods that return nothing) only need one when they are defined as returning `Unit` (which is `void` for Scala):


{% codeblock Function definition lang:scala %}
def my_proc(str: String) {
	println(str)
}

def my_proc2(str: String): Unit = {
	println(str)
}

{% endcodeblock %}

Nothing overly complex, and if I believe the error message ("warning: Detected apparent refinement of Unit; are you missing an '=' sign?"), the reason for this syntax is to support other constructs. Still, I had this error a few times as I wrote my code, before I finally internalized the rule.

The rest of the syntax (as introduced today) is straightforward, and easy to get used to (at least for me).

Exercises
---------

For a first day, the exercise was rather demanding.

### Tic-Tac-Toe

My first version is, admittedly, ugly. I build a list of the possible lines (i.e. rows, columns or  diagonals) in the board (as a list of pairs of indices) in `lines`, then in the function `winner` I iterate over this list and check if the positions for each pair have the same content, and if this content belong to a player or not.

For the bonus part, I use `print_board`, which displays a board with numbers on unused locations, and `play`, which asks players in turn for their move, checks whether the move is valid, whether it is a winning move, and switches between players when needed.

The `play` function relies on variables more than I would like, but on the other hand it is harder to model a board with no update at all.

{% include_code Tic-Tac-Toe lang:scala 7l7w/scala/tictactoe.scala %}

When running it, it will first test the code by playing a predefined game. Then the bonus code is run, and an interactive game can be tried: 

```
$ scala tictactoe.scala 
Test on empty board
-----
|123|
|456|
|789|
-----
-----
|123|
|4X6|
|789|
-----
-----
|O23|
|4X6|
|789|
-----
-----
|O2X|
|4X6|
|789|
-----
-----
|OOX|
|4X6|
|789|
-----
-----
|OOX|
|4X6|
|X89|
-----
After game:
Player X is the winner
And now play:
-----
|123|
|456|
|789|
-----
1 Player X move:
5
-----
|123|
|4X6|
|789|
-----
2 Player O move:
1
-----
|O23|
|4X6|
|789|
-----
3 Player X move:
2
-----
|OX3|
|4X6|
|789|
-----
4 Player O move:
4
-----
|OX3|
|OX6|
|789|
-----
5 Player X move:
8
Player X is the winner
```

The interactive code is not great (see below for an improvement); especially a draw 

#### Tic-Tac-Toe with class

A second version, slightly cleaned up (but presumably still ugly), this time in a class. Only one variable remains (the board); everything else is immutable.

{% include_code Tic-Tac-Toe with class lang:scala 7l7w/scala/tictactoe_class.scala %}

It still handles normal victory:
```
$ scala  tictactoe_class.scala 
-----
|123|
|456|
|789|
-----
Player X move: 5
-----
|123|
|4X6|
|789|
-----
Player O move: 1
-----
|O23|
|4X6|
|789|
-----
Player X move: 2
-----
|OX3|
|4X6|
|789|
-----
Player O move: 4
-----
|OX3|
|OX6|
|789|
-----
Player X move: 8
Player X won!
```

as well as the more common case of draw:

```
$ scala  tictactoe_class.scala 
-----
|123|
|456|
|789|
-----
Player X move: 5
-----
|123|
|4X6|
|789|
-----
Player O move: 1
-----
|O23|
|4X6|
|789|
-----
Player X move: 2
-----
|OX3|
|4X6|
|789|
-----
Player O move: 8
-----
|OX3|
|4X6|
|7O9|
-----
Player X move: 3
-----
|OXX|
|4X6|
|7O9|
-----
Player O move: 7
-----
|OXX|
|4X6|
|OO9|
-----
Player X move: 4
-----
|OXX|
|XX6|
|OO9|
-----
Player O move: 6
-----
|OXX|
|XXO|
|OO9|
-----
Player X move: 9
Draw!
```

Wrapping Up Day 1
-----------------

Scala seems easy enough to get started with. The syntax is readable; the semantic close enough to Java's (and other mainstream languages) that there is little surprise.