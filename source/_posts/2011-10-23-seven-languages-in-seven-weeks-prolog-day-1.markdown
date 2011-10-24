---
layout: post
title: "Seven Languages in Seven Weeks Prolog Day 1"
date: 2011-10-23 16:45
comments: true
categories:  [Books]
tags: [7languages7weeks, prolog]
series: "Seven Languages in Seven Weeks"
---
The third language in the series is [Prolog](http://en.wikipedia.org/wiki/Prolog). I first encountered it at university many years ago, and found it a really exciting and different language. 
<!--more-->
The exercises we had to do at the time were significantly more complex (computing symbolic derivatives and integrals) that what the book proposes, so I have to say I didn't really learn anything.

About Prolog
------------

Prolog is really different. A lot of languages claim to be, but with unification and backtracking as the core control mechanisms, Prolog certainly stands apart.

Basically, Prolog could be seen as a kind of database engine: it is possible to define relations that represent set of facts, as in the [Relational model](http://en.wikipedia.org/wiki/Relational_model). But the notion of rules takes Prolog beyond that: each rules defines how to create new facts from known ones (either defined, or previously created from rules too); and each query is an attempt to find a fact that matches the query terms.

Given the definition above, it might seem surprising that Prolog could be good for anything but logic. Yet this is a [Turing Complete language](http://en.wikipedia.org/wiki/Prolog#Turing_completeness), and there are many other areas where a Prolog solution can feel quite natural.

Still, logic is the main strength of Prolog (which comes from the French *Pro*grammation *Log*ique), and a number of limitations of the implementations restricts it to that niche.

### A note on building GNU Prolog 1.4.0 on MacOS X 10.7 (Lion)

The book recommends using [GNU Prolog](http://www.gprolog.org/) (for reasons that will become clear on Day 3), but I have been using [SWI Prolog](http://www.swi-prolog.org/) since I first needed a reliable Prolog engine at university, so I started the exercises with the latter.

Eventually, I tried to port my code to GNU Prolog, and found that it would crash. In practice, this is what the error looks like:
```
GNU Prolog 1.4.0
By Daniel Diaz
Copyright (C) 1999-2011 Daniel Diaz
| ?- I1 is 1 + 1.

Fatal Error: Segmentation Violation
```

In other words, a basic arithmetic operation causes a segmentation fault.

Fortunately, the [users-prolog mailing list](http://lists.gnu.org/archive/html/users-prolog/) had the [answer](http://lists.gnu.org/archive/html/users-prolog/2011-07/msg00013.html): Lion uses llvm-gcc by default, which causes problems for a number of software packages.

I use [Homebrew](http://mxcl.github.com/homebrew/) to install new packages; the formula for GNU Prolog contains a declaration that requires the build to use gcc instead of llvm-gcc. But because gcc is actually llvm-gcc, somehow this declaration is not working. So, digging a bit deeper, I found another [post](http://stevesmiscellany.com/journal/brew_xcode_llvm_and_the_gcc) with the solution: an explicit `--use-gcc` flag.

So with the following:
```
sudo brew install gnu-prolog --use-gcc
```
GNU Prolog compiles into something useable.

Exercises
---------

The exercises today are very basic, 

### A knowledge base about books

Rather than a collection of my favourite books (which would take too long), I just input a few from the [Pragmatic Bookshelf](http://pragprog.com/):

{% include_code Book Knowledge Base lang:prolog 7l7w/prolog/books.pl %}

Querying it:
{% codeblock Querying the book knowledge base lang:prolog %}
| ?- book(andy_hunt, B).

B = programming_ruby ? a

B = pragmatic_thinking_and_learning

B = pragmatic_unit_testing

B = practices_of_an_agile_developer

yes
{% endcodeblock %}

The `a` after the first answer is the command to display all the solutions.

### Music Knowledge Base

{% include_code Music Knowledge Base lang:prolog 7l7w/prolog/music.pl %}

Then finding all the known guitar players is just:
{% codeblock Guitar Players lang:prolog %}
| ?- plays(A, guitar).

A = jimmy_hendrix ? a

A = eric_clapton

A = jimmy_page

A = neil_young

no
{% endcodeblock %}

Not an exercise, but just to highlight the similarity with the relational model, here is a kind of join:
{% codeblock Rock Piano Players lang:prolog %}
| ?- style(A, rock), plays(A, piano).

A = jerry_lee_lewis ? a

no
{% endcodeblock %}

And this wraps up Day 1.