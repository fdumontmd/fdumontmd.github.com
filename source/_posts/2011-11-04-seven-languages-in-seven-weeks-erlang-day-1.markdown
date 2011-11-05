---
layout: post
title: "Seven Languages in Seven Weeks Erlang Day 1"
date: 2011-11-04 12:51
comments: true
categories: [Books]
tags: [7languages7weeks, erlang]
series: "Seven Languages in Seven Weeks"
---
New week, new language. This time is the turn of [Erlang](http://www.erlang.org/), which comes from [Ericsson](http://www.ericsson.com/) (the phone company), and was used internally on internal products before being unleashed on the world.
<!--more-->
With it's root in the telecom industry, it is not surprising that Erlang has a particular focus on availability and reliability; it comes with a strong set of tools for network applications, but little for end user interaction (the GUI is minimalist).

The most surprising aspect of Erlang's origin, perhaps, is that the first version was an interpreter written in Prolog (actually, Erlang was meant as kind of extension to Prolog).

This is particularly obvious in Erlang's syntax. Prolog programmers should feel mostly at home.

Erlang is functional
--------------------

Erlang is also a purely functional language, with little to no updates (there are updatable stores, but you really have to look for them). Variables can be assigned a value only once, using a mechanism similar to unification (further assignments are valid only to the extend that both sides are identical, or the left hand is an unassigned variable).

{% codeblock Erlang: assignment lang:erlang %}
Eshell V5.8.5  (abort with ^G)
1> V = 3.
3
2> V = 3.
3
3> V = 4.
** exception error: no match of right hand side value 4
4> V = X.
* 1: variable 'X' is unbound
5> X = V.
3
{% endcodeblock %}

Erlang recursion
----------------

The book introduces recursion in the module “yet_again”. Now, recursion is nothing special (few languages don't support it), but functional languages tend to rely on it, so they often have a special optimization called [tail recursion](http://en.wikipedia.org/wiki/Tail_call).

Typically, any function call allocate some space on the stack, which is usually a limited resource. A language that relies on recursion rather than iteration risks to run out of stack.

Tail recursion is an optimization technique that replaces a function call by a jump, reusing the stack space already allocated. It effectively turns a recursion into an iteration.

In the book, both defined functions (`another_factorial` and `another_fib`) use non tail recursion (also referred to as body recursion). It is clear that `another_fib`'s performance is going to be terrible (it is doubly recursive); I was surprised to find that `another_factorial`'s performance is actually pretty good.

I wrote tail recursive versions of both functions (simply using an accumulator):

{% include_code My Yet Again module lang:erlang 7l7w/erlang/yet_again.erl %}

I also added some code to measure the time of any of these functions.

Clearly, `tr_fib` is much faster than `another_fib` (the former is, through tail recursion optimization, iterative, while the latter is double recursive). But it turns out that `another_factorial` is systematically faster (although not by much) than `tr_fact`, at least on my machine, even for pretty large input numbers.

I could not find a definitive reason why this is so (the closest I came was suggested by this [post](http://erlang.org/pipermail/erlang-questions/2007-July/028155.html) and subsequent messages, which suggest the order of multiplication in the tail recursive version causes the accumulator to grow quickly and triggers more garbage collection than the body recursive version).

Still, it is enough to draw a few conclusions:

 * never assume anything about the performance of a piece of code without measuring it;
 * Erlang's stack handling is awesome;
 * Erlang's optimization is just as impressive.

Exercises
---------

### Couting words

I define a word as a sequence (at least 1) of non-space characters. I use two functions: one that looks for the beginning of words, and one that look for the end of words. That way I always know what state I'm in (between words, or in one), and can keep the count correctly.

Both functions are body-recursive, which as seen above is probably not a problem.

{% include_code Counting Words lang:erlang 7l7w/erlang/cw.erl %}

Testing the module:

{% codeblock Testing Module cw lang:erlang %}
1> cw:count_words("   a  lot   of   space   ").
4
2> cw:count_words("not a lot of space").
5
3> cw:count_words("word").
1
{% endcodeblock %}

### Counting to 10

Erlang has a good support in the standard library for usual functional programming idioms, such as generating lists, and applying a function over elements of a list.

{% include_code Counting to 10 lang:erlang 7l7w/erlang/count.erl %}

The code is more generic than strictly required (which is always good):

{% codeblock Testing Module count lang:erlang %}
1> count:count_to_10().
1
2
3
4
5
6
7
8
9
10
[1,2,3,4,5,6,7,8,9,10]
2> count:count_to(5).
1
2
3
4
5
[1,2,3,4,5]
{% endcodeblock %}

### Matching input

Nothing fancy here; just basic pattern matching and some calls from the standard library.

{% include_code Pattern Matching lang:erlang 7l7w/erlang/check.erl %}

The code recognizes the input as requested. Everything else will generate an error.

{% codeblock Testing Module check lang:erlang %}
1> c(check).
{ok,check}
2> check:print_result(success).
success
ok
3> check:print_result({error, "Something went wrong!"}).
Error: Something went wrong!
ok
{% endcodeblock %}

And so we conclude Day 1.