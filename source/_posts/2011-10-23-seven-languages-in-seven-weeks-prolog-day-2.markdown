---
layout: post
title: "Seven Languages in Seven Weeks Prolog Day 2"
date: 2011-10-23 23:10
comments: true
categories: [Books]
tags: [7languages7weeks, prolog]
series: "Seven Languages in Seven Weeks"
---
The second day with Prolog is about the main data structure (the list) and the writing of rules.
<!--more-->

### Lists
Lists in Prolog are just the same as in Lisp: either empty, or a pair with the head (a single element) and the rest of the list:
{% codeblock Lists in Prolog lang:prolog %}
| ?- [1,2,3] = L.

L = [1,2,3]

yes
| ?- [1,2,3] = [H|T].

H = 1
T = [2,3]

yes
| ?- [1,2,3] = [E1|[E2|T]].      

E1 = 1
E2 = 2
T = [3]

yes
| ?- [1,2,3] = [E1, E2, E3].

E1 = 1
E2 = 2
E3 = 3

yes
{% endcodeblock %}

### Rules

Rules are superficially similar to the definition of functions in functional languages (especially [Erlang](http://www.erlang.org/)), but the similarity is treacherous. In both, rules or functions can have multiple definitions, each with different patterns; in both, the pattern must match for the rules to fire or the function to be executed; but in functional languages, only the body of the first matching pattern will be executed, whereas in Prolog all the matching patterns can fire (there are ways to control that, but they are not covered in this book).

Another way in which they differ is that they do not return a value: whatever they return must be unified against one of the parameters. While this might appear clumsy, it has a significant benefit: rules can relate parameters in more than one direction (see the section 'Using Rules in Both Directions' in the book). Consider this:
{% codeblock Rules invocation lang:prolog %}
| ?- length([1,2,3], L).

L = 3

yes
| ?- length(L, 3).

L = [_,_,_]

yes
{% endcodeblock %}

The first query is a typical invocation; the second ask what a list of length 3 might look like. Rules do not always support such multiple interpretations, and it is not always easy to know which does. But writing such rules is like writing functions that handle infinite data structure in Haskell: a necessary step on the way from superficial knowledge of the language to deeper understanding.

The second answer is a list of 3, unbound variables. Combined with other rules, 

Sometimes the alternative interpretation is unbounded. Consider [`member`](http://www.gprolog.org/manual/gprolog.html#htoc209):
{% codeblock member invocation lang:prolog %}
| ?- member(X, [1,2,3]).

X = 1 ? a

X = 2

X = 3

yes
| ?- member(1, L).

L = [1|_] ? ;

L = [_,1|_] ? ;

L = [_,_,1|_] ? ;

L = [_,_,_,1|_] ? ;

L = [_,_,_,_,1|_] ? ;

L = [_,_,_,_,_,1|_] ? ;

L = [_,_,_,_,_,_,1|_] ? ;

L = [_,_,_,_,_,_,_,1|_] ? 

yes
{% endcodeblock %}

The first query will match for each element (3 in total), as `X` is unconstrained. But the second query tries to find what a list that contains 1 look like; obviously there is an infinite number of such list (so I aborted the query after the 8 first answers).

I could constrain the list in different ways, for instance:
{% codeblock Constrained member invocation lang:prolog %}
| ?- length(L, 3), member(1, L).

L = [1,_,_] ? a

L = [_,1,_]

L = [_,_,1]

yes
{% endcodeblock %}

Or more interestingly, querying the sublists of length 3 of another list:
{% codeblock Sublists of length 3 lang:prolog %}
| ?- length(L, 3), sublist(L, [1,2,3,4,5]).  

L = [3,4,5] ? a

L = [2,4,5]

L = [2,3,5]

L = [2,3,4]

L = [1,4,5]

L = [1,3,5]

L = [1,3,4]

L = [1,2,5]

L = [1,2,4]

L = [1,2,3]

no
{% endcodeblock %}
Now, if you have read the book already past Prolog Day 3, think about what the kind of code above could have done to reduce the list of variables.

### Higher-order Rules

Prolog would be a very limited language without a way to build queries from individual components. Fortunately, it comes  with a number of predicates that let you do just that.

[`call`](http://www.gprolog.org/manual/gprolog.html#htoc67) is one such predicates, and it is fairly powerful.

The most basic invocation just specifies the target predicate, then the arguments, each as a separate argument to `call`:

{% codeblock call invocation lang:prolog%}
| ?- call(length, L, 3).

L = [_,_,_]

yes
{% endcodeblock %}

Passing the predicate as a atom via a variable is also supported:

{% codeblock Dynamic predicate invocation lang:prolog %}
| ?- P = length, call(P, L, 3).

L = [_,_,_]
P = length

yes
{% endcodeblock %}

The actual predicate could come from even stranger places:

{% codeblock Dynamic predicate computation lang:prolog %}
| ?- member(P, [length]), call(P, L, 3).

L = [_,_,_]
P = length

yes
{% endcodeblock %}

Finally, `call` supports a kind of currying (partial application): the first argument can specify it's first parameters:
{% codeblock Dynamic predicate computation lang:prolog %}
| ?- member(P, [member(x), member(y)]), length(L, 2), call(P, L).

L = [x,_]
P = member(x) ? a

L = [_,x]
P = member(x)

L = [y,_]
P = member(y)

L = [_,y]
P = member(y)

yes
{% endcodeblock %}

Here `P` is unified with two partially applied predicates, `member(x)` and `member(y)`. Then each is applied to a list of length 2. Note that backtracking works across `call`.

As a side note: a predicate followed by a list of arguments (either variables or atoms or other valid Prolog values) is called a structure. It is a kind of names tuple. In a Prolog file, it can be used to define facts. At the Prolog prompt, it can be used to run queries. But it can also be used as generic data structure, or as argument to `call` and similar functions.

Exercises
---------

After the preamble above, the exercises won't be too taxing.

### Hanoi Tower

This was not exactly an exercise, but the implementation is really simple. It doesn't take too long to come up with a recursive algorithm (and for those who really can't, check the Wikipedia [page](http://en.wikipedia.org/wiki/Tower_of_Hanoi)):

* to move one disk from a peg to another, just do it;
* to move a stack of disks from one peg to a second one, first move all but the last disk to the third peg, move the last disk to the second peg, then move all the disks previously moved from third to second.

A translation in Prolog is equally simple:
{% include_code Hanoi Tower lang:prolog 7l7w/prolog/hanoi.pl %}

It should be easy to match the algorithm to the implementation.

{% codeblock Testing Hanoi implementation lang:prolog %}
| ?- hanoi(3).
Move disk from left to right
Move disk from left to center
Move disk from right to center
Move disk from left to right
Move disk from center to left
Move disk from center to right
Move disk from left to right

true ? ;

no
{% endcodeblock %}

### Not expressions

Prolog cannot handle not expressions properly. Fundamentally, all it can say is whether a query can be proven to true. If it says no, it just means it could not be proven true, which is slightly different from being proven false.

Because Prolog is a Turing complete language, it could also fail to return an answer to a specific query, meaning either the query is false, or it just needs a bit more time to be proven true...

There are newer languages ([Coq](http://coq.inria.fr/) for instance) that were designed as not Turing complete (which makes them interesting. Anybody can invent a Turing complete language. It takes far more work to come up with a useful language that isn't complete), and can handle not expressions over larger logic domains.

### Reverse elements in a list

I am using an accumulator to build the reverse. Note that in Prolog, predicates have a specific number of arguments called the predicate arity. Predicates with the same name but different arity are different predicates. In the solution below, I have two predicates: `my_reverse/2` and `my_reverse/3`.

{% include_code my_reverse lang:prolog 7l7w/prolog/reverse.pl %}

Let's ignore the first line for a moment. `my_reverse/2` invokes `my_reverse/3`, passing an empty accumulator. `my_reverse/3` just iterates over the element of the list, adding them to the accumulator. When it runs out of element, the accumulator becomes the result.

First, the code works for at least the basic case:

{% codeblock my_reverse, basic case lang:prolog %}
| ?- my_reverse([1,2,3], R).

R = [3,2,1]

yes
{% endcodeblock %}

More interestingly, the code also works when the parameters are both variables, or partially defined lists:

{% codeblock my_reverse, undefined or partially defined parameters lang:prolog %}
| ?- my_reverse(L, R).

L = []
R = [] ? ;

L = [A]
R = [A] ? ;

L = [A,B]
R = [B,A] ? ;

L = [A,B,C]
R = [C,B,A] ? ;

L = [A,B,C,D]
R = [D,C,B,A] ? ;

L = [A,B,C,D,E]
R = [E,D,C,B,A] ? 

yes
| ?- L = [1,2,3|_], my_reverse(L, R).

L = [1,2,3]
R = [3,2,1] ? ;

L = [1,2,3,A]
R = [A,3,2,1] ? ;

L = [1,2,3,A,B]
R = [B,A,3,2,1] ? ;

L = [1,2,3,A,B,C]
R = [C,B,A,3,2,1] ? ;

L = [1,2,3,A,B,C,D]
R = [D,C,B,A,3,2,1] ? ;

L = [1,2,3,A,B,C,D,E]
R = [E,D,C,B,A,3,2,1] ? 

(1 ms) yes
{% endcodeblock %}

The first query just states that both lists are reverse of each other, but does not mention any content. There is an infinite number of such list, and the code dutifully return them in order; the fresh variables used as elements for the first list are indeed in reverse order in the second.

The second query specifies a prefix, but not the end of the first parameter. Once again, there is an infinite number of possible answers, a few of which are shown above.

Now back to the first line in the definition: the code checks if the second parameter is [`compound`](http://www.gprolog.org/manual/gprolog.html#htoc71), in other word if it is at least partially defined. This is to make the following query work:

{% codeblock my_reverse, reversed parameters lang:prolog %}
| ?- my_reverse(L, [1,2,3]).

L = [3,2,1]

yes
{% endcodeblock %}

Without this line, the first result would be returned, but then Prolog would hang looking for a second (non existing) result. I must admit I do not fully understand why (rules with multiple interpretations are complex to design). I also make sure this rule is the only one to match but using the cut `!` operator: once the rule has started to match, any backtracking that could have happened in this rule is _cut_. In other words, the second pattern will not match, even though it could.

Backtracking introduced by previous rules is still available:

{% codeblock cut and backtracking lang:prolog %}
| ?- member(L, [[1,2,3], [4,5,6]]), my_reverse(R, L).

L = [1,2,3]
R = [3,2,1] ? ;

L = [4,5,6]
R = [6,5,4]

yes
| ?- 
{% endcodeblock %}

Here, the `member` clause can backtrack over the two elements; the cut in `my_reverse` does not prevent it.

### The smallest element in a list

This time the code is not fancy at all. Using arithmetic operators (such as min) tends to constraint the code in a way that prevent fancy use (as in `my_reverse` above). So `my_min` does what it needs to do as simply as possible:

{% include_code my_min lang:prolog 7l7w/prolog/min.pl %}

Either there's only one element in the list, and this is the minimum, or the minimum of a list is the minimum between the first element of the list, and the minimum of the rest of the list.

### Sort the elements of a list

For this exercise, I will use the insert sort. Slow but easy. Faster implementations are provided as standard predicates in decent Prologs anyway.

{% include_code, my_sort lang:prolog 7l7w/prolog/sort.pl %}

Once again, nothing fancy: the empty list is already sorted; to sort a larger list, sort the tail of the list, then insert the head at the right position.

To insert an element in a sorted list, if the list is empty, then the singleton list with that element is the answer. Otherwise, compare the element to the head: if smaller, the element is prefixed as new head of the list; otherwise, insert the element in the rest of the list.

Wrapping up Day 2
-----------------

As I mentioned [yesterday](/blog/2011/10/23/seven-languages-in-seven-weeks-prolog-day-1), Prolog really is different. I suspect many readers among those who had no previous exposure to this language must have been left rather confused. The problem is that Prolog introduces a lot of features (unification, pattern matching, backtracking) that are unusual in mainstream languages. Perhaps a different order in the languages (Erlang first?) would have helped assimilate some of these features before tackling backtracking.