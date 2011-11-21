---
layout: post
title: "Seven Languages in Seven Weeks Haskell Day 3.5"
date: 2011-11-21 13:21
comments: true
categories: [Books]
tags: [7languages7weeks, haskell]
series: "Seven Languages in Seven Weeks"
---
Haskell Day 3 had one exercise I forgot about: to implement monads in a different language. In this post I correct this oversight.
<!--more-->
There are various implementations of monads for other languages. Some are well suited for the exercise (because they have a flexible syntax like Clojure, for instance). With others the whole thing sticks out like a sore thumb. I think the latter is useful to highlight Haskell's features.

I choose Java, which shows how helpful Haskell's syntax and type system really is, compared to a mainstream language. Regarding types, maybe generics would have helped, but I'm not familiar enough with them to figure it out (for professional reasons, I got stuck with Java 1.4 for quite a long time).

I implements a List Monad, and creates a list of pairs where the first element is smaller than the second. In Haskell, this is what it would look like:

{% codeblock List Monad example lang:haskell %}
do x <- [1..5]
   y <- [1..5]
   if x < y 
     then return (x,y) 
     else fail "Not a valid pair"
{% endcodeblock %}

Haskell's notation for functions is especially useful here. Note how the Java codes forces me to tie `MakePair1` and `MakePair2`, while the Haskell equivalent seems to have no special construct whatsoever (the do notation hides the underlying anonymous functions).

{% include_code lang:java 7l7w/haskell/ListMonad.java %}

The implementation is otherwise fairly straightforward given the definition of monads:

 * `monadDo` and `wrap` (Haskell's `return`) both create a new `ListMonad`;
 * `fail` is also a way to create a `ListMonad` which represents failure. I pass a `String` argument to have the same signature as Haskell's `fail`, but in List Monads such argument is ignored, so I ignore it here as well;
 * `bind` is the `>>=` version, which collects the results of the `func` argument run over each element of the current List content; a new `ListMonad` with the results as content is returned
 * finally, `getContent` gives back the current content of the `ListMonad`.

The test code is nothing fancy (I certainly wouldn't want to try and write my maze solving algorithm in Java using the ListMonad), but it tests all the features.

Running it produces the expected output:
```
(1, 2)
(1, 3)
(1, 4)
(1, 5)
(2, 3)
(2, 4)
(2, 5)
(3, 4)
(3, 5)
(4, 5)
```

Aren't you glad you can write this in Haskell instead?