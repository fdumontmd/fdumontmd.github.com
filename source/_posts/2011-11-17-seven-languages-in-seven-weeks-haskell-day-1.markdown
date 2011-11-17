---
layout: post
title: "Seven Languages in Seven Weeks Haskell Day 1"
date: 2011-11-17 07:58
comments: true
categories: [Books]
tags: [7languages7weeks, haskell]
series: "Seven Languages in Seven Weeks"
---
The final language of the book is Haskell. Haskell is challenging and satisfying in many ways. It can be elegant and slow, or, in the hands of experts, elegant and fast (ugly but fast is usually for intermediate users).
<!--more-->
I had learned Haskell already, so I did not really benefit from this chapter. Still, some of the exercises were fun (and I tried to improve those that were not).

### On 4 + 1.0

The book does not explain what happens in this expression, but I believe there is something deep and cool about Haskell here. When typing `4 + 1.0` in ghci, the output comes back as `5.0`, as expected. But the actual behaviour is not exactly usual.

Most languages would do that kind of processing:

 * when parsing, 4 is tagged as integer, while 1.0 is tagged as float;
 * at runtime, the operator `+` checks its arguments:
   * if the types are different, it promotes the argument with the least general type to the other type. In this case, 4 is promoted to float;
   * then it call the addition routine for the correct type.

Haskell does nothing like that. The type of `+` is `(+) :: Num a => a -> a -> a`. In other words, its arguments must have the same type (it is similar to Ocaml in this regard). Yet the operation succeeds.

This is because what the parser does is similar to replacing the numeric literal by a call to the [`read`](http://haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#v:read) function, with just enough constraints on the type to be compatible with the syntax of the literal.

Let's look at that `read` function. First of all, it cannot be called with no type constraint:

```
Prelude> read "4.0" :: Int
*** Exception: Prelude.read: no parse
Prelude> read "a"

<interactive>:1:1:
    Ambiguous type variable `a0' in the constraint:
      (Read a0) arising from a use of `read'
    Probable fix: add a type signature that fixes these type variable(s)
    In the expression: read "a"
    In an equation for `it': it = read "a"
```

What Haskell says here (quite forcefully) is that it does not know how to interpret the string "4" without a specific type to guide it. Lets give it one:

```
Prelude> read "4" :: Int
4
```

If we propose to read "4" as an `Int`, `read "4"` returns an `Int` of value 4. We can try with other types:

```
Prelude> read "4" :: Float
4.0
```

Same thing here: we constraint the value of `read "4"` to be a `Float`, so it is one. Notice how the type system can feed constraints back into a function through the return value.

So what happened in `4 + 1.0`? The parser replaced the literals by calls to `read`, with constraints on literals whose syntax cannot be an Integral value (such as 1.0, which indicates a `Float`). So what is really executed is

```
Prelude> read "4" + (read "1.0" :: Float)
5.0
```

The `+` operator constraints both operands to the same type, so `read "4"` receives the constraint `:: Float` as well; both operands have the same type, always. There is no implicit conversion happening at run time.

### On ranges

The book mentions that ranges can be built with fractional numbers, but this is really a bad idea. Haskell does support it, but will use float arithmetic, which has notorious problems for such usage. Consider this:
```
Prelude> [1, 0.9 .. 0]
[1.0,0.9,0.8,0.7000000000000001,0.6000000000000001,0.5000000000000001,0.40000000000000013,0.30000000000000016,0.20000000000000018,0.1000000000000002,2.220446049250313e-16]
Prelude> 
Prelude> map (/ 10) [10, 9 .. 0]
[1.0,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1,0.0]
Prelude> 
```

The first version (fractional ranges) accumulates small errors, but the second version (integral ranges mapped into fractional ranges) is correct.

Exercises
---------

### The Haskell wiki

The wiki is [here](http://haskell.org/haskellwiki/Haskell).

### allEven

I have 6 (or 7) variants of `allEven`. The most natural one is `allEven_2'`, based on [`filter`](http://haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#v:filter). `allEven_5` is the same as the book version, but abstract the recursion away in [`foldr`](http://haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#v:foldr). `allEven_6` is strict, which means it does not work on infinite list. I tried to explain how it works [here](/blog/2011/11/11/haskell-foldr-as-foldl/). Of course there would never be any good reason to use anything like it, it is merely the kind of jokes Haskell programmers (some of them at least) enjoy (some of the time).

{% include_code lang:haskell 7l7w/haskell/alleven.hs %}

### reverse

The natural way would of course to use [`reverse`](http://haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#v:reverse). `reverse_1` is the one most beginners would think of. `reverse_2` uses an accumulator, which is abstracted in the [`foldl`](http://haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#v:foldl) function, as seen in `reverse_3` or `reverse_4`. Finally, `reverse_5` is the same kind of joke as `allEven_6` above, based on expressing `foldl` in terms `foldr`, as I explained [here](/blog/2011/11/09/haskell-foldl-as-foldr/).

{% include_code lang:haskell 7l7w/haskell/myreverse.hs %}

### Colour pairs

This is simple with list comprehensions. A way to ensure that only one of (black, blue) and (blue, black) is available is to filter the pairs so that the first element is smaller or equal than the second one.

{% codeblock Colour pairs lang:haskell %}
let colours =["black", "white", "blue", "yellow", "red"]
[(x, y) | x <- colours, y <- colours, x <= y]
{% endcodeblock %}

produces (reindented for readability):
```
[("black","black"),("black","white"),("black","blue"),
("black","yellow"),("black","red"),("white","white"),
("white","yellow"),("blue","white"),("blue","blue"),
("blue","yellow"),("blue","red"),("yellow","yellow"),
("red","white"),("red","yellow"),("red","red")]
```

There are exactly 15 pairs, as expected.

### Multiplication table

Nothing simpler:

{% codeblock Multiplication table lang:haskell %}
[(x, y, x*y) | x <- [1..11], y <- [1..11]]
{% endcodeblock %}

produces (reindented for readability):
```
[(1,1,1),(1,2,2),(1,3,3),(1,4,4),(1,5,5),(1,6,6),(1,7,7),(1,8,8),(1,9,9),(1,10,10),(1,11,11),
(2,1,2),(2,2,4),(2,3,6),(2,4,8),(2,5,10),(2,6,12),(2,7,14),(2,8,16),(2,9,18),(2,10,20),(2,11,22),
(3,1,3),(3,2,6),(3,3,9),(3,4,12),(3,5,15),(3,6,18),(3,7,21),(3,8,24),(3,9,27),(3,10,30),(3,11,33),
(4,1,4),(4,2,8),(4,3,12),(4,4,16),(4,5,20),(4,6,24),(4,7,28),(4,8,32),(4,9,36),(4,10,40),(4,11,44),
(5,1,5),(5,2,10),(5,3,15),(5,4,20),(5,5,25),(5,6,30),(5,7,35),(5,8,40),(5,9,45),(5,10,50),(5,11,55),
(6,1,6),(6,2,12),(6,3,18),(6,4,24),(6,5,30),(6,6,36),(6,7,42),(6,8,48),(6,9,54),(6,10,60),(6,11,66),
(7,1,7),(7,2,14),(7,3,21),(7,4,28),(7,5,35),(7,6,42),(7,7,49),(7,8,56),(7,9,63),(7,10,70),(7,11,77),
(8,1,8),(8,2,16),(8,3,24),(8,4,32),(8,5,40),(8,6,48),(8,7,56),(8,8,64),(8,9,72),(8,10,80),(8,11,88),
(9,1,9),(9,2,18),(9,3,27),(9,4,36),(9,5,45),(9,6,54),(9,7,63),(9,8,72),(9,9,81),(9,10,90),(9,11,99),
(10,1,10),(10,2,20),(10,3,30),(10,4,40),(10,5,50),(10,6,60),(10,7,70),(10,8,80),(10,9,90),(10,10,100),(10,11,110),
(11,1,11),(11,2,22),(11,3,33),(11,4,44),(11,5,55),(11,6,66),(11,7,77),(11,8,88),(11,9,99),(11,10,110),(11,11,121)]
```

### Map-colouring problem

For this one, the code can be a bit simpler than Prolog's. The reason is that Prolog does not really have a different or not-equal operator: it has to use positive facts and cannot infer the relation is commutative. In Haskell, the list comprehension can have guards that are arbitrary boolean expressions, so we need to state only once that two states have different colours.

{% codeblock Map-colouring problem lang:haskell %}
let colours = ["red", "green", "blue"]
[(alabama, mississippi, georgia, tennessee, florida) | 
  alabama <- colours, 
  mississippi <- colours, 
  georgia <- colours, 
  tennessee <- colours, 
  florida <- colours, 
  mississippi /= tennessee, 
  mississippi /= alabama, 
  alabama /= tennessee, 
  alabama /= georgia, 
  alabama /= florida, 
  georgia /= florida, 
  georgia /= tennessee]
{% endcodeblock %}

produces (reindented for readability):
```
[("red","green","green","blue","blue"),
("red","blue","blue","green","green"),
("green","red","red","blue","blue"),
("green","blue","blue","red","red"),
("blue","red","red","green","green"),
("blue","green","green","red","red")]
```

And that's all for Day 1.