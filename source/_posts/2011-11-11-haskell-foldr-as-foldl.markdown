---
layout: post
title: "Haskell: foldr as foldl"
date: 2011-11-11 08:03
comments: true
categories: [Haskell, Tutorial]
tags: [haskell]
---
In a previous [post](blog/2011/11/09/haskell-foldl-as-foldr/), I tried to show how to derive the formula expression `foldl` in terms of `foldr`. Unsurprisingly, there is a way to express `foldr` in terms `foldl`: `foldr f a bs = foldl (\g b x -> g (f b x)) id bs a`. Today I'll try to derive this definition.
<!--more-->

Of course, `foldl` is strict, so it cannot work on infinite lists. Therefore, the new `foldr` would be similarly limited.

I'll start again from a standard definition for both functions. First `foldr`:

{% codeblock foldr lang:haskell %}
foldr :: (b -> a -> a) -> a -> [b] -> a
foldr f a [] = a
foldr f a (b:bs) = f b (foldr f a bs)
{% endcodeblock %}

Then `foldl`:

{% codeblock foldl lang:haskell %}
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f a [] = a
foldl f a (b:bs) = foldl f (f a b) bs
{% endcodeblock %}

Derivation
----------

Once again, I will use the function building fold rather than value building one. This time, the `f` argument to `foldr` has already the right type, so I will not need `flip`.

I first reorganize the arguments order a bit, putting `a` at the end:

{% codeblock foldr_alt basic equation lang:haskell %}
foldr f a bs = foldr_alt f bs a
{% endcodeblock %}

The base case is once again `id`:
{% codeblock foldr_alt on empty list lang:haskell %}
foldr_alt f [] = id
{% endcodeblock %}

and is trivially true:
```
foldr f a [] == a                  -- by definition of foldr
             == id a               -- by definition of id
             == (foldr_alt f []) a -- by definition of foldr_alt
```

The recursive case is not surprising either:
{% codeblock foldr_alt on non empty list lang:haskell %}
foldr_alt f (b:bs) = (f b) . (foldr_alt f bs)
{% endcodeblock %}

It follows from the induction hypothesis: given `foldr f a bs == foldr_alt f bs a`, then `foldr f a (b:bs) == foldr_alt f (b:bs) a`

```
foldr f a (b:bs) == f b (foldr f a bs)           -- by definition of foldr
                 == f b (foldr_alt f bs a)       -- by induction hypothesis
                 == (f b) . (foldr_alt f bs) $ a -- currying and definition of . and $
                 == (foldr_alt f (b:bs)) $ a     -- by definition of foldr_alt
                 == foldr_alt f (b:bs) a         -- uncurrying and definition of $
```

But `foldl` is nowhere to be seen... which is not surprising considering that `foldl` is tail recursive while `foldr` and `foldr_alt` are both body recursive... maybe using an accumulator, we could turn `foldr_alt` to a tail recursive function.

The initial value for the accumulator must be the identity of the composition function, that is, `id`.

{% codeblock accumulator version of foldr_alt lang:haskell %}
foldr_alt f bs = foldr_acc f id bs
{% endcodeblock %}

`foldr_acc` is once again easy to define. The base case:

{% codeblock foldr_acc base case lang:haskell %}
foldr_acc f acc [] = acc
{% endcodeblock %}

For the recursive case, notice that `f b` is composed with the _rest_ of the function to the right. As the accumulator represents the previous part of the function, `f b` will be composed with this accumulator to the left:

{% codeblock foldr_acc recursive case lang:haskell %}
foldr_acc f acc (b:bs) = foldr_acc (acc . (f b)) bs
{% endcodeblock %}

The proof is less straightforward; I am not very familiar with equational reasoning, so maybe something simpler is possible. Note that in this proof, I need the list argument to be finite.

First, the base case:
```
foldr_alt f [] == id                 -- by definition of foldr_alt
               == foldr_acc f id []  -- by definition of foldr_acc
```

For the recursive case, I will not actually use an induction hypothesis. Instead, I will use the fact that the list is finite, and the fact that the composition function is, well, a function. `f == g` implies `acc . f == acc . g` for any terminating `acc`.

```
acc . foldr_alt f (b1:b2:bs) == acc . (f b1) . (foldr_alt f (b2:bs))  
	-- by definition of foldr_alt
                         == acc . (f b1) . (f b2) . (foldr_alt f bs)  
	-- by definition of foldr_alt again
                         == acc . (f b1) . (f b2) . .. (f bn)         
	-- by induction over the list bs, which must be finite
                         == foldr_acc f (acc .(f b1) . (f b2) . ... ) []
	-- by definition of foldr_acc f _ []
                         == foldr_acc f (acc . (f b1) . (f b2) . ..) [bn]  
	-- by definition of foldr_acc, recursive case
                         == foldr_acc f acc (b1:b2:bs)
	-- by induction over the lenght of (b1:b2:bs), and definition of foldr_acc, recursive case
```

The fact that the equation only holds for list of finite lengths should not be surprising, but might still be a limitation of my proof.

With `foldr_acc` defined as above, converting to `foldl` is immediate:
{% codeblock foldr as foldl lang:haskell %}
foldr_acc f id bs = foldl go id bs
  where go acc b = \x -> acc (f b x)
{% endcodeblock %}

If I rename `acc` to `g`, and move `x` to the parameter list (uncurrying the `go` function), I get the original definition. 

QED.