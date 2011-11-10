---
layout: post
title: "Haskell: foldl as foldr"
date: 2011-11-09 19:05
comments: true
categories: [Haskell, Tutorial]
tags: [haskell]
---
A beginner with Haskell is bound to be exposed, usually too early, to some fancy code illustrating either a cool feature of the language, or of its underlying theoretical foundations.

Among others, there is the one-liner lazy Fibonacci definition `fibs = 1:1:zipWith (+) fibs (tail fibs)`. Another one, the topic of this post, is `foldl f a bs = foldr (\b g x -> g (f x b)) id bs a`.
<!--more-->
Unfortunately for the beginner, the explanation he or she could find on, say, the [Haskell Wiki](http://www.haskell.org/haskellwiki/Foldl_as_foldr), shares the problem of many intermediate Haskell tutorials: they explain moderately complex concepts in terms of more arcane ones.

So here I will attempt to provide a justification of the equation above using fairly simple tools (higher order functions being the most difficult ones).

Building intuition
------------------

Before even getting to a definition of `foldl` in terms of `foldr`, there are a few questions we should ask:

 * is it even possible?
 * how are these two functions different
 * what should change to make them similar

So first, let's look at each function. The definitions below are very similar to the ones used in [GHC](http://www.haskell.org/ghc/) (as you can see [here](http://www.haskell.org/ghc/docs/7.2.1/html/libraries/base-4.4.0.0/Data-List.html) when looking at the source of `foldl` and `foldr`).

First `foldl`:

{% codeblock foldl lang:haskell %}
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f a [] = a
foldl f a (b:bs) = foldl f (f a b) bs
{% endcodeblock %}

`foldl` is tail recursive: it uses the `a` parameter as an accumulator.

Then `foldr`:
{% codeblock foldr lang:haskell %}
foldr :: (b -> a -> a) -> a -> [b] -> a
foldr f a [] = a
foldr f a (b:bs) = f b (foldr f a bs)
{% endcodeblock %}

`foldr` is body recursive. This is why it is lazy and able to work on infinite list. If the function `f` is a constructor that is lazy in it's second argument, then it is possible to examine some of the output before evaluating the recursive call.

So far, the two functions appear different. Let's try and look at how they process their arguments.

I define a pair of data types to play the role of the function `f`. That way, I can look at the structure of calls. I need two because `foldl` and `foldr` have different types for `f`

{% codeblock Helper Data Types lang:haskell %}
data Fl b = (Fl b) :< b | El
  deriving (Show)

data Fr b = b :> (Fr b) | Er
  deriving (Show)
{% endcodeblock %}

`Fl` is a data type whose constructor `:<` can be used in `foldl`. `Fr` has constructor `:>` and can be used in `foldr`.

Using them:

```
*Main> foldl (:<) El [1..5]
((((El :< 1) :< 2) :< 3) :< 4) :< 5
*Main> foldr (:>) Er [1..5]
1 :> (2 :> (3 :> (4 :> (5 :> Er))))
```

It seems that, to translate from `foldl` to `foldr`, there are two problems:

 * the function `f` in `foldr` uses its parameters in reverse order from the `f` in `foldl`. Perhaps something that [`flip`](http://www.haskell.org/ghc/docs/latest/html/libraries/base-4.4.0.0/Prelude.html#v:flip) could help with?
 * the list is iterated over in the opposite order. A task for [`reverse`](http://www.haskell.org/ghc/docs/latest/html/libraries/base-4.4.0.0/Prelude.html#v:reverse)? 

Let's see:

```
*Main> foldr (flip (:<)) El [1..5]
((((El :< 5) :< 4) :< 3) :< 2) :< 1
*Main> foldr (flip (:<)) El $ reverse [1..5]
((((El :< 1) :< 2) :< 3) :< 4) :< 5
```

So yes, it is indeed possible to write `foldl` in terms of `foldr`: `foldl f a bs = foldr (flip f) a $ reverse bs`. This answer the first question, and along the way we have collected some hints on how to do it (I mean, beside using `flip` and `reverse` as above)

Planning the solution
---------------------

Let's look again at `foldl`:

```
*Main> foldl (flip (:>)) Er [1..5]
5 :> (4 :> (3 :> (2 :> (1 :> Er))))
```

This time I pass `flip (:>)` as the function argument. This is to make `:>` compatible with `foldl`. `:>` is really just like `flip (:<)`, but it gives a new perspective: `:>` has type `b -> a -> a`, which is the same as saying that `(:>) b` has type `a -> a` (I apply partially. `b :>` is the same as `(:>) b`). 

In point-free notation, the result above is identical to `(:>) 5 . (:>) 4 . (:>) 3 . (:>) 2 . (:>) 1 $ Er`: 
```
*Main> (:>) 5 . (:>) 4 . (:>) 3 . (:>) 2 . (:>) 1 $ Er
5 :> (4 :> (3 :> (2 :> (1 :> Er))))
```

(I'm always happy when ghci agrees with me).

This is great for two reasons:

 * while we cannot say anything about the original `f` function (here represented as `:>`), the function composition `.` operator is associative, meaning we can put the parenthesis where we (or `foldr`) wants. In other words, if we manipulate functions `(:>) b` instead of values, and combine them with composition, we don't have to care about the fact that `foldr` and `foldl` nest expressions differently. 
 * The `a` parameter, represented here by `Er`, is removed from the iteration. As `foldr` and `foldl` use this parameter differently, if we can abstract it away, this is another difference that disappear.

So the solution can be built on two concepts:

 * use `flip f` instead of `f` so that we can operate on functions `a -> a` instead of values `a`
 * use composition over these functions to combine them, then apply the result to the `a` parameter to get the answer.

Building the solution
---------------------

First, I introduce a new function, `foldl_alt`, that is supposed to implement `foldl`. The definition of `foldl_alt` is then rewritten until `foldr` appears:

{% codeblock foldl_alt basic equation lang:haskell %}
foldl f a bs = foldl_alt f bs a
{% endcodeblock %}

First, let's handle the base case:

{% codeblock foldl_alt on empty list lang:haskell %}
foldl_alt _ [] = id
{% endcodeblock %}

This is easily shown true:
```
foldl f a [] == a                  -- by definition of foldl
             == id a               -- by definition of id
             == (foldl_alt f []) a -- by definition of foldl_alt
```

The recursive case is simple as well (by recursion):
{% codeblock foldl_alt on non empty list lang:haskell %}
foldl_alt f (b:bs) = (foldl_alt f bs) . (f' b)
  where f' = flip f
{% endcodeblock %}

Assuming `foldl f a bs == foldl_alt f bs a`, let's show by induction that `foldl f a (b:bs) == foldl_alt f (b:bs) a`:

```
foldl f a (b:bs) == foldl f (f a b) bs            -- by definition of foldl
                 == foldl_alt f bs (f a b)        -- induction hypothesis
                 == foldl_alt f bs (f' b a)       -- simple replacement
                      where f' = flip f
                 == (foldl_alt f bs) . (f' b) $ a -- point-free notation
                      where f' = flip f
```

So `foldl_alt` as currently defined is identical to `foldl`. The recursive case can be further rewritten as:

{% codeblock foldl_alt on non empty list, second version lang:haskell %}
foldl_alt f (b:bs) = comp' (f' b) (foldl_alt f bs)
  where f'    = flip f
        comp' = flip (.)
{% endcodeblock %}

Here the composition operator is replaced by `comp'`, which just takes its argument in reversed order. This is done to show the similarity of this current `foldl_alt` with the recursive case of `foldr`. Indeed, `foldl_alt f` is identical to `foldr m id` for some function `m` (for Mystery):

```
foldl_alt f [] == id == foldr m id []                            
	-- by definition of both foldl_alt and foldr
```

Now I can use induction to show that `foldl_alt f bs == foldr m id bs` implies `foldl_alt f (b:bs) == foldr m id (b:bs)`, and compute `m` at the same time:

```
foldl_alt f (b:bs) == comp' (f' b) (foldl_alt f bs)              
                        where f' = flip f
                              comp' = flip (.)
	-- by definition of foldl_alt
                   == comp' (\a -> f a b) (foldl_alt f bs)        
                        where comp' = flip (.)
	-- expand f'
                   == (\g a -> g (f a b)) (foldl_alt f bs)       
	-- expand comp' - g has type a -> a, and is bound to (foldl_alt f bs) 
                   == (\b' g a -> g (f a b')) b (foldl_alt f bs) 
	-- take b out of the function; replace it with b' that is bound to b
	               == (\b' g a -> g (f a b')) b (foldr (\b' g a -> g (f a b')) id bs)
	-- induction hypothesis
                   == foldr (\b' g a -> g (f a b')) (b:bs) id    
	-- definition of foldr, recursive case
```

When using the induction hypothesis, I replaced `m` with `(\b' g a -> g (f a b'))`. This is because this function is independent of any specific `b`: by construction it would be the same at every step of the induction (except the base case, where it is not use, and therefore can be anything we need it to be).

`b'` as a parameter to the function is bound to `b` as the value in the list. I use different names to make it clear they're different, but of course inside the function I could use `b` as the variable scope is limited to the function.

For the same reason, I replace the `a` above by `x`, as I need `a` to represent the original `a` parameter (once again I could use `a` for both). This gives:

{% codeblock foldl as foldr lang:haskell %}
foldl f a bs = foldr (\b g x -> g (f x b)) id bs a
{% endcodeblock %}

Ok, that was long. I hope it was clear (or clearer).

Next time I'll show how to implement `foldr` as `foldl` (minus the laziness).