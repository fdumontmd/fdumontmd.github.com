---
layout: post
title: "Seven Languages in Seven Weeks Haskell Day 2"
date: 2011-11-18 09:39
comments: true
categories: [Books]
tags: [7languages7weeks, haskell]
series: "Seven Languages in Seven Weeks"
---
Today introduces the functional aspects of Haskell: higher order functions, partial application of functions and lazy evaluation.
<!--more-->
Higher order functions should no longer be surprising: many languages have that, even if Haskell other features make them very easy to use.

Partial application is such a feature. A function can be passed some, but not all its arguments, meaning that it is still a function, not a value. It reduces the number of anonymous functions one needs to write when using higher order functions.

Lazy evaluation is something very unique (among the lazy languages, only Haskell is somewhat mainstream). Clojure has lazy lists, which is cool, but lazy evaluation applies to everything. A piece of data can refer to itself in its definition, as long as the part that is needed can be evaluated before the part it depends on.

For instance, a canonical definition of the Fibonacci sequence is
{% codeblock Fibonacci sequence lang:haskell %}
fibs = 1:1:zipWith (+) fibs (tail fibs)
{% endcodeblock %}

The `fibs` list is the list of Fibonacci numbers. It starts with 1, 1, then the list of itself summed with its own tail... but the 3rd number depends on the first and the second, so its ok. By the time we need to compute the 4th, the 3rd is already known, and so on.

It takes but an instant to compute the 100000th number in the sequence:
```
*Parse> fibs !! 100000
42026927029951543863190051012939151317739157026322345033047..... -- number truncated to save space
```

Which brings me to a remark on the book: why on earth is `fibNth` defined the way it is? That function exists, and is called [`!!`](TODO). The code in the book is convoluted, does not need that many parenthesis, and even if you have a problem with `!!`, there is no need to use both `take` and `drop` if you're going to take the `head` of the result (`take` will make a copy of the list for no good reason).

Exercises
---------

In general I tried to avoid standard functions that implement a significant portion of the intended behaviour. So I didn't use `sort` in my sort function, or `read` in parsing, ...

### Simple sort

A good sort algorithm is always tricky, but insertion sort is simple enough and easy to express with pattern matching. My implementation has the same signature as the standard [`sort`](TODO) function. It expects is arguments to have the class [`Ord`](TODO)s, which guarantees they can be compared.

{% include_code lang:haskell 7l7w/haskell/mysort.hs %}

Testing it:

```
*MySort> my_sort ([1..10] ++ [10, 9.. 1])
[1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10]
```

### Sort using comparison function

Sort using a specific comparison function is not harder. The standard implementation uses [`Data.Ord.Ordering`](TODO) to replace `>` by the comparison result `GT`. My implementation has the same signature as the standard [`sortBy`](TODO), but still uses the insertion sort as in `my_sort`.

{% include_code lang:haskell 7l7w/haskell/mysortby.hs %}

Testing it (using [`compare`](TODO) on the absolute value):
```
*MySort> my_sort_by (\a b -> compare (abs a) (abs b)) ([10, 9..1] ++ [-10..(-1)])
[1,-1,2,-2,3,-3,4,-4,5,-5,6,-6,7,-7,8,-8,9,-9,10,-10]
```

### Parse string into number

Parsing is not hard; to do it I break the string into a integral part, and the fractional part. Both are then cleaned to remove non digits. 

The integral part is parsed left to right (with `foldl`), each time multiplying the already parsed number by 10 before adding the current number.

The fractional part is parsed right to left (with `foldr`), dividing the already parsed number by 10 before adding the current number.

Note the use of [`fromIntegral`](TODO) function. This is used to convert and integral number (`Int`, `Integer`, ...) into any type of number. This is necessary to be allowed to divide the results and add the fractional part.

The use of fractional arithmetic makes this function less effective than `read`.

{% include_code lang:haskell 7l7w/haskell/parse.hs %}

Testing:
```
*Parse> parse "$2,345,678.99"
2345678.99
*Parse> parse "2,345"
2345.0
*Parse> parse ".99"
0.99
*Parse> parse ".234"
0.23399999999999999
```

### Lazy sequences

Once again, nothing difficult. Haskell notation pretty much reads as a specification of the problem:

{% include_code lang:haskell 7l7w/haskell/lazy.hs %}

Testing:
```
*Lazy> take 10 $ thirds 10
[10,13,16,19,22,25,28,31,34,37]
*Lazy> take 10 $ fifths 20
[20,25,30,35,40,45,50,55,60,65]
*Lazy> take 10 $ eighths 10 20
[30,38,46,54,62,70,78,86,94,102]
```

### Partial application

Notice the use of partial application of operators: if you wrap the operator and its argument in parenthesis (they are needed here), you have a function that takes the missing argument. The missing argument can be the left one as see here.

{% include_code lang:haskell 7l7w/haskell/partial.hs %}

Testing:

```
*Partial> half 10
5.0
*Partial> terminate "Hello"
"Hello\n"
```

Challenges
----------

### Greatest Common Denominator

I must have missed something, because that was hardly a challenge. I just implemented the [Euclidean algorithm](TODO):

{% include_code lang:haskell 7l7w/haskell/gcd.hs %}

Testing:
```
*GCD> my_gcd 1961 901
53
*GCD> my_gcd 901 1961 
53
*GCD> gcd 1961 901
53
```

`my_gcd` agrees with the standard [`gcd`](TODO) function.

### Lazy prime number sequences

This one was a bit trickier, yet an implementation that closely follows the [Erastothene Sieve](TODO) algorithm is fairly short.

I first need a difference function that works on infinite lists: I manage this by taking into account the fact that the lists are always sorted. The `minus` just compares the first item of its arguments, so it can work linearly on both of them. Note that this function is not able to work on finite lists, but in this context there is no need to.

The implementation follows the proposed optimizations: it puts 2 in the prime number list right from the start, and skips other even numbers. It also start filtering at `p*p`, as smaller multiples have been filtered already (being a multiple of smaller prime numbers).

{% include_code lang:haskell 7l7w/haskell/sieve.hs %}

The implementation is very slow, but can compute the first 1000 prime numbers.

```
*Sieve> primes !! 1000
7927
```

This turns out to be the first implementation on the Prime Number generator [page](TODO) on the Haskell wiki. Other implementations are much smarter and faster.

### Breaking string into lines

The exercise description seems to be missing something: a line length. So I have added that to the functions.

Breaking into words is best done with [`words`](TODO), but I implemented my version. I actually started with a first abstraction, not really necessary here, that splits a sequence based on a predicate (items that return true for the predicate are all removed). Then `my_words` is just calling that function with [`isSpace`](TODO) as the predicate.

To combine words back into lines, I used two small functions: one (`accumUntil`) builds a line one word at a time, and stops when the line is too long. It starts with a word as the first tentative line, to make sure that a line is not empty even if a word is too long to fit.

The other function (`loop`) uses the previous one to build a list of lines until the list of words is empty.

{% include_code lang:haskell 7l7w/haskell/split.hs %}

Testing:

```

```

### Justify text



{% include_code lang:haskell 7l7w/haskell/justify.hs %}

### Number lines

I finished with this one, as I reused some functions defined in the module `Justify` above.

{% include_code lang:haskell 7l7w/haskell/number.hs %}
