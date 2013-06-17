---
layout: post
title: "Psychic Modeling"
date: 2012-02-10 12:26
comments: true
categories: [Books, Algorithms]
tags: [Algorithm Design Manual, algorithms]
series: "Algorithm Design Manual"
js: [math]
---
In the [Algorithm Design Manual](http://www.algorist.com/), Stephen
Skiena entertains, edifies and educates his readers with so called
"War Stories", that is, interesting implementation challenges from his
own experience.

The first War Story is
[Psychic Modeling](http://www8.cs.umu.se/kurser/TDBAfl/VT06/algorithms/BOOK/BOOK/NODE19.HTM),
an attempt to exploit "precognition" to improve the chances of winning
the lottery.

<!-- more -->

This war story is also the subject of one of the first implementation
projects. In chapter 1.  A few years ago, when I bought the book, I
had easily solved the previous exercises, but then I reached this
implementation project, and I got stuck. I could not even get a
high level sketch of what a solution would look like.

Certainly, if I was unable to solve an exercise of the first chapter
of this book, it was hopelessly beyond my reach...

Still, I had the ambition of one day resuming my reading, and I would
from time to time give this problem another attempt.

Recently, it feels like all the pieces finally fell into places, and
after a few hours of coding I had an (naive) implementation. Yet I
still have doubts, as the only reference I have to compare my solution
with, Skiena's own paper
([Randomized Algorithms for Identifying Minimal Lottery Ticket Sets](http://www.cs.sunysb.edu/~skiena/papers/lotto.doc)\),
apparently is worse (in terms of necessary tickets) than my solution...

Note on this paper: unfortunately it is in Word format, and I found
that some characters are not properly displayed on non MS Word text
processing tools (such as Open Office). So you might have to open it
with MS Word or MS Word Viewer.

### The problem

I will use the notation from the book rather than the paper. The
problem is defined as this:

 * a lottery ticket has $k$ numbers
 * a win requires $l$ numbers from the winning ticket
 * the psychic visualises $n$ numbers
 * of which $j$ are "guaranteed" to be on the winning ticket.

### Defining "sufficient coverage"

A first difference between the paper's approach and mine is that I'm
using the notion of coverage size rather than distance: I measure how
similar two subsets are by defining their cover as the size of their
intersection; in their paper the authors use a notion of distance defined as
the size of the difference of the two subsets (perhaps to help with
the design of heuristics in the backtracking version of their algorithm).

Now, clearly the two approaches are equivalent; it is less clear that
the formulas derived from either are indeed the same.

For a given $j$-subset, how many $j$-subsets have a coverage of at
least $l$ with the first one? The covered $j$-subsets must have at
least $l$ numbers (between $l$ and $j$, to be precise) in common with
the first one, and the rest taken from the $n-j$ other numbers. This gives

<div markdown="0">
\begin{align}
\sum_{l \le i \le j} \binom{j}{i} \binom{n-j}{j-i}
\end{align}
</div>

For a given $j$-subset, how many $j$-subsets are within $j-l$ distance
of the first one? We can choose at most $j-l$ numbers out of the $n-j$
rest; and complete with numbers from the first subset. This gives

<div markdown="0">
\begin{align}
\sum_{0 \le i \le j-l} \binom{n-j}{i} \binom{j}{j-i} = \sum_{0 \le i \le j-l} \binom{n-j}{i} \binom{j}{i}
\end{align}
</div>

It took me a while to confirm it, but the formulas are indeed the
same:

<div markdown="0">
\begin{align}
\sum_{0 \le i \le j-l} \binom{n-j}{i} \binom{j}{i} &amp; = \sum_{0 \le i \le j-l} \binom{n-j}{i} \binom{j}{j-i}\\\\
&amp; = \sum_{l-j \le i \le 0} \binom{n-j}{-i} \binom{j}{j+i}&amp;&amp;\text{changing the sign of \(i\)}\\\\
&amp; = \sum_{l \le j+i \le j} \binom{n-j}{-i} \binom{j}{j+i}\\\\
&amp; = \sum_{l \le i \le j} \binom{n-j}{j-i} \binom{j}{i}&amp;&amp;\text{replacing \(j+i\) by \(i\)}\\\\
\end{align}
</div>

### Size of a ticket

Note that I do not use the $k$ size of a ticket. In fact, in my
original design, I used it but ignored $j$; reading the paper I
realised that $j$ was indeed critical: one of the $j$-subsets will be
on the winning ticket, so they are the ones we need to cover. However,
I could not understand why the paper did not use the potentially
larger size of a ticket to cover more $j$-subsets.

Restated with a complete ticket, the coverage formula becomes

<div markdown="0">
\begin{align}
\sum_{l \le i \le j} \binom{k}{i} \binom{n-k}{j-i}
\end{align}
</div>

This apparent small change actually reduces the lower bound of the
necessary tickets significantly. For $n=15$, $k=6$, $j=5$, $l=4$, for
instance, will the paper offers as a lower bound $58$, the formula
above gives $22$.

So the question is: is it valid to use the possibly larger value $k$
when generating tickets? I could not think of any reason not too, and
if I'm right, this gives each ticket a much larger cover, and
therefore a lower number of necessary tickets.

## Implementation

For a first effort, I chose to code in Haskell, and favoured simplicity
over speed. The code is indeed both simple, and wasteful, but Moore's
Law says that computers have become about 1000 times faster since the
time the paper was written, so I have some margin.

To keep things simple, sets and subsets are just lists. 

### Support functions

Such functions ought to belong to a dedicated library (and perhaps
they do); I include them to keep the implementation mostly
self-contained.

{% codeblock Support functions lang:haskell %}
fact n = product [1..n]
combi n c = (fact n) `div` (fact c * fact (n-c))

remainingNumbers js = foldr union [] js
{% endcodeblock %}

`fact` is just the factorial; `combi` computes the binomial
coefficient, and `remainingNumbers` is just the union of all the
passed $j$-subsets.

{% codeblock Generating Combinations lang:haskell %}
genCombi 0 _ = [[]]
genCombi _ [] = []
genCombi k (l:ls) = [l:cs | cs <- genCombi (k-1) ls] ++
                    genCombi k ls
{% endcodeblock %}

`genCombi k s` generates the $k$-subsets of $s$.

### Lower Bound Estimate

These are simple implementations of the formula above.

{% codeblock Lower Bound Estimates lang:haskell %}
ticketCover n k j l = sum [ (combi k i) *
                            (combi (n-k) (j-i)) | i <- [l..j]]
lowerBound n k j l = (fromIntegral $ combi n j) /
                     (fromIntegral $ ticketCover n k j l)
{% endcodeblock %}

`ticketCover` just implements the coverage estimate I defined above
(the one that uses $k$); `lowerBound` computes the lower bound for a
single win.

### Coverage

As stated above, I define the cover between two subsets as the size of
their intersection, and define sufficient coverage as the cover being
larger than $l$.

{% codeblock Defining Coverage lang:haskell %}
cover l1 l2 = length $ intersect l1 l2

coveredP l t j = l <= cover t j
notCoveredP l t j = l > cover t j

notCovered l t js = filter (notCoveredP l t) js
notCoveredBatch l ts js = foldr (notCovered l) js ts

coverageScore l t js = length $ filter (coveredP l t) js
{% endcodeblock %}

`cover` implements the cover definition; `coveredP` and `notCoveredP`
are predicates that check for (or against) sufficient coverage.

`notCovered` and `notCoveredBatch` computes the subsets that are not covered by a single ticket or a set
of tickets, respectively; they are used to compute what is left to
cover after selecting a ticket, and to check solutions.

Finally `coverageScore` computes the size of of the covered subsets by
a ticket. This function is used to compare potential tickets and
select the one with the best (i.e. largest) coverage.

{% codeblock Checking the estimates lang:haskell %}
checkFormula n k j l =
  let candidates = genCombi j [1..n]
      ticket = [1..k]
      covered = filter (coveredP l ticket) candidates
  in length covered
{% endcodeblock %}

`checkFormula` computes the size of the coverage of a single ticket;
it can be used to confirm the value of `ticketCover` above (and as far
as I can tell from my checks, it does).

### Solution Loop

The solution loop takes the parameters and a ticket candidate
generating function; it then gets one ticket at a time, computes the
$j$-subsets not covered yet, and repeat until the remaining
$j$-subsets set becomes empty.

{% codeblock Solution Loop lang:haskell %}
solve n k j l gc =
  let jtuples = genCombi j [1..n]
  in loop jtuples
 where loop [] = return []
       loop js = do
         t <- gc n k j l js
         ts <- loop $ notCovered l t js
         return (t:ts)
{% endcodeblock %}

The `solve` function expects the candidate generation function to be a
monad; this is to make it possible to use random number generators.

### Naive Ticket Selection

I do not really know how to navigate subsets, so I won't try to
implement a backtracking solution as describe in the paper. Instead, I
have what is really the simplest greedy algorithm: when a new ticket
is needed, get the one that has the best coverage among all the
possible tickets:

{% codeblock Naive Ticket Selection lang:haskell %}
getCandidate n k j l js =
  let numbers = remainingNumbers js
      tickets = genCombi k numbers
      ticketsScore = map (\t -> (coverageScore l t js, t)) tickets
  in return $ snd $ maximumBy (comparing fst) ticketsScore
{% endcodeblock %}

So for each $j$-subsets set, generate all the $k$-subsets, and compare
their coverage.

Needless to say, this function does not return anything anytime soon
for even slightly large values of $n$.

### Randomised Ticket Selection

To improve the performance (well, to get a result in my lifetime), I
am using what I understand to be the same approach as in the paper:
generates $\beta$ tickets, compare their coverage of the remaining
subsets, and keep the best one.

The different with the paper, as mentioned before, is that my tickets
are $k$-subsets rather than $j$-subsets themselves.

I first need a function to generate a random combination. I'm using a
method derived from Knuth (no reference as I don't have Volume 4 just yet).

{% codeblock Sample Generation lang:haskell %}
sample 0 _ = return []
sample _ [] = return []
sample k ds = do
  s <- sample (k-1) (tail ds)
  p <- randomRIO (0, (length ds - 1))
  let t = ds!!p
  if not (t `elem` s)
    then return (t:s)
    else return (head ds:s)
{% endcodeblock %}

The generating function is very similar to the naive one

{% codeblock Randomised Generating Function lang:haskell %}
getCandidateRandom beta n k j l js = do
  let numbers = remainingNumbers js
  tickets <- replicateM beta (sample k numbers)
  let ticketsScore = map (\t -> (coverageScore l t js, t)) tickets
  return $ snd $ maximumBy (comparing fst) ticketsScore
{% endcodeblock %}

The only difference is the `tickets` candidate set: the naive function
generates them all; the randomised one selects $\beta$ randomly.

### Compatibility with the paper version

By using `solve n j j l` instead of `solve n k j l`, my implementation
should compute subset coverage the same way the paper's implementation
does.

### Testing and Results

I will not compare speed, as this would be meaningless. But I can check
whether different values for ticket size can indeed help reduce the size of
the covering set.

Let's start with a very simple problem, where $n=5$, $k=3$, $j=3$ and
$l=2$.

I don't really need to generate the $j$-subsets, but if I do I can
check the solution.

The solution itself is computed by passing a ticket generating
function; I could have used `getCandidate`, but here I'm passing
`getCandidateRandom` with a $\beta=100$.

The `notCovered` set is empty, so the solution is at least a covering one.

The solution has two tickets, and the lower bound confirms it is
pretty good.

```
*Main> let problem = genCombi 3 [1..5]
*Main> solution <- solve 5 3 3 2 (getCandidateRandom 100)
*Main> solution
[[3,4,5],[1,2,4]]
*Main> notCoveredBatch 2 solution problem 
[]
*Main> lowerBound 5 3 3 2
1.4285714285714286
```

Next test, with $n=15$, $k=5$, $j=5$ and $l=4$. The paper reports that
they found a solution with $137$ tickets. As $k=j$, my algorithm
cannot really beat that (and indeed finds a solution of the same size,
if I try a couple of times):

```
*Main> let problem = genCombi 5 [1..15]
*Main> solution <- solve 15 5 5 4 (getCandidateRandom 100)
*Main> length solution
137
*Main> notCoveredBatch  4 solution problem
[]
*Main> lowerBound 15 5 5 4
58.88235294117647
```

For the next test, I should have a better solution than the paper, as
$k$ is larger than $j$: $n=15$, $k=6$, $j=5$, $l=4$.

The paper has a lower bound of $58$, and a solution of size $138$, but my
lower bound is $22$, and my solution has size $57$.

```
*Main> let problem = genCombi 5 [1..15]
*Main> solution <- solve 15 6 5 4 (getCandidateRandom  100)
*Main> length solution
57
*Main> notCoveredBatch 4 solution problem
[]
*Main> lowerBound 15 6 5 4
21.29787234042553
```

When the difference between $k$ and $j$ becomes large, the solution
improves significantly: with $n=18$, $k=10$, $j=7$, $l=6$, the paper
has a lower bound of $408$, mine is $18$. The paper's solution has
size $1080$, but mine is just $73$.

```
*Main> let problem = genCombi 7 [1..18]
*Main> solution <- solve 18 10 7 6 (getCandidateRandom 100)
*Main> length solution
73
*Main> notCoveredBatch 6 solution problem
[]
*Main> lowerBound 18 10 7 6
17.68
```

### Wrapping up

Even if my approach is ultimately wrong, I can say I must be close to
an actual solution. I could (and probably will, given time) try to
rewrite my solution in C, and focus on performance.

So I declare this problem conquered, I will resume my reading.

### Complete code

{% include_code Psychic Modeling Implementation algo-design-manual/psychic.hs %}
