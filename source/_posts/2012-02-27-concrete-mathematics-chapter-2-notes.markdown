---
layout: post
title: "Concrete Mathematics Chapter 2 Notes"
date: 2012-02-27 10:54
comments: true
categories: [Books, Mathematics]
tags: [Concrete Mathematics, math]
series: "Concrete Mathematics"
js: [math]
---
After a long but busy silence, I have now a few notes on the second
chapter, Sums. As with
[Chapter 1](/blog/2012/01/06/concrete-mathematics-chapter-1-notes/),
these are nothing revolutionary; just some clarifications of the
points that were not obvious to me, as well as other, random
observations.

<!--more-->

Overall, this chapter felt less overwhelming than the first, despite
being much longer and introducing very powerful techniques. I have yet
to do the exercises, though, so I may still revise this judgement.

### Notation

The authors mentions that the Sigma-notation is "... impressive to
family and friends". I can confirm that assessment.

The remark on keeping bounds simple actually goes beyond resisting
"premature optimisation", that is, removing terms just because they
are equal to zero. Sometimes, it is worth adding a zero term if it
simplifies the bounds. Such a trick is used in solving
$\sum_{1\le j\lt k\le n} \frac{1}{k-j}$, and I'll get back to this
point when I go over this solution.

The Iverson notation (or Iversonian) is a very useful tool, as is the
general Sigma-notation. About the latter, it already simplifies
variable changes a lot, but I found it useful (and less error prone)
to always write the variable change on the right margin (for instance
as $k \leftarrow k+1$) and to keep that change as the only one in a
given line of the rewrite; otherwise, no matter how trivial the
change, any error I make at that time will be hard to locate (I know;
I tried).

### Sums and Recurrence

First we see how easy it is to use the repertoire method to build
solutions to common (or slightly generalised) sums. The only problem
with the repertoire method is it requires a well furnished repertoire
of solutions to basic recurrences; I'm sure I would never have come up
with the radix-change solution to the generalised Josephus
problem. And given that there is an infinite number of functions one
could try, a more directed method is sometimes necessary.

This section also shows how to turn some recurrence equations (such as
the Tower of Hanoi one) into a sum; this method involve a choice
($s_1$ can be any non-zero value), which could either simplify or
complicate the solution. I haven't done the exercises yet, so I don't
know to what extent the choice is obvious or tricky.

Finally it shows how to turn a recurrence expressed as a sum of all
the previous values into a simpler recurrence by computing the
difference between two successive values. This is one instance of a
more general simplification using a linear combination of a few
successive values.

### Manipulation of Sums

Unsurprisingly, sums have the same basic properties as common
additions: distributive, associative and commutative laws. Only the
latter is really tricky, as it involves a change to the index
variable. As mentioned above, I found useful to make such changes
really clear and isolated in any reasoning.

With these laws confirmed, it is possible to build the first method
for solving sums: the perturbation method. It is very simple, and
while it does not always work, when it does it is very quick.

### Multiple Sums

This is perhaps the first section where I had to slow down; basically
multiple sums are not different from simple sums, and manipulations
are defined by the distributive law, but index variable changes
(especially the rocky road variety) require special attention. This,
combined with "obvious" simplifications (obvious to the authors, and
sometimes in retrospect to the reader as well), gave me some
difficulties.

For instance, the solution to

<div markdown="0">
\begin{align}
\sum_{1\le j\lt k\le n} \frac{1}{k-j}
\end{align}
</div>

The index variable change $k \leftarrow k+j$ is explained as a
specific instance of the simplification of $k+f(j)$; more perplexing
are the ranges for $j$ and $k$ when the sum is replaced by a sum of sum:

<div markdown="0">
\begin{align}
\sum_{1\le k\le n} \sum_{1\le j \le n-k} \frac{1}{k}
\end{align}
</div>

The range for $j$ is built from $1\le j$ and $k+j\le n$, so there is
nothing really strange here.

The range for $k$, however, looks like a typo: certainly the authors
meant $1\le k\lt n$. A margin graffiti confirms the range, but it does
not really explain it.

The fact is, it is safe to let $k\le n$ here, because the sum over $j$
when $k=n$ is zero: not only the expression
$\sum_{1\le j \le k-n = 0} \frac{1}{k}$ is zero because there is no
$j$ that can satisfies the range predicate, but the closed form
of this sum, $\frac{k-n}{k}$, is also zero when $k=n$.

With the closed form checked, it is safe to add extra terms to
simplify the range of $k$.

What happens if you don't see this possible simplification? As
expected, the answer remains the same:

<div markdown="0">
\begin{align}
\sum_{1\le k\lt n} \sum_{1\le j \le n-k} \frac{1}{k} &amp; = \sum_{1\le k\lt n} \frac{n-k}{k}\\\\
&amp; = \sum_{1\le k\lt n} \frac{n}{k} - \sum_{1\le k\lt n} \frac{k}{k}\\\\
&amp; = \sum_{1\le k\lt n} \frac{n}{k} - (n-1)\\\\
&amp; = \sum_{1\le k\lt n} \frac{n}{k} + \frac{n}{n} - n\\\\
&amp; = \sum_{1\le k\le n} \frac{n}{k} - n\\\\
&amp; = nH_n - n\\\\
\end{align}
</div>

So to expend on the original advice of keeping the bounds as simple as
possible: sometimes it is possible to extend the bounds (in order to
simplify them), as long as the extra terms in closed form evaluate to
zero. If the extra terms are still defined as sums, just checking that
the range is empty might not be enough. 

### General Methods

A cool and fun section on the various ways to solve a given sum.

Method 0 is to look it up. This book, written before the rise of
Internet (I remember Internet in the early 1990's; most of it was still
indexed manually on the CERN index pages...), suggests a few books as
resources.

Fortunately, some of them have migrated to the
[Web](https://oeis.org/), which is a more suitable tool than books for
such knowledge; the combination of searches and instant updates is
hard to beat (a book remains best for a content that is mostly linear
and somewhat independent of time; a novel, or textbook, for
instance. References are better on Internet, free if possible, for
a subscription otherwise).

Method 1 is guessing then proving; proving in fact should be a
complement for all the other methods (except perhaps Method 0). Having
two independents proofs is always good.

Method 2 is the perturbation method. In this section example, we see
how an apparent failure can still be exploited by being imaginative.

Method 3 is the repertoire method. In this chapter it is usually much
simpler than in the first.

Method 4 uses calculus to get a first approximation, then uses other
methods to solve the equations for the error function.

Method 5 is a clever rewriting of the problem into a sum of sums;
like the repertoire method but unlike the others, it requires some
intuition to find a solution (perhaps more than the repertoire
method); I have bad memories of trying such a method to solve problems
at university, always somehow ending up right where I started. I guess
I will try other methods if I can.

Method 6 is the topic of the next section; method 7 is for another
chapter.

### Finite and Infinite Calculus

This section was surprising and exciting, but not really that
complex. It really is a matter of adapting regular calculus reflexes to the
finite version. I have to see how it works in practice.

One thing that is causing me some trouble is the falling-power version
of the law of exponents:

<div markdown="0">
\begin{align}
x^{\underline{m+n}} &amp; = x^{\underline m}(x-m)^{\underline n}\\\\
\end{align}
</div>

While the rule is easy to prove and to remember, it is less easy than
the general one to recognise in practice; I failed to see it when it
came up in the solution to

<div markdown="0">
\begin{align}
\Sigma xH_x\delta x\\\\
\end{align}
</div>

Worse, even the explanation in the book, I had to write it down, play
with it, before seeing it.

So I'm thinking about a notation that would bring out the rule more
clearly, an extension of the _shift operator_ $E$:

<div markdown="0">
\begin{align}
E_k f(x) &amp; = f(x-k)\\\\
\end{align}
</div>

This would turn the exponent law into

<div markdown="0">
\begin{align}
x^{\underline{m+n}} &amp; = x^{\underline m} E_m x^{\underline n}\\\\
\end{align}
</div>

Whether this is useful, or whether I'll get used to the original
notation anyway, we'll see in the exercises...

### Infinite Sums

The last section is about infinite sums. The authors quite sensibly
restrict the scope to absolutely convergent sums, which have the
advantage that the three basic laws and the manipulations they allow
are still valid.

Once again, this was not overly difficult; the only point I had
trouble understanding was the existence of the subsets $F_j$ such that
$\sum_{k\in F_j} a_{j,k} \gt (A/A')A_j$ when
$\sum_{j\in G} A_j = A' \gt A$. But this last equation means that
$A/A' \lt 1$, so $(A/A')A_j \lt A_j$. The first equation is therefore
just a consequence of the fact that $A_j$ is a least upper bound.

Next post, the warmups.
