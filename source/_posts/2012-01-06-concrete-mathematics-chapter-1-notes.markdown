---
layout: post
title: "Concrete Mathematics Chapter 1 Notes"
date: 2012-01-06 13:52
comments: true
categories: [Books, Mathematics]
tags: [Concrete Mathematics, math]
series: "Concrete Mathematics"
js: [math]
---
For the first post of this hopefully long series, I have a few notes I
wrote down as I was reading Chapter 1. Nothing revolutionary, but it
gives me a chance to play with math notation.

<!--more-->

# Lines in the Plane

I must admit that my memories of Geometry are far, far away (the
subject was not addressed at all when I studied Mathematics at
university, and I had no need for Geometry in my work since), so I
spent perhaps an unreasonable amount of time to check the validity of
the most elementary steps.

It goes without saying that the exercises of a Geometric nature are
particularly challenging (as if I needed the extra difficulty).

## Intersecting lines

The notion that one line will add $k$ new regions if it intersects
other lines at $k-1$ points is due to the fact that $k-1$ distinct
lines define at least $k$ regions (more if they are not all parallel),
and one more line that intersects them all will divide these $k$
regions in two.

# Josephus Problem

## $J(5 \cdot 2^m) = 2^{m+1} + 1$

This is based on the fact that $J(10) = 5$ and $J(2n) = 2J(n) -1$.

By induction:

*Base case*: it is true for $m = 1$: $J(5\cdot 2) = J(10) = 5 =
2^{1+1} + 1$

*Recurrence*: assuming it is true for $m$,

<div markdown="0">
$$
\begin{aligned}
J(5\cdot 2^{m+1}) &amp;= J(2(5\cdot 2^m))\\\\
&amp;= 2J(5\cdot 2^m) - 1&amp;&amp;\text{as } J(2n) = 2J(n) -1\\\\
&amp;= 2(2^{m+1}+1) - 1&amp;&amp;\text{induction hypothesis}\\\\
&amp;= 2\cdot 2^{m+1} + 2 - 1\\\\
&amp;= 2^{m+2} + 1
\end{aligned}
$$
</div>

## $A(2^{m}+l) = 2^{m}$

It took me a while to convince myself that the $l$ was not a problem
here. This can be seen by considering $l$ in binary notation, and
using $A(2n) = 2A(n)$ and $A(2n+1) = 2A(n)$ to remove the rightmost
bit.

That is, with $2^m > l = (b_{m-1}b_{m-2}\cdots b_{1}b_{0})_2$,we have:

<div markdown="0">
$$
\begin{aligned}
A(2^{m}+l) &amp;= A(2^{m}+(b_{m-1}b_{m-2}\cdots b_{1}b_{0})_2)\\\\
&amp;= 2A(2^{m-1}+(b_{m-1}b_{m-2}\cdots b_{1})_2)\\\\
&amp;= 2^{2}A(2^{m-2}+(b_{m-1}b_{m-2}\cdots b_{2})_2)\\\\
&amp;= 2^{3}A(2^{m-3}+(b_{m-1}b_{m-2}\cdots b_{3})_2)\\\\
&amp;= \cdots
\end{aligned}
$$
</div>

At each iteration, whether $b_i$ is $0$ or $1$, we can ignore it when
dividing by $2$. And as $2^m < l$, it takes no more than $m$ steps
(removing the $m$ bits $b_0$ to $b_{m-1}$) to reduce $A(2^m+l)$ to
$2^mA(1) = 2^m$

## Radix-based Generalised Josephus Solution

The equation 1.18:

$$f \left( ( b_m b_{m-1} \cdots b_1 b_0)_d \right) = \left( \alpha_{b_m} \beta_{b_{m-1}} \beta_{b_{m-2}} \cdots \beta_{b_1} \beta_{b_0} \right)_c$$

is so unnaturally smart and simple that I thought the proof must be
missing. But in fact it is indeed trivial, and just as the book
states, follows from the rewriting of the argument in base $d$, then
recurrence over $m$ (with $m$ the number of digits or the argument in
base $d$).

In the next post in this series, I will start the exercises.
