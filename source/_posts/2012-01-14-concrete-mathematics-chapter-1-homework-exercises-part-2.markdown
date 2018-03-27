---
layout: post
title: "Concrete Mathematics Chapter 1 Homework Exercises Part 2"
date: 2012-01-14 12:14
comments: true
categories: [Books, Mathematics]
tags: [Concrete Mathematics, math]
series: "Concrete Mathematics"
js: [math]
---
I finally finished the homework exercises.

<!-- more -->

## Homework Exercises Part 2

### Generalized Tower of Hanoi

To solve this, I first observed that for $n=1$, we need $m_1$ moves,
and for $n \gt 1$, we need
$A(m_1, \cdots, m_{n-1}) + m_n + A(m_1, \cdots, m_{n-1})$ or
$2A(m_1, \cdots, m_{n-1}) + m_n$ moves.

This leads to the solution,

<div markdown="0">
$$
\begin{aligned}
A(m_1, \cdots, m_n) &amp;= \sum_{i=1}^n m_i 2^{n-i}\\\\
\end{aligned}
$$
</div>

which is trivially shown by induction. The base case:

<div markdown="0">
$$
\begin{aligned}
A(m_1) &amp; = \sum_{i=1}^1 m_i 2^{1-i}\\\\
&amp; = m_1 2^0\\\\
&amp; = m_1
\end{aligned}
$$
</div>

And for larger $n$, assuming
$A(m_1, \cdots, m_n) = \sum_{i=1}^n m_i 2^{n-i}$,

<div markdown="0">
$$
\begin{aligned}
A(m_1, \cdots, m_{n+1}) &amp; = 2A(m_1, \cdots, m_n) +
m_{n+1}&amp;&amp;\text{by definition}\\\\
&amp; = 2\sum_{i=1}^n m_i 2^{n-i} + m_{n+1}&amp;&amp;\text{induction hypothesis}\\\\
&amp; = \sum_{i=1}^{n} m_i 2^{n+1-i} + m_{n+1} 2^{0}\\\\
&amp; = \sum_{i=1}^{n+1} m_i 2^{n+1-i}\\\\
\end{aligned}
$$
</div>

### Zig-zag lines

A geometric problem, but very similar to the previous intersecting
lines. A zig-zag is made of 3 segments, so a pair of zig-zag lines can
intersect at 9 different points. The first zig-zag line defines two
regions; each new zig-zag adds a new region, plus one more for each
intersection point.

This gives the following recurrence equations:

<div markdown="0">
$$
\begin{aligned}
ZZ_1 &amp; = 2\\\\
ZZ_n &amp; = ZZ_{n-1} + 9(n-1) + 1\\\\
\end{aligned}
$$
</div>

Using the linearity of the recurrence equation, it is easy to see that

<div markdown="0">
$$
\begin{aligned}
ZZ_n &amp; = ZZ_1 + 9S_{n-1} + (n-1)
\end{aligned}
$$
</div>

Here I used the linearity to compute solutions to both
$ZZ_n = ZZ_{n-1} + 9(n-1)$ and $ZZ_n = ZZ_{n-1} + 1$, which are
equally trivial. Then I combined the solutions into one.

I use (again) induction to confirm the solution. The base case is
$ZZ_1 = ZZ_1 + 9S_0 + 0$. And for other $n$, assuming
$ZZ_n = ZZ_1 + 9S_{n-1} + (n-1)$

<div markdown="0">
$$
\begin{aligned}
ZZ_{n+1} &amp; = ZZ_{n} + 9n + 1&amp;&amp;\text{by definition}\\\\
&amp; = ZZ_1 + 9S_{n-1} + (n-1) + 9n + 1&amp;&amp;\text{induction hypothesis}\\\\
&amp; = ZZ_1 + 9(S_{n-1} + n) + (n-1+1)\\\\
&amp; = ZZ_1 + 9S_n + n
\end{aligned}
$$
</div>

The formula can also be written as

<div markdown="0">
$$
\begin{aligned}
ZZ_n &amp; = \frac{9n^2-7n+2}{2}
\end{aligned}
$$
</div>

### Planes cutting cheese

Again, a geometric problem. This one gave me more trouble. It
took me a while before finally seeing that a new plane intersection
with the previous ones will be a set of intersecting lines which
defines the regions the new plan will divide in two.

The number of regions formed by intersecting lines was solved in the
book, and defined as $L_n = S_n + 1$

So a plane cutting $n$ existing planes will define
$P_{n+1} = P_n + L_n$
new regions. This recurrence gives $P_5 = 26$ regions.

The book did not expect a closed formula for this exercise, as the
necessary techniques are only covered in chapter 5.

### Josephus co-conspirator

The recurrence equation for $I(n)$ follow the structure of $J(n)$, but
with different base cases:

<div markdown="0">
$$
\begin{aligned}
I(2) &amp; = 2&amp;&amp;\text{$I(1)$ is not defined}\\\\
I(2n) &amp; = 2I(n) - 1\\\\
I(2n+1) &amp; = 2I(n) + 1
\end{aligned}
$$
</div>

Here I generated the first few values to get inspired. I noticed that
$I(n)$ had increasing odd values for batches that were longer than for
$J(n)$: $3, 6, 12, 24, \cdots$.

These numbers are from the series $3\cdot 2^m$, so using the same
"intuitive" step as in the book, I tried to show that
$I(3\cdot 2^m + l) = 2l + 1$ with $0 \le l \lt 3\cdot 2^m$
(the formula does not work for $I(2)$, which has to be defined separately).

By induction on $m$: the base case is $I(3) = I(3\cdot 2^0 + l) = 1$.

Assuming $I(3\cdot 2^m + l) = 2l+1$, we have

<div markdown="0">
$$
\begin{aligned}
I(3\cdot2^{m+1} + 2l) &amp; = 2I(3\cdot 2^m + l) -1&amp;&amp;\text{by definition}\\\\
&amp;= 2(2l+1) -1&amp;&amp;\text{induction hypothesis}\\\\
&amp;= 4l+2-1\\\\
&amp;= 2(2l)+1\\\\
I(3\cdot 2^{m+1} + (2l+ 1)) &amp; = 2I(3\cdot 2^m + l) + 1&amp;&amp;\text{by definition}\\\\
&amp; = 2(2l+1) + 1&amp;&amp;\text{induction hypothesis}\\\\
\end{aligned}
$$
</div>

The book solution is defined in terms of $2^m+2^{m-1}+k$, which is
same:

<div markdown="0">
$$
\begin{aligned}
2^m+2^{m-1}+k &amp; = 2\cdot 2^{m-1} + 2^{m-1} + k\\\\
&amp; = 3\cdot 2^{m-1} + k
\end{aligned}
$$
</div>

with $1 \le m$, while I have $0 \le m$.

### Repertoire method

I put the repertoire method in its own
[post](/blog/2012/01/14/concrete-mathematics-repertoire-method/) as it
was both the most difficult exercise and the one where I learned the most.
