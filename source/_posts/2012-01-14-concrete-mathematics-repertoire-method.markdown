---
layout: post
title: "Concrete Mathematics Repertoire Method"
date: 2012-01-14 13:33
comments: true
categories: [Books, Mathematics, How-to, Article]
tags: [Concrete Mathematics, math, repertoire method]
series: "Concrete Mathematics"
js: [math]
---
The repertoire method is never really explained in the book, or
anywhere else I could find on the Internet. There are a couple of
posts on this subject, so I though I should add mine.

The repertoire method is really a tool to help with the intuitive step
of figuring out a closed formula for a recurrence equation. It does so
by breaking the original problem into smaller parts, with the hope
they might be easier to solve.

<!-- more -->

### Why it works

Let's assume we have a system of recurrence equations with parameters,
so that the unknown function can be expressed as a linear combination
of other (unknown) functions where the coefficients are the parameters:

<div markdown="0">
\begin{align}
g(1) &amp; = b(0, \alpha_1, \cdots, \alpha_m)\\\\
g(n) &amp; = r_n(g_1, \cdots, g_{n-1}, \alpha_1, \cdots, \alpha_m)\\\\
&amp; = \sum_{i=1}^m A_i(n)\alpha_i,
\end{align}
</div>

We can consider $g$ as a specific point in a $m$-dimensional function
space (determined by both the recurrence equations, and the
parameters), and because $g$ is a linear combination, we can try to
find $m$ base functions (hopefully known or easy to compute)
$f_k(n) = \sum_{i=1}^m A_i(n)\alpha_{i_k}$ with $1 \le k \le m$, expressed in
terms of $m$ linearly independent vectors
$(\alpha_{1_k},\cdots,\alpha_{m_k})$.

In other words, if we can find $m$ linearly independent parameter
vectors such that, for each, we have a known solution $f_k(n)$, then
we can express the function $g$ as a linear combination of $f_k(n)$
for any parameters (because the $m$ $f_k(n)$ form a base for the
$m$-dimensional function space defined by the recurrence equations).

### How it works

First, we need to check that the recurrence equations accept a
solution expressed as

<div markdown="0">
\begin{align}
g(n) &amp; = \sum_{i=1}^m A_i(n)\alpha_i
\end{align}
</div>

It is enough to plug this definition into the recurrence equations,
and make sure the different parameters always remain in different
terms.

Then we can either solve $f(n) = \sum_{i=1}^m A_i(n)\alpha_i$ for
known $f(n)$, or for known $\alpha_i$
parameters, as long as we end up with $m$ linearly independent
parameter vectors (or, as it is equivalent, $m$ linearly independent
known functions for specific parameters).

It is important to keep in mind that a solution can be searched from
both direction: either set a function and try to solve for the
parameters, or set the parameters and solve for the function.

### Homework exercise

Given

<div markdown="0">
\begin{align}
g(1) &amp; = \alpha\\\\
g(2n+j) &amp; = 3g(n) + \gamma n + \beta_j&amp;&amp;\text{for \(j=0, 1\) and \(n \gt 1 \)}\\\\
\end{align}
</div>

We need to check that $g$ can be written as

<div markdown="0">
\begin{align}
g(n) &amp; = \alpha A(n) + \beta_0 B_0(n) + \beta_1 B_1(n) + \gamma C(n)\\\\
\end{align}
</div>

The base case is trivial. The recurrence case is

<div markdown="0">
\begin{align}
g(2n) &amp; = 3g(n) + \gamma n + \beta_0\\\\
&amp; = 3(\alpha A(n) +  \beta_0 B_0(n) + \beta_1 B_1(n) + \gamma C(n)) + \gamma n \beta_0\\\\
&amp; = \alpha 3A(n) + \beta_0 (3 B_0(n) + 1) + \beta_1 3B_1(n) + \gamma (3C(n) + n)\\\\
g(2n+1) &amp; = 3g(n) + \gamma n + \beta_1\\\\
&amp; = 3(\alpha A(n) +  \beta_0 B_0(n) + \beta_1 B_1(n) + \gamma C(n)) + + \gamma n\beta_1\\\\
&amp; = \alpha 3A(n) + \beta_0 3 B_0(n)+ \beta_1 (3B_1(n) + 1) + \gamma (3C(n) + n)\\\\
\end{align}
</div>

so $g$ can be expressed as a linear combination of other functions,
with the parameters as the coefficients.

Now, when I tried to solve this problem, I didn't know I could set the
parameters to values that would lead to an easy solution ($\gamma = 0$
turns the problem into an easy to solve generalised radix-based
Josephus problem); instead I wasted a lot of time trying to find known
functions and solve for the parameters, which is why I have four steps
below instead of just two as in the book.

#### $g(n) = n$

As the book suggests, I tried to solve for $g(n) = n$:

<div markdown="0">
\begin{align}
1 = g(1) &amp; = \alpha&amp;&amp;\alpha = 1\\\\
2n = g(2n) &amp; = 3g(n) + \gamma n + \beta_0\\\\
&amp; = 3n + \gamma n + \beta_0&amp;&amp;\gamma = -1, \beta_0 = 0\\\\
2n+1 = g(2n+1) &amp; = 3g(n) + \gamma n + \beta_1\\\\
&amp; = 3n - n + \beta_1&amp;&amp; \beta_1 = 1\\\\
\end{align}
</div>

#### $g(2^m+l) = 3^m$

As the recurrence equation looks like the generalised radix-based
Josephus equation, I tried to solve for $g(2^m+1) = 3^m$:

<div markdown="0">
\begin{align}
1 = g(1) &amp; = \alpha&amp;&amp;\alpha = 1\\\\
3^m = g(2^m+2l) &amp; = 3g(2^{m-1}+l) + \gamma (2^{m-1} + l) + \beta_0\\\\
&amp; = 3\cdot 3^{m-1} + \gamma (2^{m-1} + l) + \beta_0&amp;&amp; \beta_0, \gamma = 0\\\\
3^m = g(2^m+2l+1) &amp; = 3g(2^{m^1}+l) + \gamma (2^{m-1} + l) + \beta_1\\\\
&amp; = 3\cdot 3^{m-1}&amp;&amp;\beta_1 = 0\\\\
\end{align}
</div>

#### $g(n) = 1$

I tried to solve for $g(n) = 1$, as it seemed useful to solve for a
constant (no linear combination of linearly independent non-constant
functions can produce a constant function).

<div markdown="0">
\begin{align}
1 = g(1) &amp; = \alpha&amp;&amp; \alpha = 1\\\\
1 = g(2n+j) &amp; = 3g(n) + \gamma n + \beta_j\\\\
&amp; = 3 + \gamma n + \beta_j&amp;&amp; \gamma = 0, \beta_j = -2\\\\
\end{align}
</div>

#### $\alpha, \beta_1 = 1, \beta_0,  \gamma = 0$

This is the step that took me the longest, and when I finally
understood I could fix the parameters, I was able to use the
radix-based Josephus solution.

The recurrence equations 
<div markdown="0">
\begin{align}
g(1) &amp; = 1\\\\
g(2n) &amp; = 3g(n)\\\\
g(2n+1) &amp; = 3g(n) + 1\\\\
\end{align}
</div>

have as solution $g(2^m + (b_m\cdots b_0)) = 3^m + (b_m\cdots b_0)_3$.

#### Solving for $g(n)$

We have the equations

<div markdown="0">
\begin{align}
A(n) - C(n) &amp; = n\\\\
A(2^m + l) &amp; = 3^m\\\\
A(n) -2(B_0(n) + B_1(n)) &amp; = 1\\\\
B_1(2^m+l) &amp; = h_3(l)&amp;&amp;\text{where \(h_3(b_m\cdots b_0) = (b_m\cdots b_0)_3\)}\\\\
\end{align}
</div>

We have two functions already defined ($A(n)$ and $B_1(n)$), and the
other two equations give us the remaining function.

Now we can solve for $g(n)$:

<div markdown="0">
\begin{align}
g(2^m+l) = \alpha 3^m &amp; + \beta_0 (\frac{3^m - 1}{2} - h_3(l))\\\\
&amp; + \beta_1 h_3(l) \\\\
&amp;+ \gamma (3^m + h_3(l) - 2^m - l)
\end{align}
</div>

The $\gamma$ term is really $h_3(n) - n$.

The $\beta_0$ term is the same as $h_3(2^m-1-l)$, as can be seen by
observing that in base $3$, $3^m$ is $1$ followed by $m$ zeroes, so
$3^m-1$ is $m$ twos, and $\frac{3^m-1}{2}$ is $m$ ones, in other words
the same representation as the binary representation of $2^m-1$.

Now, the binary representation of $l$ is the same as the
representation in base $3$ of $h_3(l)$ (by definition of $h_3$), so
the binary representation of $2^m-1-l$ is the same as the
representation in base $3$ of $\frac{3^m-1}{2} - h_3(l)$.

With these two observations, it is possible to rewrite $g$ as

<div markdown="0">
\begin{align}
g(1b_m\cdots b_0) &amp; = (\alpha\beta_{b_m}\cdots\beta_{b_0})_3 + \gamma ((1b_m\cdots b_0)_3 - (1b_m\cdots b_0)_2)
\end{align}
</div>

which is the book solution. 

### Faster solution

It is enough to solve for
$\alpha, \beta_0, \beta_1 \ne 0, \gamma = 0$,
and to find the parameters for $g(n) = n$. The first gives $A$,
$B_0$ and $B_1$ directly by the generalised radix-based Josephus
solution, and the second one adds a constraint to solve for $C$ as well.

### Wrapping up

As can be seen above, approaching the problem from both directions
(solving for known functions and solving for known parameters) can
result in time saved, and simplified expression of the solution.
