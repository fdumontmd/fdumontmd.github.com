---
layout: post
title: "Concrete Mathematics Chapter 2 Warmups"
date: 2012-02-28 19:18
comments: true
categories: [Books, Mathematics]
tags: [Concrete Mathematics, math]
series: "Concrete Mathematics"
js: [math]
---
This first batch of exercises is meant to develop familiarity with
the various concepts and notations introduced in this chapter. There
is no complex manipulation, but the trick is to be aware of the often
unmentioned assumptions about the precise meaning of the expressions.

<!--more-->

## Warmups

### $\sum_{k=4}^0 q_k$

The meaning of such an expression is not clear, so there is no real
way to fail this exercise.

A first interpretation, maybe the common one, is that the sum is zero
because the range is empty. In other words, the sum is
$\sum_{4\le k\le 0} q_k$.

A second interpretation, perhaps for those used to programming
languages with very flexible loops could argue that the sum is
$q_4 + q_3 + q_2 + q_1 + q_0$.

I toyed briefly with a negative sum, similar to integrals with
reversed bounds, but I did not come up with the nice book solution
of $\sum_{k=m}^n = \sum_{k\le n} - \sum_{k\lt m}$, which is consistent
with and extends the first interpretation. 

### Simplify $x([x\gt 0] - [x\lt 0])$

It is easy to see that the expression has the same value as $|x|$:

<div markdown="0">
\begin{align}
x([x\gt 0] - [x\lt 0]) &amp; = x (1-0)&amp;&amp;\text{when \(x\gt 0\)}\\\\
&amp; = x\\\\
x([x\gt 0] - [x\lt 0]) &amp; = x (0-1)&amp;&amp;\text{when \(x\lt 0\)}\\\\
&amp; = -x\\\\
x([x\gt 0] - [x\lt 0]) &amp; = 0&amp;&amp;\text{when \(x = 0\)}\\\\
\end{align}
</div>

### Writing out sums

The first one is easy:
<div markdown="0">
\begin{align}
\sum_{0\le k\le 5}a_k = a_0+a_1+a_2+a_3+a_4+a_5\\\\
\end{align}
</div>

The second one is tricky, is more than one way. One problem is that
$k$ is not explicitly defined, and I had assumed it was a natural,
when the authors thought of it as a integer; now the latter is in line
with the book conventions, so I was wrong and had missing terms. The
right answer is:

<div markdown="0">
\begin{align}
\sum_{0\le k^2 \le 5}a_k = a_4 + a_1 + a_0 + a_1 + a_4\\\\
\end{align}
</div>

### Triple Sum

Here it is important to restrict the bounds as much as possible (but
no more); otherwise there is a risk of introducing spurious terms.

<div markdown="0">
\begin{align}
\sum_{1\le i \lt j \lt k \le n}a_{ijk} &amp; = \sum_{i=1}^2 \sum_{j=i+1}^3 \sum_{k=j+1}^4 a_{ijk}\\\\
&amp; = \left((a_{123} + a_{124}) + a_{134} \right) + a_{234}\\\\
&amp; = \sum_{k=3}^4 \sum_{j=2}^{k-1} \sum_{i=1}^{j-1} a_{ijk}\\\\
&amp; = a_{123}+\left(a_{124} + (a_{134} + a_{234})\right)\\\\
\end{align}
</div>

The terms appear in the same order, but are grouped in sums differently.

### Incorrect derivation

The problem is the step

<div markdown="0">
\begin{align}
\sum_{j=1}^n \sum_{k=1}^n = \frac{a_j}{a_k}\sum_{k=1}^n \sum_{k=1}^n \frac{a_k}{a_k}\\\\
\end{align}
</div>

$k$ is already bound in the inner sum, so it is invalid to replace $j$
by $k$ in the outer.

### $\sum_k [1\le j\le k\le n]$

This can be worked explicitly:

<div markdown="0">
\begin{align}
\sum_k [1 \le j \le k \le n] &amp = \sum_k [1 \le j \le n] [j \le k \le n]\\\\
&amp; = \sum_{j\le k \le n} [1 \le j \le n]\\\\
&amp; = [1 \le j \le n] \sum_{j\le k \le n} 1\\\\
&amp; = [1 \le j \le n] (n-j+1)\\\\
\end{align}
</div>

### $\bigtriangledown f(x)$

The result is not surprising:

<div markdown="0">
\begin{align}
\bigtriangledown x^{\overline{m}} &amp; = x^{\overline{m}} - (x-1)^{\overline{m}}\\\\
&amp; = x(x+1)\cdots(x+m-1) - (x-1)x\cdots(x+m-2)\\\\
&amp; = x(x+1)\cdots(x+m-2)(x+m-1-(x-1))\\\\
&amp; = m x^{\overline{m-1}}\\\\
\end{align}
</div>

So $\bigtriangledown f(x)$ is the difference operator to use with
rising factorials.

### $0^{\overline{m}}$

Clearly, when $m\lt 0$, $0^{\overline{m}} = 0$; when $m = 0$,
$0^{\overline{m}} = 1$ (to make the expression
$x^{\underline{1+0}}=x^{\underline 1}(x-1)^{\underline 0}$ work when $x=1$); I
had forgotten about $m<0$, which was perhaps the easiest case, as $\frac{1}{m!}$
(it follows directly from the definition of falling factorials with negative
powers).

### Law of exponents for rising factorials

It is easy to see that $x^{\overline{m+n}} = x^{\overline m}(x+m)^{\overline n}$:

<div markdown="0">
\begin{align}
x^{\overline{m+n}} &amp; = x\cdots(x+m-1)(x+m)\cdots(x+m+n-1)\\\\
&amp; = \left( x\cdots(x+m-1) \right) \left( (x+m)\cdots(x+m+n-1) \right)\\\\
&amp; = x^{\overline m}(x+m)^{\overline n}\\\\
\end{align}
</div>

From there, the value of rising factorials for negative powers follows quickly:

<div markdown="0">
\begin{align}
1 = x^{\overline{-n+n}} &amp; = x^{\overline{-n}} (x-n)^\overline{n}\\\\
x^{\overline{-1}} &amp; = \frac{1}{(x-n)^\overline{n}}\\\\
&amp; = \frac{1}{(x-n)\cdots(x-1)}\\\\
&amp; = \frac{1}{(x-1)^{\underline{n}}}\\\\
\end{align}
</div>

### Symmetric difference of a product

To start, I quickly looked up the proof of the original derivative
product rule on
[Wikipedia](http://en.wikipedia.org/wiki/Product_rule#Proof_of_the_product_rule);
the geometric nature of the proof was illuminating (I believe I was
taught the so called
[Brief Proof](http://en.wikipedia.org/wiki/Product_rule#A_Brief_Proof)
both in high-school and at university).

This geometric proof can be used for both the infinite and the finite
calculus, and its symmetric nature (there are two ways to compute the
area of the big rectangle:
$f(x)g(x)+(f(w)-f(x))g(w) + f(x)(g(w)-g(x))$ and
$f(x)g(x)+f(w)(g(w)-g(x)) + (f(w)-f(x))g(x)$) can be used in the
finite case. The symmetry (and equality) is restored
because in the infinite calculus, $\lim_{w\rightarrow x}f(w) = f(x)$
and $\lim_{w\rightarrow x}g(w) = g(x)$, a restoration that is not
possible in the finite calculus.

However, the equivalent finite calculus formulas,
$\bigtriangleup(uv) = u\bigtriangleup v + Ev\bigtriangleup u$ and
$\bigtriangleup(uv) = Eu\bigtriangleup v + v\bigtriangleup u$, have
together the symmetry they lack on their own.

### Wrapping up

OK, that was not entirely bad (two small mistakes, both about negative
numbers blindness). Next step, the basic exercises.
