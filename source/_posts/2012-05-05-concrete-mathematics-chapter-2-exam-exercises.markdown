---
layout: post
title: "Concrete Mathematics Chapter 2 Exam Exercises"
date: 2012-05-05 14:36
comments: true
categories: [Books, Mathematics]
tags: [Concrete Mathematics, math]
series: "Concrete Mathematics"
js: [math]
---
The exam exercises for this chapter were much trickier than the
homework exercises. With the right approach, they could be solved
quickly; too bad I never found that right approach...
<!--more-->

## Exam Exercises

### $\sum_{k=1}^n(-1)^k\frac{k}{4k^2-1}$

I managed to solve this exercise using sum by parts; the book uses
partial fractions, but as far as I can tell I never learned about
these. They seem to be part of high school curriculum in America, but
might not be in Belgium (or I was sleeping that day).

Sum by parts worked for me because I eventually asked myself what kind
of function of $k$ would produce a finite difference 
$\frac{1}{4k^2-1} = \frac{1}{(2k+1)(2k-1)}$.

By looking at $\Delta (2k)^{\underline m}$, I realised that the
solution was fairly simple. I first experimented with 
$\Delta (2k)^{\underline{-1}}$, then found the right expression:

<div markdown="0">
\begin{align}
\Delta (2k-2)^{\underline{-1}} &amp; = \frac{1}{2k+1}-\frac{1}{2k-1}\\\\
&amp; = \frac{2k-1 - 2k - 1}{(2k+1)(2k-1)}\\\\
&amp; = \frac{-2}{4k^2-1}\\\\
\end{align}
</div>

For the sum by parts, I can therefore try to use:

<div markdown="0">
\begin{align}
\Delta v &amp; = \frac{1}{4k^2-1}\\\\
v &amp; = -\frac{1}{2}(2k-2)^{\underline{-1}}\\\\
Ev &amp; = -\frac{1}{2}(2k)^{\underline{-1}}\\\\
u &amp; = (-1)^kk\\\\
\Delta u &amp; = (-1)^k +(k+1)(-2 (-1)^k)\\\\
&amp; = (-1)^k - 2(k+1)(-1)^k\\\\
&amp; = (-1)^k(1-2k-2)\\\\
&amp; = -(-1)^k(2k+1)\\\\
\end{align}
</div>

The last expression was computed using the product rule for finite difference.

When I put everything into the sum by parts formula, the various
blocks felt into place with satisfying "clicks":

<div markdown="0">
\begin{align}
\sum (-1)^x\frac{x}{4x^2-1}\delta x 
= &amp; -\frac{1}{2}(-1)^x x(2x-2)^{\underline{-1}} \\\\
&amp; - \frac{1}{2} \sum (-1)^x (2x+1)(2x)^{\underline{-1}}\delta x\\\\
= &amp; -\frac{(-1)^x x}{2(2x-1)} - \frac{1}{2}\sum (-1)^x \delta x
&amp;&amp;(2x+1)(2x)^{\underline{-1}}=1\\\\
= &amp; -\frac{(-1)^x x}{2(2x-1)} + \frac{(-1)^x}{4} + c\\\\
= &amp; (-1)^x \left(\frac{1}{4} - \frac{x}{2(2x-1)} \right) + c\\\\
= &amp; (-1)^x \left(\frac{4x - 2 - 4x}{4.2.(2x-1)} \right) + c\\\\
= &amp; -\frac{(-1)^x}{8x-4} + c\\\\
\end{align}
</div>

The answer as a function of $n$ is

<div markdown="0">
\begin{align}
\sum_{k=1}^{n} (-1)^k\frac{k}{4k^2-1} 
= &amp; \sum_1^{n+1} (-1)^x\frac{x}{4x^2-1}\delta x\\\\
= &amp; \left. -\frac{(-1)^x}{8x^4}\right|_1^{n+1}\\\\
= &amp; -\frac{(-1)^{n+1}}{8n+8-4} - \frac{1}{4}\\\\
= &amp; \frac{(-1)^n}{8n+4} - \frac{1}{4}\\\\
\end{align}
</div>

### 1050

I could not do this exercise; I also failed to see the basic sum that
was at the centre of the question (quite prominently).

Even the book solution took me a while to figure out.

The book solution asks how many pairs $a$, $b$ are there such that 
$\sum_{a\le k \lt b}k = 1050$.

Rewriting this in terms of finite calculus is simple enough:

<div markdown="0">
\begin{align}
\sum_{a\le k \lt b}k &amp; = \sum_a^b x\delta x\\\\
&amp; = \frac{1}{2}\left(b^{\underline 2} - a^{\underline 2}\right)\\\\
&amp; = \frac{1}{2}\left(b(b-1) - a(a-1)\right)\\\\
&amp; = \frac{1}{2}\left(b^2-b - a^2 + a\right)\\\\
&amp; = \frac{1}{2}\left(b^2-b + ab - a^2 + a - ab\right)\\\\
&amp; = \frac{1}{2}\left(b(b-1+a) - a(a-1+b)\right)\\\\
&amp; = \frac{1}{2}(b-a)(b+a-1)\\\\
\end{align}
</div>

Now, one thing to notice is that if a sum of two integers is even, so is
the difference, and vice-versa. Therefore the product above is the
product of one even and one odd integers.

So we are now looking for ways to express 

<div markdown="0">
\begin{align}
(b-a)(b+a-1)=xy=2.1050=2100=2^2 3 5^2 7\\\\
\end{align}
</div>

as a product with $x$ is even and $y$ odd.

To compute how many ways there are to produce a divisor of a number
whose prime factors are known, it is enough to see that, for each
prime factor $p$ with multiplicity $n_p$, this prime can be left out,
or included up to $n_p$ times in the divisor; so for each prime p
there are $n_p+1$ possibilities. Summing these over the prime factors
give the number of divisors of the original number.

In the present case, this number is 12.

Now that we have the number of possible pairs of $x$ and $y$, we have
to go back to $a$ and $b$.  There might be a principle involved here,
but I don't really see it, so I'll just try to rebuild the solution
from the ground up.

We already know that either
$b-a=x$ and $b+a-1=y$ or $b-a=y$ and $b+a-1=x$. So we already see that
whatever expression we need, both $a$ and $b$ will have a
$\frac{1}{2}$ added so that their difference cancels out, and their
sum produces a $1$ that will cancel the $-1$.

Looking at sum and differences, we have $(x+y)-(x-y) = 2y$ and
$(x+y)+(x-y)=2x$, so there will be a sum and a difference involved. We
also need $a$ to be smaller than $b$, so it is a candidate for 
$\frac{1}{2}(x-y)+\frac{1}{2}$. However, we also need it to be
positive, so we add an absolute value.

So the candidate solutions are $a=\frac{1}{2}|x-y|+\frac{1}{2}$ and 
$b=\frac{1}{2}(x+y)+\frac{1}{2}$. Let's check them:

<div markdown="0">
\begin{align}
x \gt y: &amp; 
(\frac{1}{2}(x+y)+\frac{1}{2} - \frac{1}{2}(x-y)-\frac{1}{2})
(\frac{1}{2}(x+y)+\frac{1}{2} + \frac{1}{2}(x-y)+\frac{1}{2} - 1)\\\\
= &amp; (\frac{1}{2}(2y))(\frac{1}{2}(2x))\\\\
= &amp; yx\\\\
x \lt y: &amp;
(\frac{1}{2}(x+y)+\frac{1}{2} - \frac{1}{2}(y-x)-\frac{1}{2})
(\frac{1}{2}(x+y)+\frac{1}{2} + \frac{1}{2}(y-x)+\frac{1}{2} - 1)\\\\
= &amp; (\frac{1}{2}(2x))(\frac{1}{2}(2y))\\\\
= &amp; xy\\\\
\end{align}
</div>

So it all adds up.

### Riemann's zeta function

This exercise was more in line with the content of this chapter, and
easy enough. Essentially, it is just a matter of changing the order of
summation.

#### $\sum_{k\ge 2}(\zeta(k)-1) = 1$

<div markdown="0">
\begin{align}
\sum_{k\ge 2}(\zeta(k)-1) 
&amp; = \sum_{k\ge 2}(\sum_{j\ge 1}\frac{1}{j^k}-1)\\\\
&amp; = \sum_{k\ge 2}(\sum_{j\ge 2}\frac{1}{j^k})\\\\
&amp; = \sum_{j\ge 2}\sum_{k\ge 2}\frac{1}{j^k}\\\\
&amp; = \sum_{j\ge 2}\sum_{k\ge 2}(\frac{1}{j})^k\\\\
\end{align}
</div>

The inner sum is a geometric progression:

<div markdown="0">
\begin{align}
\sum_{k\ge 2}(\frac{1}{j})^k 
&amp; = \lim_{n\rightarrow \infty}
\frac{\frac{1}{j^2}-\frac{1}{j^n}}{1-\frac{1}{j}}\\\\
&amp; = \frac{\frac{1}{j^2}-0}{1-\frac{1}{j}}\\\\
&amp; = \frac{1}{j(j-1)}
\end{align}
</div>

So we need to solve
$\sum_{j\ge 2}\frac{1}{j(j-1)}=\sum_{j\ge 0}\frac{1}{(j+1)(j+2)}$,
which we already saw as well, and the value is indeed 1.

#### $\sum_{k\ge 1}(\zeta(2k)-1)$

Using the same approach:

<div markdown="0">
\begin{align}
\sum_{k\ge 1}(\zeta(2k)-1) &amp; = \sum_{k\ge 1} 
(\sum_{j\ge 1}\frac{1}{j^{2k}}-1)\\\\
&amp; = \sum_{k\ge 1}\sum_{j\ge 2}\frac{1}{j^{2k}}\\\\
&amp; = \sum_{j\ge 2}\sum_{k\ge 1}\frac{1}{j^{2k}}\\\\
&amp; = \sum_{j\ge 2}\sum_{k\ge 1}(\frac{1}{j^2})^k\\\\
\end{align}
</div>

Once again, we have a geometric progression, just as easy to solve as
the previous one:

<div markdown="0">
\begin{align}
\sum_{k\ge 1}(\frac{1}{j^2})^k 
&amp; = \lim_{n\rightarrow \infty}
\frac{\frac{1}{j^2}-\frac{1}{j^{2n}}}{1-\frac{1}{j^2}}\\\\
&amp; = \frac{\frac{1}{j^2}-0}{1-\frac{1}{j^2}}\\\\
&amp; = \frac{1}{j^2-1}\\\\
&amp; = \frac{1}{(j11)(j+1)}\\\\
&amp; = \frac{j}{(j-1)j(j+1)}\\\\
&amp; = j (j-2)^{\underline{-3}}\\\\
\end{align}
</div>

Now we have $\sum_{j\ge 2} j(j-2)^{\underline{-3}}$, which can be
summed by parts. I chose:

<div markdown="0">
\begin{align}
\Delta v &amp; = (x-2)^{\underline{-3}}\\\\
v &amp; = -\frac{(x-2)^{\underline{-2}}}{2}\\\\
Ev &amp; = -\frac{(x-1)^{\underline{-2}}}{2}\\\\
u &amp; = x\\\\
\Delta u &amp; = 1\\\\
\end{align}
</div>

and now the sum by part:

<div markdown="0">
\begin{align}
\sum_2^{\infty} x(x-2)^{\underline{-3}}\delta x
&amp; = -\frac{x}{(x-1)x} + \frac{1}{2}\sum_2^\infty
(x-1)^{\underline{-2}}\delta x\\\\
&amp; = \left. -\frac{1}{2(x-1)}-\frac{1}{2x}\right|_2^\infty\\\\
&amp; = \frac{3}{4}\\\\
\end{align}
</div>

### $\sum_{k\ge 0}\min(k, x \dot{-} k)$

I am not really sure of my solution here, despite the fact that the
outcome is identical to the book; my method is somewhat different from
the book's, and using some concepts from Chapter 3.

To prove that the two sums have the same value, I just evaluate each.

First, a basic observation on the $\dot{-}$ operator: if 
$b\le 0$, $a\dot{-}b$ is at least zero, and if $a\le 0$, at most
$a$ (otherwise it is always zero).

So if $a\le 0$ or $a\le b$, $a\dot{-}b=0$.

I will now assume that $x\ge 0$; otherwise both sums are zero.

### $\sum_{k\ge 0}\min(k,x\dot{-}k)$

First, I replace the infinity sum by a finite one: as seen above, if
$k\gt x$, $\min(k,x\dot{-}k)=0$, so

<div markdown="0">
\begin{align}
\sum_{k\ge 0}\min(k,x\dot{-}k) &amp; = \sum_{0\le k \le x}\min(k,x-k)\\\\
\end{align}
</div>

I then try to remove the $\min$ operator:

<div markdown="0">
\begin{align}
k &amp; \le x - k\\\\
2k &amp; \le x\\\\
k &amp; \le \frac{x}{2}\\\\
\end{align}
</div>

so that

<div markdown="0">
\begin{align}
\sum_{0\le k \le x}\min(k,x-k)
&amp; = \sum_{0\le k \le \frac{x}{2}} k
&amp; + \sum_{\frac{x}{2}\lt k \le x} x - k
\end{align}
</div>

The general idea here is to find a way to eliminate the $k$ terms, by
shifting each term in the second sum by an equal amount.

At this point, the number of terms in each sum is important: the total
number of terms is the number of $k$ such that $0\le k\le x$; clearly
this is $\lfloor x \rfloor + 1$.

The number of terms in the first sum is similarly 
$\lfloor\frac{x}{2}\rfloor + 1$.

The number of terms in the second sum is therefore 
$\lfloor x \rfloor - \lfloor \frac{x}{2} \rfloor$. To find an
expression for this, it helps to look at integral $x$ first.

If $x=4$, $\lfloor 4 \rfloor - \lfloor \frac{4}{2} \rfloor = 2$. 

If $x=5$, $\lfloor 5 \rfloor - \lfloor \frac{5}{2} \rfloor = 5-2 = 3$.

More generally, if $2n\le \lt 2n+1$, there will be $n+1$ terms in the
first sum, and $n$ in the second. And if $2n-1\le x\le 2n$, there will
be $n$ terms in each sum.

A first attempt for the number of terms in the second sum is 
$\lfloor \frac{x}{2} \rfloor$, but this only works for $x$ such that
$2n\le x\lt 2n+1$. But it is easy to see that 
$\lfloor \frac{x+1}{2} \rfloor$ will always work: if 
$2n\le x\lt 2n+1$, 
$\lfloor \frac{x+1}{2} \rfloor = \lfloor\frac{2n+1}{2}\rfloor = n$,
and if $2n-1\le x\lt 2n$, 
$\lfloor \frac{x+1}{2}\rfloor = \lfloor\frac{2n}{2}\rfloor = n$.

Using this value to "shift" the terms of the second sum:

<div markdown="0">
\begin{align}
\sum_{0\le k \le \frac{x}{2}} k
+ \sum_{\frac{x}{2}\lt k \le x} x - k
&amp; = \sum_{0\le k \le \frac{x}{2}} k
+ \sum_{\frac{x}{2}-\lfloor \frac{x+1}{2}\rfloor\lt k\le x- \lfloor
\frac{x+1}{2}\rfloor}
(x-\left\lfloor \frac{x+1}{2}\right\rfloor)-k\\\\
&amp; = \sum_{0\le k \le \lfloor \frac{x}{2} \rfloor} k
+ \sum_{\frac{x}{2}-\lfloor \frac{x+1}{2}\rfloor\lt k\le \lfloor
\frac{x}{2}\rfloor}
(x-\left\lfloor \frac{x+1}{2}\right\rfloor)-k\\\\
\end{align}
</div>

Now the question is whether the new $k$ in the second sum cancel the
$k$ in the first sum. Once again, let's check the cases:

 * if $2n\le x\lt 2n+1$, 
 $\frac{x}{2} \gt \lfloor\frac{x+1}{2}\rfloor$, so $k\ge 1$: this
 means the $\lfloor \frac{x}{2}\rfloor$ non-zero $k$ terms of the
 first sum are cancelled by the 
 $\lfloor \frac{x+1}{2}\rfloor = \lfloor \frac{x}{2}\rfloor$ non zero
 $k$ terms of the second sum; the zero term can safely be ignored.
 * if $2n-1\le x\lt 2n$,
  $\frac{x}{2} \lt \lfloor\frac{x+1}{2}\rfloor$, so $k\ge 0$; this
  means the $\lfloor \frac{x}{2}\rfloor+1$ $k$ terms (including 0) are
  all cancelled by the 
  $\lfloor \frac{x+1}{2}\rfloor = \lfloor \frac{x}{2}\rfloor+1$ $k$
  terms of the second sum.
  
So we can safely rewrite the sum as

<div markdown="0">
\begin{align}
\sum_{\frac{x}{2} - \lfloor \frac{x+1}{2}\rfloor \lt k \le 
\lfloor\frac{x}{2}\rfloor}(x-\left\lfloor \frac{x+1}{2}\right\rfloor)\\\\
\end{align}
</div>

And, as we already know the number of terms is 
$\left\lfloor \frac{x+1}{2}\right\rfloor$, the sum value is

<div markdown="0">
\begin{align}
\left\lfloor \frac{x+1}{2}\right\rfloor
\left(x- \left\lfloor \frac{x+1}{2}\right\rfloor\right)
\end{align}
</div>

#### $\sum_{k\le 0}(x\dot{-}(2k+1))$

This sum is much easier than the previous one. First I remove the
$\dot{-}$ operator. I need $2k+1\le x$:

<div markdown="0">
\begin{align}
2k+1 &amp; \le x\\\\
k &amp; \le \frac{x-1}{2}\\\\
\end{align}
</div>

This gives me $\lfloor \frac{x+1}{2}\rfloor$ number of terms.

So I can extract $x$ and work only on $k$

<div markdown="0">
\begin{align}
\left\lfloor \frac{x+1}{2}\right\rfloor(x-1) +
2 \sum_{0\le k\lt \lfloor \frac{x+1}{2}\rfloor}k 
&amp; = \left\lfloor \frac{x+1}{2}\right\rfloor(x-1)+
\left\lfloor\frac{x-1}{2}\right\rfloor\left\lfloor\frac{x+1}{2}\right\rfloor\\\\
&amp; = \left\lfloor \frac{x+1}{2}\right\rfloor
(x - 1 - \left\lfloor \frac{x-1}{2}\right\rfloor)\\\\
&amp; = \left\lfloor \frac{x+1}{2}\right\rfloor
(x - \left\lfloor \frac{x+1}{2}\right\rfloor)\\\\
\end{align}
</div>

So the both expressions have the same value.

## Bonus Questions

### $\vee$ Laws

I find it easier to work from the basic $\min$ operator, and scale
that up to $\vee$ (the formulas can always be derived mechanically
from the underlying algebra).

$\min$ is an associative and commutative operator, is distributive
with addition, and its neutral element is $\infty$.

I will not repeat the full list of formulas; the book has them
already.

### Undefined infinite sums

An undefined sum, according to (2.59), is one in which both the
positive sum and the negative sums are unbounded.

I define $K^+$ as $\{k\in K| a_k \gt 0\}$ and $K^-$ as
$\{k\in K| a_k\lt 0\}$.

The point about unbounded sums is that, even if I drop a large number
of terms, there are always enough remaining terms to add up to an
arbitrary amount.

For instance, given $n$ even and $E_n=K^+\setminus F_{n-1}$,
it is always true that I can find $E'_n\subset E_n$ such that

<div markdown="0">
\begin{align}
\sum_{k\in E'_n}a_k \ge A^+ - \sum_{k\in F_{n-1}}a_k\\\\
\end{align}
</div>

So if I define $F_n = F_{n-1} \cup E'_n$, $\sum_{k\in F_n}a_k \ge A^+$.

And when $n$ is odd, with $O_n=K^-\setminus F_{n-1}$, I can always
find a subset $O'_n\subset O_n$ such that

<div markdown="0">
\begin{align}
\sum_{k\in O'_n}a_k \le \sum_{k\in F_{n-1}}a_k - A^-\\\\
\end{align}
</div>

(with $K^-$, the $a_k$ are smaller than zero, so the sum can be
arbitrarily small).

If I define $F_n = F_{n-1} \cup O'_n$, $\sum_{k\in F_n}a_k \le A^-$.

As I could not do the other bonus questions, this completes Chapter 2.
