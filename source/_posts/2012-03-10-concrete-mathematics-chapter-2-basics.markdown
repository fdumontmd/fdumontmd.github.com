---
layout: post
title: "Concrete Mathematics Chapter 2 Basics"
date: 2012-03-10 11:10
comments: true
categories: [Books, Mathematics]
tags: [Concrete Mathematics, math]
series: "Concrete Mathematics"
js: [math]
---
This second batch of exercises builds on the
[previous one](/blog/2012/02/28/concrete-mathematics-chapter-2-warmups/). Once
again, there are no complex manipulations, and very often the solution
just follows from the definitions.

<!--more-->

## Basics

### $\sum_{0\le k\lt n}(a_{k+1}-a_k)b_k$

To show that

<div markdown="0">
\begin{align}
\sum_{0\le k\lt n}(a_{k+1}-a_k)b_k &amp; = a_n b_n - a_0 b_0 - \sum_{0 \le k \lt n} a_{k+1}(b_{k+1} - b_k)&amp;&amp;n\ge 0\\\\
\end{align}
</div>

I start by rewriting the sum in the right side of the equation:

<div markdown="0">
\begin{align}
\sum_{0 \le k \lt n} a_{k+1}(b_{k+1} - b_k) &amp; = \sum_{0 \le k \lt n} (a_{k+1}b_{k+1} +  a_{k+1} b_k)\\\\
&amp; = \sum_{0 \le k \lt n} a_{k+1}b_{k+1} +  \sum_{0 \le k \lt n} a_{k+1} b_k&amp;&amp;\text{associative law}\\\\
&amp; = \sum_{0 \le k-1 \lt n} a_k b_k +  \sum_{0 \le k \lt n} a_{k+1} b_k&amp;&amp;k\leftarrow k-1\\\\
&amp; = \sum_{1 \le k \le n} a_k b_k +  \sum_{0 \le k \lt n} a_{k+1} b_k\\\\
\end{align}
</div>

This latest value can now be put back into the original right:

<div markdown="0">
\begin{align}
a_n b_n - a_0 b_0 - \sum_{1 \le k \le n} a_k b_k +  \sum_{0 \le k \lt n} a_{k+1} b_k &amp; = \sum_{0\le k \lt n} a_{k+1} b_k - (a_0 b_0 + \sum_{1 \le k \le n} a_k b_k - a_n b_n)\\\\
&amp; = \sum_{0\le k \lt n} a_{k+1} b_k - \sum_{0\le k \lt n} a_k b_k\\\\
&amp; = \sum_{0\le k \lt n} (a_{k+1} b_k - a_k b_k)\\\\
&amp; = \sum_{0\le k \lt n} (a_{k+1} - a_k) b_k\\\\
\end{align}
</div>

which is indeed the left side of the equation (the but-last step is
permitted under the associative law, but that didn't fit in the margin).

### $p(k) = k + (-1)^k c$

It is clear that there is a single $p(k)$ for every possible (integer)
$k$. So I need to show that for every $m$, there is a single $k$ such
that $p(k)=m$, defining $p^{-1}$.

The book method is smart, mine clearly less so, but as far as I can
tell, still correct: for $m$, I consider $m-c$ and $m+c$. The
difference is $2c$, so they're either both even, or both
odd.

If they're both even, then $m-c+(-1)^{m-c}c=m$, so $k=m-c$. If they're
both odd, then $m+c+(-1)^{m+c}c=m$, so $k=m+c$. So $k$ is always well
defined for every $m$, and $p$ is indeed a permutation.

### $\sum_{k=0}^n (-1)^k k^2$

While I found the closed formula for the sum, I could not do it with
the repertoire method.

Solving the sum is not really difficult (although a little bit than
the repertoire method, if you know how to do the latter); one way is
to solve the positive and negative sums separately (they can be broken
down to already solved sums); another one is to compute the sum of an
even number of terms (one positive and one negative), then to compute
sums of odd number of terms (by adding a term to the previous
solution), and finally combining both to find the closed formula.

In both attempts above, I tried to remove the $(-1)^k$ factor from the
terms; when using the repertoire method I tried to do the same, which
is why I failed.

The repertoire method relies on a good intuition: one must have a
sense of general shape of the parametric functions. In retrospect, it
seems obvious, but I just couldn't see it, blinded as I was by$(-1)^k$.

Expressing the sum as a recurrence is easy:

<div markdown="0">
\begin{align}
R_0 &amp; = 0\\\\
R_n &amp; = R_{n-1} + (-1)^n n^2\\\\
\end{align}
</div>

Also, looking at the first few terms of the sum,
$-1, 3, -6, 10, -15, \dots$, it is natural to consider solutions of
the form $(-1)^n F(n)$; it is a little bit trickier to see where a good
generalisation of the recurrence above should put the additional
terms:

<div markdown="0">
\begin{align}
R_0 &amp; = \alpha\\\\
R_n &amp; = R_{n-1} + (-1)^n \left(\beta + \gamma n + \delta n^2 \right)\\\\
\end{align}
</div>

With such a form, plugging in solutions $(-1)^nF(n)$ will
simplify to $F(n) = \beta + \gamma n + \delta n^2 - F(n-1)$. 

At this stage, it becomes very easy to find the $A(n)$, $B(n)$, $C(n)$
and $D(n)$ functions (the latter being the solution we are looking
for). In fact, if all you care about is $D(n)$, then it is enough to
use $R_n = (-1)^n n$ and $R_n = (-1)^n n^2$:

#### $R_n = (-1)^n n$

<div markdown="0">
\begin{align}
R_0 &amp; = 0&amp;&amp;\alpha = 0\\\\
n &amp; = \beta + \gamma n + \delta n^n - n + 1\\\\
2n - 1 &amp; = \beta + \gamma n&amp;&amp;\beta = -1, \gamma = 2\\\\
\end{align}
</div>

which gives $-B(n)+2C(n) = (-1)^n n$.

#### $R_n = (-1)^n n^2$

<div markdown="0">
\begin{align}
R_0 &amp; = 0&amp;&amp;\alpha = 0\\\\
n^2 &amp; = \beta + \gamma n + \delta n^2 - (n-1) ^2\\\\
2 n^2 - 2n + 1 &amp; = \beta + \gamma n + \delta n^2&amp;&amp;\beta = 1, \gamma = -2, \delta = 2\\\\
\end{align}
</div>

which gives $B(n)-2C(n)+2D(n) = (-1)^n n^2$. Combining with the
previous answer, we have $2D(n) = (-1)^n (n^2-n)$, or
$D(n) = (-1)^n \frac{n^2-n}{2}$.

#### Wrapping up this exercise

In hindsight, these steps could have helped me solve this
exercise as intended:

 * compute the first few terms to see if there is something obvious
 about their shape; in this case, the $(-1)^n$ factor
 * at first, write the recurrence equations as simply as possible,
 with all the "inconvenient" parts; comparing them to the "shapes"
 identified in the previous step might give some insight about the
 general solutions, and possibly removed these difficult parts
 * only then, consider how to generalise the recurrence equations. The
 base case is always $R_0 = \alpha$; the recurrent case should add
 parameters to each term, and additional terms (with their own
 parameters) to complete some basic classes of problems (for instance,
 if there are any polynomial, there should be a term for each power
 smaller than the largest power of the original problem; another basic
 class is the generalised radix-based Josephus problem)
 * each class of problems can be solved independently; this makes it
 easier to find potential solutions and to combine them.

### $\sum_{k=1}^n k2^k$

Not overly complicated; at least the introduction of $j$ is not a
mystery (unlike the next exercise).

<div markdown="0">
\begin{align}
\sum_{1\le k\le n}k 2^k &amp; = \sum_{1\le k\le n} 2^k \sum_{1\le j\le k}1\\\\
&amp; = \sum_{1\le k\le n} \sum_{1\le j\le k} 2^k\\\\
&amp; = \sum_{1\le j\le k \le n} 2^k\\\\
&amp; = \sum_{1\le j\le n} \sum_{j\le k\le n}2^k\\\\
\end{align}
</div>

The inner sum can be rewritten as

<div markdown="0">
\begin{align}
\sum_{j\le k\le n}2^k &amp; = \sum_{1\le k\le n}2^k - \sum_{1\le k\lt j}2^k\\\\
&amp; = 2^{n+1} - 2 - 2^j + 2\\\\
&amp; = 2^{n+1} - 2^j\\\\
\end{align}
</div>

Here I use the already known
sum $\sum 2^k$. Putting this last result
in the original sum

<div markdown="0">
\begin{align}
\sum_{1\le j\le n} 2^{n+1} - 2^j &amp; = n2^{n+1} - (2^{n+1} -2)\\\\
\end{align}
</div>

### $\sum_{k=1}^n k^3$

It took me some time to convince myself that the original rewrite was
legitimate; eventually I did it by induction (the book version is much
shorter, and once you see it, much easier). Clearly it works for
$n=1$, so assuming it does for $n-1$, we have

<div markdown="0">
\begin{align}
2\sum_{1\le j\le k\le n} jk &amp; = 2\sum_{1\le j\le k\le n-1} jk + 2\sum_{1\le j\le k=n} jk\\\\
&amp; = \sum_{1\le k\lt n}(k^3+k^2) + 2n\sum_{1\le j\le n} j\\\\
&amp; = \sum_{1\le k\lt n}(k^3+k^2) + n^2(n+1)\\\\
&amp; = \sum_{1\le k\lt n}(k^3+k^2) + n^3+n^2\\\\
\end{align}
</div>

So the rewrite is correct. At this stage, (2.33) pretty much finishes it:

<div markdown="0">
\begin{align}
\sum_{1\le k\le n}(k^3+k^2) &amp; = (\sum_{1\le k\le n}k)+\sum_{1\le k\le n}k^2\\\\
\end{align}
</div>

so $\sum_{1\le k\le n}k^3=\frac{n^2(n+1)^2}{4}$.

### $\frac{x^{\underline m}}{(x-n)^{\underline m}} = \frac{x^{\underline n}}{(x-m)^{\underline n}}$

This follows directly from
$\frac{a}{b} = \frac{c}{d} \implies ad = bc$, and the use of equation (2.52).

### Rising and Falling Factorial Powers Conversions

I'll just do the conversion from raising factorial power to falling
factorial power; the other conversion is just the same.

$x^{\overline m} = \frac{1}{(x-1)^{\underline m}}$ follows from (2.51)
and (2.52).

For the other equalities, by induction on $m$, and using (2.52) and
its raising factorial powers equivalent:

<div markdown="0">
\begin{align}
x^{\underline m} &amp; = x^{\underline{m-1}}(x-m+1)\\\\
&amp; = x^{\underline 1}(x-1)^{\underline{m-1}}\\\\
&amp; = x(x-1)^{\underline{m-1}}\\\\
x^{\overline m} &amp; = x^{\overline{m-1}}(x+m-1)\\\\
&amp; = x^{\overline 1}(x+1)^{\overline{m-1}}\\\\
&amp; = x(x+1)^{\overline{m-1}}\\\\
\end{align}
</div>

#### Base case $m=0$

They all follow from definition:

<div markdown="0">
\begin{align}
x^{\overline 0} &amp; = 1\\\\
(-1)^0 (-x)^{\underline 0} &amp; = 1\\\\
(x+0-1)^{\underline 0} &amp; = 1\\\\
\end{align}
</div>

#### Other positive $m$

Assuming the relations hold for all $k, 0\le k\lt m$:

<div markdown="0">
\begin{align}
(-1)^m(-x)^{\underline m} &amp; = -\left((-1)^{m-1}(-x)^{\underline{m-1}}(-x-m+1)\right)\\\\
&amp; = (x^{\overline{m-1}})(x+m-1)\\\\
(x+m-1)^{\underline m} &amp; = (x+m-1)^{\underline{m-1}}x\\\\
&amp; = (x+1+(m-1)-1)^{\underline{m-1}}x\\\\
&amp; = (x+1)^{\overline{m-1}}x\\\\
\end{align}
</div>

#### Negative $m$

Using the recurrence relations derived from (2.52) and its raising
factorial power equivalent:

<div markdown="0">
\begin{align}
x^{\underline m} &amp; = x^{\underline{(m+1)+(-1)}}\\\\
&amp; = x^{\underline{-1}}(x+1)^{\underline{m+1}}\\\\
&amp; = \frac{(x+1)^{\underline{m+1}}}{x+1}\\\\
&amp; = x^{\underline{m+1}}(x-m-1)^{\underline{-1}}\\\\
&amp; = \frac{x^{\underline{m+1}}}{x-m}\\\\
x^{\overline m} &amp; = x^{\overline{(m+1)+(-1)}}\\\\
&amp; = x^{\overline{-1}}(x-1)^{\overline{m+1}}\\\\
&amp; = \frac{(x-1)^{\overline{m+1}}}{x-1}\\\\
&amp; = x^{\overline{m+1}}(x+m+1)^{\overline{-1}}\\\\
&amp; = \frac{x^{\overline{m+1}}}{x+m}\\\\
\end{align}
</div>

Assuming the relations hold for all $k, m\lt k\le 0$:

<div markdown="0">
\begin{align}
(-1)^m(-x)^{\underline m} &amp; = -\frac{(-1)^{m+1}(-x)^{\underline{m+1}}}{-x-m}\\\\
&amp; = \frac{x^{\overline{m+1}}}{x+m}\\\\
(x+m-1)^{\underline m} &amp; = \frac{(x+m)^{\underline{m+1}}}{x+m-1-m}\\\\
&amp; = \frac{(x-1)^{\overline{m+1}}}{x-1}\\\\
\end{align}
</div>

So the main difficulties is to derive two equalities from (2.52) (four
if we count the negative cases as well), and the identification of the
recurrence equation in the induction step (especially for
$(x+m-1)^{\underline{m\pm 1}}$).

### Absolute Convergence of Complex Sums

I suppose I could say it follows directly from the equivalence of the
metric functions (if my memory of metric space terminology is correct).

More basically, the equivalence of the propositions follows from the
relationships based on the hypotenuse formula:
$\sqrt{(Rz)^2+(Iz)^2}\le |Rz| + |Iz|$, so the absolute convergence of
the real and imaginary parts implies the absolute convergence of the
absolute value. Conversely, $|Rz|,|Iz|\le\sqrt{(Rz)^2+(Iz)^2}$, so the
absolute convergence of the absolute value also implies the absolute
convergence of both the real and imaginary parts.

### Wrapping up

This time, I found a solution to all the exercises, which is a
progress of some sort. I still have trouble with the repertoire method,
or perhaps not with the method itself but in identifying suitable
generalisations and candidate solutions. This is something that can
only be developed with practice, so I just have to be patient and
keep trying (I hope I'll get there eventually).
