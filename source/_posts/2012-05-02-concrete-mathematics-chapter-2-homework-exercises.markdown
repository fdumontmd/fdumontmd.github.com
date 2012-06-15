---
layout: post
title: "Concrete Mathematics Chapter 2 Homework Exercises"
date: 2012-05-02 13:13
comments: true
categories: [Books, Mathematics]
tags: [Concrete Mathematics, math]
series: "Concrete Mathematics"
js: [math]
---
It has been a long time since I wrote about this book; I had worked
the solutions more than a month ago, but then life happened, and I
could not find the time (or, perhaps, more accurately the courage) to
typeset my notes...

Anyway, I do have time now and am eager to go on with Chapter 3; but
first let's finish Chapter 2. Today the homework exercises, and very
soon the exams and bonus (at least the ones I could do) exercises.

<!--more-->

## Homework

### $2T_n = nT_{n-1}+3\cdot n!$

This exercise is not tricky in any way; just follow the method and
the result is guaranteed.

The recurrence equations are

<div markdown="0">
\begin{align}
T_0 &amp; = 5\\\\
2T_n &amp; = nT_{n-1}+3\cdot n!\\\\
\end{align}
</div>

The $a_n$, $b_n$ and $c_n$ series are:

<div markdown="0">
\begin{align}
a_n &amp; = 2\\\\
b_n &amp; = n\\\\
c_n &amp; = 3\cdot n!\\\\
\end{align}
</div>

The summation factor

<div markdown="0">
\begin{align}
s_n &amp; = \frac{a_{n-1}\dots a_1}{b_n\dots b_2}s_1\\\\
&amp; = \frac{2^{n-1}}{n!}s_1\\\\
\end{align}
</div>

After experimenting a bit, I found that $s_1 = 2$ is slightly easier
to work with, so the summation factor is $s_n = \frac{2^n}{n!}$.

With $S_n = \frac{2^n+1}{n!} T_n$, the recurrence equation becomes

<div markdown="0">
\begin{align}
S_n &amp; = S_{n-1} + 3\cdot 2^n\\\\
&amp; = S_0 + 3\sum_{k=1}^n 2^k\\\\
\end{align}
</div>

The sum is well-known, with $\sum_{k=0}^n 2^k = 2^{n+1}-1$, so 
$\sum_{k=1}^n 2^k = 2^{n+1}-2$.

Going back to $T_n$, we have

<div markdown="0">
\begin{align}
T_n &amp; = \frac{n!(10+3(2^{n+1}-2))}{2^{n+1}}\\\\
&amp; = \frac{n!(5+3(2^n-1))}{2^n}\\\\
&amp; = \frac{n!(5 + 3\cdot 2^n - 3)}{2^n}\\\\
&amp; = \frac{n!(3\cdot 2^n + 2)}{2^n}\\\\
&amp; = 3\cdot n! + \frac{n!}{2^{n-1}}\\\\
\end{align}
</div>

### $\sum_{k=0}^n kH_k$

Using the perturbation method:

<div markdown="0">
\begin{align}
S_{n+1} = S_n + (n+1) H_{n+1} &amp; = 0 + \sum_{k=1}^{n+1} k H_k\\\\
&amp; = \sum_{k+1=1}^{n+1}(k+1)H_{k+1}&amp;&amp;k\leftarrow k+1\\\\
&amp; = \sum_{k=0}^n (k+1) (H_k + \frac{1}{k+1})\\\\
&amp; = \sum_{k=0}^n k H_k + \sum_{k=0}^n \frac{k}{k+1} 
+ \sum_{k=0}^n H_k + \sum_{k=0}^n \frac{1}{k+1}\\\\
&amp; = S_n + \sum_{k=0}^n\frac{k+1}{k+1} + \sum_{k_0}^n H_k\\\\
&amp; = S_n + n+1 + \sum_{k=0}^n H_k\\\\
\end{align}
</div>

so $\sum_{k=0}^n H_k$ is

<div markdown="0">
\begin{align}
\sum_{k=0}^n H_k &amp; = (n+1)H_{n+1} - (n + 1)\\\\
&amp; = (n+1)H_n + (n+1)\frac{1}{n+1} - n - 1\\\\
&amp; = (n+1)H_n + 1 - n - 1\\\\
&amp; = (n+1)H_n - n\\\\
\end{align}
</div>

### More perturbation method

This exercise is just tricky in the very first step (working out the
exact meaning of $S_{n+1}$), as the sign of the terms change depending
of whether $n$ is odd or even.

This means that instead of the book equation (2.24)
$S_{n+1} = S_n + a_{n+1}$, we find something like
$S_{n+1} = a_{n+1} - S_n$.

#### $S_n = \sum_{k=0}^n (-1)^{n-k}$

First, the left hand part of the equation:

<div markdown="0">
\begin{align}
S_{n+1} &amp; = \sum_{k=0}^n (-1)^{n+1-k} + (-1)^{n+1-n-1}\\\\
&amp; = -\sum_{k=0}^n (-1)^{n-k} + 1\\\\
&amp; = 1 - S_n\\\\
\end{align}
</div>

Then, the right hand part:

<div markdown="0">
\begin{align}
S_{n+1} &amp; = (-1)^{n+1} + \sum_{k=1}^{n+1} (-1)^{n+1-k}\\\\
&amp; =
(-1)^{n+1}+\sum_{k+1=1}^{n+1}(-1)^{n+1-k-1}&amp;&amp;k\leftarrow k+1\\\\
&amp; = (-1)^{n+1} + \sum_{k=0}^n(-1)^{n-k}\\\\
&amp; = (-1)^{n+1} + S_n\\\\
\end{align}
</div>

Putting both together, $S_n = \frac{1-(-1)^{n+1}}{2}$, or, as the book
states, $S_n = [\text{\(n\) is even}]$.

#### $T_n = \sum_{k=0}^n (-1)^{n-k}k$

Using the same approach as above:

<div markdown="0">
\begin{align}
T_{n+1} &amp; = \sum_{k=0}^{n+1}(-1)^{n+1-k}k\\\\
&amp; = -\sum_{k=0}^n(-1)^{n-k}k + (-1){n+1-n-1}(n+1)\\\\
&amp; = n+1-T_n\\\\
\end{align}
</div>

and

<div markdown="0">
\begin{align}
T_{n+1} &amp; = \sum_{k=0}^{n+1}(-1)^{n+1-k}k\\\\
&amp; = (-1)^{n+1}0 + \sum_{k=1}^{n+1}(-1)^{n+1-k}k\\\\
&amp; = 0 + \sum_{k+1=1}^{n+1}(-1)^{n+1-k-1}{k+1}&amp;&amp;k\leftarrow k+1\\\\
&amp; = \sum_{k=0}^n(-1)^{n-k}k + \sum_{k=0}^n(-1)^{n-k}\\\\
&amp; = T_n + S_n\\\\
\end{align}
</div>

Together:

<div markdown="0">
\begin{align}
T_n &amp; = \frac{n+1-S_n}{2}\\\\
&amp; = \frac{1}{2}\left(n+[\text{\(n\) is odd}] \right)\\\\
&amp; = \left\lceil \frac{n}{2} \right\rceil\\\\
\end{align}
</div>

The last version uses the ceiling operator from Chapter 3.

#### $U_n = \sum_{k=0}^n (-1)^{n-k}k^2$

It will probably not be a surprised to find $U_n$ expressed in terms
of $S_n$ and $T_n$.

<div markdown="0">
\begin{align}
U_{n+1} &amp; = \sum_{k=0}^{n+1}(-1)^{n+1-k}k^2\\\\
&amp; = \sum_{k=0}^n(-1)^{n+1-k}k^2 + (-1)^{n+1-n-1}(n+1)^2\\\\
&amp; = -1\sum_{k=0}^n(-1)^{n-k}k^2 + (n+1)^2\\\\
&amp; = (n+1)^2 - U_n\\\\
\end{align}
</div>

and

<div markdown="0">
\begin{align}
U_{n+1} &amp; = \sum_{k=0}^{n+1}(-1)^{n+1-k}k^2\\\\
&amp; = (-1)^{n+1}0 + \sum_{k=1}^{n+1}(-1)^{n+1-k}k^2\\\\
&amp; = 0 + \sum_{k+1=1}^{n+1}(-1)^{n+1-k-2}(k+1)^2&amp;&amp;
k\leftarrow k+1\\\\
&amp; = \sum_{k=0}^n(-1)^{n-k}(k^2+2k+1)\\\\
&amp; = U_n + 2T_n + S_n\\\\
\end{align}
</div>

With $2T_n = n+1-S_n$, this produces $U_{n+1} = U_n + n + 1$, which
gives the answer away, but let's just continue with the current
method.

Putting both side together:

<div markdown="0">
\begin{align}
U_n &amp = \frac{(n+1)^2 - (n+1)}{2}\\\\
&amp; = \frac{(n+1)(n+1) - (n+1)}{2}\\\\
&amp; = \frac{n(n+1)}{2}\\\\
\end{align}
</div>

### Lagrange's Identity

First, I look for a usable double sum. I use the fact that for any
$j, k$, $j < k$, $(a_jb_k - a_kb_j) = -(a_kb_j - a_jb_k)$ and 
$(A_jB_k - A_kB_j)= - (A_kB_j - A_jB_k)$. This means that, with 
$s_{j,k} = (a_jb_k - a_kb_j)(A_jB_k - A_kB_j)$, $s_{j,k} = s_{k,j}$.

There is also the fact that $s_{j,j} = 0$, so now I can complete the
sum to the whole rectangle:

<div markdown="0">
\begin{align}
\sum_{1\le j,k\le n}s_{j,k} &amp; = \sum_{1\le j\lt k\le n}s_{j,k}
+ \sum_{1\le j = k \le n} s_{j,k} + \sum_{1\le k \lt j \le
n}s_{k,j}\\\\
&amp; = \sum_{1\le j \lt k \le n}s_{j,k} + 0
+ \sum_{1\le j \lt k \le n}s_{j,k}\\\\
&amp; = 2\sum_{1\le j \lt k \le n}s_{j,k}\\\\
\end{align}
</div>

The expansion of $s_{j,k}$ is 
$a_jA_jb_kB_k - a_jB_jA_kb_k - A_jb_ja_kB_k + b_jB_ja_kA_k$. Showing
the summation just for the first one (the other three are identical):

<div markdown="0">
\begin{align}
\sum_{1\le j, k \le n} a_jA_jb_kB_k &amp; = \sum_{j=1}^n\sum_{k=1}^n a_jA_jb_kB_k\\\\
&amp; = \sum_{j=1}^n a_jA_j \left(\sum_{k=1}^n b_kB_k \right)\\\\
&amp; = \left(\sum_{j=1}^n a_jA_j\right)\left(\sum_{k=1}^n b_kB_k \right)\\\\
&amp; = \left(\sum_{k=1}^n a_kA_k\right)\left(\sum_{k=1}^n b_kB_k \right)\\\\
\end{align}
</div>

Putting it all together:

<div markdown="0">
\begin{align}
\sum_{1\le j\lt k\le n}(a_jb_k - a_kb_j)(A_jB_k - A_kB_j) &amp; =
\left(\sum_{k=1}^n a_kA_k\right)\left(\sum_{k=1}^n b_kB_k\right) -
\left(\sum_{k=1}^n a_kB_k\right)\left(\sum_{k=1}^n A_kb_k\right)\\\\
\end{align}
</div>

In particular, with $a_k = A_k$ and $b_k = B_k$, the sum is
$\left(\sum_{k=1}^n a_k^2 \right)\left(\sum_{k=1}^n b_k^2 \right) - 2 \left(\sum_{k=1}^n a_kb_k \right)$.

### $\sum_{k=1}^n \frac{2k+1}{k(k+1)}$

#### Partial fractions

<div markdown="0">
\begin{align}
\sum_{k=1}^n\frac{2k+1}{k(k+1)}
&amp; = \sum_{k=1}^n(k+(k+1))\left(\frac{1}{k}-\frac{1}{k+1} \right)\\\\
&amp; = \sum_{k=1}^n\left(\frac{k}{k} + \frac{k+1}{k} - \frac{k}{k+1} - \frac{k+1}{k+1} \right)\\\\
&amp; = \sum_{k=1}^n \frac{k+1}{k} - \sum_{k=1}^n \frac{k}{k+1}\\\\
&amp; = n + H_n - \sum_{k=1}^n \frac{k}{k+1} -
\sum_{k=1}^n\frac{1}{k+1} + \sum_{k=1}^n\frac{1}{k+1}\\\\
&amp; = n + H_n - \sum_{k=1}^n \frac{k+1}{k+1} + \sum_{k=1}^n
\frac{1}{k+1}\\\\
&amp; = H_n + \sum_{k-1=1}^n \frac{1}{k}&amp;&amp;k\leftarrow k-1\\\\
&amp; = H_n + \sum_{k=2}^{n+1} \frac{1}{k}\\\\
&amp; = H_n + H_{n+1} - 1\\\\
&amp; = H_h + H_n + \frac{1}{n+1} - \frac{n+1}{n+1}\\\\
&amp; = 2H_n - \frac{n}{n+1}\\\\
\end{align}
</div>

#### Sum by parts

Using

<div markdown="0">
\begin{align}
\Delta v &amp; = \frac{1}{k(k+1)} = (k-1)^{\underline{-2}}\\\\
v &amp; = -(k-1)^{\underline{-1}}\\\\
Ev &amp; = -k^{\underline{-1}}\\\\
u &amp; = 2k+1\\\\
\Delta u &amp; = 2\\\\
\end{align}
</div>

First the sum by part

<div markdown="0">
\begin{align}
\sum \frac{2x+1}{x(x+1)}\delta x &amp; = 
-(2x+1)(x-1)^{\underline{-1}} + 2 \sum x^{\underline{-1}} \delta x + c\\\\
&amp; = -\frac{2x+1}{x} + 2 H_x + c\\\\
\end{align}
</div>

Then the evaluation

<div markdown="0">
\begin{align}
\sum_{k=1}^n\frac{2k+1}{k(k+1)} &amp; = \left. -\frac{2x+1}{x}+2H_x\right|_1^{n+1}\\\\
&amp; = -\frac{2(n+1)+1}{n+1} + 2 H_{n+1} + 2 + 1 - 2\\\\
&amp; = 2 H_{n+1} + 1 - 2 - \frac{1}{n+1}\\\\
&amp; = H_{n+1} + H_n - 1\\\\
&amp; = 2H_n - \frac{n}{n+1}\\\\
\end{align}
</div>

### $\sum_{1\le k \lt n}\frac{H_k}{(k+1)(k+2)}$

For the sum by part, I use

<div markdown="0">
\begin{align}
\Delta v &amp; = x^{\underline{-2}}\\\\
v &amp; = -x^{\underline{-1}}\\\\
Ev &amp; = -(x+1)^{\underline{-1}}\\\\
u &amp; = H_x\\\\
\Delta u &amp; = x^{\underline{-1}}\\\\
\end{align}
</div>

The sum by part

<div markdown="0">
\begin{align}
\sum_{k=1}^n H_x x^{\underline{-2}} \delta x
&amp; = -H_x x^{\underline{-1}} + \sum
(x+1)^{\underline{-1}}x^{\underline{-1}}\delta x + c\\\\
&amp; = -H_x x^{\underline{-1}} + \sum x^{\underline{-2}} \delta x + c\\\\
&amp; = -H_x x^{\underline{-1}} - x^{\underline{-1}} + c\\\\
&amp; = -(H_x + 1) x^{\underline{-1}} + c\\\\
\end{align}
</div>

The evaluation is

<div markdown="0">
\begin{align}
\sum_{0\le k \lt n} \frac{H_k}{(k+1)(k+2)} &amp; =
\left. -\frac{H_x + 1}{x+1} \right|_0^n\\\\
&amp; = 1 - \frac{H_n + 1}{n+1}\\\\
\end{align}
</div>

### Product laws

I don't think I listed all the laws for this exercise, as the only
complete list for the sum laws in the book is in the answer for this
exercise.

I will not repeat it here; suffice to say that when we replace sum by
product, the laws can be updated by replacing product by
exponentiation, and sum by product.

### $\prod_{1\le j \le k \le n}a_ja_k$

While it took me a few false starts, I eventually found that the
triangular completion used for (2.32) works here as well.

<div markdown="0">
\begin{align}
\left(\prod_{1\le j\le k \le n} a_ja_k \right)^2 &amp; = 
\left(\prod_{1\le j,k \le n}a_ja_k \right)
\left(\prod_{1\le j=k \le n}a_ja_k\right)\\\\
&amp; = \left(\prod_{1\le j,k \le n} a_j\right)
\left(\prod_{1\le j,k \le n} a_k\right)
\left(\prod_{1\le k \le n}a_k^2\right)\\\\
&amp; = \left(\prod_{1\le k \le n} a_k^n\right)
\left(\prod_{1\le j \le n} a_j^n\right)
\left(\prod_{1\le k \le n}a_k^2\right)\\\\
&amp; = \prod_{1\le k\le} a_k^{2n+2}\\\\
\end{align}
</div>

So
$\prod_{1\le j \le k \le n}a_ja_k = \left(\prod_{1\le k\le} a_k\right)^{n+1}$.

### $\sum_{k=1}^n \frac{(-2)^{\underline k}}{k}$

As suggested, I worked out $\Delta c^{\underline x}$:

<div markdown="0">
\begin{align}
\Delta c^{\underline x} &amp; = c^{\underline{x+1}} - c^{\underline x}\\\\
&amp; = c(c-1)\cdots (c-x+1)(c-x) - c(c-1)\cdots (c-x+1)\\\\
&amp; = c^{\underline x}(c-x-1)\\\\
\end{align}
</div>

I did not immediately saw the relation between this and the original
sum. First I rewrote the original sum to remove the division:

<div markdown="0">
\begin{align}
\sum_{k=1}^n \frac{(-2)^{\underline k}}{k}
&amp; = \sum_{k=1}^n \frac{(-2)^{\underline{k-1}}(-2-k+1)}{k}\\\\
&amp; = -\sum_{k=1}^n \frac{(-2)^{\underline{k-2}(k+1)(-2-k+2)}}{k}\\\\
&amp; = \sum_{k=1}^n \frac{(-2)^{\underline{k-2}(k+1)(k)}}{k}\\\\
&amp; = \sum_{k=1}^n (-2)^{\underline{k-2}}(k+1)\\\\
\end{align}
</div>

Now the relation is visible. So we have

<div markdown="0">
\begin{align}
\sum_1^{n+1}\frac{(-2)^{\underline x}}{x}\delta x &amp; = \sum_1^{n+1}(-2)^{\underline x}(x+1)\delta x\\\\
x &amp; = \left. - (-2)^{\underline{x-2}}\right|_1^{n+1}\\\\
&amp; = (-2)^{\underline{-1}} - (-2)^{\underline{n-1}}\\\\
&amp; = -1 - (-2)(-3) \cdots (-n)\\\\
&amp; = (-1)^n n! - 1\\\\
\end{align}
</div>

### Incorrect derivation

As stated in the book, the infinite sums do no converge, so the third
step is invalid.

And that's all for today.
