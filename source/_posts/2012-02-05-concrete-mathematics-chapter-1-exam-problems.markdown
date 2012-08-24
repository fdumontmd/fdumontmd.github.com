---
layout: post
title: "Concrete Mathematics Chapter 1 Exam Problems"
date: 2012-02-05 12:27
comments: true
categories: [Books, Mathematics]
tags: [Concrete Mathematics, math]
series: "Concrete Mathematics"
js: [math]
---
It took me longer than I thought, and the outcome is slightly
disappointing: I failed to solve two of the problems, and I solved the
remaining ones way too slowly, so in a real exam conditions I probably
would have solved just one or two...

<!-- more -->



## Exam Problems

### 4 Pegs Tower of Hanoi

First, it helps to see that the indices of the recurrence are actually
$S_n$:

<div markdown="0">
\begin{align}
W_{n(n+1)/2}&amp;= W_{S_n}\\\\
W_{n(n-1)/2}&amp;= W_{S_{n-1}}
\end{align}
</div>

And of course, $S_n = S_{n-1} + n$.

Setting $m=S_{n-1}$, we try to show:

<div markdown="0">
\begin{align}
W_{m+n} &amp; \le 2W_{m} + T_n\\\\
\end{align}
</div>

Now, obviously, if we have $m+n$ discs, we can move the $m$ top ones
from $A$ to $C$ using $B$ and $D$ as transfer pegs, then move the
bottom $n$ ones from $A$ to $B$ using $D$ as transfer peg, and finally
move the top $m$ ones from $C$ to $B$.

The first step takes $W_m$ moves, the second one is the classic Tower
of Hanoi problem (as we can no longer use peg $C$, we only have three
pegs), so it takes $T_n$ moves, and the last step takes $W_m$  moves again.

This is only one possible solution; the optimal one must be equal or
better, so we have

<div markdown="0">
\begin{align}
W_{m+n} &amp; \le 2W_m + T_n\\\\
\end{align}
</div>

This is true for any $m+n$ discs, and in particular for
$S_n = S_{n-1} + n$ ones.

### Specific Zigs

I could not solve this problem. I had found that the half-lines did
intersect, but then I failed to show that their intersections were all
distinct.

Even with the solution from the book, it took me a while before I
finally had a complete understanding.

One problem I had was that lines in a graph are basic college level
mathematics, but college was a long, long time ago. I pretty much had
to work from first principles.

Following the book in writing the positions as $(x_j, 0)$ and
$(x_j - a_j, 1)$, I need to find $\alpha$ and $\beta$ such that
$y=\alpha x + \beta$ is true for both points above.

<div markdown="0">
\begin{align}
0 &amp; = \alpha x_j + \beta \\\\
\beta &amp; = - \alpha x_j\\\\
1 &amp; = \alpha (x_j - a_j) - \alpha x_j\\\\
&amp; = \alpha x_j - \alpha a_j - \alpha x_j\\\\
&amp; = - \alpha a_j\\\\
\alpha &amp; = \frac{-1}{a_j}\\\\
y &amp; = \frac{x_j - x}{a_j}\\\\
\end{align}
</div>

With this given, I can try to find the intersection of lines from
different zigs, $j$ and $k$:

<div markdown="0">
\begin{align}
\frac{x_j - x}{a_j} &amp; = \frac{x_k - x}{a_k}\\\\
a_k (x_j - x) &amp; = a_j (x_k - x)\\\\
a_k x_j - a_k x &amp; = a_j x_k - a_j x\\\\
a_k x_j - a_j x_k &amp; = (a_k - a_j) x\\\\
\end{align}
</div>

Now, still following the book, I replace $x$ by $t$ with
$x=x_j - t a_j$:

<div markdown="0">
\begin{align}
a_k x_j - a_j x_k &amp; = (a_k - a_j) (x_j - t a_j)\\\\
a_k x_j - a_j x_k &amp; = a_k x_j - a_j x_j - t a_j a_k + t a_j^2\\\\
- a_j x_k &amp; = t a_j^ 2 - a_j x_j - t a_j a_k\\\\
- x_k &amp; = t a_j - x_j -t a_k&amp;&amp;\text{dividing by \(a_j\)}\\\\
x_j - x_k &amp; = t (a_j - a_k)\\\\
t &amp; = \frac{x_j - x_k}{a_j - a_k}\\\\
\end{align}
</div>

Somehow, I have a faint memory of such a result; I need to check a
college math book.

To complete, I need to show that $y = t$:

<div markdown="0">
\begin{align}
y &amp; = \frac{x_j - x}{a_j}\\\\
&amp; = \frac{x_j - x_j + t a_j}{a_j}\\\\
&amp; = \frac{t a_j}{a_j}\\\\
&amp; = t\\\\
\end{align}
</div>

So the intersection of any two pair of half-lines from different zigs
is $(x_j - t a_j, t)$. Note that $t$ has the same value whether
$j \gt k$ or $k \gt j$. To simplify further computations, I set
$j \gt k$.

There are two remaining steps: show that $t$ is different for
different pairs of $j$, $k$ (with $j \ne k$); and then show that the
four intersections for a pair $j$, $k$ are also distinct.

$a_j$ can be of two forms: $n^j$ and $n^j + n^{-n}$. So $a_j - a_k$
can be one of

<div markdown="0">
\begin{align}
&amp; n^j - n^k\\\\
&amp; n^j + n^{-n} - n^k\\\\
&amp; n^j - n^k - n^{-n}\\\\
n^j + n^{-n} - n^k - n^{-n} = &amp; n^j - n^k\\\\
\end{align}
</div>

So there are three different forms for $a_j - a_k$, which I will
simply write $n^j - n^k + \epsilon$ where $|\epsilon| \lt 1$.

<div markdown="0">
\begin{align}
t &amp; = \frac{n^{2j} - n^{2k}}{n^j - n^k + \epsilon}\\\\
&amp; = \frac{(n^j - n^k)(n^j + n^k)}{n^j - n^k + \epsilon}\\\\
\end{align}
</div>

Let's show that $n^j+n^k - 1 \lt t \lt n^j+n^k + 1$: multiply the
whole inequality by $n^j - n^k + \epsilon$. As

<div markdown"0">
\begin{align}
n^j - n^k &amp; \ge n\\\\
&amp; \ge 2\\\\
&amp; \gt |\epsilon|\\\\
\end{align}
</div>

so $n^j - n^k + \epsilon \gt 0$. Defining

<div markdown="0">
\begin{align}
N_{jk} &amp; = n^j + n^k\\\\
N'_{jk} &amp; = n^j - n^k\\\\
\end{align}
</div>

the left and right inequalities become

<div markdown="0">
\begin{align}
(N_{jk} - 1) (N'_{jk} + \epsilon) &amp; = N_{jk}N'_{jk} - N'_{jk} + \epsilon N_{jk} - \epsilon\\\\
(N_{jk} + 1) (N'_{jk} + \epsilon) &amp; = N_{jk}N'_{jk} + N'_{jk} + \epsilon N_{jk} + \epsilon\\\\
\end{align}
</div>

Subtracting $N_{jk}N'_{jk} = (n^j-n^k)(n^j+n^k)$ from the original inequality:

<div markdown="0">
\begin{align}
-N'_{jk}+\epsilon N_jk - \epsilon \lt 0 \lt N'_{jk} + \epsilon N_{jk} + \epsilon\\\\
\end{align}
</div>

I need to prove the following inequality


<div markdown"0">
\begin{align}
(n^j - n^k) &amp; \gt |\epsilon| + |\epsilon| (n^j - n^k)\\\\
\end{align}
</div>

We already know $|\epsilon| \lt 1$, so looking at the second term (and
assuming $\epsilon \ne 0$, as this case is trivial)

<div markdown"0">
\begin{align}
|\epsilon| (n^j-n^k) &amp; = n^{-n} (n^j - n^k)\\\\
&amp; = n^{j-n} - n^{k-n}\\\\
&amp;\lt 1\\\\
\end{align}
</div>

and we have

<div markdown"0">
\begin{align}
n^j - n^k &amp; \ge 2
&amp; \gt |\epsilon| + |\epsilon (n^j - n^k)|\\\\
\end{align}
</div>

So the inequalities are established. $N_{jk}$ can be seen as a number
in based $n$ where the digits are all zeroes except the $j$ and $k$ ones,
$N_{jk} = N_{j'k'} \implies j=j', k=k'$, and therefore $t$ uniquely
defines $j$ and $k$ or, two pairs of zigs must have different $t$.

I still need to show that for a given pair, when $t$ is the same, the
intersections are different. There are three different values of
$t$, so two intersections points have the same height. This happens
for

<div markdown="0">
\begin{align}
t &amp; = \frac{n^{2j} - n^{2k}}{n^j - n^k}\\\\
\end{align}
</div>

which happens when $a_j = n^j$, $a_k = n^k$ and $a_j = n^j + n^{-n}$,
$a_k = n^k + n^{-n}$. But the $x = x_j - t a_j$ value for
intersections is different: $t n^j$ and $t (n^j + n^{-n})$, so there
are indeed four distinct intersection points.

### 30 degrees Zigs

I could not solve this problem. Once again, my lack of intuition with
geometry was to blame.

But if we have two zigs with half-lines angles $\phi$, $\phi + 30^{\circ}$
and $\theta$, $\theta + 30^{\circ}$, then for any two pairs of
half-lines from the two zigs to intersect, their angles must be
between $0^{\circ}$ and $180^{\circ}$. Taken together, these
constraints give $30^{\circ} \lt |\phi - \theta| \lt 150^{\circ}$.

__Update: The original version of this post had a lower bound of
$0$. Thanks to Tailshot for pointing out the error__

This means there cannot be more than $5$ such pairs (and to be honest,
I would have said 4, but the book says it's indeed 5).

### Recurrence Equations

Using the repertoire method, solve the recurrence equations

<div markdown="0">
\begin{align}
h(1) &amp; = \alpha\\\\
h(2n+j) &amp; = 4h(n) + \gamma_j n + \beta_j\\\\
\end{align}
</div>

The general form of $h(n)$ is

<div markdown="0">
\begin{align}
h(n) &amp; = \alpha A(n) + \beta_0 B_0(n) + \beta_1 B_1(n) + \gamma_0 C_0(n) + \gamma_1 C_1(n)\\\\
\end{align}
</div>

We get three of these functions directly by solving

<div markdown="0">
\begin{align}
h(1) &amp; = \alpha\\\\
h(2n+j) &amp; = 4h(n) + \beta_j\\\\
h(2^m+b_m\cdots b_0) &amp; = (1\beta_{b_m}\cdots\beta_{b_0})_4\\\\
\end{align}
</div>

So we have a solution for $A(n)$, $B_0(n)$ and $B_1(n)$.

Setting $h(n) = n$

<div markdown="0">
\begin{align}
\alpha &amp; = 1\\\\
2n+j &amp; = 4n + \gamma_j n + \beta_j\\\\
\beta_j &amp; = j\\\\
\gamma_j &amp; = -2\\\\
\end{align}
</div>

which gives the equation $n = A(n) + B_1(n) -2(C_0(n) + C_1(n))$.

Setting $h(n) = n^2$

<div markdown="0">
\begin{align}
\alpha &amp; = 1\\\\
4n^2 + 4jn + j &amp; = 4n^2 + \gamma_j n + \beta_j\\\\
\beta_j &amp; = j\\\\
\gamma_j &amp; = 4j\\\\
\end{align}
</div>

which gives the  equation $n^2 = A(n) + B_1(n) + 4C_1(n)$

The latest gives us $C_1(n) = (n^2 - A(n) - B_1(n))/4$. To solve for
$C_0$, one can either replace the value of $C_1$ in the equation for
$h(n) = n$ above, or, equivalently, add twice that equation to the one
for $h(n) = n^2$, which eliminates $C_1(n)$:

<div markdown="0">
\begin{align}
2n + n^2 &amp; = 3A(n) + 3B_1(n) -4C_0(n)\\\\
C_0(n) &amp; = \frac{3A(n) + 3B_1(n) - n^2 - 2n}{4}\\\\
\end{align}
</div>

### Good and Bad Persons in Josephus Problem

It took me a while, as I was trying to find a recurrence equation of
some sort which would help me with this problem and the bonus one
(where Josephus' position is fixed but he can pick $m$). Eventually I
found one, which did not help me with the bonus problem, but led me to
a solution for this problem.

Obviously, if we have $k$ persons and want to remove the last one in
the first round, we can choose $m=k$ and that will work. Actually, any
multiple $m=ak$ works as well.

This shows that at each round, if we have $k$ persons left, and we
start counting on the first one, when $m=ak$ we will remove the $k^{th}$
person then start counting from the first one again.

Back to the original problem: there are $2n$ persons, and we want to
get rid of the $n+1, \cdots, 2n$ first. If we take
$m=lcm(n+1,\cdots, 2n)$, then for the first $n$ rounds the last (bad)
person will be remove, leaving only the good ones at the end.

When first solving the problem, I picked $m=\prod_{i=1}^n (n+i)$,
which has the same property as the least common multiple, but is
larger. Perhaps a smaller number is better for the nerves of the
participants.

### Bonus Problems

I tried to solve the bonus questions, but after repeatedly failing, I
had a glimpse at the solutions: they obviously require either
knowledge of later chapters, or other concepts I know nothing about,
so I will get back to these bonus problems after I finish the book.

I am now working through Chapter 2. It is a much larger chapter than
the first, so it will take me some time.
