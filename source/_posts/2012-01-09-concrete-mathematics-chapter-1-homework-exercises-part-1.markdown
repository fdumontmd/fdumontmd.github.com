---
layout: post
title: "Concrete Mathematics Chapter 1 Homework Exercises Part 1"
date: 2012-01-09 20:23
comments: true
categories: [Books, Mathematics]
tags: [Concrete Mathematics, math]
series: "Concrete Mathematics"
js: [math]
---
I am working my way through the homework exercises, and so far I have
had more success than with the warmups. Here's what I have solved so far.

<!--more-->

## Homework Exercises

### Basic recurrence

This one was fairly simple, so simple that I wasted one hour trying to
improve the solution.

Just computing the first few terms of the sequence:

<div markdown="0">
\begin{align}
Q_0 &amp; = \alpha \\\\
Q_1 &amp; = \beta \\\\
Q_2 &amp; = \frac{1+\beta}{\alpha}\\\\
Q_3 &amp; = \frac{1+\alpha+\beta}{\alpha\beta}\\\\
Q_4 &amp; = \frac{\alpha\left( 1 + \alpha + \alpha\beta + \beta\right)}{\alpha\beta(1+\beta)}\\\\
&amp; = \frac{(1+\alpha)(1+\beta)}{\beta(1+\beta)}\\\\
&amp; = \frac{1+\alpha}{\beta}\\\\
Q_5 &amp; = \frac{\alpha\beta\left(1+\alpha+\beta \right)}{\beta \left( 1+\alpha+\beta \right)}\\\\
&amp; = \alpha\\\\
Q_6 &amp; = \frac{\beta\left( 1+\alpha \right)}{1+\alpha}\\\\
&amp; = \beta
\end{align}
</div>

So the sequence is cyclic. I tried to find a closed formula, but the
book does not go that far, and it is unlikely to be possible.

### Product of averages

#### $P(n)$ implies $P(n-1)$

First of all, I checked that nothing fishy was going on with the
selection of a particular $x_n$, but if $P(n)$ is true, then it is
true for every $x_1\cdots x_n$ set, and in particular for one with a
specific $x_n$.

So nothing fishy is going on.

Assuming the given value for $x_n$, we have

<div markdown="0">
\begin{align}
\left( \frac{x_1 + \cdots + x_n}{n} \right)^n &amp; = \left( \frac{x_1 + \cdots + x_{n-1} + \frac{x_1+\cdots+x_{n-1}}{n-1}}{n}\right)^n\\\\
&amp; = \left( \frac{(n-1)x_1 + \cdots + (n-1)x_{n-1}+x_1+\cdots + x_{n-1}}{n(n-1)}\right)^n\\\\
&amp; = \left( \frac{n(x_1+\cdots+x_{n-1})}{n(n-1)}\right)^n\\\\
&amp; = \left( \frac{x_1+\cdots+x_{n-1}}{n-1}\right)^n\\\\
&amp; = x_n^n
\end{align}
</div>

So, assuming $P(n)$, we have

<div markdown="0">
\begin{align}
x_1\cdots x_{n-1}x_n &amp;\le x_n^n\\\\
x_1\cdots x_{n-1} &amp;\le x_n^{n-1}\\\\
x_1\cdots x_{n-1} &amp;\le \left( \frac{x_1+\cdots+x_{n-1}}{n-1}\right)^{n-1}&amp;&amp;\text{i.e. $P(n-1)$}
\end{align}
</div>

#### $P(n)$ and $P(2)$ implies $P(2n)$

<div markdown="0">
\begin{align}
x_1\cdots x_{2n} &amp; = x_1\cdots x_{n}x_{n+1}\cdots x_{2n}&amp;&amp;\text{associativity}\\\\
&amp; \le \left(\frac{x_1+\cdots+x_n}{n}\right)^n \left(\frac{x_{n+1}+\cdots+x_{2n}}{n}\right)^n&amp;&amp;\text{applying $P(n)$ twice}\\\\
&amp; = \left( \frac{x_1+\cdots+x_n}{n}\frac{x_{n+1}+\cdots+x_{2n}}{n}\right)^n\\\\
&amp; \le \left( \left(\frac{\frac{x_1+\cdots+x_n}{n} + \frac{x_{n+1}+\cdots+x_{2n}}{n}}{2} \right)^2\right)^n&amp;&amp;\text{applying $P(2)$}\\\\
&amp; = \left( \frac{x_1+\cdots+x_n+x_{n+1}+\cdots+x_{2n}}{2n}\right)^{2n}&amp;&amp;\text{i.e. $P(2n)$}\\\\
\end{align}
</div>

#### $P(n) \forall n \ge 1$

The case for $P(1)$ is trivial, and $P(2)$ is already proven. We have
$P(n)$ implies $P(n-1)$ and $P(n)$ implies $P(2n)$.

One first approach is to use the basic induction step: we have $P(1)$,
$P(2)$, and we need $P(n) \implies P(n+1)$.

But $P(n) \implies P(2n) \implies P(2n-1) \implies \cdots \implies
P(2n-(n-1))$. The last one is $P(n1+)$, so the induction step holds.

Alternatively, we can show that to prove $P$ for a given $n$, we need
to prove $P$ for a smaller value. As naturals have a minimum, we must
eventually rely on $P(2)$, which would prove the whole chain.

To see this, for $n \ge 3$, if $n = 2m$, we need to prove $P(m)$; if
$n = 2m+1$, we need to prove $P(2m+2)$, which is implied by $P(m+1)$.

So, $\forall n \ge 3, \exists m \lt n \mid P(m) \implies P(n)$. That
with the base cases is enough to establish $P(n) \forall n$.

### Clockwise Tower of Hanoi

First, both $Q_0$ and $R_0$ are trivial.

Then, to move $n$ discs from $A$ to $B$, you need to move $n-1$ discs
from $A$ to $C$ (counter-clockwise), then move one disc from $A$ to
$B$, then move the $n-1$ discs from $C$ to $B$ (again,
counter-clockwise).

This means $Q_n = R_{n-1} + 1 + R_{n-1} = 2R_{n-1} + 1$.

The case for $R_n$ is a bit more complex. My first (flawed) attempt
was to observe that to move $n$ discs from $B$ to $A$, you could move
them from $B$ to $C$, then $C$ to $A$. In other words,

<div markdown="0">
\begin{align}
R_n &amp; \ge 2Q_n\\\\
&amp; = Q_n + 2R_{n-1} + 1&amp;&amp;\text{Replacing one \(Q_n\) by \(2R_{n-1}+1\)}\\\\
&amp; = Q_n + 4Q_{n-1} + 1&amp;&amp;\text{Replacing \(R_{n-1}\) by \(2Q_{n-1}\)}\\\\
\end{align}
</div>

But the $4Q_{n-1}$ means moving the stack of $n-1$ discs $4$ times,
which is the same as moving it just one time (as $3$ times bring it
back to its original position).

So we're left with just $Q_n + Q_{n-1} + 1$. But as I said, this
reasoning is flawed, as it mixes the count of moves with the effect of
moves (where $3$ moves are the same as $0$ move).

While it is possible to repair this reasoning by introducing special
operators that take two parameters (the number of discs, and the
number of steps), it is simpler to try and express $R_n$ strictly in
terms $n-1$ stacks and $1$ disc moves.

So, to move $n$ discs from $B$ to $A$, you need to move $n-1$ discs
from $B$ to $A$ (counter-clockwise), then one disc from $B$ to $C$
(clockwise), then the $n-1$ discs from $A$ to $B$ (clockwise), then
one disc from $C$ to $A$ (clockwise), then finally the $n-1$ discs
from $B$ to $A$ (counter-clockwise).

Or,

<div markdown="0">
\begin{align}
R_n &amp;= R_{n-1} + 1 + Q_{n-1} + 1 + R_{n-1}\\\\
&amp; = 2R_{n-} + 1 + Q_{n-1} + 1\\\\
&amp; = Q_n + Q_{n-1} + 1&amp;&amp;\text{definition of \(Q_n\)}\\\\
\end{align}
</div>

As the recurrence is expressed (initially) only in terms of necessary
moves of strictly smaller stacks, there is no risk of hiding moves
that are equivalent to no moves (as in my first attempt), so the
equation is the minimum number of moves.

### Double Tower of Hanoi

#### Basic Problem

First, notice we should keep each pair together, because otherwise
they would block larger discs from moving. So each pair of move should
be used to relocate a pair of identical discs to another peg. So we
should expect to need twice as many moves as the original tower.

More precisely, with $A(n)$ the number of moves required to solve a
$2n$ Double Tower of Hanoi, we have:

<div markdown="0">
\begin{align}
(1) &amp; = 2\\\\
A(n) &amp; = A(n-1) + 2 + A(n-1)\\\\
&amp; = 2A(n-1) + 2\\\\
\end{align}
</div>

Using $A(n) + 2= U(n)$, we get

<div markdown="0">
\begin{align}
U(1) &amp; = 4\\\\
U(n) &amp; = 2U(n-1)\\\\
&amp; 2^{n+1}\\\\
A(n) &amp; = 2^{n+1} -2\\\\
     &amp; = 2(T_n)\\\\
\end{align}
</div>

#### Order Preserving

If we consider all the $2n$ discs as different, then in $T_{2n}=2^{2n}-1$
moves, we can recreate the same order as the original.

Of course, we can do better. It is enough to move each pair an even
number of times: the first time will switch their order; the second
one will restore it, ...

This is similar to the warmup problem where we cannot move any disc
directly between any two pegs, so using the same constraint to move
any pair would get us to the target order in less than $2\cdot 3^n-1$.

But there is still a better way.

A $2$ discs problem needs exactly $3$ moves.  And the $4$ discs
problem will require just $11$ moves, rather than the $18$ that the
above formula predicts.

A wild guess: the number of moves is $4(2^n-1)-1$.

In trying to solve (or even write) the recurrence equation, it is
important to keep in mind that several ways to move the discs, each
with its own count, will be used.

We know we need two pairs of moves to relocate the two bottom discs
while keeping the order (assuming there are other discs). In doing so,
we will move the next two discs an even number of times, requiring
another 2 times to keep their order. So as long as we make sure the
last operation has the right (even) number of moves, we do not need to
keep this constraint on the other operations.

To recap: we need to move the last pair of discs $2$ times. So we first move
the $n-1$ pairs to a peg, then move the last pair to the other peg,
then the $n-1$ pairs to the first peg, then the last pair to the last
peg. At this stage, the $n-1$ discs have move an even number of times,
so their in the right order, even if we used the non order preserving
solution that was computed above (requiring $2T_{n-1}$ moves).

At this stage, a bit of notation should clarify:

<div markdown="0">
\begin{align}
B(1) &amp; = 3\\\\
B(n) &amp; = A(n-1) + 2 + A(n-1) + 2 + B(n-1)\\\\
&amp; = 2T_{n-1} + 2 + 2T_{n-1} + 2 + B(n-1)\\\\
&amp; = 4T_{n-1}+4+B(n-1)\\\\
\end{align}
</div>

Trying to prove the guess above:

<div markdown="0">
\begin{align}
B(1) &amp; = 4(2^1-1) - 1\\\\
&amp; = 3\\\\
B(n) &amp; = 4(2^{n-1} -1) + 4 + B(n-1)\\\\
&amp; = 4\cdot 2^{n-1} + 4(2^{n-1} - 1) -1\\\\
&amp; = 4\cdot 2^n -4 -1\\\\
&amp; = 4(2^n-1)-1
\end{align}
</div>

So the guess was right. It can also be rewritten as

<div markdown="0">
\begin{align}
4(2^n-1)-1 &amp = 4\cdot 2^n - 4 -1\\\\
&amp; = 2^{n+2} - 5\\\\
\end{align}
</div>

which is the book solution.

This being a bonus exercise, I currently experience an intense, although
pointless, sense of pride and achievement.

No doubt the other exercises will cut me down to size.
