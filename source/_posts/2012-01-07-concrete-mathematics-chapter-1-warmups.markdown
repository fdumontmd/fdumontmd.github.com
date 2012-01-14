---
layout: post
title: "Concrete Mathematics Chapter 1 Warmups"
date: 2012-01-07 19:09
comments: true
categories: [Books, Mathematics]
tags: [Concrete Mathematics, math]
series: "Concrete Mathematics"
js: [math]
---
It took me far longer than it should have, and I had a very partial
success; I guess my excuse is that my brain was still cold...

At least I can claim I did try to solve all the exercises; I really
spent hours on this.

<!--more-->

## Warmups

### Horse colour

I kind of botched this one, as I tried to answer it even before
reading the chapter... and my first instinct was that such a use of
induction (taking numbered subsets) was invalid.

Of course it is not. This is a perfectly valid approach, but, as the
book states, in the present case it breaks down for $n=2$.

Properly expressed with math notation, it becomes clear that the
``same colour'' concept is a binary relation (a reflexive, symmetric
and transitive one). The key is _binary_: if every pair of horses were
the same colour, then induction could be used.

### Tower of Hanoi Variation

The description in the book is somewhat confusing, as it states the
restriction in terms of absolute positions (that is, no direct move
between left peg and right peg), rather than relative (if you want to
move a disc between peg $A$ and peg $B$, you must first move it to peg $C$,
then to peg $B$).

The first approach does not work (that is, it is impossible to solve
the problem under these conditions), but obviously the authors meant
the second approach.

#### Number of moves

This variation can be solved using the exact same tools as the
original problem.

Assuming we want to move a stack from $A$ to $B$, using $C$ as
transfer peg: a single disc can be moved in $2$ steps ($A$ to $C$, $C$
to $B$); to move more than $1$, you first need to move the $n-1$ from
$A$ to $B$, then move $1$ disk from $A$ to $C$, move the $n-1$ discs
from $B$ back to $A$, move the one disc from $C$ to $B$, and finally
move the $n-1$ discs from $A$ to $B$.

More concisely:

<div markdown="0">
$$
\begin{align}
T_1 &amp;= 2&amp;&amp;\text{base case}\\\\
T_n &amp;= T_{n-1} + 1 + T_{n-1} + 1 + T_{n-1}\\\\
&amp; = 3T_{n-1} + 2&amp;&amp;\text{recurrence equation}
\end{align}
$$
</div>

Using the exact same method as in the book, let's define
$T_n + 1= U_n$:

<div markdown="0">
$$
\begin{align}
U_1 &amp;= T_1 + 1\\\\
&amp; = 3\\\\
U_n &amp;= T_n + 1\\\\
&amp; = 3(U_{n-1} -1) + 3\\\\
&amp; = 3U_{n-1} - 3 + 3\\\\
&amp; = 3U_{n-1}
\end{align}
$$
</div>

Then, $U_n = 3^n$, and $T_n = 3^n-1$.

#### Arrangements

As discs must be sorted, to describe an arrangement it is enough to
list the peg for each disc. As there are $3$ pegs, this means
there are $3^n$ different arrangements.

The variation takes $3^n-1$ moves, but counting the starting position
as well, this means $3^n$ different positions, which is the same as
the total number of arrangements.

### Tower of Hanoi, Initial Setup Variation

Once again, by induction: to move a disk to peg $B$:

*Base case*: moving the smallest disc takes at most $1$ move ($0$ if
it is already on peg $B$), so $T_1 \le 1$;
*Recurrence*: to move the disc of size $n$, assuming it is on $A$, we
need to move all the smaller discs to $C$ (to clear both $A$ and $B$),
then move the disc of size $n$, and finally move all the smaller discs
to $B$. Calling the clearing operation $Cl_n$, we have
$T_n \le Cl_{n-1} + 1 + T_{n-1}$.

A moment of thought is enough to realise that $Cl_n$ amounts to the
same operation as $T_n$ (that is, move each disc to a specific peg,
no matter where it currently is), so we have $Cl_n = T_n$, and
therefore $T_n \le 2T_{n-1} + 1$, which is the same recurrence equation
as the original problem.

Therefore there is no position that is more that $2^n-1$ moves from
the target position.

### Venn Diagram with 4 circles

I completely failed to solve this one, even though I spent most of the
time on this problem alone. I had the intuition that it could not be
done; I also found that the maximum number of regions would be 14, but
not matter what I tried, I could not prove it.

I tried to use Geometry, hoping that a minimal list of constraints on
the circles would prove that some of the regions that should be
restricted to two circles were in fact always covered by three or
more.

Eventually, when I gave up and looked at the solution, I still could not
understand it. So a circle can only intersect another one in at most 2
points. OK, so what?

After more research (the Google kind, this time), I found
[this paper](http://www.brynmawr.edu/math/people/anmyers/PAPERS/Venn.pdf)
which explains why. Each intersection point creates a single new
region. Although once again I have no intuition I can trust in this
domain, in this case the reasoning seems similar enough to
intersecting lines that I feel somewhat confident.

So the above observation gives a recurrence equation:

<div markdown="0">
$$
\begin{align}
C_1 &amp;= 2\\\\
C_n &amp;= C_{n-1} + 2(n-1)
\end{align}
$$
</div>

Already, we have that $C_4 = 14$, which is less than the required
$16$ for a Venn diagram (and according to this
[document](http://www.combinatorics.org/Surveys/ds5/VennEJC.html)),
four circles form a _Euler diagram_, not a Venn diagram.

Clearly a triangular number sequence is hiding in there. The
recurrence equations above can be rewritten as

<div markdown="0">
$$
\begin{align}
C_n &amp;= 2+\sum_{i=1}^{n}2(i-1)\\\\
&amp;= 2+2\sum_{i=0}^{n-1}i\\\\
&amp;= 2+2\frac{n(n-1)}{2}\\\\
&amp;= n^2-n+2
\end{align}
$$
</div>

### Bounded Regions in the Plane

Another one where my intuition for Geometry completely failed me. I
had a correct start, identifying that each new line intersecting the
existing ones at $k$ points could at best create $k-1$ new bounded
regions, but when I try to check this I fumbled.

Yet the reason it simple: a line intersecting 2 others will either
define a bounded triangle, or cut an existing bounded region in two.

The new bounded regions are not made of arbitrary triple of lines, but
are next to each others in the plane; really this is similar to the
fence problem. So a line cutting $k$ other lines will create at best
$k-1$ new bounded region. The equality is achieved if there are no
parallel lines, and all the intersection points are distinct.

As the book observes, each new line will also add two new unbounded
regions (the original problem had that a new line would create
$k+1$ new regions).

Once again, the triangular number sequence is not far:

<div markdown="0">
$$
\begin{align}
B_i &amp; = 0 &amp;&amp\text{for $1 \le i \lt 3$}\\\\
B_3 &amp; = 1\\\\
B_n &amp; = B_{n-1} + n - 2\\\\
&amp; = \sum_{i=2}^{n} i-2\\\\
&amp; = \sum_{i=0}^{n-2} i\\\\
&amp; = \frac{(n-1)(n-2)}{2}\\\\
&amp; = S_{n-2}
\end{align}
$$
</div>

### Invalid Recurrence

The recurrence for $H$ has a number of problems. The one I found is
that it only establishes the induction hypothesis for going from an
even number to an odd one; nothing can be said for going from an odd
number to an even one (and indeed, the hypothesis breaks then).

As the book mentions, another problem is the base case, which is
incompatible with the induction hypothesis.

### Wrapping up

I spent way too much time on these exercises, but most of it was on
exercises with a geometric nature: I could not find an algebraic
description of these problems that would be suitable for the kind of
treatment this chapter is about. But once I had the equations, I was
able to solve the problems without trouble.

Next, the homework exercises.
