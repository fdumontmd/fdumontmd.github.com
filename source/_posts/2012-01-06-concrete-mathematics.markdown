---
layout: post
title: "Concrete Mathematics"
date: 2012-01-06 13:16
comments: true
categories: [Books, Mathematics]
tags: [concMath, math]
series: "Concrete Mathematics"
short_title: "Intro"
js: [math]
---
Stephen Hawking once said that his editor had warned him that each
equation in his book would halve the readership.

With that in mind, and taking into account the number of readers of
this blog (or lack thereof), would I dare put any equations?

You better believe it!
<!--more-->

I just picked up my old copy of
[Concrete Mathematics](http://en.wikipedia.org/wiki/Concrete_Mathematics),
a book I have too long neglected. The ultimate goal, of course, is
slaying the
[Beast](http://en.wikipedia.org/wiki/The_Art_of_Computer_Programming),
which I should try to complete before Donald E. Knuth passes away.
While I wish him a very long life, long enough at least to complete
[Volume 5](http://en.wikipedia.org/wiki/The_Art_of_Computer_Programming#Volumes),
and better yet 6 and 7, I should not take his remarkable health as an
excuse to dither.

For the math notation, I use [MathJax](http://www.mathjax.org/), a
JavaScript library that can parse either
[MathMl](http://www.w3.org/Math/), or much better
[LaTeX](http://www.latex-project.org/) (which is based on
[TeX](http://www.math.upenn.edu/TeX.html), another gift of Donald
E. Knuth to the world).

The setup for this blog is based on this
[post](http://greglus.com/blog/2011/11/29/integrate-MathJax-LaTeX-and-MathML-Markup-in-Octopress/).

The quality of rendering is variable: pretty good in Firefox, OK in
Safari or Chrome, and no idea in IE or Opera. Of course, it is not as
good as the output of LaTeX, but for the Web it is acceptable.

For instance, given the recurrence
<div markdown="0">
$$
\begin{align}
f(j) &amp; = \alpha_j, &amp;&amp;\text{for $1 \leq j \lt  d$}\\\\
f(dn + j) &amp; = cf(n) + \beta_j, &amp;&amp;\text{for $0 \leq j \lt d$ and $n \geq 1$}
\end{align}
$$
</div>
then the solution is

$$f \left( ( b_m b_{m-1} \cdots b_1 b_0)_d \right) = \left( \alpha_{b_m} \beta_{b_{m-1}} \beta_{b_{m-2}} \cdots \beta_{b_1} \beta_{b_0} \right)_c$$

(refer to the book for explanations).

Isn't this lovely?
