---
layout: post
title: "Machine Learning in Action - Naïve Bayes"
date: 2012-04-09 13:51
comments: true
categories: [Books, Computer, Mathematics]
tags: [Machine Learning in Action, math]
js: [math]
---
I am currently reading
[Machine Learning in Action](http://www.manning.com/pharrington/), as
I need something light between sessions with
[Concrete Mathematics](http://www-cs-faculty.stanford.edu/~uno/gkp.html). This
book introduces a number of important machine learning algorithms,
each time with a complete implementation and one or more test data sets; it also
explains the underlying mathematics, and provides information about
additional reference material (mostly heavier and more expensive books).

However, in Chapter 4 about Naïve Bayes classifiers, I didn't see how
the implementation derived by the maths. Eventually, I confirm that it
could not, and try to correct it.
<!-- more -->

It is of course possible that the implementation is eventually
correct, and derives from more advanced theoretical concepts or
practical concerns, but the book mentions neither; on the other hands,
I found papers
([here](http://trevorstone.org/school/spamfiltering.pdf) or
[here](http://www.cs.cmu.edu/%7Etom/mlbook/NBayesLogReg.pdf)\) that
seem to confirm my corrections.

Everything that follows assumes the book's implementation was
wrong. Humble and groveling apologies to the author if it was not.

## What exactly is the model

The book introduces the concept of conditional probability using balls
in buckets. This makes the explanation clearer, but this is just one
possible model; each model (or
[distribution](http://en.wikipedia.org/wiki/Probability_distribution)\)
uses dedicated formulas.

The problem is that the book then uses set of words or bags of words
as it these were the same underlying model, which they are not.

### Set of words

If we are only interested in whether a given word is present in a
message or not, then the correct model is that of a biased coin where
tails indicate the absence of the word, and heads its presence.

This is also known as a
[Bernoulli trial](http://en.wikipedia.org/wiki/Bernoulli_trial),
and the estimator for the probability of presence is the mean
presence: the number of documents in which the word is present,
divided by the total number of documents.

The book algorithm does not implement this model correctly, as its
numerator is the count of documents in which the word is present
(correct), but the denominator is the total number of words
(incorrect).

### Bag of words

If we want to consider the number of times a word is present in
messages, then the balls in buckets model is correct (it is a
also known as
[Categorical distribution](http://en.wikipedia.org/wiki/Categorical_distribution)),
and the code in the book adequately implements it.

## There is a word for it: Additive Smoothing

The book then improves the algorithm in two different ways. One is the
use of logarithms to prevent underflow. The other is to always use one
as the basic count for words, whether they are present or not.

This is in fact not so much a trick as a concept called
[Additive smoothing](http://en.wikipedia.org/wiki/Additive_smoothing),
where a basic estimator $\theta_i = \frac{w_i}{N}$ is replaced by 
$\hat{\theta}_i = \frac{w_i + \alpha}{N + \alpha d}$ 

$\alpha$ is a so-called smoothing parameter, and $d$ is the total
number of words.

If the model is Bernoulli trial, $w_i$ is the number of documents
where word $i$ is present, and $N$ is the total number of documents.

If the model is categorical distribution, $w_i$ is the total count of
word $i$ is the documents and $N$ is the total count of words in the documents.

As we are interested in $P(w_i|C_j)$ (with $C_0, C_1$ the two
classes we are building a classifier for), $N$ above is restricted to
documents in the relevant class; $\alpha$ and $d$ are independent of
classes.

So the correct formula becomes

<div markdown="0">
\begin{align}
\hat{\theta}_{i,j} = \frac{x_i,j+\alpha}{N_j+\alpha d}\\\\
\end{align}
</div>

With $\alpha=1$ as a smoothing parameter, the book should have used
`numWords` instead of `2.0` as an initial value for both `p0Denom` and
`p1Denom`.

## Putting it together

The differences with the code from the book are minor: first I
introduce a flag to indicates whether I'm using set of words
(Bernoulli trials)  or bags of words (categorical distribution) as a
model. Then I initialise `p0Denom` and `p1Denom` with `numWords` as
explained above; finally I check the `bag` flag to know what to add to
either denominators.

{% codeblock new trainingNB0 lang:python %}
def trainNB0(trainMatrix, trainCategory, bag=False):
    numTrainDocs = len(trainMatrix)
    numWords = len(trainMatrix[0])
    pAbusive = sum(trainCategory)/float(numTrainDocs)
    p0Num = ones(numWords); p1Num = ones(numWords)
    p0Denom = numWords; p1Denom = numWords
    for i in range(numTrainDocs):
        if trainCategory[i] == 1:
            p1Num += trainMatrix[i]
            if bag:
                p1Denom += sum(trainMatrix[i])
            else:
                p1Denom += 1
        else:
            p0Num += trainMatrix[i]
            if bag:
                p0Denom += sum(trainMatrix[i])
            else:
                p0Denom += 1
    p1Vect = log(p1Num/(p1Denom+numWords))
    p0Vect = log(p0Num/(p0Denom+numWords))
    return p0Vect, p1Vect, pAbusive
{% endcodeblock %}

## Evaluation

For the Spam test, the book version has an average error of 6%. The
rewritten version has an error between 3% and 4%. The Spam test uses
messages as set, for which my version is the most different.

For the New-York/San Francisco messages classification, I did not
measure any difference in error rates; this test uses messages as
bags, for which the book version was mostly correct (the only
difference was in the denominators).

## So what?

OK, well, but the book algorithm still works, at least on the original
data.

But how well exactly would it work with other data? As the algorithm
does not seem to implement any kind of sound model, is there any way
to quantify the error we can expect? By building on theoretical
foundations, at least we can quantify the outcome, and rely on the
work of all the brilliant minds who improved that theory.

Theories (the scientific kind, not the hunch kind) provide well
studied abstractions. There are always cases where they do not apply,
and other cases where they do, but only partially or imperfectly. This
should be expected as abstractions ignore part of the real world
problem to make it tractable.

Using a specific theory to address a problem is very much similar to
looking for lost keys under a lamppost: maybe the keys are not there,
but that's where the light is brightest, so there is little chance to
find them anywhere else anyway.

## A bad book then?

So far, this was the only chapter where I had anything bad to
say about the book. And even then, it was not that bad. 

The rest of the book is very good; the underlying concepts are well
explained (indeed, that's how I found the problem in the first place),
there is always data to play with, and the choice of language and
libraries ([Python](http://www.python.org/),
[Numpy](http://numpy.scipy.org/) and
[matplotlib](http://matplotlib.sourceforge.net/)\) is very well
suited to the kind of exploratory programming that makes learning
much easier.

So I would recommend this book as an introduction to this subject, and
I'm certainly glad I bought it.
