---
layout: post
title: "Psychic Modeling (fast version)"
date: 2012-02-13 20:23
comments: true
categories: [Books, Algorithms]
tags: [Algorithm Design Manual, algorithms]
series: "Algorithm Design Manual"
js: [math]
---
In [Psychic Modeling](/blog/2012/02/10/psychic-modeling/), I described
a reasonably understandable implementation of a ticket generator for
the
[Psychic Modeling Problem](http://www8.cs.umu.se/kurser/TDBAfl/VT06/algorithms/BOOK/BOOK/NODE19.HTM). While
this version is not overly slow, it is not amazingly fast either.

As I'm refreshing my C skills, I thought it would be interesting to
try and implement a version as fast as possible.

<!-- more -->

### Design

I represent a subset as bit patterns in a 32-bits integer. This means
I am limited to 32 different values (in other words, $n$ must be no
larger than 32). The upside is that I have extremely fast intersection
(`&`) and union (`|`) operations, among others.

### Memory Management

I use a work memory allocated at the beginning of the search;
additional memory is allocated on the stack (using C99 features), and
the selected tickets are just printed to avoid having to remember
them.

The work memory is large enough to store $1 + \beta$ times a block
large that can hold the complete set of $j$-subsets. The first block
keeps the remaining $j$-subsets, and there's an extra block for each
random ticket: each time (for a total of $\beta$) a random ticket is
generated, the $j$-subsets that are not covered yet is computed for
this ticket; after I have generated $\beta$ tickets, I copy the work
block of the best one over the first one.

I could have use just 3 blocks, a reference, the best so far, and one
for the current random ticket, and copy from the current to the best
each time the current ticket is better. There would be more copy
operations, but perhaps less movement between the cache and the
memory. The current design requires less than 2M, and only one copy
operation per random ticket.

### Non portable features

I am using a few GCC
[built-in](http://gcc.gnu.org/onlinedocs/gcc-4.6.2/gcc/Other-Builtins.html)
bit-level operations (number of bits, index of least significant 1
bit, and count of trailing zeroes);
[Bit Twiddling Hacks](http://www-graphics.stanford.edu/~seander/bithacks.html)
and [Hacker's Delight](http://www.hackersdelight.org/) have portable
alternatives.

I also use `/dev/random` as a source of random numbers; replacing
`dev_random` by `random` would restore portability (but the output
would always be the same, and the random state is reset when the
program starts).

### Performance

So, is it fast?

```
$ time ./psychic 18 10 7 6 > output.txt 

real    0m0.289s
user    0m0.238s
sys     0m0.050s
```

The program found 71 tickets covering all 7-subsets with at least 6
numbers in less than a second. Even when the conditions are not that
good, it remains fast:

```
$ time ./psychic 18 7 7 6 > output.txt 

real    0m5.445s
user    0m5.007s
sys     0m0.430s
```

Here it generated 1077 tickets using the smaller ticket size from
[Younas and Skiena paper](http://www.cs.sunysb.edu/~skiena/papers/lotto.doc);
the paper had a 1080 tickets solution, so my version is effective.

Of course, it would be useless and unfair to compare the speed of this
version against the numbers from the paper; more relevant is the
difference with the Haskell version: while the latter was not meant to
be fast, it is hundreds of times slower. I suppose it would be
interesting to try and make it faster, but I suspect it would be just
as ugly or uglier than the C version. And I like to keep using Haskell as a
design and exploratory tool. 

### Overview of the code

#### `solve`

The main function, `solve`, is more complex than in the Haskell
version. It allocates the work memory, and fills it with `init`. A
first ticket is used in `init` to filter out $j$-subsets.

Then the loop for the other tickets starts. It of course stops when
there are no remaining $j$-subsets.

The subset of remaining numbers is computed with `funion` (fold
union), and the `digits` array prepared to be used in `sample`. It
consists of the individual bits of the number representing the
remaining numbers subset. It is computed by repeatedly isolating the
rightmost 1 bit (with `d & -d`), then clearing this bit (with `d &= d -1`).

A first ticket is randomly generated and its uncovered set
computed. It is also set as the best new ticket (and indeed is the
best so far). Then for the remaining $\beta-1$ new tickets, the
uncovered set is computed as well, and if the new set is smaller than
the best's, the new ticket becomes the best as well.

The best ticket is printed, the main work memory is updated with the
best uncovered set, and if there are any remaining $j$-subsets to
find, we loop.

{% codeblock solve lang:c %}
void solve(UINT n, UINT k, UINT j, UINT l)
{
    UINT r = combi(n, j);
    UINT *work = malloc(sizeof(UINT) * (BETA+1) * r);

    UINT t = first_perm(k);

    show_set(t);

    r = init(r, j, l, t, work);

    while (r) {
        UINT d = funion(r, work);
        int bc = bits_count(d);

        UINT digits[bc];

        int i = 0;

        while (d) {
            digits[i++] = d & -d;
            d &= d - 1;
        }

        UINT best_ticket, best_remaining, best_pos = 0;

        best_ticket = sample(k, bc, digits);
        best_remaining = check_cover(r, l, best_ticket, work, work+r);

        for (UINT i = 1; i < BETA; ++i) {
            UINT new_ticket = sample(k, bc, digits);
            UINT new_remaining = check_cover(r, l, new_ticket,
                                             work, work+((1+i)*r));

            if (new_remaining < best_remaining) {
                best_ticket = new_ticket;
                best_remaining = new_remaining;
                best_pos = i;
            }
        }

        show_set(best_ticket);
        if (best_remaining)
            memcpy(work, work+(best_pos+1)*r, sizeof(UINT) * best_remaining);
        r = best_remaining;
    }

    free(work);
}
{% endcodeblock %}

#### `init`

`init`'s purpose it to avoid wasting a loop over the $j$-subsets by
merging the generation of $j$-subsets with the coverage of a first
permutation (defined as `[1..k]` in `solve`). The returned value is
not size of the not yet covered set of $j$-subsets. 

If all tickets had to be generated randomly, 0 could be passed instead
of a ticket to keep all $j$-subsets.

{% codeblock init lang:c %}
UINT init(UINT c, UINT n, UINT l, UINT k, UINT w[])
{
    UINT i = 0;
    UINT v = first_perm(n);

    while (c--) {
        if (bits_count(v & k) < l)
            w[i++] = v;
        v = next_perm(v);
    }

    return i;
}
{% endcodeblock %}

#### `check_cover`

`check_cover` has a similar design as `init`, but reads the
$j$-subsets from the work memory `from` instead of generating them.

{% codeblock check_cover lang:c %}
UINT check_cover(UINT r, UINT l, UINT t, UINT from[], UINT to[])
{
    UINT i = 0;

    for (UINT j = 0; j < r; ++j)
        if (bits_count(from[j] & t) < l)
            to[i++] = from[j];

    return i;
}
{% endcodeblock %}

#### `sample`

`sample` is very similar to the Hashell version (indeed they are both
based on the same algorithm); here the `digits` array plays the role
that `ds` played in the Haskell version.

{% codeblock sample lang:c %}
UINT sample(UINT k, UINT n, UINT ds[]) {
    if (k == 0)
        return 0;
    if (n == 0)
        return 0;

    UINT s = sample(k-1, n-1, ds);
    UINT p = ds[randomR(n)];
    if (p & s)
        return ds[n-1] | s;
    else
        return p | s;
}
{% endcodeblock %}

#### `next_perm`
The `next_perm` is from [Bit Twiddling Hacks](http://graphics.stanford.edu/~seander/bithacks.html#NextBitPermutation), and explained [here](http://www.alexbowe.com/generating-binary-permutations-in-popcount-or).

{% codeblock next_perm lang:c %}
UINT next_perm(UINT v)
{
    UINT t = v | (v - 1);
    return (t + 1) | (((~t & -~t) - 1) >> (__builtin_ctz(v) + 1));
}
{% endcodeblock %}

#### Compiling and running

Using `gcc`, the necessary option is `-std=c99` to activate C99
support; `-O3` gives much (really) better performance, while `-Wall`
is in general a good idea:

```
$ gcc-4.6.2 -Wall -O3 -std=c99 psychic.c -o psychic
```

To run it, just pass the $n$, $k$, $j$ and $l$ parameters on the
command line. There is no checks, so avoid mistakes. The program
outputs the generated tickets:

```
$ ./psychic 5 3 3 2
1, 2, 3
1, 4, 5
```

### Wrapping up

After I completed the Haskell version, I found it not overly difficult
to implement the C one. I was lucky to have discovered Bit Twiddling
Hacks the week before; the code fragments there were very helpful in
writing efficient set oriented functions over words.

Surprisingly, I had just one bug to track (I was using a variable both
as parameter and temporary storage in one of the function); that was
lucky as I'm not sure I could have debugged such code.

### Complete Code

{% include_code Psychic Modeling Fast Version lang:c algo-design-manual/psychic.c %}
