#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef unsigned int UINT;
#define BETA 100

#define first_perm(n) ((1 << n) - 1);

/* Mac OS X/Linux specific */
FILE *drand;

UINT def_random() {
    UINT r;
    fread(&r, sizeof(UINT), 1, drand);
    return r;
}

#define _random() def_random()

/* GCC specific definitions */
#define bits_count(v) __builtin_popcount(v)
#define least_one(v) __builtin_ffs(v)

/* http://www-graphics.stanford.edu/~seander/bithacks.html#NextBitPermutation */
/* return next lexicographic permutation */
UINT next_perm(UINT v)
{
    UINT t = v | (v - 1);
    return (t + 1) | (((~t & -~t) - 1) >> (__builtin_ctz(v) + 1));
}

/* following
   http://mikeash.com/pyblog/friday-qa-2011-03-18-random-numbers.html */
/*
 * return a random UINT 0 <= r < n
 */
UINT randomR(UINT n)
{
    UINT two31 = 1U << 31;
    UINT max = (two31 / n) * n;

    while (true) {
        UINT r = _random();
        if (r < max)
            return r % n;
    }
}

/* http://rosettacode.org/wiki/Evaluate_binomial_coefficients#C */
/*
 * binomial coefficient
 */
UINT combi(UINT n, UINT k)
{
    UINT r = 1;
    UINT d = n - k;

    if (d > k) {
        k = d;
        d = n - k;
    }

    while (n > k) {
        r *= n--;
        while (d && !(r % d))
            r /= d--;
    }

    return r;
}

/*
 * select k digits from ds. n is length of ds
 */
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

/*
 * displays subset expressed as bit positions
 */
void show_set(UINT v)
{
    bool f = true;
    while (v) {
        if (!f)
            printf(", ");
        printf("%d", least_one(v));
        f = false;
        v &= v - 1;
    }

    printf("\n");
}

/*
 * union of all subsets
 */
UINT funion(UINT c, UINT w[])
{
    UINT r = 0;

    for (UINT j = 0; j < c; ++j)
        r |= w[j];

    return r;
}

/*
 * init work memory with subsets that are far from
 * initial ticket (defined as first_perm)
 */
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

/*
 * copies subsets that are far from passed ticket
 */
UINT check_cover(UINT r, UINT l, UINT t, UINT from[], UINT to[])
{
    UINT i = 0;

    for (UINT j = 0; j < r; ++j)
        if (bits_count(from[j] & t) < l)
            to[i++] = from[j];

    return i;
}

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

int main(int argc, char *argv[])
{
    UINT n, k, j, l;

    sscanf(argv[1], "%u", &n);
    sscanf(argv[2], "%u", &k);
    sscanf(argv[3], "%u", &j);
    sscanf(argv[4], "%u", &l);

    drand = fopen("/dev/random", "r");

    solve(n, k, j, l);

    fclose(drand);

    return 0;
}
