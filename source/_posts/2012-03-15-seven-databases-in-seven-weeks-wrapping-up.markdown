---
layout: post
title: "Seven Databases in Seven Weeks Wrapping Up"
date: 2012-03-15 10:57
comments: true
categories: [Books]
tags: [7databases7weeks, databases, redis, couchdb, neo4j, mongodb, riak, postgresql, hbase]
series: "Seven Databases in Seven Weeks"
---
This has lasted a little bit longer than seven weeks (the release
schedule of the beta versions did not help; my day job did not help
either), but finally I finished the book.
<!--more-->

### Pro

I liked that the book started with PostgreSQL. All too often, I am put
of by the amazingly uninformed criticisms of the NoSQL crowd about
relational databases; this left me with the general impression that a
younger generation of engineers was just too ignorant to figure SQL
out, so they build something new (without the benefits of decades of
experience...).

By having a balance approach, the book cleared this misconception
([Hadoop, the Definitive Guide](http://shop.oreilly.com/product/0636920010388.do)
also has a balance coverage in its introduction).

Each database's strengths and weaknesses are correctly (as far as I
can tell) reported, along with its position in the CAP triangle, and
intended or ideal usage.

A recapitulative (but already partially incorrect, at least in the 5.0
beta version) overview of all the databases properties in Appendix A
is also very useful.

### Cons

Well, this is not exactly a problem of the book itself, but rather of
the tools it covers: the rapid and sometimes radical changes in some
of the databases meant that the technical information in the book was
already obsolete.

The book's intention is not to be a detailed tutorial; for instance,
they skip installations (really, most technical books should skip
installation and go straight to setup and use; think of the number of
trees that would save), but the search for corrections was heavily
taxing my already sparse free time.

All this will eventually improve, as the tools and documentation
mature; right now using them is a bit too involved for the broad but
shallow approach this book follows.

Compared to
[Seven Languages in Seven Weeks](http://pragprog.com/book/btlang/seven-languages-in-seven-weeks),
I found this book more challenging. But this is perhaps a consequence
of my prior exposure to a variety of languages and programming
concepts; I suspect many people may find this book much easier.

### Recommendation

Of all the books I have read recently, this is the one that changed
and enlarged my views the most.

If you are, like me, a traditional software engineer with years of
experience in relational databases but little exposure to newer kind
of storage, you will benefit from this presentation of many databases and
solution designs.

If, however, you already come from the NoSQL database and have
experience in a few of the covered tools, this one book might not be
the ideal one to convince you of the strengths of PostgreSQL. The
problem with relational databases is that, having been the defacto
standard storage solutions for decades, nobody remember why they
became popular in the first place (they actually replaced databases
that looked pretty much like document or graph databases, only much
more primitive).

Still, given its price, as a broad introduction to many different
data tools and techniques, this book is hard to beat. I certainly am
glad for having read it, and I think you would be too.
