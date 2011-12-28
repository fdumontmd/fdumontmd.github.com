---
layout: post
title: "Seven Databases in Seven Weeks Neo4j Day 1"
date: 2011-12-28 17:05
comments: true
categories: [Books]
tags: [7databases7weeks, databases, neo4j]
series: "Seven Databases in Seven Weeks"
---
As the book is still in beta and incomplete, I skip CouchDB (the
chapter is not there yet in beta 2.0), and will spend this week with
[Neo4j](http://neo4j.org/).

Neo4j is a graph database, meaning it focuses on navigation between
vertices (called nodes in Neo4j), through edges (called
relationships). While other databases made it possible to join various
pieces of data, Neo4j treats this as the main semantic mechanism

<!--more-->

Neo4j can be distributed for high-availability, and is partition
tolerant, but sharding is not supported (at the time of writing).

The first day focuses on basic creation and navigation of data. [Nodes](http://docs.neo4j.org/chunked/stable/graphdb-neo4j-nodes.html)
and
[relationships](http://docs.neo4j.org/chunked/stable/graphdb-neo4j-relationships.html)
are the basic entities; by default nodes have just an id, while
relationships are identified by the out and in nodes, and a type.

To spice this up a bit, it is possible to attach
[properties](http://docs.neo4j.org/chunked/stable/graphdb-neo4j-properties.html)
to both nodes and relationships. Values can be scalar or arrays of
basic types (boolean, number, or string).

To navigate the data, the easiest seems to be the use of
[Gremlin](https://github.com/tinkerpop/gremlin/wiki), a language and
database independent graph traversal language (the language has to be
a JVM one).

## Exercises

### Neo4j Wiki

The Wiki is [here](http://wiki.neo4j.org/content/Main_Page).

### Gremlin Documentation

There is a [wiki](https://github.com/tinkerpop/gremlin/wiki).

### List of Gremlin Steps

They are listed on the
[wiki](https://github.com/tinkerpop/gremlin/wiki/Gremlin-Steps).

### Neo4j Shells

It is hard not to find them, as they're already in the Web Admin
Console. Both
[Cipher](http://docs.neo4j.org/chunked/snapshot/cypher-query-lang.html)
and the
[ReST API](http://docs.neo4j.org/chunked/snapshot/rest-api.html) can
be used directly from the console, although the ReST API is limited
there (for instance the `traverse` operation is not supported). Full
access requires an external client such as `curl`.

### Find all node names with another shell

In Cipher, there is no direct way to use all nodes as a starting
point, so instead I try to find all nodes linked to the first one
through a path that can be empty (i.e. the starting node is also
included). To remove duplicates, I use the
[`DISTINCT`](http://docs.neo4j.org/chunked/snapshot/query-aggregation.html#aggregation-distinct)
function, but it must be applied in the context of an aggregation, so
I have to apply
[`COLLECT`](http://docs.neo4j.org/chunked/snapshot/query-aggregation.html#aggregation-collect)
as well:

```
START n=node(0)
MATCH n-[*0..]-x
RETURN COLLECT(DISTINCT x.name)
```

which produces

```
==> +----------------------------------------------------------------------------------------------------------------------------------------------------------------------+
==> | collect(distinct x.name)                                                                                                                                             |
==> +----------------------------------------------------------------------------------------------------------------------------------------------------------------------+
==> | List(Prancing Wolf Ice Wine 2007, riesling, Prancing Wolf Spatleses 2007, Prancing Wolf Winery, Prancing Wolf Kabinett 2002, Tom, Wine Expert Monthly, Patty, Alice) |
==>
+----------------------------------------------------------------------------------------------------------------------------------------------------------------------+
```

Not exactly as easy as the Gremlin equivalent `g.V.name`.

There is no way to achieve anything similar using the
ReST API, as
its traversal operation only returns full objects (either nodes,
relationships or paths), and not properties.

### Delete all the nodes and edges in your database

Well, the book already showed the powerful `g.clear` Gremlin
command. It should be followed by `g.addVertex()` to get back to the
original state (with just one node).

And that's all for today.
