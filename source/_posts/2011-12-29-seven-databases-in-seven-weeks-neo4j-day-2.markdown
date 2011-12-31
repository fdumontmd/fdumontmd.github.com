---
layout: post
title: "Seven Databases in Seven Weeks Neo4j Day 2"
date: 2011-12-29 09:17
comments: true
categories: [Books]
tags: [7databases7weeks, databases, neo4j]
series: "Seven Databases in Seven Weeks"
---
Today we play further with Neo4j, exploring the ReST API, indexes, and
algorithms in various languages.

The ReST API is always available, although not the easiest thing to
work with. Besides what the book covers, I also learned how to extend
it, and how to bypass it for large loads.

Indexing can be manual, as the book shows, or automatic (although
the
[documentation](http://docs.neo4j.org/chunked/snapshot/auto-indexing.html)
warns this is still an experimental feature).

Finally, the algorithms are mostly provided by an external library,
[JUNG](http://jung.sourceforge.net/), so its use require direct access
to the data, bypassing the server.

<!--more-->

### Creating an index on relationship

As the index is of type `exact`, there is no need to create it first
(although it is
[possible](http://docs.neo4j.org/chunked/snapshot/rest-api-indexes.html#rest-api-create-node-index)). Just
inserting data in the index will do:

```
curl -X POST http://localhost:7474/db/data/index/relationship/published \
-H "Content-Type: application/json" \
-d '{ "uri" : "http://localhost:7474/db/data/relationship/0", \
"key" : "date", "value" : "1916-11-28" }'
```

### About the ReST API

Clearly this is not how one would want to program. I copied the
[`importer.rb`](http://media.pragprog.com/titles/rwdata/code/neo4j/importer.rb)
code from the book (instead of just using a downloaded version), and
ran it for hours before finding a bug in the data to create
indexes... Running it again with this bug fixed made it much faster
(as actors were reused instead of being duplicated).

There is a higher level API, [Neo4j.rb](http://neo4j.rubyforge.org/),
which runs on JRuby (so it does not use the ReST API). It should be
noted that this is not really a driver, but a library to manage a
Neo4j database directly in Ruby. Still, with it, it is possible to
create the database that will be used by a server. There are other
alternatives (the Gremlin console, for instance), but for Ruby it
seems to be one of the most advanced, and is still being improved.

There is also a ReST API wrapper called
[neography](https://github.com/maxdemarzi/neography), but as I'm
trying to save time I'll go with Neo4j.rb.

To use this API you first need to clone the Git repository:

```
git clone git://github.com/andreasronge/neo4j.git
```

In the `neo4j` directory, build then install the gem (making sure
the default Ruby is JRuby):

```
$ gem build neo4j.gemspec
  Successfully built RubyGem
  Name: neo4j
  Version: 1.3.1
  File: neo4j-1.3.1-java.gem
$ gem install neo4j-1.3.1-java.gem
... (lot's of output elided)
```

As I said above, it is possible to use it to feed data into a
database, but it should not be used while the server is running. I
used it to create the movie network, as it was significantly faster
than the book Ruby script.

To do so, I first rewrite the import script to use the `neo4j` gem. I
am also using the
[`Neo4j::Batch::Inserter`](http://neo4j.rubyforge.org/guides/batch_insert.html)
for extra performance; the resulting code is less readable, but not
significantly so. The script is mostly the same size as the original
one, but much easier to understand (if you know the `neo4j` gem).

{% include_code 7d7w/neo4j/importer_driver.rb %}

I first shut down the Neo4j server. I defined a
`NEO4J_HOME` environment variable as the root of the Neo4j instance,
and cleared the content of `$NEO4J_HOME/data/graph.db` with

```
rm -rf $NEO4J_HOME/data/graph.db/*
```

While not strictly necessary, this step helps ensure that the database
is always in a known (i.e. empty) state each time.

Finally I ran the script with

```
jruby importer_driver.rb performance.tsv
```

The whole import took a little above 1 hour on my not really powerful
macBook Air. The original script never finished, even after running a
few hours.

I also found that index creation is the main cost: my first attempt at
loading data did not use indexes at all: the whole file was loaded in
less than 3 minutes (but of course the resulting graph was
unusable). 

The script is not complete; it should certainly handle exceptions and
close the database properly. But for an initial load it does the job.

After it finished, I just restarted the server.

Note: I strongly suggest backing up the `data/graph.db` directory just
after the initial load (and before starting the server). I had a crash
while running the Kevin Bacon queries, and Neo4j unhelpfully lost the
property data file, forcing me to import again...

A data corruption during a read only operation does not inspire
confidence...

### Indexes

Once thing I had not properly understood, and which caused me some
problems as I was trying to learn how to use the driver, is that
all indexes use Lucene. They are either `exact` or `fulltext`, and can
be queried as shown here:

```
$ curl http://localhost:7474/db/data/index/node/
{
  "movies" : {
    "template" : "http://localhost:7474/db/data/index/node/movies/{key}/{value}",
    "provider" : "lucene",
    "type" : "exact"
  },
  "actors" : {
    "template" : "http://localhost:7474/db/data/index/node/actors/{key}/{value}",
    "provider" : "lucene",
    "type" : "exact"
  }
}
```

So the fact that the driver only supports Lucene indexes is not a
limitation. There is nothing else (although presumably there could be).

### Extending Neo4j

As I found on this
[post](http://blog.neo4j.org/2010/12/neo4j-12-m06-is-out-better-rest.html),
it is fairly easy to extend the ReST API with arbitrary
code. Deploying the code is a simple as copying the jar at the right
location.

The
[official documentation](http://docs.neo4j.org/chunked/snapshot/server-plugins.html)
is mostly an updated version of the post above.

I claimed
[yesterday](/blog/2011/12/28/seven-databases-in-seven-weeks-neo4j-day-1/)
that it was impossible to the ReST API directly to list just the names
of all the nodes.

Of course, today I know I could pass a Gremlin expression through
ReST, and get the same result as in the console. But that could be
considered cheating.

The alternative is to use extend the ReST with a plugin, as I show
here.

As always, the use of Maven is recommended. My `pom.xml` loads the
`server-api` for Neo4j:

{% include_code lang:xml 7d7w/neo4j/list_names/pom.xml %}

The Java code is simplified by the use of annotations. The code
returns an iterator that extract the names of the underlying node
iterator:

{% include_code lang:java 7d7w/neo4j/list_names/ListNames.java %}

Finally, it is important to have add a file
`META-INF/services/org.neo4j.server.plugins.ServerPlugin` with the
complete name of the new plugins (in this case, just one):

{% include_code lang:raw 7d7w/neo4j/list_names/org.neo4j.server.plugins.ServerPlugin %}

The jar should be copied to the `plugins` directory of the Neo4j
instance, and Neo4j restarted.

It is possible to test the correct deployment of the plugin using the
ReST API:

```
$ curl  http://localhost:7474/db/data/
{
  "relationship_index" : "http://localhost:7474/db/data/index/relationship",
  "node" : "http://localhost:7474/db/data/node",
  "relationship_types" : "http://localhost:7474/db/data/relationship/types",
  "neo4j_version" : "1.5",
  "batch" : "http://localhost:7474/db/data/batch",
  "extensions_info" : "http://localhost:7474/db/data/ext",
  "node_index" : "http://localhost:7474/db/data/index/node",
  "reference_node" : "http://localhost:7474/db/data/node/0",
  "extensions" : {
    "CypherPlugin" : {
      "execute_query" : "http://localhost:7474/db/data/ext/CypherPlugin/graphdb/execute_query"
    },
    "GremlinPlugin" : {
      "execute_script" : "http://localhost:7474/db/data/ext/GremlinPlugin/graphdb/execute_script"
    },
    "ListNames" : {
      "list_all_names" : "http://localhost:7474/db/data/ext/ListNames/graphdb/list_all_names"
    }
  }
}
```

The query returns the list of each extension, as well as the URL to
call it. Using the `GET` method, the extension is self documenting:

```
$ curl http://localhost:7474/db/data/ext/ListNames/grphdb/list_all_names
{
  "extends" : "graphdb",
  "description" : "List all the node names",
  "name" : "list_all_names",
  "parameters" : [ ]
}
```

Finally, it can be invoked with the `POST` method:

```
$ curl -X POST http://localhost:7474/db/data/ext/ListNames/grphdb/list_all_names
[ "", "actor", "film", "Leif Andrée", "7X - This is Our Kids ", ...
```

## Of course Kevin Bacon

This section is about the code of the book version beta 2.0.

I had trouble to get the code to work in Neo4j 1.5. Here I document
the alternative code I came up with and used.

### Defining steps in Gremlin

I could not get the book code to define the `costars` step to work: it
seems `outV` does not accept a filter expression as argument.

Even with the addition of a dedicated `filter` step, I could not
filter properly. Instead, I started from scratch, using the
[Gremlin wiki](https://github.com/tinkerpop/gremlin/wiki/User-Defined-Steps)
code as a basis:

```
Gremlin.defineStep('costars',
                   [Vertex, Pipe],
                   {_().sideEffect{start = it}.
                       outE('Movie#acted_in').inV.inE('Movie#acted_in').
                       outV.filter{!start.equals(it)}.uniqueObject()})
```

Note the use of `sideEffect` to introduce the variable `start` into
the expression. Not doing this (and instead following the book code),
the filter was not working at all (i.e. the start node was
still part of the result). Also I have a different type for the
relationship (`Movie#acted_in`) as it was generated by Neo4j.rb.

### From Elvis to Kevin Bacon

The `loop` step does not emit intermediate node by default, so while
the query in the book is accepted, it does not return any result
because the actual degree of separation between Elvis and Kevin Bacon
is just 3.

The latest version of Gremlin extends the basic `loop` pattern to emit
intermediate nodes if requested, but this is not possible with the
version embedded in Neo4j 1.5 admin console.

The standalone Gremlin shell version 1.3 is a bit too old (it links
against Neo4j version 1.5.M01, whose database format is not compatible
with version 1.5's format). So I tried the current head of the Git
[repository](https://github.com/tinkerpop/gremlin).

To build it you will need to download half the Internet, so be
patient.

The build command is `mvn install` (the install step will make the
scripts to launch the console).

Once started, you can load the database with:

```
g = new Neo4jGraph('/users/x/neo4j-enterprise-1.5/data/graph.db')
```

The code `costars` step that was working in the console no longer does
in the shell. I had to replace the `uniqueObject()` step with
`dedup()`, and make sure
everything is on a single line:

```
Gremlin.defineStep('costars', [Vertex, Pipe], {_().sideEffect{start = it}.outE('Movie#acted_in').inV.inE('Movie#acted_in').outV.filter{!start.equals(it)}.dedup()})
```

Finally, the command to find nodes by index has to explicitly use the
index:

```
bacon = g.idx('Actor_exact')[['name':'Kevin Bacon']].next()
elvis = g.idx('Actor_exact')[['name':'Elvis Presley']].next()
```

(if you created the data using the original import command, the index
name is `actors`).

As frustrating as it all is, the end result is that the `loop` step
can now be used as needed:

```
elvis.costars.loop(1){ it.loops < 4}{true}.filter{it.equals(bacon)}.paths.next().name.grep{it}
```

I also had to change the query once more to use `next` instead of `>> 1` as
in the book, as that does not work in the latest version of Gremlin
either.

### Random walk

Once again, I had to change the code from the book to get it to work:
adding a dedicated `filter` step did the trick:

```
bacon.outE.filter{ rand.nextDouble() <= 0.01 }.inV.inE.outV.loop(5){ it.loops < 3 }.count()
```

The `loop` argument does not change, as the filter expression already
counted as a step in the book version.

### Centrality

I had a small problem with the book code: the query is not run if the
command is followed by `; ''` (which the book uses to prevent the
display of the results). Just running this:

```
role_count = [:]; count = 0
g.V.in.groupCount(role_count).loop(2){ count++ < 1000 }
role_count.sort{a,b -> a.value <=> b.value}
```

works. Why on earth would such a small change have such an impact is
beyond me. Now I'm scared of Groovy.

### JUNG Algorithms

This time the book code was working as intended, but I found that
there is an even more central actor than Donald Sutherland: Bobby
Vitale...

Exercises
---------

### Neo4j ReST API

The documentation is [here](http://docs.neo4j.org/chunked/stable/rest-api.html).

### Binding or ReST API

See above my useo Neo4j.rb.

### API for the JUNG project

The API is [here](http://jung.sourceforge.net/doc/api/index.html).

### Path-finding as a step

I am using the Gremlin shell, rather than the Neo4j console.

```
Gremlin.defineStep('path_to', [Vertex, Pipe], {Vertex to, Integer max -> _().costars.loop(1){ it.loops < max }{true}.filter{it.equals(to)}})     
```

I used the possibility to pass arguments to the closure to introduce
both the target node and the loop limit as parameters. Otherwise the
code is identical to the one I was using above. With this, the
path from Elvis to Kevin Bacon becomes

```
gremlin> elvis.path_to3(bacon, 4).paths.next().name.grep{it}
==>Elvis Presley    
==>Frankie and Johnny
==>Nathan Lane
==>He Said, She Said
==>Kevin Bacon
```

### A family graph

I used Ruby (with the Neo4j.rb library). The code first defines a
`family` data structure that maps each family member to other members
keyed by their relationships.

The code first iterate over the family members and inserts them; it
then goes over the `family` structure a second time to insert the
relationships.

{% include_code lang:ruby 7d7w/neo4j/family.rb %}

### Run a JUNG algorithm

I tried to run a simple Dijkstra shortest path algorithm in Gremlin, but
eventually had to give up as the shell kept giving me weird exceptions
when I tried to load the required class. Furthermore, the graph being
directed from the actor nodes to the movie nodes, there is not path
between anything but an actor and one of its movies (and the JUNG
class to transform a directed graph to an undirected one seems to
convert the whole graph eagerly).

Eventually I gave up, dumped the movie database, and used the family
graph instead.

The code is in Java, the language I used after Groovy scared me with
these weird exceptions (Java is boring but predictable).

The hardest perhaps was to figure out the dependencies for the
`pom.xml`:

{% include_code lang:xml 7d7w/neo4j/graph_algo/pom.xml %}

The code in Java is verbose; especially I could not find a simple way
to look up nodes in the BluePrints graph, nor could I use the
properties of the Neo4j nodes from the BluePrints vertices...

{% include_code lang:java 7d7w/neo4j/graph_algo/GraphAlgorithm.java %}

Running it produces

```
Distance between Trent and Dave:3.0
Distance between Trent and everybody:
Trent => 0.0
Carol => 1.0
Peggy => 1.0
Alice => 1.0
Bob => 1.0
Walter => 2.0
Eve => 2.0
Dave => 3.0
```

Wrapping Up Day 2
-----------------

I must say that today was a rather frustrating experience. Neo4j
ecosystem is still evolving, but this means that most of the
documentation I came upon was already obsolete. The navigation on the
data was at time very hard to figure out, and the error messages
(really, the underlying Java exception) not helpful.
