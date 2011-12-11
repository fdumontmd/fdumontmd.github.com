---
layout: post
title: "Seven Databases in Seven Weeks Riak Day 3"
date: 2011-12-09 22:32
comments: true
categories: [Books]
tags: [7databases7weeks, databases, riak]
series: "Seven Databases in Seven Weeks"
---
Today we complete the tour of Riak features. First conflict resolution
with vector clocks; then pre and post-commit hooks, and finally
searching Riak data with a [Solr](http://lucene.apache.org/)
compatible interface.

<!--more-->

### Setting up Riak

Over the last few days, I have been trying different ways to get Riak up and running.

Following the book advice, I recommend installing Riak from the
sources. Actually, you can just build it, build the `devrel` target,
and run from the `dev` directory.

On Mac OS X, [Homebrew](http://mxcl.github.com/homebrew/) usually
works for me, but I like my servers to run with their own user, so I
`sudo brew install` the packages. In the case of Riak, this does not
work at all (the files have incorrect ownership and useless permissions).

There is a small bug in Riak 1.0.2 that causes it to return a 500 HTTP
error when the precommit hook fails, instead of the expected 403. The
problem is a spurious variable binding in Erlang. The patch below
fixes this error; it also make it possible to build Riak with Erlang R14B04.

{% include_code riak 1.0.2 patch lang:text 7d7w/riak/riak-1.0.2.patch %}

Apply it in the Riak 1.0.2 directory: `patch -p1 < ../riak-1.0.2.patch`.

It can also be applied on a repository pulled from [github](https://github.com/basho/riak), but the
latest version has already the patch for Erlang R14B04, so you can
ignore that patch.

### Vector Clocks

[Vector clocks](http://en.wikipedia.org/wiki/Vector_clock) are a
common mechanism to attach a precise time to events occurring
concurrently. By having each event producer keep tracks of the vector
clock of events it is responding to, it becomes possible to identify
sequences and branches in the timing of events (when running
concurrently, time is no longer linear, but can be a graph, or even
worse, a tree, where various participants ignore each other's
responses).

In particular, vector clocks allow a participant to detect conflicts
in the data, and take remedial actions.

Unfortunately, the notion of conflict resolution is not trivial; and
with Riak all or nothing updates, there is almost no information left
to do a merge (with concurrent file modifications, on the other hand,
if the updated areas are different, it might be possible to apply both
changes to the original version, which is what version control systems
typically do).

It does not help that, once again, the example chosen by the authors
to illustrate the concept is poor: the idea that a score given by a
number of judges can simply be averaged when a conflict occurs gives a
rather weird meaning to the score. It would make more sense for each
score to be stored in a different property, and averaged when they are
all present...

It could have been useful to show how more elaborate data (with
multiple properties) can be merge based on the identity of the client
(the book passes a client id but does not use it afterwards, so it
might not be possible to retrieve it).

### Pre and post-commit hooks

Riak allows code to be executed before and after changes on the
database. This is similar to checks and triggers in PostgreSQL, but
the post-commit hooks are more powerful as they can perform pretty
much anything (although I have not explored triggers in other
languages, such as Perl, Python, ... that PostgreSQL supports).

Note: when I tried the example, I had a 500 Internal Server Error
instead of the expected 403 Forbidden return code. I eventually
tracked it down to a bug in the Erlang base code; see my explanations
above to install and patch Riak.

### Indexing and Searching

Unlike what is stated in the first beta of the book, search is a
standard feature in Riak (at least 1.0.2). Just edit the `app.config`
file, look for search, and change the `enable` property to `true`.

Once search is enabled, it is recommended to change the index schema
to declare how to index and search various fields. Otherwise, the
search will not work as the book describes it. In particular, search
for a specific breed will not be case insensitive.

So, after enabling search in each server, I use the command
`dev1/bin/search-cmd install animals` to enable auto-indexing on data
updates (indexes can also be built from files, to the extent that you
have them).

Then I exported the default index with `dev1/bin/search-cmd get-schema
animals` (the output must be piped into a file).

I modified the file to add a declaration for the `breed` field,
following examples from the original
[documentation](http://wiki.basho.com/Riak-Search---Schema.html).

{% include_code animals schema lang:js 7d7w/riak/animals.json %}

Finally, I loaded the schema back into Riak with `dev1/bin/search-cmd set-schema animals animals.json`

Now I can load the data as the book proposes (note that as I'm using
the standard `dev1` Riak server instead of a dedicated one, the port
is 8091 and not 8098).

```
$ curl -X PUT http://127.0.0.1:8091/riak/animals/dragon \
-H "Content-Type: application/json" \
-d '{"nickname" : "Dragon", "breed" : "Briard", "score" : 1 }'
$ curl -X PUT http://127.0.0.1:8091/riak/animals/ace \
-H "Content-Type: application/json" \
-d '{"nickname" : "The Wonder Dog", "breed" : "German Shepherd", "score" : 3 }'
$ curl -X PUT http://127.0.0.1:8091/riak/animals/rtt \
-H "Content-Type: application/json" \
-d '{"nickname" : "Rin Tin Tin", "breed" : "German Shepherd", "score" : 4 }'
```

And now the output of a search is as expected:

```
$ curl http://localhost:8091/solr/animals/select?q=breed:shepherd
<?xml version="1.0" encoding="UTF-8"?>
<response>
  <lst name="responseHeader">
    <int name="status">0</int>
    <int name="QTime">1</int>
    <lst name="params">
      <str name="indent">on</str>
      <str name="start">0</str>
      <str name="q">breed:shepherd</str>
      <str name="q.op">or</str>
      <str name="filter"></str>
      <str name="df">value</str>
      <str name="wt">standard</str>
      <str name="version">1.1</str>
      <str name="rows">2</str>
    </lst>
  </lst>
  <result name="response" numFound="2" start="0" maxScore="0.353553">
    <doc>
      <str name="id">ace
      </str>
      <str name="breed">German Shepherd
      </str>
      <str name="nickname">The Wonder Dog
      </str>
      <str name="score">3
      </str>
    </doc>
    <doc>
      <str name="id">rtt
      </str>
      <str name="breed">German Shepherd
      </str>
      <str name="nickname">Rin Tin Tin
      </str>
      <str name="score">4
      </str>
    </doc>
  </result>
</response>
```

Exercises
---------

### Indexing on score

For this I first modified the index schema again:

{% include_code animals schema improved lang:js 7d7w/riak/animals-score.json %}

Then I reentered the data (see above) to get it indexed.

Finally, I input a query in Firefox, to let it figure out the HTTP
escape characters. The result is used below with curl (both outputs
were identical):

```
curl http://localhost:8091/solr/animals/select?q=score:%5B2%20TO%204%5D
<?xml version="1.0" encoding="UTF-8"?>
<response>
  <lst name="responseHeader">
    <int name="status">0</int>
    <int name="QTime">3</int>
    <lst name="params">
      <str name="indent">on</str>
      <str name="start">0</str>
      <str name="q">score:[2 TO 4]</str>
      <str name="q.op">or</str>
      <str name="filter"></str>
      <str name="df">value</str>
      <str name="wt">standard</str>
      <str name="version">1.1</str>
      <str name="rows">2</str>
    </lst>
  </lst>
  <result name="response" numFound="2" start="0" maxScore="0.00000e+0">
    <doc>
      <str name="id">ace
      </str>
      <str name="breed">German Shepherd
      </str>
      <str name="nickname">The Wonder Dog
      </str>
      <int name="score">3
      </int>
    </doc>
    <doc>
      <str name="id">rtt
      </str>
      <str name="breed">German Shepherd
      </str>
      <str name="nickname">Rin Tin Tin
      </str>
      <int name="score">4
      </int>
    </doc>
  </result>
</response>
```

### Distributed Riak

I don't see much of a problem with this, I'll give it a try when I get
home. There are already 4 different development servers easily
available in a standard Riak package, so using one on each machine
would do the trick. Of course, when adding the various servers to the
ring, the hostname must be changed, but this should really be a piece
of cake.

Wrapping up Riak
----------------

It seems Riak is a low level data store that trades easy of use and
packaged features for high availability and flexible for
performance cost.

I can see how this could be in theory appealing in some circumstances,
but I don't really see how to put such a framework to use.

SQL databases don't just come with a nice engine optimised for
relational queries; they also come with decades of experience, general
guidelines for schema design and domain specific schema organisations.

All these contribute to make the meaning of the data clear and
useful. With Riak the quorum option can be used as a decision
mechanism, but beside it is not obvious what meaning to give to a
piece of information that has two or more concurrent variations. And
of course consistency can no longer be implemented on more than one key
no matter what.

This means that a solution based on Riak will be significantly
different from one using an SQL database. Perhaps it would feel more
natural to an object oriented programmer who thinks in terms of
objects and object references. But even in this context, the vector
clock based resolution should still be difficult to design properly.

The book in this regard fails even to acknowledge the problem; it
concentrates on tools but does not give any framework to guide in the
design of a solution built on Riak.

I still has to check the
[Amazon Dynamo Paper](http://www.allthingsdistributed.com/2007/10/amazons_dynamo.html)
and the other relevant literature, so I have hope yet I will come up with a
reasonable understanding of all this.
