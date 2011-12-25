---
layout: post
title: "Seven Databases in Seven Weeks MongoDB Day 2"
date: 2011-12-24 14:49
comments: true
categories: [Books]
tags: [7databases7weeks, databases, mongodb]
series: "Seven Databases in Seven Weeks"
---
Today the book covers all kinds of queries goodness in MongoDB:
indexing, advanced group queries, and MapReduce.

Once again, the contrast with Riak is stark. MongoDB is able to
optimize queries on its JSON documents because it understand the
format directly (whereas it is stored as an opaque block in Riak).
Using JavaScript is also simpler: no need to quote the function codes;
just pass a function object to the commands that need one.

<!--more-->


### Indexes

MongoDB comes by default with fairly sophisticated indexing
options. Perhaps not as many as PostgreSQL, but still very
flexible. Two basic types, range (B-Tree) and geospatial indexes;
multikeys (with the ability to sort each key in a different order);
sparse, ...

Combined with the
[`explain`](http://www.mongodb.org/display/DOCS/Explain) function,
this makes classic (i.e. non MapReduce) queries usable.

Thus MongoDB is a good hybrid between traditional databases (although
document rather than schema oriented), and new MapReduce platforms
such as [`Hadoop`](http://hadoop.apache.org/).

### Aggregation

MongoDB also supports a number of aggregation functions. The most
flexible one,
[`group`](http://www.mongodb.org/display/DOCS/Aggregation#Aggregation-Group),
is not compatible with sharding, but otherwise it provides yet more
coverage of relational database features.

### MapReduce

Using MongoDB's
[`mapreduce`](http://www.mongodb.org/display/DOCS/MapReduce) is much
easier than using Riak`s: the functions do not have to be passed as
strings, they can be stored in the server directly from the shell, and
because MongoDB understand JSON directly, there is not need to first
parse the document

On the other hand, Riak's agnostic approach makes
it possible to MapReduce other kinds of data.

## Exercises

### Shortcut for the admin commands

I could not find a single place with the info. The mongo shell
[API](http://api.mongodb.org/js/current/) has no central list of
functions; instead they are spread in the documentation or source for
each prototype.

In general, an admin command that takes a MongoDB object as a
first argument will have an equivalent method in the relevant prototype.

For instance, the
[`dbStats`](http://www.mongodb.org/display/DOCS/List+of+Database+Commands)
command takes a
[`DB`](http://api.mongodb.org/js/current/symbols/_global_.html#DB);
in the
[`db.js`](http://api.mongodb.org/js/current/symbols/src/shell_db.js.html)
source file of the `DB` prototype, there is a `stats` method that
invokes the `dbStats` command.

### Online documentation for Queries and Cursors

As stated in the
[documentation](http://www.mongodb.org/display/DOCS/Queries+and+Cursors),
MongoDB returns a cursor for each queries; it is up to the client to
iterate over the cursor to retrieve results.

The mongo shell usually hides the existence of cursors, but even there
it is possible to expose them, using JavaScript.

### MongoDB documentation for MapReduce

The documentation is
[here](http://www.mongodb.org/display/DOCS/MapReduce).

### Collection function code

In each case, I got the code by running `db.towns.functionName` (note
the absence of parenthesis). The mongo shell direct access to
JavaScript source code is especially convenient.

#### Collection `help`

The source code is just a long list of `print` statements.

#### Collection `findOne`

The code will first execute a query, returning a cursor. The cursor is
then checked for the presence of results; if there is any, the first
one is returned.

#### Collection `stats`

This function will simply delegate the job to the `runCommand` method,
invoking the `collStats` command.

### Finalize method

The `finalize` function is very simple: rename the attribute `count`
to `total`:

{% codeblock finalize function lang:javascript %}
finalize = function(key, value) {
    return { total: value.count };
}
{% endcodeblock %}

To use it, just add the `finalize` attribute to the `mapReduce` command:

{% codeblock lang:javascript %}
results = db.runCommand({
    mapReduce: 'phones',
    map: map,
    reduce: reduce,
    finalize: finalize,
    out: 'phones.report'
})
{% endcodeblock %}

Finally, I can check  the result with `db.phones.report.find()`:

```
> db.phones.report.find()
{ "_id" : { "digits" : [ 0, 1, 2, 3, 4, 5, 6 ], "country" : 1 }, "value" : { "total" : 35 } }
{ "_id" : { "digits" : [ 0, 1, 2, 3, 4, 5, 6 ], "country" : 2 }, "value" : { "total" : 30 } }
{ "_id" : { "digits" : [ 0, 1, 2, 3, 4, 5, 6 ], "country" : 3 }, "value" : { "total" : 35 } }
{ "_id" : { "digits" : [ 0, 1, 2, 3, 4, 5, 6 ], "country" : 4 }, "value" : { "total" : 22 } }
{ "_id" : { "digits" : [ 0, 1, 2, 3, 4, 5, 6 ], "country" : 5 }, "value" : { "total" : 35 } }
{ "_id" : { "digits" : [ 0, 1, 2, 3, 4, 5, 6 ], "country" : 6 }, "value" : { "total" : 19 } }
{ "_id" : { "digits" : [ 0, 1, 2, 3, 4, 5, 6 ], "country" : 7 }, "value" : { "total" : 32 } }
{ "_id" : { "digits" : [ 0, 1, 2, 3, 4, 5, 6 ], "country" : 8 }, "value" : { "total" : 32 } }
{ "_id" : { "digits" : [ 0, 1, 2, 3, 5 ], "country" : 1 }, "value" : { "total" : 7 } }
{ "_id" : { "digits" : [ 0, 1, 2, 3, 5 ], "country" : 2 }, "value" : { "total" : 5 } }
{ "_id" : { "digits" : [ 0, 1, 2, 3, 5 ], "country" : 3 }, "value" : { "total" : 5 } }
{ "_id" : { "digits" : [ 0, 1, 2, 3, 5 ], "country" : 4 }, "value" : { "total" : 10 } }
{ "_id" : { "digits" : [ 0, 1, 2, 3, 5 ], "country" : 5 }, "value" : { "total" : 6 } }
{ "_id" : { "digits" : [ 0, 1, 2, 3, 5 ], "country" : 6 }, "value" : { "total" : 4 } }
{ "_id" : { "digits" : [ 0, 1, 2, 3, 5 ], "country" : 7 }, "value" : { "total" : 6 } }
{ "_id" : { "digits" : [ 0, 1, 2, 3, 5 ], "country" : 8 }, "value" : { "total" : 5 } }
{ "_id" : { "digits" : [ 0, 1, 2, 3, 5, 6 ], "country" : 1 }, "value" : { "total" : 116 } }
{ "_id" : { "digits" : [ 0, 1, 2, 3, 5, 6 ], "country" : 2 }, "value" : { "total" : 103 } }
{ "_id" : { "digits" : [ 0, 1, 2, 3, 5, 6 ], "country" : 3 }, "value" : { "total" : 118 } }
{ "_id" : { "digits" : [ 0, 1, 2, 3, 5, 6 ], "country" : 4 }, "value" : { "total" : 104 } }
has more
```

### Use of driver

I used Java, and simply reimplemented the original Phones collection
in a different database:

{% include_code lang:java 7d7w/mongo/src/main/java/jp/wakatta/MongoTest.java %}

For the complete project, I just used Maven to fetch the MongoDB
driver:

{% include_code lang:xml 7d7w/mongo/pom.xml %}

Ugly, but it does the job.

And that's all for today.
