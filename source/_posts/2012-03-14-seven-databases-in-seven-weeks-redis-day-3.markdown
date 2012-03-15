---
layout: post
title: "Seven Databases in Seven Weeks Redis Day 3"
date: 2012-03-14 15:48
comments: true
categories: [Books]
tags: [7databases7weeks, databases, redis, couchdb, neo4j, mongodb]
series: "Seven Databases in Seven Weeks"
---
Wow, almost two months since I wrote
[Day 2](/blog/2012/01/21/seven-databases-in-seven-weeks-redis-day-2/),
and more than one since the last post in this series... Time to bring
it to an end.

<!--more-->

Today is less about Redis (indeed, it is hardly used at all), and more
about a concept: Polyglot Persistence, and about an implementation
that showcases the concept.

In fact, I spent most of my time browsing the documentation of
[Node.js](http://nodejs.org/), the library/framework the authors used
to build the demo application.

## Polyglot Persistence

Polyglot Persistence, the use of several kinds of storage systems in a
project, makes even more sense than Polyglot Programming (the
use of several languages in a project).

While languages are, by and large, equivalent in expressive power, and
mostly a matter of choice, culture, or comparative advantage (some
languages favour small teams, other large ones), storage systems are
sufficiently different that they are not interchangeable.

Once the idea of eventual consistency takes root, it is only a simple
extension to view the data as services available from a number of
sources, each optimised for its intended use (instead of a single,
default source that only partially meets the more specialised needs),
and with its own update cycles.

The problem, of course, is that it introduces several levels of
complexity: development, deployment, monitoring, and a dizzying range
of potential errors, failures, ...

## Polyglot Persistent Service

The implementation described in the book is small enough to fit in
less than 15 pages, yet rich enough to show what is possible.

The databases are (with the versions I used):

 * Redis 2.4.8
 * CouchDB 1.1.1
 * Neo4j Community 1.6.1

and the glue language is Node.js.

### Redis

Redis is used first as initial storage for the first data take-on. It
is then used to track the transfer of data between CouchDB and the
other databases, and finally to support auto-completion of band names.

### CouchDB

CouchDB is intended as the System Of Records (i.e. master database)
for the system. Data is meant to be loaded into CouchDB first, then
propagated to the other databases.

Beside that, it is not used much, and after the exercises, not used at all...

### Neo4j

Neo4j keeps a graph of bands, members, and instruments (or roles), and
their relationships.

### Node.js

Node.js is a framework/library for JavaScript based on the concept of
event-based programming (similar to, but perhaps more radical than,
Erlang). All I/O is done in continuation-passing style, which means
that whenever a I/O operation is initiated, one of the argument is a
function to handle whatever the operation produces (or deal with the
errors).

This is good from a performance point of view, but it is of course
more complex to design and code with. Still, it looks like a fun tool
to glue various servers together.

### Book Code Fixes

I had to fix some of the code from the authors (nothing serious, and
all reported in the [errata](http://pragprog.com/titles/rwdata/errata)):

 * `populate_couch.js`: the `trackLineCount` has an off-by-one
 error. The check for completion should be `totalBands <=
 processedBands`
 * `bands.js`: the initialisation of `membersQuery` in the function
 for the `/band` route has a syntax error. It should be

```
membersQuery = 'g.V[[name:"'+bandName+'"]]' 
             + '.out("member").in("member").uniqueObject.name';
```

### Updating the Code

The book uses a now dated version of Neo4j, so the queries do not
work. The shortcut to access a node by index does not work anymore,
and the `uniqueObject` step has been replaced by `dedup`.

Here are the updated relevant portions:

{% codeblock /band Route lang:javascript %}
membersQuery = 'g.idx("bands")[["name":"'+bandName+'"]]'
             + '.out("member").in("member").dedup.name';
{% endcodeblock %}

and

{% codeblock /artist Route lang:javascript %}
rolesQuery = 'g.idx("artists")[["name":"'+artistName+'"]].out("plays").role.dedup';
bandsQuery = 'g.idx("artists")[["name":"'+artistName+'"]].in("member").name.dedup';
{% endcodeblock %}

## Exercises

I'm not sure what the second homework exercise was supposed to be
about: Neo4j already contains information about members and
memberships. Perhaps it dates from an early draft, before this
chapter's code evolved into what it is now. In any case, the first
exercise had enough Neo4j anyway.

### Adding Band Member's start and end dates

The start and end dates for memberships in bands is sometimes
provided; the purpose of this exercise is to use this information.

#### Pre-populate

I load the start and end dates into their own key in Redis. The key
format are `from:bandName:artistName` and `to:bandName:artistName`.

First I take the data from the relevant columns:
{% codeblock Extracting Data lang:javascript %}
var
  artist = data[2],
  band = data[3],
  roles = buildRoles(data[4]),
  from = data[5],
  to = data[6];
{% endcodeblock %}

Then, if they're not empty, I create the keys in Redis:

{% codeblock Updating Redis lang:javascript %}
if (from != '')
  redis_client.set('from:' + band + ':' + artist, from);
if (to != '')
  redis_client.set('to:' + band + ':' + artist, to);
{% endcodeblock %}

#### CouchDB

Adding the information to CouchDB is not hard; the main difficulty is
to figure out how to modify the `populate_couch.js` script
(continuation-passing style is *hard*).

Eventually, I just reused the `roleBatch` (therefore renamed
`artistInfoBatch`) to retrieve the roles, from and to information.

{% codeblock Retrieving the Information lang:javascript %}
var artistInfoBatch = [];
artists.forEach(function(artistName) {
  artistInfoBatch.push([
    'smembers',
    'artist:' + bandName + ':' + artistName
  ]);
  artistInfoBatch.push([
    'get',
    'from:' + bandName + ':' + artistName
  ]);
  artistInfoBatch.push([
    'get',
    'to:' + bandName + ':' + artistName
  ]);
});
{% endcodeblock %}

The putting it in CouchDB is trivial:

{% codeblock Building Documents lang:javascript %}
artists.forEach( function(artistName) {
  var artist = { name: artistName, role : artistInfo[i++] },
      from = artistInfo[i++],
      to = artistInfo[i++];
  if (from)
    artist['from'] = from;
  if (to)
    artist['to'] = to;
  artistDocs.push(artist);
});
{% endcodeblock %}

#### Neo4j

Neo4j was the hardest piece of the puzzle: I didn't know, and could
not find any definitive documentation on, how to relationship
properties at creation time. Eventually I found that adding them to
the `data` attribute passed at creation time did the trick (although
it still took me more time to understand how to use them).

The problem to do so is that the `neo4j_caching_client.js` library
does not support adding properties to relationships, but it was easy
enough to modify this library to add this feature.

{% codeblock Relationship properties lang:javascript %}
neo4jClient.createRelationship = function(fromNode, toNode, type, callback, props) {
  var fromPath = (fromNode || '').replace(/^.*?\/db\/data\//, ''),
      rel = { to: toNode, type: type };
  if (props)
    rel.data = props;
  neo4jClient.post(
    [fromPath, 'relationships'], rel, callback
  );
}
{% endcodeblock %}

then the relevant properties can be passed to the function above in
the `graph_sync.js` script:

{% codeblock Passing from and to properties lang:javascript %}
var props = {};
progress.emit('progress', 'artist');
if (artist.from)
  props.from = artist.from;
if (artist.to)
  props.to = artist.to;
relate(bandNode.self, artistNode.self, 'member', function(){
  progress.emit('progress', 'member');
}, props);
{% endcodeblock %}

#### Using the new data

To make use of the new data, I tried to differentiate between current
and old members of a band. I simply define a current member as one
whose `to` property is null.

Figuring how to write a Gremlin query that extracted the information I
needed was challenging: the documentation is often sparse, and many
concepts barely explained.

I found that I could collect nodes or relationships along a path by
naming them (with the step `as`), and then gather all of them in a
single row of a
[`Table`](http://docs.neo4j.org/chunked/stable/gremlin-plugin.html#rest-api-returning-nested-pipes).
I used this to get both the `from`, `to`
properties and the artist `name` property in a single query. However,
I spent some time tracking a bug in my filters where apparently, null
`to` would not be returned as current members. I finally realise that
when a given node or relationship is given two different names, these
names will appear in reverse order in the `Table`.

So in my case, the query:

```
g.idx("bands")[["name":"Nine Inch Nails"]].outE("member").as("from").as("to")
.filter{it.to != null}.inV.as("name")
.table(new Table()).{it.to}{it.from}{it.name}.cap()
```

I give the names `from` and `to` to the relationship, but used them in
reverse order in the `Table` closures. Is this the intended behaviour
or a bug? Does anybody know?

It seems like a common problem with some NoSQL databases: the query
language feels very much adhoc, and not entirely sound or fully
thought through. Despite its many defects, SQL was at least based (if
sometimes remotely) on the relational calculus, which gave a precise
meaning to queries. It was further specified in different standards,
so that even its defects were fully clarified (XPath/XQuery is another
pretty well specified query language). When playing with NoSQL
databases that pretend to have a query language, I often find it
difficult to go beyond the simpler examples, precisely because of this
linguistic fuzziness.

But I solved it for this case, so now I have my `Table`. It is an
object with two properties: `columns` is an array of column names, and
`data` is an array of arrays (each one being a row). To convert them
to an array of objects, I use the following code:

{% codeblock Convert Table data lang:javascript %}
function convertGremlinTable(table) {
    return fromTableToObject(table[0][0].columns, table[0][0].data);
}

function fromTableToObject(columns, data) {
    var res = [];
    for (var i = 0; i < data.length; i++) {
        var obj = {};
        for (var j = 0; j < columns.length; j++) {
            if (data[i][j] != 'null')
                obj[columns[j]] = data[i][j];
        }
        res.push(obj);
    }

    return res;
}
{% endcodeblock %}

The rest of the code is just the nested Node.js event functions, and
the formatting using the [`mustache`](http://mustache.github.com/)
(which was pretty cool and easy to use).

#### Full Code

{% include_code 7d7w/redis/day3/from-to/pre_populate.js %}

{% include_code 7d7w/redis/day3/from-to/populate_couch.js %}

{% include_code 7d7w/redis/day3/from-to/graph_sync.js %}

{% include_code 7d7w/redis/day3/from-to/neo4j_caching_client.js %}

{% include_code 7d7w/redis/day3/from-to/bands.js %}

### Add Music Samples

The book (in beta 5.0) suggested to use Riak's Luwak, but this
component has recently been removed, and there seems to be no
replacement at this time. So I went with MongoDB's
[GridFS](http://www.mongodb.org/display/DOCS/GridFS) instead. This is
a little more complex than a simple replacement of the client
libraries: MongoDB does not have an HTTP ReST API for GridFS, so I
need to stream the content of the file through the server.

#### Overview

To keep things simple, I load only on sample per band; the file name
must be the same as the CouchDB key, followed by '.mp3'.

To access MongoDB from Node.js, I use
[node-mongodb-native](http://github.com/christkv/node-mongodb-native),
which can be installed with `npm`. It has all the expected features of
a client, including GridFS support (with one caveat, see below).

To stream the file from the server, I use a dedicated port, for no
better reason than because
[Brick.js](http://bricksjs.com/index.html), that the authors used to
build the service, was giving me trouble, while the standard `http`
module did not.

When displaying the band information, I check whether a file exists
with the same name as the band's key: if it does, I add a link to the
dedicated streaming port, passing the key as parameter:

{% codeblock Adding Sample Link lang:javascript %}
mongoClient.open(function(err, db) {
  mongodb.GridStore.exist(db, bandKey + '.mp3', function(err, exist) {
    if (exist)
      body += '<a href="http://'+host+':'+streamPort+'?band={{bandK}}">Sample</a>';
{% endcodeblock %}

Then, I create a new `http` server to send the music:

{% codeblock Streaming Files from GridFS lang:javascript %}
http.createServer(function(request, response) {
    var band = url.parse(request.url, true).query.band;   
    mongoClient.open(function(err, db) {
        var gs = new mongodb.GridStore(db, band+'.mp3', "r");
        gs.open(function(err, gs) {
            console.log("streaming...");
            response.writeHeader(200, {
                'Content-type': 'audio/mpeg, audio/x-mpeg, audio/x-mpeg-3, audio/mpeg3',
                // magic headers to stream mp3...
                'X-Pad': 'avoid browser bug',
                'Cache-Control': 'no-cache',
                'Content-Length': gs.length});
// cannot use gridstore streams; somehow file always
// truncated - load in memory instead
//            gs.stream(true).pipe(response);
            gs.read(gs.length, function(err, data) {
                response.write(data);
                response.end();
                db.close();
            });
        });
    });
}).listen(streamPort);
{% endcodeblock %}

The only problem I had (but it took me a while to figure it out) was
that the stream support in the MongoDB client for GridFS content is
(as far as I can tell) defective: it will close the stream after just
one or two chunks' worth of data
([Issue in Github](https://github.com/christkv/node-mongodb-native/issues/540)).

So instead I have to load the whole file in memory then write it in
the response... Clearly not the best approach, but hey, it works!

#### Full Code

{% include_code 7d7w/redis/day3/mp3/bands.js %}

## Wrapping Up

Well, that was a long day. I should have enjoyed it, but the lack of
maturity in some of the tools (Neo4j's always evolving query language
and the GridFS streaming bug) caused hours of frustration. The main
cause, however, was missing knowledge: faced with an unexpected
behaviour, I had no idea whether it was a bug (find a workaround) or
an incorrect invocation (rework the query to correct it).

The exposition of polyglot persistence through the music information
service were pretty good, given the space constraint. Of course it
skipped the really ugly and tedious parts (how to incrementally keep
the databases in sync when the main records are updated, not merely
created); given the variation in data models, data manipulation (or
lack thereof) and query between the different databases, this can
easily become a nightmare (especially if incremental updates are not
part of the initial design).

Another upcoming book, [Big Data](http://www.manning.com/marz/), takes
a very different approach (no updates, only appends). I look forward
to reading it.
