---
layout: post
title: "Seven Databases in Seven Weeks CouchDB Day 3"
date: 2012-02-01 18:06
comments: true
categories: [Books]
tags: [7databases7weeks, databases, couchdb]
series: "Seven Databases in Seven Weeks"
---
Today is a bit juicier than the previous days (together). On the menu,
advanced views (full MapReduce), replication, conflict management, and
change monitoring.

<!-- more -->

### Advanced views

[Advanced views](http://wiki.apache.org/couchdb/Introduction_to_CouchDB_views)
in CouchDB are, as noted yesterday, materialized output of MapReduce
computations.

This has a cost: such computations are saved, so they take more time
than with other implementations, the first time at least.

Updating the views, on the other hand, is fairly fast (CouchDB
recomputes only what is necessary). Views have to be planned, but
once there they are fairly cheap. For exploratory queries, other databases
might be more appropriate.

CouchDB's reduce functions distinguishes between the first invocation,
and the following ones (on values that have already gone through the
reduce function). This makes it possible to implement a `_count`
function which counts the number of values (the first invocation
transforms values into numbers, and the following ones add the numbers
up).

### Replication

[Replication](http://wiki.apache.org/couchdb/Replication) is the
one-way process of replicating the changes of one database on
another. Replication can be between any two databases, whether on the
same server or on different ones. It can be one time, or
continuous. The documents to replicate can be filtered, or selected by
`_id`.

Replication is a lower level mechanism than what MongoDB, for
instance, proposes (where there is a strict hierarchy of masters and
slaves), and closer to the flexible approach or Riak.

Of course, when concurrent writes are permitted, conflicts can occur,
and CouchDB handles them.

### Conflicts

[Concurrent updates](http://wiki.apache.org/couchdb/Replication_and_conflicts)
can cause conflicts, and CouchDB detects them so they can be dealt
with.

First, conflicts cannot happen on a single server: updates to a
document must refer to the latest revision, otherwise the update
fails. So clients are directly aware that they need to resubmit the
(merged) document.

When replication is enabled, conflicts result from concurrent updates
in two replicated databases. At the next replication, one version will
be selected as winning, and replicated to other databases. The other
versions are still accessible from the `_conflicts` attribute
(initially, only in the losing databases).

If two ways replications are in place, eventually, all databases will
have the `_conflicts` attribute populated (with all the losing
revisions, if there are more than one).

This makes it possible to implement a remedial action; it is possible
to have views with only documents in conflicts, or to filter changes
for conflicts, and implement merging actions in monitoring scripts.

CouchDB documentation helpfully provides some
[advice](http://wiki.apache.org/couchdb/How_to_design_for_replication)
for designing conflict-aware applications.

### Changes

Changes are dedicated views that contains a list of updates for a
specific database. The
[parameters](http://wiki.apache.org/couchdb/HTTP_database_API#Changes)
support starting at a given revision (in this case, a database
revision, not a document revision), filtering documents, and keeping
the stream open in several ways.

This makes it possible (easy, even) to monitor (interesting or
relevant) changes, to synchronize with other systems, or to
automatically resolve conflicts, for instance.

When using Long-Polling, I found that one very large datasets, the
`JSON.parse` invocation could take a long time, and would suggest to
always use a `limit` parameter on the query, to cut the dataset down
to manageable chunks.

## Exercises

### Built-in Reduce Functions

There are three, documented on the
[Wiki](http://wiki.apache.org/couchdb/Built-In_Reduce_Functions).

They are implemented directly in Erlang, so they have a better
performance than JavaScript functions.

#### `_sum`

This function behaves just as the reduce function from the book; it
sums the values by key. It is useful when the map functions uses
`emit(key, 1);` (or some other numeric value).

#### `_count`

It is similar to `_sum`, but it counts the number of values rather
than merely summing them. It is useful when the value is not a number.

#### `_stat`

This is an extension of `_sum` which computes additional statistics
(minimum, maximum, ...) on the numeric values.

### Filtering `_changes` output

Filters are nicely described in
[CouchDB The Definitive Guide](http://guide.couchdb.org/draft/notifications.html#filters).

To create a new filter, I first create a design document to store the function:

```
$ curl -X PUT http://localhost:5984/music/_design/filters \
-d '{ "filters": { "by_country": "function(doc, req) {
return doc.country == req.query.country; }" } }'
```

The `by_country` function retrieves a `country` parameter from the
request, and compares it against the record `country` attribute; only
the matching records are returned.

To monitor only updates to bands from Spain, for instance, I can use

```
curl http://localhost:5984/music/_changes?filter=filters/by_country\&country=ESP
```

To monitor for conflicts, I have the following design document:

{% codeblock lang:js %}
{
   "_id": "_design/filters",
   "_rev": "3-ec032384bf365d3caef0ed91185ae45a",
   "filters": {
       "by_country": "function(doc, req) { return doc.country == req.query.country; }",
       "conflicts": "function(doc, req) { return doc._conflicts; }"
   }
}
{% endcodeblock %}

With that, I can then listen for changes, keeping only the conflicts:

```
$ curl http://localhost:5984/music-repl/_changes?filter=filters/conflicts\&since=26000
{"results":[
{"seq":26994,"id":"theconflicts","changes":[{"rev":"2-cab47bf4444a20d6a2d2204330fdce2a"}]}
],
"last_seq":27000}
```

Because CouchDB only set the `_conflicts` attribute on the
losing database; the winner database (the one in which the winning
revision was initially created) does not know about conflicts. This
means I must check against `music-repl` instead of `music`.

### Replication HTTP API

The API is documented
[here](http://www.couchbase.org/sites/default/files/uploads/all/documentation/couchbase-api-misc.html#couchbase-api-misc_replicate_post).

To use it, simply pass the `source` and `target` databases to the
`_replicate` URL:

```
$ curl -X POST http://localhost:5984/_replicate \
-H 'Content-Type: application/json' \
-H 'Accept: application/json' -d \
'{ "source" : "music", "target" : "music-repl" }'
```

### `_replicator` database

The
[`_replicator` database](http://docs.couchbase.org/couchdb-release-1.1/couchb-release-1.1-replicatordb.html)
is an alternative to the use of the
`_replicate` URL above: documents inserted in the `_replicator`
database will, if properly formed, cause a replication job to be
started (either one-off, or continuous).

Deleting the document will cancel the replication job.

Document describing replications are updated to reflect the progress
of the job.

The command below triggers a replication from `music` to `music-repl`:

```
$ curl -X PUT http://localhost:5984/_replicator/music-rep \
-H 'Content-type: application/json' \
-d '{ "source" : "music", "target" : "music-repl" }'
{"ok":true,"id":"music-rep","rev":"1-ba761c16b5ca36848b2474758cbc4b22"}
```

Using the `watch_changes_longpolling_impl.js` script on the `_replicator`
database, it is possible to monitor the replication job:

```
$ node watch_changes_longpolling_impl.js _replicator
... elided ...
{ seq: 2,
  id: 'music-rep',
  changes: [ { rev: '1-ba761c16b5ca36848b2474758cbc4b22' } ],
  doc: 
   { _id: 'music-rep',
     _rev: '1-ba761c16b5ca36848b2474758cbc4b22',
     source: 'music',
     target: 'music-repl' } }
{ seq: 3,
  id: 'music-rep',
  changes: [ { rev: '2-d1b4fc9da1ef17d43fa91dd7b345a9e6' } ],
  doc: 
   { _id: 'music-rep',
     _rev: '2-d1b4fc9da1ef17d43fa91dd7b345a9e6',
     source: 'music',
     target: 'music-repl',
     _replication_state: 'triggered',
     _replication_state_time: '2012-02-02T10:23:44+09:00',
     _replication_id: 'ab65eb4c4ca880bf65e02626573ef683' } }
{ seq: 4,
  id: 'music-rep',
  changes: [ { rev: '3-b6d32c3ce979af8dc2190735aa39d4f3' } ],
  doc: 
   { _id: 'music-rep',
     _rev: '3-b6d32c3ce979af8dc2190735aa39d4f3',
     source: 'music',
     target: 'music-repl',
     _replication_state: 'completed',
     _replication_state_time: '2012-02-02T10:23:46+09:00',
     _replication_id: 'ab65eb4c4ca880bf65e02626573ef683' } }
... elided ...
```

The first change is when the document is created; the second when the
job starts, and the third when it successfully completes.

Unlike the `_replicate` based API, continuous jobs stored in
`_replicator` will resume when the database is restarted.

### Continuous watcher skeleton

The approach is to keep input in a buffer, then extract as many line
from the buffer as possible (if the last line is incomplete, it is put
back into the buffer), and parse each line as a JSON object.

The format of each parsed object is different: each change is in its
own object, so there is no `results` attribute any more.

{% codeblock watch_changes_continuous.js lang:js %}
var http_options =
    {
        host: watcher.host,
        port: watcher.port,
        path: '/' + watcher.db + '/_changes' +
            '?feed=continuous&include_docs=true&since=' + watcher.last_seq
    };

var processLine = function(line) {
    if (line.length > 0) {
        var output = JSON.parse(line);
        if (output) {
            // don't emit last_seq
            // watcher.last_seq not used in this code
            if (output.last_seq)
                watcher.last_seq = output.last_seq;
            else
                watcher.emit('change', output);
        } else {
            watcher.emit('error', line);
        }
    }
}

var checkForData = function(buffer) {
    lines = buffer.split("\n");
    // if the last character is line return
    // use the last line; otherwise put it back
    // into the buffer to be completed later
    if (buffer.charAt(buffer.length-1) == '\n')
        buffer = ""
    else
        buffer = lines.pop()
    // process the remaining lines one at a time
    lines.forEach(processLine);
    return buffer;
};

http.get(http_options, function(res) {
    var buffer = '';
    res.on('data', function (chunk) {
        buffer += chunk;
        buffer = checkForData(buffer);
    });
    res.on('end', function() {
        checkForData(buffer);
    })
})
    .on('error', function(err) {
        watcher.emit('error', err);
    });
{% endcodeblock %}

### Continuous watcher implementation

I just inserted the code block above in the original
`watch_changes_skeleton.js`; no other modifications were required.

With the code block above, both the long polling and the continuous
outputs are identical.

### Conflicts view

As I said above, conflicts are only created in the losing database, so
to test this I must use the `music-repl` database.

Otherwise, the code is simple: iterate on the `_conflicts` attribute,
and for each revision it contains, emit that revision mapped to the
document `_id`:

{% codeblock lang:js %}
{
   "_id": "_design/conflicts",
   "_rev": "4-1f5c35d83a4cfc7783d60f665946dc6d",
   "language": "javascript",
   "views": {
       "conflicts": {
           "map": "function(doc) { (doc._conflicts || []).forEach(function(rev) { emit(rev, doc._id); }); }"
       }
   }
}
{% endcodeblock %}

Testing it:

```
$ curl http://localhost:5984/music-repl/_design/conflicts/_view/conflicts | python -mjson.tool
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   129    0   129    0     0  37478      0 --:--:-- --:--:-- --:--:-- 64500
{
    "offset": 0, 
    "rows": [
        {
            "id": "theconflicts", 
            "key": "2-0c969fbfa76eb7fcdf6412ef219fcac5", 
            "value": "theconflicts"
        }
    ], 
    "total_rows": 1
}
```

And this completes Day 3 and this overview of CouchDB.
