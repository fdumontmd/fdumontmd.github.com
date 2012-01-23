---
layout: post
title: "Seven Databases in Seven Weeks Redis Day 2"
date: 2012-01-21 13:30
comments: true
categories: [Books]
tags: [7databases7weeks, databases, redis]
series: "Seven Databases in Seven Weeks"
---
Performance tuning with Redis can be achieved in different ways, as we
see today. First there are basic changes in the client side (such as
pipelines), then configurations options (frequency of saves, ...), and
finally distribution of load.

<!-- more -->
### Pipeline

Redis low level protocol supports the notion of pipelines: sending
commands in batch, and collect all the results at the end, instead of
waiting for results between each command. This should save a round
trip delay for each command, so there can be huge performance boosts
for specific usages, as the informal benchmarks below show.

### Distributed Redis

Redis servers can be distributed for performance or memory concern,
but much of the work falls on the client side.

#### Slaves

Slaves in Redis are just the opposite of
[MongoDB](http://www.mongodb.org/)'s. Whereas MongoDB's slaves are
meant to be written to, so that updates are automatically pushed to
the master, Redis slaves are, or should be, read-only. Updates are
only propagated from master to slaves.

There is no integrated support for failover; it has to be implemented
in client code.

So slaves are mainly a mechanism to distribute reads; combined with
monitoring client code, they can also be used to data replication and
failover.

Note that each slave needs as much memory as the master, as it
contains the same data.

#### Sharding

By itself, Redis does not support sharding, and relies on the client
library to spread accesses over several instances. There is a
ongoing development to have real Redis Clusters, but for the time
being it has to be simulated.

One issue not mentioned in the book is that sharding breaks
transactions and pipelines: there is no guarantees that the relevant
keys are all in the same instance, so the Redis Ruby client, for
instance, will raise an exception when invoking `MULTI`.

The Java client, Jedis, has a mechanism to "tag" a key such that keys
with the same tag are guaranteed to be on the Redis server. This makes
the distribution of keys predictable, and allows the use of
transactions (provided all the involved keys have the same tag).

This shows that not only this is a client side feature, but the actual
extent of the feature may vary widely. And of course, there is no
reason to think that different clients will shard keys the same way.

Properly setup, sharding will distribute the data over each
node, reducing the memory load of each node.

## Exercises

### Performance tests

I first tried to rewrite the code in Java, to measure the cost of Ruby's
convenience. The code in Java is clumsier than in Ruby, but it ran
a bit faster (105 seconds instead of 155 seconds for the Ruby version
using `hiredis`).

{% include_code Simple ISBN Loader 7d7w/redis/isbn/ISBNLoader.java %}

Using pipelines, the difference was 11 seconds against 26 seconds
(again, the Ruby version is using `hiredis`).

{% include_code Pipelined ISBN Loader 7d7w/redis/isbn-pipeline/ISBNLoader.java %}

Disabling snapshots and append only file did not improve the time
significantly compared to the default (snapshots but no append only file).

Enabling the append only file and setting it to `always` was almost 3
times as slow for the pipelined Java version (27 seconds). For the
original Ruby version (with `hiredis`), it was even worse (1101
seconds). This means the overhead of writing to file can be mitigated
with pipelines.

To recap: disabling snapshots did not improve performance measurably,
but enabling append only file `always` degrades the performance
significantly; using pipelines makes it a bit better, but it is still
much slower.

### URL Shortening Service

The exact setup to implement is not described, so what I did is to
distribute data between two shards of one master and two slaves.

There is no direct support for such a layout in Jedis (nor, as far as I
can tell, in the Ruby library), so I had to write some of it myself.

As always with Redis, the writes are restricted to the masters, and
the reads are distributed over the slaves (and the masters as well, if
needed).

#### Distribution over slaves

Jedis does not support slaves directly. What the documentation
proposes is to have a dedicated client to the master to write on, and
a sharded pool to the slaves. However, such an approach would be
difficult, as I need to shard the writes to the masters as well (I
would have to use a different sharding algorithm, and manage the
routing of commands through the tree of Redis instances).

Fortunately, Redis user Ingvar Bogdahn had posted an implementation of
a
[Round Robin pool of slaves](http://groups.google.com/group/jedis_redis/msg/c8c76371cf543e36). This
implementation manages a connection pool to a master, and another
connection pool to a set of slaves. The commands are properly
distributed: all the write commands are sent to the master, and the
reads commands are distributed over the slaves.

I had to fix the code in some places: a command implementation was
missing, another was incorrect, and finally the password was never
sent to the master, causing authentication errors. But the bulk of the
code is Ingvar's, and I was glad to use it.

The classes are

 * [`UniJedis`](https://github.com/fdumontmd/url-shortener/blob/master/src/main/java/redis/clients/jedis/UniJedis.java): provides pools for both master and a set of slaves, and dispatches commands to the correct pool.
 * [`RoundRobinPool`](https://github.com/fdumontmd/url-shortener/blob/master/src/main/java/redis/clients/jedis/RoundRobinPool.java): implements a pool with Round Robin access
 * [`ChainableTransaction`](https://github.com/fdumontmd/url-shortener/blob/master/src/main/java/redis/clients/jedis/ChainableTransaction.java): (not used in this project) provides a fluent interface for Redis transactions.
 * [`DBKeys`](https://github.com/fdumontmd/url-shortener/blob/master/src/main/java/redis/clients/jedis/DBKeys.java): (not used in this project) abstracts database and keys.

#### Sharding

Sharding is directly supported by Jedis, but as organized the code is
restricted to a set of clients to specific instances.

There are basic, generic classes
([`Sharded`](https://github.com/xetorthio/jedis/blob/master/src/main/java/redis/clients/util/Sharded.java),
[`ShardInfo`](https://github.com/xetorthio/jedis/blob/master/src/main/java/redis/clients/util/ShardInfo.java),
...) that can be used to implement sharding of arbitrary clients (such
as the Round Robin pool above), but it requires a lot of tedious code
to map each command to a method on the right shard. Worse, such code
would be the same for every kind of shard.

So I first wrote generic classes that implement sharding in terms of
generic Jedis client; the actual implementation is then much simpler
(just the constructors, and the few commands that cannot be sharded,
such as `disconnect` or `flushAll`).

 * [`BinaryShardedGJedis`](https://github.com/fdumontmd/url-shortener/blob/master/src/main/java/redis/clients/jedis/BinaryShardedGJedis.java): first level of Jedis commands implementation (binary commands)
 * [`ShardedGJedis`](https://github.com/fdumontmd/url-shortener/blob/master/src/main/java/redis/clients/jedis/ShardedGJedis.java): second level of Jedis commands implementation (`String` based commands)
 * [`UniJedisShardInfo`](https://github.com/fdumontmd/url-shortener/blob/master/src/main/java/redis/clients/jedis/UniJedisShardInfo.java): descriptor class to use with `Sharded`
 * [`ShardedUniJedis`](https://github.com/fdumontmd/url-shortener/blob/master/src/main/java/redis/clients/jedis/ShardedUniJedis.java): actual implementation of sharded `UniJedis`. As promised, the class has hardly any code.

#### Service

The code for the service itself is now fairly
small. [`JedisClient`](https://github.com/fdumontmd/url-shortener/blob/master/src/main/java/jp/wakatta/client/JedisClient.java)
is the class that builds the tree of sharded master/slaves pools. It
is loaded and initialized as a [Spring](http://www.springsource.org/)
bean. The web services are [JSR 311](http://jsr311.java.net/)
services, running over [Jersey](http://jersey.java.net/), and loaded
and initialized by Spring.

[`Admin`](https://github.com/fdumontmd/url-shortener/blob/master/src/main/java/jp/wakatta/web/Admin.java)
let the user defines a keyword for a specific URL, and
[`Client`](https://github.com/fdumontmd/url-shortener/blob/master/src/main/java/jp/wakatta/web/Client.java)
extracts a keyword from the request URL, retrieves the URL for the
this keyword, and returns a request to redirect to this URL.

Once deployed (on [Apache Tomcat](http://tomcat.apache.org/)\), it can be used in a browser or on the command line:

```
$ curl -X POST http://localhost:8080/url-shortener/u/admin --data "url=http://slashdot.org&shorter=slash"
Key[slash] mapped to URL[http://slashdot.org]
```

and for clients:

```
$ curl -I http://localhost:8080/url-shortener/u/s/slash
HTTP/1.1 303 See Other
Server: Apache-Coyote/1.1
Location: http://slashdot.org
Content-Length: 0
Date: Mon, 23 Jan 2012 06:13:29 GMT
```

The code for the whole project can be found on [Github](https://github.com/fdumontmd/url-shortener).

And this completes Day 2.
