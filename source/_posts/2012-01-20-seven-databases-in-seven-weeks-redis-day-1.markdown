---
layout: post
title: "Seven Databases in Seven Weeks Redis Day 1"
date: 2012-01-20 16:52
comments: true
categories: [Books]
tags: [7databases7weeks, databases, redis]
series: "Seven Databases in Seven Weeks"
---
After a long winter hiatus, the elves at
[Pragmatic Bookshelf](http://pragprog.com/) delivered a late but
welcome present: the third beta of
[Seven Databases in Seven Weeks](http://pragprog.com/book/rwdata/seven-databases-in-seven-weeks). The
book is not complete yet (the chapter on
[CouchDB](http://couchdb.apache.org/) is still missing), but it now
covers [Redis](http://redis.io/).

Redis is basically a key-value store, like
[Riak](http://wiki.basho.com/), but while Riak is agnostic about the
values, Redis values can be data structures (lists, queues,
dictonaries, ..., or even messaging queues). This allows Redis to act
as a synchronized shared memory for cooperating applications.

<!-- more -->

### Complex Datatypes

Redis values can have structure, and specific commands manipulate
these values in appropriate ways. Redis supports
[strings](http://redis.io/commands/#string), which can also behave
as numbers if they have the right format,
[lists](http://redis.io/commands#list) which can also be seen as
queues, and support blocking reads,
[sets](http://redis.io/commands#set),
[hashes](http://redis.io/commands#hash) (that is, dictionaries), and
[sorted sets](http://redis.io/commands#sorted_set).

### Transactions

All Redis commands are atomic, and it is possible to group a sequence
of commands into a transaction for an all or nothing execution with
the command [`MULTI`](http://redis.io/commands/multi). But a
Redis transaction is not similar to a transaction in relational
databases: it just queues all the commands and executes them when it
receives the [`EXEC`](http://redis.io/commands/exec) command. This
means it is not possible to read any data while in a transaction.

### Expiry

Perhaps nothing labels Redis as a datastore for transient data more
than expiry: keys can be marked for expiration (either relative from
the current time, or absolute).

### Messaging

Redis also supports messaging but this is a topic for
Day 2.

This
[post](http://blog.mjrusso.com/2010/10/17/redis-from-the-ground-up.html)
has a more detailed but still balanced coverage of Redis.

## Exercises

### Redis command documentation

The [documentation](http://redis.io/commands) is well done and easy to
navigate. Of all the databases I have seen so far, this is probably
the base
([PostgreSQL](http://www.postgresql.org/docs/current/static/index.html)
being a strong second).

### Create a Redis client

I'm using Java and the [Jedis](https://github.com/xetorthio/jedis)
client library.

The code is simple enough:

{% include_code simple redis client 7d7w/redis/first/RedisFirst.java %}

The `pom.xml` file:

{% include_code 7d7w/redis/first/pom.xml %}

### Create a pair of Redis clients

This one is simple as well, but having a reader and a writer allowed
me to try one writer and two readers.

First the writer program:

{% include_code Redis Push 7d7w/redis/push/RedisPush.java %}

The `poml.xml` is a bit more complex, as it creates a self-contained
jar with `MANIFEST.MF` (so I can run it from the command line easily):

{% include_code 7d7w/redis/push/pom.xml %}

The reader program:

{% include_code Redis Pop 7d7w/redis/pop/RedisPop.java %}

with its `pom.xml`:

{% include_code 7d7w/redis/pop/pom.xml %}

The `blpop` command can block on several lists, so when it receives
something it is always at least a pair: the list key, and the value.

Now, I can open three terminals to test the code: two with readers:

```
java -jar target/redis-pop-0.0.1-SNAPSHOT-jar-with-dependencies.jar
```

and one with the writer (which must be started last):

```
java -jar target/redis-push-0.0.1-SNAPSHOT-jar-with-dependencies.jar
```

The writer will simply state

```
Message inserted
```

One of the readers will get the message:
```
Waiting for messages
Messages received:
msg:queue
A new message
Waiting for messages
```

but the other one will just keep waiting:
```
Waiting for messages
```

So Redis blocking queues can only server one blocking reader at a time
(as it should).

The reader programs can be stopped with `Ctrl-c`, or by pushing
`finish` into `msg:queue` from a Redis client (twice, once for each
client):

```
redis 127.0.0.1:6379> lpush "msg:queue" "finish"
(integer) 1
redis 127.0.0.1:6379> lpush "msg:queue" "finish"
(integer) 1
```

And that's all for today.
