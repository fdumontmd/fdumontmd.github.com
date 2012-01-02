---
layout: post
title: "Seven Databases in Seven Weeks Neo4j Day 3"
date: 2011-12-30 21:15
comments: true
categories: [Books]
tags: [7databases7weeks, databases, neo4j]
series: "Seven Databases in Seven Weeks"
---
Third, last and quite short day with Neo4j. Today on the menu:
transactions, replication, and backups.

Transactions are a standard feature of relational databases, but NOSQL
databases seem to consider them too costly (of the other databases in
the book, only HBase and Redis also support transactions, as far as I
can tell). Neo4j does support them, along with rollbacks.

Replication is Neo4j's answer for High Availability and, to some
extent, Scaling. The latter is limited as Neo4j does not partition the
data, so everything has to fit in each computer in the cluster.

Finally, backups are exactly what you would expect them to be. Neo4J
offers both full and incremental backups, which update a previous
backup.

<!--more-->

### Transactions

I cannot comment much on transactions, as I could not use them: the
Gremlin shell from the Web Admin console could not find the required
enumeration (which I imported, though), while the Gremlin standalone
shell was giving me strange errors when I tried to import the relevant
classes.

I suppose pure Java would be more reliable, either as standalone code
or plugin, but I did not explore that possibility.

### High-Availability

High-availability is achieved by deploying and linking together
several instances of Neo4j. The setup is somewhat tedious, as there
are additional processes to configure and run (the coordinators), and
four different configuration files to edit. Really, this is the kind
of things you'd wish [Apache Whirr](http://whirr.apache.org/) would do
for you.

But if you want to do it manually, you should follow the
[online documentation](http://docs.neo4j.org/chunked/stable/ha-setup-tutorial.HTML)
rather than the book version (at least in beta version 2.0): the book
use the property `ha.zoo_keeper_servers` in the `neo4j.properties`
configuration file, when the correct property is
`ha.coordinators`. What is worse is that it will look like it works,
until you try to write to a slave over the ReST API, which will fail
with an exception. Writes to the master would also not be pulled by
the slaves. Using the right property name fixes these problems.

Once set up, the cluster will have one master and several slaves. The
master contains the authoritative version of the data. The book
recommends to always write to slaves, as they have to push any update
to the master before completing the update, meaning you have a
guaranteed replication of your data. However, what the book does not
explain is how to figure out which server is slave, or even whether
the list of servers in the cluster can be discovered....

Actually, it is possible to have some idea of which server is the
master by querying any server with

```
curl -H "Content-Type:application/json" -d '["org.neo4j:*"]' http://localhost:7471/db/managerver/jmx/query
```

(assuming one of the server is listening to port `7471`). A sample
reply is shown (only partially, as it is very long) one the
[HA setup tutorial](http://docs.neo4j.org/chunked/snapshot/ha-setup-tutorial.html). But
the actual address of each server is not shown, and I could not find
any way to get the address property to be properly filled.

So the proper way to use such a cluster is probably to use the
[HAProxy](http://haproxy.1wt.eu/), as explained in
[Neo4j HA documentation](http://docs.neo4j.org/chunked/snapshot/ha-haproxy.html). It
can be configured to differentiate between master and slaves, and to
restrict connections to slaves (keeping the list updated with a
check). It can also split the requests by some specific parameter (for
instance, the user id), and direct the requests the same server for a
given value of the parameter. While Neo4j does not shard the data
itself, this mechanism can be used to shard the data cache (what must
be loaded in memory).

### Backups

Neo4j support remote, full or incremental backups. Incremental backups
are properly understood as update to the previous backup (either full
or incremental), and are therefore much faster.

This is a good feature, and should be used often. But as I'm just
playing, and the notion of backup does not lend itself to exploration,
I just looked at them briefly.

## Exercises

### Neo4j licensing guide

The [guide](http://neo4j.org/licensing-guide/) is fortunately quite
short.

### Read-only slaves

This seems to be a description of the original HA feature in Neo4j,
but as far as I can tell it does not exist anymore. In fact, there is
an
[update](https://github.com/neo4j/enterprise/commit/480256bfff036784dc82897d2348a16e3fbf6c03#ha/src/docs/dev/operation.txt)
to the official documentation to remove the mention of read-only
slave.

There used to be a Java class to create a server as read-only slave,
as documented
[here](http://wiki.neo4j.org/index.php?title=Online_Backup_HA&redirect=no#Starting_a_read-only_slave),
but it no longer exists either.

### Maximum number of nodes supported

[34.4 billion nodes](http://docs.neo4j.org/chunked/stable/questions.html#id474370).

### Replication across three physical servers

As I already [explained](/blog/2011/12/17/seven-databases-in-seven-weeks-riak-on-ec2/) how to setup a cluster of EC2 virtual machines
for Riak, I will go skip all the details.

I launched four instances: one will be the HAProxy server, the
remaining three the Neo4j servers.

#### Security Setup

All the rules but the first one are internal (i.e. the source is the
name of the security group, which should be specific to the cluster).

 * 22 (SSH) - source `0.0.0.0/0`
 * 2181: coordinator client port
 * 2888: quorum election port
 * 3888: leader election port
 * 6001: inter cluster communication port
 * 7474: web interface for the Neo4j servers
 * 8080: admin interface for HAProxy
 * 80: web interface for the proxy

Neo4j does not need ranges, unlike Riak.

#### Instance setups

I connect to each of the Neo4j server, and download the Enterprise
edition:

```
wget http://dist.neo4j.org/neo4j-enterprise-1.5-unix.tar.gz
```

First step is to configure the coordinators. I edit the
`conf/coord.cfg` file and replace the server.1 property with the block

```
server.1=10.202.90.131:2888:3888    
server.2=10.202.81.171:2888:3888    
server.3=10.195.78.222:2888:3888
```

(I got the IP addresses by using the `ifconfig` command on each
instance). I also update the `data/coordinator/myid` of each instance
with own number (1 to 3).

I then modified each `conf/neo4j.properties`, setting each to its own
`ha.server_id`, and setting the `ha.coordinators` to
`10.202.90.131:2181,10.202.81.171:2181,10.195.78.222:2181`. I also
changed the `ha.server` to use the `eth0` IP address rather than `localhost`.

Finally, I modified each `conf/neo4j-server.properties`:

 * the web server needs to listen to the `eth0` IP address rather than
   `localhost` (for instance, `org.neo4j.server.webserver.address=10.202.90.131`);
 * the server needs to be set to HA mode:
 `org.neo4j.server.database.mode=HA`

Surprisingly enough, the three servers did start and were configured properly...

I checked the setup with

```
curl -H "Content-Type:application/json" -d '["org.neo4j:*"]'
http://10.202.90.131:7474/db/manage/server/jmx/query
```

I looked for the string `InstancesInCluster`, and made sure there were
three known servers.

Finally I pushed something into the second (slave) server using

```
curl -i -X POST http://10.202.81.171:7474/db/data/node \
-H "Content-Type: appliction/json" \
-d '{"name": "P.G. Wodehouse", "genre": "British Humour"}'
```

then tried to retrieve it from the third (slave) server with

```
$ curl http://10.195.78.222:7474/db/data/node/1
{
  "outgoing_relationships" : "http://10.195.78.222:7474/db/data/node/1/relationships/out",
  "data" : {
    "genre" : "British Humour",
    "name" : "P.G. Wodehouse"
  },
  "traverse" : "http://10.195.78.222:7474/db/data/node/1/traverse/{returnType}",
  "all_typed_relationships" : "http://10.195.78.222:7474/db/data/node/1/relationships/all/{-list|&|types}",
  "property" : "http://10.195.78.222:7474/db/data/node/1/properties/{key}",
  "self" : "http://10.195.78.222:7474/db/data/node/1",
  "properties" : "http://10.195.78.222:7474/db/data/node/1/properties",
  "outgoing_typed_relationships" : "http://10.195.78.222:7474/db/data/node/1/relationships/out/{-list|&|types}",
  "incoming_relationships" : "http://10.195.78.222:7474/db/data/node/1/relationships/in",
  "extensions" : {
  },
  "create_relationship" : "http://10.195.78.222:7474/db/data/node/1/relationships",
  "paged_traverse" : "http://10.195.78.222:7474/db/data/node/1/paged/traverse/{returnType}{?pageSize,leaseTime}",
  "all_relationships" : "http://10.195.78.222:7474/db/data/node/1/relationships/all",
  "incoming_typed_relationships" : "http://10.195.78.222:7474/db/data/node/1/relationships/in/{-list|&|types}"
```

So far so good...

### Load-balancer

Well, [HAproxy](http://haproxy.1wt.eu) seems a good choice, so I'll go with that.

The
[documentation](http://docs.neo4j.org/chunked/snapshot/ha-haproxy.html)
proposes to restrict access to slaves using a "small extension". This
is in fact a piece of Java code that can be downloaded from
[Github](https://github.com/dmontag/neo4j-hastatus-extension).

The compiled jar should be copied to the `lib` directory of each
instance, and the `conf/neo4j-server.properties` configuration file
updated to contain the line

```
org.neo4j.server.thirdparty_jaxrs_classes=org.neo4j.server.hastatus=/hastatus
```

as documented on the page above.

#### Testing Locally

As a first test, I deployed HAProxy on my own machine, using this
configuration file:

{% include_code lang:raw 7d7w/neo4j/haproxy_local.cfg %}

I had installed HAProxy with
[Homebrew](http://mxcl.github.com/homebrew/). The config above does
not bind to port `*:80`, so I can run it without root privileges:

```
/usr/local/sbin/haproxy -f haproxy_local.cfg
```

Once up, I opened a browser on
[HAProxy stat page](http://localhost:8080/haproxy?stats) (it is not
JSON, you really need a browser), to check that two instances of Neo4j
were configured as slaves and available.

Finally, I checked a Gremlin script with:

```
$ curl -X POST
http://localhost:7000/db/data/ext/GremlinPlugin/graphdb/execute_script
-H "content-type:application/json" -d '{"script":"g.V.name"}'
[ "null", "null", "null", "null", "P.G. Wodehouse", "null", "P.G. Wodehouse", "P.G. Wodehouse" ]
```

(the `7000` is the HAProxy port, not any of the Neo4j ports). I had a
few P.G. Wodehouse nodes I inserted when I was testing writes to slaves.

Ok, this is ready to be tested on the AWS cluster.

#### Deploying on the cloud

I used the small cluster deployed in the previous exercise. I just
copied the Neo4j HAStatus extension jar to each machine (in the `lib`
directory), and changed the `conf/neo4j-server.properties` exactly as
above.

I quickly checked that the extension was installed with:

```
curl http://10.202.90.131:7474/hastatus/master
curl http://10.202.81.171:7474/hastatus/slave
curl http://10.195.78.222:7474/hastatus/slave
```

(each is supposed to return nothing. If there's a problem, these
commands will return an error page).

Everything looks fine. Time to set up the HAProxy machine.

Once again, I followed the instructions from the
[Neo4j documentation](http://docs.neo4j.org/chunked/stable/ha-haproxy.html):
first I installed the "Development Tools":

```
yum -y groupinstall 'Development Tools'
```

This step is very fast because they all are stored in the Amazon
Cloud.

I retrieved the HAProxy code:

```
wget http://haproxy.1wt.eu/download/1.4/src/haproxy-1.4.18.tar.gz
```

To build it, I used the command `make TARGET=26` (which means build
for a recent version of Linux).

I did not copy the executable, as I will run it without root
privileges anyway.

I created a file `haproxy.cfg` that contains:

```
global
    daemon
    maxconn 256

defaults
    mode http
    timeout connect 5000ms
    timeout client 50000ms
    timeout server 50000ms

frontend http-in
    bind *:7000
    default_backend neo4j-slaves

backend neo4j-slaves
    option httpchk GET /hastatus/slave
    server s1 10.202.90.131:7474 maxconn 32 check
    server s2 10.202.81.171:7474 maxconn 32 check
    server s3 10.195.78.222:7474 maxconn 32 check
 
listen admin
    bind *:8080
    stats enable
```

which is essentially the same file as the file `haproxy_local.cfg`
above.

I established SSH tunnels to ports `7000` and `8080`, checked the
status of the proxy on `http://localhost:8080/haproxy?stats` (I had
made a mistake to one of the IP address, so I fixed it and restarted
the proxy).

Finally, I was able to run

```
$ curl -X POST http://localhost:7000/db/data/ext/GremlinPlugin/graphdb/execute_script \
-H "content-type:application/json" -d '{"script":"g.V.name"}'
[ "null", "P.G. Wodehouse" ]
```

And all was good.

## Wrapping up Neo4j

This is another database I had to fight all along the way. The
book, the available documentation, and the actual behaviour of the
database overlap only partially. Figuring out what is actually
possible and how to achieve it was harder than for any other databases
in the book.

One thing that was especially irritating is the error handling of the
Gremlin shell: a syntax error such as a missing closing quote renders
the shell unusable: it keeps complaining about the syntax error, but
offers no way to actually correct it. And I could find no way to
reset the shell, except by restarting the whole server...

This, and the fact that both the embedded interpreter or the
standalone shell are unstable in their own different ways (not to
mention slightly incompatible) makes Gremlin useless. But the
alternatives, Cipher or Java, are not really usable either: Cipher is
too limited, Java too verbose and its syntax ill suited.

This said, Neo4j occupies a fairly specific niche which does not have
many alternatives. Let's hope the ecosystem stabilises into
something more coherent and stable.

## The other databases

It seems the Redis might be available soon, but CouchDB is not there
yet. So I will probably switch to a different book for the time being.
