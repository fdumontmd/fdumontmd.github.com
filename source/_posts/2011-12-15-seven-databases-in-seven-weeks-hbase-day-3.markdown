---
layout: post
title: "Seven Databases in Seven Weeks HBase Day 3"
date: 2011-12-15 16:27
comments: true
categories: [Books]
tags: [7databases7weeks, databases, hbase]
series: "Seven Databases in Seven Weeks"
---
The third day with HBase is a bit short, but opens to a world of
possibilities: the Cloud.

This is where HBase belongs. No personal (or even that many corporate)
networks are large enough to let it perform correctly.

HBase depends on a large number of servers running in parallel for
its performance, and there are few other places to find that many 
machines.

<!--more-->

### Thrift

The first topic for today is [Thrift](http://thrift.apache.org/), a
generic remote interface to program servers (and a gift from the new
Evil Empire, Facebook).

It is a tool to document a binary API, and generate client stubs to
use this API. HBase supports such an API, making it possible to write
clients in a variety of languages.

Using Thrift on your own project (on the server side, if you have any)
would make it possible to use different languages on the client side,
depending on whichever better fits the needs (scripting languages for
glue scripts, ...)

When I tried the example from the book, I had to change the connection
address of the `thrift_example.rb` code from `localhost` to
`127.0.0.1`, otherwise Thrift would refuse the connection.

### Whirr

[Whirr](http://whirr.apache.org/) is far more exciting. It is a tool
to deploy and configure some specific servers on (among others)
[Amazon EC2](http://aws.amazon.com/ec2/).

The first, and perhaps the most complex step is to open an account on
[AWS](http://aws.amazon.com/). It will require a phone, a credit card,
a computer, and some time. And perhaps a couple of emails if the
account opening remains stuck in "Pending verification" status.

Once this is done, Whirr can be used to create instances (be careful
with that: Amazon will charge at least one hour for each server even
if you take it down after a couple of minutes), download and install
specific servers (mostly from the [Hadoop](http://hadoop.apache.org/)
family), configure them, all of this from the comfort of the command
line (which is my case is cosily close to a cup of warm coco, so it is
very comfortable indeed).

All you have to do is retrieve you security token from your AWS
account page, create a public/private key pair, then write a recipe
file (which describes what kind of machines and how many you need,
what to install on each, ...), and Whirr takes care of the rest. The
first two steps only have to be done once; you can deploy as many
recipes as you need.

The setup process takes a few minutes, then you can connect with SSH
to one of your remote servers.

Whirr also creates a security configuration for each recipe, opening
only the ports that are required by the servers in the recipe,
limiting source of the connections to specific servers. You can also
edit the security rules directly in the recipe if you want.

The ease with which this can be done is really surprising. It reminds
me of how easy it was to deploy a Rails application on
[Heroku](http://www.heroku.com/).

Now, I do not have any foreseen uses for such computing capacity, but
I can see how it could be helpful for any organisation to be able to
run occasional large data processing jobs without having to maintain a
permanent data center.

Exercises
---------

There is only one exercise today: to open a Thrift connection to an
AWS deployed HBase.

The method described in the book is to open the port 9090 to the
world, and to hope to be the only one to know about this port: a
likely possibility, but who would want to take such a chance in
production?

Fortunately, there is a better solution: SSH Tunneling. It is very
easy to set up and requires nothing but what we already have.

The general idea is to open a ssh tunnel between a local port and a
remote port: whatever you puts in the local port is taken by ssh,
transported over the SSH connection; once it reaches the remote
machine, the remote ssh instance will forward the data to the remote
port, as if it was a client running on the remote machine.

The transport between the two machines only requires the remote one to
have the SSH port open (which is both the case, and secure). You have
to use authentication and encryption for the transport.

And what is required to implement this SSH tunneling:

```
ssh -i keys/id_rsa -f ${USER}@<SERVER_NAME> -L 9090:<SERVER_NAME>:9090 -N
```

(from the directory where you created the `keys` directory)

Here I map the local port 9090 to the remote machine's port 9090. That
way I don't even have to change my `thrift_example.rb` code. But of
course, if I had to connect to different machines, I would use
different ports.

The Thrift server must be started as in the book (binding to
`0.0.0.0`) because the binding will be from the network IP, not the
loop IP:

```
sudo /usr/local/hbase-0.90.3/bin/hbase-daemon.sh start thrift -b 0.0.0.0
```

With this in place, and after creating some tables in the remote
HBase:

```
$ ruby thrift_example.rb 
links
  from:
    maxVersions: 1
    compression: NONE
    bloomFilterType: ROWCOL
  to:
    maxVersions: 1
    compression: NONE
    bloomFilterType: ROWCOL
wiki
  revision:
    maxVersions: 2147483647
    compression: NONE
    bloomFilterType: NONE
  text:
    maxVersions: 2147483647
    compression: GZ
    bloomFilterType: ROW
```

(be careful not to use LZO as a compression algorithm in the remote
HBase, as I did when I tried the first time: the default HBase has no
LZO support and will fail when you try to enable a table with LZO compression).

To take a tunnel down, you'll have to find and kill it (as far as I
can tell). If you have no other ssh connections, `killall ssh` is a
simple solution.

Wrapping up HBase
-----------------

I like what I see with HBase: the project has strong backers among its
users (Yahoo, Facebook, ...); it belongs to a large family of tools
that help to design Big Data solutions, and integrates well with some
Cloud networks

The model is easy to understand (the book mentions the possibility of
eventual consistency due to regional replication, but this remains a
simpler model than Riak's), and close to the original MapReduce
concept.

This is really one tool I will have a closer look to in the near
future.
