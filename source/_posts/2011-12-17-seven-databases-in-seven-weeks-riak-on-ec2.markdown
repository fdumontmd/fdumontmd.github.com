---
layout: post
title: "Seven Databases in Seven Weeks Riak on EC2"
date: 2011-12-17 14:36
comments: true
categories: [Books]
tags: [7databases7weeks, databases, riak, ec2]
series: "Seven Databases in Seven Weeks"
---

The third day with Riak had proposed to deploy Riak on a
[EC2](http://aws.amazon.com/ec2/) cluster as an exercise. I could not
do it then due to poor network connectivity and lack of time (I was
traveling), but I did it since and here I explain how.

<!--more-->

EC2 is a service from Amazon to commission a number of virtual
computers with specific performance characteristics, and with use
charged by the hour (the rate depending of the performance).

The whole process is fairly simple and flexible. I got my computers up
and running in minutes. Setting up Riak was a bit more involved (I
should probably had tried on my local network first), but eventually I
was able to load the data and run the queries I wanted.

In outline, here's the process

 * figure out the security requirements
 * think about the cluster organisation
 * create a few instances of virtual computers
 * create the security configuration
 * connect to each machine and set it up
 * open an SSL tunnel
 * ...
 * Profit!

And now the details.

### Security requirements

There will be a few machines, and I need each Riak instance to speak
to each other. This means I need to keep all the required ports open.

As each instance is on a different machine (presumably, I could have
several instances on each machine but I want to keep things simple), I
can use the same ports for each instance. I will just have to give
them different names.

Basho has a helpful
[page](http://wiki.basho.com/Network-Security-and-Firewall-Configurations.html)
on this topic. It lists the defaults ports (all of them TCP):

 * epmd's listener: 4369
 * handoff_port listener: 8099
 * web_port: 8098
 * pb_port: 8087
 * plus a range than can be configured. As I have just a small
   network, I restrict this range to 6000-6999.

These are the ports I will open in my configuration settings.

### Cluster organisation

I want a simple setup:

 * 3 machines
 * first one is the ring leader
 * first one is also my interface for client connections

So I need to set 3 machines up, make the last two join the first, and
open an SSL tunnel from my local machine to the ring leader.

### EC2 instances

First I had to sign up to [AWS](http://aws.amazon.com/).

Once this is done, I can use the console, select the EC2 tab, and
click on "Launch Instance".

I chose "Launch Classic Wizard" to have as much flexibility as possible:

{% img https://lh6.googleusercontent.com/-MKXddn82uO8/Tu23KOzMhGI/AAAAAAAAB6Y/XpOren_ANVk/s640/Screen%252520Shot%2525202011-12-18%252520at%25252015.36.26.png %}

Then I select a basic 64bit Amazon Linux:

{% img https://lh3.googleusercontent.com/--F8ajmA-58c/Tu23KHI9YNI/AAAAAAAAB6U/1fCrHF_tqg0/s640/Screen%252520Shot%2525202011-12-18%252520at%25252015.36.34.png %}

I request 3 instances of type Micro (I'm just playing; I don't really care about performance):

{% img https://lh4.googleusercontent.com/-w31FLsR0iC8/Tu2222Q_ZtI/AAAAAAAAB58/E-E86lTf7xg/s640/Screen%252520Shot%2525202011-12-18%252520at%25252015.36.44.png %}

I just click through the next two screens (I do not have any specific need for such tuning):

{% img https://lh5.googleusercontent.com/-kRBmxXnSwRc/Tu222wmgj4I/AAAAAAAAB50/U1Rwuqv6R9o/s640/Screen%252520Shot%2525202011-12-18%252520at%25252015.37.05.png %}
{% img https://lh3.googleusercontent.com/-WiqCpx2PT70/Tu222Iz5nTI/AAAAAAAAB5s/k1ZuuVs1yrE/s640/Screen%252520Shot%2525202011-12-18%252520at%25252015.37.08.png %}

For the key pair, I just give a name, then download the file. I will use it later to connect to my new machines:

{% img https://lh4.googleusercontent.com/-JEnCFjVIM54/Tu222D8yOGI/AAAAAAAAB5o/qdAleXN-iDU/s640/Screen%252520Shot%2525202011-12-18%252520at%25252015.37.28.png %}

For the security, I first name my group, then I can use this name as the source parameter for the rules. All the rules are Custom TCP ones; I just need to give the ports (as determined above):

{% img https://lh3.googleusercontent.com/-dSxG8pYQLEc/Tu222KSxpgI/AAAAAAAAB5w/AtiqjF-QTts/s640/Screen%252520Shot%2525202011-12-18%252520at%25252015.41.18.png %}

And then I can finally start my new machines.

On my first attempts, two of the machines did not start; I just created a couple of new machines of the same kind (64bit Amazon Linux Micro), and put them in the same security group.

Here are the complete security rules (I had to add some after the initial setup):

{% img https://lh6.googleusercontent.com/-tSUcf1TbrLQ/Tu23KB_o3wI/AAAAAAAAB6Q/2OopRB6HFeE/s640/Screen%252520Shot%2525202011-12-18%252520at%25252017.05.42.png %}

With this, I have a small cluster of machines. Time to connect and put
them to good use.

### Configuring each machine

From the EC2 Dashboard, I can access my instances, and get the address for each.

Using the private key downloaded earlier, I can open an SSH connection
to each machine. Note that the user is by default called `ec2-user`:

```
ssh -i riak-private.pem ec2-user@<MACHINE_NAME>
```

On each, I first need to install riak. I download it directly from
Basho [website](http://downloads.basho.com/riak/CURRENT/).

Each machine has some basic tools, but no Erlang compiler. To avoid
any complications, I chose and retrieved a binary distribution:

```
wget http://downloads.basho.com/riak/CURRENT/riak-1.0.2-1.el6.x86_64.rpm
```

Then I  installed the  downloaded package  (`ec2-user` can  use `sudo`
without having to provide a password):

```
sudo rpm -Uvh riak-1.0.2-1.el6.x86_64.rpm
```

There are a few error messages (or perhaps warnings), but the package
is installed and running.

Riak is installed but not started yet. It is important to get the
configuration right before starting it.

#### Common configuration

For each server, I give it a name that includes the local network IP
address (not the loopback IP address `127.0.0.1`). It is important,
otherwise the machines cannot talk to each others.

I can get it by running `ifconfig`. I look for the ethernet setup and especially the `inet` value:
this is the IP address in Amazon private network. I need to remember
the IP address for what I will call the first machine, so I copy it
somewhere (and call it here `<IP_ADDRESS_1>`)

Using `sudo -e /etc/riak/vm.args`, I can edit the name of each node. I set it to `riak_ec2_1@<IP_ADDRESS>`, (or `riak_ec2_2`, `riak_ec2_3`, ...).

Note: the editor will be `vi`.

I also need to change the `app.config` file:

```
sudo -e /etc/riak/app.config`
```

I first need to insert a range restriction configuration. I insert the
following block near the top:

```
{ kernel, [
            {inet_dist_listen_min, 6000},
            {inet_dist_listen_max, 6999}
            ]},
```

For the first machine (the one named `riak_ec2_1`), I also need to
extend the `http` interface list. By default it only listen to the
loopback interface (`127.0.0.1`), but I also need it to listen to the
ethernet interface (see above). Otherwise, I will not be able to set
the SSL tunnel up.

So I replace the lines that read
```
{http, [ {"127.0.0.1", 8098 } ]},
```
by something like
```
{http, [ {"127.0.0.1", 8098 }, {"<IP_ADDRESS_1>", 8098} ]},
```

Once this is done, I started the instances:

```
sudo riak start
```

I can test each instance with:
```
curl http://localhost:8098/stats | python -mjson.tool
```

Finally, I can link all instances together by executing on the second
and third machine:

```
sudo riak-admin join riak_ec2_1@<IP_ADDRESS_1>
```

Testing again the status should show three instances in the same ring.

In case of error, uninstall riak, delete the `/var/lib/riak` directory,
and start again (as I did).

### Setting up the SSL tunnel

Nothing simpler. Using the first machine name (the one I configured
for connection from the ethernet interface):

```
ssh -i riak-private.pem -f ec2-user@<MACHINE_NAME> \
-L 8098:ec2-user@<MACHINE_NAME>:8098 -N
```

And now I can run from my local machine

```
curl http://localhost:8098/stats | python -mjson.tool
```

and get the status of the Riak server as if it was local.

### Loading data and running queries

The book suggest to use the example data from Basho's
[website](http://wiki.basho.com/Loading-Data-and-Running-MapReduce-Queries.html),
which is what I did. I downloaded the
[`goog.csv`](http://wiki.basho.com/attachments/goog.csv) data file and
the [`load_data`](http://wiki.basho.com/attachments/load_data)
script. I changed the port number in the latter to use 8098 instead of
8091, then I just ran it: the data is send through the SSL tunnel and
loaded in my EC2 cluster (but it is very slow).

Once this was done, I checked the various queries proposed on the page, and also implemented the MapReduce Challenge:

### MapReduce Challenge

The MapReduce Challenge is to compute the days with the highest volume
of dollar traded. This first step is to compute this value, as it is
not part of the initial data.

I use a definition found
[here](http://wiki.fool.com/Daily_dollar_volume), which makes
intuitive sense: `Volume*(High - Low)/2`

#### Dollar Traded Volume by Month

I need to keep both the date and the amount, so my map function will
keep both items in a data structured indexed by month:

{% codeblock Dollar Traded by Month lang:javascript %}
function(value, keyData, arg) {
  var data = Riak.mapValuesJson(value)[0];
  var month = value.key.split('-').slice(0,2).join('-');
  var pair = {};
  pair['date'] = value.key;
  pair['traded'] = data.Volume * (data.High - data.Low) / 2;
  var obj = {};
  obj[month] = pair;
  return [obj];
}
{% endcodeblock %}

Then when reducing, I can compare the traded amount and keep the best:

{% codeblock Highest by month lang:javascript %}
function(values, arg) {
  return [ values.reduce(function(acc, item) {
    for(var month in item) {
      if(acc[month]) {
        acc[month] = acc[month]['traded'] < item[month]['traded'] ?
  item[month] : acc[month];
      } else acc[month] = item[month];
    }
    return acc;
  })
  ];
}
{% endcodeblock %}


To test the whole, I can use the following command, then copy and
paste the query, and execute with `Ctrl-D`. The command below is run on
my local machine, but thanks to the SSL tunnel, it will execute on the
cluster:

```
curl -X POST http://127.0.0.1:8098/mapred \
-H "Content-Type: application/json" -d @-
```

And the query:
```
{"inputs":"goog",
 "query":[{"map":{"language":"javascript",
                  "source":"function(value, keyData, arg) {
  var data = Riak.mapValuesJson(value)[0];
  var month = value.key.split('-').slice(0,2).join('-');
  var pair = {};
  pair['date'] = value.key;
  pair['traded'] = data.Volume * (data.High - data.Low) / 2;
  var obj = {};
  obj[month] = pair;
  return [obj];
}"}},
         {"reduce":{"language":"javascript",
                    "source":"function(values, arg) {
  return [ values.reduce(function(acc, item) {
    for(var month in item) {
      if(acc[month]) {
        acc[month] = acc[month]['traded'] < item[month]['traded'] ?
  item[month] : acc[month];
      } else acc[month] = item[month];
    }
    return acc;
  })
  ];
}",
         "keep":true}}]}
```

#### Day by Highest Dollar Traded

For this one I use a similar approach: I create pairs with the date
and the amount of dollar traded, but this pair is what I return
directly (instead of returning it indexed by month).

Indexing by date would not work as in the reduce step I would have to
keep the result in a simple (i.e. not indexed) variable, and therefore
would forget the date. By keeping both data items (the date and the
volume traded in dollar) in the same object, I can reduce a list of
such objects to a single item and still retain all the relevant information.

The mapping function creates just a pair object from the data:
{% codeblock Dollar Traded by Day lang:javascript %}
function(value, keyData, arg) {
  var data = Riak.mapValuesJson(value)[0];
  var obj = {};
  obj['date'] = value.key;
  obj['traded'] = data.Volume * (data.High - data.Low) / 2;
  return [obj];
}
{% endcodeblock %}

The reducing function retains the best date by volume traded in dollar
for each batch. It is simpler than the previous one as the values are
simple (i.e. not indexed):

{% codeblock Highest ever lang:javascript %}
function(values, arg){
  return [ values.reduce(function(acc, item){
             if(acc) { acc = (acc['traded'] < item['traded']) ? item : acc; }
             else { acc = item; }
             return acc;
            })
         ];
}
{% endcodeblock %}

And the query:
```
{"inputs":"goog",
 "query":[{"map":{"language":"javascript",
                  "source":"function(value, keyData, arg) {
  var data = Riak.mapValuesJson(value)[0];
  var obj = {};
  obj['date'] = value.key;
  obj['traded'] = data.Volume * (data.High - data.Low) / 2;
  return [obj];
}"}},
         {"reduce":{"language":"javascript",
                    "source":"function(values, arg){
  return [ values.reduce(function(acc, item){
                 if(acc) { acc = (acc['traded'] < item['traded']) ? item : acc; }
                 else { acc = item[date]; }
             return acc;
            })
         ];
}",
         "keep":true}}]}
```

And that's it for today.
