---
layout: post
title: "Seven Databases in Seven Weeks MongoDB Day 3"
date: 2011-12-25 17:18
comments: true
categories: [Books]
tags: [7databases7weeks, databases, mongodb]
series: "Seven Databases in Seven Weeks"
js: [googlemaps, london_map]
css: [gmap]
---
Final day with MongoDB. First to cover geospatial indexing; then to
explore MongoDB's approach to the CAP theorem.

<!--more-->

Like PostgreSQL, MongoDB has integrated support for geometric of
geographic data and queries. Finding the neighbours of a point or
location is trivial, and such queries are optimized using dedicated
kind of indexes.

Regarding the CAP theorem, MongoDB strictly separates Availability
from Partition tolerance: replica sets are designed for availability,
using a quorum approach (like Riak) to select the most recent data in
case of conflict.

Sharding is the dedicated mechanism for partitions. A replica set can
own a shard of the data.

Unlike with Riak, where availability and partitioning are functions of
the properties set on buckets, MongoDB requires the whole topology to
be explicitly configured. I assume that what MongoDB loses in
flexibility, it gets it back in predictability.

## Exercises

### Replica set configuration options

The documentation is
[here](http://www.mongodb.org/display/DOCS/Replica+Set+Configuration).

### Spherical geo index

I don't know if this is another instance of the book describing
features from old version of MongoDB, but there is no such thing as a
spherical geo index.

Spherical searches rely on standard `2d` indexing, as explained
[here](http://www.mongodb.org/display/DOCS/Geospatial+Indexing#GeospatialIndexing-TheEarthisRoundbutMapsareFlat).

### Find all cities within a 50 mile radius of London

To solve this exercise, it is necessary to format the data as required
in the
[documentation](http://www.mongodb.org/display/DOCS/Geospatial+Indexing#GeospatialIndexing-NewSphericalModel). Unfortunately,
the data files in the code for the second beta version of the book use
latitude, longitude whereas MongoDB expects longitude, latitude
(i.e. a X, Y coordinate).

I used the small script below to reformat the data file, and imported
the reformated one:

```
sed -e 's/^\(.*\)latitude\(.*\), *longitude\([^}]*\)}\(.*\)$/\1longitude\3, latitude\2}\4/' \
mongo_cities1000.json > mongo_cities1000_lon_lat.json
```

With that loaded, and with the geospatial indexing in place, MongoDB
is ready to run the queries.

First I need to locate London. There are a few places named London,
but I assume the authors meant the one in England. I create a `centre`
variable to be used in the queries.

{% codeblock Finding London lang:javascript %}
var london = db.cities.findOne( { name: 'London', country: 'GB' }, { location: 1} );
var centre = [london.location.longitude, london.location.latitude];
{% endcodeblock %}

As indicated in the documentation, I have to measure distances in
radians. For this I need to know the
[Earth Radius](http://en.wikipedia.org/wiki/Earth_radius) in miles.

{% codeblock Earth Radius and Range lang:javascript %}
var earthRadius = 3959;
var range = 500;
{% endcodeblock %}

Finally I can run my queries. I have a few options:

#### `geoNear` command

I can pass the `spherical: true` option to the `geoNear` command. By
default, the query will only return 100 results.

{% codeblock geoNear lang:javascript %}
var result = db.runCommand(
    { geoNear: "cities",
      near: centre,
      spherical: true,
      maxDistance: range/earthRadius } )
{% endcodeblock %}

As it turns out, there are far more cities in this range. A circle of
500 miles radius around London includes much of Western Europe:

<div class="gmap" id="map_canvas"></div>

To get unlimited results, I set the number of possible results to the
number of cities:

{% codeblock unlimited geoNear lang:javascript %}
var result = db.runCommand(
    { geoNear: "cities",
      near: centre,
      spherical: true,
      num: db.cities.count(),
      maxDistance: range/earthRadius } )
{% endcodeblock %}

#### `$within` operator

Alternatively, I can use the `$within` operator. I get the spherical
behaviour by specifying a `centerSphere`:

{% codeblock $within lang:javascript %}
var result = db.cities.find(
    { location:
      { $within: {
          $centerSphere: [centre, range/earthRadius]
      }}})
{% endcodeblock %}

This query will return cities within the range, just like the
unlimited `geoNear` one.

### Sharded replicas

This is the kind of things that is not overly difficult, but
tedious. And I don't like tedious.

As a good UNIX geek, I'd rather spend hours to automate what would
have taken me 10 minutes to do manually. So here's the automated setup
in Bash scripts.

First I create all the necessary directories:

```
mkdir mongo{1..6}
mkdir mongoconfig
```

Then I start two sets of 3 replicas that can also be sharded:

```
for i in {1..6}; do
  s=$(( (i-1 ) / 3 + 1))
  mongod --replSet shard$s --shardsvr --dbpath ./mongo$i \
  --port 2701$i --logpath ./mongod_$i.log --rest &
done
```

I setup each replica set:

```
for i in 1 2; do
  members=""
  p=$(( (i-1) * 3 + 1))
  for j in 1 2 3; do
    members="$members, { _id: $j, host: 'localhost:2701$((p+j-1))'}"
  done
  members=${members:1} 
  mongo localhost:2701$p <<HERE
rs.initiate({
  _id: 'shard$i',
  members: [ $members ]
})
HERE
done
```

At this point it is good to wait a minute for the replica sets to be
fully online.

Next step is to figure out the shards URL: they are composed of the
shard name, followed by the list of comma separated servers:

```
shards=()
for i in 1 2; do
  p=$(( (i-1) * 3 + 1))
  output=`mongo --quiet localhost:2701$p/test<<HERE
db.isMaster().setName + '/' + db.isMaster().hosts
HERE`
  shards=( "${shards[@]}" ${output%?bye} )
done
```

Now it is time to start the config server. I move it to the port `27019` as `27016` is already in use:

```
mongod --configsvr --dbpath ./mongoconfig --port 27019 --logpath=mongoconfig.log &
sleep 1
mongos --configdb localhost:27019 --chunkSize 1 --port 27020 --logpath=mongos.log &
```

And finally I add the shards to the config mongo, and enable sharding
on `test` for both the `cities` collection and GridFS:

```
for shard in ${shards[@]}; do
  echo $shard
  mongo localhost:27020/admin --quiet <<HERE
db.runCommand( { addshard: "$shard" })
HERE
done
mongo localhost:27020/admin --quiet <<HERE
db.runCommand( { enablesharding : "test" } );
db.runCommand( { shardcollection : "test.cities", key : {name : 1} } );
db.runCommand( { shardcollection : "test.fs.chunks", key : { files_id : 1 } } )
HERE
```

I can check that everything looks ok with:

```
mongo localhost:27020/admin --quiet <<HERE
db.runCommand( { listshards : 1 })
HERE
```

Of course, all these scripts would be useless to actually distribute
the servers on different machines. Given some time, I'll try to setup
a AWS EC2 cluster as I did for Riak.

At this point, I tried to import the cities data file. It was somewhat
slower than without replicas, but not significantly so.

I also added a file, using the same command as in the book.

Now, to test the replicas, I killed the two primary servers (to
identify them, I used `ps auxw | grep mongod`, which gave me the
process id I needed to kill).

With two servers down, `mongofiles -h localhost:27020 get my_file.txt`
was still able to retrieve the file.

## Wrapping up MongoDB

MongoDB is the first database besides PostgreSQL I feel comfortable
using. They both provide more "database-like" features than either
Riak or HBase: integrated queries, advanced indexing, ... The use of
JavaScript is well integrated and pleasant to use.

Moreover, MongoDB's approach to the CAP theorem is simple. While it is
less flexible or dynamic than Riak's, its simplicity makes it easy to
reason about.
