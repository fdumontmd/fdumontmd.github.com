---
layout: post
title: "Seven Databases in Seven Weeks Riak Day 2"
date: 2011-12-08 22:59
comments: true
categories: [Books]
tags: [7databases7weeks, databases, riak]
series: "Seven Databases in Seven Weeks"
---
Day 2 with Riak, to cover Mapreduce, distribution, rings and consistency.

Mapreduce will be familiar to anyone who has been paying attention to Google. The basic idea comes from functional programming, but Google showed how it could be used to distribute the load over many machines (this in turn spawned two distinct recent developments in software: one is the range of databases explicitly built around the concept of Mapreduce; the other is the idea that functional languages are better suited to do concurrent and parallel programming).

<!--more-->

### A followup to yesterday simple trick

There is an alternative, maybe even simpler than yesterday's use of `python -mjson.tool`. It is shown (maybe a bit late) in the book: using an `Accept: text/plain` header in the HTTP request, as in:

```
curl -H "Accept: text/plain" http://localhost:8091/stats
```

This asks Riak to return plain text instead of json data; Riak helpfully interprets that as meaning the same data, but formatted for people. The advantage of this approach is that is still works even when `curl` generates extra output:

```
curl -v -H "Accept: text/plain" http//localhost:8091/stats
```

will both generate verbose debugging output, and get the data in human format.

### Nothing wrong with Mapreduce per se, but...

I understand that the examples have to remain simple enough to be implementable on a (possible shared) single personal computer, but I still think that the authors are being dishonest when they contrast Mapreduce to an "SQL" based solution. 

The problem with this solution is that it is just what a self-taught coder with no understanding of SQL might produced (ok, I'm being unfair. Some college educated programmer I have known actually write code like that as well). But this idea of pushing the code to the data instead of pulling the data to the code is exactly what a database like PostgreSQL does. 

An actual SQL solution would be much simpler, would run fully in the database (no need for a script to create the initial data), and would deliver the data long before you'd be done coding and debugging the Javascript Mapreduce code.

{% codeblock A real SQL solution lang:sql %}
-- SQL supports composite primary key, which this problem maps well to
CREATE TABLE hotel (
  floor int NOT NULL,
  room  int  NOT NULL,
  capacity int NOT NULL,
  style char(6) CHECK (style IN ('single', 'double', 'queen', 'king', 'suite') ),
  PRIMARY KEY (floor, room)
);

-- a simple function to provide a easy to use random range
CREATE OR REPLACE FUNCTION random(numeric, numeric)
RETURNS numeric AS
$$
   SELECT ($1 + ($2 - $1) * random())::numeric;
$$ LANGUAGE 'sql' VOLATILE;

-- using generate_series and the random function above
-- to fill all the rooms, in pure PostgreSQL 
INSERT INTO hotel (floor, room, style, capacity) 
  (SELECT f, r, (ARRAY['single', 'double', 'queen', 'king', 'suite'])[random(1, 5)], random(1,8) FROM     
    generate_series(1,100) AS f, generate_series(0,99) AS r);

-- the basic query
SELECT style, SUM(capacity) FROM hotel GROUP BY style;

-- same query with filter
SELECT style, SUM(capacity) FROM hotel WHERE floor <= 10 GROUP BY style;
{% endcodeblock %}

It does not help that actually testing the code on Riak, I had to wait about 10 seconds while PostgreSQL answer was immediate. Of course, I have 3 instances of Riak on a single machine, which is certainly not a typical setup.

Riak is on a specific point among the trade off line to which data stores are constrained by the [CAP theorem](http://en.wikipedia.org/wiki/CAP_theorem). It would have been better to work on an example (maybe using more complex data, loaded from a file, like the movie database in PostgreSQL Day 3) that plays into Riak strengths rather showing how badly it performs compared to a much easier and more natural solution in SQL.

It might have been enough to ask the reader to imagine the dataset to be so large that it could not fit in a single server (for instance, a simulation of the [Hilbert's Hotel](http://en.wikipedia.org/wiki/Hilbert's_paradox_of_the_Grand_Hotel)), or to simulate a rocky network by shutting down some nodes at awkward moments, and show how Riak still delivers the goods. In such conditions most SQL databases would start to show their limits, and these would be legitimate.

### Another comment on the example

If you run all the examples and the exercises, you will notice something interesting: there is a floor 101. That's actually a bug in the initial seeding script: the room number should range over 0 to 99, not 1 to 100. Mapping composite keys to a single value is not always safe. Oh well...

I've changed my script after I went through everything, rebuild the data, and ran everything again.


### Partial updates are not supported, unless they are

So I said yesterday that partial updates are not supported. And if they are, I really have not found a way to get them to work.

Except for buckets. You can update properties of a bucket without having to specify every one of them. Which is good. But still, it might have been good for the rest of the data to have this feature.

The fact that the properties of buckets are probably fixed, meaning you can't remove any, makes a partial update non ambiguous. Supporting partial updates on generic data would require a way to specify whether the update was complete or partial, and a way to remove properties. I can see why Riak designers chose not to go that way, but it still feels a bit ad hoc overall.

### CAP choices

The section called "On Consistency and Durability" is more interesting, and better shows which features Riak attempts to provide.

The ability to choose from various strategies to implement consistency and durability certainly introduces additional ways to make a mistake, but it also offers flexibility where the relational databases typically offer little.

Exercises
---------

### Online Riak Mapreduce documentation

The documentation is [http://wiki.basho.com/MapReduce.html](http://wiki.basho.com/MapReduce.html).

### Riak contrib functions

Google is a good friend. The central [site](http://contrib.basho.com/) and the [repository](https://github.com/basho/riak_function_contrib).

### Keys filter documentation

[http://wiki.basho.com/Key-Filters.html](http://wiki.basho.com/Key-Filters.html)

### Total capacity by floor

The first step is to map the original data to an array of capacity indexed by floor. What has not been seen so far is how to get the `key` of an object, but it is just another property (directly in `v`, not in the `values` array):

{% codeblock Mapping data lang:js %}
function(v) {
  var parsed_data = JSON.parse(v.values[0].data); 
  var data = {};
  var floor = ~~(parseInt(v.key) / 100);
  data[floor] = parsed_data.capacity; 
  return [data];
}
{% endcodeblock %}

A quick test:

```
curl -X POST -H "content-type:application/json" http://localhost:8091/mapred --data @-
{
  "inputs":[
    ["rooms","101"],["rooms","102"],["rooms","103"]],
  "query":[
    {"map":{
      "language":"javascript",
      "source":
		"function(v) {
		  var parsed_data = JSON.parse(v.values[0].data); 
		  var data = {};
		  var floor = ~~(parseInt(v.key) / 100);
		  data[floor] = parsed_data.capacity; 
		  return [data];
		}"}
	} ]
}

Ctrl-D

[{"1":3},{"1":2},{"1":2}]
```

Ok, looking good. Now the `reduce` part. It is strictly identical to the book's `reduce` function, but I just renamed some variables to make it clear what is being iterated over:

{% codeblock Reducing data lang:js %}
function(v) {
  var totals = {};
  for (var i in v) {
    for(var floor in v[i]) {
      if( totals[floor] ) 
        totals[floor] += v[i][floor];
      else
        totals[floor] = v[i][floor];
    } 
  }
  
  return [totals];
}
{% endcodeblock %}

Testing:

```
curl -X POST -H "content-type:application/json" http://localhost:8091/mapred --data @-
{
  "inputs":[
    ["rooms","101"],["rooms","102"],["rooms","103"]],
  "query":[
    {"map":{
      "language":"javascript",
      "source":
		"function(v) {
		  var parsed_data = JSON.parse(v.values[0].data); 
		  var data = {};
		  var floor = ~~(parseInt(v.key) / 100);
		  data[floor] = parsed_data.capacity; 
		  return [data];
		}"}
	},
	{"reduce": {
		"language": "javascript",
		"source":
		  "function(v) {
			  var totals = {};
			  for (var i in v) {
			    for(var floor in v[i]) {
			      if( totals[floor] ) 
			        totals[floor] += v[i][floor];
			      else
			        totals[floor] = v[i][floor];
			    } 
			  }

			  return [totals];
			}"}
	} ]
}

Ctrl-D

[{"1":7}]
```

Ok, still looking good. Let's run it on the whole set:

```
curl -X POST -H "content-type:application/json" http://localhost:8091/mapred --data @-
{
  "inputs":"rooms",
  "query":[
    {"map":{
      "language":"javascript",
      "source":
		"function(v) {
		  var parsed_data = JSON.parse(v.values[0].data); 
		  var data = {};
		  var floor = ~~(parseInt(v.key) / 100);
		  data[floor] = parsed_data.capacity; 
		  return [data];
		}"}
	},
	{"reduce": {
		"language": "javascript",
		"source":
		  "function(v) {
			  var totals = {};
			  for (var i in v) {
			    for(var floor in v[i]) {
			      if( totals[floor] ) 
			        totals[floor] += v[i][floor];
			      else
			        totals[floor] = v[i][floor];
			    } 
			  }

			  return [totals];
			}"}
	} ]
}

Ctrl-D

[
    {
        "1": 456, 
        "10": 445, 
        "100": 452, 
        "11": 448, 
        "12": 482, 
        "13": 452, 
        "14": 489, 
        "15": 467, 
        "16": 461, 
        "17": 471, 
        "18": 426, 
        "19": 426, 
        "2": 394, 
        "20": 413, 
        "21": 428, 
        "22": 460, 
        "23": 447, 
        "24": 443, 
        "25": 430, 
        "26": 430, 
        "27": 447, 
        "28": 486, 
        "29": 429, 
        "3": 437, 
        "30": 434, 
        "31": 415, 
        "32": 483, 
        "33": 460, 
        "34": 440, 
        "35": 519, 
        "36": 492, 
        "37": 422, 
        "38": 413, 
        "39": 439, 
        "4": 451, 
        "40": 440, 
        "41": 458, 
        "42": 386, 
        "43": 488, 
        "44": 428, 
        "45": 423, 
        "46": 487, 
        "47": 463, 
        "48": 408, 
        "49": 422, 
        "5": 417, 
        "50": 464, 
        "51": 434, 
        "52": 429, 
        "53": 468, 
        "54": 412, 
        "55": 440, 
        "56": 427, 
        "57": 458, 
        "58": 420, 
        "59": 438, 
        "6": 426, 
        "60": 464, 
        "61": 446, 
        "62": 412, 
        "63": 431, 
        "64": 445, 
        "65": 435, 
        "66": 444, 
        "67": 449, 
        "68": 460, 
        "69": 474, 
        "7": 436, 
        "70": 473, 
        "71": 431, 
        "72": 457, 
        "73": 426, 
        "74": 454, 
        "75": 463, 
        "76": 406, 
        "77": 464, 
        "78": 441, 
        "79": 502, 
        "8": 421, 
        "80": 477, 
        "81": 422, 
        "82": 441, 
        "83": 466, 
        "84": 447, 
        "85": 488, 
        "86": 486, 
        "87": 414, 
        "88": 463, 
        "89": 494, 
        "9": 446, 
        "90": 441, 
        "91": 459, 
        "92": 433, 
        "93": 488, 
        "94": 450, 
        "95": 442, 
        "96": 479, 
        "97": 460, 
        "98": 429, 
        "99": 445
    }
]
```


### Restrict the capacity count to floors 42 and 43

It should be enough that the key be between 4200 and 4390: 

```
curl -X POST -H "content-type:application/json" http://localhost:8091/mapred --data @-
{
  "inputs":{
	"bucket": "rooms",
	"key_filters": [["string_to_int"], ["between", 4200, 4399]]
  },
  "query":[
    {"map":{
      "language":"javascript",
      "source":
		"function(v) {
		  var parsed_data = JSON.parse(v.values[0].data); 
		  var data = {};
		  var floor = ~~(parseInt(v.key) / 100);
		  data[floor] = parsed_data.capacity; 
		  return [data];
		}"}
	},
	{"reduce": {
		"language": "javascript",
		"source":
		  "function(v) {
			  var totals = {};
			  for (var i in v) {
			    for(var floor in v[i]) {
			      if( totals[floor] ) 
			        totals[floor] += v[i][floor];
			      else
			        totals[floor] = v[i][floor];
			    } 
			  }

			  return [totals];
			}"}
	} ]
}

Ctrl-D

[{"42":386,"43":488}]
```

The output matches what was computed for the whole set of rooms, which is always a good thing.

And this completes Day 2. I must admit I have so far mixed feelings: I understand better what Riak tries to achieve, but the example is just not the kind of things Riak should be used for, and the Mapreduce syntax is a bit heavy (although, fortunately, very regular).

Tomorrow will cover Vector Clocks, the mechanism that Riak exposes both to move conflicts resolution to the client side, and to help the client to implement a decent resolution.