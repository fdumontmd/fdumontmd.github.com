---
layout: post
title: "Seven Databases in Seven Weeks HBase Day 1"
date: 2011-12-11 12:57
comments: true
categories: [Books]
tags: [7databases7weeks, databases, hbase]
series: "Seven Databases in Seven Weeks"
---
New week, new database. This week is about [HBase](), a product that
has a significant enterprisy feel about it. First it is written in
Java, the de facto enterprise language. Then it is already in
production in very large big data consumers (Facebook among others).

Perhaps more surprising is the fact that it even runs at all on a
single, personal computer (as the book states, 5 dedicated servers is
the recommended minimal configuration).
<!--more-->
Today is a fairly short day. Getting HBase to run, creating a single
table and a couple of rows, and that's it.

As for Riak, I recommend downloading the
[HBase package](http://www.apache.org/dyn/closer.cgi?path=hbase/hbase-0.90.3/hbase-0.90.3.tar.gz)
rather than trying your luck with the Homebrew version. HBase runs
directly from the extraction directory, and already includes all the
dependencies.

Just edit the hbase-site.xml configuratio file as the book recommends,
and you're good to go.

Exercises
---------

### put_many function

This function is more an exercise in Ruby than in HBase. The code is
just a variant of what is already in the book.

{% include_code put_many.rb lang:ruby 7d7w/hbase/put_many.rb %}

### Use the put_many function

Invoking the `put_many` function then checking the insert:

{% codeblock Testing put_many lang:ruby %}
put_many 'wiki', 'Some title', {
  "text:" => "Some article text",
  "revision:author" => "jschmoe",
  "revision:comment" => "no comment" }

get 'wiki', 'Some title'
{% endcodeblock %}

generates

```
COLUMN                CELL                                                      
 revision:author      timestamp=1323575657943, value=jschmoe                    
 revision:comment     timestamp=1323575657943, value=no comment                 
 text:                timestamp=1323575657943, value=Some article text          
3 row(s) in 0.5340 seconds
```

And that's all for today. Tomorrow will be a bit more fun: first a
significant take on of Wikipedia files, then using HBase to play with
the loaded data.
